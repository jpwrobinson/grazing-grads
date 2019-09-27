

library(tidyverse) 
library(funk) 
library(gridExtra) 
library(here) 
library(rethinking)

## bite predictions for each species, sourced from bite_rate_models.R

# UVC data load
load("data/UVC_WIO_herb_benthic.Rdata")
grazers<-pred %>% filter(FG == 'Herbivore Grazer')
## add genera column
grazers$genus<-str_split_fixed(grazers$species, pattern = '\\ ', n=2)[,1]

## match bite rate for each species, from species predictions in bite_rate_models
grazers$bite.rate<-grazer.bites$median[match(grazers$species, grazer.bites$class)]
## if species is missing, use genera
grazers$bite.rate[is.na(grazers$bite.rate)]<-
          grazer.bites$median[match(grazers$genus[is.na(grazers$bite.rate)], grazer.bites$class)]
## if genera is missing, use global mean
grazers$bite.rate[is.na(grazers$bite.rate)]<-grazer.bites$median[grazer.bites$preds == 'global.mean']

## now convert bite rate to algal consumption based on biomass
grazers$g.carbon.day<-0.0342 * grazers$mass.g^0.816
## now convert bite rate to total bites per day
grazers$daily.bites<-grazers$bite.rate*60*12
## divide by daily bite rate to get grams carbon per bite 
grazers$carbon.per.bite<-grazers$g.carbon.day/(grazers$daily.bites)
## scale up to estimate grams consumed per minute
grazers$cropping<-grazers$carbon.per.bite * grazers$bite.rate

# estimate mean total cropping function per site
h <- grazers %>% 
  ## sum cropping in each transect
        group_by(dataset, date, reef, site, management, transect, 
                 unique.id, depth, FG, transect.area,
                         hard.coral, macroalgae, rubble, substrate, complexity, fish.biom) %>%
          summarise(cropping = sum(cropping), biom=sum(biomass.kgha), abund = sum(abundance.500m2)) %>%
  ## mean cropping across transects at each site
          group_by(dataset, date, reef, site, management, unique.id, depth, FG, transect.area,
                         hard.coral, macroalgae,  rubble, substrate, complexity, fish.biom) %>%
          summarise(cropping.gram.ha = mean(cropping), biom=mean(biom), abund = mean(abund)) 

## correct cropping to per hectare
## for surveys = 100m2, which is when abundance.500m2 = 5
h$cropping.gram.ha[which(h$transect.area == 100)] <- h$cropping.gram.ha[which(h$transect.area == 100)]/0.01 
## for surveys = 250m2, which is when abundance.500m2 = 2
h$cropping.gram.ha[which(h$transect.area == 250)] <- h$cropping.gram.ha[which(h$transect.area == 250)]/0.025 
## for surveys = 153.9m2, which is when abundance.500m2 = 3.24806 (Seychelles)
h$cropping.gram.ha[is.na(h$transect.area)] <- h$cropping.gram.ha[is.na(h$transect.area)]/0.01539 

## sum within unique.id to account for different transect areas in Maldives
h <- h %>% group_by(dataset, date, reef, site, management, unique.id, depth, FG,
                         hard.coral, macroalgae,  rubble, substrate, complexity, fish.biom) %>%
          summarise(cropping.gram.ha = sum(cropping.gram.ha), biom=sum(biom), abund = sum(abund)) 

save(h, file = 'results/cropper_function.Rdata')


#### Scraping functions = potential area grazed
## subset UVC data for scrapers
scrapers.uvc<-pred %>% filter(FG == 'Herbivore Scraper')
scrapers.uvc$species<-as.factor(scrapers.uvc$sp)
## add genera column
scrapers.uvc$genus<-str_split_fixed(scrapers.uvc$species, pattern = '\\ ', n=2)[,1]
scrapers.uvc$genus<-as.factor(scrapers.uvc$genus)

## predictive model load for scraping bite rates
## sourced from bite_rate_models.R

## generate bite predictions for each species
d.pred <-data.frame(sp = scrapers.uvc$species, Genus = scrapers.uvc$genus, TL=scrapers.uvc$length.cm, dataset='NA')
## subset for species in bite rate obs
d.pred <- d.pred[d.pred$sp %in% scrapers$sp,]; d.pred<-droplevels(d.pred)
a.dataset.zero = matrix(0, 1000, 3)
link.obs.sp<-link(scrape.m, n = 1000, data = as.list(d.pred), replace=list(X3 = a.dataset.zero))
scrapers.uvc$biterate[scrapers.uvc$sp %in% scrapers$sp]<-apply(link.obs.sp, 2, median)

## uncertainty
pred.PI<-apply(link.obs.sp, 2, PI, prob = 0.95)
scrapers.uvc$upper[scrapers.uvc$sp %in% scrapers$sp]<-pred.PI[2,]
scrapers.uvc$lower[scrapers.uvc$sp %in% scrapers$sp]<-pred.PI[1,]

## how much biomass and abunance, and how many species, require genus level predictions?
sum(scrapers.uvc$biomass.kgha[is.na(scrapers.uvc$biterate)])/sum(scrapers.uvc$biomass.kgha)*100 ### 19%
sum(scrapers.uvc$abundance.500m2[is.na(scrapers.uvc$biterate)])/sum(scrapers.uvc$abundance.500m2)*100 ### 21%

## assign genus for missing species, sourced from bite_rate_models.R
scrapers.uvc$biterate[is.na(scrapers.uvc$biterate)]<-scraper.bites$median[match(scrapers.uvc$genus[is.na(scrapers.uvc$biterate)], scraper.bites$class)]
  

## now add area predictions for body size
load(file = 'results/models/scraper_bite_area_model.Rdata')
d.pred <-data.frame(TL=scrapers.uvc$length.cm)
link.obs.sp<-link(scrape.m2, n = 1000, data = as.list(d.pred))
scrapers.uvc$bitearea<-data.frame(median = apply(link.obs.sp, 2, median))
pred.PI<-apply(link.obs.sp, 2, PI, prob = 0.95)
scrapers.uvc$upperarea<-pred.PI[2,]
scrapers.uvc$lowerarea<-pred.PI[1,]

## what proportion biomass is global means?
newsp<-unique(scrapers.uvc$species)[!unique(scrapers.uvc$species) %in% scraper.bites$class] ## new species

## now convert bite rate to area scraped
scrapers.uvc$scraping<-scrapers.uvc$biterate * scrapers.uvc$bitearea / 1000000

# estimate mean total scraping function per site
h <- scrapers.uvc %>% 
  ## sum scraping in each transect
        group_by(dataset, date, reef, site, management, transect, 
                 unique.id, depth, FG, transect.area,
                         hard.coral, macroalgae, rubble, substrate, complexity, fish.biom) %>%
          summarise(scraping = sum(scraping), biom=sum(biomass.kgha), abund = sum(abundance.500m2)) %>%
  ## mean scraping across transects at each site
          group_by(dataset, date, reef, site, management, unique.id, depth, FG, transect.area,
                         hard.coral, macroalgae,  rubble, substrate, complexity, fish.biom) %>%
          summarise(scraping = mean(scraping), biom=mean(biom), abund = mean(abund)) 

## correct scraping to per hectare
## for surveys = 100m2, which is when abundance.500m2 = 5
h$scraping[which(h$transect.area == 100)] <- h$scraping[which(h$transect.area == 100)]/0.01 
## for surveys = 250m2, which is when abundance.500m2 = 2
h$scraping[which(h$transect.area == 250)] <- h$scraping[which(h$transect.area == 250)]/0.025 
## for surveys = 153.9m2, which is when abundance.500m2 = 3.24806 (Seychelles)
h$scraping[is.na(h$transect.area)] <- h$scraping[is.na(h$transect.area)]/0.01539 

## sum within unique.id to account for different transect areas in Maldives
h <- h %>% group_by(dataset, date, reef, site, management, unique.id, depth, FG,
                         hard.coral, macroalgae,  rubble, substrate, complexity, fish.biom) %>%
          summarise(scraping = sum(scraping), biom=sum(biom), abund = mean(abund)) 

## drop annoying dplyr things
scrapers.uvc$bitearea<-scrapers.uvc$bitearea[,1]
scrapers.uvc$scraping<-scrapers.uvc$scraping[,1]
save(scrapers.uvc, h, file = 'results/scraper_function.Rdata')
