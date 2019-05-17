
library(here)
setwd(here('grazing-grads'))
library(rethinking)

#### Grazer model


# Grazer function is cropping/consumption of turf algae. 
# We infer this function from the total bite rate of the grazer community, and assume all bites are the same size. 
# Thus, the predictive model estimates bite rates by species and families. 
#  We can use a hierarchical Bayesian model to predict new levels (i.e. species), based on their genus.

grazers<-droplevels(bite[bite$FG == 'Herbivore Grazer',])
colnames(grazers)[colnames(grazers) == 'Bite.rate']<-'biterate'
bite.prior=mean(grazers$biterate) ## 30.85, log = 3.43

graze.m<-map2stan(
        alist(
          biterate ~ dgamma2(mu, scale),
          log(mu) ~ a + X1[sp] + X2[Genus] + X3[dataset], 
          X1[sp] ~ dnorm(0, sigmar),
          X2[Genus] ~ dnorm(0, sigmar2),
          X3[dataset] ~ dnorm(0, sigmar3),
          a ~ dnorm(3.43, 10),
          scale ~ dexp(2),
          c(sigmar, sigmar2, sigmar3) ~ dcauchy(0, 1)
        ),
        data=grazers, iter = 3000, chains =1, cores = 4)

## Predictions for species, with genus effect
d.pred <-data.frame(sp = unique(grazers$sp), dataset = 'NA')
d.pred$Genus<-grazers$Genus[match(d.pred$sp, grazers$sp)]
a.dataset.zero = matrix(0, 1000, 3)

link.obs.sp<-link(graze.m, n = 1000, data = as.list(d.pred), replace = list(X3 = a.dataset.zero))
pred.mean<-data.frame(median = apply(link.obs.sp, 2, median))
pred.PI<-apply(link.obs.sp, 2, PI, prob = 0.95)
pred.mean$upper<-pred.PI[2,]
pred.mean$lower<-pred.PI[1,]
pred.mean$class <- d.pred$sp #pred.mean$Genus <- d.pred$Genus
pred.mean$class<- factor(pred.mean$class, level = rev(levels(pred.mean$class)))
pred.mean$preds<-'Species'

## Predictions for genus, removing species effect
d.pred <- data.frame(Genus = unique(grazers$Genus), sp = 'NA', dataset = 'NA')
a.sp.zero = matrix(0, 1000, 9)
a.dataset.zero = matrix(0, 1000, 3)

link.obs.genus<-link(graze.m, n = 1000, data = as.list(d.pred), replace= list(X1= a.sp.zero, X3 = a.dataset.zero))
pred.mean.genus<-data.frame(median = apply(link.obs.genus, 2, median))
pred.PI<-apply(link.obs.genus, 2, PI, prob = 0.95)
pred.mean.genus$upper<-pred.PI[2,]
pred.mean.genus$lower<-pred.PI[1,]
pred.mean.genus$class <- d.pred$Genus
pred.mean.genus$class<- factor(pred.mean.genus$class, level = rev(levels(pred.mean.genus$class)))
pred.mean.genus$preds<-'Genus'

## Predictions for global mean, removing species and genus effect
d.pred <- data.frame(Genus = 'NA', sp = 'NA', dataset = 'NA')
a.sp.zero = matrix(0, 1000, 9)
a.genus.zero = matrix(0, 1000, 5)
a.dataset.zero = matrix(0, 1000, 3)

link.obs.mean<-link(graze.m, n = 1000, data = as.list(d.pred), replace= list(X1= a.sp.zero, X2 = a.genus.zero, X3 = a.dataset.zero))
pred.global<-data.frame(median = apply(link.obs.mean, 2, median))
pred.PI<-apply(link.obs.mean, 2, PI, prob = 0.95)
pred.global$upper<-pred.PI[2,]
pred.global$lower<-pred.PI[1,]
pred.global$class <- 'global.mean'
pred.global$preds<-'global.mean'

p<-rbind(pred.mean, pred.mean.genus, pred.global)


#### Scraper model

# Scraper function is removal of dead coral substrate, endolithic material, and detritus. 
# We infer this function from the total bite rate of the scraper community, and accounting for changes in bite scar volume with size. 
# Thus, the predictive model estimates bite rates by species, genera and size. 
# We can use a hierarchical Bayesian model to predict new levels (i.e. species), based on their genus.

scrapers<-droplevels(bite[bite$FG == 'Herbivore Scraper',])
colnames(scrapers)[colnames(scrapers) == 'Bite.rate']<-'biterate'
bite.prior=mean(scrapers$biterate) ## 22.15, log = 3.10

scrape.m<-map2stan(
        alist(
          biterate ~ dgamma2(mu, scale),
          log(mu) ~ a + B*TL + X1[sp] + X2[Genus] + X3[dataset], 
          X1[sp] ~ dnorm(0, sigmar),
          X2[Genus] ~ dnorm(0, sigmar2),
          X3[dataset] ~ dnorm(0, sigmar3),
          a ~ dnorm(3.10, 10),
          B ~ dnorm(0, 5),
          scale ~ dexp(1),
          c(sigmar, sigmar2, sigmar3) ~ dcauchy(0, 1)
        ),
        data=scrapers, warmup = 1500, iter = 5000, chains =1, cores = 4)

## now model for area scraped
area<-read.csv('scrape_sizes.csv')
area.prior=mean(area$bitearea) ## 85.67, log = 4.45

scrape.m2<-map2stan(
        alist(
          bitearea ~ dgamma2(mu, scale),
          log(mu) ~ a + B*TL, #+ X1[species] + X2[genus],
          #X1[species] ~ dnorm(0, sigmar),
          #X2[genus] ~ dnorm(0, sigmar2),
          a ~ dnorm(4.45, 5),
          B ~ dnorm(0, 2),
          scale ~ dexp(5)
          #c(sigmar, sigmar2) ~ dcauchy(0, 1)
        ),
        data=area, warmup = 1500, iter = 5000, chains =1, cores = 4)


## evaluating predictions for each species

## Predictions for species, with genus effect
d.pred <-data.frame(sp = unique(scrapers$sp), TL = mean(scrapers$TL), dataset = 'NA')
d.pred$Genus<-scrapers$Genus[match(d.pred$sp, scrapers$sp)]
a.dataset.zero = matrix(0, 1000, 3)

link.obs.sp<-link(scrape.m, n = 1000, data = as.list(d.pred), replace=list(X3 = a.dataset.zero))
pred.mean<-data.frame(median = apply(link.obs.sp, 2, median))
pred.PI<-apply(link.obs.sp, 2, PI, prob = 0.95)
pred.mean$upper<-pred.PI[2,]
pred.mean$lower<-pred.PI[1,]
pred.mean$class <- d.pred$sp #pred.mean$Genus <- d.pred$Genus
pred.mean$class<- factor(pred.mean$class, level = rev(levels(pred.mean$class)))
pred.mean$preds<-'Species'

## Predictions for genus, removing species effect
d.pred <- data.frame(Genus = unique(scrapers$Genus), sp = 'NA', TL = mean(scrapers$TL), dataset = 'NA')
a.sp.zero = matrix(0, 1000, 27)
link.obs.genus<-link(scrape.m, n = 1000, data = as.list(d.pred), replace= list(X1= a.sp.zero, X3 = a.dataset.zero))
pred.mean.genus<-data.frame(median = apply(link.obs.genus, 2, median))
pred.PI<-apply(link.obs.genus, 2, PI, prob = 0.95)
pred.mean.genus$upper<-pred.PI[2,]
pred.mean.genus$lower<-pred.PI[1,]
pred.mean.genus$class <- d.pred$Genus
pred.mean.genus$class<- factor(pred.mean.genus$class, level = rev(levels(pred.mean.genus$class)))
pred.mean.genus$preds<-'Genus'

## Predictions for global mean, removing species and genus effect
d.pred <- data.frame(Genus = 'NA', sp = 'NA', dataset = 'NA',TL = mean(scrapers$TL))
a.sp.zero = matrix(0, 1000, 27)
a.genus.zero = matrix(0, 1000, 4)
a.dataset.zero = matrix(0, 1000, 3)

link.obs.mean<-link(scrape.m, n = 1000, data = as.list(d.pred), replace= list(X1= a.sp.zero, X2 = a.genus.zero, X3 = a.dataset.zero))
pred.global<-data.frame(median = apply(link.obs.mean, 2, median))
pred.PI<-apply(link.obs.mean, 2, PI, prob = 0.95)
pred.global$upper<-pred.PI[2,]
pred.global$lower<-pred.PI[1,]
pred.global$class <- 'global.mean'
pred.global$preds<-'global.mean'

p<-rbind(pred.mean, pred.mean.genus, pred.global)


## evaluating bite area predictions for each species
d.pred <- data.frame(TL = seq(min(area$TL), max(area$TL), length.out =30))
link.obs.size<-link(scrape.m2, n = 1000, data = as.list(d.pred))
pred.mean.size<-data.frame(median = apply(link.obs.size, 2, median))
pred.PI<-apply(link.obs.size, 2, PI, prob = 0.95)
pred.mean.size$upper<-pred.PI[2,]
pred.mean.size$lower<-pred.PI[1,]
pred.mean.size$TL<-d.pred$TL

