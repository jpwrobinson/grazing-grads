
library(here)
library(piecewiseSEM)
require(gridExtra)
library(grid)
library(lme4)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(funk)
library(scales)

## load t values
load(file = 'results/tvalues_croppers.Rdata'); crop<-mm.crop[[1]]
load(file = 'results/tvalues_scrapers.Rdata'); scrape<-mm.scrape[[1]]

est<-rbind(crop, scrape)

## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##
          ## ------------ NOW PLOTTING FIGURES -------------- ## 
## -------------- ## ## -------------- ## ## -------------- ## ## -------------- ##

## setup formatting information

linewidth = 2
pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")
cols<-c(pal[5], pal[12], pal[18])
cols.named<-c('Croppers' = pal[5], 'Scrapers' = pal[12])
theme_set(theme_sleek())
ylab<-rev(c('Hard coral', 'Available\nsubstrate', 'Rubble', 'Macroalgae', 'Structural\ncomplexity',
        'Fishable\nbiomass', 'No-take reef', 'Remote reef'))#,'Species richness'))

## reorder factor levels here - careful this is manual, check plot is ok
## for t value plot
est$Var<-factor(est$Var)

est$Var<-factor(est$Var, levels=levels(est$Var)[rev(c(5,8,7,6,3,4,1,2))])
est$indicator<-ifelse(est$indicator == 'cropping.gram.ha', 'Croppers', 'Scrapers')
est$sign<-'Positive'
est$sign[est$Var == 'hard.coral' & est$indicator == 'Croppers']<-'Mixed'
est$sign[est$Var == 'macroalgae' & est$indicator == 'Croppers']<-'Negative'
est$sign[est$Var == 'hard.coral' & est$indicator == 'Scrapers']<-'Mixed'
est$sign[est$Var == 'macroalgae' & est$indicator == 'Scrapers']<-'Mixed'
est$sign[est$Var == 'fish.biom' & est$indicator == 'Scrapers']<-'Mixed'

cols.named2<-c('Positive' = '#67a9cf', 'Mixed' = '#999999', 'Negative' = '#ef8a62')


g.rel.effects <- ggplot(est, aes(Var, RI.t.ratio, fill=indicator, col=indicator)) + 
              geom_hline(yintercept=0, linetype='dashed') +
              geom_bar(stat='identity', size=1, position = position_dodge(width=0.4)) +
              scale_color_manual(values = cols.named) +
              scale_fill_manual(values = cols.named) +
              facet_wrap(~ indicator) +
              guides(shape = FALSE) +
              labs(x='', y = 'Relative effect size') +
              scale_y_continuous(breaks=seq(0, 1, 0.25), labels=c(0, 0.25, 0.5, 0.75, 1)) +
              scale_x_discrete(labels = ylab) + coord_flip() +
              theme(legend.position = 'none',
                legend.title=element_blank(),
                axis.text=element_text(size=14),
                axis.title.x=element_text(size=18),
                strip.text.x=element_text(color = 'black', size=18)) + 
              geom_vline(xintercept = 3.5, size=2, col='grey90') 

## output for Rmd
print(g.rel.effects)

# ## save 
# pdf(file = "figures/Figure2.pdf", width=12, height=6)
# g.rel.effects
# dev.off()
