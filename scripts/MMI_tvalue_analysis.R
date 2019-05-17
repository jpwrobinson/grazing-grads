
library(here)
setwd(here('grazing-grads'))

library(lme4)
library(tidyverse)
library(cowplot)

source('scripts/MMI_tvalue_func.R')
source('scripts/scaling_function.R')

## Script fits multiple models to cropper/scraper functions, measures t-values and weighted model preds

## croppers
load(file = 'cropper_function.Rdata')

## center explanatory covariates
h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'cropping.gram.ha'))

## fit global model
m.full<-glmer(cropping.gram.ha ~  hard.coral + macroalgae + rubble + substrate + complexity + 
        	fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + 
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action = na.fail)

## estimated weighted-t-values and predictions
mm.crop<-mmi_tvalue(m.full, dataset=h.pred, t.subset=TRUE, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy'), 
		 ranef = c('dataset', 'reef'), indicator = 'cropping.gram.ha', family = 'Gamma')


## scrapers
load(file = 'results/models/scraper_function.Rdata')

## center explanatory covariates
h.pred<-scaler(h, ID=c('date', 'dataset', 'reef', 'site', 'transect', 'unique.id', 'scraping'))

## fit global model
m.full<-glmer(scraping ~ hard.coral + macroalgae + rubble + substrate + complexity + 
        	fish.biom + Fished.Protected.dummy + Fished.Unfished.dummy  + #biom +
          (1 | dataset/reef) , ## random, nested = reefs within datasets
                data = h.pred, family='Gamma'(link='log'), na.action = na.fail)

## estimated weighted-t-values and predictions
mm.scrape<-mmi_tvalue(m.full, dataset=h.pred, exp.names = c('hard.coral', 'macroalgae', 'rubble', 'substrate', 'complexity', 
          'fish.biom', 'Fished.Protected.dummy', 'Fished.Unfished.dummy'), 
		 ranef = c('dataset', 'reef'), indicator = 'scraping', family = 'Gamma')

