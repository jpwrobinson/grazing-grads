# grazing-grads
This repository contains data and R scripts accompanying Robinson et al. Habitat and fishing control grazing potential on coral reefs. (In Press, Functional Ecology)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3415041.svg)](https://doi.org/10.5281/zenodo.3415041)

R packages used to tidy, analyse and visualize data are given in [R_session_info](R_session_info.txt).

The manuscript can be reproduced by knitting [robinson_reproducible.Rmd](robinson_reproducible.Rmd), which regenerates model predictions and embeds figures and text into a PDF.

*Folders contain:*

**data**

- [bite_rates.csv](data/bite_rates.csv) contains feeding observations
- [cropper_attributes.Rdata](data/cropper_attributes.Rdata) contains average cropping, richness and size of each cropping species
- [predicted_bite_rates.Rdata](data/predicted_bite_rates.Rdata) contains the species and genus predictions for bite rates of croppers (`crop.bites`) and scrapers (`scrap.bites`)
- [scrape_sizes.csv](data/scrape_sizes.csv) contains scraped area observations
- [scraper_attributes.Rdata](data/scraper_attributes.Rdata) contains average scraping, richness and size of each scraping species
- [UVC_grazing_rates.Rdata](data/UVC_grazing_rates.Rdata) contains the site-level UVC datasets, with explanatory benthic and fishing covariates, and grazing rates for croppers (`crop`) and scrapers (`scrap`)
- [UVC_WIO_herb_benthic.Rdata](data/UVC_WIO_herb_benthic.Rdata) contains transect-level UVC observations

**scripts**

- [01_bite_rate_models.R](scripts/01_bite_rate_models.R) fits Bayesian models to feeding observation datasets to estimate grazing rates for croppers and scrapers
- [02_convert_biom_to_bites.R](scripts/02_convert_biom_to_bites.R) converts cropper and scraper biomass to grazing rates
- [03_MMI_tavlue_analysis.R](scripts/03_MMI_tavlue_analysis.R) runs multi-model inference to examine drivers of grazing rates for croppers and scrapers
- [MMI_tvalue_func.R](scripts/MMI_tvalue_func.R) is `mmi_tvalue`, a function for AIC-based multimodel inference, which standardizes t-values and model predictions across a top-model set derived from a global model
- [MMI_tvalue_analysis.R]([scripts/MMI_tvalue_analysis.R]) applies MMI_tvalue_func.R to grazing rates derived from UVC datasets
- [scaling_function.R](scripts/scaling_function.R) is `scaler`, a function to scale and centre covariates before statistical modelling
