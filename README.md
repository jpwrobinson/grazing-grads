# grazing-grads
This repository contains data and R scripts accompanying Robinson et al. Habitat and fishing control grazing potential on coral reefs. (In Press, Functional Ecology)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3415041.svg)](https://doi.org/10.5281/zenodo.3415041)

R packages used to tidy, analyse and visualize data are given in [R_session_info](R_session_info.txt).

The manuscript can be reproduced by knitting [robinson_reproducible.Rmd](robinson_reproducible.Rmd), which regenerates model predictions and embeds figures and text into a PDF.

*Folders contain:*

**data**

- [predicted_bite_rates.Rdata](data/predicted_bite_rates.Rdata) contains the species and genus predictions for bite rates of croppers (`crop.bites`) and scrapers (`scrap.bites`)
- [UVC_grazing_rates.Rdata](data/UVC_grazing_rates.Rdata) contains the UVC datasets analysed, with explanatory benthic and fishing covariates, and grazing rates for croppers (`crop`) and scrapers (`scrap`)

**scripts**

- [bite_rate_models.R](scripts/bite_rate_models.R) fits Bayesian models to feeding observation datasets to estimate grazing rates for croppers and scrapers
- [MMI_tvalue_func.R](scripts/MMI_tvalue_func.R) is `mmi_tvalue`, a function for AIC-based multimodel inference, which standardizes t-values and model predictions across a top-model set derived from a global model
- [MMI_tvalue_analysis.R]([scripts/MMI_tvalue_analysis.R]) applies MMI_tvalue_func.R to grazing rates derived from UVC datasets
- [scaling_function.R](scripts/scaling_function.R) is `scaler`, a function to scale and centre covariates before statistical modelling
