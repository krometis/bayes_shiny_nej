# R Shiny app for Bayesian approach to integrated testing
This repository contains the R Shiny app for Bayesian approach to integrated testing described in the Naval Engineers Journal article "Using Data and Analytics to Build Confidence in Systems Operational Performance in the Face of Limited Resources" by Kyle Risher, Daniel Wolodkin, Justin Krometis, Victoria Sieck, Laura Freeman, Jeremy Werner, and Sandra Hobson.

## Instructions
1. Download and install R and [RStudio](https://posit.co/download/rstudio-desktop/) for free online
2. Open RStudio
3. Install the packages required for the functionality of the app by running the following command in the RStudio console:
```{r}
install.packages(c("shiny", "shinythemes", "grid", "markdown", "tidyr", "dplyr", "reshape2", "fitdistrplus", "rstan", "ggplot2", "gridExtra", "cowplot", "colorspace"))
```
4. Open the `ReliabilityApp.R` file
5. Click "Run App" to launch the web application

Note that the input file `SimTorpedo_10.csv` used in the paper is provided as part of the repository and can be uploaded on the "Dataset" tab of the app.
