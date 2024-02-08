#load libraries for shiny
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(shinythemes)))
suppressMessages(suppressWarnings(library(grid)))
suppressMessages(suppressWarnings(library(markdown)))

#script to load necessary libraries and scripts to run the stryker analysis

#data manipulation
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(reshape2)))

#fitting distributions
suppressMessages(suppressWarnings(library(fitdistrplus)))

#bayesian
suppressMessages(suppressWarnings(library(rstan)))

#plotting
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(gridExtra))) #plot arrangement
suppressMessages(suppressWarnings(library(cowplot)))
suppressMessages(suppressWarnings(library(colorspace)))

#load custom functions
#source("lib/general_utility_functions.R")
#source("lib/stryker_functions.R")          #Stryker analysis from R script file
source("lib/DOTE_plotting_functions.R")    #DOT&E plotting specs
#source("shiny_functions.R")
source("lib/reliability_functions.R")

#set the default ggplot2 theme to DOT&E specifications
theme_set(DOTE_theme) 

#set options for quicker rstan computations
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
