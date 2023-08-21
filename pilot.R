# This script computes and summarises GLMMs for a pilot study aimed at describing long-term changes in IADL after STN-DBS in PD.
# It ought to be run only after 00_import.R!

# Inputs: pre-formatted longitudinal data (d0), scoring file (sc)
# Outputs: time contrasts derived from an appropriate GLMM

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse", "ggplot2", "rstanarm", "bayesplot" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works only in RStudio)
setwd( dirname(getSourceEditorContext()$path) )

# prepare a folder for sessions info, models, tables and figures
sapply( c("sess","mods","tabs","figs") , function(i) if( !dir.exists(i) ) dir.create(i) )

# prepare colorblind palette
cbPal <- c( "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7" )

# set theme for plotting in ggplot2
theme_set( theme_minimal(base_size = 14) )

# read the input files
d0 <- read.csv( "_nogithub/data/data_stn_bil.csv", sep = "\t" )
sc <- read.csv( "_nogithub/raw/scoring.csv", sep = ";" )