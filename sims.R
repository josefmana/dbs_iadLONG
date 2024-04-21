# This script simulates motor and cognitive ADL, measure it via 10 Likert items, and tests which statistical procedure
# can identify them correctly.

# clear environment
rm( list = ls() )

# list required packages into a character object
pkgs <- c("here","tidyverse","MASS")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare folders for preprocess data, models, tables and figures
sapply( c("_data","mods","tabs","figs"), function(i) if( !dir.exists(i) ) dir.create(i) )
