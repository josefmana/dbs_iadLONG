# This script extracts descriptions (numerical & visual). It ought to be run only after 00_import.R!

# Inputs: pre-formatted longitudinal data (d0), scoring file (sc)
# Outputs:

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse", "ggplot2" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works only in RStudio)
setwd( dirname(getSourceEditorContext()$path) )

# prepare a folder for sessions info, tables and figures
sapply( c("sess","tabs","figs") , function(i) if( !dir.exists(i) ) dir.create(i) )

# prepare colorblind palette
#cbPal <- c( "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7" )

# set theme for plotting in ggplot2
theme_set( theme_minimal(base_size = 14) )

# read the input files
d0 <- read.csv( "_nogithub/data/data_stn_bil.csv", sep = "\t" )
sc <- read.csv( "_nogithub/raw/scoring.csv", sep = ";" )


# ---- prepare a trimmed data set ----

# transform reverse-coded scores and calculate sum scores
for ( i in sc$scale ) {
  
  # if there are reverse items, flip them
  if ( !is.na( with( sc, rev[scale==i] ) ) ) {
    rev <- paste0( i,"_",unlist( strsplit( sc[sc$scale==i,"rev"], "," ) ) ) # extract items to be reverse coded
    for ( j in rev  ) d0[ , j ] <- sc[sc$scale==i,"max"] + 1 - d0[ , j ] # loop through reverse coded items
    rm(rev)
  }
  
  # calculate sum scores
  d0[ , i ] <- rowSums( d0[ , paste0(i, "_", unlist( strsplit( sc[sc$scale==i,"it"], "," ) ) ) ] )
  
}

# extract only relevant columns
d1 <- d0[ , c(1:9,19:20,25,237:242,117:118,123,185:186,191:194,215:216,221:236) ]


# ---- visualise presence/absence of single observations ----

# begin by visualising FAQ data
d1 %>%
  # pre-process the table
  filter( faq_patient %in% c(1,NA) ) %>% # keep only patientss self-report
  filter( !event %in% c("refresh","operace") ) %>% # visualise neither refresh nor surgery
  select( id, event, surg_to_psycho, faq ) %>% # trim the table
  mutate( faq_present = ifelse( is.na(faq), F, T ),
          time = ifelse( faq_present, round(surg_to_psycho,2), NA ),
          event = factor( event, levels = c( "screening", paste0( "r", seq(1,19,2) ) ), ordered = T )
          ) %>%
  # plotting proper
  ggplot( aes( y = event, x = reorder(id,faq_present), fill = faq_present, label = time ) ) +
  geom_tile( color = "white" ) +
  geom_text( color = "white", angle = 90 ) +
  scale_fill_manual( values = c("grey81", "#0072B2") ) +
  labs( y = NULL, x = NULL ) +
  theme( legend.position = "bottom", axis.text.x = element_text( angle = 90, vjust = 0.5 ) )

# save it
ggsave( "figs/faq_present.jpg", dpi = 300, width = 2.5 * 13.1, height = 8.94 )


# ---- session info ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/desc.txt" )

