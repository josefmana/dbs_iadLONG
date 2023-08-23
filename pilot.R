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
sapply( paste0( c("mods","tabs","figs"), "/pilot" ), function(i) if( !dir.exists(i) ) dir.create(i) )

# set theme for plotting in ggplot2
theme_set( bayesplot::theme_default( base_size = 18 ) )

# set-up Stan options
mcc = 8 # all CPU cores
ch = 4 # four chains
it = 1500 # total iterations per chain
wu = 500 # warm-um iterations per chain
ad = .999 # adapt_delta parameter
s = 87542 # seed for reproducibility

# read the input files
d0 <- read.csv( "_nogithub/data/data_stn_bil.csv", sep = "\t" )
sc <- read.csv( "_nogithub/raw/scoring.csv", sep = ";" )


# ---- prepare functions ----

# sanity plot showing distribution of observations across patients and events
sanity_plot <- function( data = d1, output = "figs/pilot/sanity_precheck.jpg" ) {
  
  # plot it
  data %>%
    # pre-process the table
    mutate( faq_present = ifelse( is.na(faq), F, T ), time = ifelse( faq_present, round(surg_to_psycho,2), NA ) ) %>%
    # plotting proper
    ggplot( aes( y = event, x = reorder(id,faq_present), fill = faq_present, label = time ) ) +
    # add data presence indicators and times from surgery
    geom_tile( color = "white" ) + geom_text( color = "black", angle = 90 ) +
    # format
    scale_fill_manual( values = "grey81" ) + labs( y = NULL, x = NULL ) +
    theme( legend.position = "none", axis.text.x = element_text( angle = 90, vjust = 0.5 ) )
  
  # save it
  ggsave( output, dpi = 300, width = 2 * 13.1, height = 8.94 )
  
}


# ---- preprocess data ----

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

# drop rows
d1 <- d1 %>%
  # pre-process the table
  filter( faq_patient %in% c(1,NA) ) %>% # keep only patients self-report
  filter( !event %in% c( "refresh", "operace", paste0( "r",seq(7,19,2) ) ) ) %>% # drop surgery, refresh and re-tests further than five years post-surgery
  filter( id %in% d1[ with( d1, event == "screening" & !is.na(faq) ) , "id" ] ) %>% # keep only patients with FAQ screening measured
  filter( complete.cases(faq) ) %>% # keep only observations with FAQ
  # re-format event variable to an ordered one
  mutate( event = factor( event, levels = c( "screening", paste0( "r", seq(1,5,2) ) ), ordered = T ) )

# keep only patients with at least one post-test (in addition to a pre-test)
d1 <- d1[ with( d1, id %in% names( which( table(id) != 1 ) ) ) , ]

# plot FAQ-presence summaries for sanity check
sanity_plot( data = d1, output = "figs/pilot/sanity_precheck.jpg" )

# weird cases:
# IPN157, r5, seems all-right according to REDCap
# IPN247, r3, seems all-right according to REDCap
# IPN283, r3, non-scheduled at 2 year mark, will keep the data point though (and will keep an eye on it as well)
# IPN584, r3, twice for some reason, dropping this one
# IPN243, r1, closer to r3 so re-coding (r1 -> r3)

# re-code/drop cases identified above
d1 <- d1[ with(d1, !(id == "IPN584" & event == "r3") ) , ]
d1[ with( d1, id == "IPN243" & event == "r1"), "event" ] <- "r3"


# ---- describe data ----

# list columns to be described (continuous cvar and nominal nvar)
cvar <- c("edu_years","hy_stage","age_surg","age_psycho","surg_to_psycho","faq","pdaq","drs","bdi","staix1","staix2","age_medic","surg_to_medic","ledd")
nvar <- c("sex","edu_type","type_pd","asym_park")

# extract M (SD) and Md (IQR) descriptions
dtab <- lapply(
  # loop through measures of central tendency and variability measures
  setNames( c("mean_sd","median_IQR"), c("mean_sd","median_IQR") ),
  function(i) sapply(
    # loop through longitudinal events
    levels(d1$event),
    function(j) sapply(
      # loop through continuous variables
      cvar,
      function(k)
        # export results in format 'central tendency (variability)'
        paste0(
          do.call( sub( "_.*", "", i ), list( x = d1[ d1$event == j, k ], na.rm = T ) ) %>% round(2) %>% sprintf( "%.2f", . ), " (",
          do.call( sub( ".*_", "", i ), list( x = d1[ d1$event == j, k ], na.rm = T ) ) %>% round(2) %>% sprintf( "%.2f", . ), ")"
        )
    )
  )
)

# add ratio of females/males and akinetic-rigid/tremordominanat patients at each event
for( i in names(dtab) ) dtab[[i]] <- rbind.data.frame(
  # add nominal variables frequencies at the top of the tables
  sapply( levels(d1$event), function(j)
    sapply( nvar[c(1,3)], function(k)
      table( d1[ d1$event == j , k ] ) %>% paste( collapse = "/" )
      )
    ),
  dtab[[i]]
) %>%
  # add a column denoting variables
  rownames_to_column( "variable" )

# save the descriptive tables as .csv
for ( i in names(dtab) ) write.table( dtab[[i]], paste0("tabs/pilot/desc_",i,".csv"), sep = ",", row.names = F, quote = F )

# plot FAQ-presence summaries after dealing with 'problematic' cases
sanity_plot( data = d1, output = "figs/pilot/sanity_postcheck.jpg" )


# ---- statistical modelling ----

# prepare a data set for Bayesian hierarchical modelling
# will fit two Gaussian LMMs using (i) raw FAQ, (ii) log-transformed FAQ (+1)
# adding one before the log-transformation is not really well thought through and should be double checked to see
# if it biases the results; nevertheless, adding one lead to and interpretable point 0 in the transformed data ( lfaq = 0 <=> faq = 0)
# still, using a mixtureor shifted lognormal likelihood may be better (even better, do an item-level analysis instead)
d <- with( d1, data.frame( faq = faq, lfaq = log(faq+1), event = event, id = factor(id) ) )

# start by fitting a raw Gaussian LMM
m.raw <- stan_lmer( faq ~ 1 + (1 | event) + (1 | id), data = d, chains = ch, iter = it, warmup = wu, cores = mcc, adapt_delta = ad, seed = s )
m.log <- update( m.raw, formula = lfaq ~ 1 + (1 | event) + (1 | id) )


# NEXT STEPS:
# re-estimate models via brms:
# 1) find an appropiate likelihood/link combo (hurdle lognormal w/ identity link?)
# 2) fit separately
# 2.1) unpooled categorical events model
# 2.2) partially pooled categorical events model
# 2.3) monotonic (ordinal) events model
# 3) compare and contrast
# 4) summarise
