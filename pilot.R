# This script computes and summarises GLMMs for a pilot study aimed at describing long-term changes in IADL after STN-DBS in PD.
# It ought to be run only after 00_import.R!

# Inputs: pre-formatted longitudinal data (d0), scoring file (sc)
# Outputs: time contrasts derived from an appropriate GLMM

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse", "ggplot2", "brms", "tidybayes" )

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
theme_set( theme_default( base_size = 18 ) )

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
# will fit three (hurdle) lognormal GLMMs using events as: (i) 'fixed effects', (ii) 'random effects' and (iii) monotonic effects
d <- with( d1, data.frame( id = factor(id), event = event, faq = faq ) )

# set-up dummy coding for event unpooled effects via indicator variables
contrasts(d$event) <- contr.treatment( length( levels(d$event) ) )

# representation: set-up linear models
f <- list( fix = bf( faq ~ 1 + event + (1 | id) ), # events unpooled (with 'fixed' priors)
           ran = bf( faq ~ 1 + (1 | event) + (1 | id) ), # paritally pooled (statistically) independent events ('random effects')
           mon = bf( faq ~ 1 + mo(event) + (1 | id) ) # events modelled as a monotonic ordinal predictor
           )

# representation: set-up priors
p <- list(
  # set-up such that there is almost zero change to get scores higher than log(30) (Intercept, b),
  # with expected inter-patient and residual variability about 10 FAQ points (log(10) ~ 1/0.44) (which is too high but will not break the model) (sd, sigma),
  # and default brms priors for hurdle mixture parameter (hu)
  fix = c( prior( normal(1.6,0.6), class = Intercept ), prior( beta(1,1), class = hu ), prior( exponential(0.44), class = sd ), prior( exponential(0.44), class = sigma ), prior( normal(0,1), class = b ) ),
  ran = c( prior( normal(1.6,0.6), class = Intercept ), prior( beta(1,1), class = hu ), prior( exponential(0.44), class = sd ), prior( exponential(0.44), class = sigma ) ),
  mon = c( prior( normal(1.6,0.6), class = Intercept ), prior( beta(1,1), class = hu ), prior( exponential(0.44), class = sd ), prior( exponential(0.44), class = sigma ), prior( normal(0,1), class = b ), prior( dirichlet(1), class = simo, coef = moevent1 ) )
)

# implementation: let the models learn from data
m <- lapply( setNames( names(f), names(f) ),
             function(i)
               brm( family = hurdle_lognormal( link = "identity", link_sigma = "identity", link_hu = "logit" ),
                    formula = f[[i]], prior = p[[i]],
                    data = d, sample_prior = T, save_pars = save_pars(all = T),
                    control = list( adapt_delta = ad ),
                    chains = ch, iter = it, warmup = wu, cores = mcc, seed = s,
                    file = paste0( "mods/pilot/",i,".rds" ), file_refit = "on_change",
                    save_model = paste0("mods/pilot/",i,".stan")
               )
             )

# soft model checking via Rhat
cbind.data.frame( Rhat = sapply( names(m) , function(i) max( rhat(m[[i]]), na.rm = T ) ) %>% round(3) )

# plot Pareto-ks to see potentially influential cases
par( mfrow = c(3,1) )
for( i in names(m) ) plot( loo( m[[i]] ), label_points = T )
par( mfrow = c(1,1) )

# compare the models via loo
with( m , loo( fix, ran, mon ) )


# ---- posterior prediction ----

# add retrodictions of means for each row in the data set
# as well as intervals of retrodictions of single cases
ppred <- d %>%
  cbind.data.frame( ., fitted(m$mon) %>% as.data.frame() %>% rename( "fit2.5" = "Q2.5", "fit97.5" = "Q97.5" ) %>% select(-`Est.Error`) ) %>%
  cbind.data.frame( ., predict(m$mon) %>% as.data.frame() %>% rename( "pred2.5" = "Q2.5", "pred97.5" = "Q97.5" ) %>% select( pred2.5, pred97.5 ) )

# plot model's reproduction of observed data
ppred %>%
  ggplot( aes(x = event, y = faq) ) +
  geom_point( color = "#E69F00", size = 5 ) + # observed data
  geom_point( aes(x = event, y = Estimate ), size = 5.5, shape = 1 ) + # median prediction
  geom_linerange( aes(ymin = fit2.5, ymax = fit97.5), linewidth = 3 ) + # 95% posterior predictive interval of the mean
  geom_linerange( aes(ymin = pred2.5, ymax = pred97.5), linewidth = 1 ) + # 95% posterior predictive interval of the score
  scale_x_discrete( labels = c( "pre", paste0( "r", seq(1,5,2) ) ) ) +
  facet_wrap( ~ id, nrow = 7 ) +
  theme_bw( base_size = 18 )

# save it
ggsave( "figs/pilot/data_reproduction.jpg", dpi = 300, width = 13.1, height = 14.3 )

# compute contrasts against screening from posterior predictions of the mean
contr <- posterior_epred( m$mon, newdata = data.frame( event = levels(d$event), faq = NA, id = "IPN000" ),
                          allow_new_levels = T, re_formula = NA
                          ) %>%
  # format it
  `colnames<-`( levels(d$event) ) %>%
  as.data.frame() %>%
  # calculate the contrasts
  mutate( 'r1-screening' = r1 - screening,
          'r3-screening' = r3 - screening,
          'r5-screening' = r5 - screening
          )

# extract summaries of the contrasts
csum <- apply( contr, 2, function(x) c( median(x), hdi(x) ) ) %>%
  t() %>% as.data.frame() %>%
  `colnames<-`( c("Md","95HDI_low", "95HDI_upp") ) %>%
  round(2) %>%
  rownames_to_column( "contrast" )

# save it as csv
write.table( csum, "tabs/pilot/contrasts_sum.csv", sep = ",", row.names = F, quote = F )