# This script computes and summarises GLMMs for a pilot study aimed at describing long-term changes in IADL after STN-DBS in PD.

# clear environment
rm( list = ls() )

# list required packages into a character object
pkgs <- c("here","tidyverse","gt","ggplot2", "brms", "tidybayes")

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare folders for preprocess data, models, tables and figures
sapply( c("_data","mods","tabs","figs"), function(i) if( !dir.exists(i) ) dir.create(i) )

# set theme for plotting in ggplot2
theme_set( theme_minimal( base_size = 12 ) )

# function for printing round numbers
rprint <- function( x, dec=2 ) sprintf( paste0("%.",dec,"f"), round( x, dec ) )


# DATA IMPORT ----

v <- read.csv( "_raw/pilot/var_names.csv", sep = "," ) # variables to keep for re-formatting
s <- read.csv( "_raw/pilot/scoring.csv", sep = ";" ) # scoring
c <- read.csv( "_raw/pilot/columns.csv", sep = ",", header = F ) %>% unlist() %>% as.character() # variables to keep for final data set
df <- read.csv( "_raw/pilot/ITEMPO-ManaLongitudinlnNeur_DATA_2023-08-14_0908.csv", sep = "," ) %>% select( v$orig ) # data


# ---- reformat ----

# extract FAQ item scores
for( i in unlist( strsplit( with( s, it[scale=="faq"] ), "," ) ) ) {
  
  df[ , paste0("faq_",i) ] <-
    
    case_when(
      
      df[ , paste0("faq_uvod_",i) ] == 1 ~ df[ , paste0("faq_vykon_",i) ], # the patient evaluated an activity directly
      df[ , paste0("faq_uvod_",i) ] == 2 ~ df[ , paste0("faq_nikdy_",i) ]  # the patient evaluated an activity indirectly
      
    )
  
}

# change nominal variables from numbers to labels
for ( i in with( v, orig[complete.cases(X1)] ) ) for ( j in 0:(ncol(v)-3) ) df[ which( df[[i]] == j ) , i ] <- v[ v$orig == i, paste0("X",j) ]

# rename event types to more parsimonious coding
df <-
  
  df %>%
  mutate( redcap_event_name = redcap_event_name %>% gsub( "nvtva_|_arm_1", "", . ) )

# rename variables where specified
colnames(df)[ colnames(df) %in% with(v, orig[ !is.na(rename) ] ) ] <- na.omit(v$rename)


# ---- transform time fields ----

# extract screening and surgery data separately
vdat <- names(df)[ grepl("date",names(df)) ] # time variables (as dates)
d0 <- lapply( setNames( na.omit(unique(v$from)), na.omit(unique(v$from)) ), function(i) df[ df$event == i , ] ) # screening and surgery data
fix <- lapply( setNames( na.omit(unique(v$from)), na.omit(unique(v$from)) ), function(i) with( v, ifelse( is.na(rename), orig, rename )[ from == i ] ) %>% na.omit() )

# re-format date variables
for ( i in c( "birth", names(df)[ grepl("date",names(df)) ] ) ) df[[i]] <- ifelse( df[[i]] == "", NA, df[[i]] ) %>% as.Date( "%Y-%m-%d" )

# redistribute all screening and surgery variables across rows
for ( i in names(d0) ) for( j in fix[[i]] ) for (k in 1:nrow(df) ) if( !df$id[k] %in% d0[[i]]$id ) next else df[ k , j ] <- d0[[i]][ d0[[i]]$id == df$id[k], j ]

# compute time differences in years
for ( i in vdat ) {
  
  # calculate age at each event for each measure
  df[[ paste0( "age", gsub("date","",i) ) ]] <- time_length( with( df, difftime( get(i), birth ) ), "years" )
  
  # calculate time (in years) from surgery
  if( !grepl("surg",i) ) df[[ paste0( "surg_to", gsub("date","",i) ) ]] <- time_length( with( df, difftime( get(i), date_surg ) ), "years" )
  
  # calculate years of disease duration and medication
  for ( j in names(df)[ grepl("since",names(df)) ] ) df[[ paste0( gsub("_since","_init2",j), gsub("date_","",i) ) ]] <- year(df[[i]]) - df[[j]]
  
}


# ---- make final changes and save ----

# drop rows of patients with target different than bilateral STN
df <-
  
  df %>%
  filter( target == "stn" & side_surg_right == 1 & side_surg__left == 1 ) %>%
  select( all_of(c) ) %>%
  mutate( across( everything(), ~ifelse( .x == "", NA, .x ) ) )

# save the resulting data frame as .csv
write.table( df, here("_data","pilot_data.csv"), sep = "\t", na = c("NA",""), quote = F, row.names = F )


# DATA PROCESSING ----

# keep only objects of interest
rm( list = ls()[ !( ls() %in% c("df","s","rprint") ) ] )

# transform reverse-coded scores and calculate sum scores
for ( i in s$scale ) {
  
  # if there are reverse items, flip them
  if ( !is.na( with( s, rev[scale==i] ) ) ) {
    rev <- paste0( i,"_",unlist( strsplit( s[s$scale==i,"rev"], "," ) ) ) # extract items to be reverse coded
    for ( j in rev  ) df[ , j ] <- s[s$scale==i,"max"] + 1 - df[ , j ] # loop through reverse coded items
    rm(rev)
  }
  
  # calculate sum scores
  df[ , i] <- rowSums( df[ , paste0(i, "_", unlist( strsplit( s[s$scale==i,"it"], "," ) ) ) ] )
  
}

# extract only relevant columns
d1 <- df[ , c(1:9,19:20,25,237:242,117:118,123,185:186,191:194,215:216,221:236) ]

# drop rows
d1 <-
  
  d1 %>%
  
  # pre-process the table
  filter( faq_patient %in% c(1,NA) ) %>% # keep only patients self-report
  filter( !event %in% c( "refresh", "operace", paste0( "r",seq(7,19,2) ) ) ) %>% # drop surgery, refresh and re-tests further than five years post-surgery
  filter( id %in% d1[ with( d1, event == "screening" & !is.na(faq) ) , "id" ] ) %>% # keep only patients with FAQ screening measured
  filter( complete.cases(faq) ) %>% # keep only observations with FAQ
  
  # re-format event variable to an ordered one
  mutate( event = factor( event, levels = c( "screening", paste0( "r", seq(1,5,2) ) ), ordered = T ) )

# keep only patients with at least one post-test (in addition to a pre-test)
d1 <- d1[ with( d1, id %in% names( which( table(id) != 1 ) ) ) , ]

# prepare a sanity plot checking time before/after surgery at each time-point/patient 
d1 %>%
  
  # pre-process the table
  mutate( faq_present = ifelse( is.na(faq), F, T ), time = ifelse( faq_present, round(surg_to_psycho,2), NA ) ) %>%
  
  # plotting proper
  ggplot( aes( y = event, x = reorder(id,faq_present), fill = faq_present, label = time ) ) +
  geom_tile( color = "white" ) + geom_text( color = "black", angle = 90 ) + # add data presence indicators and times from surgery
  scale_fill_manual( values = "grey81" ) + labs( y = NULL, x = NULL ) +
  theme( legend.position = "none", axis.text.x = element_text( angle = 90, vjust = 0.5 ) )

# save it
ggsave( filename = here("figs","pilot_sanity_plot.jpg"), dpi = 300, width = 16, height = 6 )

# weird cases:
# IPN157, r5, seems all-right according to REDCap
# IPN247, r3, seems all-right according to REDCap
# IPN283, r3, non-scheduled at 2 year mark, will keep the data point though (and will keep an eye on it as well)
# IPN584, r3, twice for some reason, dropping this one
# IPN243, r1, closer to r3 so re-coding (r1 -> r3)

# re-code/drop cases identified above
d1 <- d1[ with(d1, !(id == "IPN584" & event == "r3") ) , ]
d1[ with( d1, id == "IPN243" & event == "r1"), "event" ] <- "r3"


# STATISTICAL MODELLING ----

# set-up Stan options
mcc = 8 # all CPU cores
ch = 4 # four chains
it = 2000 # total iterations per chain
wu = 1000 # warm-um iterations per chain
ad = .999 # adapt_delta parameter
seed = 87542 # seed for reproducibility


# ---- DATA DESCRIPTION ----

# list continuous columns to be described
cvar <-
  
  data.frame(
    var = c("surg_to_psycho","age_surg","edu_years","hy_stage","age_psycho","faq","drs","bdi","ledd"),
    nms = c("Years after surgery","Age at surgery (years)","Education (years)","HY stage","Age at assessment (years)","FAQ (range 0-30)","DRS-2 (range 0-144)","BDI-II (range 0-63)","LEDD (mg)"),
    blk = c( "Time from surgery", rep("Baseline characteriscs",3), rep("Longitudinal outcomes",5) )
  )

# list nominal columns to be described
nvar <-
  
  data.frame(
    var = c("sex","type_pd"),
    nms = c("Sex (females/males)","PD Type (akinetic-rigid/tremordominant)"),
    blk = rep("Baseline characteriscs",2)
  )
  

# extract M (SD) and Md (IQR) descriptions
dtab <-
  
  lapply(
    
    # loop through measures of central tendency and variability measures
    setNames( c("mean_sd","median_IQR"), c("mean_sd","median_IQR") ),
    function(i)
      
      sapply(
        
        # loop through longitudinal events
        levels(d1$event),
        function(j)
          
          sapply(
            
            # loop through continuous variables
            cvar$var,
            function(k)
              
              # export results in format 'central tendency (variability)'
              paste0(
                do.call( sub( "_.*", "", i ), list( x = d1[ d1$event == j, k ], na.rm = T ) ) %>% rprint(2), " (",
                do.call( sub( ".*_", "", i ), list( x = d1[ d1$event == j, k ], na.rm = T ) ) %>% rprint(2), ")"
              )
  
          )
      )
    
  )

# add ratio of females/males and akinetic-rigid/tremordominanat patients at each event
for( i in names(dtab) ) {
  
  dtab[[i]] <-
    
    rbind.data.frame(
      
      # add nominal variables frequencies at the top of the tables
      sapply(
        
        levels(d1$event),
        function(j)
          
          sapply(
            
            nvar$var,
            function(k)
              table( d1[ d1$event == j , k ] ) %>% paste( collapse = "/" )
            
          )
        
      ), dtab[[i]]
      
    ) %>%
    
    # tidy it up
    add_column( block = sapply( rownames(.), function(j) with( do.call( rbind.data.frame, list(cvar,nvar) ), blk[var==j] ) ), .before = 1) %>%
    add_column( var = sapply( rownames(.), function(j) with( do.call( rbind.data.frame, list(cvar,nvar) ), nms[var==j] ) ), .before = 1) %>%
    rename( "Preop." = "screening", "Y1" = "r1", "Y3" = "r3", "Y5" = "r5" ) %>%
    slice( 3, 1:2, 4:n() )
  
}

# save the descriptive tables as .csv
for ( i in names(dtab) ) write.table( dtab[[i]], here( "tabs", paste0("pilot_desc_",i,".csv") ), sep = ",", row.names = F, quote = F )

# prepare a formatted description table
t1 <-
  
  dtab$mean_sd %>%
  
  gt( rowname_col = "var", groupname_col = "block", caption = "Table 1. Sample description" ) %>%
  cols_align( -1, align = "center" ) %>%
  tab_source_note( source_note = "Preop.: pre-surgery assessment; Y1: year one assessment, Y3: year three assessment, Y5: year five assessment; PD: Parkinson's Disease; HY: Hoehn and Yahr scale; FAQ = Functional Activities Assessment; DRS-2 = Dementia Rating Scale, second edition; BDI-II: Beck Depression Inventory, second edition; LEDD: levodopa equivalent daily dose. Values represent mean (standard deviation)." )

# save it
gtsave( t1, here("tabs","pilot_desc_table.docx") )


# ---- STATISTICAL MODELLING ----

# prepare a data set for Bayesian hierarchical modelling
# will fit three (hurdle) lognormal GLMMs using events as: (i) 'fixed effects', (ii) 'random effects' and (iii) monotonic effects
d <- with( d1, data.frame( id = factor(id), event = event, faq = faq ) )

# set-up dummy coding for event unpooled effects via indicator variables
contrasts(d$event) <- contr.treatment( length( levels(d$event) ) )

# representation: set-up linear models
f <-
  
  list(
    fix = bf( faq ~ 1 + event + (1 | id) ), # events unpooled (with 'fixed' priors)
    ran = bf( faq ~ 1 + (1 | event) + (1 | id) ), # partially pooled (statistically) independent events ('random effects')
    mon = bf( faq ~ 1 + mo(event) + (1 | id) ) # events modeled as a monotonic ordinal predictor
  )

# representation: set-up priors
p <-
  
  list(
    # set-up such that there is almost zero change to get scores higher than log(30) (Intercept, b),
    # with expected inter-patient and residual variability about 10 FAQ points (log(10) ~ 1/0.44) (which is too high but will not break the model) (sd, sigma),
    # and default brms priors for hurdle mixture parameter (hu)
    fix = c( prior( normal(1.6,0.6), class = Intercept ), prior( beta(1,1), class = hu ), prior( exponential(0.44), class = sd ), prior( exponential(0.44), class = sigma ), prior( normal(0,1), class = b ) ),
    ran = c( prior( normal(1.6,0.6), class = Intercept ), prior( beta(1,1), class = hu ), prior( exponential(0.44), class = sd ), prior( exponential(0.44), class = sigma ) ),
    mon = c( prior( normal(1.6,0.6), class = Intercept ), prior( beta(1,1), class = hu ), prior( exponential(0.44), class = sd ), prior( exponential(0.44), class = sigma ), prior( normal(0,1), class = b ), prior( dirichlet(1), class = simo, coef = moevent1 ) )
  )

# implementation: let the models learn from data
m <-
  
  lapply(
    
    setNames( names(f), names(f) ),
    function(i)
      
      brm(
        family = hurdle_lognormal( link = "identity", link_sigma = "identity", link_hu = "logit" ),
        formula = f[[i]], prior = p[[i]],
        data = d, sample_prior = T, save_pars = save_pars(all = T),
        control = list( adapt_delta = ad ),
        chains = ch, iter = it, warmup = wu, cores = mcc, seed = seed,
        file = here( "mods", paste0("pilot_",i,".rds") ), file_refit = "on_change",
        save_model = here( "mods", paste0("pilot_",i,".stan") )
      )
    
  )

# soft model checking via Rhats
cbind.data.frame( Rhat = sapply( names(m) , function(i) max( rhat(m[[i]]), na.rm = T ) ) %>% round(3) )

# plot Pareto-ks to see potentially influential cases
par( mfrow = c(3,1) )
for( i in names(m) ) plot( loo( m[[i]] ), label_points = T )
par( mfrow = c(1,1) )

# compare the models via loo
with( m , loo( fix, ran, mon ) )


# POSTERIOR PREDICTION ----

# select the model for presentation
# why is it arguably the worst one (fixed)?
m0 <- m$fix


# ---- posterior predictive check (a.k.a. retrodictions) ----

# add retrodictions of means for each row in the data set
# as well as intervals of retrodictions of single cases
ppred <-
  
  d %>%
  cbind.data.frame( ., fitted(m0) %>% as.data.frame() %>% rename( "fit2.5" = "Q2.5", "fit97.5" = "Q97.5" ) %>% select(-`Est.Error`) ) %>%
  cbind.data.frame( ., predict(m0) %>% as.data.frame() %>% rename( "pred2.5" = "Q2.5", "pred97.5" = "Q97.5" ) %>% select( pred2.5, pred97.5 ) )

# plot model's reproduction of observed data
ppred %>%
  
  # anonymise patients
  mutate( id = paste0( "patient ", sprintf( "%02d", as.numeric(ppred$id) ) ) ) %>%
  
  # plotting proper
  ggplot( aes(x = event, y = faq) ) +
  geom_point( color = "#E69F00", size = 5 ) + # observed data
  geom_point( aes(x = event, y = Estimate ), size = 5.5, shape = 1 ) + # median prediction
  geom_linerange( aes(ymin = fit2.5, ymax = fit97.5), linewidth = 3 ) + # 95% posterior predictive interval of the mean
  geom_linerange( aes(ymin = pred2.5, ymax = pred97.5), linewidth = 1 ) + # 95% posterior predictive interval of the score
  labs( y = "FAQ (range 0-30)", x = NULL ) +
  scale_x_discrete( labels = c( "Pre.", paste0( "Y", seq(1,5,2) ) ) ) +
  facet_wrap( ~ id, nrow = 7 ) +
  theme_bw( base_size = 18 )

# save it
ggsave( here("figs","pilot_data_reproduction.jpg"), dpi = 300, width = 16, height = 10 )


# ---- table of contrasts ----

# compute contrasts against screening from posterior predictions of the mean
contr <-
  
  posterior_epred(
    
    m0,
    newdata = data.frame( event = levels(d$event), faq = NA, id = "IPN000" ),
    allow_new_levels = T,
    re_formula = NA
    
  ) %>%
  # format it
  `colnames<-`( levels(d$event) ) %>%
  as.data.frame() %>%
  rename( "Preop." = "screening", "Y1" = "r1", "Y3" = "r3", "Y5" = "r5" ) %>%
  # calculate the contrasts
  mutate(
    'Y1-Preop.' = Y1 - `Preop.`,
    'Y3-Preop.' = Y3 - `Preop.`,
    'Y5-Preop.' = Y5 - `Preop.`
  )

# extract summaries of the contrasts
t2 <-
  
  apply( contr, 2, function(x) c( median(x), sum(x>0)/length(x), hdi(x) ) ) %>%
  t() %>%
  as.data.frame() %>%
  mutate( V5 = paste0( rprint(V1), " [", rprint(V3), ", ", rprint(V4), "]" ), V2 = rprint(V2,3) ) %>%
  select(V5,V2) %>%
  `colnames<-`( c("Md [95% HDI]","Pr(b>0)") ) %>%
  rownames_to_column( "Contrast" )

# save it as csv
write.table( t2, here("tabs","pilot_contrasts.csv"), sep = ",", row.names = F, quote = F )

# format the table
t2 <-
  
  t2 %>%
  gt( caption = "Table 2. Model estimates and contrast comparisons" ) %>%
  cols_align( -1, align = "center" ) %>%
  tab_source_note( source_note = "Preop.: pre-surgery assessment; Y1: year one assessment, Y3: year three assessment, Y5: year five assessment; Md: median model estimate, HDI: Highest Density Interval; Pr(b>0): probability that the difference is positive." ) %>%
  tab_footnote( "Model estimates on the outcome scale (i.e., in terms of expected median FAQ score in a sample of patients)", locations = cells_column_labels( columns = `Md [95% HDI]` ) )

# save it
gtsave( t2, here("tabs","pilot_contr_table.docx") )


# ---- SESSION INFO ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = here("scripts","pilot.txt") )
