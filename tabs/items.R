# This script differentiates FAQ items to motor and cognitive scales.

# clear environment
rm( list = ls() )

# list required packages into a character object
pkgs <- c("here","tidyverse","lm.beta","gt")

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare folders for preprocess data, models, tables and figures
sapply( c("_data","mods","tabs","figs"), function(i) if( !dir.exists(i) ) dir.create(i) )


# IN-HOUSE FUNCTIONS ----

# numbers shinaningans
rprint <- function( x, dec=2 ) sprintf( paste0("%.",dec,"f"), round( x, dec ) ) # printing rounded numbers
zerolead <- function(x) sub( "0.", ".", x, fixed = T ) # get rid of leading zero

# collapse table to a cell
tabcol <- function(x) paste( table( x, useNA = "always"), collapse = "/" )

# summarise central tendency and variability
cenvar <-
  
  function(x, ct = "mean", var = "sd", dec = 2, sep = " ± ", end = "" ) {
    
    paste0(
      rprint( do.call( ct,list(x, na.rm = T) ), dec), sep,
      rprint( do.call(var,list(x, na.rm = T) ), dec), end
    )

  }

# extract values from linear regression
parex <- function(mod) {
  
  N <- length( residuals(mod) ) # number of observations used
  
  for( i in c("moca","updrs_iii_on") ) {
    
    assign( paste0("b_",i), rprint( lm.beta(mod)$standardized.coefficient[i], 2 ) )
    assign( paste0("p_",i), summary(mod)$coefficients[i,"Pr(>|t|)"] )
    assign( paste0("aster_",i), ifelse( get(paste0("p_",i)) < .05, "*", "" ) )
    assign( paste0("p_",i), ifelse( get(paste0("p_",i)) < .001, "< .001", zerolead( rprint( get(paste0("p_",i)), 3 ) ) )  )
    
  }
  
  return(
    c( n = N,
       moca = paste0(b_moca," (",p_moca,")",aster_moca),
       updrs_iii = paste0(b_updrs_iii_on," (",p_updrs_iii_on,")",aster_updrs_iii_on)
       )
  )
}

# extract coefficient and CIs
bci <- function(mod) cbind( b = summary(mod)$coefficients[ ,"Estimate"], confint(mod) )


# DATA IMPORT ----

# extract all data
d0 <-
  
  read.csv( here("_raw","preop_assessment.csv"), sep = "," ) %>% # pre-surgery data
  filter( label == "MoCA" ) %>% # keep MoCA only
  rename( "moca" = "score" ) %>%
  select( id, sex, hy_stage, type_pd, asym_park, pd_dur, edu_years, age_years, moca ) %>%
  
  # add UPDRS III
  left_join(
    read.csv( here("_raw","motor_sum_scores.csv"), sep = "," ) %>%
      filter( event == "screening" ) %>%
      select( id, med, ledd_mg, updrs_iii ) %>%
      pivot_wider( names_from = med, names_prefix = "updrs_iii_", values_from = updrs_iii )
  ) %>%
  
  # add FAQ
  left_join(
    readRDS( here("_raw","resp_data.rds") )$faq[ , "screening", ] %>%
      t() %>%
      as.data.frame() %>%
      `colnames<-`( paste0("faq_", colnames(.) ) ) %>%
      rownames_to_column("id")
  )# %>%
  
  # add PDAQ
#  left_join(
#    readRDS( here("_raw","item_responses.rds") )$pdaq[ , "screening", ] %>%
#      t() %>%
#      as.data.frame() %>%
#      `colnames<-`( paste0("pdaq_", colnames(.) ) ) %>%
#      rownames_to_column("id")
#  )

# filter out missing data (for pilot)
d1 <-
  
  d0 %>%
  filter_at( vars( starts_with("faq_") ), any_vars( !is.na(.) ) ) %>% # keep patients with at least one FAQ item answered
  filter_at( vars(moca,updrs_iii_on), all_vars( !is.na(.) ) ) # keep patients with both MoCA and Med-ON UPDRS III
  
# pivot longer for IRTs
d2 <-
  
  d1 %>%
  pivot_longer(
    cols = starts_with("faq"),
    values_to = "response",
    names_to = "item",
    names_transform = function(x) sub("faq_","",x)
  )


# SAMPLE DESCRIPTION ----

# summarise all the main variables
t1 <-
  
  data.frame(
    
    var = c( "Sex (F/M/NA)",
             "HY stage(1/1.5/2/2.5/3/4/NA)",
             "PD type (akinetic-rigid/tremor-dominant/NA)",
             "PD duration (years)",
             "LEDD (mg)",
             "Education (years)",
             "Age (years)",
             "MoCA",
             "UPDRS-III (med ON)",
             "UPDRS III (med OFF)",
             paste0( "FAQ item #", 1:10 )
             ),
    
    val = c( sapply( c("sex","hy_stage","type_pd"), function(i) tabcol( d1[[i]]) ),
             sapply( c("pd_dur","ledd_mg","edu_years","age_years","moca","updrs_iii_on","updrs_iii_off"), function(i) cenvar( d1[[i]] ) ),
             sapply( paste0("faq_",1:10), function(i) cenvar( d1[[i]] ) )
             )
    
  ) %>%
  
  gt() %>%
  cols_label( var = "Variable", val = "Value" ) %>%
  cols_align( -1, align = "center") %>%
  tab_caption( caption = md("**Table 1.** Descriptive statistics") ) %>%
  tab_footnote(
    locations = cells_column_labels("val"),
    footnote = "Presented as number of observations for nominal variables and mean ± standard deviation otherwise."
  )


# CONCEPTUAL REPLICATION ----

# fit a set of linear regressions separately for each item
# prepare formulas with or without potential confounders
form <-
  
  list(
    naked = lapply( 1:10, function(i) as.formula( paste0("faq_",i," ~ 1 + moca + updrs_iii_on") ) ),
    demo = lapply( 1:10, function(i) as.formula( paste0("faq_",i," ~ 1 + moca + updrs_iii_on + sex + age_years") ) ),
    becker = lapply( 1:10, function(i) as.formula( paste0("faq_",i," ~ 1 + moca + updrs_iii_on + sex + age_years + pd_dur") ) ), # will drop a lot of cases due to missing information about PD duration
    ledd = lapply( 1:10, function(i) as.formula( paste0("faq_",i," ~ 1 + moca + updrs_iii_on + sex + age_years + ledd_mg") ) )
  )

# and the same formulas with variables scaled
formsc <-
  
  list(
    naked = lapply( 1:10, function(i) as.formula( paste0("scale(faq_",i,") ~ scale(moca) + scale(updrs_iii_on)") ) ),
    demo = lapply( 1:10, function(i) as.formula( paste0("scale(faq_",i,") ~ scale(moca) + scale(updrs_iii_on) + scale(sex) + scale(age_years)") ) ),
    becker = lapply( 1:10, function(i) as.formula( paste0("scale(faq_",i,") ~ scale(moca) + scale(updrs_iii_on) + scale(sex) + scale(age_years) + scale(pd_dur)") ) ), # will drop a lot of cases due to missing information about PD duration
    ledd = lapply( 1:10, function(i) as.formula( paste0("scale(faq_",i,") ~ scale(moca) + scale(updrs_iii_on) + scale(sex) + scale(age_years) + scale(ledd_mg)") ) )
  )

# re-code sex so that it can be scaled
d1 <- d1 %>% mutate( sex = ifelse( sex == "female", 0, 1 ) )

# fit it
m0 <-
  
  lapply(
    
    setNames( names(form), names(form) ),
    function(i)
      lapply( 1:10, function(j) lm( formula = form[[i]][[j]], data = d1 ) )
      
  )

# extract model parameters
t2 <-
  
  lapply(
    
    setNames( names(m0), names(m0) ),
    function(i)
      sapply( 1:10, function(j) parex( m0[[i]][[j]] ) ) %>%
      t() %>%
      `rownames<-`( paste0( "Item ", 1:nrow(.) ) )
    
  ) %>%
  
  do.call( cbind.data.frame, . ) %>%
  
  gt( rownames_to_stub = T ) %>%
  cols_align( -1, align = "center" ) %>%
  tab_caption( caption = md("**Table 2.** Models comparisons") ) %>%
  tab_spanner( label = "No covariates", columns = starts_with("naked"), gather = F ) %>%
  tab_spanner( label = "Demographics covariates", columns = starts_with("demo"), gather = F ) %>%
  tab_spanner( label = "Becker et al. (2020)", columns = starts_with("becker"), gather = F ) %>%
  tab_spanner( label = "LEDD adjusted", columns = starts_with("ledd"), gather = F ) %>%
  
  cols_label(
    ends_with(".n") ~ "n",
    ends_with("moca") ~ "MoCA",
    ends_with("updrs_iii") ~ "UPDRS-III"
  ) %>%
  
  tab_footnote( locations = cells_column_spanners("No covariates"), footnote = "faq ~ 1 + moca + updrs_iii" ) %>%
  tab_footnote( locations = cells_column_spanners("Demographics covariates"), footnote = "faq ~ 1 + moca + updrs_iii + sex + age" ) %>%
  tab_footnote( locations = cells_column_spanners("Becker et al. (2020)"), footnote = "faq ~ 1 + moca + updrs_iii + sex + age + pd_duration" ) %>%
  tab_footnote( locations = cells_column_spanners("LEDD adjusted"), footnote = "faq ~ 1 + moca + updrs_iii + sex + age + ledd" ) %>%
  tab_footnote( locations = cells_column_labels( contains("updrs_iii") ), footnote = "assessed in medication ON state" ) %>%
  tab_source_note( source_note = "n: number of patients included; values in MoCA and UPDRS-III columns represent standardised regression coefficients with p-value from t-test for each coefficient equalling zero in brackets; in the original, Becker et al. (2020) used the model used in the third column group and report significant MoCA effects for items 1, 2, 7 and 9, and significant UPDRS-III effects for items 3, 4, 5, 6, 7 and 10; *p < .05" )


# RESULTS SAVING ----

gtsave( t1, here("tabs","irt_descriptives.docx") )
gtsave( t2, here("tabs","irt_replication.docx") )

# GRAPH FORM ----

t3 <- 
  
  lapply(
    
    names(m0),
    function(k)
      
      lapply(
        
        1:10,
        function(j)
          sapply( c("moca","updrs_iii_on"),
                  function(i) bci( lm( formsc[[k]][[j]], data = d1 ) )[ paste0("scale(",i,")"), ]
                  ) %>%
          t() %>%
          as.data.frame() %>%
          rownames_to_column("test") %>%
          mutate( item = j, model = k, `Data set` = "Prague")
        
      ) %>%
      
      do.call( rbind.data.frame, . )
    
  ) %>%
  
  do.call( rbind.data.frame, . ) %>%
  
  add_row( test = rep( c("moca","updrs_iii_on"), 10 ),
           b = c( -.21, .04, -.26, .04, .01, .30, -.02, .30, -.02, .18, -.07, .19, -.20, .24, -.14, .13, -.17, .09, -.12, .15),
           `2.5 %` = NA, `97.5 %` = NA, item = sort( rep( 1:10, 2 ) ), model = "becker", `Data set` = "Tubingen"
           ) %>%
  
  mutate(
    Model = case_when(
      model == "naked" ~ "FAQ ~ MoCA + UPDRS-III",
      model == "demo" ~ "FAQ ~ MoCA + UPDRS-III +\n  Sex + Age",
      model == "becker" ~ "FAQ ~ MoCA + UPDRS-III +\n  Sex + Age + PD duration",
      model == "ledd" ~ "FAQ ~ MoCA + UPDRS-III +\n  Sex + Age + LEDD"
    ),
    Predictor = case_when(
      test == "moca" ~ "MoCA",
      test == "updrs_iii_on" ~ "UPDRS-III"
    ),
    Item = case_when(
      item == 1 ~ "1. Accounting and finances (C)",
      item == 2 ~ "2. Tax and business records (C)",
      item == 3 ~ "3. Shopping alone (M)",
      item == 4 ~ "4. Skills and hobbies (M)",
      item == 5 ~ "5. Using appliances (M)",
      item == 6 ~ "6. Meal preparation (M)",
      item == 7 ~ "7. Tracking current events (C/M)",
      item == 8 ~ "8. Information uptake",
      item == 9 ~ "9. Remembering important events (C)",
      item == 10 ~ "10. Travelling out of house (M)",
      .default = NA
    )
  ) %>%
  
  select(b, `2.5 %`, `97.5 %`, Item, Predictor, Model, `Data set`)

# plot it
t3 %>%
  
  mutate( Item = factor( Item, levels = unique(Item), ordered = T ) ) %>%
  ggplot() +
  aes( x = b, xmin = `2.5 %`, xmax = `97.5 %`, y = Model, fill = `Data set`, colour = `Data set`, linetype = Predictor, shape = Model ) +
  geom_point( size = 3, position = position_dodge(width = .5) ) +
  geom_linerange( position = position_dodge(width = .5) ) +
  geom_vline( xintercept = 0, linetype = "solid", colour = "black", linewidth = .5 ) +
  scale_y_discrete( name = NULL, labels = NULL, breaks = NULL ) +
  scale_fill_manual( values = c("navyblue","red") ) +
  scale_colour_manual( values = c("navyblue","red") ) +
  scale_shape_manual( values = c(21:24) ) +
  labs( x = "Regression beta weight", title = "Figure 1. Association of the FAQ items with cognitive and motor status", subtitle = "Comparison of distinction between cognitive and motor items in PD patients in Prague and Tubingen") + 
  facet_wrap( ~ Item, nrow = 5, scales = "free_y" ) +
  theme_bw() +
  theme( legend.position = "right", legend.key.height = unit(1,"cm") )

# save it
ggsave( plot = last_plot(),
        filename = here("figs","FAQ items distinction.jpg"),
        dpi = 300,
        width = 7,
        height = 10.3
        )


# ---- SESSION INFO ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = here("scripts","irt.txt") )

