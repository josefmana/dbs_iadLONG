# Run this script first. It extracts and formats data from raw tables to analysis-ready outcomes.

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse", "lubridate" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works only in RStudio)
setwd( dirname(getSourceEditorContext()$path) )

# prepare a folder for sessions info and pre-processed data
sapply( c("sess","_nogithub/data") , function(i) if( !dir.exists(i) ) dir.create(i) )


# ---- read the raw data ----

v <- read.csv( "_nogithub/raw/var_names.csv", sep = "," ) # variables to keep for re-formatting
c <- read.csv( "_nogithub/raw/columns.csv", sep = ",", header = F ) %>% unlist() %>% as.character() # variables to keep for final data set
df <- read.csv( "_nogithub/raw/ITEMPO-ManaLongitudinlnNeur_DATA_2023-08-14_0908.csv", sep = "," ) %>% select( v$orig ) # data


# ---- reformat ----

# extract FAQ item scores
for( i in unlist( strsplit( with( s, it[scale=="faq"] ), "," ) ) ) df[ , paste0("faq_",i) ] <- case_when(
  df[ , paste0("faq_uvod_",i) ] == 1 ~ df[ , paste0("faq_vykon_",i) ], # the patient evaluated an activity directly
  df[ , paste0("faq_uvod_",i) ] == 2 ~ df[ , paste0("faq_nikdy_",i) ]  # the patient evaluated an activity indirectly
)

# change nominal variables from numbers to labels
for ( i in with( v, orig[complete.cases(X1)] ) ) for ( j in 0:(ncol(v)-3) ) df[ which( df[[i]] == j ) , i ] <- v[ v$orig == i, paste0("X",j) ]

# rename event types to more parsimonious coding
df <- df %>% mutate( redcap_event_name = redcap_event_name %>% gsub( "nvtva_|_arm_1", "", . ) )

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
  for ( j in names(df)[ grepl("since",names(df)) ] ) df[[ paste0( gsub("_since","_init2",j), gsub("date_","",i) ) ]] <- year( df[[i]] ) - df[[j]]

}


# ---- make final changes and save ----

# drop rows of patients with target different than bilateral STN
df <- df %>%
  filter( target == "stn" & side_surg_right == 1 & side_surg__left == 1 ) %>%
  select( all_of(c) ) %>%
  mutate( across( everything(), ~ifelse( .x == "", NA, .x ) ) )

# save the resulting data frame as .csv
write.table( df, "_nogithub/data/data_stn_bil.csv", sep = "\t", na = c("NA",""), quote = F, row.names = F )


# ----------- session info -----------

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/import.txt" )
