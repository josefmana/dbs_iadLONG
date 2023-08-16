# Run this script first. It extracts and formats data from raw tables to analysis-ready outcomes.

# list required packages into a character object
pkgs <- c( "rstudioapi", "dplyr", "tidyverse" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works only in RStudio)
setwd( dirname(getSourceEditorContext()$path) )

# prepare a folder for sessions info and pre-processed data
sapply( c("sess","_nogithub/data") , function(i) if( !dir.exists(i) ) dir.create(i) )

# read the raw data
s <- read.csv( "_nogithub/raw/scoring.csv", sep = ";" )
v <- read.csv( "_nogithub/raw/var_names.csv", sep = "," ) # variables to keep
d0 <- read.csv( "_nogithub/raw/ITEMPO-ManaLongitudinlnNeur_DATA_2023-08-14_0908.csv", sep = "," ) %>% select( v$orig ) # data


# ---- reformatting ----

# extract FAQ item scores
for( i in unlist( strsplit( with( s, it[scale=="faq"] ), "," ) ) ) {
  # loop through all items and extract FAQ item scores
  d0[ , paste0("faq_",i) ] <- case_when( d0[ , paste0("faq_uvod_",i) ] == 1 ~ d0[ , paste0("faq_vykon_",i) ], # the patient evaluated an activity directly
                                         d0[ , paste0("faq_uvod_",i) ] == 2 ~ d0[ , paste0("faq_nikdy_",i) ]  # the patient evaluated an activity indirectly
                                         )
  # remove the original columns
  d0 <- d0[ , !colnames(d0) %in% paste("faq",c("uvod","nikdy","vykon"),i,sep="_") ]
}

# transform reverse-coded scores
for ( i in s$scale ) {
  if ( is.na( with( s, rev[scale==i] ) ) ) next # if there ain't no reverse item continue to the next scale
  else for ( j in paste0( i,"_",unlist( strsplit( s[s$scale==i,"rev"], "," ) ) ) ) d0[ , j ] <- s[s$scale==i,"max"] + 1 - d0[ , j ] # otherwise, flip around the midpoint
}

# change nominal variables from numbers to labels
for ( i in with( v, orig[complete.cases(X1)] ) ) for ( j in 0:(ncol(v)-3) ) d0[ which( d0[[i]] == j ) , i ] <- v[ v$orig == i, paste0("X",j) ]

# rename variables where specified
colnames(d0)[ colnames(d0) %in% with(v, orig[ !is.na(rename) ] ) ] <- na.omit(v$rename)

# re-format date variables
for( i in c( "birth", names(d0)[ grepl("date",names(d0)) ] ) ) d0[[i]] <- ifelse( d0[[i]] == "", NA, d0[[i]] ) %>% as.Date( "%Y-%m-%d" )

# rename event types to more parsimonious coding and shift FAQ items
d0 <- d0 %>% mutate( event = event %>% gsub( "nvtva_|_arm_1", "", . ) ) %>% relocate( starts_with("faq"), .after = date_quest )


# ---- distribute time constant data to all rows ----

# extract screening and surgery data separately
d1 <- lapply( setNames( na.omit(unique(v$from)), na.omit(unique(v$from)) ), function(i) d0[ d0$event == i , ] )
fix <- lapply( setNames( na.omit(unique(v$from)), na.omit(unique(v$from)) ), function(i) with( v, ifelse( is.na(rename), orig, rename )[ from == i ] ) %>% na.omit() )

# 
for ( i in names(d1) ) for( j in fix[[i]] ) for (k in 1:nrow(d0) ) if( !d0$id[k] %in% d1[[i]]$id ) next else d0[ k , j ] <- d1[[i]][ d1[[i]]$id == d0$id[k], j ]

# next:
# calculate time intervals
# drop unneeded variables
# export separate data files for PDAQ and FAQ



# ---- preparing data sets for FAQ and PDAQ ----

# 

