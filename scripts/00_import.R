# This script prepares FAQ items from wide to long format and adds information for each item

# clear environment
rm( list = ls() )

# list required packages into a character object
pkgs <- c("here","tidyverse")

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare folders for preprocess data, models, tables and figures
sapply( c("_data"), function(i) if( !dir.exists(i) ) dir.create(i) )


# IN-HOUSE FUNCTIONS ----

# extract data from arrays
datex <- function(data) {
  
  lapply( with( dimnames(data), setNames(id,id) ),
          function(i)
            
            sapply( dimnames(data)$item, function(j) data[ j, , i] ) %>%
            as.data.frame() %>%
            rownames_to_column("event") %>%
            mutate( id = i, .before = 1 )

          ) %>%
    
    do.call( rbind.data.frame, . ) %>%
    `rownames<-`( 1:nrow(.) ) %>%
    return()
  
}


# DATA READ --- 

for ( i in c("faq","pdaq") ) assign( x = i, readRDS( here("_raw","resp_data.rds") )[[i]] )

# extract tables
faq <- datex(faq)
pdaq <- datex(pdaq)

## faq ----

# pivot longer
faq <-
  faq %>%
  pivot_longer( all_of( as.character(1:10) ), names_to = "item", values_to = "response" ) %>%
  mutate(
    min = 0, 
    max = 3,
    item_name =
      case_when(
        item == 1 ~ "Accounting and finances",
        item == 2 ~ "Tax and business records",
        item == 3 ~ "Shopping alone",
        item == 4 ~ "Skills and hobbies",
        item == 5 ~ "Using appliances",
        item == 6 ~ "Meal preparation",
        item == 7 ~ "Tracking current events",
        item == 8 ~ "Information uptake",
        item == 9 ~ "Remembering important events",
        item == 10 ~ "Travelling out of house",
        .default = NA
      )
  )

# drop patients with no responses
faq <-
  faq %>%
  left_join(
    faq %>%
      group_by(id,event) %>%
      summarise( exclude = ( sum( is.na(response) ) == 10 ) ),
    by = c("id","event")
  ) %>%
  filter( exclude == FALSE ) %>%
  select( !exclude )

## PDAQ ----

# pivot longer
pdaq <-
  pdaq %>%
  pivot_longer( all_of( as.character(1:15) ), names_to = "item", values_to = "response" ) %>%
  mutate(
    min = 0, 
    max = 4,
    item_name =
      case_when(
        item == 1 ~ "Reading",
        item == 2 ~ "Time keeping",
        item == 3 ~ "Counting money",
        item == 4 ~ "Following instructions",
        item == 5 ~ "Handling unfamiliar problems",
        item == 6 ~ "Explaining stepwise procedures",
        item == 7 ~ "Remembering lists",
        item == 8 ~ "Using map",
        item == 9 ~ "Remembering new information",
        item == 10 ~ "Multitasking",
        item == 11 ~ "Using new appliances",
        item == 12 ~ "Understanding own finances ",
        item == 13 ~ "Maintaining thoughts",
        item == 14 ~ "Discussing current events",
        item == 15 ~ "Remembering the date",
        .default = NA
      )
  )

# drop patients with no responses
pdaq <-
  
  pdaq %>%
  left_join(
    pdaq %>%
      group_by(id,event) %>%
      summarise( exclude = ( sum( is.na(response) ) == 15 ) ),
    by = c("id","event")
  ) %>%
  filter( exclude == FALSE ) %>%
  select( !exclude )
