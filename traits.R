library(tidyverse)
library(rfishbase)
all.data <- read_csv("processed_benthic_data_all_years.csv")

# Alexa to connect WORMS APHIAID to SLB 
# Emma to decide specific traits

taxdat <- all.data |> 
  select(WoRMS_aphiaID, species) |> 
  distinct()

# see all the tables available
fb_tables(server="sealifebase")

# I get 207 

# let's start with reproductive traits 
repro <- reproduction(server="sealifebase", species_list = taxdat$species) 
# do we get different columns from different tables?
repro2 <- fb_tbl("reproduc", server="sealifebase") # get back the whole table, not just a species subset; columns look the same as above so I don't think we need to do this separately 

eggdev <- fb_tbl("eggdev", server="sealifebase") # very few species; skip this one 
larvalswimspeed <- fb_tbl("larvalswimspeed", server="sealifebase") # empty! 
# information about larvae
# pelagic larval duration; free swimming; 