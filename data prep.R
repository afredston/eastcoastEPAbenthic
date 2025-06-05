
# DATA PREP:
# this script cleans and amalgamates the EPA benthic invertebrate survey and 
# benthic grab data into the dataset used for the analyses of our manuscript.


# Starting with a clean environment:
################################################################################
rm(list=ls())
################################################################################

# Loading all necessary packages:
################################################################################
library("tidyverse") # for data manipulation
library("here") # for relative file paths
library("taxize") #for standardizing taxonomy
#remotes::install_github("ropensci/rfishbase")
rfishbase::available_releases()
library("rfishbase") # for sealifebase to get trait data
library("stringi") # necessary for rfishbase
library("lubridate") #for working with dates
################################################################################

# Loading all potentially relevant data:
# downloaded all benthic macroinvertebrate files from EPA. (early may 2025)
# had to also download site info files for some.
# didn't download any of the other water chemistry, etc. files yet.
################################################################################
#early 2000's data w species counts:
BC2000<-read_csv(here("current work", "raw data", "nca_benthicdata.csv")) 
#2010 data w species counts (used in water quality assessments):
BC2010a<-read_csv(here("current work", "raw data", 
                       "assessed_ncca2010_benthic_data.csv")) 
#2010 data w species counts (not used in water quality assessments): 
#NOTE: as I was processing the data I realised these were all from seccondary 
#visits to the same sites as in those used in the water quality assessments so I 
#am excluding this data from our analyses as well, since I limiting our data to 
#the first visits only (see Step 3)
BC2010b<-read_csv(here("current work", "raw data", 
                       "not_assessed_ncca2010_benthic_data.csv")) 
#2010 data w site info (used in water quality assessments):
BC2010asites<-read_csv(here("current work", "raw data", 
                            "assessed_ncca2010_siteinfo.revised.06212016.csv"))
#2010 data w site info (not used in water quality assessments): 
# NOTE: again all from seccondary visits to the same sites in the assessed data 
#so can ignore this data (see Step 3)
BC2010bsites<-read_csv(here("current work", "raw data", 
                        "not_assessed_ncca2010_siteinfo.revised.06212016.csv")) 
#2010 data w necessary info on grabs:
BG2010<-read_csv(here("current work", "raw data", 
                      "ncca2010_benthic_indicator_status.csv")) 
#2015 data on the just the grabs:
BG2015<-read_csv(here("current work", "raw data", 
                      "ncca_2015_benthic_grab_estuarine-data.csv")) 
#2015 data w species counts:
BC2015<-read_csv(here("current work", "raw data", 
                      "ncca_2015_benthic_count_estuarine-data.csv")) 
#2015 data w site info:
BC2015sites<-read_csv(here("current work", "raw data", 
                           "ncca_2015_site_information_estuarine-data.csv")) 
#2020 data on just the grabs:
BG2020<-read_csv(here("current work", "raw data", "ncca20_bentgrab_data.csv")) 
#2020 data w species counts:
BC2020<-read_csv(here("current work", "raw data", "ncca20_bentcount_data.csv")) 
#2020 data w estuary site info:
BC2020Esites<-read_csv(here("current work", "raw data", 
                            "ncca20_siteinfo_estuaries_data_2.csv")) 
#2020 reef flat site info:
#NOTE: we don't use this data since we are focusing on estuaries only since they
#were sampled across each year we're working with data from
BC2020Rsites<-read_csv(here("current work", "raw data", 
                            "ncca20_siteinfo_reef_flats_data.csv")) 
################################################################################

# Step 1: the 2015 count data doesn't have densities of the taxa observed in 
# each sample or the grab areas from which to calculate densities so we need to 
# get the grab areas from the grab data and add them to this to be able to 
# calculate these later
################################################################################
#for this year also need grab area data so we can calculate species densities to 
#add to this data later
ga<-BG2015 %>% select(UID, FINAL_GRAB_AREA) 
ga<-unique(ga)
BC2015<-merge(BC2015, ga, by="UID", all.x=TRUE, all.y=FALSE)
################################################################################

# Step 2: Cut data so only considering east coast marine estuaries 
################################################################################
BC2000<-BC2000 %>%
  filter(NCA_REGION == "East_Coast")

coords<-BC2010asites %>% select(UID, ALAT_DD, ALON_DD, NCA_REGION, VISIT_NO)
coords<-unique(coords) #to remove any possibly duplicated info
BC2010a<-merge(BC2010a, coords, by = "UID") #merging these two data frames 
#so can get the coordinates of each grab sample since these aren't in the count 
#data for this year
BC2010a<-BC2010a %>%
  filter(NCA_REGION == "East Coast")

coords<-BC2010bsites %>% select(UID, TLAT_DD, TLON_DD, NCA_REGION, VISIT_NO)
coords<-unique(coords) #to remove any possibly duplicated info
BC2010b<-merge(BC2010b, coords, by = "UID") 
BC2010b<-BC2010b %>%
  filter(NCA_REGION == "East Coast")

coords<-BC2015sites %>% select(UID, LON_DD, LAT_DD, NCA_REG) # get coordinates 
#for each site and region info from the 2010 data
coords<-unique(coords) #to remove any possibly duplicated info
BC2015<-merge(BC2015, coords, by="UID")
BC2015<-BC2015 %>%
  filter(NCA_REG == "East_Coast")

BC2020 %>% filter(HABITAT == "EST")
coords<-BC2020Esites %>% select(UID, LON_DD, LAT_DD, NCA_REG) # get coordinates 
#for each site and region info from the 2020 estuary data
coords<-unique(coords) #to remove any possibly duplicated info
BC2020<-merge(BC2020, coords, by = "UID")
BC2020<-BC2020 %>%
  filter(NCA_REG == "East_Coast")
################################################################################

# Step 3: taking only the data from visit 1 to each site
################################################################################
# early 2000s all from only one visit
BC2010a<-BC2010a %>% filter(VISIT_NO == 1)
#BC2010b<-BC2010b %>% filter(VISIT_NO.x == 1) # ALL FROM VISIT 2 so ignore
BC2015<-BC2015 %>% filter(VISIT_NO == 1)
BC2020<-BC2020 %>% filter(VISIT_NO == 1)
################################################################################


# Step 4: Get and WoRMS_aphiaIDs for the taxonomy if not given already (BC2000).
# And adding a column with current species names according to its WoRMS_aphiaID 
# for anything ID'd to species
################################################################################
#defining a function to get the scientific name of any ID'd to species by their 
#WORMS_aphiaID 
species_from_wids<-function(wids){
  widslist_data<-classification(wids, db="worms", return_id=TRUE) #not too slow
  species<-rep(NA, length(wids))
  for(i in 1:length(wids)){
    print(i)
    if(is.na(wids[i])){} else {
      if(any(widslist_data[[i]]$rank=="Species")){ #if the taxon is ID'd to species 
        species[i]<-widslist_data[[i]]$name[which(widslist_data[[i]]$rank=="Species")] #record it's species
      }
    }
  }
  return(species)
}

#early 2000's:
######################## SLOW TO RUN! SAVED A COPY OF THE OUTPUT OF THIS CHUNK 
## TO AVOID EVER HAVING TO RERUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#names<-unique(BC2000$taxon) #1471 of them
#wids<-get_wormsid(names, accepted=TRUE) #~1.5 hour #342 unidentified, 
##used accepted name over alternatives, if not ID's to species didn't worry to 
##much about it just made sure to not pick a name given to species since any 
##records not ID'd to species will be filtered out anyways
#species<-species_from_wids(wids)
#txn_cors<-data_frame(names, species, wids)
#write_csv(txn_cors, "txn_cors2000.csv")
########################
txn_cors<-read_csv("txn_cors2000.csv")
BC2000x<-BC2000 %>% mutate(names = taxon) #create column to match up 
#the original taxon names in the 2000s data to the original taxon names in the 
#2010 data
BC2000cor<-merge(BC2000x, txn_cors, by="names", all.x=TRUE) %>%
  select(-names)
BC2000cor<-BC2000cor %>% filter(!is.na(species)) #removing any records not ID'd 
#to species

###OLD METHODS using other data sets not worms:
#BC2000x<-BC2000 %>% mutate(TAXON_ORIGINAL = taxon) #create column to match up 
##the original taxon names in the 2000s data to the original taxon names in the 
##2010 data
#txn_cors<-BC2010a %>%
#  select("TAXON_ORIGINAL", "TAXON_CORRECTED", "WORMS_APHIAID") #taxonomic 
#txn_cors<-unique(txn_cors) #to remove any possibly duplicated info
##corrections applied to 2010 data
#BC2000cor<-merge(BC2000x, txn_cors, by="TAXON_ORIGINAL", all.x=TRUE) #matching 
##up the corrections applied to the 2010 data to the 2000s data
#
## CHECK out the corrections that were applied:
##BC2000cor %>% select("TAXON_ORIGINAL", "taxon", 
##                     "TAXON_CORRECTED", "WORMS_APHIAID") %>%
##  filter(taxon != TAXON_CORRECTED)
##nrow(unique(BC2000))
##nrow(unique(BC2000x))
##nrow(unique(BC2000cor))
##nrow(BC2000)
##nrow(BC2000cor)
##nrow(BC2000x)

#2010
wids<-as.wormsid(unique(BC2010a$WORMS_APHIAID[!is.na(BC2010a$WORMS_APHIAID)])) 
#slow! but not as bad as when you have to convert from common names
species<-species_from_wids(wids)
txn_cors<-data_frame(species, wids) %>% mutate(WORMS_APHIAID=as.character(wids))
#create column to match up the wids
BC2010acor<-merge(BC2010a, txn_cors, by="WORMS_APHIAID", all.x=TRUE)
BC2010acor<-BC2010acor %>% filter(!is.na(species)) #removing any records not ID'd 
#to species

#2015
wids<-as.wormsid(unique(BC2015$APHIA_ID[!is.na(BC2015$APHIA_ID)])) 
#slow! but not as bad as when you have to convert from common names
species<-species_from_wids(wids)
txn_cors<-data_frame(species, wids) %>% mutate(APHIA_ID=as.character(wids))
#create column to match up the wids
BC2015cor<-merge(BC2015, txn_cors, by="APHIA_ID", all.x=TRUE)

#2020
wids<-as.wormsid(unique(BC2020$APHIA_ID[!is.na(BC2020$APHIA_ID)])) 
#slow! but not as bad as when you have to convert from common names
species<-species_from_wids(wids)
txn_cors<-data_frame(species, wids) %>% mutate(APHIA_ID=as.character(wids))
#create column to match up the wids
BC2020cor<-merge(BC2020, txn_cors, by="APHIA_ID", all.x=TRUE)
################################################################################

# Step 5: creating our dataset by extracting and calculating the common data 
# columns of interest to us and assembling one complete dataset to work with.
# GOAL: assemble a dataset with all the necessary data and the following columns
# 1) siteID
# 2) density_m2
# 3) lon
# 4) lat
# 5) year
# 6) WoRMS_aphiaID
# 7) species
################################################################################
BC2000cor 
#getting the info we care about: 
#1) siteID = SITE_ID 
#2) density_m2 = num_m2
#3) lon = LON_DD 
#4) lat = LAT_DD 
#5) year = SAMPYEAR 
#6) WoRMS_aphiaID = wids
#7) species
BC2000.sub<-BC2000cor %>% 
  mutate(siteID = SITE_ID,
         density_m2 = tot_num_m2,
         lon = LON_DD,
         lat = LAT_DD,
         year = SAMPYEAR,
         WoRMS_aphiaID = wids) %>%
  select(siteID, density_m2, lon, lat, year, WoRMS_aphiaID, species)
# NOTE!: 3 observations are no longer unique....not sure why but that should not 
# happen... so removing them for now...
nrow(unique(BC2000cor))
nrow(unique(BC2000.sub))
BC2000.sub<-unique(BC2000.sub)


BC2010acor 
#getting the info we care about: 
#1) siteID = SITE_ID.x 
#2) density_m2 = TOTAL/Final_Grab_Area(m2)
#3) lon = ALON_DD
#4) lat = ALAT_DD
#5) year = 2010
#6) WoRMS_aphiaID = wids
#7) species = species
# also worth noting that the map datum for the coordinates is also included in 
# this data and confirms it as WGS84 which we will be using for our maps
# need to fix the final grab area column's name to comply with R's column naming 
#conventions: 
colnames(BC2010acor)<-c(colnames(BC2010acor[,1:34]), "FINAL_GRAB_AREA_m2", 
                     colnames(BC2010acor[,36:length(colnames(BC2010acor))]))
# totaling counts of a given taxa ID within each grab (since we don't care about
#treating life stages or condition, etc. differently but the data for this year 
#are divided into these additional sub categories)
BC2010atot<-BC2010acor %>% group_by(UID, SITE_ID, DATE_COL, SAMPLE_ID, 
                               FINAL_GRAB_AREA_m2, VISIT_NO, ALON_DD, ALAT_DD, 
                               species, wids) %>%
  summarise(total=sum(TOTAL)) %>% ungroup()
BC2010a.sub<-BC2010atot %>% mutate(siteID = SITE_ID,
                                density_m2 = total/FINAL_GRAB_AREA_m2,
                                lon = ALON_DD,
                                lat = ALAT_DD,
                                year = 2010,
                                WoRMS_aphiaID = wids) %>%
  select(siteID, density_m2, lon, lat, year, WoRMS_aphiaID, species)

BC2015cor
#getting the info we care about: 
#1) siteID = SITE_ID
#2) density_m2 = TOTAL/FINAL_AREA_GRAB ***need area of grabs for this data***
#3) lon = LON_DD
#4) lat = LAT_DD
#5) year = YEAR
#6) WoRMS_aphiaID = APHIA_ID
#7) species = species
BC2015.sub<-BC2015cor %>% mutate(siteID = SITE_ID,
                              density_m2 = TOTAL/as.numeric(FINAL_GRAB_AREA),
                              lon = LON_DD,
                              lat = LAT_DD,
                              year = YEAR,
                              WoRMS_aphiaID = wids) %>%
  select(siteID, density_m2, lon, lat, year, WoRMS_aphiaID, species)


BC2020cor
#getting the info we care about: 
#1) site_ID = SITE_ID
#2) density_m2 = DENSITY
#3) lon = LON_DD 
#4) lat = LAT_DD
#5) year = 2020
#6) WoRMS_aphiaID = APHIA_ID
#7) species = species
BC2020.sub<-BC2020cor %>% mutate(siteID = SITE_ID,
                              density_m2 = DENSITY,
                              lon = LON_DD,
                              lat = LAT_DD,
                              year = 2020, 
                              WoRMS_aphiaID = wids) %>%
  select(siteID, density_m2, lon, lat, year, WoRMS_aphiaID, species)

#putting it all together:
all.data<-rbind(BC2000.sub, BC2010a.sub, BC2015.sub, BC2020.sub)
################################################################################

# Step 6: saving all the data we want to work with
################################################################################
write_csv(all.data, "processed_benthic_data_all_years.csv")
################################################################################


###OLD METHODS using other data sets not worms:
## Step 6: adding the taxa names from the data associated with their aphiaID from 
## WoRMS and keeping only observations ID'd to species like in the Hale paper.
## NOTE: I wanted to match the taxonomy from WoRMS to the aphiaIDs and look for 
## any discrepencies between the taxa names provided and the aphiaIDs to confirm
## these are all up to date and should cross reference appropiately with 
## SeaLifeBase for attatching the trait data but I can't seem to get any of the 
## necessary functions for this to work (see Step 7) so I'm just using the 
## corrected taxa names as provided in the data we have for now.
#################################################################################
#txn_cors<-txn_cors %>% mutate(WoRMS_aphiaID = WORMS_APHIAID)
#all.data.sp<-merge(all.data,txn_cors, by = "WoRMS_aphiaID") # get corrected 
##names for the 
#
#name1<-rep(NA, length(unique(all.data.sp$TAXON_CORRECTED)))
#name2<-rep(NA, length(unique(all.data.sp$TAXON_CORRECTED)))
#for (i in 1:length(unique(all.data.sp$TAXON_CORRECTED))){
#  name1[i]<-str_split(unique(all.data.sp$TAXON_CORRECTED)[i], " ")[[1]][1]
#  name2[i]<-str_split(unique(all.data.sp$TAXON_CORRECTED)[i], " ")[[1]][2]
#}
#taxa<-tibble(unique(all.data.sp$TAXON_CORRECTED), name1, name2)
#colnames(taxa)<-c("TAXON_CORRECTED", "name1", "name2")
#all.data.sp<-merge(all.data.sp, taxa, by="TAXON_CORRECTED")
#
##exclude observations not ID'd to species
#sp.data<-all.data.sp %>% filter(!is.na(name2)) %>% 
#  select(TAXON_CORRECTED, WoRMS_aphiaID, siteID, density_m2, lon, lat, year, 
#         name1, name2)
#colnames(sp.data)<-c("full_name", "WoRMS_aphiaID", "siteID", "density", "lon", 
#                     "lat", "year", "genus", "species")
#sp.data
#################################################################################
#
## Step 7: Confirm taxanomy and combine data with traits in SeaLifeBase
## NOTE: not having any luck with this...jumping to step 8 for now to work with 
## what we've got
################################################################################
#slb_accepted <- species(server = "sealifebase", fields=c("SpecCode", "Species"))
## ********TRYING TO MATCH UP SPECIES IN THE EARLY 2000s DATA WITH EITHER WORMS OR SEALIFEBASE!!! NOT WORKING!!!************
#BC2000cor %>% select(TAXON_CORRECTED, WORMS_APHIAID) %>% unique() #to look at corrected names and their supposed AphiaIDs in WoRMS
#BC2000cor %>% select(TAXON_CORRECTED) %>% unique() %>% validate_names() #trying to match sealifebase gives us nothing!!!
#validate_names("Acanthohaustorius intermedius") #trying name by name I'm getting nothing either....
## searching this species name directly in sealifebase.org's online portal I get a result though...
#wormsIDs2000<-BC2000cor %>% select(WORMS_APHIAID) %>% unique() #extracting the IDs...
#wormsIDs2000
#worms_downstream(id=wormsIDs2000[,1], downto="species") #...and searching for them in WoRMS also is giving me nothing
#worms_downstream(id=158014, downto="species")
#downstream(sci_id=158014, db = 'worms', downto = 'genus')
#################################################################################
#
## Step 8: saving all the data we want to work with
#################################################################################
#write_csv(sp.data, "processed_benthic_data_all_years.csv")
##################################################################################








