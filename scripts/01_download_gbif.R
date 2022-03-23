################################################################################
###                                                                          ###
###                TUTORIAL TO DOWNLOAD OCCURRENCES FROM GGBIF               ###
###                     USING A LIST OF KNOWN NAMES OF SPECIES               ###
###https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/   ###
###                                                                          ###
### Created by Daphne Spier in 03 set 2020                                   ###
### Modified by Danielle Moreira in 22 mar 2022                              ###
###                                                                          ###
################################################################################


library(dplyr)
library(purrr)
library(readr)
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize) # for get_gbifid_

#install.packages("remotes")
#remotes::install_github("ropensci/taxize")
if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, purrr, readr, magrittr, rgbif)
library(taxize)

#The important part here is to use rgbif::occ_download with pred_in and to fill in your gbif credentials.

# fill in your gbif.org credentials. You need to create an account at gbif if you don't have it.

user <- "XXX" # your gbif.org username
pwd <- "XXXX" # your gbif.org password
email <- "XXX@gmail.com" # your email

#############################################################################

oc <- read.csv("/home/danimoreira/01_INMA_BHRD/bancoDados/listas/lista_especies_BHRD_correta.csv", sep = ';')
names(oc)

gbif_taxon_keys <-
  oc %>% 
  #read.csv("./data/registros/spp_Gualaxo/1_spp_gualaxo.csv", sep = ';') %>% #For an file with a list of spp names
  pull(species) %>% #Specify the column from the list
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(path = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  filter(kingdom == "Plantae") %>% # remove anything that might have matched to a non-plant
  pull(usagekey) # get the gbif taxonkeys

# gbif_taxon_keys should be a long vector like this c(2977832,2977901,2977966,2977835,2977863)
# !!very important here to use pred_in!!


# use matched gbif_taxon_keys from above
occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN')),
  pred("country", "BR"),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 1950),
  format = "SIMPLE_CSV",
  #pred("geometry","POLYGON((-43.86 -17.57, -43.88 -21.49, -39.79 -19.86, -39.46 -17.98, -43.86
  #     -17.57))"),
  #pred("continent", "South America"),
  user=user,pwd=pwd,email=email
)

#to check the downloaded list
d <- occ_download_get('0191174-210914110416597') %>%
  occ_download_import()


# Read the species records points and Check taxonomy -----------------------------------------

## I use the comand fread() from data.table package because with read.delim or read.csv, R is not reading all rows

library(data.table)
#?fread
spp.table <- fread("./results/gbif/gbif-BHRD-all-0191124-210914110416597.csv")

#number of species
list <- unique(spp.table$species)

#Check names
#install.packages("devtools")
#install_github("gustavobio/flora")
library(flora)

check <- get.taxa(list)

write.csv(check, "./results/list_gbif_check_0191124.csv")
