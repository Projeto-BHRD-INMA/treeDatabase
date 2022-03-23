
library("remotes")
install_github("LimaRAF/plantR")
library("plantR")


library(data.table)
#?fread
spp.gbif <- fread("./results/gbif-BHRD-all-0191124-210914110416597.csv")

#gbif <- read.delim("D:/01_INMA_BHRD/GDM/Data/Biotic/gbif_download.csv")

rgbif()

names(spp.gbif)
occs <- formatDwc(gbif_data = spp.gbif,
                  country = "countryCode"
                  )

#data editing
occs<-formatOcc(occs)
occs<-formatLoc(occs)
occs<-formatCoord(occs)
occs<-formatTax(occs)

#data validation
occs<-validateLoc(occs)
occs<-validateCoord(occs)
occs<-validateTax(occs)
occs<-validateDup(occs)

#data summary

summary<- summaryData(occs)
flags<- summaryFlags(occs)
checklist<-checklist(occs)


############### Tutorial #####################################
occs_splink <- rspeciesLink(species = "Euterpe edulis")

occs_gbif <- rgbif2(species = "Euterpe edulis")

names(occs_gbif)

occs <- formatDwc(splink_data = occs_splink,
                  gbif_data = occs_gbif)

occs <- formatOcc(occs)
occs <- formatLoc(occs)
occs <- formatCoord(occs)
occs <- formatTax(occs)


#############################################################

spp <- fread("D:/01_INMA_BHRD/bancoDados/species_gbif_MG_ES_20_08_2020.csv")

spp2 <- subset(spp.gbif.table, select=c("V1", "family", "scientificName", ))


prepDup(spp.gbif.table,
        col.names = c(family = "family", species = "scientificName", col.name = "recordedBy", col.last.name = "last.name", col.number = "recordNumber", col.year = "year", col.loc = "locality", loc.str = "loc.correct"), 
        comb.fields = list(c("family", "col.last.name", "col.number", "col.loc"), 
                           c("family", "col.year", "col.number", "col.loc"), 
                           c("species", "col.last.name", "col.number", "col.year"), 
                           c("col.year", "col.last.name", "col.number", "col.loc")),
        rec.ID = "numTombo",
        noYear = "s.d.",
        noName = "s.n.",
        noNumb = "s.n.",
        ignore.miss = TRUE
)

as.data.frame(spp)

is.data.frame(spp)

getDup(spp)


library(LimaRAF/plantR)

df <- data.frame(id=c("a_1","b_3","c_7","d_5","e_3",
                      "f_4","g_2","h_8","i_9","j_6","k_7","l_1"),
                 str1=c("a","b","c","l","l","p","p","p",NA,NA,"x","y"),
                 str2=c("d","d","e","k","k","o","o","o",NA,NA,"v","w"),
                 str3=c("f","g","f","n","n","s","r","s","t","t","z","u"),
                 str4=c("h","i","j","m","m","q","q","q",NA,NA,"ab","ac"))

getDup(df)
