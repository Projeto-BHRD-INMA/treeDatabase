#Using plantR with our data from gbif.

gbif<-read.csv2("./data/gbif_3.csv")

remotes::install_github("LimaRAF/plantR")
library("plantR")

occs <- formatDwc(gbif_data = gbif)

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

saveData(occs)

#write.table(occs, "./data/occs_saved.csv", row.names=F, sep=",", dec=".")


