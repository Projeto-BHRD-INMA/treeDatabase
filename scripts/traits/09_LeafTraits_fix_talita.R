#olhando os dados de atributos foliares que vieram do LT-Brazil (Mariano et al. 2021); e juntando com os traits que temos dos outros bancos de dados...

library(dplyr)

lf<-read.csv2("./data/Dataset_LF.csv")
#essa é a planilha original

nati<-read.csv("./data/02_data_leaf_MA.csv")
#essa é a planilha da Nati


#para fix o nome das spp. a planilha original veio com nome dos autores. o PlantR tem uma função que arruma, ela cria uma nova coluna com o nome das spp sem os autores.

remotes::install_github("LimaRAF/plantR")
library("plantR")

lf<-lf%>%
  mutate(scientificName=Species)

lf2<-fixSpecies(lf)#essa é a função.

nati<-nati%>%
  mutate(scientificName=Species)

nati2<-fixSpecies(nati)
#### refazendo como nati fez.. ela usou o filtro pra MA primeiro.

length(unique(nati2$scientificName.new)) #filtrando a planilha para MA primeiro, temos 589 spp na lista

# mas qtas dessas estao na nossa lista da bacia?

traits<-read.csv(file = "data/all_traits.csv")

ocorre_p<-which(traits$search.str %in% nati2$scientificName.new)

whoy<-traits$search.str[ocorre_p] #quem ocorre
whoy<-as.data.frame(whoy)

convert_dataw = whoy[['whoy']]
print(convert_dataw)

nati_tr<-nati2%>%
  filter(scientificName.new %in% convert_dataw)

length(unique(nati_tr$scientificName.new)) #sao 326 spp


# agora SEM FILTRAR para MA primeiro. usando os dados brutos...
ocorre<-which(traits$search.str %in% lf2$scientificName.new)

whon<-traits$search.str[-ocorre] #quem nao ocorre
whon<-as.data.frame(whon)

whos<-traits$search.str[ocorre] #quem ocorre
whos<-as.data.frame(whos)

convert_data1 = whos[['whos']]
print(convert_data1)

tr<-lf2%>%
  filter(scientificName.new %in% convert_data1)

length(unique(tr$scientificName.new)) #sao 494 spp
#entao temos mais matches...

write.table(tr, "./data/LT_BHRD.csv", row.names=F, sep=",", dec=".")#para ter a tabela dos dados de LT brutos para as spp da BHRD, sem tirar as medias. importante para dados tipo MAP, MAET, etc... bredth, para ter o max e min.

#agora tem q fazer as medias por spp dessas 494 spp e dai juntar

#first
str(tr)

#making them as.numeric
cols.num <- c("MAT", "MAP", "MAET","AI", "Elev", "pH", "OC", "CEC", "Clay", "Silt", "Sand", "NPP", "LMA", "Nmass", "Pmass", "NPratio" )
tr[cols.num] <- sapply(tr[cols.num],as.numeric)
sapply(tr, class)

#fazendo as medias para todas as colunas
mean_LT_BHRD <- tr%>%
  group_by(scientificName.new) %>%
  select(MAT:NPratio) %>%
  summarise(
    MAT = mean(MAT, na.rm = TRUE),
    MAP = mean(MAP, na.rm = TRUE),
    MAET = mean(MAET, na.rm = TRUE),
    AI = mean(AI, na.rm = TRUE),
    Elev = mean(Elev, na.rm = TRUE),
    pH = mean(pH,  na.rm = TRUE),
    OC = mean(OC, na.rm = TRUE),
    TN = mean(TN, na.rm = TRUE),
    CEC = mean(CEC, na.rm = TRUE),
    Clay = mean(Clay, na.rm = TRUE),
    Silt = mean(Silt, na.rm = TRUE),
    Sand = mean(Sand,  na.rm = TRUE),
    NPP = mean(NPP, na.rm = TRUE),
    LMA = mean(LMA, na.rm = TRUE),
    Nmass = mean(Nmass, na.rm = TRUE),
    Pmass = mean(Pmass, na.rm = TRUE),
    NPratio = mean(NPratio, na.rm = TRUE)
  )
warnings()

# left join com as spp da BHRD pra compor uma planilha só.

BHRD_all<-traits%>%
left_join(mean_LT_BHRD, by =c("search.str"="scientificName.new"))

write.table(BHRD_all, "./data/all_traits_withLT.csv", row.names=F, sep=",", dec=".")

write.table(mean_LT_BHRD, "./data/Leaf_MA_mean_fixed.csv", row.names=F, sep=",", dec=".")


