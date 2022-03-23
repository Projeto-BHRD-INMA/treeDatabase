#exploring all tratis and LT tratis

library(dplyr)

traits<-read.csv("./data/all_traits_withLT.csv") # all traits
lfe<-read.csv("./data/Leaf_MA_mean_fixed.csv") #so os Leaf traits para as spp que ocorrem na BHRD


na_count <-sapply(lfe, function(y) sum(length(which(is.na(y)))))
#should give you a list with the counts for each column.

#transform into df
na_count <- data.frame(na_count)

#dentre essas que temos dados de LT, como sao os outros traits?:
tri<-read.csv(file = "data/all_traits.csv", sep = ",", dec = ".", header = T) #sao os traits sem os LT

ex<-lfe%>%
  left_join(tri, by =c("scientificName.new"="search.str"))

na_count <-sapply(ex, function(y) sum(length(which(is.na(y)))))
#should give you a list with the counts for each column.

#transform into df
na_count <- data.frame(na_count)

#nao tá ruim... podemos começar com essas que temos mais informaçoes!!!

write.table(ex, "./data/recorte1.csv", row.names=F, sep=",", dec=".")


