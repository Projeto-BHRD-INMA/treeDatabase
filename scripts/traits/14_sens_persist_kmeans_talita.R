#fazendo sensitivity to persist in place.
#usando k-means clustering... baseado em Aubin, Boisvert, Royer-Tardiff
#k-means used only for traits(variables) that are continuous (not categorical)

library(dplyr)

ex<-read.csv("./data/recorte1.csv")

str(ex)

ex$LMA<-as.numeric(ex$LMA)
ex$MaxHeight_m<-as.numeric(ex$MaxHeight_m)
ex$SeedMass_g<-as.numeric(ex$SeedMass_g)

r1<-ex%>%
  select(LMA, Seed.type, seeddiam, dispersal.syndrome, wooden, SeedMass_g, MaxHeight_m, scientificName.new)

library(tidyr)
r1<-r1%>%
  drop_na()

length(unique(r1$scientificName.new))


#esse é agora (r1) o numero de spp que tem info para todos os traits que precisamos usar. sao 266 spp.
#mas agora vamos usar, a partir desse recorte, so as q sao usadas para restauração

rest<-read.csv("./data/restaurar_MA_BHRD.csv")

ex2<- rest%>%
  left_join(r1, by =c("scientificName.new"="scientificName.new"))

#tem spp duplicada. tirar:

length(unique(ex2$scientificName.new))
dups2 <- duplicated(ex2[, "scientificName.new"])
sum(dups2)
ex2 <- ex2[!dups2, ]

write.table(ex2, "./data/recorte2.csv", row.names=F, sep=",", dec=".")

ex2<-read.csv("./data/recorte2.csv")

#for WOOD DENSITY
wood<-ex2%>%
    select(scientificName.new, wooden)

set.seed(20)
clusters <- kmeans(wood[,2], 5)
wood$cluster <- as.factor(clusters$cluster)

c1<-wood%>%
  filter(cluster=="1")%>%
  arrange(wooden)%>%
  mutate(class="med-high")%>%
  mutate(score.w="2")

c2<-wood%>%
  filter(cluster=="2")%>%
  arrange(wooden)%>%
  mutate(class="med-low")%>%
  mutate(score.w="4")

c3<-wood%>%
  filter(cluster=="3")%>%
  arrange(wooden)%>%
  mutate(class="low")%>%
  mutate(score.w="5")

c4<-wood%>%
  filter(cluster=="4")%>%
  arrange(wooden)%>%
  mutate(class="med")%>%
  mutate(score.w="3")

c5<-wood%>%
  filter(cluster=="5")%>%
  arrange(wooden)%>%
  mutate(class="high")%>%
  mutate(score.w="1")

cc<-bind_rows(c1, c2)
ca<-bind_rows(c3,c4)
cb<-bind_rows(cc, ca)
wood_all<-bind_rows(cb, c5)

#for the purpose of our ranking... high and med-high wood density are TOLERANT. Low and med-low wood density are sensitive. and MEDIUM is MEDIUM...?

# FOR LMA
lma<-ex2%>%
  select(scientificName.new, LMA)%>%
  filter(!(scientificName.new=="Plinia rivularis")) # pq aqui ela é outlier e bagunça o clustering, mas o SLA dela é super HIGH...

set.seed(20)
clusters2 <- kmeans(lma[,2], 5)
lma$cluster <- as.factor(clusters2$cluster)

l1<-lma%>%
  filter(cluster=="1")%>%
  arrange(LMA)%>%
  mutate(class="low")%>%
  mutate(score.l="5")

l2<-lma%>%
  filter(cluster=="2")%>%
  arrange(LMA)%>%
  mutate(class="High")%>%
  mutate(score.l="1")

l3<-lma%>%
  filter(cluster=="3")%>%
  arrange(LMA)%>%
  mutate(class="med-high")%>%
  mutate(score.l="2")

l4<-lma%>%
  filter(cluster=="4")%>%
  arrange(LMA)%>%
  mutate(class="med~low")%>%
  mutate(score.l="4")

l5<-lma%>%
  filter(cluster=="5")%>%
  arrange(LMA)%>%
  mutate(class="med")%>%
  mutate(score.l="3")


lc<-bind_rows(l1, l2)
la<-bind_rows(l3,l4)
lb<-bind_rows(lc, la)
LMA_all<-bind_rows(lb, l5)

#for plant height
alt<-ex2%>%
  select(scientificName.new, MaxHeight_m)

set.seed(20)
clusters3 <- kmeans(alt[,2], 5)
alt$cluster <- as.factor(clusters3$cluster)

a1<-alt%>%
  filter(cluster=="1")%>%
  arrange(MaxHeight_m)%>%
  mutate(class="low")%>%
  mutate(score.a="1") #aqui o low é 1 pq é o menos sensível

a2<-alt%>%
  filter(cluster=="2")%>%
  arrange(MaxHeight_m)%>%
  mutate(class="high")%>%
  mutate(score.a="5")

a3<-alt%>%
  filter(cluster=="3")%>%
  arrange(MaxHeight_m)%>%
  mutate(class="med-high")%>%
  mutate(score.a="4")

a4<-alt%>%
  filter(cluster=="4")%>%
  arrange(MaxHeight_m)%>%
  mutate(class="med")%>%
  mutate(score.a="3")

a5<-alt%>%
  filter(cluster=="5")%>%
  arrange(MaxHeight_m)%>%
  mutate(class="med-low")%>%
  mutate(score.a="2")

ac<-bind_rows(a1, a2)
aa<-bind_rows(a3,a4)
ab<-bind_rows(ac, aa)
alt_all<-bind_rows(ab, a5)

# SEED TYPE
seed<-ex2%>%
  select(scientificName.new, Seed.type)

s1<-seed%>%
  filter(!(Seed.type=="recalcitrant"))%>%
  mutate(score.s="1")

s2<-seed%>%
  filter(Seed.type=="recalcitrant")%>%
  mutate(score.s="5")

seed_all<-bind_rows(s1,s2)

# Dipsersal
Disp<-ex2%>%
  select(scientificName.new, dispersal.syndrome)
Disp$dispersal.syndrome<-as.factor(Disp$dispersal.syndrome)

d1<-Disp%>%
  filter(dispersal.syndrome=="autochoric")%>%
  mutate(score.d="5")

d2<-Disp%>%
  filter(dispersal.syndrome=="anemochoric")%>%
  mutate(score.d="3")

d3<-Disp%>%
  filter(dispersal.syndrome=="zoochoric")%>%
  mutate(score.d="1")

dd<-bind_rows(d1,d2)
disp_all<-bind_rows(dd, d3)

#for Seed mass - to achando estranho e nao vou usar no momento.

mass<-ex2%>%
  select(scientificName.new, SeedMass_g)%>%
  mutate(seed_mass_mg=(SeedMass_g)*1000)%>%
  filter(!(scientificName.new=="Pouteria venosa")) # pq aqui ela é outlier e bagunça o clustering, seed mass super HIGH...

set.seed(20)
clusters4 <- kmeans(mass[,2], 5)
mass$cluster <- as.factor(clusters4$cluster)

m1<-mass%>%
  filter(cluster=="1")%>%
  arrange(SeedMass_g)%>%
  mutate(class="med-high")%>%
  mutate(score.m="2")

m2<-mass%>%
  filter(cluster=="2")%>%
  arrange(SeedMass_g)%>%
  mutate(class="med-high")%>%
  mutate(score.m="2")

m3<-mass%>%
  filter(cluster=="3")%>%
  arrange(SeedMass_g)%>%
  mutate(class="med-high")%>%
  mutate(score.m="2")

m4<-mass%>%
  filter(cluster=="4")%>%
  arrange(SeedMass_g)%>%
  mutate(class="med-high")%>%
  mutate(score.m="2")

m5<-mass%>%
  filter(cluster=="5")%>%
  arrange(SeedMass_g)%>%
  mutate(class="med-high")%>%
  mutate(score.m="2")

#juntando todas para entao fazer a media
wood_all2<-wood_all%>%
  select(scientificName.new, score.w)
LMA_all2<-LMA_all%>%
  select(scientificName.new, score.l)
alt_all2<-alt_all%>%
  select(scientificName.new, score.a)
seed_all2<-seed_all%>%
  select(scientificName.new, score.s)
disp_all2<-disp_all%>%
  select(scientificName.new, score.d)

xx<-wood_all2%>%
  left_join(LMA_all2, by =c("scientificName.new"="scientificName.new"))

yy<-seed_all2%>%
  left_join(alt_all2, by =c("scientificName.new"="scientificName.new"))

all<-xx%>%
  left_join(yy, by =c("scientificName.new"="scientificName.new"))


all<-read.csv("./data/all_scores_persistNEW.csv")

all<-all%>%
  select(scientificName.new, score.w, score.l, score.s, score.a)

all2<-all%>%
  left_join(disp_all2, by =c("scientificName.new"="scientificName.new"))


write.table(all2, "./data/all_scores_persistNEW.csv", row.names=F, sep=",", dec=".")
