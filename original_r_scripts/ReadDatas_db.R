library(stringr)
library(lubridate)

#lectures des datas initiales en Db
Nb_Pop<-14 #Modifier le nombre final de fichier, ici 14

AOE_db<-c()
for (i in 1:Nb_Pop)
{
  new<-read.csv2(paste0("datas/AOEspectraDB (",i,").csv"),sep=",",dec=".", header=F)
  AOE_db<-rbind(AOE_db,new)
}

NOISE_db<-c() #bruit calculé (s1-s2)/2
for (i in 1:Nb_Pop)
{
  new<-read.csv2(paste0("datas/AOEspectraDBnoise (",i,").csv"),sep=",",dec=".", header=F)
  NOISE_db<-rbind(NOISE_db,new)
}

NOISEb_db<-c() #bruit fourni par le constructeur
for (i in 1:Nb_Pop)
{
  new<-read.csv2(paste0("datas/AOEspectraSOAE (",i,").csv"),sep=",",dec=".", header=F)
  NOISEb_db<-rbind(NOISEb_db,new)
}

#Labels
label<-read.table("datas/Freq.txt")
colnames(AOE_db)<-c("ind","code","side","sub","stim",as.vector(t(label)))
colnames(NOISE_db)<-c("ind","code","side","sub","stim",as.vector(t(label)))

#Pop # EXTRAIT LE NOM DE LA POPULATION EN PRENANT LES 3 PREMIERES LETTRE DU NOM DE L'INDIVIDU EN PREMIERRE COLONNE
pop<-substr(AOE_db[,1],1,3)
AOE_db<-cbind(pop,AOE_db)
NOISE_db<-cbind(pop,NOISE_db)
NOISEb_db<-cbind(pop,NOISEb_db)

#Anthropo et selection âge
anthr<-c()
for (i in 1:Nb_Pop)
{
  new<-read.csv2(paste0("datas/anthropo-data (",i,").csv"),sep=";",dec=".", header=T)
  anthr<-rbind(anthr,new)
}

anthr$birth_date<-str_sub(anthr$birth_date,4,13)

#Date de référence pour le calcul de l'âge
date_ref<-ymd("2019-12-31") 

#Calcul des âges
anthr$age<-as.numeric(as.duration(as.period(date_ref-ymd(anthr$birth_date))))/(60*60*24*365)

#Selection des individus à partir de l'âge
select<-anthr[(anthr$age<55)& (anthr$age>18),]$tube_code
match<-match(AOE_db$ind,select)

AOE_db<-AOE_db[!is.na(match),]
NOISE_db<-NOISE_db[!is.na(match),]
NOISEb_db<-NOISEb_db[!is.na(match),]
  
#classification Pop SI NECESSAIRE
classif<-read.csv2("datas/ClassificationPop.csv",sep=";",dec=".", header=T)


#--------------------------------------------------
#REtrait des individus considérés comme mal mesurés
#--------------------------------------------------
#ATTENTION, A N'EXECUTER QUE SI DES PROFILS ATYPIQUES ONT 2T2 DETECT2S ET DONC MODIFIER LA LISTE DES IDENTIFIANTS EN CONSEQUENCE
list_excl<-c("X3ST8FE1.DTA","X3ST8FD9.DTA","X3ST8G38.DTA","X3ST8G42.DTA","X3ST8G44.DTA","X3ST8G41.DTA",
"X3STA9E6.DTA","X3STA9E5.DTA","X3STACA2.DTA","X3STAD66.DTA","X3STAA49.DTA","X3STAA46.DTA","X3SS5QA2.DTA")

vect_excl<-c()

for (i in list_excl)
{
  c<-which(AOE_db$code==i)
  vect_excl<-c(vect_excl,c)
}

AOE_db<-AOE_db[-vect_excl,]
NOISE_db<-NOISE_db[-vect_excl,]
NOISEb_db<-NOISEb_db[-vect_excl,]




