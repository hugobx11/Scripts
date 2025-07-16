#SCRIPT DEDIE A LA COMPARAISON DES PROFILS COMPLETS PAR INDIVIDU ET PAR SIDE
#Objectif identifier les profils qui se correlent mal pour un indvidu et une side donnée
# Permet de ='éliminer les profils suspects

#Creation du tableau de valeurs uniquement pour les cross-correlations
AOE_db_mes<-AOE_db[,-c(1:6)]
NOISE_db_mes<-NOISE_db[,-c(1:6)]

var_db<-AOE_db[,c(1:6)]
SNR_mes<-AOE_db_mes-NOISE_db_mes

#Fichier SNR total
SNR<-cbind(var_db,SNR_mes)

#Selection des individus non sous-stimulés uniquement
AOE_db_mes<-AOE_db_mes[var_db$sub=="false",]
AOE_db_r<-AOE_db[var_db$sub=="false",]
NOISE_db_mes<-AOE_db_mes[var_db$sub=="false",]
SNR_mes<-SNR_mes[SNR$sub=="false",]
var_db<-var_db[var_db$sub=="false",]

#Exriture du tableau final pour utilisation ultéerieure Excel
write.table(AOE_db_r,paste("AOE_db_f.csv"),sep =";", dec = ".",row.names = FALSE)



ccf_list<-list()
comb_list<-list()
ind_list<-levels(as.factor(var_db[,2])) #liste des individus
index<-1

for (i in ind_list) #boucle sur les individus
{
  #Selection des audiagrammes de chaque side pour chaque individu
  tab_L<-AOE_db_mes[(var_db$ind==i & var_db$side=="L"),]
  tab_R<-AOE_db_mes[(var_db$ind==i & var_db$side=="R"),]
  
  #identification de toutes les combinaisons entre les courbes selectionnées
    # pour les L
  if (length(tab_L[,1])>1)
  {
    L<-t(combn(seq(1:length(tab_L[,1])),2)) #combinaisons possibles entre audiogrammes 2 à 2
    cc<-c()
    cb<-c()
    for (l in 1:length(L[,1])) #Calcul Cross-Correlation popur chaque combinaison
      {
        t<-ccf(as.vector(t(tab_L[L[l,1],])),as.vector(t(tab_L[L[l,2],])),plot=FALSE)
        cc<-c(cc,t$acf[which(t$lag==0)]) #decalage = 0
        cb<-c(cb,paste(L[l,1],L[l,2]))
      }
  ccf_list<-append(ccf_list,list(cc))
  comb_list<-append(comb_list,list(cb))
  names(ccf_list)[index]<-paste(i,"L")
  names(comb_list)[index]<-paste(i,"L")
  
  index=index+1
  }

  #Pour les R
  if (length(tab_R[,1])>1)
  {  
    R<-t(combn(seq(1:length(tab_R[,1])),2))#combinaisons possibles entre audiogrammes 2 à 2
    cc<-c()
    cb<-c()
    for (l in 1:length(R[,1]))  #Calcul Cross-Correlation popur chaque combinaison
      {
        t<-ccf(as.vector(t(tab_R[R[l,1],])),as.vector(t(tab_R[R[l,2],])),plot=FALSE)
        cc<-c(cc,t$acf[which(t$lag==0)])
        cb<-c(cb,paste(R[l,1],R[l,2]))
      }
  
    ccf_list<-append(ccf_list,list(cc))
    comb_list<-append(comb_list,list(cb))
   names(ccf_list)[index]<-paste(i,"R")
   names(comb_list)[index]<-paste(i,"R")
   
   index=index+1
   
  }
}

#Sélection des croscorrelation inférieures à un seuil tresh
tresh<-0.7
ID<-as.vector(t(which(sapply(ccf_list,min)<tresh)))
ano_list<-list()
index<-1
temp<-c()
for (j in ID)
{
  idex<-which(ccf_list[[j]]<tresh)
  ano_list<-append(ano_list,list((comb_list[[j]][idex])))
  names(ano_list)[index]<-names(ccf_list[j])
  index<-index+1
}


ccf_list[[924]]
names(ccf_list[924])

length(ano_list)
ano_list #contient tous les valeurs de profils non suffisamment corrélés par rapport au seuil. Le numéro correspond 
# à l'ordre d'apparition du profil our un individu/side donné qu'on retrouve dans le fichier csv créé plus haut

