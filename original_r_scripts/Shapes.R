#CALCULS DES CARACTERISTIQUES DES AUDIOGRAMMES
#TABLEAU DE SORTIES
# synth_table : contient les caractéristiques par audiogramme (replicats)
# synth_table_av_rep : contient les caractéristiques moyennes ind/side calculées sur les 
#caractéristiques par replicat
# audio_av: audiogramme moyens
# synth_table_av_moy: contient les caractéristiques moyennes ind/side calculées sur les 
#audiogrammes moyens


#--------------------------------------------
                   #PREPARATION
#--------------------------------------------
#Creation du tableau de valeurs popur le traitement numérique

AOE_db_mes<-AOE_db[,-c(1:6)]
NOISE_db_mes<-NOISE_db[,-c(1:6)]

#Selec réduite à la plage Q1(SNR)>3
 lim1<-which(colnames(AOE_db_mes)=="546.88") #limite basse de la plge de selection
 lim2<-which(colnames(AOE_db_mes)=="5117.19") #limite haute de la plge de selection
 
AOE_db_mes<-AOE_db_mes[,c(lim1:lim2)]
NOISE_db_mes<-NOISE_db_mes[,c(lim1:lim2)]

# Labels des lignes
var_db<-AOE_db[,c(1:6)]

# SNR
SNR_mes<-AOE_db_mes-NOISE_db_mes
#Fichier SNR total
SNR<-cbind(var_db,SNR_mes)

#Selection des individus non sous-stimulés uniquement
AOE_db_mes<-AOE_db_mes[var_db$sub=="false",]
AOE_db_r<-AOE_db[var_db$sub=="false",]
NOISE_db_mes<-AOE_db_mes[var_db$sub=="false",]
SNR_mes<-SNR_mes[SNR$sub=="false",]
var_db<-var_db[var_db$sub=="false",]

#Tableau des fréquences
freq<-as.vector(t(label))
freq_selec<-freq[c(lim1:lim2)]

#-----------------------------------------------------------------------------------
#                            CALCULS DES CARACTERISTIQUES DES COURBES 
#--------------------------------------------------------------------------------
#Amplitude moyenne par replicat
#Valeur max par audiogramme - 2 methodes: valeur max mesurée et 
#valeur max sur regression loess, ce qui revient à faire une fen^trage sur plusieurs valeurs
# Coefficient d'asymétrie = nombre de freq<freq(max) / nombre de freq>freq(max)

#APPROCHE PAR REPLICAT
#Calcul des amplitudes moyennes par replicats
ampl_moy<-c()
ind_list<-levels(as.factor(var_db[,2])) #liste des individus

for (i in ind_list)
{
  tab<-AOE_db_mes[var_db$ind==i,]
  
  for (k in 1:length(tab[,1]))
  {
    audio<-as.vector(t(tab[k,]))
    audio<-as.numeric(audio)
    m_temp<-mean(audio)
    ampl_moy<-c(ampl_moy,m_temp)
  }
}

#Ajout d'une variable pour caractériser la side qui a la plus grande amplitude (pour tester plus tard la latéralisation)
tag_side<-c()
var_upd<-c()
ind_list<-levels(as.factor(var_db[,2])) #liste des individus

for (i in ind_list)
{

  tab<-AOE_db_mes[var_db$ind==i,]
  tab_L<-AOE_db_mes[(var_db$ind==i & var_db$side=="L"),]
  tab_R<-AOE_db_mes[(var_db$ind==i & var_db$side=="R"),]
  
  mean_L<-mean(apply(tab_L,1,mean))
  mean_R<-mean(apply(tab_R,1,mean))
  
  Le<-"NO"
  if  (is.na(mean_L)==FALSE & is.na(mean_R)==FALSE)
  {
  Le<-ifelse(mean_L>mean_R,TRUE,FALSE)
  }
  
  for (k in 1:length(tab[,1]))
  {
    if (var_db[var_db$ind==i,]$side[k]=="L" & Le==TRUE) {tag_side<-c(tag_side,"High")}
    else 
    {
      if (var_db[var_db$ind==i,]$side[k]=="L" & Le==FALSE) {tag_side<-c(tag_side,"Low")}
      else
      {
        if (var_db[var_db$ind==i,]$side[k]=="R" & Le==TRUE) {tag_side<-c(tag_side,"Low")}
        else
        {
          if (var_db[var_db$ind==i,]$side[k]=="R" & Le==FALSE) {tag_side<-c(tag_side,"High")}
          else{ if (Le=="NO") {tag_side<-c(tag_side,"Lone")}  }
        }
      }
    }
  }
  var_upd<-rbind(var_upd,var_db[var_db$ind==i,])
}

# Tableau de synthèse avec les amplitudes moyenne par replicat et la latéralisation "High" "Low"
synth_table<-cbind(var_upd,level=tag_side,ampl=ampl_moy)
















#Calculs Max sur chaque replicats
freq_max<-c() #tableau des fréquences max mesurées (en cas d'égalité, on prend la première)
sym_max<-c() #tableau des coef de symétrie sur les max mesurés
freq_loess_max1<-c() #tableau des frequences max sur LOESS span=0.1
sym_loess_max1<-c() #tableau des coef de symétrie sur les max LOESS 0.1
freq_loess_max2<-c() #tableau des frequences max sur LOESS span=0.2
sym_loess_max2<-c() #tableau des coef de symétrie sur les max LOESS 0.2

for (i in ind_list)
{
  tab<-AOE_db_mes[var_db$ind==i,]
  
  for (k in 1:length(tab[,1]))
  {
    audio<-as.vector(t(tab[k,]))
    audio<-as.numeric(audio)
    
    #frequence maximale mesurée
    f_max<-freq_selec[which(audio==max(audio))]
    freq_max<-c(freq_max,f_max[1])
    temp_sym<-length(freq_selec[freq_selec<f_max[1]])/length(freq_selec[freq_selec>=f_max[1]])
    sym_max<-c(sym_max,temp_sym)
    
    #frequence maximale courbe loess avec voisinnage N=0.1*nombre de fréquences
    mod1=loess(audio~freq_selec,span=0.1)
    yfit1=predict(mod1,newdata=freq_selec)
    f_loess1<-freq_selec[which(yfit1==max(yfit1))]
    freq_loess_max1<-c(freq_loess_max1,f_loess1)
    temp_loess_sym1<-length(freq_selec[freq_selec<f_loess1])/length(freq_selec[freq_selec>=f_loess1])
    sym_loess_max1<-c(sym_loess_max1,temp_loess_sym1)
    
    #frequence maximale courbe loess avec voisinnage N=0.2*nombre de fréquences
    mod2=loess(audio~freq_selec,span=0.2)
    yfit2=predict(mod2,newdata=freq_selec)
    f_loess2<-freq_selec[which(yfit2==max(yfit2))]
    freq_loess_max2<-c(freq_loess_max2, f_loess2)
    temp_loess_sym2<-length(freq_selec[freq_selec<f_loess2])/length(freq_selec[freq_selec>=f_loess2])
    sym_loess_max2<-c(sym_loess_max2,temp_loess_sym2)
  }
}







































#PLATEAUX MAX
#Certains profils n'ont pas de max ponctuel mais montrent un plateau max
# Il s'agit ici de déterminer le range et la médiane de ce plateau
# Afin d'éviter les fortes variations locales, on part d'une courbe lissée loess 
# qui permet de détecter les plateaux de manière beaucoup plus pertinente


#Calculs Max sur chaque replicats
max_start<-c() #tableau des fréquences max mesurées (en cas d'égalité, on prend la première)
max_end<-c() #tableau des coef de symétrie sur les max mesurés
max_med<-c() #tableau de la frequence mediane du plateau
max_amp_av<-c() #tableau de l'a frequence mediane du plateau l'amplitude moyenne du plateau max

loess<-0.2
rate<-0.01
level<-0.9 #hauteur pour le calcul du plateau, 0.9=90%

for (i in ind_list)
{
  tab<-AOE_db_mes[var_db$ind==i,]
  
  for (k in 1:length(tab[,1]))
  {
    audio<-as.vector(t(tab[k,]))
    audio<-as.numeric(audio)
    
    #courbe loess avec voisinnage span
    mod1=loess(audio~freq_selec,span=loess)
    yfit1=predict(mod1,newdata=freq_selec)
    #f_loess1<-freq_selec[which(yfit1==max(yfit1))]
    #freq_loess_max1<-c(freq_loess_max1,f_loess1)
    
    high_sel<-freq_selec[which(yfit1>((max(yfit1)-min(yfit1))*level+min(yfit1)))]
    max_start<-c(max_start,min(high_sel))
    max_end<-c(max_end,max(high_sel))
    max_med<-c(max_med,mean(min(high_sel):max(high_sel)))
    
    amp_av<-mean(audio[which(freq_selec==min(high_sel)):which(freq_selec==max(high_sel))])
    max_amp_av<-c(max_amp_av,amp_av)
  }
}


# Tableau de synthèse par audiogramme avec réplicats
 synth_table<-cbind(synth_table,f_max=freq_max,sym_max=sym_max,f_Lw1=freq_loess_max1,sym_Lw1=sym_loess_max1,
                    f_Lw2=freq_loess_max2,sym_Lw2=sym_loess_max2,plat_start=max_start,plat_end=max_end,plat_med=max_med,
                    plat_amp=max_amp_av)


synth_table_mes<-synth_table[,-c(1:7)]
var_db_synth<-synth_table[,c(1:7)]

write.table(synth_table,paste("shape_repl.csv"),sep =";", dec = ".",row.names = FALSE)












# Tableau de synthèse par moyenne par audiogramme (calculée sur les valeurs par replicat)
#Moyennage des replicas
ind_list<-levels(as.factor(var_db_synth[,2])) #liste des individus

audio_rep_av<-c()

for (i in ind_list)
{

  tab_L<-synth_table_mes[(var_db_synth$ind==i & var_db_synth$side=="L"),]
  tab_R<-synth_table_mes[(var_db_synth$ind==i & var_db_synth$side=="R"),]
  
  tempL<-as.data.frame(cbind(ind=i,side="L",t((apply(tab_L,2,mean)))))
  tempR<-as.data.frame(cbind(ind=i,side="R",t((apply(tab_R,2,mean)))))
  
  audio_rep_av<-as.data.frame(rbind(audio_rep_av,tempL,tempR))
}

#Suppression des NaN qui apparaissent lorsqu'un individu n'a pas de mesures sur une side ou les deux
audio_rep_av<-data.frame(sapply(audio_rep_av, function(x) ifelse(x=="NaN", NA, x)))
synth_table_av_rep<-audio_rep_av[complete.cases(audio_rep_av),]

write.table(synth_table_av_rep,paste("shape_ind-side.csv"),sep =";", dec = ".",row.names = FALSE)










#---------------------------------------------------------
#             APPROCHE AUDIOGRAMME MOYEN
#----------------------------------------------------------
#Calculs sur les moyennes des replicats

#Moyennage des replicas
ind_list<-levels(as.factor(var_db[,2])) #liste des individus

audio_av<-c()

for (i in ind_list)
{
  tab_L<-AOE_db_mes[(var_db$ind==i & var_db$side=="L"),]
  tab_R<-AOE_db_mes[(var_db$ind==i & var_db$side=="R"),]
 
  tempL<-as.data.frame(cbind(ind=i,side="L",t((apply(tab_L,2,mean)))))
  tempR<-as.data.frame(cbind(ind=i,side="R",t((apply(tab_R,2,mean)))))

  audio_av<-as.data.frame(rbind(audio_av,tempL,tempR))
}

#Suppression des NaN qui apparaissent lorsqu'un individu n'a pas de mesures sur une side ou les deux
audio_av<-data.frame(sapply(audio_av, function(x) ifelse(x=="NaN", NA, x)))
audio_av<-audio_av[complete.cases(audio_av),]
colnames(audio_av)<-c("ind","side",freq_selec)
var_db_av<-audio_av[,c(1:2)]
audio_av_mes<-audio_av[,-c(1:2)]

#Calcul des caractéristiques des audiogrammess sur les audiogrammes moyens
ampl_moy<-c() #Mtableau des amplitudes moyennes
freq_max<-c() #tableau des fréquences max mesurées (en cas d'égalité, on prend la première)
sym_max<-c() #tableau des coef de symétrie sur les max mesurés
freq_loess_max1<-c() #tableau des frequences max sur LOESS span=0.1
sym_loess_max1<-c() #tableau des coef de symétrie sur les max LOESS 0.1
freq_loess_max2<-c() #tableau des frequences max sur LOESS span=0.2
sym_loess_max2<-c() #tableau des coef de symétrie sur les max LOESS 0.2
ind_list<-levels(as.factor(var_db_av[,1])) #liste des individus


for (i in ind_list)
{
  tab<-audio_av_mes[var_db_av$ind==i,]
  
  for (k in 1:length(tab[,1]))
  {
    audio<-as.vector(t(tab[k,]))
    audio<-as.numeric(audio)
    
    #Amplitude moyenne
    m_temp<-mean(as.numeric(audio))
    ampl_moy<-c(ampl_moy,m_temp)

    #frequence maximale mesurée
    f_max<-freq_selec[which(audio==max(audio))]
    freq_max<-c(freq_max,f_max[1])
    temp_sym<-length(freq_selec[freq_selec<f_max[1]])/length(freq_selec[freq_selec>=f_max[1]])
    sym_max<-c(sym_max,temp_sym)
    
    #frequence maximale courbe loess avec voisinnage N=0.1*nombre de fréquences
    mod1=loess(audio~freq_selec,span=0.1)
    yfit1=predict(mod1,newdata=freq_selec)
    f_loess1<-freq_selec[which(yfit1==max(yfit1))]
    freq_loess_max1<-c(freq_loess_max1,f_loess1)
    temp_loess_sym1<-length(freq_selec[freq_selec<f_loess1])/length(freq_selec[freq_selec>=f_loess1])
    sym_loess_max1<-c(sym_loess_max1,temp_loess_sym1)
    
    #frequence maximale courbe loess avec voisinnage N=0.2*nombre de fréquences
    mod2=loess(audio~freq_selec,span=0.2)
    yfit2=predict(mod2,newdata=freq_selec)
    f_loess2<-freq_selec[which(yfit2==max(yfit2))]
    freq_loess_max2<-c(freq_loess_max2, f_loess2)
    temp_loess_sym2<-length(freq_selec[freq_selec<f_loess2])/length(freq_selec[freq_selec>=f_loess2])
    sym_loess_max2<-c(sym_loess_max2,temp_loess_sym2)
  }
}

# Tableau de synthèse par audiogramme moyen
synth_table_av_moy<-cbind(var_db_av[,1:2],ampl=ampl_moy,f_max=freq_max,sym_max=sym_max,f_Lw1=freq_loess_max1,sym_Lw1=sym_loess_max1,
                   f_Lw2=freq_loess_max2,sym_Lw2=sym_loess_max2)

  
