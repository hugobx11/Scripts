#Ajoute les données anthropo
#TABLEAU DE SORTIES
# synth_table_anthr : contient les caractéristiques par audiogramme (replicats)
# synth_table_av_rep_anthr : contient les caractéristiques moyennes ind/side calculées sur les 
#caractéristiques par replicat
# audio_av: audiogramme moyens
# synth_table_av_moy_anthr: contient les caractéristiques moyennes ind/side calculées sur les 
#audiogrammes moyens


#Tableau par replicats
line_temp<-c()
synth_table_anthr<-c()

for (i in 1:length(synth_table[,1]))
{
  temp1<-synth_table[i,]
  temp2<-anthr[anthr$tube_code==synth_table[i,2],]
  temp3<-classif[classif$Code_pop==synth_table[i,1],]
  line_temp<-cbind(temp1,temp2,temp3)
  synth_table_anthr<-rbind(synth_table_anthr,line_temp)
}


write.table(synth_table_anthr,paste("synth_table_anthr.csv"),sep =";", dec = ".",row.names = FALSE)


#Tableau par moyenne sur replicats
line_temp<-c()
synth_table_av_rep_anthr<-c()

for (i in 1:length(synth_table_av_rep[,1]))
{
  temp1<-synth_table_av_rep[i,]
  temp2<-anthr[anthr$tube_code==synth_table_av_rep[i,1],]
  temp3<-classif[classif$Code_pop==synth_table[which(synth_table[,2]==synth_table_av_rep[i,1])[1],1],]
  line_temp<-cbind(temp1,temp2,temp3)
  synth_table_av_rep_anthr<-rbind(synth_table_av_rep_anthr,line_temp)
}

write.table(synth_table_av_rep_anthr,paste("synth_table_av_rep_anthr.csv"),sep =";", dec = ".",row.names = FALSE)



#Tableau par audio moyen
line_temp<-c()
synth_table_av_moy_anthr<-c()

for (i in 1:length(synth_table_av_moy[,1]))
{
  temp1<-synth_table_av_rep[i,]
  temp2<-anthr[anthr$tube_code==synth_table_av_moy[i,1],]
  temp3<-classif[classif$Code_pop==synth_table[which(synth_table[,2]==synth_table_av_moy[i,1])[1],1],]
  line_temp<-cbind(temp1,temp2,temp3)
  synth_table_av_moy_anthr<-rbind(synth_table_av_moy_anthr,line_temp)
}

write.table(synth_table_av_moy_anthr,paste("synth_table_av_moy_anthr.csv"),sep =";", dec = ".",row.names = FALSE)


