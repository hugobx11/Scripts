#Seuil SNR

AOE_db_mes<-AOE_db[,-c(1:6)]
NOISE_db_mes<-NOISE_db[,-c(1:6)]
var_db<-AOE_db[,c(1:6)]
SNR_mes<-AOE_db_mes-NOISE_db_mes

#Fichier SNR total
SNR<-cbind(var_db,SNR_mes)

SNR_mes<-SNR_mes[SNR$sub=="false",]
SNR<-SNR[SNR$sub=="false",]


#Boxplot par frequences
SNR_stack<-stack(SNR_mes)
boxplot(SNR_stack[,1]~SNR_stack[,2],ylim=c(-50,50))
abline(h = 3, col="red",lwd=3)
#abline(h = 1, col="blue",lwd=3)

#save gaph
cairo_ps("graph1slide1.eps")
boxplot(SNR_stack[,1]~SNR_stack[,2],ylim=c(-50,50))
abline(h = 3, col="red",lwd=3)
dev.off()


setEPS()
postscript("graph1.eps", horizontal = FALSE, onefile = FALSE, paper = "special")

dev.off()


#Représentation des limites SNR pour les médianes en fonction des fréquences

#GLOBAL
median<-tapply(SNR_stack[,1],SNR_stack[,2],median)
plot(label[,1],median)
abline(h = 3, col="red",lwd=3)
#abline(h = 1, col="blue",lwd=3)

lim<-c()
inf<-as.numeric(names(which(median>3)))
for (k in 2:length(inf)-1)
{
  if ((inf[k+1]-inf[k])>40)
  {lim<-c(lim,inf[k],inf[k+1])}
}

abline(v = lim[1], col="orange",lwd=1)
abline(v = lim[2], col="orange",lwd=1)
abline(v = lim[3], col="orange",lwd=1)
abline(v = lim[4], col="orange",lwd=1)
abline(v = lim[5], col="orange",lwd=1)
abline(v = lim[6], col="orange",lwd=1)
legend("topright",legend=c(lim[1],lim[2],lim[3],lim[4]),lty=1,col="orange")

lim # Lecture des limites sur le profils MEDIAN


#Save grpah
setEPS()
cairo_ps("graph2slide1.eps")
plot(label[,1],median)
abline(h = 3, col="red",lwd=3)
abline(v = lim[1], col="orange",lwd=1)
abline(v = lim[2], col="orange",lwd=1)
abline(v = lim[3], col="orange",lwd=1)
abline(v = lim[4], col="orange",lwd=1)
abline(v = lim[5], col="orange",lwd=1)
abline(v = lim[6], col="orange",lwd=1)
legend("topright",legend=c(lim[1],lim[2],lim[3],lim[4]),lty=1,col="orange")
dev.off()


#Représentation des limites SNR pour les Q1 en fonction des fréquences
quant <- function(df) quantile(df, probs=c(0.25))
Q1<-tapply(SNR_stack[,1],SNR_stack[,2],quant)
plot(label[,1],Q1)
abline(h = 3, col="red",lwd=3)
#abline(h = 1, col="blue",lwd=3)

lim<-c()
inf<-as.numeric(names(which(Q1>3)))
for (k in 2:length(inf)-1)
{
  if ((inf[k+1]-inf[k])>40)
  {lim<-c(lim,inf[k],inf[k+1])}
}
lim<-c(lim,inf[length(inf)])

abline(v = lim[1], col="orange",lwd=1)
abline(v = lim[2], col="orange",lwd=1)
abline(v = lim[3], col="orange",lwd=1)
legend("topright",legend=c(lim[1],lim[2],lim[3]),lty=1,col="orange")


cairo_ps("graph3slide1.eps")
Q1<-tapply(SNR_stack[,1],SNR_stack[,2],quant)
plot(label[,1],Q1)
abline(h = 3, col="red",lwd=3)
abline(v = lim[1], col="orange",lwd=1)
abline(v = lim[2], col="orange",lwd=1)
abline(v = lim[3], col="orange",lwd=1)
legend("topright",legend=c(lim[1],lim[2],lim[3]),lty=1,col="orange")
dev.off()

lim #LEcture des limites Q1


#Par populations sur les profils medians

pop_list<-levels(as.factor(pop[]))
par(mfrow=c(4,4))
res<-matrix(data=NA, nrow=length(pop_list),ncol=7)
i=1
for (p in pop_list)
{
  print(i)
  #Fichier SNR total
  SNR_mes_pop<-SNR_mes[SNR$pop==p,]
  SNR_stack<-stack(SNR_mes_pop)
  
   #Boxplot par frequences
   median<-tapply(SNR_stack[,1],SNR_stack[,2],median)
   plot(label[,1],median,main=p)
   abline(h = 3, col="red",lwd=3)
   abline(h = 1, col="blue",lwd=3)
  
  lim<-c()
  inf<-as.numeric(names(which(median>3)))
  for (k in 2:length(inf)-1)
  {
    if ((inf[k+1]-inf[k])>40)
    {lim<-c(lim,inf[k],inf[k+1])}
  }

  res[i,2]=as.numeric(lim[1])
  res[i,3]=as.numeric(lim[2])
  res[i,4]=as.numeric(lim[3])
  res[i,5]=as.numeric(lim[4])
  res[i,6]=as.numeric(lim[5])
  res[i,7]=as.numeric(lim[6])

  abline(v = lim[1], col="orange",lwd=1)
  abline(v = lim[2], col="orange",lwd=1)
  abline(v = lim[3], col="orange",lwd=1)
  abline(v = lim[4], col="orange",lwd=1)
  abline(v = lim[5], col="orange",lwd=1)
  abline(v = lim[6], col="orange",lwd=1)
  #legend("topright",legend=c(lim[1],lim[2],lim[3],lim[4],lim[5],lim[6]),lty=1,col="orange")

  res[i,1]=p
  
  i=i+1
}

par(mfrow=c(1,1))
res<-as.data.frame(res)
res #Lecture des limites





