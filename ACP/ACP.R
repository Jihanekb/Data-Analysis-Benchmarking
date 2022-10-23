#------------------------------------------------- Préparation du jeu des données ------------------------------------------------
#Spécification du répertoire du travail
getwd()
setwd("D:/2A-2IA/ADD/Projet/ACP")

#Chargement du jeu des données
data <- read.csv("Pred_temp_moy.csv")

#Visualisation du jeu des données
View(data)
#Exploration des attributs
str(data)
summary(data)
#Calcul des corrélations entre variables
cor(data)

#--------------------------------------------------------- ACP --------------------------------------------------------
#Chargement du package nécessaire
library(FactoMineR)
#Application d'une ACP normée sur le tableau des variables quantitatives
res<-PCA(data,ncp=5,axes=c(1,2))
res

#si on ne réduit pas le nuage : une variable à forte variance va à tirer à tout l'effet de l'ACP vers elle .

#Chargement du package nécessaire
library(psych)
#Calcul de l'indice KMO et des MSAi
KMO(cor(data))

#Calcul des valeurs propres de la matrice de corr�lation
res$eig

#Graphiques des valeurs propres
plot(1:18,res$eig[,1],type="b",ylab="Valeurs propres",xlab="Composantes",main="graphique des valeurs propres")
barplot(res$eig[,1],main="Eigenvalues", names.arg=paste("dim",1:nrow(res$eig)))

#Enregistrement des valeurs propres du jeu de données
vp<-res$eig[,1]
write.table(vp,"Valeurs propres.csv",sep=",",col.names=TRUE,dec=',', row.names=FALSE)

#la dimension du sous espace
var(res$eig[2:18,1])*(18-2)*100/(var(res$eig[,1])*17) #24.8613
var(res$eig[3:18,1])*(18-3)*100/(var(res$eig[,1])*17) #9.304451
var(res$eig[4:18,1])*(18-4)*100/(var(res$eig[,1])*17) #4.318294
#Donc La dimension du sous espace sera egal a 3
res<-PCA(data,ncp=3,axes=c(1,2))  
#--------------------------------------------------------- Nuage des variables  --------------------------------------------------------
#Calcul de : cos2
res$var$cos2

#Cumul des cos2
print(t(apply(res$var$cos2,1,cumsum)),digit=2)

#Contribution des variables au sous espace
cont<-res$var$contrib
cont
write.table(cont,"Cont_Var.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)

#CAH
data<-read.csv2("Cont_VarBis.csv",row.names=1)
data$var <- as.character(data$var)
Cat_var<-HCPC(data,nb.clust=-1)
Cat_var$desc.var
Cat_var$desc.axes

#le nuage des variables projeté sur les 2 premiers axes.
plot(res,choix="var", cex=0.75,title="Projection des 18 variables sur le plan 1-2")

#--------------------------------------------------------- Nuage des individus  --------------------------------------------------------
#Calcul de : coord, cos2, contrib
res$ind$cos2

#Cumul des cos2
for(i in 1:197){
  if((t(apply(res$ind$cos2,1,cumsum))[i,3])>=0.8){
    print(paste("l'individu",i, "est bien representé"))
  }
  if((t(apply(res$ind$cos2,1,cumsum))[i,3])>=0.6 && (t(apply(res$ind$cos2,1,cumsum))[i,3])<0.8){
    print(paste("l'individu",i, "est moyennement representé"))
  }
  if((t(apply(res$ind$cos2,1,cumsum))[i,3])<0.6){
    print(paste("l'individu",i, "est faiblement representé"))
  }
}

#Contribution des individus au sous espace
cont<-res$ind$contrib
cont
write.table(cont,"Cont_Ind.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)
data<-read.csv2("Cont_IndBis.csv",row.names=1)

#Application de la CAH 
Cat_Ind<-HCPC(data,nb.clust=-1)
Cat_Ind$desc.var

#le nuage des individus projet� sur les 2 premiers axes.
plot(res,choix="ind", cex=0.75,title="Projection des 197 observations sur le plan 1-2")