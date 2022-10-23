#------------------------------------------------- Préparation du jeu des données ------------------------------------------------
#Spécification du répertoire du travail
getwd()
setwd("D:/2A-2IA/ADD/Projet/Classification")

#Chargement du jeu des données
data <- read.csv("Pred_temp_moy.csv")

#Visualisation du jeu des données
View(data)

#rendre les variables centrées réduites
data_cr<-scale(data)
#Enregistrement du nouveau jeu des données dans un nouveau fichier csv
write.table(data_cr , 'data_cr.csv', sep=",", col.names=TRUE, row.names=FALSE)
#Chargement du nouveau fichier
newdata_cr<- read.csv('data_cr.csv')

#Visualisation du nouveau jeu des données
View(newdata_cr)
#Exploration des attributs
str(newdata_cr)


#------------------------------------------------- Classification -------------------------------------------------
#------------------------------------------------- K-means
#Application de k-means sur le jeu des données centrées réduites (centers = 4)
groupes.kmeans <- kmeans(newdata_cr,centers=4,nstart=5)
#Installation des librairies nécessaires
install.packages("ggpubr")
install.packages("factoextra")
#Importation de ces librairies
library(ggpubr)
library(factoextra)
#Affichage des clusters crées par le k-means
fviz_cluster(groupes.kmeans, data = newdata_cr,palette = c("#2E9FDF", "#00AFBB", "#E7B800","#e72a00"), geom = "point",ellipse.type = "convex", ggtheme = theme_bw())
#Calcul du taux d'inertie
##On part d'un N = 20
N = 20
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(newdata_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
inertie.expl
N = 40
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(newdata_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
inertie.expl
N = 70
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(newdata_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
inertie.expl
N = 80
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(newdata_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
inertie.expl
N = 90
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(newdata_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
inertie.expl
N = 97
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(newdata_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
inertie.expl
N = 100
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(newdata_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
inertie.expl
N = 101
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(newdata_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
inertie.expl
### On a (max(inertie.expl)= 0.9516566)>=0.95 pour N = 101 et (max(inertie.expl)= 0.9472728)<0.95 pour N = 100 
### donc On choisi N = 101 pour qu'elle soit le plus petit entier tel que le taux d’inertie expliquée de la classification à N classes est supérieur à 0.95

#Déterminer Nc le nombre de classes
##l’évolution de la proportion d’inertie expliquée par la partition
plot(1:N,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
#20 classes
var(inertie.expl[21:N])*(N-21)*100/(var(inertie.expl)*(N-1))#= 10.32352
#22 classes
var(inertie.expl[23:N])*(N-23)*100/(var(inertie.expl)*(N-1))#=8.71119
#25 classes
var(inertie.expl[26:N])*(N-26)*100/(var(inertie.expl)*(N-1))#=6.912301
#26 classes
var(inertie.expl[27:N])*(N-27)*100/(var(inertie.expl)*(N-1))#=6.394093
#29 classes
var(inertie.expl[30:N])*(N-30)*100/(var(inertie.expl)*(N-1))#=5.042496
#30 classes
var(inertie.expl[31:N])*(N-31)*100/(var(inertie.expl)*(N-1)) #=4.665542
### D'où Nc = 30
#Appliquation de k-means sur le jeu des données centrées et réduites (centers=30)
groupes.kmeans <- kmeans(newdata_cr,centers=30,nstart=5)
#Affichages Des clusters crées
fviz_cluster(groupes.kmeans, data = newdata_cr, geom = "point",ellipse.type = "convex", ggtheme = theme_bw())

#----------------------------------------------------- CAH
##Installation des librairies nécéssaires
install.packages("FactoMineR")
#Importation de ces librairies
library(FactoMineR)
#Appliquation de CAH sur les données centrées et réduites
Res<-HCPC(newdata_cr,nb.clust=-1)
#Distribution selon les deux axes
Res
#Distribution des groupes
plot(Res)
#Affichage du dendrogramme de la CAH
fviz_dend(Res,
cex = 0.7,                     # Taille du text
palette = "jco",               # Palette de couleur ?ggpubr::ggpar
rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
rect_border = "jco",           # Couleur du rectangle
labels_track_height = 0.8      # Augment l'espace pour le texte
)

#Les variables quantitatives les plus corrélées avec la variable classification et description des classes retenues par variables 
Res$desc.var

#Calcul du taux d'inertie
I<-Res$call
I

#Calcul de l'inertie Inter/Inertie total,avant la consolidation de la CAH
I$bw.before.consol #= 6.514275
#Calcul de l'inertie Inter/Inertie total,après la consolidation de la CAH
I$bw.after.consol #= 6.676362

