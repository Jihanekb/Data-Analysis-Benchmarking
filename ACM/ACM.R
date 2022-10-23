#------------------------------------------------- Préparation du jeu des données ------------------------------------------------
#Spécification du répertoire du travail
getwd()
setwd("D:/2A-2IA/ADD/Projet/ACM")

#Chargement du jeu des données
data <- read.csv("Pred_temp_moy.csv")

#Visualisation du jeu des données
View(data)
nb_col=18
nb_rows=197

#Importation des librairies
library(FactoMineR)
library(psych)

#transformation des variables quantitatives en variables qualitatives
L = 0
o=1 
m=0
taux_indice=1
TAUX=0
p=0
#Discrétisation
for(i in 1:nb_col){ 
  km=kmeans(data[,i], 3,nstart = 25)
  
  TAUX[taux_indice]=(km$betweenss/km$totss)
  taux_indice=taux_indice+1
  
  if( (km$betweenss/km$totss)>0.5 ){ #Si le taux de discrétisation est supérieur à 0.5, on transforme la variable en 3 modalité
    
    
    if(p==0){
      all_cluster=cbind( km$cluster)
      L[o]=3:3
      o=o+1
      m=m+3
      p=1
    }
    else{
      all_cluster=cbind(all_cluster,km$cluster)
      m=m+3
      L[o]=3
      o=o+1
    }
    
    
  }
  else{ #Sinon, il est possible de la discrétiser en 4 classes
    km=kmeans(data[,i], 4, iter.max = 100)
    
    if(p==0){
      all_cluster=cbind(km$cluster)
      L[o]=4:4
      m=m+4
      o=o+1
      p=1
    }
    else{
      all_cluster=cbind(all_cluster,km$cluster)
      m=m+4
      L[o]=4
      o=o+1
    }
     
  }
  
}
##Taux de discrétisation
TAUX
### Taux >= 0.5 pour toutes les variables donc on aura une transforamation en 3 modalités.

##Tableau qualitative
data_categ<-data.frame(data)
for(i in 1:nb_col){
  
  data_categ[,i]=all_cluster[,i]
  
}
data_categ

##Taux de discrétisation en %
TAUX_x=TAUX*100  
Taux_=data.frame(matrix(ncol = nb_col, nrow = 1))
Taux_=setNames(Taux_, names(data))
for(i in 1:nb_col){
  Taux_[1,i]=paste(round(TAUX_x[i],2),"%")
}
Taux_

#Sauvegarde des Tableaux dans un fichier Excel
## Installation des librairies necessaire pour l'enregistrement d'un fichier Excel
install.packages("writexl")
## Importation des librairies necessaire pour l'enregistrement d'un fichier Excel
library("writexl")
##Enregistrement des variables quantitatives dans un fichier Excel
write_xlsx(data.frame(data),"data_quanti.xlsx")
##Enregistrement des variables qualitatives dans un fichier Excel
write_xlsx(data.frame(data_categ),"data_quali.xlsx")
##Enregistrement du taux en % de discrétisation de chaque transformation dans un fichier Excel
write_xlsx(data.frame(Taux_),"TAUX.xlsx")

#Construction du tableau disjonctif complet
g=c(1) 
for(i in 2:m){
  a=c(1)
  g=cbind(g,a)
  
}

TDC=data.frame(g)

for(z in 1:nb_rows){ 
  count_V=1 
  p=0
  for(i in 1:length(L)){
    
    for(j in 1:L[i]){
          ## data[z,i]
          if(j==all_cluster[z,i]){
              if(p==0){
              V=1:1
              p=1
              count_V=count_V+1
              }
              else{
                V[count_V]=1
                count_V=count_V+1
              }
          }
          else{
            if(p==0){
              V=0:0
              p=1
              count_V=count_V+1
            }
            else{
              V[count_V]=0
              count_V=count_V+1
            }
          }
            
          
    }
  } 
  TDC[z+1,]=V 
}
#RENAMING
TDC=TDC[-1,]
for(i in 1:length(L)){
    principal_name=names(data)[i]
    for(j in 1:L[i]){
          column_name=paste0(principal_name,j)
          #print(column_name)
          names(TDC)[(i-1)*3+j]=column_name
    }
}
TDC
##Enregistrement du tableau disjonctif complet dans un fichier Excel
write_xlsx(data.frame(TDC),"TDC.xlsx")


#la fréquence de chaque modalité et recherche de la présence d’une modalité rare (fréquence < 0.01).
propmod=apply(TDC,2,sum)/(nrow(TDC))
propmod ##Affichage des fréquences
Bad_modalite=""
bad=1
faible_freq=0
for(i in 1:length(propmod)){
  if(propmod[i]<0.01){
    Bad_modalite[bad]=names(propmod)[i]
    bad=bad+1
    print(paste("La modalité",names(propmod)[i], " est a faible frequence" ,"sa frequence est ",propmod[i]) ) 
    faible_freq=1
  }
}
if(faible_freq==0){
  print( "Aucune modalité n'est de faible fréquence .")
} 


#--------------------------------------------------------- ACM ------------------------------------------------------------
#Changement de type pour correspondre a la fonction MCA
i=0
while(i < ncol(data_categ)){
  i=i+1  
  data_categ[,i] = as.factor(data_categ[,i])
}
str(data_categ)
#Application de l’ACM
res<-MCA(data_categ,graph=FALSE,axes=c(2.3))
#nombre de modalités
p<-ncol(TDC)
#nombre de variables
s <- ncol(data) 
#nombre d'individus
n <- nrow(data)
print(paste("On a",p,"modalités" ,s,"varialbes","ainsi que ",n,"individus"))

#Valeurs propres et pourcentage d’inertie de chaque valeur propre ainsi que le cumul des pourcentages d’inertie
res$eig 

#Tracez le graphique des valeurs propres.
plot(res$eig[,1],type="b",main="Scree plot")

#Choix de dimension de sous espace
##Appliquons la regle : Var(val prop éliminées)/var(val prop) < 0.05
##On retient tout les valeurs propres qui la verifie .
###dim=5
var(res$eig[6:36,1])*(36-6)/(var(res$eig[,1])*35) #0.1081354
###dim=6
var(res$eig[7:36,1])*(36-7)/(var(res$eig[,1])*35) #0.087937
###dim=7
var(res$eig[8:36,1])*(36-8)/(var(res$eig[,1])*35) #0.06947203
###dim=8
var(res$eig[9:36,1])*(36-9)/(var(res$eig[,1])*35) #0.05702777
###dim=9
var(res$eig[10:36,1])*(36-10)/(var(res$eig[,1])*35) #0.04547664
## Donc La dimension du sous espace est donc egal a 9  

#Application de l’ACM
res<-MCA(data_categ,ncp = 9,graph=FALSE,axes=c(2.3))

#--------------------------------------------------------- Nuage de modalités ------------------------------------------------------------
#cos2 sur chaque composantes
res$var$cos2

#pour le cumul
print(t(apply(res$var$cos2,1,cumsum)),digit=2)

#Distinction des modalités bien représentées, moyennement représentées et faiblement représentées sur le sous espace
cos_sum_var=t(apply(res$var$cos2,1,cumsum))
for(i in 1:length(cos_sum_var[,9])){
  if(cos_sum_var[i,9]>=0.8){
    print(paste("la modalité",i, "est bien representé"))
  }
  if(cos_sum_var[i,9]>=0.6 && cos_sum_var[i,9]<0.8){
    print(paste("la modalité",i, "est moyennement representé"))
  }
  if(cos_sum_var[i,9]<0.6){
    print(paste("la modalité",i, "est faiblements representé"))
  }
}

#la contribution des modalités dans chaque axe du sous espace
contrib <- res$var$contrib 
contrib

#Application de la CAH au tableau des contributions des modalités aux axes du sous espace
cont<-HCPC(as.data.frame(contrib),nb.clust=-1)
cont
cont$desc.var
cont$data.clust

#le nuage des modalités projeté sur les 2 premiers axes.
plot(res,invisible="ind")

#--------------------------------------------------------- Nuage des individus ------------------------------------------------------------
#Le cos2 des individus sur le sous espace
res$ind$cos2

#le cumul
print(t(apply(res$ind$cos2,1,cumsum)),digit=2)

#Distinction des individus bien représentés, moyennement représentés et faiblement représentés sur le sous espace
cos_sum_ind=t(apply(res$ind$cos2,1,cumsum))
for(i in 1:length(cos_sum_ind[,9])){
  if(cos_sum_ind[i,9]>=0.8){
    print(paste("l'individu",i, "est bien representé"))
  }
  if(cos_sum_ind[i,9]>=0.6 && cos_sum_ind[i,9]<0.8){
    print(paste("l'individu",i, "est moyennement representé"))
  }
  if(cos_sum_ind[i,9]<0.6){
    print(paste("l'individu",i, "est faiblement representé"))
  }
}

#La contribution des individus dans chaque axe du sous espace
contrib <- res$ind$contrib 
contrib

#Application de la CAH au tableau des contributions des individus aux axes du sous espace
cont<-HCPC(as.data.frame(res$ind$contrib),nb.clust=-1)
cont$desc.var
cont$data.clust

#--------------------------------------------------------- Nuage des variables ------------------------------------------------------------
#Les coefficients de corrélation des variables avec les projections sur les axes du sous espace
res$var$eta2

#Le graphique des coefficients de corrélation des variables avec les projections sur le 1er plan factoriel
plot(res,choix="var")