#------------------------------------------- Régression linéaire Multiple ---------------------------------------------------------
#Spécification du répertoire du travail
getwd()
setwd("D:/2A-2IA/ADD/Projet/Regression Linéaire Multiple")

#Chargement du jeu des données
data <- read.csv("Pred_temp_moy.csv")

#Visualisation du jeu des données
View(data)


#------------------------------ Modèle de la régression linéaire multiple incluant toutes les variables ------------------------------
model = lm( Next_Tmoy~., data = data)

# Coefficients du modèle
coef(model) 

# Sommaire du modèle
summary(model) # Les variables non significatives, ce sont les variables qu’ils ont Pr(>|t|) >= 5%

# Coefficient de détermination
summary(model)$r.squared
summary(model)$adj.r.squared


#--------------------------------------------------------- Procédure Step -----------------------------------------------------------
modelstep <- step(model)

#sommaire du modèle amélioré par Step
summary(modelstep) 

#AIC du modèle initial
AIC(model)
#AIC du modèle amélioré par Step
AIC(modelstep)

#Tests de validation 
## Test d'homoscédasticité
plot(predict(modelstep), resid(modelstep))
abline(h=0)
##Tests de Normalité
shapiro.test(resid(modelstep)) #test de shapiro
ks.test(resid(modelstep),pnorm) #test de ks
## Recherche des valeurs aberrantes 
seqx=seq(1,197,length=197)
abr=abs(data$Next_Tmoy-predict(modelstep))/0.7684 #RSE= 0.7684
plot(seqx,abr)
abline(h=2, lty=2,col=2)

#------------------------------------------ Méthode Pas à Pas basée sur le test de Fisher --------------------------------------------
nva=ncol(data)-1
Fish = rep(0,nva)
p=ncol(data)-1
cible_col=18
precision_del=0.10
precision_aj=0.10
## Introduction
for (i in  1:p ) {
mod1<-lm(data[,cible_col]~data[,i])
Fish[i]=var(predict(mod1))*(nrow(data)-1)/(deviance(mod1)/df.residual(mod1))
}
df2=nrow(data)-2
V=match(max(Fish),Fish):match(max(Fish),Fish)
print('Ajout :')
print('Fisher :')
print( max(Fish) )
print('p value :')
print((  1-pf(max(Fish),1,df2) ))
print('pos :')
print( match(max(Fish),Fish) )
t=0
for(i in V){
if(t==0){
dev=cbind(data[,i])
t=1
}
else{
dev=cbind(dev,data[,i])
}
}
mod<-lm(data[,18]~dev)
print("R squared aj")
print(summary(mod)$adj.r.squared)
print(summary(mod))
lastaj_pvalue=0
i=0
break_=0
while(1){
##--------------AJOUT------------------
Fish = rep(0,nva)
t=0
for(i in V){
if(t==0){
dev=cbind(data[,i])
t=1
}
else{
dev=cbind(dev,data[,i])
}
}
SCR1<-deviance(lm(data[,cible_col]~dev))
for (i in 1:p) {
mod<-lm(data[,cible_col]~cbind(dev,data[,i]))
SCR2=deviance(mod)
Fish[i]=(SCR1-SCR2)/(SCR2/(nrow(data)-length(V)-2))
}
df2=nrow(data)-length(V)-2
##---TEST AJOUT---
if((1-pf(max(Fish),1,df2))<precision_aj){
print('Ajout :')
print('Fisher :')
print( max(Fish) )
print('p value :')
print((  1-pf(max(Fish),1,df2) ))
print('pos :')
print( match(max(Fish),Fish) )
V[length(V)+1]=match(max(Fish),Fish)
print(V)
break_=0
}
else{
print('Ajout :')
print('Nothing')
print('Fisher :')
print( max(Fish) )
print('p value :')
print((  1-pf(max(Fish),1,df2) ))
print('pos :')
print( match(max(Fish),Fish) )
lastaj_pvalue=1-pf(max(Fish),1,df2)
break_=break_+1
}
##--------------Retrait------------------
Fish = rep(0,length(V))
t=0
for(i in V){
if(t==0){
dev=data[,i]
t=1
}
else{
dev=cbind(dev,data[,i])
}
}
SCR3=deviance(lm(data[,cible_col]~dev))
i=1
for(j in 1:length(V)){
m=0
for(z in 1:length(V)){
if(z==j){
next
}
if(m==0){
ret=cbind(data[,V[z]])
m=1
}
else{
ret=cbind(ret,data[,V[z]])
}
}
##mod<-lm(data[,18]~ret)
SCR2<-deviance(lm(data[,cible_col]~ret))
Fish[i]=(SCR2-SCR3)/(SCR3/(nrow(data)-length(V)-1))
i=i+1
}
## prob -2 ou -1
df2=nrow(data)-length(V)-1
##---TEST Retrait---
if((1-pf(min(Fish),1,df2))>precision_del){
print('Delete :')
print('Fisher :')
print( min(Fish) )
print('p value :')
print((  1-pf(min(Fish),1,df2) ))
print('pos :')
print( V[match(min(Fish),Fish)] )
V=V[-(match(min(Fish),Fish):match(min(Fish),Fish))]
print(V)
break_=0
}
else{
print('Delete :')
print('Nothing')
print('p value :')
print((  1-pf(min(Fish),1,df2) ))
print('pos :')
print( V[match(min(Fish),Fish)] )
print('Fisher :')
print( min(Fish) )
if(break_==1){
print('Last del p value')
print(1-pf(min(Fish),1,df2))
print('Last ajout p value')
print(lastaj_pvalue)
break
}
}
t=0
for(i in V){
if(t==0){
dev=cbind(data[,i])
t=1
}
else{
dev=cbind(dev,data[,i])
}
}
mod<-lm(data[,18]~dev)
print("R squared aj")
print(summary(mod)$adj.r.squared)
print(summary(mod))
}

t=0
for(i in V){
if(t==0){
dev=cbind(data[,i])
t=1
}
else{
dev=cbind(dev,data[,i])
}
}

## Tests de validation
### Test d'omoscédasticité
plot(predict(mod), resid(mod))
abline(h=0) 
###Test de normalité
shapiro.test(resid(mod)) #test de shapiro
ks.test(resid(mod),pnorm) #test de ks
### Recherche de valeurs aberrantes
seqx=seq(1,197,length=197)
abr=abs(data$Next_Tmoy-predict(mod))/0.7711  #RSE = 0.7711
plot(seqx,abr)
abline(h=2, lty=2,col=2)

## AIC du modèle obtenu par la méthode pas à pas
AIC(mod)
