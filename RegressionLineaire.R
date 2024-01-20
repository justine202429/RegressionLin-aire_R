#Projet 2 sur la régression linéaire 
setwd("/Users/justinekosinski/Desktop/MIAGE/L3/S1/Statistiques/Projet2")
data_imc_TP2_groupe1=read.table("data_imc_TP2_groupe1.txt",header=TRUE) 
is.data.frame(data_imc_TP2_groupe1)
View(data_imc_TP2_groupe1)
attach(data_imc_TP2_groupe1)

#Partie 1 : régression linéaire simple 
# Question 1
# nuage de points
par(mfrow=c(1,1))
plot(AGE,TTAILLE, pch="•",main="Liaison entre le tour de taille et l'âge", xlab="Âge",ylab="Tour de taille",
     xlim=c(min(AGE), max(AGE)), ylim=c(min(TTAILLE), max(TTAILLE)))

# coefficient de corrélation linéaire et test associé 
correlation <- cor(AGE,TTAILLE)
cor.test(AGE,TTAILLE)
round(correlation, 2)
# Il existe une liaison linéaire significative (p<5%) positive modérée entre ttaille et age (r=0,55) 

# Question 2 
# Modèle théorique : ttaille_i = beta_0 + beta_1 age_i + epsilon_i
# ttaille_i = a age_i + b + e_i pour i=1,...,n
regression =lm(TTAILLE~AGE) # droite de regression
regression
rounded_coefficients <- round(coef(regression), 2)
cat("Coefficients arrondis:", rounded_coefficients, "\n")
# a_chapeau=0.54  et b_chapeau=68.93
# L'équation de la droite de régression est : y = 0,54 x + 68.93

summary(regression)
# R2=0,30
# 30% de la variation totale des tours de taille est expliquée par le modèle linéaire,
# c'est-à-dire par la variation de l'âge. 
# Ce coefficient de détermination est proche de 0 donc le modèle ajuste pas bien les données.

# Test de validité globale du modèle
# H0 : tous les paramètres sont nuls sauf la constante
# p-value = 6.112e-06<5% donc on rejette H0 donc le modèle est globalement valide

# Test de significativité du paramètre de Surface
# H0 : a=0 contre H1 : a différent de 0
# p-value = 6.112e-06<5% (lue dans la dernière colonne du tableau coefficients) donc on rejette H0
# et la variable age est significative
# On peut donc commenter son paramètre estimé  a_chapeau=0,54
# Le coefficient de "AGE" (0.54) indique que, en moyenne, le tour de taille augmente d'environ 0.54 unité pour
# chaque augmentation d'une unité de l'âge.

# Question 3
# Ajout de la droite de régression et du barycentre du nuage de points
abline(regression,col="red")
points(mean(AGE),mean(TTAILLE),pch="+",col="blue",cex=1.5)
# argument cex=1.5 pour augmenter la taille du marqueur
# on vérifie la propriété vue en cours : 
# la droite de régression passe par le point moyen du nuage

# Question 4
# sert à vérifier la normalité des erreurs et à repérer d'éventuels points aberrants
qqnorm(regression$residuals)
qqline(regression$residuals)
# on voit que les points ne sont pas vraiment 
# alignés ce qui remet en cause l'hypothèse de normalité des erreurs
hist(regression$residuals,freq=FALSE)
#l'histogramme des résidus est légèrement asymétrique avec un étalement à gauche
#ce qui confirme que l'hypothèse de normalité des erreurs n'est pas vérifiée tout à fait sur ces données. 

res_std=regression$residuals/11.28
#calcul des résidus standardisés (11.28 désigne l'écart-type des résidus)
# cette valeur est la "residual standard error " et se lit dans la sortie de summary (regression)

plot(res_std,ylim=c(-5,5)) #Représentation graphique des résidus standardisés
abline(h=-2,col="red")
abline(h=2,col="red")

# Les points aberrants sont ceux qui ont un résidu standardisé > 2 en valeur absolue
identify(1:length(regression$residuals),regression$residuals/11.28) 
help(identify)
which(res_std< -2) # points aberrants (obs 36 et 51)
which(res_std> 2) # points aberrants (obs 11 et 56)
# Obtenir les indices des observations aberrantes dans les données d'origine
aberrant_indices <- c(which(res_std < -2), which(res_std > 2))
# Afficher les indices des observations aberrantes
print(aberrant_indices)
# Afficher le graphique avec les points aberrants mis en évidence
plot(AGE, TTAILLE, type="n", main="Liaison entre le tour de taille et l'âge", xlab="Âge",
     ylab="Tour de taille")
text(AGE, TTAILLE, labels=1:length(regression$residuals), cex=0.8)
points(AGE[aberrant_indices], TTAILLE[aberrant_indices], pch=16, col="red", cex=1.5)
# l'individu 56 a un tour de taille très eleve par rapport à son âge, ce qui 
# explique que son tour de taille soit mal prédit par la droite

#Partie 2 : régression linéaire multiple
# Question 1 
reg = lm(TTAILLE~SEXE+TAILLE+CHOL+IMC)
summary(reg)
# *** ==> très significative 
# coeff de determination : R2 = 0.87 87% de la variation du tour de taille est expliqué par la variation
# des variables explicatives du modèle, à savoir le sexe (recodé avec les labels "homme" et "femme"), 
# la taille (en cm), le  taux de cholestérol (en g/L), l'imc
# ce coefficient de détermination est proche de 1, donc on a affaire à un 
# bon modèle qui ajuste bien les données

# Test de validité globale du modèle
#H0 : Tous les paramètres sont nuls sauf la constante
#H0 : a_1=a_2=a_3=a_4=0
# pvalue< 2.2e-16 <5% donc on rejette H0 : le modèle est gobalement valide (au moins l'un des 
# paramètres n'est pas nul)

#Tests de significativité des paramètres
# les variables qui ont un effet significatif sur le tour de taille
# sont celles dont la p-valeur du test de significativité est inférieure 
#  à 5 %. On lit la p-valeur dans la dernière colonne du tableau coefficients
# par exemple, test de H0 :a_2=0 contre H1 a_2 différent de 0
# p-value< 2e-16 <5% donc on rejette H0 
# et la variable TAILLE a un effet significatif sur le tour de taille
# en comparant toutes les p-valeurs à 5 %, on peut donc conclure que la taille et l'imc 
# sont significatives mais pas le sexe (p=0.80 > 0.05) et le taux de cholestérol (p=0.9 > 0.05)

# Question 2 
#Ici, on veut un pas à pas avec
# un critère qui permette d'avoir à la fin toutes les variables significatives
# On va le faire "à la main"
# on enlève d'abord la variable la moins significative du modèle (celle avec
# la plus grande p-valeur de la sortie de reg
#  à savoir CHOL  
reg2 = lm(TTAILLE~SEXE+TAILLE+IMC)
summary(reg2)
# SEXE est NS donc on l'enlève
reg3 = lm(TTAILLE~TAILLE+IMC)
summary(reg3)
# toutes les variables sont significatives donc on a fini le pas à pas 
# et on retient comme modèle final le modèle 3

#Modèle 3 :  TTAILLE_i = c_0+ c_1*TAILLE_i+c_2+*TAILLE_i+c_3+e_i pour i=1,...,n
# On conclut ici aussi que le modèle est globalement valide (avec le test)
# quant au R2 ajusté, il est quasiment identique et comme on préfère 
# garder un modèle avec moins de paramètres et où toutes les variables
# sont significatives, on retient bien le modèle 3

# Test de validité globale du modèle
#H0 : Tous les paramètres sont nuls sauf la constante
#H0 : a_1=a_2=0
# pvalue< 2.2e-16 <5% donc on rejette H0 : le modèle est gobalement valide (au moins l'un des 
# paramètres n'est pas nul)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -43.42253   10.87473  -3.993 0.000189 ***
#  TAILLE        0.42729    0.06178   6.916 4.39e-09 ***
#  IMC           2.32966    0.12764  18.252  < 2e-16 ***

# c1_chapeau = 0.42729
# Toutes choses égales par ailleurs, 1 cm supplémentaire  fait augmenter le tour de taille de 0.43 cm

# c2_chapeau = 2.32966
# Toutes choses égales par ailleurs, une augmentation d'une unité dans l'indice de masse corporelle (IMC) 
# est associée à une augmentation de 2.33 cm dans le tour de taille.

# Question 3 
qqnorm(reg3$residuals)  
qqline(reg3$residuals) # assez alignés
hist(reg3$residuals,freq=FALSE) # assez symétrique 
# les résidus pourraient suivre approximativement une distribution normale
res_std4=reg3$residuals/4.91
plot(res_std4,ylim=c(-2.5,2.5))
abline(h=-2,col="red")
abline(h=2,col="red")
# Identification des points aberrants
aberrant_points <- which(abs(res_std4) > 2)# 4 points aberrants ici : 14 33 41 44
# Mettre en surbrillance les points aberrants sur le graphique
points(aberrant_points, res_std4[aberrant_points], pch = 20, col = "red")

detach(data_imc_TP2_groupe1)
