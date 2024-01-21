# Installation des packages nécessaires pour l'analyse
install.packages('MASS')    # Installation de MASS pour des fonctions statistiques et des jeux de données
install.packages('questionr') # Installation de questionr pour des fonctions utiles en statistiques, y compris le calcul des Odds Ratios
install.packages("blorr")   # Installation de blorr pour des analyses et visualisations avancées de la régression logistique
install.packages("dplyr")   # Installation de dplyr pour la manipulation de données et des opérations de traitement de données


library("MASS")    # Chargement du package MASS. Il contient des fonctions pour l'analyse statistique et des jeux de données.

library("questionr") # Chargement du package questionr. Utile pour des fonctions d'analyse statistique, notamment pour les Odds Ratios.

library("ROCR")    # Chargement du package ROCR. Ce package est utilisé pour évaluer les performances des modèles prédictifs, notamment via la courbe ROC.

library("blorr")   # Chargement du package blorr. Fournit des outils pour l'analyse et la visualisation de modèles de régression logistique.

library("psych")   # Chargement du package psych. Il est utile pour les analyses psychométriques et la description statistique des données.


data("birthwt", package = "MASS") # charger dataset 
print(head(birthwt)) # afficher 

head(birthwt)  # afficher les premiers lignes 

tail(birthwt) #afficher les derniers 

str(birthwt)#ptl ftv

dim(birthwt) # dimension 

birthwt$race= as.factor(birthwt$race) # # Convertir la variable 'race' en facteur
birthwt$race

=> On a 3 niveaux

birthwt$smoke= as.factor(birthwt$smoke) # Convertir la variable 'smoke' en facteur
birthwt$smoke

birthwt$ht= as.factor(birthwt$ht) # # Convertir la variable 'ht' en facteur
birthwt$ht

birthwt$ui=as.factor(birthwt$ui) #  ui
birthwt$ui

birthwt$low= as.factor(birthwt$low) # low 
birthwt$low

table(birthwt$low)

table(birthwt$ui)

table(birthwt$ht)# classe minoritaire

table(birthwt$smoke)

table(birthwt$race)

table(birthwt$ptl)

table(birthwt$ftv)

summary(birthwt)

data=birthwt

describe(birthwt$age)
hist(data$age)

hist(data$lwt)

table(data$smoke)
birthwt=data

# Tracé du poids du bébé en fonction de l'âge de la mère
plot(birthwt$age, birthwt$bwt, 
     xlab="Age de la mère", 
     ylab="Poids du bébé", 
     xlim=c(10,50), 
     ylim=c(700,5000))
# Cette commande crée un graphique à points (scatterplot) avec l'âge de la mère sur l'axe des x et le poids du bébé sur l'axe des y.

# Ajout d'une ligne de régression
abline(lm(bwt ~ age, data=birthwt), col="red")
# Cette ligne ajoute une ligne de régression linéaire au graphique, représentant la tendance du poids du bébé en fonction de l'âge de la mère.


data = subset(birthwt ,select= -bwt)
head(data)

# Cette ligne initialise la graine du générateur de nombres aléatoires avec la valeur 123.
set.seed(123)
# Cette ligne crée un vecteur 'index_train' contenant un échantillon aléatoire de 90 % des indices de lignes de la variable 'data'.
index_train = sample(1:nrow(data), 0.9 * nrow(data))
# Cette ligne crée un nouveau jeu de données 'data_train' en extrayant les lignes de 'data' correspondant aux indices stockés dans 'index_train'.
data_train = data[index_train,]
# Cette ligne crée un autre jeu de données 'data_test' en extrayant les lignes de 'data' qui ne sont pas incluses dans 'index_train'.
data_test = data[-index_train,]
# Cette ligne affiche les six premières lignes de la variable 'data'.
head(data)


str(data)

cat("Number of training data:", nrow(data_train), "\n")
cat("Test data count:", nrow(data_tes), "\n")

#y et x sont quatitatives

modelcomplet = glm(low~.,data=data_train,family=binomial(link="logit"))

#  nous calculons l'AIC pour le modèle "modelcomplet".
aic_value = AIC(modelcomplet)

aic_value # affichage aic 

final_model =step(modelcomplet,direction ="both")

summary(final_model)  #Modèle de régression logistique binaire

aic_value = AIC(final_model)
print(aic_value)

influences = influence.measures(final_model)
print(influences)


significant_vars = summary(final_model)$coefficients[, 4] < 0.05
print(significant_vars)

# Supposons que 'modele_logit' est notre modèle de régression logistique
# Calcul des Odds Ratios et des intervalles de confiance

#ou bien
odds_ratios=odds.ratio(final_model)

print(odds_ratios)

coef(final_model)
OR=exp(coef(final_model))
print(OR)
CI=exp(confint(final_model))
print(CI)


ORCI=cbind(OR,CI)
ORCI

cookdistance = cooks.distance(final_model)  # la distance de cookie 

print(cookdistance)

prediction= predict(final_model,newdata = data_tes,type="response")


print(prediction)

length(data_tes) # longueur de test  10%

conf_matrix =table(data_tes$low, ifelse(prediction>0.5,0,1))
print(conf_matrix)

length(ifelse(prediction>0.5,0,1))

accuracy =sum(diag(conf_matrix))/sum(conf_matrix)

accuracy
L'accuracy de votre modèle est de 0.36, ce qui signifie que 36% des prédictions étaient correctes.
Cette performance est relativement faible, indiquant que le modèle pourrait nécessiter des améliorations ou des ajustements.
sensitivity =conf_matrix[2,2] /sum(conf_matrix[,2])

sensitivity
Cela signifie que votre modèle a correctement identifié 20% des cas réels positifs. En d'autres termes, il a manqué 80% des cas positifs 
Une faible sensibilité indique que le modèle n'est pas très efficace pour détecter les cas positifs.
specifictity =conf_matrix[1,1] /sum(conf_matrix[,1])

specifictity

roc_pred =prediction(prediction,data_tes$low) #'roc_pred' est un vecteur de probabilités prédites par notre modèle
print(roc_pred)

# Utilisation d'une fonction alternative pour évaluer le modèle

auc_value =as.numeric(performance(roc_pred,"auc")@y.values)

print(auc_value)# Affichage de l'AUC


roc_curve=performance(roc_pred,"tpr","fpr") # tpr = true positif , frp : faux positive 
#perf = performance(pred, "tpr", "fpr")
roc_curve

plot(roc_curve ,main="courbe roc",col="red",lwd=3)

#blorr::eval_binomial(tes_data,prediction)
blr_confusion_matrix(final_model,cutoff=0.5)
La matrice de confusion montre la performance d'un classificateur binaire. On y voit 105 vrais négatifs et 21 vrais positifs, indiquant les prédictions correctes.
* L'exactitude du modèle est de 74,12%, signifiant qu'il prédit correctement les issues 74,12% du temps.
* La sensibilité (ou rappel) est de 38,18%, indiquant la proportion de positifs réels bien identifiés, tandis que la spécificité est de 91,30%, reflétant la proportion de négatifs réels bien identifiés. 
* La valeur prédictive positive est de 67,74%, et la valeur prédictive négative est de 75,54%, qui indiquent respectivement la probabilité que les prédictions de classe positive/négative soient correctes. 
* Le test de McNemar donne une valeur-p de 0,0005, ce qui peut indiquer une différence significative entre les taux de faux positifs et de faux négatifs. 
* Le Kappa de 0,3327 montre une concordance modérée au-delà du hasard entre la prédiction et la référence.
# 6. Transformation des variables 'ftv' et 'ptl' en variables qualitatives

birthwt$ftv = cut(birthwt$ftv, breaks = c(-Inf, 0, 1, 2, Inf), labels = c("0", "1", "2", "3+"), include.lowest = TRUE) # 4 MODALIT7B
birthwt$ptl = cut(birthwt$ptl, breaks = c(-Inf, 0, Inf), labels = c("0", "1+"), include.lowest = TRUE)  #2 Modalité 

birthwt$ftv 

#### 04 modalités

birthwt$ptl

#### 02 modalités

# 7. Nouvelle division des données après codage

indices_new = sample(1:nrow(birthwt), size = 0.9 * nrow(birthwt))
train_data_new = birthwt[indices_new, ]
test_data_new = birthwt[-indices_new, ]

# Régression logistique binaire sur la nouvelle base de données

model_new = glm(low ~ ., data = data_train, family = binomial())
summary(model_new)

model_new = glm(low ~ ., data = train_data_new, family = binomial())
model_step_new = step(model_new, trace = 0)
print(summary(model_step_new))
odds_ratios_new = odds.ratio(model_step_new)
print(odds_ratios_new)

summary(model_step_new)

## 8. Évaluation des indicateurs de performance 

colnames(data_tes)

probs_new = predict(model_step_new, newdata = test_data_new, type = "response")
print(probs_new)

# Les predictions 

preds_new =ifelse(probs_new > 0.5, 1, 0)
print(preds_new)

# Matrice conf

conf_matrix_new = table(Predicted = preds_new, Actual = test_data_new$low)
print(conf_matrix_new)

pred_new = prediction(probs_new, test_data_new$low)
auc_value =as.numeric(performance(pred_new,"auc")@y.values)
auc_value
Interprétation de l'AUC :
Une valeur AUC de 0,5 indique une performance équivalente à un hasard aléatoire.
Une valeur AUC plus proche de 1,0 indique une meilleure capacité du modèle à distinguer correctement entre les classes positives et négatives.
Une valeur AUC inférieure à 0,5 suggérerait que le modèle fait pire que le hasard, bien que cela soit rare dans la pratique
roc_curve2=performance(pred_new,"tpr","fpr")
#perf = performance(pred, "tpr", "fpr")
roc_curve2

# Tracé de la courbe ROC
# Calcul de l'AUC, sensibilité, spécificité, etc.
plot(roc_curve2, main="Courbe ROC", col="red")

#8

#model_performance =blorr::blr_model_performance(model_step_new)
blr_confusion_matrix(model_step_new,cutoff=0.5)

## Interpretation 
