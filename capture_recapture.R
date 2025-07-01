rm(list = ls()) ## Pour nettoyer mon environnement de travail

## Installation des packages nécessaire pour le projet
install.packages("dplyr")
install.packages("tidyverse")
install.packages("Rcapture")

## Charger les librairie pour commencer le travail
library(dplyr)
library(tidyverse)

# Fixer une graine aléatoire pour la reproductibilité
set.seed(2025)

# Supposons 1000 entreprises au total
n_total <- 1000

# On suppose que 200 entreprises fraudent réellement (mais toutes ne sont pas détectées)
fraudeurs_reels <- sample(1:n_total, 200)

# AGIRC-ARRCO détecte 120 fraudeurs parmi ces 200
det_agirc <- sample(fraudeurs_reels, 120)

# URSSAF détecte 100 fraudeurs, dont un certain recouvrement avec AGIRC
det_urssaf <- sample(fraudeurs_reels, 100)

# Construire une base binaire pour capture-recapture
# 1 = détecté, 0 = non détecté
# On ne garde que les ID détectés par au moins un organisme

all_ids <- union(det_agirc, det_urssaf)

# Créer un data frame indiquant la présence dans chaque source
df_capture <- data.frame(
  id_entreprise = all_ids,
  AGIRC = as.integer(all_ids %in% det_agirc),
  URSSAF = as.integer(all_ids %in% det_urssaf)
)

## Aperçu de la base de données 
head(df_capture)

## MODELISATION AVEC LE PACKAGE RCAPTURE

## Le package Rcapture est conçu pour estimer la population totale à partir de données de recapture 
#(comme ici : détection par AGIRC et URSSAF).

# Nous allons utiliser la fonction closedp() qui applique plusieurs modèles log-linéaires
# et donne une estimation du nombre total d'individus (fraudeurs ici),
# y compris ceux non observés dans aucun fichier.

## Charger la librairie Rcapture
library(Rcapture)

# Utiliser la fonction closedp pour estimer la population totale de fraudeurs
# On lui passe uniquement les colonnes de captures (AGIRC et URSSAF)

modele <- closedp(df_capture[, c("AGIRC", "URSSAF")])

## Afficher les résultats
print(modele)

# Interprétation technique

# Le modèle Mt est statistiquement le mieux ajusté (AIC et BIC les plus faibles).
# Il estime qu’il y a environ 200 fraudeurs au total, alors que seulement 160 ont été détectés.
#Cela signifie qu’environ 40 entreprises fraudeuses (20%) n’ont été détectées par aucun des deux organismes.
# Le modèle Mb, plus conservateur, estime seulement 180 fraudeurs, ce qui ferait 20 non détectés.
# Le modèle M0 donne une estimation plus haute (42 non détectés), mais avec un ajustement un peu moins bon.

                ## Conclusion opérationnelle
# Grâce à cette estimation, nous mettons en évidence l’existence probable 
# d’un noyau invisible de fraudeurs (non détectés par les contrôles actuels
#estimé entre 20 et 42 entreprises selon les modèles.

##        VISUALISATION DES RESULTATS

# Chargement de la librairie ggplot2
library(ggplot2)

# Créer un dataframe des résultats
res_fraude <- data.frame(
  Modele = c("M0", "Mt", "Mb"),
  Estimation_totale = c(201.7, 200.0, 180.0),
  Observes = 160
)

# Calculer les non détectés
res_fraude$Non_detectes <- res_fraude$Estimation_totale - res_fraude$Observes

# Convertir les données en format long pour ggplot
library(tidyr)
df_plot <- res_fraude %>%
  select(Modele, Observes, Non_detectes) %>%
  pivot_longer(cols = c(Observes, Non_detectes), names_to = "Type", values_to = "Nombre")

# Tracer le graphique
ggplot(df_plot, aes(x = Modele, y = Nombre, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Estimation du nombre de fraudeurs (observés vs non détectés)",
       x = "Modèle statistique",
       y = "Nombre de fraudeurs",
       fill = "Type de détection") +
  scale_fill_manual(values = c("Observes" = "#1f77b4", "Non_detectes" = "#ff7f0e")) +
  theme_minimal(base_size = 14)


library(plotly)

# Créer le graphique ggplot comme avant
p <- ggplot(df_plot, aes(x = Modele, y = Nombre, fill = Type, 
                         text = paste("Modèle:", Modele, "<br>",
                                      "Type:", Type, "<br>",
                                      "Nombre:", Nombre))) +
  geom_bar(stat = "identity") +
  labs(title = "Estimation du nombre de fraudeurs (observés vs non détectés)",
       x = "Modèle statistique",
       y = "Nombre de fraudeurs",
       fill = "Type de détection") +
  scale_fill_manual(values = c("Observes" = "#1f77b4", "Non_detectes" = "#ff7f0e")) +
  theme_minimal(base_size = 14)

# Convertir en graphique interactif plotly
ggplotly(p)

## Ajouter les intervalles de confiances

## Chargement des librairie
library(ggplot2)
library(dplyr)
library(tidyr)

# Résultats du modèle
res_fraude <- data.frame(
  Modele = c("M0", "Mt", "Mb"),
  Estimation_totale = c(201.7, 200.0, 180.0),
  Std_err = c(11.8, 11.5, 9.5),
  Observes = 160
)


# Calculer les bornes de l'intervalle de confiance à 95 % loi normal 
# centrée et réduite

res_fraude <- res_fraude %>%
  mutate(
    CI_lower = Estimation_totale - 1.96 * Std_err,
    CI_upper = Estimation_totale + 1.96 * Std_err,
    Non_detectes = Estimation_totale - Observes
  )


# Graphique à barres + intervalle de confiance
ggplot(res_fraude, aes(x = Modele)) +
  geom_bar(aes(y = Observes, fill = "Observés"), stat = "identity") +
  geom_bar(aes(y = Non_detectes, fill = "Non détectés"), stat = "identity", bottom = res_fraude$Observes) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.2,
    color = "black",
    linewidth = 1.2
  ) +
  scale_fill_manual(values = c("Observés" = "#1f77b4", "Non détectés" = "#ff7f0e")) +
  labs(
    title = "Estimation du nombre total de fraudeurs avec IC à 95%",
    x = "Modèle statistique",
    y = "Nombre de fraudeurs",
    fill = "Type de détection"
  ) +
  theme_minimal(base_size = 14)


p2 <- ggplot(res_fraude, aes(x = Modele)) +
  geom_bar(aes(y = Observes, fill = "Observés"), stat = "identity") +
  geom_bar(aes(y = Non_detectes, fill = "Non détectés"), stat = "identity", 
           position = position_stack(reverse = TRUE)) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.2,
    color = "black",
    linewidth = 1.2
  ) +
  scale_fill_manual(values = c("Observés" = "#1f77b4", "Non détectés" = "#ff7f0e")) +
  labs(
    title = "Estimation du nombre total de fraudeurs avec IC à 95%",
    x = "Modèle statistique",
    y = "Nombre de fraudeurs",
    fill = "Type de détection"
  ) +
  theme_minimal(base_size = 14)

# Convertir en graphique interactif avec plotly
ggplotly(p, tooltip = c("y", "fill")) %>%
  layout(hovermode = "x unified")