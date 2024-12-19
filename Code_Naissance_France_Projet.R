#Code Final

# Nettoyage de l'environnement

# Chargement des librairies
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(tseries) # pour adf.test

# Charger les données
cat("Chargement des données...\n")
data <- read_excel("//Users/mehdifehri/Desktop/R/Données/Data R Ajustée.xlsx") %>%
  rename_with(~ gsub("-", "_", .), everything()) %>%
  # Suppression de certaines colonnes non nécessaires
  select(-fem, -sco_jeune, -pop, -parc_logement, -viedans5, -acceuilenf, -agemat, -solo, -loyers, 
         -tailleMenage, -consoalcool, -opi_surpoids, - opi_nervosite, -opi_depression, -chomagefem, -opi_famille,
         -inegalité_w_privée, -synthé_Oiseau, -tx_emploifem, -depseniors, -pib_hab, -wage_h, -opi_work_fem,
         -lt_interest_rate,-cadre_vie,-opi_guerre, -opi_amelio_niv_vie, -opi_violence,)

cat("Les colonnes spécifiées ont été supprimées avec succès.\n")


##############################################################
# VISUALISATION DES TENDANCES DES VARIABLES EXPLICATIVES
##############################################################

# Identifier les colonnes numériques (hors 'Temps' et 'fec')
numeric_cols <- setdiff(colnames(data), c("Temps", "fec"))

for (col in numeric_cols) {
  print(
    ggplot(data, aes_string(x = "Temps", y = col)) +
      geom_line(color = "blue") +
      labs(title = paste("Tendance de la variable", col), x = "Temps", y = col) +
      theme_minimal()
  )
}

##############################################################
# INTERPOLATION TRIMESTRIELLE ET DÉCALAGE TEMPOREL
##############################################################

# Identifier les colonnes numériques (hors 'Temps')
numeric_cols <- setdiff(colnames(data), "Temps")

# Interpolation trimestrielle et création des variables décalées (lags)
data_clean <- data %>%
  complete(Temps = seq(min(Temps), max(Temps), by = 0.25)) %>%
  mutate(across(all_of(numeric_cols),
                ~ approx(Temps[!is.na(.)], .[!is.na(.)], xout = Temps)$y)) %>%
  arrange(Temps) %>%
  mutate(across(setdiff(numeric_cols, "fec"), ~ lag(., n = 4), .names = "lag_{col}")) %>%
  drop_na(starts_with("lag_"))

# Sélectionner uniquement les colonnes nécessaires pour le modèle
data_work <- data_clean %>%
  select(Temps, fec, starts_with("lag_"))

# Sauvegarder le DataFrame intermédiaire
write_xlsx(data_work, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work.xlsx")
cat("Le fichier 'Data_Work.xlsx' a été sauvegardé après l'interpolation et le lag.\n")

rm(data)
rm(data_clean)

##############################################################
# PREPARATION DE LA SERIE 'fec' POUR L'ANALYSE
##############################################################

# Installer et charger les packages nécessaires
# install.packages("readxl")
# install.packages("tseries") # pour adf.test
library(readxl)
library(tseries)
library(ggplot2)
library(dplyr)

# 1. Lecture du fichier Excel
data_work <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work.xlsx")

# Vérifier la présence des colonnes "Temps" et "fec"
if(!("Temps" %in% colnames(data_work)) | !("fec" %in% colnames(data_work))) {
  stop("Les colonnes 'Temps' et/ou 'fec' n'existent pas dans le data_work.")
}

# 2. Définir les variables explicatives : toutes sauf "fec" et "Temps"
vars_explicatives <- setdiff(colnames(data_work), c("fec", "Temps"))

# 3. Visualiser la variable fec dans le Temps
plot(data_work$Temps, data_work$fec, type = "l",
     main = "Série temporelle de fec (brute)",
     xlab = "Temps", ylab = "fec", col = "blue")

# 4. Estimation de la tendance linéaire
modele_tendance <- lm(fec ~ Temps, data = data_work)
tendance_estimee <- fitted(modele_tendance)

# Ajout de la tendance sur le graphique
lines(data_work$Temps, tendance_estimee, col = "red", lwd = 2)
title(sub = "La ligne rouge représente la tendance estimée")

# 5. Test ADF sur la série brute fec
cat("\n--- Test ADF sur la série brute (fec) ---\n")
adf_result_fec <- adf.test(data_work$fec, alternative = "stationary")
print(adf_result_fec)
if (adf_result_fec$p.value < 0.05) {
  cat("\nInterprétation : La série brute 'fec' est stationnaire (H₀ rejetée).\n")
} else {
  cat("\nInterprétation : La série brute 'fec' n'est pas stationnaire (H₀ non rejetée).\n")
}

# 6. Dé-trending de la série fec
data_work$fec_detrend <- data_work$fec - tendance_estimee

# Test ADF sur la série détrendue
cat("\n--- Test ADF sur la série détrendue (fec_detrend) ---\n")
adf_result_detrend <- adf.test(data_work$fec_detrend, alternative = "stationary")
print(adf_result_detrend)

# 7. Différenciation de la série détrendue
data_work$fec_detrend_diff <- c(NA, diff(data_work$fec_detrend))

# Test ADF sur la série détrendue différenciée
cat("\n--- Test ADF sur la série détrendue différenciée (fec_detrend_diff) ---\n")
adf_result_detrend_diff <- adf.test(na.omit(data_work$fec_detrend_diff), alternative = "stationary")
print(adf_result_detrend_diff)

# 8. Différenciation directe de la série brute
data_work$fec_diff <- c(NA, diff(data_work$fec))

# Test ADF sur la série brute différenciée
cat("\n--- Test ADF sur la série brute différenciée (fec_diff) ---\n")
adf_result_fec_diff <- adf.test(na.omit(data_work$fec_diff), alternative = "stationary")
print(adf_result_fec_diff)


# 9. Créer data_work_trend avec les variables nécessaires
# Inclure toutes les variables (Temps, fec, fec_detrend, fec_detrend_diff, fec_diff et création de fec_diff_relative)
data_work_trend <- data.frame(
  Temps = data_work$Temps,
  fec = data_work$fec,
  fec_detrend = data_work$fec_detrend,
  fec_detrend_diff = data_work$fec_detrend_diff,
  fec_diff = data_work$fec_diff
) %>%
  mutate(fec_var_relative = fec_diff / fec) # Ajouter fec_diff_relative

# Ajouter les variables explicatives
for (var in vars_explicatives) {
  data_work_trend[[var]] <- data_work[[var]]
}


# 10. Nettoyer le data_work_trend pour supprimer les lignes avec des NA
data_work_trend <- data_work_trend[complete.cases(data_work_trend), ]

# 11. Créer le dataframe fec_transformed (sans variables explicatives)
# On garde que Temps, fec, fec_detrend, fec_detrend_diff, fec_diff
fec_transformed <- data_work_trend[, c("Temps", "fec", "fec_detrend", "fec_detrend_diff", "fec_diff","fec_var_relative")]


# 12. Visualisation séparée des séries transformées
# Fec détrendue sur le temps
plot(fec_transformed$Temps, fec_transformed$fec_detrend, type = "l", col = "purple",
     main = "Fec détrendue sur le Temps",
     xlab = "Temps", ylab = "fec_detrend")

# Superposer fec_detrend_diff et fec_diff sur un seul graphique avec un label en bas
plot(data_work_trend$Temps, data_work_trend$fec_detrend_diff, type = "l", col = "green",
     main = "Comparaison : Fec détrendue différenciée et Fec brute différenciée",
     xlab = "Temps", ylab = "Valeurs", lwd = 2)
lines(data_work_trend$Temps, data_work_trend$fec_diff, col = "blue", lwd = 2)

# Ajouter des labels en bas
legend("bottom", legend = c("Fec détrendue différenciée", 
                            "Fec brute différenciée"), 
       col = c("green", "blue"), lwd = 2, bty = "n", horiz = TRUE)


# 13. Tableau résumé des tests ADF
# Rassembler les p-values et la conclusion stationnaire dans un tableau
stationarity_results <- data.frame(
  Variable = c("fec", "fec_detrend", "fec_detrend_diff", "fec_diff"),
  p_value = c(adf_result_fec$p.value, adf_result_detrend$p.value, adf_result_detrend_diff$p.value, adf_result_fec_diff$p.value),
  Stationary = ifelse(c(adf_result_fec$p.value, adf_result_detrend$p.value, adf_result_detrend_diff$p.value, adf_result_fec_diff$p.value) < 0.05, 
                      "Stationary", "Non-stationary")
)

cat("\n--- Résumé des tests ADF ---\n")
print(stationarity_results)

cat("\nTout est terminé.\n")

# 1. Modèle : Fec expliquée par Fec_detrend
modele_fec_vs_detrend <- lm(fec ~ fec_detrend, data = data_work_trend)
r2_fec_vs_detrend <- summary(modele_fec_vs_detrend)$r.squared

# 2. Modèle : Fec_detrend_diff expliquée par Fec_diff
modele_detrend_diff_vs_diff <- lm(fec_detrend_diff ~ fec_diff, data = data_work_trend)
r2_detrend_diff_vs_diff <- summary(modele_detrend_diff_vs_diff)$r.squared

# Afficher les résultats
cat("Proportion de variance expliquée :\n")
cat(sprintf("- Fec expliquée par Fec_detrend : %.2f%%\n", r2_fec_vs_detrend * 100))
cat(sprintf("- Fec_detrend_diff expliquée par Fec_diff : %.2f%%\n", r2_detrend_diff_vs_diff * 100))

# Ajuster le dataframe data_work_trend
data_work_trend <- data_work_trend %>%
  select(Temps, fec_var_relative, everything(), -fec, -fec_detrend, -fec_detrend_diff, -fec_diff) # Réorganiser et supprimer les colonnes inutiles

write_xlsx(data_work_trend, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work_trend.xlsx")


rm(adf_result_detrend)
rm(adf_result_detrend_diff)
rm(adf_result_fec)
rm(adf_result_fec_diff)
rm(data_work)
rm(stationarity_results)
rm(modele_detrend_diff_vs_diff)
rm(modele_fec_vs_detrend)
rm(modele_tendance)


##############################################################
# Standardisation des données 
##############################################################

data_work_trend <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work_trend.xlsx")

# Renommer 'fec_diff_relative' en 'fec'
data_work_trend <- data_work_trend %>%
  rename(fec = fec_var_relative)

# 1. Modèle standardisé (centré et réduit)
data_model_standardized <- data_work_trend %>%
  mutate(across(starts_with("lag_"), ~ scale(.)))



##############################################################
# CALCUL DES R² POUR LE MODÈLE STANDARDISÉ
##############################################################

cat("\n### Calcul des R² pour le modèle standardisé ###\n")

# Calculer le \(R^2\) pour chaque variable explicative du modèle standardisé
r2_standardized <- data_model_standardized %>%
  summarise(across(starts_with("lag_"), 
                   ~ summary(lm(data_model_standardized$fec ~ .))$r.squared, 
                   .names = "R2_{col}")) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "R2") %>%
  mutate(Variable = gsub("^R2_", "", Variable)) %>%
  arrange(desc(R2))

# Afficher et sauvegarder les résultats des \(R^2\)
print(r2_standardized)
write_xlsx(r2_standardized, "//Users/mehdifehri/Desktop/R/R2_Standardized_Results.xlsx")

##############################################################
# RÉGRESSION INITIALE SUR LE MODÈLE STANDARDISÉ
##############################################################

cat("\n### Régression initiale sur le modèle standardisé ###\n")

# Modèle OLS sur le modèle standardisé
model_standardized <- lm(fec ~ . - Temps, data = data_model_standardized)
print(summary(model_standardized))


##############################################################
# DÉTECTION ET TRAITEMENT DES OUTLIERS SUR LE MODÈLE STANDARDISÉ
##############################################################

cat("\n### Détection et traitement des outliers sur le modèle standardisé ###\n")

# Étape 1 : Ajuster le modèle global sur `data_work1`
model_standardized <- lm(fec ~ ., data = data_work_trend %>% select(-Temps))

# Étape 2 : Calculer les résidus bruts et standardisés
residuals_standardized_df <- data.frame(
  Index = seq_len(nrow(data_work_trend)),
  Resid = residuals(model_standardized),      # Résidus bruts
  Std_Resid = rstandard(model_standardized),  # Résidus standardisés
  Temps = data_work_trend$Temps,
  fec = data_work_trend$fec
)

# Étape 3 : Visualiser les résidus bruts
plot_residus_bruts <- ggplot(residuals_standardized_df, aes(x = Index, y = Resid)) +
  geom_point(color = "darkorange") +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(
    title = "Résidus Bruts (Modèle Standardisé)",
    x = "Index d'observation",
    y = "Résidus bruts"
  ) +
  theme_minimal()

print(plot_residus_bruts)

# Étape 4 : Visualiser les résidus standardisés (zoomé sur ±3)
threshold_zoom <- 2
plot_residus_standardises <- ggplot(residuals_standardized_df %>% filter(abs(Std_Resid) <= threshold_zoom)) +
  geom_point(aes(x = Index, y = Std_Resid), color = "blue") +
  geom_hline(yintercept = c(-1, 1), color = "red", linetype = "dashed") +
  labs(
    title = "Résidus Standardisés (Zoomé)",
    x = "Index d'observation",
    y = "Résidus standardisés"
  ) +
  theme_minimal()

print(plot_residus_standardises)

# Étape 5 : Identifier les outliers
threshold <-2   # Seuil pour identifier les outliers
outliers_standardized_df <- residuals_standardized_df %>%
  filter(abs(Std_Resid) > threshold)

cat("\n### Résumé des outliers identifiés ###\n")
print(outliers_standardized_df)

# Étape 6 : Créer un nouveau DataFrame sans les outliers (`data_work2`)
data_work2 <- data_work_trend %>%
  mutate(Index = seq_len(nrow(data_work_trend))) %>%
  filter(!Index %in% outliers_standardized_df$Index) %>%
  select(-Index)

# Étape 7 : Tableau récapitulatif des outliers
recap_outliers <- outliers_standardized_df %>%
  select(Temps, fec, Std_Resid)

# Sauvegarder les résultats finaux
write_xlsx(data_work2, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work2.xlsx")
write_xlsx(recap_outliers, "//Users/mehdifehri/Desktop/R/Code Final Fec/Recap_Outliers.xlsx")

rm(recap_outliers)
rm(model_standardized)
rm(outliers_standardized_df)
rm(residuals_standardized_df)

##############################################################
# PARTIE 2 : DÉTECTION DES ALIAS (COLINÉARITÉ PARFAITE)
##############################################################

cat("\n### Détection des alias (colinéarité parfaite) ###\n")

# Étape 1 : Ajuster le modèle global pour `data_work2`
model_alias <- lm(fec ~ ., data = data_work2 %>% select(-Temps))

# Étape 2 : Utiliser le modèle global pour identifier les alias
alias_info <- alias(model_alias)

# Étape 3 : Extraire les colinéarités parfaites
alias_matrix <- alias_info$Complete  # Matrice de colinéarités parfaites

# Vérifier si des alias existent
if (is.null(alias_matrix)) {
  cat("Aucune colinéarité parfaite détectée dans le modèle.\n")
} else {
  # Étape 4 : Identifier les variables colinéaires
  alias_pairs <- which(alias_matrix != 0, arr.ind = TRUE)
  
  # Extraire les noms des variables impliquées dans les colinéarités
  alias_summary <- data.frame(
    Variable_1 = rownames(alias_matrix)[alias_pairs[, 1]],
    Variable_2 = colnames(alias_matrix)[alias_pairs[, 2]]
  ) %>%
    distinct()
  
  # Afficher la liste des colinéarités parfaites
  cat("Colinéarités parfaites détectées :\n")
  print(alias_summary)
  
  # Étape 5 : Sauvegarder les colinéarités dans un fichier Excel
  write_xlsx(alias_summary, "//Users/mehdifehri/Desktop/R/Colinear_Variables_Data_Work2.xlsx")
  cat("La liste des colinéarités parfaites a été sauvegardée dans 'Colinear_Variables_Data_Work2.xlsx'.\n")
}

# Étape 6 : Extraire les variables concernées par les alias
alias_vars <- rownames(alias_info$Complete)
cat("Variables impliquées dans les alias avec l'intercept :\n")
print(alias_vars)

# Étape 7 : Supprimer les variables alias et créer un nouveau DataFrame `data_work3`
data_work3 <- data_work2 %>%
  select(-all_of(alias_vars))

# Étape 8 : Sauvegarder le DataFrame mis à jour
write_xlsx(data_work3, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work3.xlsx")
cat("Le nouveau DataFrame sans alias a été sauvegardé sous 'Data_Work3.xlsx'.\n")

# Étape 9 : Résumer les variables supprimées
removed_variables_summary <- data.frame(Variables_Supprimées = alias_vars)
write_xlsx(removed_variables_summary, "//Users/mehdifehri/Desktop/R/Code Final Fec/Removed_Variables_Alias.xlsx")
cat("Résumé des variables supprimées enregistré sous 'Removed_Variables_Alias.xlsx'.\n")

rm(data_work2)
rm(alias_info)
rm(model_alias)
rm(removed_variables_summary)
rm(alias_matrix)
rm(alias_pairs)
rm(alias_summary)

##############################################################
# PARTIE 3 : ANALYSE DES VARIABLES EXPLICATIVES (CORRÉLATION, MDS, etc.)
##############################################################

library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)
library(reshape2)
library(stats)
library(igraph)

# Charger le DataFrame `data_work3` directement
data_expl <- data_work3 %>%
  select(-Temps, -fec)  # Exclure les colonnes non explicatives

# Calculer la matrice de corrélation
cor_mat <- cor(data_expl, use = "complete.obs")
write_xlsx(as.data.frame(cor_mat), "//Users/mehdifehri/Desktop/R/Code Final Fec/Correlation_Matrix_Data_Work3.xlsx")

cat("Matrice de corrélation (sans variable dépendante) :\n")
print(cor_mat)

# Heatmap
correlation_melted <- melt(cor_mat)
heatmap_plot <- ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = "Corrélation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Matrice de corrélation (data_work3)", x = "Variables", y = "Variables")

print(heatmap_plot)
ggsave("//Users/mehdifehri/Desktop/R/Code Final Fec/Correlation_Heatmap_Data_Work3.png", plot = heatmap_plot,
       width = 10, height = 8, dpi = 300)

cat("Matrice de corrélation affichée, sauvegardée en Excel et heatmap PNG.\n")

# Identification des paires de variables fortement corrélées
corr_threshold <- 0.5
high_corr_pairs <- correlation_melted %>%
  filter(value > corr_threshold & Var1 != Var2) %>%
  arrange(desc(value)) %>%
  mutate(pair = paste0(pmin(as.character(Var1), as.character(Var2)), "-", pmax(as.character(Var1), as.character(Var2)))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

cat("Paires de variables avec corrélation >", corr_threshold, ":\n")
print(high_corr_pairs)
write_xlsx(high_corr_pairs, "//Users/mehdifehri/Desktop/R/Code Final Fec/High_Correlation_Pairs_Data_Work3.xlsx")

# Compter les variables les plus fréquentes dans les paires corrélées
variable_counts <- high_corr_pairs %>%
  select(Var1, Var2) %>%
  pivot_longer(cols = everything(), values_to = "Variable") %>%
  group_by(Variable) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

cat("\n### Variables les plus corrélées ###\n")
print(variable_counts)


##############################################################
# 4. VISUALISATION DE LA STRUCTURE DES VARIABLES (DENDRO, MDS, RÉSEAU)
##############################################################

cat("\n### Visualisation des structures des variables ###\n")

# Création d'une matrice de distance basée sur 1 - |corr|
dist_mat <- as.dist(1 - abs(cor_mat))

### Dendrogramme
hc <- hclust(dist_mat, method = "complete")
pdf("//Users/mehdifehri/Desktop/R/Code Final Fec/Dendrogramme_Data_Work3.pdf", width = 10, height = 8)
plot(hc, main = "Dendrogramme des variables (data_work3)", xlab = "Variables", sub = "")
abline(h = 0.2, col = "red", lty = 2)  # Ajuster le seuil si nécessaire
dev.off()
cat("Dendrogramme sauvegardé en PDF.\n")

### MDS (Multi-Dimensional Scaling)
mds_res <- cmdscale(dist_mat, k = 2)
mds_df <- data.frame(
  Dim1 = mds_res[,1],
  Dim2 = mds_res[,2],
  Variable = rownames(mds_res)
)

mds_plot <- ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Variable)) +
  geom_point() +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Représentation MDS des variables (data_work3)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

print(mds_plot)
ggsave("//Users/mehdifehri/Desktop/R/Code Final Fec/MDS_Plot_Data_Work3.pdf", plot = mds_plot, width = 10, height = 8)
cat("MDS plot affiché et sauvegardé en PDF.\n")

### Graphe de réseau
high_corr <- melt(cor_mat) %>%
  filter(value > corr_threshold & Var1 != Var2) %>%
  mutate(pair = paste(pmin(as.character(Var1), as.character(Var2)),
                      pmax(as.character(Var1), as.character(Var2)), sep = "-")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

g <- graph_from_data_frame(high_corr[, c("Var1", "Var2")], directed = FALSE)

plot(g,
     layout = layout_with_fr(g),
     vertex.size = 5,
     vertex.label.cex = 0.5,
     main = paste("Graphe de réseau des variables (corr >", corr_threshold, ")"))
cat("Graphe de réseau affiché.\n")

rm(g)
rm(hc)
rm(high_corr)
rm(high_corr_pairs)
rm(mds_df)
rm(mds_res)
rm(variable_counts)
rm(data_expl)
rm(cor_mat)
rm(correlation_melted)
rm(data_work_trend)
rm(fec_transformed)
rm(alias_summary)

#####################################################################
# Test VIF et suppression des variables avec VIF élevé (standardisation incluse)
#####################################################################

# Charger les bibliothèques nécessaires
library(car)  # Pour le calcul du VIF
library(dplyr)
library(writexl)

data_work3 <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work3.xlsx")

cat("\n### Début de la détection des VIF élevés dans data_work3 ###\n")

# Identifier les variables explicatives
variables_explicatives <- setdiff(names(data_work3), c("fec", "Temps"))

# Créer une copie des données pour le processus VIF
data_for_vif <- data_work3

# Définir le seuil de VIF
vif_threshold <- 1500

# Initialiser les listes pour stocker les résultats
iteration_results <- list()
removed_variables <- data.frame(Iteration = integer(), Variable = character(), VIF_Value = numeric(), stringsAsFactors = FALSE)

# Boucle itérative pour supprimer les variables avec VIF élevé
iteration <- 1
while (TRUE) {
  cat("\n--- Itération", iteration, "---\n")
  
  # Ajuster un modèle linéaire avec les variables explicatives restantes
  current_model <- lm(fec ~ ., data = data_for_vif[, c("fec", variables_explicatives)])
  
  # Calculer le VIF pour chaque variable explicative
  vif_values <- vif(current_model)
  
  # Afficher les VIF actuels
  cat("Facteurs d'inflation de la variance (VIF) actuels :\n")
  print(vif_values)
  
  # Sauvegarder les résultats de l'itération
  iteration_results[[iteration]] <- data.frame(Variable = names(vif_values), VIF = vif_values, Iteration = iteration)
  
  # Identifier les variables avec un VIF supérieur au seuil
  high_vif_vars <- names(vif_values[vif_values > vif_threshold])
  
  # Vérifier s'il reste des variables avec un VIF élevé
  if (length(high_vif_vars) == 0) {
    cat("Toutes les variables ont un VIF <= ", vif_threshold, ". Fin de la boucle.\n")
    break
  }
  
  # Identifier la variable avec le VIF maximum
  variable_to_remove <- high_vif_vars[which.max(vif_values[high_vif_vars])]
  max_vif_value <- max(vif_values[high_vif_vars])
  cat("Variable avec le VIF le plus élevé :", variable_to_remove, "(", max_vif_value, ")\n")
  
  # Ajouter la variable supprimée à la liste des variables supprimées
  removed_variables <- rbind(removed_variables, data.frame(Iteration = iteration, Variable = variable_to_remove, VIF_Value = max_vif_value))
  
  # Supprimer cette variable des données et des variables explicatives
  data_for_vif <- data_for_vif %>% select(-all_of(variable_to_remove))
  variables_explicatives <- setdiff(variables_explicatives, variable_to_remove)
  
  # Augmenter le compteur d'itérations
  iteration <- iteration + 1
}

data_work4 <- data_for_vif

# Sauvegarder les données finales sans VIF élevé
write_xlsx(data_work4, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4.xlsx")
cat("Les données finales après suppression des variables avec VIF > ", vif_threshold, " ont été sauvegardées sous 'Data_Work4.xlsx'.\n")



# Imprimer les noms des colonnes du nouveau DataFrame `data_work4`
cat("\nNoms des colonnes du DataFrame `data_work4` :\n")
print(names(data_for_vif))

# Sauvegarder le DataFrame
write_xlsx(data_work4, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_work4.xlsx")
cat("Le fichier a été sauvegardé avec succès.\n")



rm(current_model)
rm(data_work3)
rm(iteration_results)
rm(removed_variables)
rm(data_for_vif)


##############################################################
####### Régression sur modèle initial et modèle auxiliaire log #######
##############################################################

library(readxl)
library(dplyr)
library(lmtest)
library(writexl)

# Charger les données
data_work4 <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4.xlsx")
data_work4_log <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4_log.xlsx")

# Modèle OLS de base
model_ols <- lm(fec ~ . - Temps, data = data_work4)
print(summary(model_ols))

# Création d'un modèle auxiliaire avec variables explicatives log
# Transformer les variables explicatives en log avec un préfixe et nettoyer les non-log
data_work4_log <- data_work4 %>%
  mutate(across(-c(Temps, fec), log, .names = "log_{.col}")) %>%
  select(Temps, fec, starts_with("log_"))
write_xlsx(data_work4_log, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4_Log.xlsx")

# Modèle OLS avec les variables explicatives transformées en log
model_ols_log <- lm(fec ~ . - Temps, data = data_work4_log)
print(summary(model_ols_log))

# Test RESET pour le modèle OLS (base)
cat("\n### Test RESET pour le modèle OLS ###\n")
reset_test_ols <- resettest(model_ols, power = 2:3, type = "fitted")
print(reset_test_ols)

if (reset_test_ols$p.value > 0.05) {
  cat("\nInterprétation : Le modèle de base est correctement spécifié. (p-value =", reset_test_ols$p.value, ")\n")
} else {
  cat("\nInterprétation : Le modèle de base est mal spécifié. (p-value =", reset_test_ols$p.value, ")\n")
}

# Test RESET pour le modèle OLS log-transformé
cat("\n### Test RESET pour le modèle OLS (log-transformé) ###\n")
reset_test_ols_log <- resettest(model_ols_log, power = 2:3, type = "fitted")
print(reset_test_ols_log)

if (reset_test_ols_log$p.value > 0.05) {
  cat("\nInterprétation : Le modèle log-transformé est correctement spécifié. (p-value =", reset_test_ols_log$p.value, ")\n")
} else {
  cat("\nInterprétation : Le modèle log-transformé est mal spécifié. (p-value =", reset_test_ols_log$p.value, ")\n")
}

##############################################################
# TABLEAU COMPARATIF DES RÉSULTATS
##############################################################

# Initialisation du tableau récapitulatif
model_comparison <- data.frame(
  Model = character(),
  Adjusted_R2 = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  RESET_p_value = numeric(),
  RESET_Interpretation = character(),
  stringsAsFactors = FALSE
)

# Remplir le tableau pour chaque modèle
models <- list(
  "Model OLS" = model_ols,
  "Model OLS (Log)" = model_ols_log
)

# Fonction pour obtenir l'interprétation du test RESET
perform_reset <- function(model) {
  reset_results <- resettest(model, power = 2:3, type = "fitted")
  if (reset_results$p.value > 0.05) {
    interpretation <- "Correctement spécifié"
  } else {
    interpretation <- "Mal spécifié"
  }
  return(list(p_value = reset_results$p.value, interpretation = interpretation))
}

# Remplir le tableau avec les résultats pour chaque modèle
for (name in names(models)) {
  model <- models[[name]]
  summary_model <- summary(model)
  reset_results <- perform_reset(model)
  
  model_comparison <- rbind(model_comparison, data.frame(
    Model = name,
    Adjusted_R2 = summary_model$adj.r.squared,
    AIC = AIC(model),
    BIC = BIC(model),
    RESET_p_value = reset_results$p_value,
    RESET_Interpretation = reset_results$interpretation
  ))
}

# Afficher le tableau comparatif des modèles
print(model_comparison)

# Sauvegarder le tableau comparatif dans un fichier Excel
write_xlsx(model_comparison, "//Users/mehdifehri/Desktop/R/Code Final Fec/Model_Comparison.xlsx")

rm(model)
rm(model_comparison)
rm(models)
rm(summary_model)
rm(model_ols)
rm(model_ols_log)
rm(reset_results)
rm(reset_test_ols)
rm(reset_test_ols_log)

# Charger les bibliothèques nécessaires
library(broom)
library(writexl)
library(dplyr)

#############################################################
# Modèle final et visualisation des résidus
#############################################################

data_work4_log <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4_log.xlsx")
variables_explicatives_ols <- setdiff(colnames(data_work4_log), c("Temps", "fec"))
formule_ols <- as.formula(paste("fec ~", paste(variables_explicatives_ols, collapse = " + ")))

# Ajustement du modèle de régression
model_final <- lm(formule_ols, data = data_work4_log)

# Extraction des résidus
residus <- residuals(model_final)

cat("\n### Visualisations des résidus ###\n")

# Résidus vs valeurs ajustées
plot(fitted(model_final), residus, main = "Résidus vs valeurs ajustées",
     xlab = "Valeurs ajustées", ylab = "Résidus", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)

# Résidus vs indices
plot(1:length(residus), residus, main = "Résidus vs indices",
     xlab = "Indice", ylab = "Résidus", pch = 19, col = "green")
abline(h = 0, col = "red", lty = 2)

# Histogramme des résidus
hist(residus, breaks = 15, col = "gray", main = "Histogramme des résidus", xlab = "Résidus")

# QQ-Plot des résidus
qqnorm(residus, main = "Q-Q Plot des résidus")
qqline(residus, col = "red")
