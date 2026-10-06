# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# SAE 2.03 - Régression sur données réelles
# Thématique: Etude de données issues d'un radar de golf
# Auteurs: MOUELE Drecy | KINSIKLOUNON Ariane | BENNANI Selma | Moulidia Aura Nadira
# Date de création: 02/03/2025
# Source: Radar_Golf.csv 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#                   Chargement et préparation des données
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #


# --------------------------------------------------------------------------------- #
## --- Chargement des données ---
readLines(con = "../Data/Radar_Golf.csv",
          n = 10)

# Importation des données
dataset = read.table(file = "../Data/Radar_Golf.csv", 
                     sep = "\t", 
                     header = TRUE)

# Visualistaion d'un extrait des données 
head(x = dataset, 
     n = 10) 

# selection des variables d'interet 
subset(dataset,
       select = c("Club","Vitesse_Club","Angle_Attaque",
                  "Chemin_Club_Cible","Face_Club_Cible",
                  "Smash_Factor","Angle_Décollage",
                  "Direction_Tir","Backspin", 
                  "Sidespin","Distance_Carry",
                  "Déviation_Carry")) -> workdataset
subset(workdataset,
       subset = Club == "Pitching Wedge") -> workdataset

# le prof n'as pas dit clairement de transformer club
# en facteur mais c'est dans la sortie dans le descriptive 
within(workdataset,{
  Club = as.factor(Club)}) -> workdataset             

str(workdataset)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#       Analyse statistique des facteurs en lien avec la distance au carry
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# --------------------------------------------------------------------------------- #

# Question 1 :
# ============================================================ #
# on fait une correlation entre chaque variabe avec cor(workdataset[, -1] en excluant la premere var club (-1) car elle est de type factor 
# on enleve les NA avec use = "complete.obs") puis on calcule  l’intensite de la correlation entre Distance_Carry et le reste des variables
correlation_full <- cor(workdataset[, -1], use = "complete.obs")[, "Distance_Carry"]

ordre <- c("Vitesse_Club", "Angle_Attaque", "Chemin_Club_Cible",
           "Face_Club_Cible", "Smash_Factor", "Angle_Décollage",
           "Direction_Tir", "Backspin", "Sidespin")


vals   <- as.numeric(correlation_full[ordre])
print(vals)
vals_pourcent <- vals * 100
print(vals_pourcent)
top3   <- order(abs(vals_pourcent), decreasing = TRUE)[1:3]
colors <- ifelse(seq_along(vals_pourcent) %in% top3, "red3", "white")

par(mar = c(4, 16, 5, 10))
barplot(vals_pourcent, horiz = TRUE, las = 1, col = colors, border = "black",
        xlim = c(-50, 50), names.arg = ordre,
        xlab = "Pourcentage",
        main = "Corrélation linéaire de Pearson avec la distance carry",
        axes = FALSE)

axis(1, at = c(-50, 0, 50))

# Question 2 :
# ============================================================ #
#Distance_carry verus Backspin

with(workdataset, {
  plot(Distance_Carry  ~ Backspin,
       pch = 16,
       col = "blue",
       bg = "blue",
       las = 1,
       cex = 0.8,
       main = "Distance_carry verus Backspin",
       xlab = "Backspin ",
       ylab = "Distance_Carry",
       xlim = c(6500,8500),
       ylim = c(102,114))
  
  sm <- lowess(Backspin,Distance_Carry,f = 0.5)
  
  lines(sm,
         col = "red",
         lwd = 3)
})

#Distance_carry verus Smash_Factor

with(workdataset, {
  plot(Distance_Carry ~ Smash_Factor,
       pch = 16,
       col = "blue",
       bg = "blue",
       las = 1,
       cex = 0.8,
       main = "Distance_carry verus Smash_Factor",
       xlab = "Smash_Factor ",
       ylab = "Distance_Carry",
       xlim = c(1.22,1.30),
       ylim = c(102,114))
  
  sm <- lowess(Smash_Factor,Distance_Carry,f = 0.5)
  
  lines(sm,
        col = "red",
        lwd = 3)
})

#Distance_carry verus Chemin_Club_Cible

with(workdataset, {
  plot(Distance_Carry ~ Chemin_Club_Cible,
       pch = 16,
       col = "blue",
       bg = "blue",
       las = 1,
       cex = 0.8,
       main = "Distance_carry verus Chemin_Club_Cible",
       xlab = "Chemin_Club_Cible ",
       ylab = "Distance_Carry",
       xlim = c(3,7),
       ylim = c(102,114))
  
  sm <- lowess(Chemin_Club_Cible,Distance_Carry,f = 0.5)
  
  lines(sm,
        col = "red",
        lwd = 3)
})

# Question 3 :
# ============================================================ #
# Backspin

backspin = lm(Distance_Carry ~ Backspin, data=workdataset) 
print(backspin)

summary(backspin)
anova(backspin)

# Smash_Factor

smashfactor = lm(Distance_Carry ~ Smash_Factor, data=workdataset) 
print(smashfactor)

summary(smashfactor)
anova(smashfactor)

# Chemin_Club_Cible

cheminclub = lm(Distance_Carry ~ Chemin_Club_Cible, data=workdataset) 
print(cheminclub)

summary(cheminclub)
anova(cheminclub)

# Question 4 :
# ============================================================ #
# Visualisation de la droite des moindres carres, Backspin

smooth = with(workdataset,
              loess(Distance_Carry ~ Backspin, span = 0.9))
smooth = data.frame(backspin = workdataset$Backspin,
                    smooth = fitted(smooth))
smooth = smooth[order(smooth$backspin, decreasing = FALSE),]

with(workdataset,
     plot(Distance_Carry ~ Backspin,
          pch = 21,
          col = "black",
          bg = "purple",
          las = 1,
          cex = 0.7,
          main = "Distance carry versus Backspin",
          xlab = "Backspin (en tour/min)",
          ylab = "Distance Carry (en mètres)"
     ))
with(smooth,
     lines(smooth ~ backspin,
           col = "red",
           lty = 1,
           lwd = 2))
abline(backspin,
       col = "blue",
       lty = 1,
       lwd = 2)
legend(x = "topright",
       lty = c(1, 1),
       lwd = c(3, 3),
       col = c("red", "blue"),
       legend = c("Régression lissée", "Droite des moindres carrés"),
       bty = "n")

# Visualisation de la droite des moindres carres, Smash_Factor

smooth = with(workdataset,
              loess(Distance_Carry ~ Smash_Factor, span = 0.9))
smooth = data.frame(smash_factor = workdataset$Smash_Factor,
                    smooth = fitted(smooth))
smooth = smooth[order(smooth$smash_factor, decreasing = FALSE),]

with(workdataset,
     plot(Distance_Carry ~ Smash_Factor,
          pch = 21,
          col = "black",
          bg = "purple",
          las = 1,
          cex = 0.7,
          main = "Distance carry versus Smash Factor",
          xlab = "Smash Factor",
          ylab = "Distance Carry (en mètres)"
     ))
with(smooth,
     lines(smooth ~ smash_factor,
           col = "red",
           lty = 1,
           lwd = 2))
abline(smashfactor,
       col = "blue",
       lty = 1,
       lwd = 2)
legend(x = "topleft",
       lty = c(1, 1),
       lwd = c(3, 3),
       col = c("red", "blue"),
       legend = c("Régression lissée", "Droite des moindres carrés"),
       bty = "n")

# Visualisation de la droite des moindres carres, Chemin_Club_Cible

smooth = with(workdataset,
              loess(Distance_Carry ~ Chemin_Club_Cible, span = 0.9))
smooth = data.frame(chemin_club = workdataset$Chemin_Club_Cible,
                    smooth = fitted(smooth))
smooth = smooth[order(smooth$chemin_club, decreasing = FALSE),]

with(workdataset,
     plot(Distance_Carry ~ Chemin_Club_Cible,
          pch = 21,
          col = "black",
          bg = "purple",
          las = 1,
          cex = 0.7,
          main = "Distance carry versus Chemin de Club",
          xlab = "Chemin de club (en degrés)",
          ylab = "Distance Carry (en mètres)"
     ))
with(smooth,
     lines(smooth ~ chemin_club,
           col = "red",
           lty = 1,
           lwd = 2))
abline(cheminclub,
       col = "blue",
       lty = 1,
       lwd = 2)
legend(x = "topright",
       lty = c(1, 1),
       lwd = c(3, 3),
       col = c("red", "blue"),
       legend = c("Régression lissée", "Droite des moindres carrés"),
       bty = "n")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#       Analyse statistique des facteurs en lien avec la deviation carry
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# --------------------------------------------------------------------------------- #

#Question 1 :

correlation <- cor(workdataset[, -1], use = "complete.obs")[, "Déviation_Carry"]

ordre_2 <- c("Vitesse_Club", "Angle_Attaque", "Chemin_Club_Cible",
           "Face_Club_Cible", "Smash_Factor", "Angle_Décollage",
           "Direction_Tir", "Backspin", "Sidespin")


vals_2 <- as.numeric(correlation[ordre_2])
print(vals_2)
vals_3 <- vals_2 * 100
top3_2   <- order(abs(vals_3), decreasing = TRUE)[1:3]
colors_2 <- ifelse(seq_along(vals_3) %in% top3_2, "red3", "white")

par(mar = c(4, 10, 4, 5))
barplot(vals_3, 
        horiz = TRUE, 
        las = 1, 
        col = colors_2,
        border = "black",
        xlim = c(-100, 100), 
        names.arg = ordre_2,
        xlab = "Pourcentage",
        main = "Corrélation linéaire de Pearson avec la déviation carry",
        axes = FALSE)

axis(1, at = c(-100, -50, 0, 50, 100))

# Question 2 :

#Déviation_Carry versus Sidespin # PAS OK revoire la courbe de regresssion et l'intervalle de l'axe des y
par(mar = c(4, 9, 4, 5))
plot(workdataset$Déviation_Carry ~ workdataset$Sidespin,
     pch = 16,
     col = "blue",
     las = 1,
     cex = 0.8,
     main = "Déviation carry versus Sidespin",
     xlab = "Sidespin (en tours/min)",
     ylab = "Déviation carry (en mètres)",
     xlim = c(0,800),
     ylim = c(-6,9))

sm <- lowess(workdataset$Sidespin, workdataset$Déviation_Carry, f = 0.8)

lines(sm$x, sm$y,
      col = "red",
      lwd = 2)

#Déviation_Carry versus Face_Club_Cible # OK (revoire l'intervalle de l'axe des y)
par(mar = c(4, 9, 4, 5))
plot(workdataset$Déviation_Carry ~ workdataset$Face_Club_Cible,
     pch = 16,
     col = "blue",
     las = 1,
     cex = 0.8,
     main = "Déviation_Carry versus Face_Club_Cible",
     xlab = "Face_Club_Cible (en degrés)",
     ylab = "Déviation_Carry (en mètres)",
     xlim = c(-2,4.5),
     ylim = c(-6,9))

sm <- lowess(workdataset$Face_Club_Cible,workdataset$Déviation_Carry,f = 0.6)

lines(sm$x, sm$y,
      col = "red",
      lwd = 2)


#Déviation_Carry versus Direction_Tir # OK (revoire l'intervalle de l'axe des y)
par(mar = c(4, 9, 4, 5))
plot(workdataset$Déviation_Carry  ~ workdataset$Direction_Tir,
     pch = 16,
     col = "blue",
     las = 1,
     cex = 0.8,
     main = "Déviation_Carry versus Direction_Tir",
     xlab = "Direction_Tir (en degrés)",
     ylab = "Déviation_Carry (en mètres)",
     xlim = c(-1,4.5),
     ylim = c(-6,9))

sm <- lowess(workdataset$Direction_Tir,workdataset$Déviation_Carry,f = 0.5)

lines(sm$x, sm$y,
      col = "red",
      lwd = 2)

# ============================================================ #
# 2é type de code pour la question 1 et 2 
'''
# -- Corrélation linéaire 

# Valeurs numériques

workdataset_num <- workdataset[, sapply(X = workdataset, 
                                        FUN = is.numeric)]

workdataset_num[["Déviation_Carry"]] <- NULL

# Coefficients de cor. Pearson

Pearsons <- 100*cor(x = workdataset[["Déviation_Carry"]], 
                    y = workdataset_num, 
                    method = "pearson")

Pearsons <- as.vector(x = Pearsons)

names(x = Pearsons) <- names(x = workdataset_num)

par(mar = c(5, 10, 4, 2))

# Coefficients en valeurs absolues

valeurs_abs <- abs(x = Pearsons)

top3 <- sort(x = valeurs_abs, 
             decreasing = TRUE)[3]

couleurs <- ifelse(test = valeurs_abs >= top3, 
                   yes = "#CC0000", 
                   no = "white")

# Diagramme barplot

barplot(height = Pearsons,
        horiz = TRUE,
        las = 1,
        border = "black",
        col = couleurs,
        cex.names = 0.8,
        main = "Corrélation linéaire de Pearson avec la déviation Carry",
        xlab = "Coefficient de corrélation linéaire (%)",
        xlim = c(-100, 100))

abline(v = 0, lty = 2)

par(mar = c(5, 4, 4, 2))


# -- Nuage de dispersion & Régression lissée

layout(matrix(c(1, 1,
                2, 3), 
              nrow = 2, 
              byrow = TRUE))

par(mar = c(4, 4, 3, 1))

graph = function(varx, xlab = NULL){
  
  Loess = loess(workdataset[["Déviation_Carry"]] ~ workdataset[[varx]])
  
  temp = data.frame(workdataset[, c("Déviation_Carry", varx)],
                    Loess = fitted(Loess))
  
  temp = temp[order(temp[[varx]]),]
  
  plot(workdataset[["Déviation_Carry"]] ~ workdataset[[varx]],
       pch = 21,
       col = "black",
       bg = "blue",
       main = paste("Déviation_Carry versus ",
                    varx,
                    sep = ""),
       xlab = xlab,
       ylab = "Déviation_Carry (en mètres)")
  
  lines(temp$Loess ~ temp[[varx]],
        col = "red",
        lwd = 2)
}

par(mfrow = c(2,2))

graph("Sidespin", xlab = "Sidespin (en tour/min)")
graph("Face_Club_Cible", xlab = "Face de Club à la Cible (en degrés)")
graph("Direction_Tir", xlab = "Direction de Tir (en degrés")

par(mfrow = c(1,1))'''
# ============================================================ #


# Question 3: 
# ============================================================ #
# ---- Modélisation ----

# Modèle 1 : Déviation_Carry vs Sidespin    

model1 = lm(formula = Déviation_Carry ~ Sidespin,
            data = workdataset)

summary(object = model1)

anova(object = model1)

# Modèle 2 : Déviation_Carry vs Face_Club_Cible 

model2 = lm(formula = Déviation_Carry ~ Face_Club_Cible,
            data = workdataset)

summary(object = model2)

anova(object = model2)

# Modèle 3 : Déviation_Carry vs Direction_Tir          

model3 = lm(formula = Déviation_Carry ~ Direction_Tir,
            data = workdataset)

summary(object = model3)

anova(object = model3)

# question 4 :
# ============================================================ #
# Nuage de dispersion - régression lissée - droite des moindres carrés 

layout(matrix(c(1, 1,
                2, 3), 
              nrow = 2, 
              byrow = TRUE))

par(mar = c(4, 4, 3, 1))

graph2 = function(varx, xlab = NULL, a = NULL, x = x){
  
  Loess = loess(workdataset[["Déviation_Carry"]] ~ workdataset[[varx]])
  
  temp = data.frame(workdataset[, c("Déviation_Carry", varx)],
                    Loess = fitted(Loess))
  
  temp = temp[order(temp[[varx]]),]
  
  plot(workdataset[["Déviation_Carry"]] ~ workdataset[[varx]],
       pch = 21,
       col = "black",
       bg = "blue",
       main = paste("Déviation_Carry versus ",
                    varx,
                    sep = ""),
       xlab = xlab,
       ylab = "Déviation_Carry (en mètres)")
  
  lines(temp$Loess ~ temp[[varx]],
        col = "red",
        lwd = 2)
  
  abline(a = a,
         col = "purple",
         lty = 1,
         lwd = 3)
  
  legend(x = x,
         lty = c(2,1),
         lwd = c(3,3),
         col = c("red", "purple"),
         legend = c("Régression lissée", "Droite des moindres carrés"),
         bty = "n")
}

par(mfrow = c(2,2))

graph2("Sidespin", xlab = "Sidespin (en tour/min)", a = model1, x = "topright")
graph2("Face_Club_Cible", xlab = "Face de Club à la Cible (en degrés)", a = model2, x = "topleft")
graph2("Direction_Tir", xlab = "Direction de Tir (en degrés", a = model3, x = "topleft")

par(mfrow = c(1,1))

# ============================================================ #
# ---- Comparaison des modèles ----

# AUtomatisation

models = list(Model1 = model1,
              Model2 = model2,
              Model3 = model3)

lapply(X = models,
       FUN = summary) -> extractmodels

print(extractmodels)

# Extraction des R^2

sapply(X = seq_along(extractmodels),
       FUN = function(x) round(100*extractmodels[[x]]$r.squared, 2)) -> R.Squared

setNames(object = R.Squared,
         nm = paste0("model", 1:3)) -> R.Squared

print(x = R.Squared)

