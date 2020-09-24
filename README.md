# AnalyseDonneesMassives_ForestFires
# FIRE

rm(list = ls()) #  clean l'environnement global
setwd("C:/Users/mimi/Desktop/M2/Analyse de données massives/projet")
require(data.table)
fire <- fread("fire.csv",
              header = TRUE,
              na.strings = "",
              blank.lines.skip = TRUE
              )
dim(fire)
head(fire)
tables()

fire[1:10,]

fire[, Shape := NULL]
