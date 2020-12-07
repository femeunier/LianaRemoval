rm(list = ls())

library(rhdf5)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(stringr)

source("~/Documents/ED2/R-utils/h5read_opt.r")

# h5file <- "/home/femeunier/Documents/ED2/ED/run/histo/Gigante_removal-S-2004-01-01-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_PFTs_bareground-Q-2000-01-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_PFTs_bareground-Q-1960-01-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_removal_bareground-Q-1952-01-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_PFTs_bareground-Q-2004-12-00-000000-g01.h5"
h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_removal-Q-2014-12-00-000000-g01.h5"

mymont    = lapply(h5read_opt(h5file),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

PACO <- mymont$PACO.N
PFT <- mymont$PFT
Hite <- mymont$HITE
DBH <- mymont$DBH
patch_num <- length(PACO)
PACOID <- rep(1:patch_num,PACO)
PA_area <- mymont$AREA[PACOID]
NPLANT <- mymont$NPLANT
LAI <- mymont$LAI.CO
Bleaf <- mymont$BLEAF

df <-
  data.frame(
    pft = PFT,
    dbh = DBH,
    paid = PACOID,
    pa_area = PA_area,
    lai = LAI,
    nplant = NPLANT,
    Bleaf
  )

params <- data.frame(pft = c(2,3,4,17),SLA = c(16,11.64,9.66,12))

ggplot(data = df) +
  geom_point(aes(x= dbh,y = Bleaf,color = as.factor(pft))) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw()
