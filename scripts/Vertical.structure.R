rm(list = ls())

library(dplyr)
library(rhdf5)

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")

h5file_control <- "./outputs/removal-2014-02.h5"
h5file_control <- "./outputs/control-2010-12.h5"
mymont    = lapply(h5read_opt(h5file_control),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

DBH <- mymont$DBH
PFT <- mymont$PFT
HITE <- mymont$HITE
NPATCHES <- mymont$NPATCHES.GLOBAL
PATCH <- rep(1:(NPATCHES),mymont$PACO.N)
LAI <- mymont$LAI.CO

patch.struct <- data.frame(dbh = DBH,pft = PFT,h = HITE,pa = PATCH,lai = LAI)
patch.struct.pa <- patch.struct %>% filter(pa < 9) %>% group_by(pa) %>% mutate(cumLAI = cumsum(lai),
                                                                               relH = h/max(h))

ggplot(data = patch.struct.pa,
       aes(x = cumLAI,y = relH, color = as.factor(pft))) +
  geom_point() +
  theme_bw()
