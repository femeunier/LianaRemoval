rm(list = ls())

library(rhdf5)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(stringr)

source("~/Documents/ED2/R-utils/h5read_opt.r")


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
Bd <- mymont$BDEAD

mymont$MMEAN.NPPLEAF.PY
mymont$MMEAN.NPPWOOD.PY


C2B <- 2
# Tree
dbh_crit <- 96
dbhs <- seq(0.1,150,length.out = 1000)
b1Bs_small <-  0.04#0.2226658911
b1Bs_large <-  0.04#0.40 #0.2297666222
b2Bs_small <-  2.79#2.4323608875
b2Bs_large <-  2.79#2.55 #2.4255735874
Bd_tree <- b1Bs_small / C2B * dbhs ** b2Bs_small
Bd_tree[dbhs >dbh_crit]  <- b1Bs_large / C2B * dbhs[dbhs >dbh_crit] ** b2Bs_large
# Liana
dbh_crit_L<- 11
b1Bs_small_L <-  0.062 #0.2748999894
b1Bs_large_L <-  0.062 #0.45 #0.2718995214
b2Bs_small_L <-  3.01 #2.6937301159
b2Bs_large_L <-  3.01 #2.75 #2.5711865425
Bd_liana <- b1Bs_small_L / C2B * dbhs ** b2Bs_small_L
Bd_liana[dbhs > dbh_crit_L]  <- b1Bs_large_L / C2B * dbhs[dbhs > dbh_crit_L] ** b2Bs_large_L

Bd_df <- data.frame(dbh = c(dbhs,dbhs),Bd = c(Bd_tree,Bd_liana),pft = c(rep(3,length(dbhs)),rep(17,length(dbhs))))

df <-
  data.frame(
    pft = PFT,
    dbh = DBH,
    paid = PACOID,
    pa_area = PA_area,
    lai = LAI,
    nplant = NPLANT
  ) %>% mutate(Bdead = case_when(
    pft == 17 & dbh <= dbh_crit_L ~ b1Bs_small_L / C2B * dbh ** b2Bs_small_L,
    pft == 17 & dbh > dbh_crit_L ~ b1Bs_large_L / C2B * dbh ** b2Bs_large_L,
    pft != 17 & dbh <= dbh_crit ~ b1Bs_small / C2B * dbh ** b2Bs_small,
    pft != 17 & dbh > dbh_crit ~ b1Bs_large / C2B * dbh ** b2Bs_large,
  ),
  patch_t = case_when(paid <= 8 ~ "control",
                      paid >  8 ~ "removal"))

df_sum <- df %>% group_by(pft,patch_t) %>% summarise(AGB = sum(Bdead*nplant)/(length(unique(paid))))


ggplot() +
  geom_point(data = df,aes(x= dbh,y = Bd,color = as.factor(pft))) +
  geom_line(data = Bd_df,aes(x= dbh,y = Bd,color = as.factor(pft))) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw()


head(df_sum)
