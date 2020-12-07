rm(list = ls())

library(rhdf5)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(stringr)

source("~/Documents/ED2/R-utils/h5read_opt.r")

system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft29/analy/analysis-Q-2014-01-00-000000-g01.h5",
                      "./outputs/"))

h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/analysis-Q-2014-01-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_pft-Q-2014-01-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/mod2_4/Gigante_PFTs_bareground-Q-1957-02-00-000000-g01.h5"

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

w.nplant  = NPLANT  * PA_area
NPPleaf = mymont$MMEAN.NPPLEAF.CO
NPPwood = mymont$MMEAN.NPPWOOD.CO
NPPcroot = mymont$MMEAN.NPPCROOT.CO
NPPsap = mymont$MMEAN.NPPSAPWOOD.CO
NPPfroot = mymont$MMEAN.NPPFROOT.CO
NPP = mymont$MMEAN.NPP.CO
Bstorage = mymont$MMEAN.BSTORAGE.CO
Bleaf = mymont$MMEAN.BLEAF.CO
AGB = mymont$AGB.CO
CB = mymont$MMEAN.CB.CO
recruit = mymont$RECRUIT.DBH
AGB_mort <- AGB*apply(mymont$MMEAN.MORT.RATE.CO,1,sum)*NPLANT

table_mort <- data.frame(mort = AGB*apply(mymont$MMEAN.MORT.RATE.CO,1,sum)*NPLANT,PFT,PACOID) %>% mutate(patch_t = case_when(PACOID < 9 ~ "control",
                                                                                                                             TRUE ~ "removal"),
                                                                                                         GF = case_when(PFT == 17 ~ "Liana",
                                                                                                                        PFT != 17 ~ "Tree"))



ggplot(table_mort %>% group_by(patch_t,PFT,PACOID) %>% summarise(S = sum(mort))) +
  geom_boxplot(aes(x = as.factor(PFT),y = S,fill = patch_t)) +
  theme_bw()

ggplot(table_mort %>% group_by(patch_t,GF,PACOID) %>% summarise(S = sum(mort))) +
  geom_boxplot(aes(x = as.factor(GF),y = S,fill = patch_t)) +
  theme_bw()



df <- data.frame(pa = PACOID,
                 NPPleaf,NPPwood,NPPcroot,NPPsap,NPPfroot,
                 NPP,Bstorage,AGB,CB,Bleaf,
                 DBH,PFT,
                 recruit,
                 AGB_mort,
                 n = NPLANT)

df %>% filter(recruit == 1)

df_sum <- df %>% group_by(pa) %>% summarise(NPPleaf = sum(NPPleaf*n),
                                                      NPPcroot = sum(NPPcroot*n),
                                                      NPPsap = sum(NPPsap*n),
                                                      NPPfroot = sum(NPPfroot*n),
                                                      NPPwood = sum(NPPwood*n),
                                                      NPP = sum(NPP*n),
                                                      AGB = sum(AGB*n),
                                                      AGB_mort = sum(AGB_mort),
                                                      Bleaf = sum(Bleaf*n),
                                                      CB = sum(CB*n),
                                                      Bstorage = sum(Bstorage*n)) %>%
  mutate(NPP2 = NPPleaf + NPPcroot + NPPsap + NPPfroot + NPPwood,
         frac = NPPcroot/(NPPcroot + NPPwood),
         frac2 = NPP2/NPP,
         mort = AGB*0.03809596,
         patch_t = case_when(pa < 9 ~ "control",
                             TRUE ~ "removal"))


df_sum %>% group_by(patch_t) %>% summarise(NPPleaf_m = mean(NPPleaf),
                                           SD = sd(NPPleaf),
                                           AGB_mort = mean(AGB_mort))

sum(NPPleaf * w.nplant , na.rm = TRUE)
sum(NPPwood * w.nplant , na.rm = TRUE)
sum(NPP * w.nplant , na.rm = TRUE)

