rm(list = ls())

library(rhdf5)
library(ggplot2)
library(dplyr)
library(tidyr)

source("~/Documents/ED2/R-utils/h5read_opt.r")

system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control-Q-2011-03-00-000000-g01.h5","./outputs/"))

# h5file <- "/home/femeunier/Documents/ED2/ED/run/histo/Gigante_removal-S-2004-01-01-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_removal-Q-2009-12-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_removal-Q-2011-03-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/analysis-Q-2014-01-00-000000-g01.h5"
h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_pft_long_control-Q-2011-03-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_pft-S-2014-02-01-000000-g01.h5"

mymont    = lapply(h5read_opt(h5file),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

PACO <- mymont$PACO.N
PFT <- mymont$PFT
Hite <- mymont$HITE
DBH <- mymont$DBH
patch_num <- length(PACO)
PACOID <- rep(1:patch_num,PACO)
NPLANT <- mymont$NPLANT
LAI <- mymont$LAI.CO

Bleaf <- mymont$BLEAF*NPLANT
# Bdead <- mymont$AGB.CO*NPLANT
Bdead <- mymont$BDEAD*NPLANT*0.7

dbhs = seq(min(DBH[PFT == 17]),max(DBH[PFT == 17]),length.out = 1000)

# plot(DBH[PFT != 17],LAI[PFT!=17]/NPLANT[PFT!=17],col='red',type='p',log='xy')
# lines(DBH[PFT == 17],LAI[PFT==17]/NPLANT[PFT==17],type='p')
# lines(dbhs,0.0957/2*(dbhs**1.89))
# lines(dbhs,0.06/2*(dbhs**1.89),col='red')

dbhs = seq(10,110,length.out = 1000)
plot(dbhs,0.77/2*(dbhs**2.23),log='xy',type='l')
lines(DBH[PFT==3],mymont$BDEAD[PFT==3],type='p')

GPP <- mymont$MMEAN.GPP.CO*NPLANT
LAI <- mymont$LAI.CO
fsc <- mymont$MMEAN.FAST.SOIL.C
ssc <- mymont$MMEAN.SLOW.SOIL.C
structsc <- mymont$MMEAN.STRUCT.SOIL.C

df <- data.frame(pft = PFT,dbh = DBH,hite = Hite,patch = PACOID,nplant = NPLANT,lai = LAI,Bl = Bleaf,Bd = Bdead)

ggplot(data = df %>% filter(patch < 17),
       aes(x = as.factor(patch),y = hite)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = as.factor(pft))) +
  theme_bw()

df_sum <- df %>% group_by(pft,patch) %>% summarise(LAI = sum(lai),
                                                   LAI_top = sum(lai[hite > 0.95*max(hite)]),
                                                   Bd_tot = sum(Bd),
                                                   Bd = sum(Bd[(dbh>10 & pft != 17) | (dbh>1 & pft == 17)]),
                                                   Bl = sum(Bl)) %>% arrange(patch) %>% mutate(patch_t = case_when(patch < 9 ~ "control",
                                                                                                                   TRUE ~ "removal"))


df_sum <- df_sum %>% mutate(LAI_top = case_when(pft == 17 ~ LAI_top,
                                                pft != 17 ~ LAI))

df_sum <- df_sum %>% mutate(GF = case_when(pft <= 4 ~ "Tree",
                                           pft == 17 ~ "Liana"))


# LAI

ggplot(data = df_sum %>% filter(patch < 17) %>% group_by(patch_t,pft) %>% summarise(LAI = mean(LAI)),
       aes(x = as.factor(patch_t),y = LAI,fill = as.factor(pft))) +
  geom_bar(stat = "identity",position = "stack") +
  theme_bw()

ggplot(data = df_sum %>% filter(patch < 17),
       aes(x = as.factor(patch),y = LAI,fill = as.factor(pft))) +
  geom_bar(stat = "identity",position = "stack") +
  theme_bw()

df_sum %>% filter(patch < 17) %>% group_by(patch,GF) %>% summarise(m = sum(LAI),
                                                                   patch_t = patch_t[1]) %>% group_by(patch_t,GF) %>% summarise(m = mean(m))
df_sum %>% filter(patch < 17) %>% group_by(patch) %>% summarise(m = sum(LAI),
                                                                patch_t = patch_t[1]) %>% group_by(patch_t) %>% summarise(m = mean(m))


# Bd
ggplot(data = df_sum %>% filter(patch < 17) %>% group_by(patch_t,pft) %>% summarise(Bd = mean(Bd)),
       aes(x = as.factor(patch_t),y = Bd,fill = as.factor(pft))) +
  geom_bar(stat = "identity",position = "stack") +
  theme_bw()

ggplot(data = df_sum %>% filter(patch < 17),
       aes(x = as.factor(patch),y = Bd,fill = as.factor(pft))) +
  geom_bar(stat = "identity",position = "stack") +
  theme_bw()

df_sum %>% filter(patch < 17) %>% group_by(patch,GF) %>% summarise(m = sum(Bd))
df_sum %>% filter(patch < 17) %>% group_by(patch,GF) %>% summarise(m = sum(Bd),
                                                                   patch_t = patch_t[1]) %>% group_by(patch_t,GF) %>% summarise(m = mean(m))

df_sum %>% filter(patch < 17) %>% group_by(patch,GF) %>% summarise(m = sum(Bd),
                                                                   patch_t = patch_t[1]) %>% group_by(patch_t,GF) %>% summarise(m = mean(m))

df_sum %>% filter(patch < 17) %>% group_by(patch,pft) %>% summarise(m = sum(Bd),
                                                                   patch_t = patch_t[1]) %>% group_by(patch_t,pft) %>% summarise(m = mean(m))



# cbind(df_sum %>% filter(patch < 17,pft == 17) %>% group_by(patch) %>% summarise(m = mean(LAI)),
#       df_sum %>% filter(patch < 17,pft != 17) %>% group_by(patch) %>% summarise(m_T = mean(LAI)))
# df_sum %>% filter(patch < 17) %>% group_by(patch,GF) %>% summarise(m = sum(LAI))



df_height <- df %>% group_by(patch) %>% summarise(hite_liana = ifelse(length(which(pft == 17))>0,pmax(hite[pft == 17]),NA),
                                                  hite_tree = ifelse(length(which(pft != 3))>0,pmax(hite[pft != 3]),NA)) %>%
  pivot_longer(cols = c(hite_liana,hite_tree),
               names_to = "hite",
               values_to = "value")


ggplot(data = df_height) +
  geom_bar(aes(x = patch,y = value,fill = hite),stat = 'identity',position="dodge") +
  theme_bw()

ggplot(data = df,aes(x = as.factor(patch),y = hite)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = as.factor(pft)),size=0.1) +
  theme_bw()

# unique(df %>% filter(patch == 1,pft == 17) %>% pull(dbh))
# 33.465553 28.583555 26.008257 23.450781 20.936256 18.489437 16.134102 13.892498 11.784812  9.828712  8.038947  6.427034
# 7.279052  9.398261  8.197657  5.130009 11.728785  5.712390  6.250290  4.584993 14.525929  8.825488 13.625460 10.174875 11.024674  3.863479
# 6.772930  4.254078  3.504156  3.232697  2.917062  2.667772  2.460533  2.267955  2.116715  2.009325  1.908426  1.805199  1.703032  1.601935
# 1.505657  1.402768  1.302982  1.203354  1.103237  1.003151

# 33.495525 28.603861 26.028427 23.464392 20.944908 18.495422 16.153011 13.902468 11.796346  9.842607  8.053282  6.441935
df_soil <- data.frame(patch = 1:length(fsc),
                      fast.soil.c = fsc,
                      slow.soil.c = ssc,
                      struc.soil.c = structsc) %>% pivot_longer(cols = c(fast.soil.c,slow.soil.c,struc.soil.c),
                                                                names_to = "soil.carbon",
                                                                values_to = "value")

ggplot(data = df_soil %>% filter(patch < 17)) +
  geom_bar(aes(x = patch,y = value,fill = soil.carbon),stat = 'identity',position="stack")+
  theme_bw()


