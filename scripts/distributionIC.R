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
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_removal_bareground-Q-2000-01-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_removal_bareground-Q-2005-01-00-000000-g01.h5"
h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_PFTs_bareground-Q-2015-01-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_removal_bareground-Q-2025-01-00-000000-g01.h5"
# h5file <- "/home/femeunier/Documents/projects/LianaRemoval/outputs/Gigante_removal-Q-2014-12-00-000000-g01.h5"
pIC <- FALSE

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

df <-
  data.frame(
    pft = PFT,
    dbh = DBH,
    paid = PACOID,
    pa_area = PA_area,
    lai = LAI,
    nplant = NPLANT
  ) %>% rename(DBH = dbh,PFT = pft) %>% #filter((pft == 17 & dbh < 0.1 & nplant > 1))
  mutate(
    DBH_class = case_when(
      PFT == 17 & DBH <= 0.1 ~ 0,
      PFT == 17 & DBH < 2 ~ 1,
      PFT == 17 & DBH < 4 ~ 2,
      PFT == 17 & DBH < 6 ~ 3,
      PFT == 17 & DBH < 8 ~ 4,
      PFT == 17 & DBH < 10 ~ 5,
      PFT == 17 & DBH < 12 ~ 6,
      PFT == 17 & DBH < 14 ~ 7,
      PFT == 17 & DBH < 16 ~ 8,
      PFT == 17 & DBH < 18 ~ 9,
      PFT == 17 & DBH >= 18 ~ 10,
      PFT != 17 & DBH < 1 ~ 0,
      PFT != 17 & DBH < 10 ~ 1,
      PFT != 17 & DBH < 20 ~ 2,
      PFT != 17 & DBH < 30 ~ 3,
      PFT != 17 & DBH < 40 ~ 4,
      PFT != 17 & DBH < 50 ~ 5,
      PFT != 17 & DBH < 60 ~ 6,
      PFT != 17 & DBH < 70 ~ 7,
      PFT != 17 & DBH < 80 ~ 8,
      PFT != 17 & DBH < 90 ~ 9,
      PFT != 17 & DBH < 100 ~ 10,
      PFT != 17 & DBH >= 100 ~ 11
    ),
    BA = (pi*DBH**2)/4
  )

df_sum <- df %>% filter(DBH_class > 0) %>% mutate(is_liana = (PFT == 17)) %>%
  group_by(is_liana, DBH_class) %>% summarise(n = sum(nplant*pa_area)*10000,
                                              BA = sum(BA*nplant*pa_area),
                                              LAI = sum(lai*pa_area),
                                              dbh_m = mean(DBH)) %>% mutate(PFT = case_when(is_liana ~ 17,
                                                                                            !is_liana ~ 3))
# Only if prescribed initiation conditions!
if (pIC){
  df_sum <- df_sum %>% mutate(n = case_when(is_liana ~ n*2,
                                            !is_liana ~ n),
                              LAI = case_when(is_liana ~ LAI*2,
                                              !is_liana ~ LAI))
}


df_sum %>% group_by(is_liana) %>% summarise(N = sum(n),
                                            LAI = sum(LAI))


csspssfile_name <- "Gigante_DBH.lat9.000lon-79.000"
census.file <- file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".css"))
census.data <- read.table(census.file,header = TRUE) %>% mutate(lai =
                                                                  case_when(pft == 17 ~ 0.045*(dbh**1.89),
                                                                            TRUE ~ 0.019*(dbh**1.85))*n*20)

# ggplot(data = census.data)+
#   geom_point(aes(x= dbh,y = lai,color = as.factor(pft))) +
#   scale_y_log10()+
#   scale_x_log10()+
#   theme_bw()

census <- census.data %>% rename(DBH = dbh,PFT = pft) %>%
  mutate(
    DBH_class = case_when(
      PFT == 17 & DBH <=1 ~ 0,
      PFT == 17 & DBH < 2 ~ 1,
      PFT == 17 & DBH < 4 ~ 2,
      PFT == 17 & DBH < 6 ~ 3,
      PFT == 17 & DBH < 8 ~ 4,
      PFT == 17 & DBH < 10 ~ 5,
      PFT == 17 & DBH < 12 ~ 6,
      PFT == 17 & DBH < 14 ~ 7,
      PFT == 17 & DBH < 16 ~ 8,
      PFT == 17 & DBH < 18 ~ 9,
      PFT == 17 & DBH >= 18 ~ 10,
      DBH < 1 ~ 0,
      DBH < 10 ~ 1,
      DBH < 20 ~ 2,
      DBH < 30 ~ 3,
      DBH < 40 ~ 4,
      DBH < 50 ~ 5,
      DBH < 60 ~ 6,
      DBH < 70 ~ 7,
      DBH < 80 ~ 8,
      DBH < 90 ~ 9,
      DBH < 100 ~ 10,
      DBH >= 100 ~ 11
    ),
    BA = (pi*DBH**2)/4
  )

census_sum <- census %>% filter(!(DBH_class %in% c(-1))) %>% group_by(patch,PFT,DBH_class) %>% summarise(N = sum(n)*10000,
                                                                                                        LAI = sum(lai),
                                                                                                        BA = sum(BA*n)) %>%
  group_by(DBH_class,PFT) %>% summarise(n = mean(N),
                                        SD = sd(N),
                                        LAI = mean(LAI),
                                        SD_LAI = sd(LAI),
                                        BA = mean(BA),
                                        SD_BA = sd(BA)) %>% mutate(is_liana = (PFT == 17))

census_sum %>% group_by(PFT) %>% summarise(n = sum(n),
                                           LAI = sum(LAI))

data.and.simus <-
  bind_rows(list(
    census_sum %>% mutate(source = "data"),
    df_sum %>% mutate(source = "simu")
  )) %>% mutate(
    source2 = case_when(
      source == "data" & PFT == 3 ~ "data_tree",
      source == "data" & PFT == 17 ~ "data_liana",
      source == "simu" & PFT == 3 ~ "simu_tree",
      source == "simu" & PFT == 17 ~ "simu_liana",
      TRUE ~ source
    )
  )

ggplot(data = data.and.simus %>% filter(DBH_class > 1)) +
  geom_bar(aes(x = as.factor(DBH_class),y = n,fill = source2),stat = "identity",position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("black","black","darkblue","darkgreen")) +
  # scale_y_log10(limits = c(1,10010),breaks = c(2,101,10001),labels = c(1,100,10000),expand = c(0,0)) +
  facet_wrap(.~ is_liana,scales = "free") +
  labs(x="",y="") +
  theme_bw()

# ggplot(data = data.and.simus %>% filter(DBH_class >0)) +
#   geom_bar(aes(x = as.factor(DBH_class),y = BA,fill = source2),stat = "identity",position = position_dodge2(preserve = "single")) +
#   scale_fill_manual(values = c("black","black","darkblue","darkgreen")) +
#   # scale_y_log10(limits = c(1,10000)) +
#   facet_wrap(.~ is_liana,scales = "free") +
#   theme_bw()
#
# ggplot(data = df_sum %>% filter(DBH_class > 0)) +
#   geom_bar(aes(x = as.factor(DBH_class),y = LAI,fill = as.factor(is_liana)),stat = "identity") +
#   scale_fill_manual(values = c("darkgreen","darkblue")) +
#   facet_wrap(.~ is_liana,scales = "free") +
#   theme_bw()
