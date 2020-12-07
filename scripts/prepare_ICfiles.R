rm(list = ls())

library(LianaRemoval)
library(dplyr)
library(ggplot2)

csspssfile_name <- "Gigante_adult_replaced.lat9.000lon-79.000"
dbh_minL <- 3.
## Arrange cohorts
# Control
Gigante_control_mod <- Gigante_control %>% mutate(patch = case_when(patch == 10 ~ 2,
                                                                    patch == 12 ~ 3,
                                                                    patch == 13 ~ 5,
                                                                    patch == 16 ~ 7,
                                                                    TRUE ~ patch))

# href <- 61.7;b1Ht <- 0.11;b2Ht <- 0.87;hmax <- 35;
href <- 61.7;b1Ht <- 0.11;b2Ht <- 2.5;hmax <- 35;

Gigante_control_mod <- Gigante_control_mod %>% group_by(patch) %>% mutate(hite = pmin(35,61.7*(1 -exp(-0.035*(dbh**0.69))))) %>%
  mutate(hite = case_when(pft == 17 & dbh > dbh_minL ~ pmin(35,0.5 + max(hite[pft==3])),
                          pft == 17 ~ pmin(hmax, href*(1 -exp(-b1Ht*(dbh**b2Ht)))),
                          TRUE ~ hite))

# changing hite

Control_sum <- Gigante_control_mod %>% group_by(patch) %>% summarise(n_all = sum(n),
                                                                     n_tree = sum(n[pft != 17]),
                                                                     n_liana = sum(n[pft == 17]),
                                                                     dbh_max = max(dbh),
                                                                     dbh_min = min(dbh),
                                                                     dbh_all = mean(dbh),
                                                                     dbh_liana = mean(dbh[pft == 17])) %>% arrange(patch)

Gigante_removal_mod <- Gigante_removal %>% mutate(patch = case_when(patch == 9 ~ 1,
                                                                    patch == 11 ~ 4,
                                                                    patch == 14 ~ 6,
                                                                    patch == 15 ~ 8,
                                                                    TRUE ~ patch))

Gigante_removal_mod <- Gigante_removal_mod %>% group_by(patch) %>% mutate(hite = pmin(35,61.7*(1 -exp(-0.035*(dbh**0.69))))) %>% ungroup()


Gigante_sum <- Gigante_removal_mod %>% group_by(patch) %>% summarise(n_all = sum(n),
                                                                     dbh_max = max(dbh),
                                                                     dbh_min = min(dbh),
                                                                     dbh_all = mean(dbh)) %>% arrange(patch)

# Gigante_control_mod %>% group_by(patch,pft) %>% summarise(N = sum(n),
#                                                           dbh_m = max(dbh),
#                                                           dbh_M = mean(dbh),
#                                                           Nlarge = sum(n[dbh > 10]),
#                                                           Nsmall = sum(n[dbh < 10])) %>% arrange(pft)

# # Replace
# patch1 <- Gigante_control_mod %>% filter(patch == 1) %>% ungroup()
# patch2 <- Gigante_control_mod %>% filter(patch == 2) %>% ungroup()
patch6 <- Gigante_control_mod %>% filter(patch == 6) %>% ungroup()
Gigante_control_mod_replaced <- bind_rows(list(Gigante_control_mod %>% filter(!(patch %in% c(4))),
                                               patch6 %>% mutate(patch = 4))) %>% arrange(patch)
# Gigante_control_mod <- Gigante_control_mod_replaced


Ncohorts <- nrow(Gigante_control_mod)
Gigante_all <- bind_rows(list(Gigante_control_mod %>% arrange(patch) %>% mutate(cohort = 1:length(cohort)),
                              Gigante_removal_mod %>% arrange(patch) %>% mutate(cohort = Ncohorts + (1:length(cohort)),
                                                                                patch = patch+8)))

# Just adults?
# Gigante_all <- Gigante_all %>% filter(!(pft == 3 & dbh < 10))

# Gigante_all <- Gigante_all %>% mutate(n = case_when(pft == 17 ~ n*2,
#                                                     TRUE ~ n))

# Gigante_all_mod <-
#   Gigante_all %>% mutate(pft = case_when(pft == 3 & dbh <= 10 ~ 4,
#                                          pft == 3 ~ 3,
#                                          pft == 17 ~ 17))


ggplot(data = Gigante_all,aes(x = as.factor(patch),y = hite)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = as.factor(pft)),size=0.1) +
  theme_bw()


write.table(x = Gigante_all,
            file = file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".css")),row.names = FALSE,col.names = TRUE)

## Arrange Patch
FSC <- 0.15
STSC <- 5
#
patch_file <- "/home/femeunier/Documents/R/ED2_Support_Files/pss+css_processing/sites/Gigante/Gigante_all.lat9.000lon-79.000.pss"
Gigante_patch <- read.table(patch_file,header = TRUE) %>% filter(patch < 17) %>% mutate(area = 1/length(area),
                                                                                        fsc = case_when(patch < 9 ~ FSC,
                                                                                                        patch > 8 ~ FSC + 0.07),
                                                                                        stsc = case_when(patch < 9 ~ STSC,
                                                                                                        patch > 8 ~ STSC + 0.8),
                                                                                        stsl = stsc)

write.table(x = Gigante_patch,
            file = file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".pss")),row.names = FALSE,col.names = TRUE)

system2("scp",c(file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".css")),"hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/inputs/"))
system2("scp",c(file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".pss")),"hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/inputs/"))


