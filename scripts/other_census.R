rm(list = ls())

library(ggplot2)
library(LianaRemoval)
library(dplyr)

dbh_minL <- 3.
csspssfile_name <- "Gigante_newPFT.lat9.000lon-79.000"
csspssfile_name_pre <- "Gigante_newPFT"
csspssfile_name_post <- ".lat9.000lon-79.000"

# Trees
file <- "~/Downloads/Tree_data_liana_removal_experiment.txt"
data <- read.table(file,header = TRUE)

pft.brks    = c(-Inf,c((0.53+0.71)/2,(0.71+0.9)/2),Inf) # PFT 2,3,4
# pft.brks    = c(-Inf,0,1,Inf) # Only PFT = 3
# pft.cut     = as.numeric(cut(data$WD,pft.brks))

data_select <- data %>% filter(!is.na(DBH_2011))%>% mutate(dbh = DBH_2011/10) %>% dplyr::select(Parcela,WD,dbh,Treatment)
data_control <- data_select %>% filter(Treatment == "C") %>% mutate(patch = case_when(Parcela == 1 ~ 1,
                                                         Parcela == 4 ~ 2,
                                                         Parcela == 6 ~ 3,
                                                         Parcela == 8 ~ 4,
                                                         Parcela == 10 ~ 5,
                                                         Parcela == 12 ~ 6,
                                                         Parcela == 13 ~ 7,
                                                         Parcela == 16 ~ 8)) %>% dplyr::select(dbh,patch,WD)

data_removal <- data_select %>% filter(Treatment == "R") %>% mutate(patch = case_when(Parcela == 2 ~ 9,
                                                         Parcela == 3 ~ 10,
                                                         Parcela == 5 ~ 11,
                                                         Parcela == 7 ~ 12,
                                                         Parcela == 9 ~ 13,
                                                         Parcela == 11 ~ 14,
                                                         Parcela == 14 ~ 15,
                                                         Parcela == 15 ~ 16)) %>% dplyr::select(dbh,patch,WD)


# href <- 61.7;b1Ht <- 0.11;b2Ht <- 2.5;hmax <- 35;

Tree_control <- data_control %>% mutate(time = 2000,pft = 1+as.numeric(cut(WD,pft.brks)),
                                       cohort = 1:(nrow(data_control)),
                                       hite = pmin(35, 61.7 *
                                                     (1 - exp(-0.035 * (dbh ** 0.69)))),
                                       n = 1 / (60*60),
                                       bdead =  0,
                                       balive = 0,
                                       lai = 0) %>% dplyr::select(-c(WD))

Tree_removal <- data_removal %>% mutate(time = 2000,pft = 1+as.numeric(cut(WD,pft.brks)),
                                       cohort = 1:(nrow(data_removal)),
                                       hite = pmin(35, 61.7 *
                                                     (1 - exp(-0.035 * (dbh ** 0.69)))),
                                       n = 1 / (60*60),
                                       bdead =  0,
                                       balive = 0,
                                       lai = 0) %>% dplyr::select(-c(WD))



# Lianas
Lianas <- Gigante_control %>% mutate(patch = case_when(patch == 10 ~ 2,
                                                       patch == 12 ~ 3,
                                                       patch == 13 ~ 5,
                                                       patch == 16 ~ 7,
                                                       TRUE ~ patch)) %>% filter(pft == 17)

# href <- 61.7;b1Ht <- 0.11;b2Ht <- 0.87;hmax <- 35;
href <- 61.7;b1Ht <- 0.11;b2Ht <- 2.5;hmax <- 35;

Control_patch <- bind_rows(list(Tree_control,
                                Lianas)) %>% group_by(patch) %>% mutate(hite = pmin(35,61.7*(1 -exp(-0.035*(dbh**0.69))))) %>%
  mutate(hite = case_when(pft == 17 & dbh > dbh_minL ~ pmin(hmax,0.5 + max(hite[pft!=17])),
                          pft == 17 ~ pmin(0.5 + max(hite[pft!=17]),pmin(hmax, href*(1 -exp(-b1Ht*(dbh**b2Ht))))),
                          TRUE ~ hite))


Removal_patch <- Tree_removal

# repacle
# patch6 <- Control_patch %>% filter(patch == 6) %>% ungroup()
# Control_patch <- bind_rows(list(Control_patch %>% filter(!(patch %in% c(4))),
#                                 patch6 %>% mutate(patch = 4))) %>% arrange(patch)

Gigante_all <- bind_rows(list(Control_patch,Removal_patch))
Gigante_all <- Gigante_all %>% ungroup() %>% mutate(cohort = 1:nrow(Gigante_all)) %>% dplyr::select(
  time,patch,cohort,dbh,hite,pft,n,bdead,balive,lai) %>% mutate(n = 1/(60*60))

ggplot(data = Gigante_all %>% mutate(GF = case_when(pft == 17 ~ "Liana",
                                                    TRUE ~ "Tree")),
       aes(x = as.factor(patch),y = hite)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = as.factor(pft),size=GF)) +
  scale_color_manual(values = c("#9FFF8C","#44CC29","#137300","#1E64C8"),
                     labels = c('Early','Mid','Late','Liana')) +
  scale_size_manual(values = c(0.01,0.2)) + guides(size = FALSE) +
  labs(x = "", y = "Height [m]",color = "PFT") +
  theme_bw()

Gigante_all %>% filter(pft != 17) %>% group_by(patch) %>% summarise(h = max(hite))

write.table(x = Gigante_all,
            file = file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".css")),row.names = FALSE,col.names = TRUE)

## Arrange Patch

old.SOC <- sum(c(0.1495,6.126,4.546))
f.FSC <- 0.1495/old.SOC; f.SSC <- 4.546/old.SOC; f.StSC <- 6.126/old.SOC;
TotSOC <- 5.7414
liana.FSC <- 0.123 + 0.014 + 0.122
liana.StSC <- 1.78 + 0.77  # 1.448/0.7

patch_file <- "/home/femeunier/Documents/R/ED2_Support_Files/pss+css_processing/sites/Gigante/Gigante_all.lat9.000lon-79.000.pss"
Gigante_patch <- read.table(patch_file,header = TRUE) %>% filter(patch < 17) %>% mutate(area = 1/(length(area)/2),
                                                                                        fsc = case_when(patch < 9 ~ f.FSC*TotSOC,
                                                                                                        patch > 8 ~ f.FSC*TotSOC + liana.FSC),
                                                                                        stsc = case_when(patch < 9 ~ f.StSC*TotSOC,
                                                                                                         patch > 8 ~ f.StSC*TotSOC + liana.StSC),
                                                                                        stsl = stsc,
                                                                                        ssc = case_when(patch < 9 ~ f.SSC*TotSOC,
                                                                                                         patch > 8 ~ f.SSC*TotSOC))

write.table(x = Gigante_patch,
            file = file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".pss")),row.names = FALSE,col.names = TRUE)


system2("scp",c(file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".css")),"hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/inputs/"))
system2("scp",c(file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".pss")),"hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/inputs/"))


# Control plots only
write.table(x = Gigante_all %>% filter(patch %in% seq(1,8)),
            file = file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name_pre,
                                                                              "_control",
                                                                              csspssfile_name_post,".css")),row.names = FALSE,col.names = TRUE)

write.table(x = Gigante_patch %>% filter(patch %in% seq(1,8)),
            file = file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name_pre,
                                                                              "_control",
                                                                              csspssfile_name_post,".pss")),row.names = FALSE,col.names = TRUE)

system2("scp",c(file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name_pre,
                                                                           "_control",
                                                                           csspssfile_name_post,".css")),"hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/inputs/"))
system2("scp",c(file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name_pre,
                                                                           "_control",
                                                                           csspssfile_name_post,".pss")),"hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/inputs/"))


# Removal plots only
write.table(x = Gigante_all %>% filter(patch %in% seq(9,16)),
            file = file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name_pre,
                                                                              "_removal",
                                                                              csspssfile_name_post,".css")),row.names = FALSE,col.names = TRUE)

write.table(x = Gigante_patch %>% filter(patch %in% seq(9,16)),
            file = file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name_pre,
                                                                              "_removal",
                                                                              csspssfile_name_post,".pss")),row.names = FALSE,col.names = TRUE)

system2("scp",c(file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name_pre,
                                                                           "_removal",
                                                                           csspssfile_name_post,".css")),"hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/inputs/"))
system2("scp",c(file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name_pre,
                                                                           "_removal",
                                                                           csspssfile_name_post,".pss")),"hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/inputs/"))



#######################################################################

plot(data$DBH_2011,data$AGB_2011,log = 'xy')


data %>% group_by(Treatment,Parcela) %>% summarise(AGB = sum(AGB_2011,na.rm=TRUE)/(60*60)/2.0988)

ggplot(data = data) +
  geom_point(aes(x = DBH_2011/10,y = AGB_2011/2)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

LM <- lm(data = data,
         formula = log(AGB_2011) ~ log(DBH_2011/10))

coefs <- coef(LM)
exp(coefs[1])
