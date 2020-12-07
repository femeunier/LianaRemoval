rm(list = ls())

library(stringr)

csspssfile_name <- "Gigante_DBH.lat9.000lon-79.000"
census.file <- file.path("/home/femeunier/Documents/data/gigante/",paste0(csspssfile_name,".css"))
census.data <- read.table(census.file,header = TRUE) %>% mutate(lai =
                                                                  case_when(pft == 17 ~ 0.045*(dbh**1.89),
                                                                            TRUE ~ 0.019*(dbh**1.85))*n*20)
#
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
    )
  )

census_sum <- census %>% filter(!(DBH_class %in% c(0))) %>% group_by(patch,PFT,DBH_class) %>% summarise(N = sum(n)*10000,
                                                                                                   LAI = sum(lai)) %>%
  group_by(DBH_class,PFT) %>% summarise(Ntot = mean(N),
                                    SD = sd(N),
                                    LAI_m = mean(LAI),
                                    SD_LAI = sd(LAI)) %>% mutate(is_liana = (PFT == 17))

census_sum %>% group_by(PFT) %>% summarise(n = sum(Ntot),
                                           LAI = sum(LAI_m))

ggplot(data = census_sum) +
  geom_errorbar(aes(x = as.factor(DBH_class),y = Ntot,ymin = 0.9*Ntot,ymax = Ntot + SD,
                    color = as.factor(is_liana)),width=0.2) +
  geom_bar(aes(x = as.factor(DBH_class),y = Ntot,fill = as.factor(is_liana)),stat = "identity") +
  scale_fill_manual(values = c("darkgreen","darkblue")) +
  scale_y_log10(limits = c(1,5000)) +
  scale_color_manual(values = c("darkgreen","darkblue")) +
  facet_wrap(.~ is_liana,scales = "free") +
  theme_bw()

# ggplot(data = census_sum) +
#   geom_errorbar(aes(x = as.factor(DBH_class),y = LAI_m,ymin = 0.9*LAI_m,ymax = LAI_m + SD_LAI,
#                     color = as.factor(is_liana)),width=0.2) +
#   geom_bar(aes(x = as.factor(DBH_class),y = LAI_m,fill = as.factor(is_liana)),stat = "identity") +
#   scale_fill_manual(values = c("darkgreen","darkblue")) +
#   scale_color_manual(values = c("darkgreen","darkblue")) +
#   facet_wrap(.~ is_liana,scales = "free") +
#   theme_bw()

