rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

control.census <- read.table(file.path("/home/femeunier/Documents/data/gigante/","Gigante_newPFT_control.lat9.000lon-79.000.css"),
                             header = TRUE) %>% mutate(patch_t = "control")

removal.census <- read.table(file.path("/home/femeunier/Documents/data/gigante/","Gigante_newPFT_removal.lat9.000lon-79.000.css"),
                             header = TRUE) %>% mutate(patch_t = "removal")

censuses <-
  bind_rows(list(control.census, removal.census)) %>% mutate(
    dbh_group = case_when(
      pft != 17 & dbh < 10 ~ 0,
      pft != 17 & dbh < 20 ~ 1,
      pft != 17 & dbh < 30 ~ 2,
      pft != 17 & dbh < 40 ~ 3,
      pft != 17 & dbh < 50 ~ 4,
      pft != 17 & dbh < 60 ~ 5,
      pft != 17 & dbh < 70 ~ 6,
      pft != 17 & dbh < 80 ~ 7,
      pft != 17 & dbh >= 80 ~ 8,
      pft == 17 & dbh < 1 ~ 0,
      pft == 17 & dbh < 3 ~ 1,
      pft == 17 & dbh < 5 ~ 2,
      pft == 17 & dbh < 7 ~ 3,
      pft == 17 & dbh < 9 ~ 4,
      pft == 17 & dbh < 11 ~ 5,
      pft == 17 & dbh >= 11 ~ 6
    )
  ) %>% mutate(patch = case_when(patch < 9L ~ patch,
                                 TRUE ~ (patch - 8L))) %>% filter(dbh_group > 0)

census.sum <- censuses %>% group_by(pft,patch_t,patch,dbh_group) %>% summarise(n = sum(n)) %>% ungroup() %>%
  complete(patch_t = c("control","removal"),patch = 1:8,pft = c(2,3,4,17),dbh_group = seq(1,8),fill = list(n = 0)) %>% group_by(pft,patch_t,dbh_group) %>%
  summarise(sd = sd(n, na.rm = TRUE),
            n = mean(n,na.rm = TRUE),
            N = length(dbh_group))

tot.sum <- censuses %>% filter(pft != 17) %>% group_by(patch_t,patch,dbh_group) %>% summarise(n = sum(n)) %>% ungroup() %>%
  complete(patch_t = c("control","removal"),patch = 1:8,dbh_group = seq(1,8),fill = list(n = 0)) %>% group_by(patch_t,dbh_group) %>%
  summarise(sd = sd(n, na.rm = TRUE),
            n = mean(n,na.rm = TRUE),
            N = length(dbh_group),
            se = sd/sqrt(N))

p.val <- censuses %>% filter(pft != 17) %>% group_by(patch_t,patch,dbh_group) %>% summarise(n = sum(n)) %>% ungroup() %>%
  complete(patch_t = c("control","removal"),patch = 1:8,dbh_group = seq(1,8),fill = list(n = 0)) %>% group_by(dbh_group) %>%
  summarise(p.val =  summary(aov(formula = n ~ as.factor(patch_t)))[[1]][1,5])

p.val_pft <- censuses %>% group_by(pft,patch_t,patch,dbh_group) %>% summarise(n = sum(n)) %>% ungroup() %>%
  complete(patch_t = c("control","removal"),patch = 1:8,pft = c(2,3,4,17),dbh_group = seq(1,8),fill = list(n = 0)) %>% group_by(pft,dbh_group) %>%
  summarise(p.val =  summary(aov(formula = n ~ as.factor(patch_t)))[[1]][1,5])

barwidth = 0.4

control.sum <- filter(census.sum, patch_t == "control") %>%
  group_by(dbh_group) %>% arrange(-pft)

removal.sum <- filter(census.sum, patch_t == "removal") %>%
  group_by(dbh_group) %>% arrange(-pft)

ggplot() +
  geom_errorbar(data = tot.sum %>% filter(patch_t == 'control'),
                mapping = aes(x = dbh_group, ymin = 0.9*(n)*10000, ymax = (n + sd)*10000),
                width = 0.2) +
  geom_bar(data = control.sum %>% filter(pft != 17),
           mapping = aes(x = dbh_group, y = n*10000, fill = as.factor(pft)),
           stat="identity",
           position='stack',
           width = barwidth) +
  geom_errorbar(data = tot.sum %>% filter(patch_t == 'removal'),
                mapping = aes(x = dbh_group  + barwidth + 0.01, ymin = 0.9*(n)*10000, ymax = (n + se)*10000),
                width = 0.2) +
  geom_bar(data = removal.sum %>% filter(pft != 17),
           mapping = aes(x = dbh_group  + barwidth + 0.01, y = n*10000, fill = as.factor(pft)),
           stat="identity",
           position='stack',
           width = barwidth) +
  labs(fill = "Plant Functional Type", x = "DBH [cm]", y = "Tree density [ind/ha]") +
  scale_fill_manual(values = c("#9FFF8C","#44CC29","#137300"),
                    labels = c("Early","Mid","Late")) +
  scale_x_continuous(breaks = barwidth/2 + seq(1:length(unique(tot.sum$dbh_group))),
                     labels = c("10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-Inf") ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.85, 0.85),
        text = element_text(size = 24))

ggsave(plot = last_plot(),
      filename = "./Figures/TreePFTcomposition.png",width = 15,height = 10,dpi = 300)


tot.sum.liana <- censuses %>% filter(pft == 17,
                                     patch_t == "control") %>% group_by(patch,dbh_group) %>% summarise(n = sum(n)) %>% ungroup() %>%
  complete(patch = 1:8,dbh_group = seq(1,8),fill = list(n = 0)) %>% group_by(dbh_group) %>%
  summarise(sd = sd(n, na.rm = TRUE),
            n = mean(n,na.rm = TRUE),
            N = length(dbh_group),
            se = sd/sqrt(N))


liana2plot <- tot.sum.liana %>% filter(n > 0)

ggplot() +
  geom_errorbar(data = liana2plot,
                mapping = aes(x = dbh_group, ymin = 0.9*(n)*10000, ymax = (n + sd)*10000),
                width = 0.2) +
  geom_bar(data = control.sum %>% filter(n > 0) %>% filter(pft == 17),
           mapping = aes(x = dbh_group, y = n*10000, fill = as.factor(pft)),
           stat="identity",
           position='stack') +
  labs(fill = "PFT", x = "DBH [cm]", y = "Liana density [stem/ha]") +
  scale_fill_manual(values = c("#1E64C8")) + guides(fill = FALSE) +
  scale_x_continuous(breaks = seq(1:length(unique(liana2plot$dbh_group))),
                     labels = c("1-3","3-5","5-7","7-9","9-11","11-Inf") ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 24))

ggsave(plot = last_plot(),
       filename = "./Figures/LianaPFTcomposition.png",width = 15,height = 10,dpi = 300)
