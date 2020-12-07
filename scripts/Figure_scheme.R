rm(list = ls())

library(dplyr)
library(rhdf5)
library(purrr)
library(tidyr)
library(LianaRemoval)
library(ggplot2)

source("~/Documents/ED2/R-utils/h5read_opt.r")

system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000010611/control-Q-2011-03-00-000000-g01.h5",
                      "./outputs/Initial_control.h5"))
system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000010611/removal-Q-2011-03-00-000000-g01.h5",
                      "./outputs/Initial_removal.h5"))

h5file_control <- "./outputs/Initial_control.h5"
h5file_removal <- "./outputs/Initial_removal.h5"



OP.all2plot <- bind_rows(list(
  Qfile2Cstocks(h5file = h5file_control) %>% mutate(treatment = "Control",
                                                    time = "Initial carbon stocks"),
  Qfile2Cstocks(h5file_removal) %>% mutate(treatment = "Removal",
                                           time = "Initial carbon stocks"))) %>% mutate(time = as.factor(time))


OP.all2plot_m <- OP.all2plot %>% filter(stat == "mean") %>%   filter(!(pft == 18 & type == "biomass")) %>%
  group_by(treatment,time) %>%
  mutate(pos_X = case_when(treatment == "Control" ~ 1,
                           treatment == "Removal" ~ 2),
         pos_Y = case_when(var == "FSC" & time == "End" & treatment == "control" ~ sum(value[var == "FSC"]/2),
                           var == "SSC" ~ sum(value[var == "FSC"] + value[var == "SSC"]/2),
                           var == "StructSC" ~ sum(value[var == "FSC"] + value[var == "SSC"] + value[var == "StructSC"]/2),
                           TRUE ~ 0),
         label = case_when(var %in% c("SSC","StructSC") ~ var,
                           var == "FSC" & time == "End" & treatment == "control" ~ "",
                           TRUE ~ "")) %>%
  mutate(GF = case_when(pft == 18 ~ "Soil",
                        TRUE ~ "Else")) %>% group_by(var,time,GF,treatment) %>% summarise(value = sum(value),
                                                                                          label = label[1])


AGB <- subset(OP.all2plot_m,var == "agb")
BGB <- subset(OP.all2plot_m,var != "agb")

BGB$var <- factor(BGB$var,levels = c("bgb","FSC","SSC","StructSC"))

ggplot() +
  geom_bar(data = AGB,
           aes(x = as.factor(treatment),y = value, fill = as.factor(GF)),
           color = "black",alpha = 0.4,width = 0.4,
           stat = "identity",position = "stack") +
  geom_bar(data = BGB,
           aes(x = as.factor(treatment),y = -value, fill = factor(GF,levels = c("Soil","Else"))),
           color = "black",alpha = 0.4,width = 0.4,
           stat = "identity",position = "stack") +
  scale_fill_manual(values = c("#137300","darkgrey")) +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "",y = "Carbon stocks [kgC/m²] \n Belowground \t Aboveground",fill = "Source") +
  theme_bw() + theme(text = element_text(size = 20),
                     axis.title.y = element_text(hjust = 0.55),
                     legend.position = "top") +
  guides(fill = FALSE)


ggsave(plot = last_plot(), dpi = 300,height = 20,width = 12,units = "cm",
       filename = file.path(".","Figures","Scheme_stocks.png"))

