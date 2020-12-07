rm(list = ls())

library(dplyr)
library(rhdf5)
library(purrr)
library(tidyr)
library(ggplot2)

source("~/Documents/ED2/R-utils/h5read_opt.r")

# Initial stocks
system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control-Q-2011-03-00-000000-g01.h5",
                      "./outputs/Initial_control.h5"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removal-Q-2011-03-00-000000-g01.h5",
                      "./outputs/Initial_removal.h5"))

# short-experiment stocks
system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control-Q-2014-02-00-000000-g01.h5",
                      "./outputs/Short_control.h5"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removal-Q-2014-02-00-000000-g01.h5",
                      "./outputs/Short_removal.h5"))

# short-experiment stocks
system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control-Q-2021-02-00-000000-g01.h5",
                      "./outputs/Long_control.h5"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removal-Q-2021-02-00-000000-g01.h5",
                      "./outputs/Long_removal.h5"))

h5file_control <- "./outputs/Initial_control.h5"
h5file_removal <- "./outputs/Initial_removal.h5"
h5file_control_short <- "./outputs/Short_control.h5"
h5file_removal_short <- "./outputs/Short_removal.h5"
h5file_control_long <- "./outputs/Long_control.h5"
h5file_removal_long <- "./outputs/Long_removal.h5"


OP.all2plot <- bind_rows(list(
  Qfile2Cstocks(h5file = h5file_control) %>% mutate(treatment = "Control",
                                                    time = "Initial carbon stocks"),
  Qfile2Cstocks(h5file_removal) %>% mutate(treatment = "Removal",
                                           time = "Initial carbon stocks"),
  Qfile2Cstocks(h5file = h5file_control_short) %>% mutate(treatment = "Control",
                                                         time = "After 3 years"),
  Qfile2Cstocks(h5file = h5file_removal_short) %>% mutate(treatment = "Removal",
                                                         time = "After 3 years"),
  Qfile2Cstocks(h5file = h5file_control_long) %>% mutate(treatment = "Control",
                                                         time = "After 10 years"),
  Qfile2Cstocks(h5file = h5file_removal_long) %>% mutate(treatment = "Removal",
                                                         time = "After 10 years"))) %>% mutate(time = as.factor(time))

OP.all2plot$time <- factor(OP.all2plot$time,levels = c("Initial carbon stocks",'After 3 years',"After 10 years"))

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
  mutate(GF = case_when(pft == 17 ~ "Liana",
                        pft == 18 ~ "Soil",
                        TRUE ~ "Tree"))

OP.all2plot_sd <- bind_rows(list(OP.all2plot %>% filter(pft == 18,((type == "soil" & var == "bgb") | (type == "biomass" & var == "agb"))),
                                 OP.all2plot %>% filter((var == "bgb" & stat == "mean" & pft == 18) | (type == "soil" & stat == "mean")) %>% ungroup() %>% group_by(treatment,time) %>%
                                   summarise(value = sum(value),
                                             N = mean(N)) %>% ungroup() %>% mutate(type = "soil",var = "bgb",stat = "mean",pft = 18))) %>% pivot_wider(
                                               names_from = stat,values_from = value)

P.vals <- OP.all2plot_sd %>% group_by(time,var) %>% summarise(p.val = t.test2(mean[treatment == "Control"],mean[treatment == "Removal"],
                                                                              sd[treatment == "Control"],sd[treatment == "Removal"],
                                                                              N[treatment == "Control"],N[treatment == "Removal"])) %>%
  mutate(signif = case_when(p.val <= 0.05 ~ "**",
                            p.val <= 0.10 ~ "*",
                            TRUE ~ ""))
OP.all2plot_sd <- OP.all2plot_sd %>% left_join(P.vals) %>% ungroup()

ggplot() +
  geom_errorbar(data =  subset(OP.all2plot_sd,var == "agb"),
                aes(x = as.factor(treatment),y = mean,ymin = mean,ymax = mean + sd),width = 0.2) +
  geom_bar(data = subset(OP.all2plot_m,var == "agb"),
           aes(x = as.factor(treatment),y = value, fill = as.factor(pft)),
           color = "black",alpha = 0.4,
           stat = "identity",position = "stack") +
  geom_text(data = subset(OP.all2plot_sd,var == "agb") %>% group_by(time) %>% summarise(value_m = max(mean),
                                                                                        signif = signif[1],
                                                                                        treatment = treatment[1]),
            aes(x = 1.5,label = signif, y = value_m*1.05),size = 5) +
  geom_errorbar(data =  subset(OP.all2plot_sd,var == "bgb"),
                aes(x = as.factor(treatment),y = -mean,ymin = -mean,ymax = -mean - sd),width = 0.2) +
  geom_bar(data = subset(OP.all2plot_m,var != "agb"),
           aes(x = as.factor(treatment),y = -value, fill = as.factor(pft)),
           color = "black",alpha = 0.4,
           stat = "identity",position = "stack") +
  geom_text(data = subset(OP.all2plot_sd,var == "bgb") %>% group_by(time) %>% summarise(value_m = max(mean),
                                                                                        signif = signif[1],
                                                                                        treatment = treatment[1]),
            aes(x = 1.5,label = signif, y = -value_m*1.08),size = 5) +
  facet_wrap(~ time) +
  geom_text(data = subset(OP.all2plot_m,var != "agb"),
            aes(pos_X,
                -pos_Y, group=as.factor(treatment), label=label), size=4) +
  scale_fill_manual(values = c("#1E64C8","darkgrey","#9FFF8C","#44CC29","#137300"),
                    labels = c("Liana","Soil","Early","Mid","Late")) +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "",y = "Carbon stocks [kgC/m²] \n Belowground \t Aboveground",fill = "Source") +
  theme_bw() + theme(text = element_text(size = 16),
                     axis.title.y = element_text(hjust = 0.55),
                     legend.position = "top") +
  guides(fill = guide_legend(title.position = "top",label.vjust = 0.5))


ggsave(plot = last_plot(), dpi = 300,height = 20,width = 16,units = "cm",
       filename = file.path(".","Figures","Carbon.Stocks.all.png"))



