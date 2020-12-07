rm(list = ls())

library(dplyr)
library(rhdf5)
library(purrr)
library(tidyr)
library(LianaRemoval)
library(ggplot2)

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control-Q-2010-12-00-000000-g01.h5",
#                       "./outputs/control-2010-12.h5"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removal-Q-2010-12-00-000000-g01.h5",
#                       "./outputs/removal-2010-12.h5"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control-Q-2021-02-00-000000-g01.h5",
#                       "./outputs/control-2014-02.h5"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removal-Q-2021-02-00-000000-g01.h5",
#                       "./outputs/removal-2014-02.h5"))

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_147/analy/analysis-Q-2010-12-00-000000-g01.h5",
#                       "./outputs/control-2010-12.h5"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_147/analy/analysis-Q-2010-12-00-000000-g01.h5",
#                       "./outputs/removal-2010-12.h5"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_147/analy/analysis-Q-2014-02-00-000000-g01.h5",
#                       "./outputs/control-2014-02.h5"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_147/analy/analysis-Q-2014-02-00-000000-g01.h5",
#                       "./outputs/removal-2014-02.h5"))

h5file_control <- "./outputs/control-2010-12.h5"
h5file_removal <- "./outputs/removal-2010-12.h5"
h5file_control_last <- "./outputs/control-2014-02.h5"
h5file_removal_last <- "./outputs/removal-2014-02.h5"

OP.all2plot <- bind_rows(list(
  Qfile2Cstocks(h5file = h5file_control) %>% mutate(treatment = "control",
                                           time = "Beginning"),
  Qfile2Cstocks(h5file_removal) %>% mutate(treatment = "removal",
                                           time = "Beginning"),
  Qfile2Cstocks(h5file = h5file_control_last) %>% mutate(treatment = "control",
                                                time = "End"),
  Qfile2Cstocks(h5file = h5file_removal_last) %>% mutate(treatment = "removal",
                                                time = "End")))

OP.all2plot_m <- OP.all2plot %>% filter(stat == "mean") %>%   filter(!(pft == 18 & type == "biomass")) %>%
  group_by(treatment,time) %>%
  mutate(pos_X = case_when(treatment == "control" ~ 1,
                           treatment == "removal" ~ 2),
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

P.vals <- OP.all2plot_sd %>% group_by(time,var) %>% summarise(p.val = t.test2(mean[treatment == "control"],mean[treatment == "removal"],
                                                                              sd[treatment == "control"],sd[treatment == "removal"],
                                                                              N[treatment == "control"],N[treatment == "removal"])) %>%
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
  labs(x = "",y = "Carbon stocks [kgC/m²]",fill = "Source") +
  theme_bw()


ggsave(plot = last_plot(), dpi = 300,height = 20,width = 12,units = "cm",
       filename = file.path(".","Figures","Carbon.Stocks.short.png"))

# OP.all2plot.sum <-
#   OP.all2plot %>% group_by(treatment,GF,time,var) %>% summarise(value = sum(value),
#                                                               type = type[1],
#                                                               pos_X = pos_X[1],
#                                                               pos_Y = pos_Y[1],
#                                                               label = label[1]) %>%
#   group_by(treatment,time) %>%
#   mutate(pos_Y = case_when(var == "SSC" ~ sum(value[GF == "Tree" & var != "agb"] + value[var == "FSC"] + value[var == "SSC"]/2),
#                            var == "StructSC" ~ sum(value[GF == "Tree" & var != "agb"]+ value[var == "FSC"] + value[var == "SSC"] + value[var == "StructSC"]/2),
#                            TRUE ~ 0))
#
# ggplot() +
#   geom_bar(data = subset(OP.all2plot.sum,var == "agb"),
#            aes(x = as.factor(treatment),y = value, fill = as.factor(GF)),
#            color = "black",alpha = 0.4,
#            stat = "identity",position = "stack") +
#   geom_bar(data = subset(OP.all2plot.sum,var != "agb"),
#            aes(x = as.factor(treatment),y = -value, fill = as.factor(GF)),
#            color = "black",alpha = 0.4,
#            stat = "identity",position = "stack") +
#   facet_wrap(~ time) +
#   geom_text(data = subset(OP.all2plot.sum,var != "agb"),
#             aes(pos_X,
#                 -pos_Y, group=as.factor(treatment), label=label), size=4) +
#   scale_fill_manual(values = c("#1E64C8","darkgrey","#137300"),
#                     labels = c("Liana","Soil","Tree")) +
#   geom_hline(yintercept = 0, linetype = 3) +
#   labs(x = "",y = "Carbon stocks [kgC/m²]",fill = "Source") +
#   theme_bw()



