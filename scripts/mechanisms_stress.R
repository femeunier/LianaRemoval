rm(list = ls())

library(ggplot2)
library(cowplot)
library(dplyr)
library(albedo)
library(pracma)
library(reshape2)
library(purrr)
library(tidyr)
library(LianaRemoval)

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_147/analy/analysis.RData",
                      "./outputs/control_early.RData"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_147/analy/analysis.RData",
                      "./outputs/removal_early.RData"))

# Vars = c("fast.soil.c","slow.soil.c","struct.soil.c")
Vars = c("sm.stress","par.gnd","leaf.par","transp","npp")

load("./outputs/control_early.RData")
control <- datum2df(datum, vars = Vars,  pfts = c(2, 3, 4, 17), name = "control")

load("./outputs/removal_early.RData")
removal <- datum2df(datum, vars = Vars,  pfts = c(2, 3, 4, 17), name = "removal")

all <- bind_rows(list(control,removal)) %>% mutate(time = year + month/12)

ggplot(data = all %>% filter(var %in% c("sm.stress",
                                        "leaf.par",
                                        "transp",
                                        "npp"))) +
  geom_line(aes(x = time, y = value,
                linetype = as.factor(simulation),
                color = as.factor(pft))) +
  facet_wrap(~ var, scales = "free") +
  scale_color_manual(values = c("#9FFF8C","#44CC29","#137300","#1E64C8","darkgrey")) +
  theme_bw()

ggsave(plot = last_plot(), dpi = 300,height = 25,width = 35,units = "cm",
       filename = file.path(".","Figures","patch_dyn.png"))

all.diff <-
  all %>% filter(var == c("sm.stress",
                          "leaf.par",
                          "transp",
                          "npp")) %>%
  dplyr::select(time,var,simulation,time,pft,value) %>% pivot_wider(names_from = simulation,
                                                                values_from = -c(time,pft,simulation)) %>%
  # replace_na(list(value_removal = 0)) %>%
  dplyr::select(time,pft,var_control,value_control,value_removal) %>% rename(var = var_control) %>%
  mutate(diff = value_control - value_removal)

ggplot(data = all.diff) +
  geom_line(aes(x = time, y = diff,
                color = as.factor(pft))) +
  scale_color_manual(values = c("#9FFF8C","#44CC29","#137300","#1E64C8")) +
  facet_wrap(~ var, scales = "free",nrow = 2) +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "",y = "Control - Removal",color = "PFT") +
  theme_bw() + theme(text = element_text(size = 16))

ggsave(plot = last_plot(), dpi = 300,height = 25,width = 35,units = "cm",
       filename = file.path(".","Figures","patch_diff.png"))

