rm(list = ls())

library(ggplot2)
library(dplyr)
library(LianaRemoval)
library(albedo)
library(reshape2)
library(tidyr)

# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_17/analy/analysis.RData","./outputs/removal.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_17/analy/analysis.RData","./outputs/control.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000010611/control.RData","./outputs/control.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000010611/removal.RData","./outputs/removal.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_147/analy/analysis.RData","./outputs/removal.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_147/analy/analysis.RData","./outputs/control.RData"))

year.min = 2011
year.max = 2016

Vars <- c("nep","gpp","npp","het.resp","plant.resp","lai")
load("./outputs/control.RData")
control <- datum2df(datum, vars = Vars)

load("./outputs/removal.RData")
removal <- datum2df(datum, vars = Vars)

simulations <- bind_rows(list(bind_rows(list(control %>% mutate(simulation = "control"),
                              removal %>% mutate(simulation = "removal"))) %>% filter(var %in% c("nep","het.resp")),
                              bind_rows(list(control %>% mutate(simulation = "control"),
                                             removal %>% mutate(simulation = "removal"))) %>% filter(var %in% c("gpp","npp","plant.resp","lai")) %>% group_by(var,time,simulation,year, month) %>%
                                summarise(value = sum(value,na.rm = TRUE)) %>% mutate(pft = 18))) %>%
  filter(year >= year.min & year <= year.max)

ggplot(data = simulations) +
  geom_line(aes(x = time,y = value,color = simulation)) +
  facet_wrap(~ var) +
  theme_bw()

simulations.sum <- simulations %>% group_by(var,month,simulation) %>% summarise(value_m = mean(value),
                                                                                value_sd = sd(value))

ggplot(data = simulations.sum) +
  geom_line(aes(x = month,y = value_m,color = simulation)) +
  geom_ribbon(aes(x = month,
                  ymin = value_m - value_sd,
                  ymax = value_m + value_sd,fill = simulation),alpha = 0.4,color = NA) +
  facet_wrap(~ var) +
  theme_bw()

simulations.sum %>% filter(var == "nep") %>% select(month,simulation,value_m) %>% pivot_wider(values_from = value_m,names_from = simulation) %>%
  mutate(diff = removal - control) %>% pull(diff) %>% sum()

simulations.sum %>% group_by(var,simulation) %>% summarise(m = mean(value_m))

simulations.sum.year <- simulations %>% group_by(var,simulation,year) %>% summarise(value = mean(value)) %>%
  group_by(var,simulation)

simulations.sum.year %>% summarise(value_m = mean(value),
                                   value_sd = sd(value),
                                   N = length(value)) %>% group_by(var) %>%
  summarise(pval = t.test2(value_m[simulation == "control"],value_m[simulation == "removal"],
                           value_sd[simulation == "control"],value_sd[simulation == "removal"],
                           N[simulation == "control"],N[simulation == "removal"]))

ggplot(data = simulations.sum.year) +
  geom_boxplot(aes(x = simulation, y = value,fill = simulation )) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()


simulations.sum.year %>% group_by(var,simulation) %>% summarise(m = mean(value),
                                                                sd_v = sd(value),
                                                                se = sd(value)/length(value))

