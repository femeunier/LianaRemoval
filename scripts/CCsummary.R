rm(list = ls())

library(ggplot2)
library(dplyr)
library(LianaRemoval)
library(albedo)
library(reshape2)
library(tidyr)

system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000010765/control.RData","./outputs/control.RData"))
system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000010765/removal.RData","./outputs/removal.RData"))

year.min = 2011
year.max = 2014

Vars <- c("nep","gpp","npp","het.resp","plant.resp")
load("./outputs/control.RData")
control <- datum2df(datum, vars = Vars)

load("./outputs/removal.RData")
removal <- datum2df(datum, vars = Vars) %>% mutate(value = case_when(is.na(value) ~ 0,
                                                                     TRUE ~ value))


simulations <- bind_rows(list(bind_rows(list(control %>% mutate(simulation = "control"),
                                             removal %>% mutate(simulation = "removal"))) %>% filter(var %in% c("nep","het.resp")),
                              bind_rows(list(control %>% mutate(simulation = "control"),
                                             removal %>% mutate(simulation = "removal"))) %>% filter(var %in% c("gpp","npp","plant.resp")),
                              bind_rows(list(control %>% mutate(simulation = "control"),
                                             removal %>% mutate(simulation = "removal"))) %>% filter(var %in% c("gpp","npp","plant.resp")) %>% group_by(var,time,simulation,year, month) %>%
                                summarise(value = sum(value,na.rm = TRUE)) %>% mutate(pft = 18))) %>%  filter(year >= year.min & year <= year.max) %>%
  mutate(GF = case_when(pft == 17 ~ "Liana",
                        pft == 18 ~ "Ecosystem",
                        TRUE ~ "Tree")) %>% group_by(var,simulation,year,GF,time) %>% summarise(value = sum(value)) %>%
  mutate(year.num = floor((time-1)/12)+1) %>% group_by(var,simulation,GF,year.num) %>% add_count()

simulations.sum.year <- simulations %>% group_by(var,simulation,year.num,GF) %>% summarise(value = mean(value)) %>%
  group_by(var,simulation,GF) %>% summarise(value_m = mean(value),
                                            value_sd = sd(value),
                                            N = length(value),
                                            value_se = sd(value)/sqrt(length(value)))


simulations %>% group_by(var,simulation,year.num,GF) %>% summarise(value = mean(value)) %>% ungroup() %>%
  group_by(var,GF) %>% summarise(p.val = kruskal.test(formula = value ~ as.factor(simulation))$p.value) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                                                                     p.val <= 0.10 ~ "*",
                                                                                                                                     TRUE ~ ""))

