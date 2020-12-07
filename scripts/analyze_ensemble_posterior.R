rm(list = ls())

library(ggplot2)
library(dplyr)
library(albedo)
library(reshape2)
library(tidyr)

N = 100
init = 99000011369
directory <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out"

year.min = 2011
year.max = 2021

Vars <- c("nep","gpp","npp","het.resp","plant.resp")

all.OP <- data.frame()

for (i in seq(1,N)){

  print(i)
  local.dir <- file.path(directory,paste0(init+i))

  if (file.exists(file.path(local.dir,"control.RData")) & file.exists(file.path(local.dir,"removal.RData"))){

    load(file.path(local.dir,"control.RData"))
    control <- datum2df(datum, vars = Vars)

    load(file.path(local.dir,"removal.RData"))
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

    simulations.sum.year <- simulations %>% group_by(var,simulation,year.num,GF) %>% summarise(value = mean(value))

    all.OP <- bind_rows(list(all.OP,
                             simulations.sum.year %>% mutate(irun = i)))
  }
}

saveRDS(object = all.OP,file = file.path(".","OP.posterior.runs.RDS"))

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/analyze_ensemble_posterior.R hpc:/data/gent/vo/000/gvo00074/felicien/R

