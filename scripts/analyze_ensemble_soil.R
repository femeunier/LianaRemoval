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
year.max = 2014

Vars = c("fast.soil.c","struct.soil.c","slow.soil.c")

all.OP <- data.frame()

for (i in seq(1,N)){

  print(i)
  local.dir <- file.path(directory,paste0(init+i))

  if (file.exists(file.path(local.dir,"control.RData")) & file.exists(file.path(local.dir,"removal.RData"))){

    load(file.path(local.dir,"control.RData"))
    control <- datum2df(datum, vars = Vars,  pfts = c(2, 3, 4, 17), name = "control")

    load(file.path(local.dir,"removal.RData"))
    removal <- datum2df(datum, vars = Vars,  pfts = c(2, 3, 4, 17), name = "removal") %>% mutate(value = case_when(is.na(value) ~ 0,
                                                                                                                   TRUE ~ value))

    all <- bind_rows(list(control,removal)) %>% mutate(time = year + month/12)

    all.OP <- bind_rows(list(all.OP,
                             all %>% mutate(irun = i)))

  }
}

saveRDS(object = all.OP,file = file.path(".","OP.posterior.soil.RDS"))

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/analyze_ensemble_soil.R hpc:/data/gent/vo/000/gvo00074/felicien/R
