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
all.OP.nep <- data.frame()

for (i in seq(1,N)){

  print(i)
  local.dir <- file.path(directory,paste0(init+i))

  if (file.exists(file.path(local.dir,"control.RData")) & file.exists(file.path(local.dir,"removal.RData"))){

    load(file.path(local.dir,"control.RData"))
    control <- datum$emean$agb
    control.nep <- datum$emean$nep

    load(file.path(local.dir,"removal.RData"))
    removal <-  datum$emean$agb
    removal.nep <- datum$emean$nep

    all.OP <- bind_rows(list(all.OP,
                             data.frame(agb = control,month=datum$month,year=datum$year,treatment = "control") %>% mutate(irun = i),
                             data.frame(agb = removal,month=datum$month,year=datum$year,treatment = "removal") %>% mutate(irun = i)))

    all.OP.nep <- bind_rows(list(all.OP.nep,
                             data.frame(nep = control.nep,month=datum$month,year=datum$year,treatment = "control") %>% mutate(irun = i),
                             data.frame(nep = removal.nep,month=datum$month,year=datum$year,treatment = "removal") %>% mutate(irun = i)))
  }
}

saveRDS(object = all.OP,file = file.path(".","AGB_dynamics.RDS"))
saveRDS(object = all.OP.nep,file = file.path(".","NEP_dynamics.RDS"))

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/agb.dynamics.R hpc:/data/gent/vo/000/gvo00074/felicien/R
