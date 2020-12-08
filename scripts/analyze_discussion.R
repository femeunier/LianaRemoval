rm(list = ls())

library(ggplot2)
library(dplyr)
library(albedo)
library(reshape2)
library(tidyr)

# load("/home/femeunier/Documents/projects/LianaRemoval/outputs/removal.RData")
# control <- datum
# removal <- datum

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
    control <- datum

    load(file.path(local.dir,"removal.RData"))
    removal <- datum

    df_control <- data.frame(time = control$year + (control$month - 1)/12,
                             run = i,
                             topsoil.wc = control$emean$soil.water[,15],
                             subsoil.wc = control$emean$soil.water[,13],
                             par.gnd = control$emean$par.gnd,
                             tree.seeds = apply(control$szpft$bseeds[,12,c(2,3,4)],1,sum))
    df_removal <- data.frame(time = removal$year + (removal$month - 1)/12,
                             run = i,
                             topsoil.wc = removal$emean$soil.water[,15],
                             subsoil.wc = removal$emean$soil.water[,13],
                             par.gnd = removal$emean$par.gnd,
                             tree.seeds = apply(removal$szpft$bseeds[,12,c(2,3,4)],1,sum))

    all.OP <- bind_rows(list(all.OP,
                             df_control %>% mutate(type = "control"),
                             df_removal %>% mutate(type = "removal")))
  }
}

saveRDS(object = all.OP,file = file.path(".","OP.posterior.runs.RDS"))

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/analyze_discussion.R hpc:/data/gent/vo/000/gvo00074/felicien/R

