rm(list = ls())

library(ggplot2)
library(cowplot)
library(dplyr)
library(albedo)
library(pracma)
library(reshape2)
library(purrr)
library(LianaRemoval)

final.year <- 2014

N = 100
init = 99000011369
directory <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out"

all.OP <- data.frame()

for (i in seq(1,N)){


  print(i)
  local.dir <- file.path(directory,paste0(init+i))

  if (file.exists(file.path(local.dir,"control.RData")) & file.exists(file.path(local.dir,"removal.RData"))){

    load(file.path(local.dir,"control.RData"))

    AGB <- datum$emean$agb
    BGB <- datum$emean$biomass - AGB
    Soil.carbon <- datum$emean$fast.soil.c + datum$emean$slow.soil.c + datum$emean$struct.soil.c

    Nd <- length(AGB)

    all.OP <- bind_rows(list(all.OP,
                             data.frame(value = c(AGB,BGB,Soil.carbon),
                                        year = datum$year,
                                        month = datum$month,
                                        var = c(rep("AGB",Nd),rep("BGB",Nd),rep("Soil.Carbon",Nd)),
                                        treatment = "control",
                                        irun = i)))

    load(file.path(local.dir,"removal.RData"))


    AGB <- datum$emean$agb
    BGB <- datum$emean$biomass - AGB
    Soil.carbon <- datum$emean$fast.soil.c + datum$emean$slow.soil.c + datum$emean$struct.soil.c

    Nd <- length(AGB)

    all.OP <- bind_rows(list(all.OP,
                             data.frame(value = c(AGB,BGB,Soil.carbon),
                                        year = datum$year,
                                        month = datum$month,
                                        var = c(rep("AGB",Nd),rep("BGB",Nd),rep("Soil.Carbon",Nd)),
                                        treatment = "removal",
                                        irun = i)))

  }
}

saveRDS(object = all.OP,
        file = "all_OP_C.RDS")

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/analyze_ensemble_C.R hpc:/data/gent/vo/000/gvo00074/felicien/R


