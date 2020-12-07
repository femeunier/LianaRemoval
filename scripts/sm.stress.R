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


all.OP <- data.frame()

for (i in seq(1,N)){

  print(i)
  local.dir <- file.path(directory,paste0(init+i))

  if (file.exists(file.path(local.dir,"control.RData")) & file.exists(file.path(local.dir,"removal.RData"))){
    load(file.path(local.dir,"control.RData"))

    sm.stress <- datum$emean$sm.stress
    leaf.psi.md <- datum$emean$leaf.psi.md
    leaf.psi.pd <- datum$emean$leaf.psi.pd
    leaf.par <- datum$emean$leaf.par
    het.resp <- datum$emean$het.resp
    lai <- datum$emean$lai
    par.gnd <- datum$emean$par.gnd

    transp <- datum$emean$transp
    et <- datum$emean$et
    gpp <- datum$emean$gpp

    control <- bind_rows(list(datum2df(datum, vars = c("sm.stress","leaf.psi.md","leaf.psi.pd","leaf.par","lai","transp","wflxlc","transp","gpp","nplant","bdead","agb"),  pfts = c(2, 3, 4, 17), name = "control"),
                              data.frame(var = c(rep("sm.stress",length(sm.stress)),rep("leaf.psi.md",length(sm.stress)),rep("leaf.psi.pd",length(sm.stress)),rep("leaf.par",length(sm.stress)),rep("het.resp",length(sm.stress)),rep("lai",length(sm.stress)),rep("par.gnd",length(sm.stress)),rep("transp",length(sm.stress)),rep("et",length(sm.stress)),rep("gpp",length(sm.stress))),
                                         time = 1:length(sm.stress),
                                         pft = 18,value = c(sm.stress,leaf.psi.md,leaf.psi.pd,leaf.par,het.resp,lai,par.gnd,transp,et,gpp),simulation = "control",
                                         month = datum$month,year = datum$year)))

    load(file.path(local.dir,"removal.RData"))

    sm.stress <- datum$emean$sm.stress
    leaf.psi.md <- datum$emean$leaf.psi.md
    leaf.psi.pd <- datum$emean$leaf.psi.pd
    leaf.par <- datum$emean$leaf.par
    het.resp <- datum$emean$het.resp
    lai <- datum$emean$lai
    par.gnd <- datum$emean$par.gnd

    transp <- datum$emean$transp
    et <- datum$emean$et
    gpp <- datum$emean$gpp


    removal <- bind_rows(list(datum2df(datum, vars = c("sm.stress","leaf.psi.md","leaf.psi.pd","leaf.par","lai","wflxlc","transp","gpp","nplant","bdead","agb"),  pfts = c(2, 3, 4, 17), name = "removal"),
                              data.frame(var = c(rep("sm.stress",length(sm.stress)),rep("leaf.psi.md",length(sm.stress)),rep("leaf.psi.pd",length(sm.stress)),rep("leaf.par",length(sm.stress)),rep("het.resp",length(sm.stress)),rep("lai",length(sm.stress)),rep("par.gnd",length(sm.stress)),rep("transp",length(sm.stress)),rep("et",length(sm.stress)),rep("gpp",length(sm.stress))),
                                         time = 1:length(sm.stress),
                                         pft = 18,value = c(sm.stress,leaf.psi.md,leaf.psi.pd,leaf.par,het.resp,lai,par.gnd,transp,et,gpp),simulation = "removal",
                                         month = datum$month,year = datum$year)))

    all <- bind_rows(list(control,removal)) %>% mutate(time = year + month/12)

    all.OP <- bind_rows(list(all.OP,
                             all %>% mutate(irun = i)))
  }
}

saveRDS(object = all.OP,file = file.path(".","OP.posterior.smstress.RDS"))

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/sm.stress.R hpc:/data/gent/vo/000/gvo00074/felicien/R
