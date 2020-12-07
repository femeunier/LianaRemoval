rm(list = ls())

library(dplyr)
library(rhdf5)
library(purrr)
library(tidyr)
library(ggplot2)

source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")

###################################################################################
# Function
Qfile2Cstocks <- function(h5file,agf.bs = 0.7){

  mymont    = lapply(h5read_opt(h5file),FUN=aperm)
  names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

  PFT = mymont$PFT

  bdeadconow = mymont$BDEAD
  bfrootconow = mymont$MMEAN.BROOT.CO
  bleafconow = mymont$MMEAN.BLEAF.CO
  bsapwoodconow = mymont$BSAPWOODA+mymont$BSAPWOODB
  bstorageconow = mymont$MMEAN.BSTORAGE.CO
  bseedsconow = mymont$BSEEDS.CO
  bsapwoodb = mymont$BSAPWOODB
  bsapwooda = mymont$BSAPWOODA

  nplantconow = mymont$NPLANT
  areasi = mymont$AREA.SI
  npatches = mymont$SIPA.N
  areapa = mymont$AREA * rep(areasi,times=npatches)
  ncohorts = mymont$PACO.N
  ipa = rep(seq(1,length(areapa)),mymont$PACO.N)
  areaconow = rep(areapa,times=ncohorts)
  w.nplant  = nplantconow  * areaconow

  bcrootconow = bsapwooda + (1. - agf.bs) * bdeadconow
  bstemconow = bsapwoodb + agf.bs* bdeadconow
  brootconow = bfrootconow + bcrootconow
  baliveconow = bleafconow + bfrootconow + bsapwoodconow
  biomassconow = baliveconow + bstorageconow + bseedsconow + bdeadconow

  agbconow = bsapwoodb + agf.bs* bdeadconow + bleafconow + bseedsconow + agf.bs*bstorageconow
  bgbconow = bsapwooda + (1. - agf.bs) * bdeadconow + bfrootconow + (1. - agf.bs) * bstorageconow


  biomass_sd <-
    data.frame(pft = PFT,
               agb = agbconow,
               bgb = bgbconow,
               nplant = nplantconow,
               areapa = areaconow,
               patch = ipa) %>% group_by(patch) %>% summarise(agb = sum(agb*nplant,na.rm = TRUE),
                                                              bgb = sum(bgb*nplant,na.rm = TRUE),
                                                              areapa = areapa[1]) %>% ungroup() %>%
    summarise(agb_m = weighted.mean(x = agb,w = areapa),
              bgb_m = weighted.mean(x = bgb,w = areapa),
              agb_sd = sqrt(sum(areapa * (agb - agb_m)^2)),
              bgb_sd = sqrt(sum(areapa * (bgb - bgb_m)^2))) %>% select(c(agb_sd,bgb_sd)) %>% rename(agb = agb_sd,
                                                                                                    bgb = bgb_sd) %>%
    pivot_longer(cols = c("agb","bgb")) %>% mutate(stat = "sd",pft = 18, type = "biomass") %>% rename(var = name)

  tmp.pft <-
    data.frame(pft = PFT,
               agb = agbconow,
               bgb = bgbconow,
               w = w.nplant) %>% group_by(pft) %>% summarise(agb = sum(agb*w,na.rm = TRUE),
                                                             bgb = sum(bgb*w,na.rm = TRUE))

  pft.all <- bind_rows(list(
    bind_rows(list(tmp.pft,
                   tmp.pft %>% ungroup() %>% summarise(agb = sum(agb),
                                                       bgb = sum(bgb)) %>% mutate(pft = 18))) %>%
      mutate(type = "biomass") %>% pivot_longer(cols = c("agb","bgb"),
                                                names_to = "var",
                                                values_to = "value") %>% mutate(stat = "mean"),
    biomass_sd))

  bgb_pa_temp <-
    data.frame(bgb = bgbconow,
               nplant = nplantconow,
               areapa = areaconow,
               patch = ipa) %>% group_by(patch) %>% summarise(bgb = sum(bgb*nplant,na.rm = TRUE)) %>% pull(bgb)

  bgb_pa <- rep(0,length(areapa))
  bgb_pa[1:length(bgb_pa_temp)] <- bgb_pa_temp

  soil.all <- bind_rows(list(
    data.frame(FSC = weighted.mean(mymont$MMEAN.FAST.SOIL.C,areapa),
               SSC = weighted.mean(mymont$MMEAN.SLOW.SOIL.C,areapa),
               StructSC = weighted.mean(mymont$MMEAN.STRUCT.SOIL.C,areapa)) %>% mutate(pft = 18,
                                                                                       type = "soil") %>% pivot_longer(cols = c("FSC","SSC","StructSC"),
                                                                                                                       names_to = "var",
                                                                                                                       values_to = "value") %>% mutate(stat = "mean"),
    data.frame(FSC = mymont$MMEAN.FAST.SOIL.C,
               SSC = mymont$MMEAN.SLOW.SOIL.C,
               StructSC = mymont$MMEAN.STRUCT.SOIL.C,
               bgb = bgb_pa,
               areapa,
               ipa = seq(1,length(areapa))) %>% mutate(bgb.all = FSC + SSC + StructSC + bgb) %>% dplyr::select(areapa,ipa,bgb.all) %>% ungroup() %>%
      summarise(bgb_m = weighted.mean(bgb.all,areapa),
                bgb_sd = sqrt(sum(areapa * (bgb.all - bgb_m)^2))) %>%
      select(c(bgb_sd)) %>% rename(value = bgb_sd) %>% mutate(stat = "sd",pft = 18,type = "soil",var = "bgb")))



  OP.all <- bind_rows(list(pft.all,
                           soil.all)) %>% mutate(N = length(areapa))

  return(OP.all)
}

#####################################################################################

N = 100
init = 99000011370
directory <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out"

all.OP <- data.frame()

for (i in seq(1,N)){
  print(i)
  local.dir <- file.path(directory,paste0(init+i))

  if(all(file.exists(c(file.path(local.dir,"control-Q-2011-03-00-000000-g01.h5"),
                      file.path(local.dir,"removal-Q-2011-03-00-000000-g01.h5"),
                      file.path(local.dir,"control-Q-2014-03-00-000000-g01.h5"),
                      file.path(local.dir,"removal-Q-2014-03-00-000000-g01.h5"),
                      file.path(local.dir,"control-Q-2021-02-00-000000-g01.h5"),
                      file.path(local.dir,"removal-Q-2021-02-00-000000-g01.h5"))))){


  OP.all2plot <- bind_rows(list(
    Qfile2Cstocks(h5file = file.path(local.dir,"control-Q-2011-03-00-000000-g01.h5")) %>% mutate(treatment = "Control",
                                                      time = "Initial carbon stocks"),
    Qfile2Cstocks(h5file = file.path(local.dir,"removal-Q-2011-03-00-000000-g01.h5")) %>% mutate(treatment = "Removal",
                                             time = "Initial carbon stocks"),
    Qfile2Cstocks(h5file = file.path(local.dir,"control-Q-2014-03-00-000000-g01.h5")) %>% mutate(treatment = "Control",
                                                            time = "After 3 years"),
    Qfile2Cstocks(h5file = file.path(local.dir,"removal-Q-2014-03-00-000000-g01.h5")) %>% mutate(treatment = "Removal",
                                                            time = "After 3 years"),
    Qfile2Cstocks(h5file = file.path(local.dir,"control-Q-2021-02-00-000000-g01.h5")) %>% mutate(treatment = "Control",
                                                           time = "After 10 years"),
    Qfile2Cstocks(h5file = file.path(local.dir,"removal-Q-2021-02-00-000000-g01.h5")) %>% mutate(treatment = "Removal",
                                                           time = "After 10 years"))) %>% mutate(time = as.factor(time))

    all.OP <- bind_rows(list(all.OP,
                             OP.all2plot %>% mutate(irun = i)))
  }
}

saveRDS(object = all.OP,file = file.path(".","OP.posterior.Cpools.RDS"))

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/analyze_ensemble_Cpools.R hpc:/data/gent/vo/000/gvo00074/felicien/R



