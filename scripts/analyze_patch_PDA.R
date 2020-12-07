rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAnRTM)
library(purrr)
library(rrtm)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)
library(ggplot2)
library(ggridges)
library(cowplot)
library(pracma)
library(BayesianTools)
library(albedo)
library(stringr)

runs = 99000008915:99000008919
Nsimulations = length(runs)

# Data
tinit = 2011+3/12
t = seq(tinit,2014,length.out = 2)
tall = seq(tinit,2014,length.out = 34)

Bcon <- (108.6 - 25.6*(exp(-0.0162246*(t-tinit))))/10
Brem <- (166.5 -83.5*(exp(-0.03574508*(t-tinit))))/10
Delta_Bcon <- Bcon - Bcon[1]
Delta_Brem <- Brem - Brem[1]
Delta_obs <- c(Delta_Bcon,Delta_Brem)[seq(2,length(c(Delta_Bcon,Delta_Brem)),2)]

Bcon.all <- (108.6 - 25.6*(exp(-0.0162246*(tall-tinit))))/10
Brem.all <- (166.5 -83.5*(exp(-0.03574508*(tall-tinit))))/10
Delta_Bcon.all <- Bcon.all - Bcon.all[1]
Delta_Brem.all <- Brem.all - Brem.all[1]
Delta_obs.all <- c(Delta_Bcon.all,Delta_Brem.all)

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/"
rundir2 <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run"
names_patch_t <- c("control","removal")
simu_name = "Test_pft_patch"

Delta_AGB <- matrix(NA,nrow = Nsimulations, ncol = 2)
Delta_AGB_all <- matrix(NA,nrow = Nsimulations, ncol = 2*length(tall))

df_params <- data.frame()

for (simu in seq(1,length(runs))){
  for (patch_type in seq(1,length(names_patch_t))){

    run_name <- runs[simu]
    run_ref <- file.path(rundir,run_name)
    datum.file <- file.path(run_ref,paste0(names_patch_t[patch_type],".RData"))

    if (file.exists(datum.file)) {
      load(datum.file)
    } else {
      next
    }

    config_file <- file.path(rundir2,run_name,paste0("config",".xml"))

    if (names_patch_t[patch_type] == "control"){
      clumping_factor <- modify(c(2,3,4,17),get_ED_default_pft,xml = config_file,var = "clumping_factor")
      Vm0 <- modify(c(2,3,4,17),get_ED_default_pft,xml = config_file,var = "Vm0")
      df_params <- bind_rows(list(df_params,
                                  data.frame(clumping_factor = clumping_factor,
                                             Vm0 = Vm0,
                                             pft = c(2,3,4,17),
                                             index = simu)))
    }

    pft <- datum$cohort$pft
    area <- datum$cohort$area
    nplant <- datum$cohort$nplant
    dbh <- datum$cohort$dbh
    agb <- datum$cohort$agb
    time <- datum$year + (datum$month)/12

    i = 1
    AGB <- unlist(map(1:length(pft),function(i){
      sum((agb[[i]]*nplant[[i]])[(dbh[[i]] > 1 & pft[[i]] == 17) | (dbh[[i]] > 10 & pft[[i]] != 17)])}))

    AGB_init <- AGB[1]
    AGB_end <- AGB[length(AGB)]
    AGBs <- linspace(0,AGB_end - AGB_init,n = length(which(datum$year < 2014)))

    Delta_AGB[simu,patch_type] <- AGB_end - AGB_init
    Delta_AGB_all[simu,seq((patch_type-1)*length(AGBs) + 1,(patch_type)*length(AGBs))] <- AGBs
  }
}

RMSE <- sqrt(apply((matrix(sort(rep(Delta_obs,Nsimulations)),nrow = Nsimulations,ncol = 2) - Delta_AGB)**2,1,sum)/2)
RMSE.lin <- sqrt(apply((matrix(rep(Delta_obs.all,Nsimulations),nrow = Nsimulations,byrow = TRUE) - Delta_AGB_all)**2,1,sum)/ncol(Delta_AGB_all))

saveRDS(list(param = df_params,results = RMSE), file = "Results_PDA.RDS")

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/analyze_patch_PDA.R hpc:/data/gent/vo/000/gvo00074/felicien/R
