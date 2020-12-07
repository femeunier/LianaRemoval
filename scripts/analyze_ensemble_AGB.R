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

Nsimulations = 50

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run"
ED2IN_files <- c(file.path(ref_dir,c("ED2IN_pft_long_control","ED2IN_pft_long_removal")))
names_patch_t <- c("control","removal")

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run/runs"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out"

simu_name = "Test_pft_patch"


if(!dir.exists(rundir)) dir.create(rundir)
if(!dir.exists(outdir)) dir.create(outdir)


all.OP <- data.frame()

for (simu in seq(1,Nsimulations)){
  for (patch_type in seq(1,length(ED2IN_files))){
    ed2in <- read_ed2in(ED2IN_files[patch_type])

    ed2in$ITOUTPUT <- 0

    run_name <- paste0(simu_name,"_",names_patch_t[patch_type],"_",simu)

    run_ref <- file.path(rundir,run_name)
    out_ref <- file.path(outdir,run_name)


    out.Rdatafile <- file.path(out_ref,"analy",paste0("analysis.RData"))

    if (file.exists(out.Rdatafile)){
       load(out.Rdatafile)
       AGB <- datum$emean$agb
       BGB <- datum$emean$biomass - AGB
       Soil <- datum$emean$fast.soil.c + datum$emean$slow.soil.c + datum$emean$struct.soil.c

       all.OP <- bind_rows(list(all.OP,
                                data.frame(agb = AGB,bgb = BGB,soilC = Soil,
                                           isimu = simu,
                                           treatment = names_patch_t[patch_type])))
    }
  }
}


saveRDS(object = all.OP, file = "AGB_ensemble.RDS")

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/analyze_ensemble_AGB.R hpc:/data/gent/vo/000/gvo00074/felicien/R
