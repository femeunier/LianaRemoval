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

list_dir <- list()

Nsimuperjob = 1
isimu = 0

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run"
ED2IN_files <- c(file.path(ref_dir,c("ED2IN_pft_long_removal")))
names_patch_t <- c("removal")

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run/runs"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out"

simu_name = "Test_pft_patch"

# Clean
# system2("rm",c("-rf",paste0(file.path(rundir,simu_name),"*")))
# system2("rm",c("-rf",paste0(file.path(outdir,simu_name),"*")))

if(!dir.exists(rundir)) dir.create(rundir)
if(!dir.exists(outdir)) dir.create(outdir)

##########################################################################################

for (simu in seq(1,Nsimulations)){
  for (patch_type in seq(1,length(ED2IN_files))){
    ed2in <- read_ed2in(ED2IN_files[patch_type])

    ed2in$ITOUTPUT <- 0

    run_name <- paste0(simu_name,"_",names_patch_t[patch_type],"_",simu)

    isimu = isimu + 1

    run_ref <- file.path(rundir,run_name)
    out_ref <- file.path(outdir,run_name)

    if(!dir.exists(run_ref)) dir.create(run_ref)
    if(!dir.exists(out_ref)) dir.create(out_ref)
    if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
    if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

    # ED2IN
    ed2in_scenar <- ed2in
    ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
    ed2in_scenar$FFILOUT = file.path(out_ref,"analy","analysis")
    ed2in_scenar$SFILOUT = file.path(out_ref,"histo","history")

    # Treefall disturbance rates
    # ed2in_scenar$TREEFALL_DISTURBANCE_RATE <- tf_drate[simu]

    #write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

    # # Config
    # config_simu <- config
    #
    # # Sample
    # pft_samples <- map(1:length(prior), function(i){
    #   samples <- prior[[i]]$sample()
    #   names(samples) <- names(pft_lowers[[i]])
    #   return(samples)
    # }) %>% set_names(names(prior))
    #
    # # Tree
    # for (i in seq(1,length(pft_samples[["Tree"]])/2)){
    #   param_name <- names(pft_samples[["Tree"]][(i-1)*2+1])
    #   param0 <- pft_samples[["Tree"]][(i-1)*2+1]
    #   delta_param <- pft_samples[["Tree"]][(i-1)*2+2]
    #   params <- param0 + delta_param*(((length(config)-1):1)-1)
    #   params_actual <- pmax(pmin(params,global_max[param_name]),global_min[param_name])
    #
    #   for (ipft in seq(1,length(params_actual))){
    #     config_simu[[ipft]][param_name] <- params_actual[ipft]
    #   }
    # }
    #
    # # Liana
    # config_simu[["Liana"]][names(pft_samples[["Liana"]])] <- pft_samples[["Liana"]]
    #
    # xml <- write.config.xml.ED2(defaults = defaults,
    #                             settings = settings,
    #                             trait.values = config_simu)
    #
    # XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
    #              prefix = PREFIX_XML)

    if (isimu == 1){
      isfirstjob = TRUE
      dir_joblauncher = run_ref
      list_dir[[run_name]] = run_ref
    } else{
      isfirstjob = FALSE
    }

    # job.sh
    write_joblauncher(file =  file.path(dir_joblauncher,"job.sh"),
                      nodes = 1,ppn = 18,mem = 16,walltime = 6,
                      prerun = "ml purge ; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                      CD = run_ref,date.init = "2011/03/01",
                      ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872",
                      ED2IN = "ED2IN",
                      firstjob = isfirstjob)

    if (isimu == Nsimuperjob){
      isimu = 0
    }
  }
}


dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")


# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/generate_ensemble_2patch.R hpc:/data/gent/vo/000/gvo00074/felicien/R

