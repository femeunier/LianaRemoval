rm(list = ls())

library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run/runs"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out"

Nsimulations = 50
simu_name = "Test_pft_patch"
names_patch_t <- c("control","removal")

isimu = 0
list_dir <- list()

for(simu in seq(1,Nsimulations)){
  for (patch_type in seq(1,length(names_patch_t))){

  run_name <- paste0(simu_name,"_",names_patch_t[patch_type],"_",simu)

  isimu = isimu + 1

  run_ref <- file.path(rundir,run_name)

  if (isimu == 1){
    isfirstjob = TRUE
    dir_joblauncher = run_ref
    list_dir[[run_name]] = run_ref
  } else{
    isfirstjob = FALSE
  }

  write_joblauncher_Ronly(file =  file.path(dir_joblauncher,"jobR.sh"),
                          nodes = 1,ppn = 1,mem = 16,walltime = 12,
                          prerun = "ml purge ; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                          CD = run_ref,
                          ED2IN = "ED2IN",date.init = "2011/03/01",date.end = "2014/03/01",
                          Rplot_function = '/data/gent/vo/000/gvo00074/felicien/R/read_and_plot_ED2_Q2R_tspft.r',
                          firstjob = isfirstjob)
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"Rjobs.sh"),
                              list_files = list_dir,
                              job_name = "jobR.sh")

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/rerun_Rjobs.R hpc:/data/gent/vo/000/gvo00074/felicien/R
