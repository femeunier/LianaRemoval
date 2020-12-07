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

Nsimuperjob = 1
isimu = 0

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run"
ED2IN_files <- c(file.path(ref_dir,c("ED2IN_pft_long_control","ED2IN_pft_long_removal")))
names_patch_t <- c("control","removal")

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run/runs"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out"

simu_name = "Test_pft_patch"

# Clean
# system2("rm",c("-rf",paste0(file.path(rundir,simu_name),"*")))
# system2("rm",c("-rf",paste0(file.path(outdir,simu_name),"*")))

if(!dir.exists(rundir)) dir.create(rundir)
if(!dir.exists(outdir)) dir.create(outdir)

##############################################################################
# Default

PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"
defaults <- list_dir <- list()

# Default settings
settings <- list(model = list(revision = "git",
                              config.header = NULL),
                 pfts = list(pft = list(num = 2,
                                        ed2_pft_number = 2,
                                        name = "Early"),
                             pft = list(num = 3,
                                        ed2_pft_number = 3,
                                        name = "Mid"),
                             pft = list(num = 4,
                                        ed2_pft_number = 4,
                                        name = "Lata"),
                             pft = list(num = 17,
                                        ed2_pft_number = 17,
                                        name = "Liana")))

# Default config
config <- list()

config[["Early"]] <- unlist(list(num = 2,
                                 Vm0 = 12,
                                 mort3 = 0.008,
                                 clumping_factor = 0.4,
                                 leaf_turnover_rate = 1.25,
                                 b1Bl_large = 0.05,
                                 b2Bl_large = 1.8,
                                 b1Bl_small = 0.05,
                                 b2Bl_small = 1.8))

config[["Mid"]] <- unlist(list(num = 3,
                               Vm0 = 10.5,
                               mort3 = 0.006,
                               clumping_factor = 0.4,
                               leaf_turnover_rate = 0.9,
                               b1Bl_large = 0.05,
                               b2Bl_large = 1.8,
                               b1Bl_small = 0.05,
                               b2Bl_small = 1.8,
                               b1Bs_small = 0.2020226658911,
                               b1Bs_large = 0.20205297666222))

config[["Late"]] <- unlist(list(num = 4,
                                Vm0 = 7.5,
                                wood_Kmax = 0.01,
                                mort3 = 0.004,
                                clumping_factor = 0.4,
                                leaf_turnover_rate = 0.6,
                                b1Bl_large = 0.05,
                                b2Bl_large = 1.8,
                                b1Bl_small = 0.05,
                                b2Bl_small = 1.8,
                                b1Bs_small = 0.3407226658911,
                                b1Bs_large = 0.340297666222))


config[["Liana"]] <- unlist(
  list(
    num = 17,
    is_tropical = 1,
    leaf_turnover_rate = 1.27,
    b2Bs_large = 2.92,
    b1Bs_large = 0.3,
    b2Bs_small = 2.92,
    b1Bs_small = 0.3,
    dbh_crit =  30,
    rho = 0.462893312003502,
    wood_Kexp = 2.06151664261015,
    Vm0 = 29.0195095978388,
    wood_Kmax = 0.118592088619329,
    wood_water_cap = 0.00831146542859373*1000,
    wood_psi50 = 122.88209151827,
    growth_resp_factor = 0.352803405024027,
    SLA = 11 * 0.48,
    stoma_psi_b = 160.017481634853,
    root_respiration_factor = 0.280639319284819,
    SRA = 48.1711743548512,
    r_fract = 0.826262914185645,
    stomatal_slope = 10.4797428731951,
    root_beta = 0.0501418540509767,
    b1Bl_large = 0.09,
    b2Bl_large = 1.89,
    b1Bl_small = 0.09,
    b2Bl_small = 1.89,
    b1Ht = 0.11, #0.100034825515468,
    b2Ht = 2.5, #0.868131191794218,
    q = 0.994400362018496,
    mort2 = 15.3333587065344,
    root_turnover_rate = 1.27805201890461,
    stoma_psi_c = 2.9926889645867,
    dark_respiration_factor = 0.0279573623213031,
    quantum_efficiency = 0.057162389334215,
    mort3 = 0.01,
    leaf_psi_tlp = 204.690265902307,
    leaf_water_cap = 0.00189950774801228*100,
    seedling_mortality = 0.95,
    clumping_factor = 0.5))

##########################################################################################
# Small parametric space

pfts <- list("Tree","Liana")
pft_lowers <- list(Tree = c(clumping_factor = 0.45,delta_clumping_factor = 0.,
                            Vm0 = 7.5,Delta_Vm0 = 1),
                   Liana = c(clumping_factor = 0.4,Vm0 = 25))

pft_uppers <- list(Tree = c(clumping_factor_0 = 0.55,delta_clumping_factor = 0.,
                            Vm0_0 = 13.5,Delta_Vm0 = 3),
                   Liana = c(clumping_factor = 0.6,Vm0 = 32))

global_min <- c(clumping_factor = 0.4,Vm0 = 5)
global_max <- c(clumping_factor = 0.55,Vm0 = 13.5)

prior <- map2(pft_lowers,pft_uppers,createUniformPrior)

##########################################################################################

tf_drate <- runif(n = Nsimulations,min = 0.001,max = 0.02)

##########################################################################################

WT <- c(12,3)

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
    ed2in_scenar$TREEFALL_DISTURBANCE_RATE <- tf_drate[simu]

    write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

    # Config
    config_simu <- config

    # Sample
    pft_samples <- map(1:length(prior), function(i){
      samples <- prior[[i]]$sample()
      names(samples) <- names(pft_lowers[[i]])
      return(samples)
    }) %>% set_names(names(prior))

    # Tree
    for (i in seq(1,length(pft_samples[["Tree"]])/2)){
      param_name <- names(pft_samples[["Tree"]][(i-1)*2+1])
      param0 <- pft_samples[["Tree"]][(i-1)*2+1]
      delta_param <- pft_samples[["Tree"]][(i-1)*2+2]
      params <- param0 + delta_param*(((length(config)-1):1)-1)
      params_actual <- pmax(pmin(params,global_max[param_name]),global_min[param_name])

      for (ipft in seq(1,length(params_actual))){
        config_simu[[ipft]][param_name] <- params_actual[ipft]
      }
    }

    # Liana
    config_simu[["Liana"]][names(pft_samples[["Liana"]])] <- pft_samples[["Liana"]]

    xml <- write.config.xml.ED2(defaults = defaults,
                                settings = settings,
                                trait.values = config_simu)

    XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
                 prefix = PREFIX_XML)

    if (isimu == 1){
      isfirstjob = TRUE
      dir_joblauncher = run_ref
      list_dir[[run_name]] = run_ref
    } else{
      isfirstjob = FALSE
    }

    # job.sh
    write_joblauncher(file =  file.path(dir_joblauncher,"job.sh"),
                          nodes = 1,ppn = 18,mem = 16,walltime = WT[patch_type],
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


# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/generate_ensemble_patch.R hpc:/data/gent/vo/000/gvo00074/felicien/R

