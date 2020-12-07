rm(list = ls())

library(stringr)
library(purrr)
# library(LianaRemoval)
library(dplyr)
library(PEcAn.ED2)
library(ggplot2)

get_ED_default_pft <- function(xml, var, pft_target = NULL){

  history_xml <- XML::xmlParse(xml)
  history_list <- XML::xmlToList(history_xml)

  if (!is.null(pft_target)){
    pft <- 0
    while (pft != pft_target){
      pft <- history_list %>% .[["pft"]] %>% .[["num"]]  %>% str_replace("/n", "") %>% str_trim() %>% as.numeric()
      if (pft != pft_target){
        history_list$pft <- NULL
      }
    }
  }

  out <- history_list %>% .[["pft"]] %>% .[[var]] %>% str_replace("/n", "") %>% str_trim() %>% as.numeric()

  return(out)
}

datum2df_patch <- function(datum, vars = c("gpp","npp","agb.change","lai.change","agb","lai"),patches){

  df_sum2 <- data.frame()
  patch_types <- unique(patches)

  for (patch_type in seq(1,length(patch_types))){
    cpatch <- which(patches == patch_types[patch_type])
    for (ivar in seq(1,length(vars))){

      cvar <- vars[ivar]
      if (cvar == "agb.change"){
        cvar = "agb"
      } else if (cvar == "lai.change"){
        cvar = "lai"
      }

      dbh <- unlist(datum$cohort$dbh)
      pft <- c(unlist(datum$cohort$pft))
      if(cvar %in% c("agb","npp")){
        pos <- as.vector(which((pft == 17 & dbh >= 1) | (pft != 17 & dbh >= 10)))
        # pos <- as.vector(1:length(pft))
      } else {
        pos <- as.vector(1:length(pft))
      }

      ipa <- c(unlist(datum$cohort$ipa))[pos]
      pa_area <- c(unlist(datum$cohort$area))[pos]
      nplant <- c(unlist(datum$cohort$nplant)[pos]/pa_area)
      values <- c(unlist(datum$cohort[[cvar]]))[pos]
      pft <- pft[pos]
      dbh <- dbh[pos]

      if (vars[ivar] %in% c("lai","lai.change")){values <- values/nplant}

      Vnames <- names(values)
      years <- as.numeric(substr(Vnames,2,5))
      months <- as.numeric(substr(Vnames,7,8))

      temp_df <- data.frame(patch = ipa,pft,pa_area,values,nplant,years,months)

      cdf <- temp_df %>% filter(patch %in% cpatch) %>% group_by(pft,patch,years,months) %>% summarise(S=sum(values*nplant)) %>%
        rename(value = S) %>% ungroup() %>% mutate(patch_t = patch_types[patch_type],
                                                   var = vars[ivar])

      cdf_tot <- temp_df %>% filter(patch %in% cpatch) %>% group_by(patch,years,months) %>% summarise(S=sum(values*nplant)) %>%
        rename(value = S) %>% ungroup() %>% mutate(patch_t = patch_types[patch_type],
                                                   var = vars[ivar],
                                                   pft = 18)

      if (vars[ivar] %in% c("agb.change","lai.change")){
        cdf <- cdf %>% group_by(pft,patch) %>% mutate(value = value - mean(value[c(1)]))
        cdf_tot <- cdf_tot %>% group_by(patch) %>% mutate(value = value - mean(value[c(1)]))
      }

      df_sum2 <- bind_rows(list(df_sum2,cdf,cdf_tot))

    }
  }

  return(df_sum2)
}

remote.dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run/runs"
local.file <- file.path(remote.dir,"all_jobs.sh")

all_job.file <- readLines(local.file)[seq(2,length(readLines(local.file)),2)]

vars <- c("gpp","npp","agb.change","lai.change","agb","lai")
patches <- c(rep("control",8),rep("removal",8))

# Data
tinit = 2011+3/12
t = seq(tinit,2014,length.out = 34)
Bcon <- (108.6 - 25.6*(exp(-0.0162246*(t-tinit))))/10
Brem <- (166.5 -83.5*(exp(-0.03574508*(t-tinit))))/10

Bcon <- (108.6 + 6.7 - 25.6*(exp(-0.0162246*(t-tinit))))/10
Brem <- (166.5 - 4 -83.5*(exp(-0.03574508*(t-tinit))))/10

Delta_Bcon <- Bcon - Bcon[1]
Delta_Brem <- Brem - Brem[1]
Delta_obs <- c(Delta_Bcon,Delta_Brem)

all_OP <- all_params <- QoF <- ensemble <- data.frame()

simus2check <- seq(1,length(all_job.file))
simus2check <- simus2check[!(simus2check %in% c(0) | simus2check>250)]

for (current in seq(1,length(simus2check))){

  idir <- simus2check[current]
  # Outputs
  dir_temp <- substr(all_job.file[idir],4,nchar(all_job.file[idir]))
  dir_name <- file.path(remote.dir,basename(dir_temp))

  ED2INfile <- file.path(dir_name,"ED2IN")
  if(file.exists(ED2INfile)){
    ED2IN <- read_ed2in(ED2INfile)
    opdir <- ED2IN$FFILOUT
    datum.file <- paste0(opdir,".RData")
  } else {
    next
  }



  if (file.exists(datum.file)) {load(datum.file)
  } else {
    next
  }
  temp_df <- datum2df_patch(datum,vars,patches) %>% mutate(simu = basename(dir_name),
                                                           isimu = idir) %>% filter(years <2014)


  all_OP <- bind_rows(list(all_OP,
                           temp_df))

  # config file
  config.file <- file.path(dir_name,"config.xml")

  cparam <- data.frame(values = c(modify(c(2,3,4,17),get_ED_default_pft,xml = config.file,var = "Vm0"),
                                  modify(c(2,3,4,17),get_ED_default_pft,xml = config.file,var = "clumping_factor")),
                       pft = c("Tree","Liana"),
                       parameter = c(rep("Vm0",2),rep("clumping_factor",2))) %>%
    mutate(simu = as.character(basename(dir_name)),
           isimu = idir)

  all_params <- bind_rows(list(all_params,
                               cparam))

  # Test quality of fit
  Sim <- temp_df %>% filter(var == "agb.change",pft == 18) %>% group_by(patch_t,years,months) %>%
    summarise(V = mean(value)) %>% pull(V)
  Delta_sims <- Sim

  ensemble <- bind_rows(list(ensemble,
                             data.frame(Sim,patch_t = c(rep("control",length(Delta_sims)/2),
                                                        rep("removal",length(Delta_sims)/2)),
                                        obs = Delta_obs,
                                        ensemble = current)))

  RMSE <- sqrt(sum((Delta_sims - Delta_obs)**2)/length(Delta_sims))
  QoF <- bind_rows(list(QoF,
                        data.frame(RMSE,
                                   simu = as.character(basename(dir_name)),
                                   isimu = idir)))
}


saveRDS(object = list(ensemble,
                      QoF,
                      all_OP,
                      all_params),file = "output_removal_ensemble.RDS")

# scp /home/femeunier/Documents/projects/LianaRemoval/scripts/analyze_ensemble_remote.R hpc:/data/gent/vo/000/gvo00074/felicien/R


