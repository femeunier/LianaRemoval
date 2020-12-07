rm(list = ls())

library(stringr)
library(albedo)
library(purrr)
library(LianaRemoval)
library(dplyr)
library(ggplot2)

local.dir <- "~/Documents/projects/LianaRemoval/outputs/"
if(!dir.exists(local.dir)) dir.create(local.dir)

remote.dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/run"

file <- file.path(remote.dir,"all_jobs.sh")
system2("scp",c(paste0("hpc:",file),local.dir))

local.file <- file.path(local.dir,"all_jobs.sh")
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

all_OP <- all_params <- QoF <- data.frame()

simus2check <- seq(1,length(all_job.file))
simus2check <- simus2check[!(simus2check %in% c(0) | simus2check>100)]

for (current in seq(1,length(simus2check))){

  idir <- simus2check[current]
  # Outputs
  dir_temp <- substr(all_job.file[idir],4,nchar(all_job.file[idir]))
  dir_name <- file.path(local.dir,basename(dir_temp))
  datum.file <- file.path(dir_name,"analysis.RData")
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

  cparam <- data.frame(values = c(modify(c(3,17),get_ED_default_pft,xml = config.file,var = "Vm0"),
                                  modify(c(3,17),get_ED_default_pft,xml = config.file,var = "clumping_factor"),
                                  # modify(c(3,17),get_ED_default_pft,xml = config.file,var = "leaf_turnover_rate"),
                                  modify(c(3,17),get_ED_default_pft,xml = config.file,var = "mort3")),
                       pft = c("Tree","Liana"),
                       parameter = c(rep("Vm0",2),rep("clumping_factor",2),rep("mort3",2))) %>%
    mutate(simu = as.character(basename(dir_name)),
           isimu = idir)

  all_params <- bind_rows(list(all_params,
                               cparam))

  # Test quality of fit
  Delta_sims <- temp_df %>% filter(var == "agb.change",pft == 18) %>% group_by(patch_t,years,months) %>% summarise(V = mean(value)) %>% pull(V)

  RMSE <- sqrt(sum((Delta_sims - Delta_obs)**2)/length(Delta_sims))
  QoF <- bind_rows(list(QoF,
                        data.frame(RMSE,
                                   simu = as.character(basename(dir_name)),
                                   isimu = idir)))
}


df_sum_all <- all_OP %>% filter(pft == 18) %>%
  mutate(time = years + (months)/12) %>% group_by(simu,patch_t,time,var) %>%
  summarise(value_m = mean(value),
            value_low = value_m - 1.96*sd(value/sqrt(length(value))),
            value_high = value_m + 1.96*sd(value/sqrt(length(value)))) %>% ungroup()

df_sum <- df_sum_all #%>% dplyr::filter(simu %in% c(as.character(QoF %>% top_n(10,desc(RMSE)) %>% pull(simu))))

best_simu <- QoF %>% top_n(1,desc(RMSE)) %>% pull(simu)
QoF %>% arrange(desc(RMSE))
best_simu <- "Test92"
print(best_simu)

df_best <- df_sum %>% filter(simu %in% best_simu)

df_data <- data.frame(time = c(t,t),
                      B = c(Bcon,Brem),
                      type = c(rep("control",length(t)),rep("removal",length(t)))) %>% group_by(type) %>%
  mutate(Bchange = B - B[1])


best_params <- all_params %>% filter(simu == best_simu)

ggplot() +
  geom_line(data = df_data,
            aes(x = time,y = Bchange, group = as.factor(type)),color = "black") +
  geom_line(data = df_best %>% filter(var == "agb.change"),
            aes(x = time,y = value_m,color = patch_t,linetype = interaction(simu))) +
  geom_ribbon(data = df_best %>% filter(var == "agb.change"),
              aes(x = time,ymin = value_low,ymax = value_high,fill = interaction(simu,patch_t)),color = NA,alpha = 0.4) +
  facet_wrap(. ~ var,scales = "free") +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw()

df_sum_bis <- all_OP %>% filter(pft == 18,simu %in% best_simu) %>% mutate(time = years + (months)/12) %>% group_by(patch_t,time,var) %>%
  summarise(value_m = mean(value),
            value_low = value_m - 1.96*sd(value/sqrt(length(value))),
            value_high = value_m + 1.96*sd(value/sqrt(length(value)))) %>% ungroup()

ggplot(data = df_sum_bis) +
  geom_line(aes(x = time,y = value_m,color = as.factor(patch_t))) +
  geom_ribbon(aes(x = time,ymin = value_low,ymax = value_high,fill = as.factor(patch_t)),color = NA,alpha = 0.4) +
  facet_wrap(. ~ var,scales = "free") +
  theme_bw()


all_OP %>% filter(var == "agb",years == 2011,months == 3,simu == best_simu) %>% group_by(patch_t,pft) %>% summarise(M = mean(value))


df_sum_tris <- all_OP %>% filter(pft == 18) %>%
  mutate(time = years + (months)/12) %>% filter(simu == best_simu)

ggplot() +
  geom_line(data = df_data,
            aes(x = time,y = Bchange, col = as.factor(type))) +
  geom_line(data = df_sum_bis %>% filter(var == "agb.change"),
            aes(x = time,y = value_m,color = patch_t),linetype = 1,size=2) +
  geom_line(data = df_sum_tris %>% filter(var == "agb.change"),
            aes(x = time,y = value,color = patch_t,group=interaction(patch_t,patch)),linetype = 2) +
  # geom_line(data = df_patches %>% filter(var == "agb.change",patch %in% c(3,5,7)),
  # aes(x = time,y = value,color = patch_t,group=interaction(patch_t,patch)),linetype = 2) +
  # scale_y_continuous(limits=c(0,1)) +
  # scale_x_continuous(limits=c(2011,2015)) +
  facet_wrap(. ~ var,scales = "free") +
  theme_bw()

