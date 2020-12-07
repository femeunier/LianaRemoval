rm(list = ls())

library(dplyr)
library(ggplot2)

system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/AGB_dynamics.RDS",
                    "/home/femeunier/Documents/projects/LianaRemoval/outputs"))

AGB_dynamics <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/AGB_dynamics.RDS") %>% mutate(time = year + month/12)

OP.posterior.Cpools <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/OP.posterior.Cpools.RDS")
runs2keep <- OP.posterior.Cpools %>% filter((pft == 18 & var == "agb" & time == "After 10 years" & stat == "mean" &
                                               treatment == "Control" & value >= 9.6)) %>% pull(irun)


AGB_dynamics.agg <- AGB_dynamics %>% filter(irun %in% (runs2keep+1)) %>% group_by(time,treatment) %>% summarise(agb_m = mean(agb),
                                                                           agb_sd = sd(agb))

ggplot(data = AGB_dynamics.agg,
       aes(x = time,y = agb_m,color = as.factor(treatment),fill = as.factor(treatment),
           ymin = agb_m - agb_sd,ymax = agb_m + agb_sd)) +
  geom_line() +
  geom_ribbon(alpha = 0.5,color = NA) +
  theme_bw()


#####################################################################################


system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/NEP_dynamics.RDS",
                    "/home/femeunier/Documents/projects/LianaRemoval/outputs"))

NEP_dynamics <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/NEP_dynamics.RDS") %>% mutate(time = year + month/12)

OP.posterior.Cpools <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/OP.posterior.Cpools.RDS")
runs2keep <- OP.posterior.Cpools %>% filter((pft == 18 & var == "agb" & time == "After 10 years" & stat == "mean" &
                                               treatment == "Control" & value >= 9.6)) %>% pull(irun)

NEP_dynamics.agg <- NEP_dynamics %>% group_by(time,treatment) %>% summarise(nep_m = mean(nep),
                                                                                                                nep_sd = sd(nep))

ggplot(data = NEP_dynamics.agg,
       aes(x = time,y = nep_m,color = as.factor(treatment),fill = as.factor(treatment),
           ymin = nep_m - nep_sd,ymax = nep_m + nep_sd)) +
  geom_line() +
  geom_ribbon(alpha = 0.5,color = NA) +
  theme_bw()

NEP_dynamics.agg %>% group_by(treatment) %>% summarise(m = mean(nep_m))
