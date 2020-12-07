rm(list = ls())

library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

# system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000011137/control_climate.RData","./outputs/"))
# system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000011137/control.RData","./outputs/"))
#
# system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000011137/removal_climate.RData","./outputs/"))
# system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000011137/removal.RData","./outputs/"))

load("./outputs/control_climate.RData")
control_climate <- datum
load("./outputs/control.RData")
control <- datum
load("./outputs/removal_climate.RData")
removal_climate <- datum
load("./outputs/removal.RData")
removal <- datum

NEP.df <-
  bind_rows(list(data.frame(year = datum$year,
                      month = datum$month,
                      value = control_climate$emean$nep,
                      climate = "constant",
                      treatment = "control"),
           data.frame(year = datum$year,
                      month = datum$month,
                      value = control$emean$nep,
                      climate = "variable",
                      treatment = "control"),
           data.frame(year = datum$year,
                      month = datum$month,
                      value = removal_climate$emean$nep,
                      climate = "constant",
                      treatment = "removal"),
           data.frame(year = datum$year,
                      month = datum$month,
                      value = removal$emean$nep,
                      climate = "variable",
                      treatment = "removal"))) %>% mutate(time = year + (month-1)/12)

GPP.df <-
  bind_rows(list(data.frame(year = datum$year,
                            month = datum$month,
                            value = control_climate$emean$gpp,
                            climate = "constant",
                            treatment = "control"),
                 data.frame(year = datum$year,
                            month = datum$month,
                            value = control$emean$gpp,
                            climate = "variable",
                            treatment = "control"),
                 data.frame(year = datum$year,
                            month = datum$month,
                            value = removal_climate$emean$gpp,
                            climate = "constant",
                            treatment = "removal"),
                 data.frame(year = datum$year,
                            month = datum$month,
                            value = removal$emean$gpp,
                            climate = "variable",
                            treatment = "removal"))) %>% mutate(time = year + (month-1)/12)

NEP.df.agg <- NEP.df %>% group_by(treatment,climate,year) %>% summarise(value = mean(value)) %>% filter(!(year %in% c(2011,2021)))
GPP.df.agg <- GPP.df %>% group_by(treatment,climate,year) %>% summarise(value = mean(value)) %>% filter(!(year %in% c(2011,2021)))

saveRDS(object = NEP.df.agg,file = "./outputs/NEP.test.RDS")
saveRDS(object = GPP.df.agg,file = "./outputs/GPP.test.RDS")

NEP.df.agg %>% group_by(treatment,climate) %>% summarise(SD = sd(value))

ggplot(data = NEP.df.agg) +
  geom_line(aes(x = year, y = value, color = climate)) +
  facet_wrap(~ treatment) +
  geom_hline(yintercept = 0,linetype = 3) +
  theme_bw()



