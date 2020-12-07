rm(list = ls())

library(dplyr)
library(albedo)
library(reshape2)
library(ggplot2)
library(pracma)

# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_short_control.RData","./outputs/"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_short_removal.RData","./outputs/"))
#
# load("./outputs/Gigante_pft_short_control.RData")
# control <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "control")
#
# load("./outputs/Gigante_pft_short_removal.RData")
# removal <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "removal")

# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/control_long/analy/analysis.RData","./outputs/control_long.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/removal_long/analy/analysis.RData","./outputs/removal_long.RData"))
#
# load("./outputs/control_long.RData")
# control <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "control")
#
# load("./outputs/removal_long.RData")
# removal <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "removal")
#
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_17/analy/analysis.RData","./outputs/removal.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_17/analy/analysis.RData","./outputs/control.RData"))

load("./outputs/control.RData")
control <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "control")

load("./outputs/removal.RData")
removal <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "removal")

simulations <- bind_rows(list(control,
                              removal))

simulations <-  bind_rows(list(simulations,
                               simulations%>% group_by(var,simulation,month,year,time) %>% summarise(value = sum(value,na.rm = TRUE)) %>% mutate(pft = 18))) %>%
  arrange(simulation,var,time)

simulations <- bind_rows(list(simulations,
                              simulations %>% filter(var == "agb",
                                                     pft == 18) %>% group_by(simulation) %>% mutate(var = "agb.change",
                                                                                                       value = value - value[1])))


tinit = 2011+3/12
t = seq(tinit,
        simulations %>% pull(year) %>%max(),
        length.out = 1000)

Bcon = (108.6 - 25.6*(exp(-0.0162246*(t-tinit))))
Brem = 166.5 - 83.5*(exp(-0.03574508*(t-tinit)))

df_data <- data.frame(time = c(t,t),
                      B = c(Bcon,Brem),
                      Bchange = c(Bcon-Bcon[1],Brem-Brem[1]),
                      type = c(rep("control",length(t)),rep("removal",length(t))))


simulations_deltaAGB <-
  simulations %>% filter(var == "agb.change", pft == 18) %>% mutate(time = year + (month) / 12, value = value * 10) %>% group_by(simulation) %>%
  mutate(agb.change.lin = linspace(value[1],
                                   value[length(value)],
                                   length(value)))

ggplot() +
  geom_line(data = simulations_deltaAGB,
            aes(x = time, y = value, color = simulation), linetype = 2) +
  geom_line(data = simulations_deltaAGB,
            aes(x = time, y = agb.change.lin, color = simulation), linetype = 3) +
  geom_line(data = df_data,
            aes(x = time, y = Bchange, color = type), linetype = 1) +
  # scale_x_continuous(limits = c(2011,2031)) +
  # scale_y_continuous(limits = c(-1,50)) +
  theme_bw()

# ggplot() +
#   geom_line(data = simulations %>% filter(var == "agb",pft == 18),
#             aes(x = year + (month-1)/12, y = value*10, color = simulation), linetype = 2) +
#   geom_line(data = df_data,
#             aes(x = time, y = B, color = type), linetype = 1) +
#   theme_bw()


