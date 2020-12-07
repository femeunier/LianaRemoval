rm(list = ls())

library(dplyr)
library(albedo)
library(reshape2)
library(ggplot2)
library(pracma)
library(LianaRemoval)

# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_147/analy/analysis.RData","./outputs/removal.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_147/analy/analysis.RData","./outputs/control.RData"))
system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000010700/control.RData","./outputs/control.RData"))
system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/99000010700/removal.RData","./outputs/removal.RData"))

load("./outputs/control.RData")
control <- LianaRemoval::datum2df_patch(datum,vars=c("gpp","agb"),patches = "uniform") %>% mutate(simulation = "control")

load("./outputs/removal.RData")
removal <- LianaRemoval::datum2df_patch(datum,vars=c("gpp","agb"),patches = "uniform") %>% mutate(simulation = "removal")

simulations <- bind_rows(list(control,
                              removal)) %>% mutate(time = years + months/12)

simulations_deltaAGB.sum <-
  simulations %>% filter(var == "agb",
                         pft == 18) %>% group_by(var,simulation,time,years,months) %>%
  summarise(value_m = sum(pa_area*value)) %>% ungroup() %>% group_by(simulation) %>%
  mutate(value_m = value_m - value_m[1],
         var = "agb.change") %>% left_join(
           simulations %>% filter(var == "agb",
                       pft == 18) %>% group_by(var,simulation,patch) %>%
  mutate(value = value - value[1]) %>% group_by(var,simulation,time) %>% summarise(wm = weighted.mean(value, pa_area),
                                                                                   ws = sum(pa_area * (value - wm)^2),
                                                                                   N = length(pa_area),
                                                                                   we = ws/sqrt(N)) %>% select(var,simulation,time,ws,we) %>% ungroup() %>%
    mutate(var = "agb.change"))

# simulations_deltaAGB.sum <- simulations_deltaAGB %>% group_by(var,time,years,months,simulation) %>% summarise(value_m = weighted.mean(value,w = pa_area),
#                                                                                                               value_sd = sd(value),
#                                                                                                               N = length(value),
#                                                                                                               value_se = sd(value)/sqrt(length(value)))
# ggplot(data = temp) +
#   geom_line(aes(x = time, y = S*10, color = simulation))


# Data
tinit = 2011+3/12
tend = 2014 + 2/12
t = seq(tinit,
        tend,
        length.out = 1000)

Bcon = (108.6 - 25.6*(exp(-0.0162246*(t-tinit))))
Brem = 166.5 - 83.5*(exp(-0.03574508*(t-tinit)))

df_data <- data.frame(time = c(t,t),
                      B = c(Bcon,Brem),
                      Bchange = c(Bcon-Bcon[1],Brem-Brem[1]),
                      type = c(rep("control",length(t)),rep("removal",length(t))))

ggplot() +
  geom_line(data = simulations_deltaAGB.sum,
            aes(x = time, y = value_m*10, color = simulation), linetype = 2) +
  geom_ribbon(data = simulations_deltaAGB.sum,
            aes(x = time, ymin = 10*(value_m - ws), ymax = 10*(value_m + ws), fill = simulation), alpha = 0.4) +
  geom_line(data = df_data,
            aes(x = time, y = Bchange, color = type), linetype = 1) +
  # geom_line(data = data.frame(time = datum$year + datum$month/12, Bchange = 10*(datum$emean$agb - datum$emean$agb[1]),type = "removal"),
  #           aes(x = time, y = Bchange, color = type), linetype = 1) +
  # scale_x_continuous(limits = c(2011,2031)) +
  # scale_y_continuous(limits = c(-1,50)) +
  scale_fill_manual(values = c("#1E64C8","#137300")) +
  scale_color_manual(values = c("#1E64C8","#137300")) +
  labs(x = "",y = "Biomass change [Mg C/ha]",fill = "Treatment",color = "Treatment") +
  geom_hline(yintercept = 0, color = "black",linetype = 3) +
  theme_bw() +
  theme(legend.position = c(0.15,0.9),
        text = element_text(size = 24))

ggsave(plot = last_plot(),
       filename = "./Figures/Bchange.png",width = 15,height = 10,dpi = 300)
