rm(list = ls())

library(ggplot2)
library(cowplot)
library(dplyr)
library(albedo)
library(pracma)
library(reshape2)
library(purrr)
library(tidyr)
library(LianaRemoval)

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_17/analy/analysis.RData",
#                       "./outputs/control_early.RData"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_17/analy/analysis.RData",
#                       "./outputs/removal_early.RData"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control.RData",
                      "./outputs/control_long.RData"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removall.RData",
                      "./outputs/removal_long.RData"))

Vars = c("fast.soil.c","struct.soil.c","slow.soil.c")
# Vars = c("het.resp")

load("./outputs/control_long.RData")
control <- datum2df(datum, vars = Vars,  pfts = c(2, 3, 4, 17), name = "control")
control.swc <- datum$emean$soil.water[,16]
control.st <- datum$emean$soil.temp[,16]

load("./outputs/removal_long.RData")
removal <- datum2df(datum, vars = Vars,  pfts = c(2, 3, 4, 17), name = "removal")
removal.swc <- datum$emean$soil.water[,16]
removal.st <- datum$emean$soil.temp[,16]

all <- bind_rows(list(control,removal)) %>% mutate(time = year + month/12)


levels(all$var) <- c("FSC","StructSC","SSC")

ggplot(data = bind_rows(list(all,
                             data.frame(var = 'FSC',value = NA,time = 2010,pft = 18, simulation = "control",
                                        month = 1,year = 2010)))) +
  geom_line(aes(x = time + 3/12, y = value, color = as.factor(simulation))) +
  facet_wrap(~ var, scales = "free") +
  labs(x = "",y = "Carbon stocks [kgC/m²]",color = "Treatment") +
  scale_color_manual(values = c("#1E64C8", "#137300")) +
  scale_x_continuous(breaks = seq(2012,2022,2)) +
  theme_bw() + theme(text = element_text(size = 16),
                     legend.position = c(0.55,0.86),
                     panel.spacing = unit(2, "lines"),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

ggsave(plot = last_plot(), dpi = 300,height = 15,width = 30,units = "cm",
       filename = file.path(".","Figures","carbon.pools.png"))

all.month <- all %>% group_by(simulation,var,month) %>% summarise(value_m = mean(value),
                                                                  value_sd = sd(value))
ggplot(data = all.month %>% filter(var == Vars[1])) +
  geom_line(aes(x = month, y = value_m, color = as.factor(simulation))) +
  geom_ribbon(aes(x = month,
                  ymin = value_m - value_sd,
                  ymax = value_m + value_sd, fill = as.factor(simulation)),
              color = NA,alpha = 0.4) +
  facet_wrap(~ var, scales = "free") +
  labs(x = "",
       y = "Carbon flux [kgC/m²/yr]",
       color = "Treatment",
       fill = "Treatment") +
  scale_color_manual(values = c("#1E64C8", "#137300")) +
  scale_fill_manual(values = c("#1E64C8", "#137300")) +
  scale_x_continuous(
    breaks = seq(1:12),
    labels = c("J", "F", "M", "A", "M", "J",
               "J", "A", "S", "O", "N", "D")
  ) +
  theme_bw() + theme(text = element_text(size = 16),
                     legend.position = c(0.1,0.8))

ggsave(plot = last_plot(), dpi = 300,height = 15,width = 20,units = "cm",
       filename = file.path(".","Figures","seasonal.resp.png"))

#
# plot(control.swc,type = 'l',ylim = c(0,0.4))
# lines(removal.swc,col = "red")
#
# plot(control.st,type = 'l')
# lines(removal.st,col = "red")
#
# fcold = 0.24; Tcold = 291.15 ; fhot = 12; Thot = 318.15;
# fdry = 0.6; thetadry = 0.48; fwet = 36; thetawet = 0.98;
#
# t = 273.15+seq(0,50)
# espi_T = 1/((1+ exp(-fcold*(t - Tcold)))*(1 + exp(fhot*(t - Thot))))
# plot(t - 273.15,espi_T,type = 'l')
#
# theta = seq(0,1,length.out = 1000)
# espi_theta = 1/((1+ exp(-fdry*(theta - thetadry)))*(1 + exp(fwet*(theta - thetawet))))
# plot(theta,espi_theta,type = 'l')
