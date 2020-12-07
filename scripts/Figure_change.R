rm(list = ls())

library(dplyr)
library(rhdf5)
library(tidyr)
library(ggplot2)
library(LianaRemoval)
library(cowplot)

source("~/Documents/ED2/R-utils/h5read_opt.r")

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_147/analy/analysis-Q-2010-12-00-000000-g01.h5",
                      "./outputs/control-2010-12.h5"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_147/analy/analysis-Q-2010-12-00-000000-g01.h5",
                      "./outputs/removal-2010-12.h5"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_147/analy/analysis-Q-2014-02-00-000000-g01.h5",
                      "./outputs/control-2014-02.h5"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_147/analy/analysis-Q-2014-02-00-000000-g01.h5",
                      "./outputs/removal-2014-02.h5"))

h5file_control <- "./outputs/control-2010-12.h5"
h5file_removal <- "./outputs/removal-2010-12.h5"
df_init <- summarise_treatment(h5file_control,h5file_removal)
df_init_pft <- df_init[[2]]

h5file_control_end <- "./outputs/control-2014-02.h5"
h5file_removal_end <- "./outputs/removal-2014-02.h5"
df_end <- summarise_treatment(h5file_control_end,h5file_removal_end)
df_end_pft <- df_end[[2]]

df_pft <- bind_rows(list(df_init_pft %>% mutate(time = "init"),
                         df_end_pft %>% mutate(time = "end"))) %>% pivot_wider(names_from = time,
                                                                               values_from = -c(simulation,pft))

LAIplot <-
  ggplot(data = df_pft,
       aes(x = LAIm_init,y = LAIm_end,
           color = as.factor(pft),shape = simulation)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = LAIm_end-LAIsd_end, ymax = LAIm_end+LAIsd_end),width =0) +
  geom_errorbar(aes(xmin = LAIm_init-LAIsd_init, xmax = LAIm_init+LAIsd_init),width =0) +
  geom_abline(slope = 1,intercept = 0,color = "black",linetype = 2) +
  scale_color_manual(values = c("#9FFF8C","#44CC29","#137300","#1E64C8"),
                    labels = c("Early","Mid","Late","Liana")) +
  labs(x = "Initial LAI",y = "Final LAI", shape = "Treatment", color = "PFT") +
  scale_shape_manual(values = c(16,12)) + guides(shape = FALSE, color = FALSE) +
  theme_bw() + theme(text = element_text(size = 24))

Bdplot <-
  ggplot(data = df_pft,
       aes(x = Bdm_init,y = Bdm_end,
           color = as.factor(pft),shape = simulation)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Bdm_end-Bdsd_end, ymax = Bdm_end+Bdsd_end),width =0) +
  geom_errorbar(aes(xmin = Bdm_init-Bdsd_init, xmax = Bdm_init+Bdsd_init),width =0) +
  geom_abline(slope = 1,intercept = 0,color = "black",linetype = 2) +
  scale_color_manual(values = c("#9FFF8C","#44CC29","#137300","#1E64C8"),
                     labels = c("Early","Mid","Late","Liana")) +
  labs(x = "Initial Bd",y = "Final Bd", shape = "Treatment", color = "PFT") +
  scale_shape_manual(values = c(16,12)) +
  theme_bw() + theme(text = element_text(size = 24),
                     legend.position = c(0.2,0.8))

#########################################################################
Figure <- plot_grid(LAIplot,Bdplot,nrow = 1, rel_widths = c(1,1))
Figure

ggsave(plot = last_plot(),
       filename = "./Figures/Figure_change.png",width = 12,height = 10,dpi = 300)


