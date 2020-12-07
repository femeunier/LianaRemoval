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

# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_17/analy/analysis.RData","./outputs/removal.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_17/analy/analysis.RData","./outputs/control.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control.RData","./outputs/control.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removal.RData","./outputs/removal.RData"))

Vars = c("gpp","npp","lai","agb","plant.resp")
tinit = 2011 + 2L/12L
tend = 2024 + 2L/12L

load("./outputs/control.RData")
control <- LianaRemoval::datum2df_patch(datum, vars = Vars, patches = "uniform")

load("./outputs/removal.RData")
removal <- LianaRemoval::datum2df_patch(datum,vars = Vars, patches = "uniform")

simulations <- bind_rows(list(control %>% mutate(simulation = "control"),
                              removal %>% mutate(simulation = "removal"))) %>% rename(year = years,
                                                                                      month = months) %>% mutate(time = year + month/12)

simulations <-  simulations %>% mutate(GF = case_when(pft == 17 ~ "Liana",
                                                      pft == 18 ~ "Ecosystem",
                                                      TRUE ~ "Tree")) %>%
  complete(var = Vars,
           patch = 1:8,
           simulation = c("removal"),
           time = unique(simulations %>% filter(time >= tinit & time <= tend) %>% pull(time)),
           year = unique(simulations %>% filter(time >= tinit & time <= tend) %>% pull(year)),
           month = unique(simulations %>% filter(time >= tinit & time <= tend) %>% pull(month)),
           GF = c("Liana"),
           fill = list(value = 0,pa_area = 1,pft = 17))

simulations.sum <- simulations %>% group_by(var,patch,time,GF,simulation) %>% summarise(value = sum(value,na.rm = TRUE),
                                                                                        pa_area = mean(pa_area))

pvals <- simulations.sum %>% group_by(var,GF) %>% filter(time >= tinit & time <= tend) %>% summarise(
  y = max(value),
  m1 = weighted.mean(value[simulation == "control"],pa_area[simulation == "control"]),
  m2 = weighted.mean(value[simulation == "removal"],pa_area[simulation == "removal"]),
  s1 = sqrt(sum(pa_area[simulation == "control"] * (value[simulation == "control"] - m1)^2)),
  s2 = sqrt(sum(pa_area[simulation == "removal"] * (value[simulation == "removal"] - m2)^2)),
  N1 = length(value[simulation == "control"]),
  N2 = length(value[simulation == "removal"]),
  p.val = t.test2(m1,m2,
                  s1,s2,
                  N1,N2),
  x = 1.5) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                         p.val <= 0.10 ~ "*",
                                         TRUE ~ ""))

ggplot() +
  geom_boxplot(data = simulations.sum %>% filter(time >= tinit & time <= tend,
                                                 patch %in% seq(1,8)),
               aes(x = simulation,y = value,fill = as.factor(GF)),alpha = 0.6) +
  geom_text(data = pvals,
            aes(label = signif, y = y, x = x),size = 10) +
  facet_grid(var ~ as.factor(GF),scales = "free") +
  geom_hline(yintercept = 0,linetype = 3) +
  scale_fill_manual(values = c("darkgrey","darkblue","darkgreen")) +
  theme_bw() +
  labs(x = "", y = "", fill = "Growth form") + guides(fill = FALSE) +
  scale_y_continuous(expand = c(0,0,0.2,0)) +
  theme(text = element_text(size = 24))

simulations.sum %>% filter(time >= tinit & time <= tend,
                           patch %in% seq(1,8)) %>% ungroup() %>% group_by(var,GF,simulation) %>% summarise(m = mean(value),
                                                                                                            sd_v = sd(value),
                                                                                                            se = sd(value)/sqrt(length(value))) %>%
  filter(var %in% c("gpp","npp","plant.resp")) %>% arrange(simulation)


ggsave(plot = last_plot(),
       filename = "./Figures/Figure_fluxes.png",width = 15,height = 15,dpi = 300)


simulations.sum %>% filter(var == "gpp", GF == "Tree",time %in% unique(simulations.sum$time)[c(1,36)]) %>% group_by(simulation) %>%
  summarise(m = mean(value))
