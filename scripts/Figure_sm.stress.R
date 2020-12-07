rm(list = ls())

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(LianaRemoval)

system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/OP.posterior.smstress.RDS",
                    "/home/femeunier/Documents/projects/LianaRemoval/outputs"))
sm.stress <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/OP.posterior.smstress.RDS")


Bdead.agg <- sm.stress %>% filter(var == "bdead") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

agb.agg <- sm.stress %>% filter(var == "agb") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

agb.agg %>% group_by(pft,simulation) %>% filter(time > 2021.16) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                            TRUE ~ m)) %>%
  pivot_wider(names_from = simulation,
              values_from = m) %>% mutate(diff =
                                            removal-control,
                                          diff_rel = diff/control)


Bdead.agg %>% ungroup() %>% filter(time == max(time)) %>% mutate(m = 0.7*m)


sm.stress.agg <- sm.stress %>% filter(var == "sm.stress") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

sm.stress.agg %>% group_by(pft,simulation) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                      TRUE ~ m)) %>%
  pivot_wider(names_from = simulation,
              values_from = m) %>% mutate(diff =
                                            removal-control,
                                          diff_rel = diff/control)

psi.agg <- sm.stress %>% filter(var == "leaf.psi.md") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

psi.agg %>% group_by(pft,simulation) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                       TRUE ~ m)) %>%
  pivot_wider(names_from = simulation,
              values_from = m) %>% mutate(diff =
                                            removal-control,
                                          diff_rel = diff/control)

psi.agg2 <- sm.stress %>% filter(var == "leaf.psi.pd") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

psi.agg %>% group_by(pft,simulation) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                      TRUE ~ m)) %>%
  pivot_wider(names_from = simulation,
              values_from = m) %>% mutate(diff =
                                            removal-control,
                                          diff_rel = diff/control)


transp  %>% group_by(pft,simulation) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                      TRUE ~ m)) %>%
  pivot_wider(names_from = simulation,
              values_from = m) %>% mutate(diff =
                                            removal-control,
                                          diff_rel = diff/control)



het.resp <- sm.stress %>% filter(var == "het.resp") %>% group_by(pft,month,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

leaf.par <- sm.stress %>% filter(var == "leaf.par") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

leaf.par %>% mutate(m = case_when(is.na(m) ~ 0,
                                  TRUE ~ m)) %>% ungroup() %>% group_by(pft,time) %>% summarise(change = m[simulation == "removal"] -  m[simulation == "control"],
                                                                                                change.rel = change/m[simulation == "control"]) %>%
  group_by(pft) %>% summarise(m = mean(change),
                              mrel = mean(change.rel))





et <- sm.stress %>% filter(var == "et") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

et %>% mutate(m = case_when(is.na(m) ~ 0,
                            TRUE ~ m)) %>% ungroup() %>% group_by(pft,time) %>% summarise(change = m[simulation == "removal"] -  m[simulation == "control"],
                                                                                          change.rel = change/m[simulation == "control"]) %>%
  group_by(pft) %>% summarise(m = mean(change),
                              mrel = mean(change.rel))

nplant <- sm.stress %>% filter(var == "nplant") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))


lai <- sm.stress %>% filter(var == "lai") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

gpp <- sm.stress %>% filter(var == "gpp") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

gpp %>% group_by(pft,simulation) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                       TRUE ~ m)) %>%
  pivot_wider(names_from = simulation,
              values_from = m) %>% mutate(diff =
                                            removal-control,
                                          diff_rel = diff/control)

nplant %>% group_by(pft,simulation) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                  TRUE ~ m)) %>%
  pivot_wider(names_from = simulation,
              values_from = m) %>% mutate(diff =
                                            removal-control,
                                          diff_rel = diff/control)


ggplot(data = nplant,
       aes(x = time,y = m, color = as.factor(simulation), fill = as.factor(simulation),
           ymin = m - sd, ymax = m + sd)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_wrap(~ pft) +
  theme_bw()


ggplot(data = lai,
       aes(x = time,y = m, color = as.factor(simulation), fill = as.factor(simulation),
           ymin = m - sd, ymax = m + sd)) +
  geom_line() +
  geom_ribbon(alpha = 0.5,color = NA) +
  facet_wrap(~ pft) +
  theme_bw()

lai %>% filter(time > 2019.833,time < 2019.84,simulation == "control")

ratio_LAI <- lai %>% filter(pft %in% c(17,18)) %>% select(pft,time,simulation,m) %>% pivot_wider(names_from = simulation,
                                                                                    values_from = m) %>%
  pivot_wider(names_from = pft,
              values_from = c(control,removal)) %>% mutate(ratio = control_17/control_18) %>% pull(ratio)
mean(ratio_LAI[109:120])


sm.stress %>% filter(var == "lai",pft == 18) %>% pivot_wider(names_from = simulation,
                                                             values_from = value) %>% mutate(diff = control - removal) %>% filter(time > 2016.25 & time < 2017.25) %>% pull(diff) %>% mean()



plot(ratio_LAI)

ggplot(data = leaf.par,
       aes(x = time,y = m, color = as.factor(simulation), fill = as.factor(simulation),
           ymin = m - sd, ymax = m + sd)) +
  geom_line() +
  geom_ribbon(alpha = 0.5,color = NA) +
  facet_wrap(~ pft) +
  theme_bw()

tmp.par <- leaf.par %>% group_by(pft,simulation) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                       TRUE ~ m)) %>%
                                                                                           pivot_wider(names_from = simulation,
                                                                                                       values_from = m) %>% mutate(diff =
                                                                                                                                     removal-control,
                                                                                                                                   diff_rel = diff/control)
tmp.lai <-  lai %>% group_by(pft,simulation) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                                   TRUE ~ m)) %>%
  pivot_wider(names_from = simulation,
              values_from = m) %>% mutate(diff =
                                            removal-control,
                                          diff_rel = diff/control)


tot.tree <- weighted.mean(tmp.par %>% ungroup() %>% filter(pft != 17 & pft !=18) %>% select(control,removal) %>% pull(control),
              tmp.lai %>% ungroup() %>% filter(pft != 17 & pft !=18) %>% select(control,removal) %>% pull(control))

(tmp.par %>% filter(pft == 18) %>% pull(removal) - tot.tree)/tot.tree


ggplot(data = het.resp,
       aes(x = month,y = m, color = as.factor(simulation), fill = as.factor(simulation),
           ymin = m - sd, ymax = m + sd)) +
  geom_line() +
  geom_ribbon(alpha = 0.5,color = NA) +
  facet_wrap(~ pft) +
  theme_bw()

het.resp %>% group_by(simulation) %>% summarise(m = mean(m))

lai <- sm.stress %>% filter(var == "lai") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

transp <- sm.stress %>% filter(var == "transp") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

transp  %>% group_by(pft,simulation) %>% summarise(m = mean(m,na.rm = TRUE)) %>% mutate(m = case_when(is.na(m) ~ 0,
                                                                                                      TRUE ~ m)) %>%
  pivot_wider(names_from = simulation,
              values_from = m) %>% mutate(diff =
                                            removal-control,
                                          diff_rel = diff/control)

ggplot(data = transp,
       aes(x = time,y = m, color = as.factor(simulation), fill = as.factor(simulation),
           ymin = m - sd, ymax = m + sd)) +
  geom_line() +
  geom_ribbon(alpha = 0.5,color = NA) +
  facet_wrap(~ pft) +
  theme_bw()

par.gnd <- sm.stress %>% filter(var == "leaf.par") %>% group_by(pft,time,simulation) %>%
  summarise(m = mean(value,na.rm = T),
            sd = sd(value,na.rm = T),
            N = length(value))

ggplot(data = par.gnd,
       aes(x = time,y = m, color = as.factor(simulation), fill = as.factor(simulation),
           ymin = m - sd, ymax = m + sd)) +
  geom_line() +
  geom_ribbon(alpha = 0.5,color = NA) +
  facet_wrap(~ pft) +
  theme_bw()




sm.stress.rel <- sm.stress %>% filter(var == "leaf.par") %>% group_by(var,irun,pft) %>% mutate(change = c(value[simulation == "removal"] - value[simulation == "control"],
                                                                                                          value[simulation == "removal"] - value[simulation == "control"]),
                                                                                               rel_change = c(change/value[simulation == "control"])) %>% group_by(pft,time) %>%
  summarise(m = mean(rel_change,na.rm = T),
            sd = sd(rel_change,na.rm = T),
            N = length(rel_change))

ggplot(data = sm.stress.rel,
       aes(x = time,y = m,
           ymin = m - sd, ymax = m + sd)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_wrap(~ pft,scales = "free") +
  theme_bw()

