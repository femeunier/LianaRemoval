rm(list = ls())

library(dplyr)
library(ggplot2)

# system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/OP.posterior.runs.RDS",
#                     "/home/femeunier/Documents/projects/LianaRemoval/outputs"))

OP.posterior.runs <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/OP.posterior.runs.RDS")

OP.posterior.runs.year <- OP.posterior.runs %>% group_by(var,simulation,irun,GF) %>% summarise(value = mean(value)) %>% ungroup()

p.vals <- OP.posterior.runs.year %>%
  group_by(var,GF) %>% summarise(p.val = kruskal.test(formula = value ~ as.factor(simulation))$p.value) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                                                                      p.val <= 0.10 ~ "*",
                                                                                                                                      TRUE ~ ""))
OP.posterior.runs.year %>% group_by(var,GF,simulation) %>% summarise(value_m = mean(value,na.rm = T),
                                                                     value_sd = sd(value,na.rm = T),
                                                                     value_se = sd(value,na.rm = T)/sqrt(length(value)),
                                                                     value_high = confint(lm(formula = value ~1))[1,2],
                                                                     value_low = confint(lm(formula = value ~1))[1,1]) %>%
  left_join(p.vals)

ggplot(data = OP.posterior.runs.year,
       aes(x = as.factor(simulation),y = value)) +
  geom_boxplot() +
  facet_grid(GF ~ var) +
  theme_bw()

OP.posterior.runs.year %>% group_by(simulation,GF,var) %>% summarise(m = mean(value,na.rm = T)) %>% filter(var == "nep")

datanep <- OP.posterior.runs %>% filter(var == "nep") %>% group_by(simulation,year.num)%>% summarise(m = mean(value))
datanep %>% filter(year.num > 1) %>% pivot_wider(names_from = simulation,
                        values_from = m) %>% mutate(diff = control - removal) %>% pull(diff) %>% max()

ggplot(data = datanep,
       aes(x = year.num,color = as.factor(simulation),y = m)) +
  geom_line() +
  theme_bw()

saveRDS(object = datanep,file = "./outputs/NEP.RDS")

datagpp <- OP.posterior.runs %>% filter(var == "gpp",GF == "Ecosystem") %>% group_by(simulation,year.num)%>% summarise(m = mean(value,na.rm = TRUE))

datagpp %>% pivot_wider(names_from = simulation,
                        values_from = m) %>% mutate(diff = control - removal) %>% pull(diff) %>% mean()

ggplot(data = datagpp,
       aes(x = year.num,color = as.factor(simulation),y = m)) +
  geom_line() +
  theme_bw()

datagpp <- OP.posterior.runs %>% filter(var == "npp",GF == "Ecosystem") %>% group_by(simulation,year.num)%>% summarise(m = mean(value,na.rm = TRUE)) %>% pull(m)
datagppliana <- OP.posterior.runs %>% filter(var == "npp",GF == "Liana") %>% group_by(simulation,year.num)%>% summarise(m = mean(value,na.rm = TRUE)) %>% pull(m)
mean((datagppliana/datagpp)[1:10])
