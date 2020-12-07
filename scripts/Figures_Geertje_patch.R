#############################################
# Change the rates!!
# 100. * (1.0 - exp(- datum$szpft$agb.mort[1,12,c(2,3,4)]))
# 100. * (exp(datum$szpft$agb.recr[1,2,c(2,3,4)]) - 1.0)

rm(list = ls())

library(ggplot2)
library(cowplot)
library(dplyr)
library(albedo)
library(pracma)
library(reshape2)
library(purrr)
library(LianaRemoval)

final.year <- 2014

# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_17/analy/analysis.RData","./outputs/removal.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_17/analy/analysis.RData","./outputs/control.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control.RData","./outputs/control.RData"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removal.RData","./outputs/removal.RData"))

load("./outputs/control.RData")
control <- datum2df(datum,vars=c("gpp","agb","nppleaf",
                                 "agb.mort","agb.recr"),pfts = c(2, 3, 4, 17), name = "control")

temp_data <- LianaRemoval::datum2df_patch(
  datum,
  vars = c("gpp", "nppleaf"),
  patches = "uniform")

control_sd <- bind_rows(
  list(temp_data %>% arrange(patch,var,years,months,patch),
    datum2df_patch.cohort(datum, vars = c("agb.recr", "agb.mort","agb")) %>% arrange(patch,var,years,months,patch),
    datum2df_patch.cohort(datum, vars = c("agb.recr", "agb.mort","agb")) %>% group_by(patch, var, years, months) %>%
      summarise(value = sum(value),
                pa_area = mean(pa_area)) %>% mutate(pft = 18))) %>% mutate(simulation = "control")



load("./outputs/removal.RData")
removal <- datum2df(datum,vars=c("gpp","agb","nppleaf",
                                 "agb.mort","agb.recr"),pfts = c(2, 3, 4, 17), name = "removal")

temp_data <- LianaRemoval::datum2df_patch(
  datum,
  vars = c("gpp", "nppleaf"),
  patches = "uniform")

removal_sd <- bind_rows(
  list(bind_rows(list(temp_data,
                      temp_data %>% filter(pft == 2) %>% mutate(pft = 17,
                                                                value = 0))) %>% arrange(patch,var,years,months,patch),
    datum2df_patch.cohort(datum, vars = c("agb.recr", "agb.mort","agb")) %>% arrange(patch,var,years,months,patch),
    datum2df_patch.cohort(datum, vars = c("agb.recr", "agb.mort","agb")) %>% group_by(patch, var, years, months) %>%
      summarise(value = sum(value),
                pa_area = mean(pa_area)) %>% mutate(pft = 18))) %>% mutate(simulation = "removal")



simulations <- bind_rows(list(control,
                              removal)) %>% mutate(time = year + month/12)

simulations <-  bind_rows(list(simulations,
                               simulations%>% group_by(var,simulation,month,year,time) %>% summarise(value = sum(value,na.rm = TRUE)) %>% mutate(pft = 18))) %>% arrange(simulation,var,time)

simulations <- bind_rows(list(simulations,
                              simulations %>% filter(var == "agb") %>% group_by(simulation,pft) %>% mutate(var = "agb.change",
                                                                                                           value = 12*diff(c(value[1],value)))))

agb <-  simulations %>% filter(var == "agb", pft %in% c(2,3,4,17)) %>% pull(value)

agb.growth <- simulations %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% pull(value) -
  simulations %>% filter(var == "agb.recr", pft %in% c(2,3,4,17)) %>% pull(value) * agb +
  simulations %>% filter(var == "agb.mort", pft %in% c(2,3,4,17)) %>% pull(value) * agb

stem.prod <- simulations %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% pull(value) +
  simulations %>% filter(var == "agb.mort", pft %in% c(2,3,4,17)) %>% pull(value) * agb

agb.prod <- simulations %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% pull(value) +
  simulations %>% filter(var == "agb.mort", pft %in% c(2,3,4,17)) %>% pull(value) * agb +
  simulations %>% filter(var == "nppleaf", pft %in% c(2,3,4,17)) %>% pull(value)

simulations <- bind_rows(list(simulations,
                              simulations %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% mutate(value = agb.growth,
                                                                                                           var = "agb.growth") %>% group_by(time,simulation) %>%
                                summarise(value = sum(value,na.rm = TRUE),
                                          var = "agb.growth",
                                          pft = 18,
                                          month = month[1],
                                          year = year[1]),
                              simulations %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% mutate(value = stem.prod,
                                                                                                           var = "stem.prod") %>% group_by(time,simulation) %>%
                                summarise(value = sum(value,na.rm = TRUE),
                                          var = "stem.prod",
                                          pft = 18,
                                          month = month[1],
                                          year = year[1]),
                              simulations %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% mutate(value = agb.prod,
                                                                                                           var = "agb.prod") %>% group_by(time,simulation) %>%
                                summarise(value = sum(value,na.rm = TRUE),
                                          var = "agb.prod",
                                          pft = 18,
                                          month = month[1],
                                          year = year[1])))

simulations_sd <- bind_rows(list(control_sd,
                                 removal_sd)) %>% mutate(time = years + months/12) %>% rename(year = years,
                                                                                              month = months)

simulations_sd <- bind_rows(list(simulations_sd,
                                 simulations_sd %>% filter(var == "agb") %>% group_by(simulation,patch,pft) %>% mutate(var = "agb.change",
                                                                                                                       value = 12*diff(c(value[1],value)))))


agb.growth <- simulations_sd %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% pull(value) -
  simulations_sd %>% filter(var == "agb.recr", pft %in% c(2,3,4,17)) %>% pull(value) +
  simulations_sd %>% filter(var == "agb.mort", pft %in% c(2,3,4,17)) %>% pull(value)

stem.prod <- simulations_sd %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% pull(value) +
  simulations_sd %>% filter(var == "agb.mort", pft %in% c(2,3,4,17)) %>% pull(value)

agb.prod <- simulations_sd %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% pull(value) +
  simulations_sd %>% filter(var == "agb.mort", pft %in% c(2,3,4,17)) %>% pull(value) +
  simulations_sd %>% filter(var == "nppleaf", pft %in% c(2,3,4,17)) %>% pull(value)

simulations_sd <- bind_rows(list(simulations_sd,
                                 simulations_sd %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% mutate(value = agb.growth,
                                                                                                                 var = "agb.growth") %>% group_by(time,patch,simulation) %>%
                                summarise(value = sum(value,na.rm = TRUE),
                                          var = "agb.growth",
                                          pft = 18,
                                          month = month[1],
                                          year = year[1],
                                          pa_area = mean(pa_area,na.rm = TRUE)),
                                simulations_sd %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% mutate(value = agb.growth,
                                                                                                                var = "stem.prod") %>% group_by(time,patch,simulation) %>%
                                  summarise(value = sum(value,na.rm = TRUE),
                                            var = "stem.prod",
                                            pft = 18,
                                            month = month[1],
                                            year = year[1],
                                            pa_area = mean(pa_area,na.rm = TRUE)),
                                simulations_sd %>% filter(var == "agb.change", pft %in% c(2,3,4,17)) %>% mutate(value = agb.prod,
                                                                                                                var = "agb.prod") %>% group_by(time,patch,simulation) %>%
                                  summarise(value = sum(value,na.rm = TRUE),
                                            var = "agb.prod",
                                            pft = 18,
                                            month = month[1],
                                            year = year[1],
                                            pa_area = mean(pa_area,na.rm = TRUE))))


sd_temp <- simulations_sd %>% mutate(year = floor(time)) %>%
  group_by(year,patch,simulation,var,pft) %>% summarise(value = mean(value),
                                                        pa_area = mean(pa_area))

we <-  sd_temp %>%
  group_by(year,simulation,var,pft) %>% summarise(wm = weighted.mean(value, pa_area),
                                                  ws = sqrt(sum(pa_area * (value - wm)^2)),
                                                  N = length(pa_area),
                                                  we = ws/sqrt(N))

simulations <- left_join(simulations,we)

#########################################################################
# Net biomass change (all together)

AGB.change <- simulations %>% filter(pft == 18,var == "agb.change",
                                     time <= final.year)

AGB.change.sum <- AGB.change %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                         value_sd = mean(ws),
                                                                         value_se = mean(we),
                                                                         N = mean(N))

# pval <-  AGB.change %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])

# pval <- sd_temp %>% filter(pft == 18,var == "agb.change",
#                            year <= 2014) %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])

pval <- AGB.change.sum %>% group_by(year) %>% summarise(p.val = t.test2(m1 = value_m[simulation == "control"],m2 = value_m[simulation == "removal"],
                                                                        s1 = value_sd[simulation == "control"],s2 = value_sd[simulation == "removal"],
                                                                        n1 = N[simulation == "control"],n2 = N[simulation == "removal"]))


AGB.change.sum2plot <- AGB.change.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                        p.val <= 0.10 ~ "*",
                                                                                        TRUE ~ ""))

plotA <-
  ggplot(data = AGB.change.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.0*value_m*10,ymax = 10*(value_m+sign(value_m)*value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge(),color = "black") +
  geom_text(data = AGB.change.sum2plot %>% group_by(year) %>% summarise(value_m = max(value_m),
                                                                        signif = signif[1],
                                                                        simulation = simulation[1]),
            aes(label = signif, y = value_m*10*1.05),size = 10) +
  labs(x = "",y = "net biomass change") + guides(fill = FALSE) +
  scale_fill_manual(values = c("white","darkgrey")) +
  scale_y_continuous(expand = c(0.,0,0.1,0)) +
  theme_bw() +
  theme(text = element_text(size = 24))

#########################################################################
# Biomass mortality (all together)

mort <- simulations %>% filter(pft %in% c(2,3,4,17),
                               var == "agb.mort",
                               time <= final.year)

agb <- simulations %>% filter(pft %in% c(2,3,4,17),var == "agb",
                              time <= final.year) %>% mutate(agb = value,
                                                       var = "agb.mort") %>% select(-c(value,wm,ws,N,we))

mort <- mort %>% left_join(agb) %>% mutate(value.agb = agb*value) %>% select(-c(value,agb)) %>% rename(value = value.agb)
mort.all <- mort %>% group_by(time,simulation,year,month) %>% summarise(value = sum(value,na.rm = TRUE),
                                                                        ws = mean(ws,na.rm = TRUE),
                                                                        we = mean(we,na.rm = TRUE),
                                                                        N = mean(N))

mort.all.sum <- mort.all %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                     value_sd = mean(ws),
                                                                     value_se = mean(we),
                                                                     N = mean(N))

pval <- mort.all.sum %>% group_by(year) %>% summarise(p.val = t.test2(m1 = value_m[simulation == "control"],m2 = value_m[simulation == "removal"],
                                                                      s1 = value_sd[simulation == "control"],s2 = value_sd[simulation == "removal"],
                                                                      n1 = N[simulation == "control"],n2 = N[simulation == "removal"]))

mort.all.sum2plot <- mort.all.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                    p.val <= 0.10 ~ "*",
                                                                                    TRUE ~ ""))
plotD <-
  ggplot(data = mort.all.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+sign(value_m)*value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge(),color = "black") +
  geom_text(data = mort.all.sum2plot %>% group_by(year) %>% summarise(value_m = max(value_m),
                                                                      signif = signif[1],
                                                                      simulation = simulation[1]),
            aes(label = signif, y = value_m*10*1.05),size = 10) +
  labs(x = "",y = "biomass mortality") + guides(fill = FALSE) +
  scale_y_continuous(expand = c(0.,0,0.1,0)) +
  scale_fill_manual(values = c("white","darkgrey")) +
  theme_bw() +
  theme(text = element_text(size = 24))

#########################################################################
# Biomass recruitment (all together)

recr <- simulations %>% filter(pft %in% c(2,3,4,17),var == "agb.recr",
                               time <= final.year)

agb <- simulations %>% filter(pft %in% c(2,3,4,17),var == "agb",
                              time <= final.year) %>% mutate(agb = value,
                                                       var = "agb.recr")  %>% select(-c(value,wm,ws,N,we))


recr <- recr %>% left_join(agb) %>% mutate(value.agb = agb*value) %>% select(-c(value,agb)) %>% rename(value = value.agb)
recr.all <- recr %>% group_by(time,simulation,year,month) %>% summarise(value = sum(value,na.rm = TRUE),
                                                                        ws = mean(ws,na.rm = TRUE),
                                                                        we = mean(we,na.rm = TRUE),
                                                                        N = mean(N))

recr.all.sum <- recr.all %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                     value_sd = mean(ws),
                                                                     value_se = mean(we),
                                                                     N = mean(N))

# pval <-  recr.all %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])
# pval <- sd_temp %>% filter(pft == 18,
#                            var == "agb.recr",
#                            year <= 2014) %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation),weights = pa_area))[[1]][1,5])


pval <- recr.all.sum %>% group_by(year) %>% summarise(p.val = t.test2(m1 = value_m[simulation == "control"],m2 = value_m[simulation == "removal"],
                                                                      s1 = value_sd[simulation == "control"],s2 = value_sd[simulation == "removal"],
                                                                      n1 = N[simulation == "control"],n2 = N[simulation == "removal"]))

recr.all.sum2plot <- recr.all.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                    p.val <= 0.10 ~ "*",
                                                                                    TRUE ~ ""))

plotC <-
  ggplot(data = recr.all.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+sign(value_m)*value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge(),color = "black") +
  geom_text(data = recr.all.sum2plot %>% group_by(year) %>% summarise(value_m = max(value_m),
                                                                      signif = signif[1],
                                                                      simulation = simulation[1]),
            aes(label = signif, y = value_m*10 + 0.003),size = 10) +
  labs(x = "",y = "biomass recruitment") + guides(fill = FALSE) +
  scale_y_continuous(expand = c(0.,0,0.1,0)) +
  scale_fill_manual(values = c("white","darkgrey")) +
  theme_bw() +
  theme(text = element_text(size = 24))

#########################################################################
# Growth rates

# growth.all.temp <-
#   AGB.change %>% ungroup() %>% select(time,simulation,value) %>% rename(netbiomasschange = value) %>% left_join(
#     recr.all %>% ungroup() %>% select(time,simulation,value) %>% rename(recruitment = value)) %>% left_join(
#       mort.all %>% ungroup() %>% select(time,simulation,value) %>% rename(mort = value)) %>% mutate(growth = netbiomasschange - recruitment + mort,
#                                                                                                     stem.productivity = netbiomasschange + mort) %>%
#   mutate(year = floor(time)) %>% rename(value = growth) %>% filter(year < 2014)


# growth.all <- growth.all.temp %>% select(time,simulation,value,year)

# growth.all.sum <- growth.all %>% group_by(year,simulation) %>% summarise(value_m = mean(value))


growth <- simulations %>% filter(pft == 18,var == "agb.growth",
                                 time <= final.year)

growth.all.sum <- growth %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                 value_sd = mean(ws),
                                                                 value_se = mean(we),
                                                                 N = mean(N))


pval <- growth.all.sum %>% group_by(year) %>% summarise(p.val = t.test2(m1 = value_m[simulation == "control"],m2 = value_m[simulation == "removal"],
                                                                        s1 = value_sd[simulation == "control"],s2 = value_sd[simulation == "removal"],
                                                                        n1 = N[simulation == "control"],n2 = N[simulation == "removal"]))


growth.all.sum2plot <- growth.all.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                        p.val <= 0.10 ~ "*",
                                                                                        TRUE ~ ""))

plotB <-
  ggplot(data = growth.all.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+sign(value_m)*value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge(),color = "black") +
  geom_text(data = growth.all.sum2plot %>% summarise(value_m = max(value_m),
                                                     signif = signif[1],
                                                     simulation = simulation[1]),
            aes(label = signif, y = value_m*10*1.05),size = 10) +
  labs(x = "",y = "biomass growth") + guides(fill = FALSE) +
  scale_y_continuous(expand = c(0.,0,0.1,0)) +
  scale_fill_manual(values = c("white","darkgrey")) +
  theme_bw() +
  theme(text = element_text(size = 24))

#########################################################################
Figure1 <- plot_grid(plotA,plotB,plotC,plotD,nrow = 2)
Figure1




ggsave(plot = last_plot(),
       filename = "./Figures/Figure1_Geertje.png",width = 15,height = 15,dpi = 300)

#########################################################################
# Figure 2
stem.prod <- simulations %>% filter(pft == 18,var == "stem.prod",
                                    time <= final.year)

stem.prod.sum <- stem.prod %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                       value_sd = mean(ws),
                                                                       value_se = mean(we),
                                                                       N = mean(N))

pval <- stem.prod.sum %>% group_by(year) %>% summarise(p.val = t.test2(m1 = value_m[simulation == "control"],m2 = value_m[simulation == "removal"],
                                                                       s1 = value_sd[simulation == "control"],s2 = value_sd[simulation == "removal"],
                                                                       n1 = N[simulation == "control"],n2 = N[simulation == "removal"]))


stem.prod.sum2plot  <- stem.prod.sum  %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                        p.val <= 0.10 ~ "*",
                                                                                        TRUE ~ ""))

plot2B <-
  ggplot(data = stem.prod.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+sign(value_m)*value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge(),color = "black") +
  geom_text(data = stem.prod.sum2plot %>% summarise(value_m = max(value_m),
                                                    signif = signif[1],
                                                    simulation = simulation[1]),
            aes(label = signif, y = value_m*10*1.05),size = 10) +
  labs(x = "",y = "Stem productivity") + guides(fill = FALSE) +
  scale_y_continuous(expand = c(0.,0,0.1,0)) +
  scale_fill_manual(values = c("white","darkgrey")) +
  theme_bw() +
  theme(text = element_text(size = 24))

#########################################################################
leaf.prod <- simulations %>% filter(pft == 18,var == "nppleaf",
                                    time <= final.year)

leaf.prod.sum <- leaf.prod %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                       value_sd = mean(ws),
                                                                       value_se = mean(we),
                                                                       N = mean(N))

pval <- leaf.prod.sum %>% group_by(year) %>% summarise(p.val = t.test2(m1 = value_m[simulation == "control"],m2 = value_m[simulation == "removal"],
                                                                       s1 = value_sd[simulation == "control"],s2 = value_sd[simulation == "removal"],
                                                                       n1 = N[simulation == "control"],n2 = N[simulation == "removal"]))


leaf.prod.sum2plot  <- leaf.prod.sum  %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                        p.val <= 0.10 ~ "*",
                                                                                        TRUE ~ ""))

plot2A <-
  ggplot(data = leaf.prod.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+sign(value_m)*value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge(),color = "black") +
  geom_text(data = leaf.prod.sum2plot %>% summarise(value_m = max(value_m),
                                                    signif = signif[1],
                                                    simulation = simulation[1]),
            aes(label = signif, y = value_m*10*1.05),size = 10) +
  labs(x = "",y = "Canopy productivity") + guides(fill = FALSE) +
  scale_y_continuous(expand = c(0.,0,0.1,0)) +
  scale_fill_manual(values = c("white","darkgrey")) +
  theme_bw() +
  theme(text = element_text(size = 24))

#########################################################################
agb.prod <- simulations %>% filter(pft == 18,var == "agb.prod",
                                   time <= final.year)

agb.prod.sum <- agb.prod %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                       value_sd = mean(ws),
                                                                       value_se = mean(we),
                                                                       N = mean(N))

pval <- agb.prod.sum %>% group_by(year) %>% summarise(p.val = t.test2(m1 = value_m[simulation == "control"],m2 = value_m[simulation == "removal"],
                                                                       s1 = value_sd[simulation == "control"],s2 = value_sd[simulation == "removal"],
                                                                       n1 = N[simulation == "control"],n2 = N[simulation == "removal"]))


agb.prod.sum2plot  <- agb.prod.sum  %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                        p.val <= 0.10 ~ "*",
                                                                                        TRUE ~ ""))

plot2C <-
  ggplot(data = agb.prod.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+sign(value_m)*value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge(),color = "black") +
  geom_text(data = agb.prod.sum2plot %>% summarise(value_m = max(value_m),
                                                    signif = signif[1],
                                                    simulation = simulation[1]),
            aes(label = signif, y = value_m*10*1.05),size = 10) +
  labs(x = "",y = "Total AGB productivity") + guides(fill = FALSE) +
  scale_y_continuous(expand = c(0.,0,0.1,0)) +
  scale_fill_manual(values = c("white","darkgrey")) +
  theme_bw() +
  theme(text = element_text(size = 24))

#########################################################################
Figure2 <- plot_grid(plot2A,plot2B,plot2C,nrow = 1)
Figure2


Figures.data <- bind_rows(list(AGB.change.sum2plot %>% mutate(var = "AGB.change"),
                               growth.all.sum2plot %>% mutate(var = "biomass.growth"),
                               recr.all.sum2plot %>% mutate(var = "recruitment"),
                               mort.all.sum2plot %>% mutate(var = "mortality"),
                               agb.prod.sum2plot %>% mutate(var = "AGB.prod"),
                               leaf.prod.sum2plot %>% mutate(var = "leaf.prod"),
                               stem.prod.sum2plot %>% mutate(var = "stem.prod")))

saveRDS(object = Figures.data,
        file = "./outputs/Geertje.RDS")

ggsave(plot = last_plot(),
       filename = "./Figures/Figure2_Geertje.png",width = 15,height = 8,dpi = 300)
