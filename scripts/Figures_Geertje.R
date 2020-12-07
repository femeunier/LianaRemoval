rm(list = ls())

library(ggplot2)
library(cowplot)

load("./outputs/control.RData")
control <- datum2df(datum,vars=c("gpp","agb","nppleaf",
                                 "agb.mort","agb.recr"),pfts = c(2, 3, 4, 17), name = "control")

load("./outputs/removal.RData")
removal <- datum2df(datum,vars=c("gpp","agb","nppleaf",
                                 "agb.mort","agb.recr"),pfts = c(2, 3, 4, 17), name = "removal")

simulations <- bind_rows(list(control,
                              removal)) %>% mutate(time = year + month/12)

simulations <-  bind_rows(list(simulations,
                               simulations%>% group_by(var,simulation,month,year,time) %>% summarise(value = sum(value,na.rm = TRUE)) %>% mutate(pft = 18))) %>%
  arrange(simulation,var,time)

simulations <- bind_rows(list(simulations,
                              simulations %>% filter(var == "agb") %>% group_by(simulation,pft) %>% mutate(var = "agb.change",
                                                                                                           value = 12*diff(c(value[1],value)))))

#########################################################################
# Net biomass change (all together)

AGB.change <- simulations %>% filter(pft == 18,var == "agb.change",
                                     time <= 2014)

AGB.change.sum <- AGB.change %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                         value_sd = sd(value),
                                                                         value_se = value_sd/sqrt(length(value)))

pval <-  AGB.change %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])
AGB.change.sum2plot <- AGB.change.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                        p.val <= 0.10 ~ "*",
                                                                                        TRUE ~ "N.S."))

plotA <-
  ggplot(data = AGB.change.sum2plot,
       aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(data = AGB.change.sum2plot %>% filter(simulation == "removal"),
            aes(label = signif, y = value_m*10*1.05)) +
  labs(x = "",y = "net biomass change") + guides(fill = FALSE) +
  theme_bw()

#########################################################################
# Biomass mortality (all together)

mort <- simulations %>% filter(pft %in% c(2,3,4,17),var == "agb.mort",
                               time <= 2014)

agb <- simulations %>% filter(pft %in% c(2,3,4,17),var == "agb",
                               time <= 2014) %>% mutate(agb = value,
                                                        var = "agb.mort") %>% select(-value)

mort <- mort %>% left_join(agb) %>% mutate(value.agb = agb*value) %>% select(-c(value,agb)) %>% rename(value = value.agb)
mort.all <- mort %>% group_by(time,simulation,year,month) %>% summarise(value = sum(value,na.rm = TRUE))

mort.all.sum <- mort.all %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                     value_sd = sd(value),
                                                                     value_se = value_sd/sqrt(length(value)))

pval <-  mort.all %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])
mort.all.sum2plot <- mort.all.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                        p.val <= 0.10 ~ "*",
                                                                                        TRUE ~ "N.S."))

plotD <-
  ggplot(data = mort.all.sum2plot,
       aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(data = mort.all.sum2plot %>% filter(simulation == "control"),
            aes(label = signif, y = value_m*10*1.05)) +
  labs(x = "",y = "biomass mortality") + guides(fill = FALSE) +
  theme_bw()

#########################################################################
# Biomass recruitment (all together)

recr <- simulations %>% filter(pft %in% c(2,3,4,17),var == "agb.recr",
                               time <= 2014)

agb <- simulations %>% filter(pft %in% c(2,3,4,17),var == "agb",
                              time <= 2014) %>% mutate(agb = value,
                                                       var = "agb.recr") %>% select(-value)


recr <- recr %>% left_join(agb) %>% mutate(value.agb = agb*value) %>% select(-c(value,agb)) %>% rename(value = value.agb)
recr.all <- recr %>% group_by(time,simulation,year,month) %>% summarise(value = sum(value,na.rm = TRUE))

recr.all.sum <- recr.all %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                     value_sd = sd(value),
                                                                     value_se = value_sd/sqrt(length(value)))

pval <-  recr.all %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])
recr.all.sum2plot <- recr.all.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                    p.val <= 0.10 ~ "*",
                                                                                    TRUE ~ "N.S."))

plotC <-
  ggplot(data = recr.all.sum2plot,
       aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(data = recr.all.sum2plot %>% filter(simulation == "control"),
            aes(label = signif, y = value_m*10 + 0.002)) +
  labs(x = "",y = "biomass recruitment") + guides(fill = FALSE) +
  theme_bw()


#########################################################################
# Net biomass change (all by PFT)

#########################################################################
# Growth rates

growth.all.temp <-
  AGB.change %>% ungroup() %>% select(time,simulation,value) %>% rename(netbiomasschange = value) %>% left_join(
  recr.all %>% ungroup() %>% select(time,simulation,value) %>% rename(recruitment = value)) %>% left_join(
    mort.all %>% ungroup() %>% select(time,simulation,value) %>% rename(mort = value)) %>% mutate(growth = netbiomasschange - recruitment + mort,
                                                                                                  stem.productivity = netbiomasschange + mort) %>%
  mutate(year = floor(time)) %>% rename(value = growth) %>% filter(year < 2014)

growth.all <- growth.all.temp %>% select(time,simulation,value,year)

growth.all.sum <- growth.all %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                         value_sd = sd(value),
                                                                         value_se = value_sd/sqrt(length(value)))

pval <-  growth.all %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])
growth.all.sum2plot <- growth.all.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                        p.val <= 0.10 ~ "*",
                                                                                        TRUE ~ "N.S."))

plotB <-
  ggplot(data = growth.all.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(data = growth.all.sum2plot %>% filter(simulation == "removal"),
            aes(label = signif, y = value_m*10 *1.05)) +
  labs(x = "",y = "biomass growth") + guides(fill = FALSE) +
  theme_bw()

#########################################################################
Figure1 <- plot_grid(plotA,plotB,plotC,plotD,nrow = 2)
Figure1

#########################################################################
# Canopy productivity

NPPleaf <- simulations %>% filter(pft == 18,var == "nppleaf",
                                  time <= 2014)

NPPleaf.sum <- NPPleaf %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                   value_sd = sd(value),
                                                                   value_se = value_sd/sqrt(length(value)))

pval <-  NPPleaf %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])
NPPleaf.sum2plot <- NPPleaf.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                  p.val <= 0.10 ~ "*",
                                                                                  TRUE ~ "N.S."))

Figure2subplotA <-
  ggplot(data = NPPleaf.sum,
       aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(data = NPPleaf.sum2plot %>% filter(simulation == "control"),
            aes(label = signif, y = value_m*10*1.02)) +
  labs(x = "",y = "Canopy productivity") + guides(fill = FALSE) +
  scale_y_continuous(limits = c(0,10)) +
  theme_bw()

#########################################################################
# Stem productivity

stem.productivity.all <- growth.all.temp %>% select(time,simulation,stem.productivity,year) %>% rename(value = stem.productivity)

stem.productivity.all.sum <- stem.productivity.all %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                                    value_sd = sd(value),
                                                                                    value_se = value_sd/sqrt(length(value)))

pval <-  stem.productivity.all %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])
stem.productivity.all.sum2plot <- stem.productivity.all.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                        p.val <= 0.10 ~ "*",
                                                                                        TRUE ~ "N.S."))

Figure2subplotB <-
  ggplot(data = stem.productivity.all.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(data = stem.productivity.all.sum2plot %>% filter(simulation == "removal"),
            aes(label = signif, y = value_m*10 *1.05)) +
  labs(x = "",y = "Stem productivity") + guides(fill = FALSE) +
  scale_y_continuous(limits = c(0,10)) +
  theme_bw()

#########################################################################
# AGB productivity

AGB.prod.all <-
  stem.productivity.all %>% rename(canopy.productivity = value) %>% ungroup() %>% left_join(
  NPPleaf %>% select(time,simulation,value) %>% rename(stem.productivity = value) %>% ungroup(),
  by = c("time","simulation")) %>% mutate(value = canopy.productivity + stem.productivity) %>% select(time,simulation,year,value)


AGB.prod.all.sum <- AGB.prod.all %>% group_by(year,simulation) %>% summarise(value_m = mean(value),
                                                                             value_sd = sd(value),
                                                                             value_se = value_sd/sqrt(length(value)))

pval <-  AGB.prod.all %>% group_by(year) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(simulation)))[[1]][1,5])
AGB.prod.all.sum2plot <- AGB.prod.all.sum %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                                            p.val <= 0.10 ~ "*",
                                                                                            TRUE ~ "N.S."))
Figure2subplotC <-
  ggplot(data = AGB.prod.all.sum2plot,
         aes(x = year, y = value_m*10,fill = simulation)) +
  geom_errorbar(aes(ymin = 0.9*value_m*10,ymax = 10*(value_m+value_se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(data = AGB.prod.all.sum2plot %>% filter(simulation == "removal"),
            aes(label = signif, y = value_m*10 *1.05)) +
  labs(x = "",y = "Stem productivity") + guides(fill = FALSE) +
  scale_y_continuous(limits = c(0,10)) +
  theme_bw()

#########################################################################

Figure2 <- plot_grid(Figure2subplotA,Figure2subplotB,Figure2subplotC,nrow = 1)
Figure2

