rm(list = ls())

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(LianaRemoval)

# system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/OP.posterior.Cpools.RDS",
#                     "/home/femeunier/Documents/projects/LianaRemoval/outputs"))

OP.posterior.Cpools <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/OP.posterior.Cpools.RDS")

runs2keep <- OP.posterior.Cpools %>% filter((pft == 18 & var == "agb" & time == "After 10 years" & stat == "mean" &
                                               treatment == "Control" & value >= 9.6)) %>% pull(irun)

# correct for living biomass
# OP.posterior.Cpools <- OP.posterior.Cpools %>% mutate(value = case_when(stat == "mean" & type == "biomass" ~ 0.96*value,
                                                                        # TRUE ~ value))

OP.posterior.Cpools <- OP.posterior.Cpools %>% filter(irun %in% runs2keep)

IC_sd <- OP.posterior.Cpools %>% filter(stat == "sd",time == "Initial carbon stocks") %>% filter((type == "biomass" & var == "agb") | (type == "soil" & var == "bgb")) %>% group_by(pft,type,var,treatment) %>%
  summarise(value = mean(value)) %>% mutate(stat = "sd",N = 8)

OP.posterior.Cpools$time <- factor(OP.posterior.Cpools$time,levels = c("Initial carbon stocks",'After 3 years',"After 10 years"))

OP.all2plot <- bind_rows(list(OP.posterior.Cpools %>% filter(stat == "mean") %>% group_by(pft,type,var,treatment,time) %>%
                                summarise(value = mean(value),
                                          N = length(N)) %>% mutate(stat = "mean"),
                              OP.posterior.Cpools %>% filter(stat == "mean") %>% group_by(pft,type,var,treatment,time) %>%
                                summarise(value = sd(value),
                                          N = length(N)) %>% mutate(stat = "sd")))

OP.all2plot[OP.all2plot$stat == "sd" & OP.all2plot$time == "Initial carbon stocks" &
             OP.all2plot$pft == 18 & OP.all2plot$type == "biomass","value"] <- IC_sd$value

OP.all2plot_m <- OP.all2plot %>% filter(stat == "mean") %>%   filter(!(pft == 18 & type == "biomass")) %>%
  group_by(treatment,time) %>%
  mutate(pos_X = case_when(treatment == "Control" ~ 1,
                           treatment == "Removal" ~ 2),
         pos_Y = case_when(var == "FSC" & time == "End" & treatment == "control" ~ sum(value[var == "FSC"]/2),
                           var == "SSC" ~ sum(value[var == "FSC"] + value[var == "SSC"]/2),
                           var == "StructSC" ~ sum(value[var == "FSC"] + value[var == "SSC"] + value[var == "StructSC"]/2),
                           TRUE ~ 0),
         label = case_when(var %in% c("SSC","StructSC") ~ var,
                           var == "FSC" & time == "End" & treatment == "control" ~ "",
                           TRUE ~ "")) %>%
  mutate(GF = case_when(pft == 17 ~ "Liana",
                        pft == 18 ~ "Soil",
                        TRUE ~ "Tree"))

OP.all2plot_sd <- bind_rows(list(OP.all2plot %>% filter(pft == 18,((type == "soil" & var == "bgb") | (type == "biomass" & var == "agb"))),
                                 OP.all2plot %>% filter((var == "bgb" & stat == "mean" & pft == 18) | (type == "soil" & stat == "mean")) %>% ungroup() %>% group_by(treatment,time) %>%
                                   summarise(value = sum(value),
                                             N = mean(N)) %>% ungroup() %>% mutate(type = "soil",var = "bgb",stat = "mean",pft = 18),
                                 OP.all2plot %>% filter((var == "bgb" & stat == "sd" & pft == 18) | (type == "soil" & stat == "sd")) %>% ungroup() %>% group_by(treatment,time) %>%
                                   summarise(value = sum(value),
                                             N = mean(N)) %>% ungroup() %>% mutate(type = "soil",var = "bgb",stat = "sd",pft = 18)

                                 )) %>% pivot_wider(
                                               names_from = stat,values_from = value)

P.vals <- OP.all2plot_sd %>% group_by(time,var) %>% summarise(p.val = t.test2(mean[treatment == "Control"],mean[treatment == "Removal"],
                                                                              sd[treatment == "Control"],sd[treatment == "Removal"],
                                                                              N[treatment == "Control"],N[treatment == "Removal"])) %>%
  mutate(signif = case_when(p.val <= 0.05 ~ "**",
                            p.val <= 0.10 ~ "*",
                            TRUE ~ ""))
OP.all2plot_sd <- OP.all2plot_sd %>% left_join(P.vals) %>% ungroup()


OP.all2plot_m[OP.all2plot_m$pft == 18 & OP.all2plot_m$treatment == "Removal" & OP.all2plot_m$time == "Initial carbon stocks" & OP.all2plot_m$var == "StructSC","value"] <-
  (OP.all2plot_m %>% filter(pft == 18,treatment == "Removal",time == "Initial carbon stocks",var == "StructSC") %>% pull(value)  - 0.6)

OP.all2plot_sd[OP.all2plot_sd$pft == 18 & OP.all2plot_sd$treatment == "Removal" & OP.all2plot_sd$time == "Initial carbon stocks" & OP.all2plot_sd$var == "bgb","mean"] <-
  (OP.all2plot_sd %>% filter(pft == 18,treatment == "Removal",time == "Initial carbon stocks",var == "bgb") %>% pull(mean)  - 0.6)

ggplot() +
  geom_errorbar(data =  subset(OP.all2plot_sd,var == "agb"),
                aes(x = as.factor(treatment),y = mean,ymin = mean,ymax = mean + sd),width = 0.2) +
  geom_bar(data = subset(OP.all2plot_m,var == "agb"),
           aes(x = as.factor(treatment),y = value, fill = as.factor(pft)),
           color = "black",alpha = 0.4,
           stat = "identity",position = "stack") +
  # geom_text(data = subset(OP.all2plot_sd,var == "agb") %>% group_by(time) %>% summarise(value_m = max(mean),
  #                                                                                       signif = signif[1],
  #                                                                                       treatment = treatment[1]),
  #           aes(x = 1.5,label = signif, y = value_m*1.05),size = 5) +
  geom_errorbar(data =  subset(OP.all2plot_sd,var == "bgb"),
                aes(x = as.factor(treatment),y = -mean,ymin = -mean,ymax = -mean - sd),width = 0.2) +
  geom_bar(data = subset(OP.all2plot_m,var != "agb"),
           aes(x = as.factor(treatment),y = -value, fill = as.factor(pft)),
           color = "black",alpha = 0.4,
           stat = "identity",position = "stack") +
  # geom_text(data = subset(OP.all2plot_sd,var == "bgb") %>% group_by(time) %>% summarise(value_m = max(mean),
  #                                                                                       signif = signif[1],
  #                                                                                       treatment = treatment[1]),
  #           aes(x = 1.5,label = signif, y = -value_m*1.08),size = 5) +
  facet_wrap(~ time) +
  # geom_text(data = subset(OP.all2plot_m,var != "agb"),
  #           aes(pos_X,
  #               -pos_Y, group=as.factor(treatment), label=label), size=4) +
  scale_fill_manual(values = c("#1E64C8","darkgrey","#9FFF8C","#44CC29","#137300"),
                    labels = c("Liana","Soil","Early","Mid","Late")) +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(x = "",y = "Carbon stocks [kgC/m²] \n Belowground \t Aboveground",fill = "Source") +
  theme_bw() + theme(text = element_text(size = 16),
                     axis.title.y = element_text(hjust = 0.55),
                     legend.position = "top") +
  guides(fill = guide_legend(title.position = "top",label.vjust = 0.5))

ggsave(plot = last_plot(), dpi = 300,height = 20,width = 16,units = "cm",
       filename = file.path(".","Figures","Carbon.Stocks.all.png"))

subset(OP.all2plot_m,var == "agb") %>% filter(time == "Initial carbon stocks" & var == "agb") %>% group_by(treatment) %>% summarise(m = sum(value))
subset(OP.all2plot_m,var == "agb") %>% filter(time == "After 3 years" & var == "agb") %>% group_by(treatment) %>% summarise(m = sum(value))


OP.all2plot_m %>% filter(type == "soil",var =="FSC",time == "Initial carbon stocks") %>% pull(value) %>% diff()
OP.all2plot_m %>% filter(type == "soil",var =="StructSC",time == "Initial carbon stocks") %>% pull(value) %>% diff()


subset(OP.all2plot_m) %>% filter(time == "Initial carbon stocks" & var %in% c("agb","bgb","FSC","StructSC","SSC")) %>% group_by(treatment) %>% summarise(m = sum(value))
subset(OP.all2plot_m) %>% filter(time == "After 10 years" & var %in% c("agb","bgb","FSC","StructSC","SSC")) %>% group_by(treatment) %>% summarise(m = sum(value)) %>% pull() %>% diff() %>% abs()

subset(OP.all2plot_m) %>% filter(time == "After 10 years" & var %in% c("agb","bgb","FSC","StructSC","SSC")) %>% group_by(treatment,var) %>% summarise(m = sum(value)) %>%
  pivot_wider(names_from = treatment,values_from = m) %>% mutate(diff = Removal - Control) %>% pull(diff)/1.773*100

subset(OP.all2plot_m) %>% filter(pft == 2,var == "agb")

subset(OP.all2plot_m) %>% group_by(treatment,time) %>% summarise(s = sum(value))
