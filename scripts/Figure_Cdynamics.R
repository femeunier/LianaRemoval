rm(list = ls())

library(dplyr)
library(ggplot2)

# system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/all_OP_C.RDS",
#                     "/home/femeunier/Documents/projects/LianaRemoval/outputs"))

all_OP_C <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/all_OP_C.RDS") %>% mutate(time = year + month/12,
                                                                                                       type = "C pools")

runs2keep <- all_OP_C %>% filter(year == 2021, month == 2,treatment == "control",var == "AGB",value >9.6) %>% pull(irun)

all_OP_C <- bind_rows(list(all_OP_C,
                           all_OP_C %>% filter(irun %in% runs2keep) %>% group_by(time,treatment,irun) %>% summarise(value = sum(value),
                                                                                                                    irun = irun[1],
                                                                                                                    year = year[1],
                                                                                                                    month = month[1],
                                                                                                                    var = "Total",
                                                                                                                    type = "Total C")))

all_OP_C.agg <- all_OP_C %>% filter(irun %in% runs2keep) %>% group_by(time,var,treatment) %>% summarise(m = mean(value),
                                                                                                        s = sd(value),
                                                                                                        low = confint(lm(value ~ 1))[1,1],
                                                                                                        high = confint(lm(value ~ 1))[1,2],
                                                                                                        se = sd(value)/sqrt(length(value)),
                                                                                                        type = type[1])



ggplot(data = all_OP_C.agg,
       aes(x = time,y = m,color = as.factor(treatment),fill = as.factor(treatment),
           ymin = m -se, ymax = m + se)) +
  geom_line() +
  facet_wrap(~ var) +
  geom_ribbon(alpha = 0.5,color = NA) +
  theme_bw()


all_OP_C.agg <- all_OP_C.agg %>% group_by(treatment,var) %>% mutate(m_rel = m -m[1],
                                                                    low_rel = low - m[1],
                                                                    high_rel = high - m[1])


data = data.frame(time = c(2014,2014),
                  treatment = c("control","removal"),
                  var = "AGB",
                  m = 1.05*c(71.9 + 17.79 + 0.355 + 0.605 + 0.412,
                        79.3 + 0.929 + 1.723 + 2.429)/10)

# all_OP_C.agg[all_OP_C.agg$var == "Soil.Carbon" &all_OP_C.agg$treatment == "removal","m"] <- all_OP_C.agg[all_OP_C.agg$var == "Soil.Carbon" & all_OP_C.agg$treatment == "removal","m"] -seq(0.6,0,length.out = nrow(all_OP_C.agg[all_OP_C.agg$var == "Soil.Carbon" &all_OP_C.agg$treatment == "control","m"]))
# all_OP_C.agg[all_OP_C.agg$var == "Soil.Carbon" &all_OP_C.agg$treatment == "removal","low"] <- all_OP_C.agg[all_OP_C.agg$var == "Soil.Carbon" & all_OP_C.agg$treatment == "removal","low"] -seq(0.6,0,length.out = nrow(all_OP_C.agg[all_OP_C.agg$var == "Soil.Carbon" &all_OP_C.agg$treatment == "control","m"]))
# all_OP_C.agg[all_OP_C.agg$var == "Soil.Carbon" &all_OP_C.agg$treatment == "removal","high"] <- all_OP_C.agg[all_OP_C.agg$var == "Soil.Carbon" & all_OP_C.agg$treatment == "removal","high"] -seq(0.6,0,length.out = nrow(all_OP_C.agg[all_OP_C.agg$var == "Soil.Carbon" &all_OP_C.agg$treatment == "control","m"]))


ggplot(data = all_OP_C.agg %>% arrange(var,treatment,time) %>% filter(!(var == "Total"))) +
  geom_line( aes(x = time,y = m,color = as.factor(treatment),fill = as.factor(treatment),
                 ymin = low, ymax = high,
                 group = interaction(treatment,var))) +
  # geom_point(data = data,
  #            aes(x = time,y = m,color = as.factor(treatment))) +
  # facet_wrap(~ type,scales = "free") +
  geom_ribbon( aes(x = time,y = m,color = as.factor(treatment),fill = as.factor(treatment),
                   ymin = low, ymax = high,
                   group = interaction(treatment,var)),
               alpha = 0.5,color = NA) +
  labs(x = "",y = "Carbon stocks [kgC/m²]",color = "Treatment") +
  scale_color_manual(values = c("#1E64C8", "#137300")) +
  scale_fill_manual(values = c("#1E64C8", "#137300")) +
  scale_x_continuous(breaks = seq(2012,2022,2),
                     limits = c(2011,2022)) +
  theme_bw() + theme(text = element_text(size = 16),
                     legend.position = c(0.1,0.9),
                     panel.spacing = unit(2, "lines"),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  guides(fill = FALSE)

ggsave(plot = last_plot(), dpi = 300,height = 15,width = 20,units = "cm",
       filename = file.path(".","Figures","All.carbon.pools.png"))

rel.change <- all_OP_C.agg %>% filter(var == "AGB") %>% mutate(month = round(12*(time - floor(time)))) %>%
  filter(month == 3) %>% summarise(AGB = m,
                                   diff.abs = diff(c(m[1],m)),
                                   diff.rel = 100*diff(c(m[1],m))/AGB,
                                   time = time)

rel.change %>% filter(time > 2011.5 & time < 2014.5) %>% group_by(treatment) %>% summarise(m = mean(diff.rel))


ggplot(data = rel.change) +
  geom_line(aes(x = time, y = diff.rel,colour = as.factor(treatment))) +
  theme_bw()

ggsave(plot = last_plot(), dpi = 300,height = 15,width = 20,units = "cm",
       filename = file.path(".","Figures","All.carbon.pools.png"))


ggplot(data = all_OP_C.agg %>% arrange(var,treatment,time),
       aes(x = time,y = m_rel,color = as.factor(treatment),fill = as.factor(treatment),
           ymin = low_rel, ymax = high_rel,
           group = interaction(treatment,var))) +
  geom_line() +
  facet_wrap(~ var,scales = "free") +
  geom_ribbon(alpha = 0.5,color = NA) +
  labs(x = "",y = "Carbon stocks [kgC/m²]",color = "Treatment") +
  scale_color_manual(values = c("#1E64C8", "#137300")) +
  scale_fill_manual(values = c("#1E64C8", "#137300")) +
  scale_x_continuous(breaks = seq(2012,2022,2),
                     limits = c(2011,2022)) +
  theme_bw() + theme(text = element_text(size = 16),
                     legend.position = c(0.1,0.9),
                     panel.spacing = unit(2, "lines"),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  guides(fill = FALSE)
