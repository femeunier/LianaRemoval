rm(list = ls())

library(dplyr)
library(ggplot2)

# system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/OP.posterior.soil.RDS",
#                     "/home/femeunier/Documents/projects/LianaRemoval/outputs"))

OP.posterior.soil <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/OP.posterior.soil.RDS")


OP.posterior.soil.agg <- OP.posterior.soil %>% group_by(var,time,simulation) %>% summarise(value_m = mean(value),
                                                                                           value_low = confint(lm(value ~ 1))[1,1],
                                                                                           value_high = confint(lm(value ~ 1))[1,2],
                                                                                           value_verylow = quantile(value,probs = 0.025),
                                                                                           value_veryhigh = quantile(value,probs = 0.975))

levels(OP.posterior.soil.agg$var) <- c("FSC","StructSC","SSC")

OP.posterior.soil.agg <- bind_rows(list(OP.posterior.soil.agg,
                                        data.frame(var = 'FSC',value_m = NA,time = 2009,simulation = "control")))


ggplot(data = OP.posterior.soil.agg,
       aes(x = time,y = value_m,color = as.factor(simulation),fill = as.factor(simulation))) +
  geom_line() +
  geom_ribbon(aes(ymin = value_low, ymax = value_high),alpha=0.5,color = NA) +
  labs(x = "",y = "Carbon stocks [kgC/m²]",color = "Treatment") +
  facet_wrap(~ var,scales = "free") +
  scale_color_manual(values = c("#1E64C8", "#137300")) +
  scale_fill_manual(values = c("#1E64C8", "#137300")) +
  scale_x_continuous(breaks = seq(2012,2022,2)) +
  theme_bw() + theme(text = element_text(size = 16),
                     legend.position = c(0.55,0.86),
                     panel.spacing = unit(2, "lines"),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  guides(fill = FALSE)


ggsave(plot = last_plot(), dpi = 300,height = 15,width = 30,units = "cm",
       filename = file.path(".","Figures","carbon.pools.png"))


SC.tot <- OP.posterior.soil %>% group_by(time,simulation,irun) %>% summarise(value = sum(value)) %>%
  group_by(simulation,time) %>% summarise(value_m = mean(value),
                                          value_low = confint(lm(value ~ 1))[1,1],
                                          value_high = confint(lm(value ~ 1))[1,2],
                                          value_verylow = quantile(value,probs = 0.025),
                                          value_veryhigh = quantile(value,probs = 0.975))


ggplot(data = SC.tot,
       aes(x = time,y = value_m,color = as.factor(simulation),fill = as.factor(simulation))) +
  geom_line() +
  geom_ribbon(aes(ymin = value_low, ymax = value_high),alpha=0.5,color = NA) +
  labs(x = "",y = "Carbon stocks [kgC/m²]",color = "Treatment") +
  scale_color_manual(values = c("#1E64C8", "#137300")) +
  scale_fill_manual(values = c("#1E64C8", "#137300")) +
  scale_x_continuous(breaks = seq(2012,2022,2)) +
  theme_bw() + theme(text = element_text(size = 16),
                     legend.position = c(0.55,0.86),
                     panel.spacing = unit(2, "lines"),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  guides(fill = FALSE)
