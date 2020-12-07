rm(list = ls())

library(dplyr)
library(ggplot2)

# system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/all_OP_Geertje.RDS",
#                     "/home/femeunier/Documents/projects/LianaRemoval/outputs"))

OP.posterior.val <- readRDS("/home/femeunier/Documents/projects/LianaRemoval/outputs/all_OP_Geertje.RDS")

mod <- OP.posterior.val %>% dplyr::select(year,simulation,value_m,value_sd,var,irun) %>% rename(mod_m = value_m,
                                                                                                mod_sd = value_sd) %>%
  group_by(year,simulation,var) %>% summarise(SD = mean(mod_sd),
                                              M = mean(mod_m)) %>% ungroup() %>% rename(mod_sd = SD,
                                                                                        mod_m = M)


obs <- bind_rows(list(data.frame(year = c(2011,2011,2012,2012,2013,2013),
                                 simulation = c("control","removal"),
                                 value_m = c(0.2628323,0.96042275,0.64579755,2.4294443,0.40993106,2.9312937),
                                 value_sd = c(1.0005289,1.5096017,1.7728503,3.4048517,2.1681232,3.3493254),
                                 var = "AGB.change"),
                      data.frame(year = c(2011,2011,2012,2012,2013,2013),
                                 simulation = c("control","removal"),
                                 value_m = c(1.6758922,1.9103819,2.1408436,3.322065,1.8762696,3.0533733),
                                 value_sd = c(2.0939367,2.2833126,2.7228286,3.6007273,2.5811985,3.3197596),
                                 var = "biomass.growth"),
                      data.frame(year = c(2011,2011,2012,2012,2013,2013),
                                 simulation = c("control","removal"),
                                 value_m = c(0.06572899,0.1471202,0.09398994,0.121233135,0.1845985,0.4349747),
                                 value_sd = c(0.09936705,0.22997649,0.1579744,0.25413057,0.23135698,0.6687761),
                                 var = "recruitment"),
                      data.frame(year = c(2011,2011,2012,2012,2013,2013),
                                 simulation = c("control","removal"),
                                 value_m = c(1.3731794,1.0930219,1.3528208,1.1070997,1.6080347,0.4492534),
                                 value_sd = c(2.5446389,NA,2.5989225,3.0193205,3.2675807,1.6838883),
                                 var = "mortality"),
                      data.frame(year = c(2011,2011,2012,2012,2013,2013),
                                 simulation = c("control","removal"),
                                 value_m = c(4.5389996,4.061553,4.4863763,3.765359,4.743774,4.044902),
                                 value_sd = c(5.1092143,4.6428833,5.034488,4.4850554,5.2863774,4.653956),
                                 var = "leaf.prod"),
                      data.frame(year = c(2011,2011,2012,2012,2013,2013),
                                 simulation = c("control","removal"),
                                 value_m = c(1.6488181,1.9188062,2.100058,3.2226288,1.9145626,3.1090775),
                                 value_sd = c(2.003158,2.112564,2.5706332,3.471736,2.390688,3.2475283),
                                 var = "stem.prod"),
                      data.frame(year = c(2011,2011,2012,2012,2013,2013),
                                 simulation = c("control","removal"),
                                 value_m = c(6.299163,5.976748,6.517848,7.0203204,6.6866784,7.1504245),
                                 value_sd = c(7.1185694,6.641137,7.3039947,7.806495,7.4784174,7.809249),
                                 var = "AGB.prod"))) %>% mutate(value_m = value_m/10,
                                                                value_sd = value_sd/10,
                                                                value_sd = value_sd - value_m) %>% rename(obs_m = value_m,
                                                                                                          obs_sd = value_sd) %>%
  filter(year <= 2013)

modvsobs <- mod %>% left_join(obs)

modvsobs_agg <- modvsobs %>% group_by(simulation,var) %>% summarise(mod_m = mean(mod_m,na.rm=TRUE),
                                                                    mod_sd = mean(mod_sd,na.rm=TRUE),
                                                                    obs_m = mean(obs_m,na.rm=TRUE),
                                                                    obs_sd = mean(obs_sd,na.rm=TRUE))


data2plot <- modvsobs_agg %>% filter(var != "biomass.growth")

ggplot(data = data2plot,
       aes(x = obs_m,y = mod_m,color = as.factor(simulation),
           shape = as.factor(var))) +
  geom_point() +
  geom_errorbar(aes(ymin = mod_m - mod_sd,
                    ymax = mod_m + mod_sd)) +
  geom_errorbarh(aes(xmin = obs_m - obs_sd,
                     xmax = obs_m + obs_sd)) +
  geom_abline(slope = 1,intercept = 0,linetype = 3,color = "black")+
  scale_shape_manual(values = seq(1,length(unique(data2plot$var)))) +
  labs(color = "Treatment") +
  scale_x_continuous(name = "Observed C flux [kgC/m²/yr]") +
  scale_y_continuous(name = "Simulated C flux [kgC/m²/yr]") +
  scale_color_manual(values = c("#137300","#1E64C8")) +
  theme_bw() + theme(legend.position = c(0.15,0.9),
                     text = element_text(size = 18),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                     axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  guides(shape = FALSE)

ggsave(plot = last_plot(), dpi = 300,height = 23,width = 23,units = "cm",
       filename = file.path(".","Figures","carbon.flux.png"))


#r²
LM = lm(formula = mod_m ~ obs_m,data = data2plot)
summary(lm(formula = mod_m ~ obs_m,data = data2plot))$adj.r.squared

# RMSE
sqrt(sum(with(data2plot,(mod_m-obs_m)^2))/nrow(data2plot))

# rRMSE
sqrt(sum(with(data2plot,(mod_m-obs_m)^2))/nrow(data2plot))/median(data2plot$obs_m)
