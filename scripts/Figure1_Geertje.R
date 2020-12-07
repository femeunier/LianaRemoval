rm(list = ls())

library(dplyr)
library(albedo)
library(reshape2)
library(ggplot2)
library(LianaRemoval)

# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_short_control.RData","./outputs/"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_short_removal.RData","./outputs/"))

load("./outputs/Gigante_pft_short_control.RData")
control <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "control")
control_patch <- datum2df_patch(datum, vars = c("gpp","npp","agb.change","lai.change","agb","lai"),patches = "uniform") %>%
  mutate(patch_t = "control")

load("./outputs/Gigante_pft_short_removal.RData")
removal <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "removal")
removal_patch <- datum2df_patch(datum, vars = c("gpp","npp","agb.change","lai.change","agb","lai"),patches = "uniform") %>%
  mutate(patch_t = "removal")

OP <- bind_rows(list(control_patch,removal_patch))

# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control.RData","./outputs/"))
# system2("rsync",paste("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removal.RData","./outputs/"))
#
# load("./outputs/Gigante_pft_long_control.RData")
# control <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "control")
#
# load("./outputs/Gigante_pft_long_removal.RData")
# removal <- datum2df(datum,vars=c("gpp","agb"),pfts = c(2, 3, 4, 17), name = "removal")

OP2plot <- OP %>% group_by(patch_t,patch,years,var) %>% summarise(value = mean(value)) %>%
  group_by(patch_t,years,var)  %>% summarise(m = mean(value),
                                             sd = sd(value),
                                             n = length(value),
                                             se = sd/sqrt(length(value)))

pval <-  OP %>% group_by(patch_t,patch,years,var) %>% summarise(value = mean(value)) %>% summarise(p.val = summary(aov(formula = value ~ as.factor(patch_t)))[[1]][1,5])
OP2plot_pval <- OP2plot %>% left_join(pval) %>% mutate(signif = case_when(p.val <= 0.05 ~ "**",
                                                                          p.val <= 0.10 ~ "*",
                                                                          TRUE ~ "N.S."))


ggplot(data = OP2plot_pval %>% filter(var == "agb.change",
                                 years < 2014),
       aes(x =as.factor(years), fill = patch_t, y = m*10)) +
  geom_errorbar(aes(ymin = 0.9*m*10,ymax = 10*(m+se)),position = position_dodge(width = 0.9),width = 0.2) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(data = OP2plot_pval %>% filter(var == "agb.change",
                                    years < 2014,
                                    patch_t == "removal"),
            aes(label = signif, y = m*10*1.1)) +
  theme_bw()
