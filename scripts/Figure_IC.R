rm(list = ls())

library(dplyr)
library(rhdf5)
library(cowplot)

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_control_147/analy/analysis-Q-2010-12-00-000000-g01.h5",
#                       "./outputs/control-2010-12.h5"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft_patch_removal_147/analy/analysis-Q-2010-12-00-000000-g01.h5",
#                       "./outputs/removal-2010-12.h5"))

h5file_control <- "./outputs/control-2010-12.h5"
mymont    = lapply(h5read_opt(h5file_control),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

df_control <- data.frame(pft = mymont$PFT,
                         Hite = mymont$HITE,
                         dbh = mymont$DBH,
                         patch = rep(1:length(mymont$PACO.N),mymont$PACO.N),
                         nplant = mymont$NPLANT,
                         LAI = mymont$LAI.CO,
                         Bleaf = mymont$BLEAF*mymont$NPLANT,
                         Bdead = mymont$BDEAD*mymont$NPLANT*0.7,
                         simulation = "control")

h5file_removal <- "./outputs/removal-2010-12.h5"
mymont    = lapply(h5read_opt(h5file_removal),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

df_removal <- data.frame(pft = mymont$PFT,
                         Hite = mymont$HITE,
                         dbh = mymont$DBH,
                         patch = rep(1:length(mymont$PACO.N),mymont$PACO.N),
                         nplant = mymont$NPLANT,
                         LAI = mymont$LAI.CO,
                         Bleaf = mymont$BLEAF*mymont$NPLANT,
                         Bdead = mymont$BDEAD*mymont$NPLANT*0.7,
                         simulation = "removal")

df <- bind_rows(list(df_control,df_removal))

Npatch <- max(df %>% filter(simulation == "control") %>% pull(patch))

df_sum <-
  df %>% group_by(simulation, pft, patch) %>% summarise(
    LAI = sum(LAI),
    LAI_top = sum(LAI[Hite > 0.95 *
                        max(Hite)]),
    Bd_tot = sum(Bdead),
    Bd = sum(Bdead[(dbh >
                      10 & pft != 17) | (dbh > 1 & pft == 17)]),
    Bl = sum(Bleaf)
  ) %>% mutate(patch = case_when(simulation == "control" ~ patch,
                                 simulation == "removal" ~ patch + Npatch)) %>% ungroup()


df_sum_to <- df_sum %>% group_by(simulation,patch) %>% summarise(LAI = sum(LAI),
                                                    Bd = sum(Bd)) %>% filter(patch %in% c(1:8,Npatch + seq(1,8))) %>%
  group_by(simulation) %>% summarise(LAIm = mean(LAI),
                                     LAIsd = sd(LAI),
                                     Bdm = mean(Bd),
                                     Bdsd = sd(Bd))

df_sum %>% group_by(simulation,patch,pft) %>% summarise(LAI = sum(LAI),
                                                    Bd = sum(Bd)) %>% filter(patch %in% c(1:8,Npatch + seq(1,8))) %>%
  group_by(simulation,pft) %>% summarise(LAIm = mean(LAI),
                                     LAIsd = sd(LAI),
                                     Bdm = mean(Bd),
                                     Bdsd = sd(Bd))

df_sum_GF <- df_sum %>% filter(patch %in% c(1:8,Npatch + seq(1,8))) %>% group_by(simulation,patch) %>% mutate(GF = case_when(pft == 17 ~ "Liana",
                                                                                                                             TRUE ~ "Tree")) %>%
  group_by(simulation,patch,GF) %>% summarise(LAI = sum(LAI),
                                        Bd = sum(Bd))  %>%
  group_by(simulation,GF) %>% summarise(LAIm = mean(LAI),
                                     LAIsd = sd(LAI),
                                     Bdm = mean(Bd),
                                     Bdsd = sd(Bd))
p.vals <-
  df_sum %>% group_by(simulation,patch) %>% summarise(LAI = sum(LAI),
                                                    Bd = sum(Bd)) %>% filter(patch %in% c(1:8,Npatch + seq(1,8))) %>% ungroup() %>%
  summarise(p.val_LAI = summary(aov(formula = LAI ~ as.factor(simulation)))[[1]][1,5],
            LAImax = max(LAI),
            p.val_Bd = summary(aov(formula = Bd ~ as.factor(simulation)))[[1]][1,5],
            Bdmax = max(Bd)) %>% mutate(signifLAI = case_when(p.val_LAI <= 0.05 ~ "**",
                                                              p.val_LAI <= 0.10 ~ "*",
                                                              TRUE ~ ""),
                                        signifBd  = case_when(p.val_Bd <= 0.05 ~ "**",
                                                              p.val_Bd <= 0.10 ~ "*",
                                                              TRUE ~ ""))


# LAI
LAIplot <-
  ggplot() +
  geom_errorbar(data = df_sum_to,
                aes(x = as.factor(simulation),y = LAIm, ymin = 0.9*LAIm,ymax = LAIm + LAIsd),width = 0.2) +
  geom_bar(data = df_sum %>% filter(patch %in% c(1:8,Npatch + seq(1,8))) %>% group_by(simulation,pft) %>% summarise(LAI = mean(LAI)),
           aes(x = as.factor(simulation),y = LAI,fill = as.factor(pft)),
           stat = "identity",position = "stack") +
  scale_fill_manual(values = c("#9FFF8C","#44CC29","#137300","#1E64C8","000000"),
                    labels = c("Early","Mid","Late","Liana","Tot")) +
  labs(x = "",y = "LAI",fill = "PFT") +
  geom_text(data = p.vals,
            aes(label = signifLAI,x = 1.5, y = LAImax*1.01),size = 8) +
  scale_y_continuous(expand = c(0,0.,0.1,0.)) +
  theme_bw() + guides(fill = FALSE) +
  theme(text = element_text(size = 24))

LAIpatch <-
  ggplot(data = df_sum %>% filter(patch %in% c(1:8,Npatch + seq(1,8))),
       aes(x = as.factor(patch),y = LAI,fill = as.factor(pft))) +
  geom_bar(stat = "identity",position = "stack") +
  scale_fill_manual(values = c("#9FFF8C","#44CC29","#137300","#1E64C8"),
                    labels = c("Early","Mid","Late","Liana")) +
  theme_bw()

# Bdead
Bdplot <-
  ggplot() +
  geom_errorbar(data = df_sum_to,
                aes(x = as.factor(simulation),y = Bdm, ymin = 0.9*Bdm,ymax = Bdm + Bdsd),width = 0.2) +
  geom_bar(data = df_sum %>% filter(patch %in% c(1:8,Npatch + seq(1,8))) %>% group_by(simulation,pft) %>% summarise(Bd = mean(Bd)),
           aes(x = as.factor(simulation),y = Bd,fill = as.factor(pft)),
           stat = "identity",position = "stack") +
  scale_fill_manual(values = c("#9FFF8C","#44CC29","#137300","#1E64C8"),
                    labels = c("Early","Mid","Late","Liana")) +
  geom_text(data = p.vals,
            aes(label = signifBd,x = 1.5, y = Bdmax*1.01),size = 8) +
  labs(x = "",y = "Bd",fill = "PFT") +
  scale_y_continuous(expand = c(0,0.,0.1,0.)) +
  theme_bw() +
  theme(text = element_text(size = 24))

Bdpatch <-
  ggplot(data = df_sum %>% filter(patch %in% c(1:8,Npatch + seq(1,8))),
       aes(x = as.factor(patch),y = Bd,fill = as.factor(pft))) +
  geom_bar(stat = "identity",position = "stack") +
  scale_fill_manual(values = c("#9FFF8C","#44CC29","#137300","#1E64C8"),
                    labels = c("Early","Mid","Late","Liana")) +
  theme_bw()

#########################################################################
Figure <- plot_grid(LAIplot,Bdplot,nrow = 1, rel_widths = c(0.85,1))
Figure

ggsave(plot = last_plot(),
       filename = "./Figures/Figure_IC.png",width = 12,height = 10,dpi = 300)

