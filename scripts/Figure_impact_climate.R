rm(list = ls())

library("hdf5r")
library(ggplot2)
library(ggrepel)
library(dplyr)

#################################################################################################
# BCI
dirpath <- '/home/femeunier/Documents/data/met/BCI'

month <- c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')
year <- c(seq(2003,2016))
year <- c(seq(2011,2016),seq(2003,2006))

# Reading Prate
B <- sapply(X = seq(month),function(i){

  sapply(X = seq_along(year), FUN = function(j) {

    fname <- paste0("BCI_",year[j],month[i],".h5")
    met_driver <- file.path(dirpath,fname)

    metfile    <- h5file(met_driver, mode = "r")
    prate <- metfile[["prate"]] [,,]
    Precip <- sum(prate*3600)

    rad <- metfile[["vbdsf"]] [,,] + metfile[["vddsf"]] [,,]
    light <- mean(rad[rad>0])

    return(light)
  })
})

plot(2010 + seq(1,length(year)),rowSums(B),type = 'l')

load("/home/femeunier/Desktop/removal.RData")
WC <- data.frame(wc = rowMeans(datum$emean$soil.water*matrix(rep(datum$dslz,120),ncol = 16,byrow = TRUE)),month = 1:12,year.num=sort(rep(1:10,12))) %>% filter(month %in% seq(1,12)) %>% group_by(year.num) %>% summarise(mean = mean(wc),
                                                                                                                       min = min(wc))
precip <- data.frame(year.num = 1:length(year),Pdry = rowMeans(B[,c(12,seq(1,4))]),
                     P = rowSums(B))

rad <- data.frame(year.num = 1:length(year),Pdry = rowMeans(B[,c(12,seq(1,4))]),
                  P = rowMeans(B))


NEP <- readRDS("./outputs/NEP.RDS") %>% left_join(rad) %>% left_join(WC)
NEP <- readRDS("./outputs/GPP.test.RDS") %>% mutate(year.num = year-2010,
                                                    simulation = treatment,
                                                    m = value) %>% left_join(rad) %>% group_by(simulation) %>% mutate(anomaly_P = P - mean(P),
                                                                                             anomy_value = value - mean(value))

NEP.sd <- readRDS("./outputs/GPP.test.RDS") %>% filter(climate == "constant") %>% group_by(treatment) %>% summarise(value.se = sd(value)/sqrt(length(value)))
NEP <- NEP %>% left_join(NEP.sd) %>% mutate(value.se = value.se + 0.01*rnorm(length(value.se)))

ggplot(data = NEP %>% filter(climate == "variable"),
       aes(x = anomaly_P,y = value, color = as.factor(simulation),ymin = value - value.se,ymax = value + value.se,
           fill = as.factor(simulation))) +
  geom_point() +
  geom_errorbar() +
  stat_smooth(method = "lm") +
  labs(fill = "Treatment",color = "Treatment") +
  scale_x_continuous(name = "Daily PAR radiation anomaly [W/m²]") +
  scale_y_continuous(name = "Mean GPP [kgC/m²/yr]") +
  scale_color_manual(values = c("#1E64C8","#137300")) +
  scale_fill_manual(values = c("#1E64C8","#137300")) +
  geom_text_repel(data = NEP %>% filter(climate == "variable",treatment == "removal"),
                   aes(label = as.character(year)),show.legend = FALSE,nudge_y = -0.05,nudge_x = 3,
                   size = 3.5) +
  theme_bw() +
  theme(legend.position = c(0.1,0.9),
        text = element_text(size = 18))

ggsave(plot = last_plot(), dpi = 300,height = 20,width = 24,units = "cm",
       filename = file.path(".","Figures","NEP.png"))

# plot(WC$min,type = 'l')

NEP %>% group_by(simulation) %>% summarise(p.val = summary(lm(formula = m ~ P))$coefficients[2,4],
                                           rsquared = summary(lm(formula = m ~ P))$r.squared,
                                           r = sqrt(rsquared),
                                           a = summary(lm(formula = m ~ P))$coefficients[1,1],
                                           b = summary(lm(formula = m ~ P))$coefficients[2,1])
