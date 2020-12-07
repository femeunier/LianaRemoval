rm(list = ls())

library(albedo)
library(reshape2)
library(dplyr)
library(ggplot2)
library(cowplot)
library(purrr)
library(stringr)
library(zoo)

# system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/Test_pft29/analy/analysis.RData","./outputs/"))
# load("./outputs/analysis.RData")

# system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_removal.RData","./outputs/"))
# load("./outputs/Gigante_removal.RData")
#
system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long.RData","./outputs/"))
load("./outputs/Gigante_pft_long.RData")

# system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft.RData","./outputs/"))
# load("./outputs/Gigante_pft.RData")

# load("./outputs/Gigante_removal.RData")
# load("./outputs/Gigante_removal2.RData")
# load("./outputs/mod6/analysis.RData")
# load("./outputs/mod2_4/analysis.RData")


df <- datum2df(datum = datum,vars = c("gpp","npp"),pfts = c(3,17),name = "ref")

patches <- c(rep("control",8),rep("removal",8))
vars <- c("gpp","lai","agb","agb.change.tree","agb.change","agb.tree","fast.soil.c","struct.soil.c","slow.soil.c","npp")

patch_types <- unique(patches)
df <- data.frame()

# Data
tinit = 2011+2/12
t = seq(tinit,2014,length.out = 34)
Bcon <- (108.6 - 25.6*(exp(-0.0162246*(t-tinit))))/10
Brem <- (166.5 -83.5*(exp(-0.03574508*(t-tinit))))/10

Bcon <- (108.6 + 6.7 - 25.6*(exp(-0.0162246*(t-tinit))))/10
Brem <- (166.5 - 4 -83.5*(exp(-0.03574508*(t-tinit))))/10

Delta_Bcon <- Bcon - Bcon[1]
Delta_Brem <- Brem - Brem[1]
Delta_obs <- c(Delta_Bcon,Delta_Brem)

for (patch_type in seq(1,length(patch_types))){
  cpatch <- which(patches == patch_types[patch_type])

  for (ivar in seq(1,length(vars))){

    cvar <- vars[ivar]
    if (!(cvar %in% c("agb.tree","agb.change.tree"))){
      values <- unlist(lapply(datum$patch[[cvar]],"[",c(cpatch)))
      if (cvar == "agb.change"){
        init_values <- sapply(1:length(cpatch),function(i){unlist(lapply(datum$patch[["agb"]],"[",cpatch[i]))[1]})
        uni <- unlist(lapply(datum$patch[["agb"]],"[",c(cpatch[1])))
        values <- unlist(lapply(datum$patch[["agb"]],"[",c(cpatch))) - rep(init_values,length(uni))
      }
    } else {
      values <- unlist(lapply(datum$patch[["agb"]],"[",c(cpatch)))

      agb.liana <- unlist(map(1:length(datum$cohort$agb),function(i){
        temp <- data.frame(agb=datum$cohort$agb[[i]],
                           nplant = datum$cohort$nplant[[i]]/datum$cohort$area[[i]],
                           pa = datum$cohort$ipa[[i]],
                           dbh = datum$cohort$dbh[[i]],
                           pft = datum$cohort$pft[[i]]) %>% filter(pft == 17,
                                                                   dbh < Inf,
                                                                   pa %in% cpatch) %>%
        group_by(pa) %>% summarise(S = sum(agb*nplant)) %>% pull(S)
        return(c(temp))}))

      if (length(agb.liana)>0){values <- values - agb.liana}

      if (cvar == "agb.change.tree"){
        init_values <- values[1:length(cpatch)]
        uni <- unlist(lapply(datum$patch[["agb"]],"[",c(cpatch[1])))
        values <- values - rep(init_values,length(uni))
      }
    }

    Vnames <- names(values)
    years <- as.numeric(substr(Vnames,2,5))
    months <- as.numeric(substr(Vnames,7,8))

    if (length(cpatch >1)){
      patch <- as.numeric(substr(Vnames,9,9))
    } else{
      patch = cpatch
    }

    df <- bind_rows(list(df,
                         data.frame(value = values, var = cvar, year = years, month = months,patch_num = patch, patch_type = patch_types[patch_type])))
  }
}

# tmp <- df %>% filter(patch_num %in% seq(1,8)) %>% mutate(time = year + (month-1)/12)
# plot(tmp %>% filter(var == "lai") %>% pull(value))
# plot(tmp %>% filter(var == "agb") %>% pull(time),
#      tmp %>% filter(var == "agb") %>% pull(value))


df_sum <- df %>% mutate(time = year + (month-1)/12) %>% group_by(patch_type,time,var) %>%
  summarise(value_m = mean(value),
            value_low = value_m - 1.96*sd(value/sqrt(length(value))),
            value_high = value_m + 1.96*sd(value/sqrt(length(value)))) %>% ungroup()

vars <- c("gpp","npp","agb.change","lai.change","agb","lai")
# vars <- c("npp")
df_sum2 <- data.frame()
for (patch_type in seq(1,length(patch_types))){
  cpatch <- which(patches == patch_types[patch_type])
  for (ivar in seq(1,length(vars))){

    cvar <- vars[ivar]
    if (cvar == "agb.change"){
      cvar = "agb"
    } else if (cvar == "lai.change"){
      cvar = "lai"
    }

    dbh <- unlist(datum$cohort$dbh)
    pft <- c(unlist(datum$cohort$pft))
    if(cvar %in% c("agb","npp")){
      pos <- as.vector(which((pft == 17 & dbh >= 1) | (pft != 17 & dbh >= 10)))
      # pos <- as.vector(1:length(pft))
    } else {
      pos <- as.vector(1:length(pft))
    }

    ipa <- c(unlist(datum$cohort$ipa))[pos]
    pa_area <- c(unlist(datum$cohort$area))[pos]
    nplant <- c(unlist(datum$cohort$nplant)[pos]/pa_area)
    values <- c(unlist(datum$cohort[[cvar]]))[pos]
    pft <- pft[pos]
    dbh <- dbh[pos]

    if (vars[ivar] %in% c("lai","lai.change")){values <- values/nplant}

    Vnames <- names(values)
    years <- as.numeric(substr(Vnames,2,5))
    months <- as.numeric(substr(Vnames,7,8))

    temp_df <- data.frame(patch = ipa,pft,pa_area,values,nplant,years,months)

    cdf <- temp_df %>% filter(patch %in% cpatch) %>% group_by(pft,patch,years,months) %>% summarise(S=sum(values*nplant)) %>%
      rename(value = S) %>% ungroup() %>% mutate(patch_t = patch_types[patch_type],
                                                 var = vars[ivar])

    cdf_tot <- temp_df %>% filter(patch %in% cpatch) %>% group_by(patch,years,months) %>% summarise(S=sum(values*nplant)) %>%
      rename(value = S) %>% ungroup() %>% mutate(patch_t = patch_types[patch_type],
                                                 var = vars[ivar],
                                                 pft = 18)

    if (vars[ivar] %in% c("agb.change","lai.change")){
      cdf <- cdf %>% group_by(pft,patch) %>% mutate(value = value - mean(value[c(1)]))
      cdf_tot <- cdf_tot %>% group_by(patch) %>% mutate(value = value - mean(value[c(1)]))
    }

    df_sum2 <- bind_rows(list(df_sum2,cdf,cdf_tot))

  }
}

df_sum3 <- df_sum2 %>% mutate(GF = case_when(pft %in% c(2,3,4) ~ "Tree",
                                             pft == 17 ~ "Liana",
                                             pft == 18 ~ "Ecosystem")) %>% group_by(GF,var,patch_t,patch,years,months) %>%
  summarise(value = sum(value))

# Test quality of fit
Delta_sims <- df_sum2 %>% filter(var == "agb.change",pft == 18,years < 2014) %>% group_by(patch_t,years,months) %>% summarise(V = mean(value)) %>% pull(V)

RMSE <- sqrt(sum((Delta_sims - Delta_obs)**2)/length(Delta_sims))


# tmp <- df_sum2 %>% filter(patch %in% seq(9,16)) %>% mutate(time = years + (months-1)/12)
#
# plot(tmp %>% filter(var == "agb") %>% pull(time),
#      tmp %>% filter(var == "agb") %>% pull(value),type='l')


ggplot(data = df_sum3) +
  geom_boxplot(aes(x = patch_t,y = value,fill = as.factor(GF)),alpha = 0.6) +
  facet_grid(var ~ as.factor(GF),scales = "free") +
  geom_hline(yintercept = 0,linetype = 3) +
  scale_fill_manual(values = c("darkgrey","darkblue","darkgreen")) +
  theme_bw()

ggplot(data = df_sum3 %>% filter(var %in% c("gpp","npp","agb"))) +
  geom_boxplot(aes(x = patch_t,y = value,fill = as.factor(GF)),alpha = 0.6) +
  facet_grid(var ~ as.factor(GF),scales = "free") +
  geom_hline(yintercept = 0,linetype = 3) +
  scale_fill_manual(values = c("darkgrey","darkblue","darkgreen")) +
  theme_bw()

df_sum_bis <- df_sum2 %>% filter(pft == 18) %>% mutate(time = years + (months)/12) %>% group_by(patch_t,time,var) %>%
  summarise(value_m = mean(value),
            value_low = value_m - 1.96*sd(value/sqrt(length(value))),
            value_high = value_m + 1.96*sd(value/sqrt(length(value)))) %>% ungroup()

ggplot(data = df_sum_bis) +
  geom_line(aes(x = time,y = value_m,color = as.factor(patch_t))) +
  geom_ribbon(aes(x = time,ymin = value_low,ymax = value_high,fill = as.factor(patch_t)),color = NA,alpha = 0.4) +
  facet_wrap(. ~ var,scales = "free") +
  theme_bw()

df_sum2 %>% filter(var == "agb",years == 2011,months == 3) %>% group_by(patch_t,pft) %>% summarise(v = mean(value))

tinit = 2011+2/12
t = seq(tinit,
        df_sum2 %>% pull(years) %>%max(),
        length.out = 1000)
#t = 2012

Bcon = (108.6 - 25.6*(exp(-0.0162246*(t-tinit))))-7
Brem = 166.5 - 83.5*(exp(-0.03574508*(t-tinit)))
# plot(t,Brem/10,col = 'red',type='l')
# lines(t,Bcon/10,type='l')

df_data <- data.frame(time = c(t,t),
                      B = c(Bcon,Brem),
                      Bchange = c(Bcon-Bcon[1],Brem-Brem[1]),
                      type = c(rep("control",length(t)),rep("removal",length(t))))



df_patches <- df_sum2 %>% filter(pft == 18) %>% mutate(time = years + (months)/12)

ggplot() +
  geom_line(data = df_patches %>% filter(patch %in% seq(1,8)),
            aes(x = time,y = value,group=interaction(patch_t,patch)),color = "black",linetype = 2) +
  geom_line(data = df_patches %>% filter(patch %in% c(4)),
            aes(x = time,y = value,color = patch_t,group=interaction(patch_t,patch)),linetype = 2) +
  facet_wrap(. ~ var,scales = "free") +
  theme_bw()

ggplot() +
  geom_line(data = df_data,
            aes(x = time,y = Bchange/10, col = as.factor(type))) +
  geom_line(data = df_sum_bis %>% filter(var == "agb.change"),
            aes(x = time,y = value_m,color = patch_t),linetype = 1,size=2) +
  geom_line(data = df_patches %>% filter(var == "agb.change"),
            aes(x = time,y = value,color = patch_t,group=interaction(patch_t,patch)),linetype = 2) +
  # geom_line(data = df_patches %>% filter(var == "agb.change",patch %in% c(3,5,7)),
  # aes(x = time,y = value,color = patch_t,group=interaction(patch_t,patch)),linetype = 2) +
  # scale_y_continuous(limits=c(0,1)) +
  # scale_x_continuous(limits=c(2011,2017)) +
  facet_wrap(. ~ var,scales = "free") +
  theme_bw()

ggplot(data = df_sum) +
  geom_line(aes(x = time,y = value_m,color = patch_type)) +
  geom_ribbon(aes(x = time,ymin = value_low,ymax = value_high,fill = patch_type),color = NA,alpha = 0.4) +
  facet_wrap(. ~ var,scales = "free") +
  # scale_x_continuous(limits = c(2011,2014)) +
  theme_bw()


ggplot() +
  geom_line(data = df_data,
            aes(x = time,y = Bchange, col = as.factor(type)),linetype = 2) +
  geom_line(data = df_sum %>% filter(var == "agb.change.tree"),
            aes(x = time,y = value_m*10,color = patch_type)) +
  geom_ribbon(data = df_sum %>% filter(var == "agb.change.tree"),
              aes(x = time,ymin = value_low*10,ymax = value_high*10,fill = patch_type),color = NA,alpha = 0.4) +
  # scale_x_continuous(limits = c(2011,2014)) +
  # scale_y_continuous(limits = c(-4,10)) +
  scale_color_manual(values = c("#1E64C8","#137300")) +
  scale_fill_manual(values = c("#1E64C8","#137300")) +
  labs(x = "", y = "Above-ground biomass change [Mg C/ha]",fill = "Treatment") +
  guides(color = FALSE) +
  theme_bw()


ggplot() +
  geom_line(data = df_data,
            aes(x = time,y = B, col = as.factor(type)),linetype = 2) +
  geom_line(data = df_sum %>% filter(var == "agb.tree"),
            aes(x = time,y = value_m*10,color = patch_type)) +
  geom_ribbon(data = df_sum %>% filter(var == "agb.tree"),
              aes(x = time,
                  ymin = value_low*10,
                  ymax = value_high*10,
                  fill = patch_type),color = NA,alpha = 0.4) +
  # scale_x_continuous(limits = c(2011,2014)) +
  scale_color_manual(values = c("#1E64C8","#137300")) +
  scale_fill_manual(values = c("#1E64C8","#137300")) +
  labs(x = "", y = "Above-ground biomass change [Mg C/ha]",fill = "Treatment") +
  guides(color = FALSE) +
  theme_bw()


