rm(list = ls())

library(ggplot2)
library(dplyr)

system2("rsync",paste("-avz",paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/","output_removal_ensemble.RDS"),"./outputs/"))

tinit = 2011+3/12
t = seq(tinit,2014,length.out = 34)

ensemble <- readRDS(file.path("./outputs","output_removal_ensemble.RDS"))

RMSE <- ensemble[[2]]
select <- RMSE %>% filter(RMSE <= 5*min(RMSE)) %>% pull(isimu)

OP <- ensemble[[1]] %>% filter(ensemble %in% select)
OP <- OP %>% mutate(t = rep(t,nrow(OP)/length(t)))

obs <- OP %>% filter(ensemble == unique(OP$ensemble)[1])

ggplot() +
  geom_line(data = obs,
            aes(x = t, y = obs,color = patch_t),size = 2) +
  geom_line(data = OP,
            aes(x = t, y = Sim,color = patch_t,group = interaction(patch_t,as.factor(ensemble))),linetype = 2) +
  theme_bw()
