rm(list = ls())

system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_control.RData","./outputs/control_long.RData"))
system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/removal/out/analy/Gigante_pft_long_removal.RData","./outputs/removal_long.RData"))

load("./outputs/control_long.RData")
control <- datum

load("./outputs/removal_long.RData")
removal <- datum

plot(removal$emean$agb,type = 'l')
lines(control$emean$agb,type = 'l',col = 'red')

plot(removal$emean$agb[c(1,length(control$emean$agb))],type = 'l',col = 'black')
lines(control$emean$agb[c(1,length(control$emean$agb))],type = 'l',col = 'red')

-diff(control$emean$agb[c(1,length(control$emean$agb))])/50*10
-diff(removal$emean$agb[c(1,length(control$emean$agb))])/50*10
