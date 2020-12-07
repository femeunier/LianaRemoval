rm(list = ls())

library(rhdf5)
library(dplyr)

source("~/Documents/ED2/R-utils/h5read_opt.r")

h5file <- file.path("~/Downloads/",
                    "analysis-Q-2010-12-00-000000-g01.h5")

mymont    = lapply(h5read_opt(h5file),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")
c(mean(mymont$FAST.SOIL.C),mean(mymont$SLOW.SOIL.C),mean(mymont$STRUCTURAL.SOIL.C))
