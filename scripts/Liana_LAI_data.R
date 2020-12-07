rm(list = ls())

data.LAI <-
  as.data.frame(rbind(c(2011,6.507874,"control"),
                      c(2011,5.149606,"removal"),
                      c(2012,6.03937,"control"),
                      c(2012,4.8700786,"removal"),
                      c(2013,5.968504,"control"),
                      c(2013,5.4488187,"removal"),
                      c(2014,7.5984254,"control"),
                      c(2014,6.984252,"removal"),
                      c(2015,5.8307085,"control"),
                      c(2015,5.7834644,"removal"))) %>% rename(time = V1,LAI = V2,patch_t = V3)
