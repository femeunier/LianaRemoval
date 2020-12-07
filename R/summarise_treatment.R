#' summarise_treatment
#' @title summarise_treatment
#' @param h5file_control h5file_control
#' @param h5file_removal h5file_removal
#' @return df
#' @author FM
#' @export

summarise_treatment <- function(h5file_control,h5file_removal){

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


  df_sum_GF <- df_sum %>% filter(patch %in% c(1:8,Npatch + seq(1,8))) %>% group_by(simulation,patch) %>% mutate(GF = case_when(pft == 17 ~ "Liana",
                                                                                                                               TRUE ~ "Tree")) %>%
    group_by(simulation,patch,GF) %>% summarise(LAI = sum(LAI),
                                                Bd = sum(Bd))  %>%
    group_by(simulation,GF) %>% summarise(LAIm = mean(LAI),
                                          LAIsd = sd(LAI),
                                          Bdm = mean(Bd),
                                          Bdsd = sd(Bd))

  df_sum_PFT <- df_sum %>% filter(patch %in% c(1:8,Npatch + seq(1,8))) %>%
    group_by(simulation,patch,pft) %>% summarise(LAI = sum(LAI),
                                                Bd = sum(Bd))  %>%
    group_by(simulation,pft) %>% summarise(LAIm = mean(LAI),
                                          LAIsd = sd(LAI),
                                          Bdm = mean(Bd),
                                          Bdsd = sd(Bd))

  return(list(df_sum_GF,df_sum_PFT))

}
