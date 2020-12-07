#' datum2df_patch.cohort
#' @title datum2df_patch.cohort
#' @param datum datum
#' @param vars vars
#' @param patches pfts
#' @return df
#' @author FM
#' @export

datum2df_patch.cohort <- function(datum, vars = c("agb.recr","agb.mort")){

  df.all <-
    map_dfr(1:length(datum$month),function(i){

    dlndbhtdt <- datum$cohort$growth[[i]]/100
    dbh <- datum$cohort$dbh[[i]]
    agbconow <- datum$cohort$agb[[i]]
    pft <- datum$cohort$pft[[i]]
    pa_area <- datum$cohort$area[[i]]
    pa <- datum$cohort$ipa[[i]]
    nplant <- datum$cohort$nplant[[i]]/pa_area
    w.nplant <- nplant * pa_area
    dbhconow.1ago <- dbh * exp(-pmax(0,dlndbhtdt))
    agb.growthconow <- datum$cohort$agb.growth[[i]]/100
    agbcolmon <- agbconow * exp(-agb.growthconow/12.)
    mortconow <- -log(1 - datum$cohort$mort[[i]]/100)

    df.pa <- data.frame(patch = 1:length(datum$patch$area[[i]]), pa_area = datum$patch$area[[i]])
    # dbhconow.lastmon = dbh * exp(-pmax(0,dlndbhtdt/12))

    pfts <- c(2,3,4,17)

    recr <- mort <- agb <- matrix(NA,nrow = max(pa),ncol = length(pfts))

    for (ipa in seq(1,max(pa))){
      cpatch <- unique(pa)[ipa]
      for (ipft in seq(1,length(pfts))){

        cpft <- pfts[ipft]

        if (cpft != 17){
          sel.pop <- (pft == cpft) & (pa == cpatch) & (dbh >= 10)
          sel.est <- (pft == cpft) & (pa == cpatch) & (dbhconow.1ago >= 10)
          sel <- (pft == cpft) & (pa == cpatch) & (dbh >= 15)
        } else {
          sel.pop <- (pft == cpft) & (pa == cpatch) & (dbh >= 1.)
          sel.est <- (pft == cpft) & (pa == cpatch) & (dbhconow.1ago >= 1.)
          sel <- (pft == cpft) & (pa == cpatch) & (dbh >= 1.1)
        }

        population <- sum(nplant[sel.pop] * agbconow[sel.pop])
        established <- sum(nplant[sel.est] * agbconow[sel.est])
        recr[ipa,ipft] = exp(log(population / established)) - 1.0

        survivor <- sum(nplant[sel] * agbcolmon[sel])
        previous <- sum(nplant[sel] * agbcolmon[sel]* exp(mortconow[sel] ) )
        mort[ipa,ipft] = 1.0 - exp(- log(previous/survivor))

        agb[ipa,ipft] <- sum(nplant[sel] * agbconow[sel])

      }
    }

    recr[is.na(recr)] <- mort[is.na(mort)] <- agb[is.na(agb)] <- 0
    colnames(recr) <- colnames(mort) <- colnames(agb) <- pfts
    rownames(recr) <- rownames(mort) <- rownames(agb) <- 1:max(pa)

    df <-
      left_join(
        bind_rows(list(as.data.frame(melt(recr*agb) %>% rename(patch = Var1,
                                                               pft = Var2)) %>% mutate(var = "agb.recr"),
                       as.data.frame(melt(mort*agb) %>% rename(patch = Var1,
                                                               pft = Var2)) %>% mutate(var = "agb.mort"),
                       as.data.frame(melt(agb) %>% rename(patch = Var1,
                                                          pft =  Var2)) %>% mutate(var = "agb"))),
        df.pa, by = "patch")

    return(df %>% mutate(years = datum$year[i],
                         months = datum$month[i]))
  })

  return(df.all %>% filter(var %in% vars))
}
