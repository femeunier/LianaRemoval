rm(list = ls())

library(ParBayesianOptimization)
library(ggplot2)
library(xgboost)
library(doParallel)

###############################################################################
# Example 1
simpleFunction <- function(x) dnorm(x,3,2)*1.5 + dnorm(x,7,1) + dnorm(x,10,2)

# Find the x that maximizes our simpleFunction
xmax <- optim(8,simpleFunction,method = "L-BFGS-B",lower = 0, upper = 15,control = list(fnscale = -1))$par

# Get a visual
ggplot(data = data.frame(x=c(0,15)),aes(x=x)) +
  stat_function(fun = simpleFunction) +
  geom_vline(xintercept = xmax,linetype="dashed") +
  ggtitle("simpleFunction") +
  theme_bw()

bounds <- list(x=c(0,15))
initGrid <- data.frame(x=c(0,5,10))

FUN <- function(x) list(Score = simpleFunction(x))

set.seed(1)
optObjSimp <- bayesOpt(
  FUN = FUN
  , bounds = bounds
  , initGrid = initGrid
  , iters.n = 2
)

simpleFunction(getBestPars(optObjSimp)$x)/simpleFunction(7.023)

optObjSimp <- addIterations(optObjSimp,iters.n=5,verbose=0)
simpleFunction(getBestPars(optObjSimp)$x)/simpleFunction(7.023)

###############################################################################
# Example 2
data(agaricus.train, package = "xgboost")

Folds <- list(
  Fold1 = as.integer(seq(1,nrow(agaricus.train$data),by = 3))
  , Fold2 = as.integer(seq(2,nrow(agaricus.train$data),by = 3))
  , Fold3 = as.integer(seq(3,nrow(agaricus.train$data),by = 3))
)

scoringFunction <- function(max_depth, min_child_weight, subsample) {

  dtrain <- xgb.DMatrix(agaricus.train$data,label = agaricus.train$label)

  Pars <- list(
    booster = "gbtree"
    , eta = 0.001
    , max_depth = max_depth
    , min_child_weight = min_child_weight
    , subsample = subsample
    , objective = "binary:logistic"
    , eval_metric = "auc"
  )

  xgbcv <- xgb.cv(
    params = Pars
    , data = dtrain
    , nround = 100
    , folds = Folds
    , early_stopping_rounds = 5
    , maximize = TRUE
    , verbose = 0
  )

  print(max(xgbcv$evaluation_log$test_auc_mean))
  return(list(Score = max(xgbcv$evaluation_log$test_auc_mean),
              nrounds = xgbcv$best_iteration))
}

bounds <- list(
  max_depth = c(1L, 5L),
  min_child_weight = c(0, 25),
  subsample = c(0.25, 1))

set.seed(0)

tNoPar <- system.time(
  optObj <- bayesOpt(
    FUN = scoringFunction
    , bounds = bounds
    , initPoints = 4
    , iters.n = 4
    , iters.k = 1
  )
)

optObj$scoreSummary
getBestPars(optObj)

tNoPar

plot(optObj)

###################################################################

cl <- makeCluster(2)
registerDoParallel(cl)
clusterExport(cl,c('Folds','agaricus.train'))
clusterEvalQ(cl,expr= {
  library(xgboost)})

tWithPar <- system.time(
  optObj <- bayesOpt(
    FUN = scoringFunction
    , bounds = bounds
    , initPoints = 4
    , iters.n = 4
    , iters.k = 2
    , parallel = TRUE
  )
)
stopCluster(cl)
registerDoSEQ()

tWithPar

getBestPars(optObj)
