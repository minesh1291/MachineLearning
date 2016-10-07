#library(caret) # for dummyVars
#library(RCurl) # download https data
library(Metrics) # calculate errors
library(xgboost) # model

MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  # print (ll)
  return(ll);
}

###############################################################################

TRFile <- "TR_Mul_Feat.csv"

TrainSet <- read.csv(TRFile, header=F)
dim(TrainSet)
names(TrainSet) <- c(1:19,"class")
names(TrainSet)

###############################################################################

outcomeName <- c('class')
predictors <- names(TrainSet)[!names(TrainSet) %in% outcomeName]
adultsTrsf <- TrainSet

# play around with settings of xgboost - eXtreme Gradient Boosting (Tree) library
# https://github.com/tqchen/xgboost/wiki/Parameters
# max.depth - maximum depth of the tree
# nrounds - the max number of iterations

# take first 10% of the data only!
trainPortion <- floor(nrow(adultsTrsf)*0.4)

trainSet <- adultsTrsf[ 1:floor(trainPortion/2),]
testSet <- adultsTrsf[(floor(trainPortion/2)+1):trainPortion,]

#smallestError <- 100
#for (depth in seq(1,1,1)) {
#  for (rounds in seq(1000,1000,1)) {
    
    # train
	dtrain = xgb.DMatrix(data = as.matrix(trainSet[,predictors]), label = trainSet[,outcomeName])
	dtest = xgb.DMatrix(data = as.matrix(testSet[,predictors]), label = testSet[,outcomeName])
    watchlist <- list(train=dtrain, test=dtest)
    bst <- xgb.cv(data = dtrain,
                   max.depth=15, nround=10,num_class =3,
                   objective = "multi:softprob", verbose=1,eval_metric="mlogloss",
                   nthread=120,watchlist=watchlist,early.stop.round=3,nfold=5)
#    gc()
    
    # predict
#     predictions <- xgboost::predict(bst, as.matrix(testSet[,predictors]))
#     predictions <- matrix(predictions, ncol = 3, byrow = TRUE)
    # err <- rmse(as.numeric(testSet[,outcomeName]), as.numeric(predictions))
#     actual <- matrix(0, nrow(testSet), 3)
#     for (z in 1:nrow(testSet)){
#       actual[z,testSet[z,outcomeName]+1] <- 1
#     }
     # print (act)
    # print (dim(predictions))
#    err <- MultiLogLoss(actual,predictions)
#    if (err < smallestError) {
#      smallestError = err
#      print(paste("depth:",depth,"rounds:",rounds,"mlogloss:",err))
#      write(c("depth:",depth,"rounds:",rounds,"mlogloss:",err),ncolumns=6,sep="\t",file="result",append=T)
#    }     
#  }
#}  

###############################################################################
