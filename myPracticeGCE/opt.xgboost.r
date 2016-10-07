rm(list=ls())
#library(caret) # for dummyVars
#library(RCurl) # download https data
library(Metrics) # calculate errors
library(xgboost) # model

###############################################################################
LOG = "tmp.log" #"CV_Train_Test.log"
STD = "tmp.std" #"CV_Train_Test.STD"
TRFile = "TR_Mul_Feat.csv"
###############################################################################

#~ skipFun(){

#~ 	}

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

# take first 40% of the data only!
#~ trainPortion <- floor(nrow(adultsTrsf)*0.4)
#~ trainSet <- adultsTrsf[ 1:floor(trainPortion/2),]
#~ testSet <- adultsTrsf[(floor(trainPortion/2)+1):trainPortion,]

# take random 40% of the data only!
size=nrow(adultsTrsf)*0.6
set.seed(1291)
selected=sample(nrow(adultsTrsf),size)
trainPortion <- adultsTrsf[selected,]
otherPortion <- adultsTrsf[-selected,]

dtrain = xgb.DMatrix(data = as.matrix(trainPortion[,predictors]), label = trainPortion[,outcomeName])
dtest = xgb.DMatrix(data = as.matrix(otherPortion[,predictors]), label = otherPortion[,outcomeName])

watchlist <- list(train=dtrain, test=dtest)

sink(STD, append=T, split=F)


  for (tmp.eta in seq(0.1,1,0.05)) { 					#n=19	#n=120 increase
    #~ for (tmp.eta in seq(0.3,0.3,0.001)) { 
    print (paste("tmp.eta",tmp.eta))
      for (tmp.cols in seq(0.3,0.3,-0.1)) { 					#n=1
        #~ for (tmp.cols in seq(0.3,0.3,0.1)) { 
        print (paste("tmp.cols",tmp.cols))
        for (tmp.fold in seq(5,5,10)) { 					#n=1
          #~ for (tmp.fold in seq(3,3,20)) { 
          print (paste("tmp.fold",tmp.fold))
          for (tmp.child in seq(3,3,-0.5)) { 					#n=1
            #~ for (tmp.child in seq(3,3,20)) { 
            print (paste("tmp.child",tmp.child))
            for (tmp.delta in c(1.4,1.8)) { 				#n=2 seq(2,0,-0.2)
              #~ for (tmp.delta in seq(10,10,0.1)) { 
              print (paste("tmp.delta",tmp.delta))
              for (tmp.depth in seq(30,20,-2)) {					#n=2  c(5,10,15,20)
                #~ for (tmp.depth in seq(5,5,1)) { 
                print (paste("tmp.depth",tmp.depth))
                
            
            ###############################################################################
            # parameters
            
            #~ nrounds=1000
            nrounds=100
            cv.nfolds=tmp.fold #10
            param <- list(
              max.depth=tmp.depth,		#5,
              eta=tmp.eta,				#0.6,
              max_delta_step=tmp.delta,	#1,
              colsample_bytree=tmp.cols,	#0.6,
              min_child_weight=tmp.child,	#1
              objective='multi:softprob',
              eval_metric="mlogloss",
              num_class =3,
              verbose=1,
              nthread = 120 
            )
            
            ###############################################################################
            #CV
            
            # train
            cvBegin=proc.time()
            bstCVres <- xgb.cv(
              param,
              data = dtrain,
              nrounds,
              subsample=0.6,
              early.stop.round=5,
              nfold=cv.nfolds)
            cvEnd=proc.time()
            cvTime=cvEnd-cvBegin
            
            mloglossCV=min(bstCVres[,test.mlogloss.mean])
            bstRoundCV=which.min(bstCVres[,test.mlogloss.mean])
            ###############################################################################
            #train
            
            trainBegin=proc.time()
            bst <- xgb.train(
              param,
              data = dtrain,
              bstRoundCV,
              watchlist=watchlist
            )
            gc()
            trainEnd=proc.time()
            trainTime=trainEnd-trainBegin
            
            ###############################################################################
            # predict
            
            predBegin=proc.time()
            predictions <- xgboost::predict(bst, as.matrix(otherPortion[,predictors]))
            predictions <- matrix(predictions, ncol = 3, byrow = TRUE)
            # err <- rmse(as.numeric(testSet[,outcomeName]), as.numeric(predictions))
            actual <- matrix(0, nrow(otherPortion), 3)
            for (z in 1:nrow(otherPortion)){
              actual[z,otherPortion[z,outcomeName]+1] <- 1
            }
            # print (act)
            # print (dim(predictions))
            err <- MultiLogLoss(actual,predictions)
            #print(param)
            #print(paste("External-mlogloss:",err))
            predEnd=proc.time()
            predTime=predEnd-predBegin
            
            ###############################################################################
            # logging
            
            write(
              c(
                "External-mlogloss: ",		round(err,4),
                "mloglossCV: ",				round(mloglossCV,4),
                
                "maxDepth: ",				param$max.depth,
                "min_child_weight: ", 		param$min_child_weight,
                "etaStep: ",				param$eta,
                "deltaStep: ",				param$max_delta_step,
                "colSample: ",				param$colsample_bytree,
                #~ 		"nrounds: ",				nrounds,
                
                "cvfolds: ",				cv.nfolds,
                #~ 		"mloglossCV: ",				round(mloglossCV,4),
                #~ 		"bstRoundCV: ",				bstRoundCV,
                "cvTime (sec): ",			round(cvTime[1:3],2),
                "trainTime (sec): ",		round(trainTime[1:3],2),
                
                #~ 		"External-mlogloss: ",		round(err,4),
                "predTime (sec): ",			round(predTime[1:3],2),
                
                "totalTime (sec): ",			round(predTime[3]+trainTime[3]+cvTime[3],2),
                "totalTime (min): ",			round((predTime[3]+trainTime[3]+cvTime[3])/60,2),
                
                "nrounds: ",				nrounds,
                "bstRoundCV: ",				bstRoundCV
                
              ),ncolumns=100,sep="\t",file=LOG,append=T)
            
            
            ###############################################################################
            
          }#tmp.child
        }#tmp.fold
      }#tmp.cols
    }#tmp.delta
  }#tmp.eta
}#tmp.depth

sink()
