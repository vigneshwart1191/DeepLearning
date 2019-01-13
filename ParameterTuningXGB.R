#Read the data
#Train1 = read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vigneshwar/Results/ModelData/TrainData.csv")
#Vald1 = read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vigneshwar/Results/ModelData/ValidationOne.csv")
#Vald2 = read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vigneshwar/Results/ModelData/ValidationTwo.csv")
#Vald3 = read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vigneshwar/Results/ModelData/ValidationThree.csv")
#Test1 = read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vigneshwar/Results/ModelData/TestData.csv")
#####
Train1 = trainData[,c(-1,-2,-140)]
Vald1 = TestSet1[,c(-1,-2,-140)]
Vald2 = TestSet2[,c(-1,-2,-140)]#read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vigneshwar/Results/ModelData/ValidationTwo.csv")
Vald3 = TestSet3[,c(-1,-2,-140)]#read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vigneshwar/Results/ModelData/ValidationThree.csv")
Test1 = validationData[,c(-1,-2,-140)]#read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vigneshwar/Results/ModelData/TestData.csv")




#####
# Automated Parameter Tuning (F-Score)
# Need to change target variable name to 'Target'
# tunes 'eta', 'max_depth', 'nround'
##################################
##################################
start.time <- Sys.time()

fscore_val1 = 0
fscore_val2 = 0
fscore_val3 = 0
fscore_val1_best = 0
fscore_val2_best = 0
fscore_val3_best = 0

for (i in seq(2, 60, 5))
{ for (j in seq(4, 14, 2))
{ for (k in seq(20, 100, 10))
{
  Temp<- subset(Train1,select=-TargetVariable) # remove Target Variable
  set.seed(49)
  xgb <- xgboost(data = data.matrix(Temp), 
                 label = Train1$TargetVariable, 
                 #max_delta_step = 10, # not needed
                 eta = i/100, # ***tuning (learning rate/step size shrinkage - details below the code)(0-1)
                 max_depth = j, # ***tuning (max tree depth)(3-20)
                 nround=k, # ***tuning (max number of iterations/trees)(10-200)
                 gamma=0.3, # *tuning (Minimum Loss Reduction to make further partition on leaf node)(0.1-5)
                 # min_child_weight=8, # minimum sum of instance needed in a child (1-10)
                 subsample = 0.5, # *tuning (random sample for a tree)
                 colsample_bytree = 0.5, # *tuning (random sample of columns for a tree)
                 booster = "gbtree", # 'gblinear' option also present
                 seed = 1,
                 eval_metric = "auc",
                 objective = "binary:logistic", # "multi:softprob" for multiclass target
                 #num_class = 12, # for multiclass target
                 lambda = 0.75, # *tuning (L1 Reg - parameter for linear booster)
                 alpha = 2.74, # *tuning (L2 Reg - parameter for linear booster) old 2.74
                 nthread = 3 # number of parallel executions
  )
  
  ## Optimizing Model on AUC:
  
  # Calculate fscore and Confusion Matrix for Vald1
  Temp <- subset(Vald1,select=-TargetVariable)
  Temp$Pred <- predict(xgb, data.matrix(Temp))
  Temp$Pred = ifelse(Temp$Pred > 0.5, 1, 0)
  xtab <- table(Temp$Pred, Vald1$TargetVariable)
  CF = confusionMatrix(xtab, positive = "1")
  fscore_val1= 2*CF$byClass[1]*CF$byClass[3]/(CF$byClass[1]+CF$byClass[3])
  
  # Calculate fscore and Confusion Matrix for Vald2
  Temp <- subset(Vald2,select=-TargetVariable)
  Temp$Pred <- predict(xgb, data.matrix(Temp))
  Temp$Pred = ifelse(Temp$Pred > 0.5, 1, 0)
  xtab <- table(Temp$Pred, Vald2$TargetVariable)
  CF = confusionMatrix(xtab, positive = "1")
  fscore_val2= 2*CF$byClass[1]*CF$byClass[3]/(CF$byClass[1]+CF$byClass[3])
  
  # Calculate fscore and Confusion Matrix for Vald3
  Temp <- subset(Vald3,select=-TargetVariable)
  Temp$Pred <- predict(xgb, data.matrix(Temp))
  Temp$Pred = ifelse(Temp$Pred > 0.5, 1, 0)
  xtab <- table(Temp$Pred, Vald3$TargetVariable)
  CF = confusionMatrix(xtab, positive = "1")
  fscore_val3= 2*CF$byClass[1]*CF$byClass[3]/(CF$byClass[1]+CF$byClass[3])
  
  if(fscore_val1 > fscore_val1_best & fscore_val2 > fscore_val2_best & fscore_val3 > fscore_val3_best)
  {
    fscore_val1_best = fscore_val1
    fscore_val2_best = fscore_val2
    fscore_val3_best = fscore_val3
    eta_best = i
    max_depth_best = j
    nround_best = k
    xgb_model_best_fscore = xgb
  }
  
  print(i) # to check code progress
  print(paste0("eta_best: ", eta_best/100)) # to check best eta in progress
  print(paste0("max_depth_best: ", max_depth_best)) # to check best eta in progress
  print(paste0("nround_best: ", nround_best)) # to check best eta in progress
  print(paste0("fscore_val1_best: ", fscore_val1_best)) # to check best auc1 in progress
  print(paste0("fscore_val2_best: ", fscore_val2_best)) # to check best auc1 in progress
  print(paste0("fscore_val3_best: ", fscore_val3_best)) # to check best auc1 in progress
  
}
}
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

##################################
##################################
