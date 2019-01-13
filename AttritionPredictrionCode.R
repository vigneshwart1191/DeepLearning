set.seed(100)
library(lubridate)
library(sqldf)
library("janitor")
library(dplyr)
library(plyr)
library(randomForest)
library(pROC)
library(doBy)
library(missForest)
library(caret)
library(e1071)
library(dummies)
library(xgboost)
##########Static Data processing##########
staticData = read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vinu/StaticData.csv")
staticData = staticData[,c(1,2,5,6,8,9,10,12,13,14,15,16,19)]
##########
#staticData$Unique.Emp.Number = toupper(staticData$Unique.Emp.Number)
################
staticData$Last.Date.Worked = as.character(staticData$Last.Date.Worked)
#x = which(staticData$Hire.Status =="I" & staticData$Last.Date.Worked =="")
#staticData = staticData[-x,]
#Date conversion for date of joining and date of resignation
staticData$Date.of.Joining = as.character(staticData$Date.of.Joining)
staticData$Date.of.Joining = gsub(" ", "-", staticData$Date.of.Joining)
staticData$Date.of.Joining = format(strptime(staticData$Date.of.Joining, format = "%d-%b-%Y"), "%d/%m/%Y")
staticData$Date.of.Joining = as.Date(staticData$Date.of.Joining,format = "%d/%m/%Y")
staticData$Last.Date.Worked = as.character(staticData$Last.Date.Worked)
staticData$Last.Date.Worked = gsub(" ", "-", staticData$Last.Date.Worked)
staticData$Last.Date.Worked = format(strptime(staticData$Last.Date.Worked, format = "%d-%b-%Y"), "%d/%m/%Y")
staticData$Last.Date.Worked = as.Date(staticData$Last.Date.Worked,format = "%d/%m/%Y")

#Remove Intern
#InternID = c("51-Intern (CS)","53-Intern (CBS)")
#staticData = filter(staticData,!(staticData$Rank.Code.Name %in% InternID))
#####################Rules logic for static duplicates#################
staticData$Unique.Emp.Number = as.factor(staticData$Unique.Emp.Number)
ind1 = which(duplicated(staticData$Unique.Emp.Number))
nuStaticData = staticData[ind1,]
empId = unique(nuStaticData$Unique.Emp.Number)
rm(nuStaticData)
processFullData1 = filter(staticData,!(staticData$Unique.Emp.Number %in% empId))
DuplicateData = filter(staticData,(staticData$Unique.Emp.Number %in% empId))
x = which(DuplicateData$Hire.Status =="I" & is.na(DuplicateData$Last.Date.Worked))
rData = DuplicateData[x,]
remID = unique(rData$Unique.Emp.Number)
DuplicateData1 = filter(DuplicateData,!(DuplicateData$Unique.Emp.Number %in% remID))
processData = data.frame(matrix(ncol = 22))
processFullData = data.frame(matrix(ncol = 22))

#########Loop will take only which employee has same date of joining
for(i in 1:length(empId)){
  #print(i)
  #singleRecord = filter(s,s$Unique.Emp.Number %in% empId[i])
  singleRecord = filter(DuplicateData1,DuplicateData1$Unique.Emp.Number %in% empId[i])
  if(length(unique(singleRecord$Date.of.Joining))==1){
    if(length(which(singleRecord$Hire.Status == "A")) == 1){
      processData = subset(singleRecord,singleRecord$Hire.Status=="A")
    }else{
      processData = subset(singleRecord,singleRecord$Last.Date.Worked == max(singleRecord$Last.Date.Worked))
    }
    if(i == 1){
      processFullData = processData
    }else{
      processFullData = rbind(processFullData,processData)
    }
  }
  
}

staticProcessData = rbind(processFullData,processFullData1)

#staticData12 = subset(staticData,staticData$Date.of.Joining >=  "2013-07-01"
#                    & staticData$Date.of.Resignation <= "2014-06-30")
###Removing employees who join and resign on same years
sx13 = which(staticProcessData$Date.of.Joining >=  "2012-07-01"
             & staticProcessData$Last.Date.Worked <= "2013-06-30")
sx14 = which(staticProcessData$Date.of.Joining >=  "2013-07-01"
             & staticProcessData$Last.Date.Worked <= "2014-06-30")
sx15 = which(staticProcessData$Date.of.Joining >=  "2014-07-01"
             & staticProcessData$Last.Date.Worked <= "2015-06-30")
sx16 = which(staticProcessData$Date.of.Joining >=  "2015-07-01"
             & staticProcessData$Last.Date.Worked <= "2016-06-30")
sx17 = which(staticProcessData$Date.of.Joining >=  "2016-07-01"
             & staticProcessData$Last.Date.Worked <= "2017-06-30")
staticProcessData = staticProcessData[c(-sx13,-sx14,-sx15,-sx16,-sx17),]


#Calculated actaual age as per employee resignation for inactive employees
staticTerminate = filter(staticProcessData,(staticProcessData$Hire.Status %in% "I"))
staticActive = filter(staticProcessData,(staticProcessData$Hire.Status %in% "A"))

#REFERENCE DATA
staticTerminate$ReferenceDate = "30 Nov 2017"
staticTerminate$ReferenceDate = as.character(staticTerminate$ReferenceDate)
staticTerminate$ReferenceDate = gsub(" ", "-", staticTerminate$ReferenceDate)
staticTerminate$ReferenceDate = format(strptime(staticTerminate$ReferenceDate, format = "%d-%b-%Y"), "%d/%m/%Y")
staticTerminate$ReferenceDate = as.Date(staticTerminate$ReferenceDate,format = "%d/%m/%Y")
staticTerminate$DaysDifference = staticTerminate$ReferenceDate - staticTerminate$Last.Date.Worked
staticTerminate$DaysDifference = (staticTerminate$DaysDifference)/365
staticTerminate$DaysDifference = round(staticTerminate$DaysDifference, digits = 1)
staticTerminate$Age = (staticTerminate$Age)-(staticTerminate$DaysDifference)
staticTerminate = staticTerminate[,1:13]
#EXPERIENCE CALCULATION
staticTerminate$EyExperience = ((staticTerminate$Last.Date.Worked)-(staticTerminate$Date.of.Joining))/365
staticTerminate$EyExperience = round(staticTerminate$EyExperience, digits = 1)
#REFERENCE DATA
staticActive$ReferenceDate = "30 Nov 2017"
staticActive$ReferenceDate = as.character(staticActive$ReferenceDate)
staticActive$ReferenceDate = gsub(" ", "-", staticActive$ReferenceDate)
staticActive$ReferenceDate = format(strptime(staticActive$ReferenceDate, format = "%d-%b-%Y"), "%d/%m/%Y")
staticActive$ReferenceDate = as.Date(staticActive$ReferenceDate,format = "%d/%m/%Y")
staticActive$EyExperience = staticActive$ReferenceDate - staticActive$Date.of.Joining
staticActive$EyExperience = (staticActive$EyExperience)/365
staticActive$EyExperience = round(staticActive$EyExperience, digits = 1)
staticActive = staticActive[,c(1:13,15)]
staticFullData = rbind(staticActive,staticTerminate)
#Total Experience and Number of companies worked
staticFullData$Work.ex.prior.to.EY  = gsub("-", "0", staticFullData$Work.ex.prior.to.EY)
staticFullData$Number.of..Companies.Worked  = gsub("-", "0", staticFullData$Number.of..Companies.Worked)
staticFullData$Work.ex.prior.to.EY = as.numeric(staticFullData$Work.ex.prior.to.EY)
staticFullData$Number.of..Companies.Worked = as.numeric(staticFullData$Number.of..Companies.Worked)
staticFullData$TotalExperience = (staticFullData$Work.ex.prior.to.EY)+(staticFullData$EyExperience)
staticFullData$TotalExperience = round(staticFullData$TotalExperience, digits = 1)
staticFullData$AverageTimeSpent = ((staticFullData$TotalExperience)/((staticFullData$Number.of..Companies.Worked)+1))
staticFullData$AverageTimeSpent = round(staticFullData$AverageTimeSpent, digits = 1)
#staticFullData[is.na(staticFullData[,16]),16] <- 2
staticFullData$EyExperience = as.numeric(staticFullData$EyExperience)
staticFullData$TotalExperience = as.numeric(staticFullData$TotalExperience)
staticFullData$AverageTimeSpent = as.numeric(staticFullData$AverageTimeSpent)
#########################Yearly Data Processing############################
#Takes only the year
yearRecord  = read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vinu/YearlyUpdatedData.csv",nrows = 1,header = F)
year = remove_empty_cols(yearRecord)
newyear = levels(unlist(year[1,]))
##########
yearlyData  = read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vinu/YearlyUpdatedData.csv",skip = 1,header = T,na.strings = "")
#yearlyData = yearlyData[1:7433,]
employeeYearData = yearlyData[,1:2]
yearlyData = yearlyData[,c(-1,-2)]
#######
dummyData = yearlyData[,1:19]
actualColumns = 19
totalYearlyColumns = ncol(yearlyData)
yearColumns = 1
for(i in 1:length(newyear)){
  aggYearData1 = yearlyData[,yearColumns:(actualColumns*i)]
  colnames(aggYearData1) = colnames(dummyData)
  aggYearData1$FiscalYear = newyear[i]
  aggYearData1$Unique.Emp.Number = employeeYearData$Unique.Emp.Number
  yearColumns = yearColumns+19
  if(i==1){
    aggregateData = aggYearData1
  }else{
    aggregateData = rbind(aggregateData,aggYearData1)
  }
}
aggregateData = aggregateData[,c(21,20,1:19)]


#############################Preprocessing#########################
#ind <- apply(aggregateData[,3:19], 1, function(x) all(is.na(x)))
ind <- which(is.na(aggregateData$Job.Level...Like.65..64..63.etc.))
agg1 <- aggregateData[-ind, ]
agg1$Unique.Emp.Number = as.character(agg1$Unique.Emp.Number)
#9."Business.Travel"
#[18] "Stock.Option.Level"                                                     
#[19] "Month.in.which.Last.Appraisal.declared"  
agg1 = agg1[,c(-9,-18,-19)]
agg1$FiscalYear = gsub("2012 - 2013", "FY 2012-2013", agg1$FiscalYear)

###########Adding Leave Data###############
leaveData = read.csv("C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Sajjan/df_Cosolidated_Leave.csv")
colnames(leaveData) = c("Unique.Emp.Number","Job.Level...Like.65..64..63.etc.","FiscalYear","Leave")
leaveData$FiscalYear = gsub("2012-2013","FY 2012-2013",leaveData$FiscalYear)
leaveData$FiscalYear = gsub("2013-2014","FY 2013-2014",leaveData$FiscalYear)
leaveData$FiscalYear = gsub("2014-2015","FY 2014-2015",leaveData$FiscalYear)
leaveData$FiscalYear = gsub("2015-2016","FY 2015-2016",leaveData$FiscalYear)
leaveData$FiscalYear = gsub("2016-2017","FY 2016-2017",leaveData$FiscalYear)
leaveData$FiscalYear = gsub("2017-2018","FY 2017-2018",leaveData$FiscalYear)

agg2 = join(agg1,leaveData,type = "left",by=c("Unique.Emp.Number","FiscalYear"))

#######################Merging static and yearly data####################
FullData = join(staticFullData, agg2,
                type = "inner")

################################################################################
#Creating target variable
##########Creating actual employee status
fy = unique(FullData$FiscalYear)
#fy = fy[1:5]
#FullData = filter(FullData,FullData$FiscalYear %in% fy)
#[1] "Unique.Emp.Number",[3] "Hire.Status",[6] "Date.of.Joining",[7] "Last.Date.Worked",[17] "FiscalYear"
test = FullData[,c(1,3,6,7,17)]
FDate = test$FiscalYear
test$FiscalYear = gsub("FY 2012-2013","2013-06-31",test$FiscalYear)
test$FiscalYear = gsub("FY 2013-2014","2014-06-31",test$FiscalYear)
test$FiscalYear = gsub("FY 2014-2015","2015-06-31",test$FiscalYear)
test$FiscalYear = gsub("FY 2015-2016","2016-06-31",test$FiscalYear)
test$FiscalYear = gsub("FY 2016-2017","2017-06-31",test$FiscalYear)
test$FiscalYear = gsub("FY 2017-2018","2018-06-31",test$FiscalYear)
test$Last.Date.Worked = as.character(test$Last.Date.Worked)
#test$FiscalYear = as.Date(test$FiscalYear,format = "%Y-%m-%d")
#FullData$FiscalDate = FDate
#gsub("e", "", group)

ActualEmployeeStatus = c()
for(i in 1:nrow(test)){
  #print(i)
  if(test[i,2] == "A"){   #2"Hire.Status"
    ActualEmployeeStatus = c(ActualEmployeeStatus,"A")
  }else if(test[i,4] > test[i,5]){   #4."Last.Date.Worked"  5."FiscalYear"
    ActualEmployeeStatus = c(ActualEmployeeStatus,"A")
  }else{
    ActualEmployeeStatus = c(ActualEmployeeStatus,"I")
  }
}

FullData$ActualStatus = ActualEmployeeStatus
#x = data.frame(FullData$Unique.Emp.Number,FullData$Hire.Status,FullData$Last.Date.Worked,
#               FullData$FiscalYear,FullData$FiscalDate,FullData$ActualStatus)
#########################################
FullData = subset(FullData,FullData$Date.of.Joining <  "2017-07-01")
sx = which(FullData$Last.Date.Worked < "2013-06-30")
FullData = FullData[-sx,]
empID = unique(FullData$Unique.Emp.Number)
empID = as.character(empID)
count = c()
id = c()
for(j in 1:length(empID)){
  #print(j)
  uniqueRecord = filter(FullData,FullData$Unique.Emp.Number %in% empID[j])
  count = c(count,nrow(uniqueRecord))
  id = c(id,empID[j])
}
tx1  = data.frame(id,count)
tx = subset(tx1,tx1$count == 1)
txemp = tx$id
###
#FullData1 = filter(FullData,(FullData$Unique.Emp.Number %in% txemp))
###
FullData = filter(FullData,!(FullData$Unique.Emp.Number %in% txemp))
empID = unique(FullData$Unique.Emp.Number)
FullData$Environment.Satisfaction = as.numeric(sub("%","",FullData$Environment.Satisfaction))/100
FullData$Job.Satisfaction = as.numeric(sub("%","",FullData$Job.Satisfaction))/100
FullData$Work.Life.Balance = as.numeric(sub("%","",FullData$Work.Life.Balance))/100
FullData$Relationship.Satisfaction..with.team. = as.numeric(sub("%","",FullData$Relationship.Satisfaction..with.team.))/100
FullData$Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining. = as.numeric(sub("%","",FullData$Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining.))/100
FullData$New.Comp.Total.Cost.to.Company. = as.numeric(gsub(",","",FullData$New.Comp.Total.Cost.to.Company.))
FullData$CalcHike = (FullData$Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining.)+1
#target variable creation
for(j in 1:length(empID)){
  #print(j)
  uniqueRecord = filter(FullData,FullData$Unique.Emp.Number %in% empID[j])
  uniqueRecord$FiscalYear = sort(uniqueRecord$FiscalYear)
  #uniqueRecord[1,29] = uniqueRecord[2,29]/uniqueRecord[2,36]
  targetVariable = c()
  promotion = c()
  progression = c()
  promotion = c(promotion,"NA")
  progression = c(progression,"NA")
  for(i in 1:(nrow(uniqueRecord)-1)){
    #print(i)
    if(uniqueRecord[i+1,18] == uniqueRecord[i,18]){ #[18] "Job.Level...Like.65..64..63.etc." 
      promotion = c(promotion,"No")
    }else{
      promotion = c(promotion,"Yes")
    }
    if(uniqueRecord[i+1,19] == uniqueRecord[i,19]){ #[19] "Job.Grade"
      progression = c(progression,"No")
    }else{
      progression = c(progression,"Yes")
    }
    uniqueRecord[i,29] = uniqueRecord[i+1,29] #[29] "New.Comp.Total.Cost.to.Company."
    uniqueRecord[i,30] = uniqueRecord[i+1,30] #[30] "Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining."
    uniqueRecord[i,31] = uniqueRecord[i+1,31] #[31] "Performance.Rating...3.if.new.joining.on.5.point.scale."
    uniqueRecord[i,10] = uniqueRecord[i,10] - (nrow(uniqueRecord)-i) #Actual Age
    uniqueRecord[i,14] = uniqueRecord[i,14] - (nrow(uniqueRecord)-i) #EY Experience
    uniqueRecord[i,15] = uniqueRecord[i,15] - (nrow(uniqueRecord)-i) #Total Experience
    #uniqueRecord[i,18] = uniqueRecord[i,18] - (nrow(uniqueRecord)-i)
    #ut[i,4] =  ut[i,4] - (nrow(ut)-i)
    if(uniqueRecord[i+1,35] == "A"){      #Actual status
      targetVariable = c(targetVariable,0)
    }else{
      targetVariable = c(targetVariable,1)
    }
  }
  targetVariable = c(targetVariable,NA)
  uniqueRecord$TargetVariable = targetVariable
  uniqueRecord$Promotion = promotion
  uniqueRecord$progression = progression
  if(j==1){
    ModelData = uniqueRecord 
  }else{
    ModelData = rbind(ModelData,uniqueRecord)
  }
}

for(i in 1:nrow(ModelData)){
  if(ModelData[i,14] < 0){   #[14] "EyExperience" 
    ModelData[i,14] = 0
  } 
  if(ModelData[i,15] < 0){   #[15] "TotalExperience"
    ModelData[i,15] = 0
  } 
}

ind = which(is.na(ModelData$TargetVariable))
ModelData = ModelData[-ind,]
#exclude = c("FY 2016-2017")
#ModelData = filter(ModelData,!(ModelData$FiscalYear %in% exclude))
ModelData = ModelData[,c(-3,-4,-6,-7,-8,-23,-35,-36)]#[3] "Hire.Status",[4] "Rank.Code.Name",[6] "Date.of.Joining",[7] "Last.Date.Worked", [8] "Bill.Rate" ,[23] "Monthly.Salary",[35] "ActualStatus",[36] "CalcHike" 
ModelData = ModelData[,c(1:11,13:28,30,31,29,12)]
ModelData$Promotion = as.factor(ModelData$Promotion)
ModelData$progression = as.factor(ModelData$progression)
##############################################################################################
#ModelData$FiscalYear = as.factor(ModelData$FiscalYear)
#fyear = as.factor(c("FY 2013-2014","FY 2014-2015","FY 2015-2016","FY 2016-2017"))
#ModelData1 = filter(ModelData,ModelData$FiscalYear %in% fyear)
########################################################################################
#converting factors to numeric 
#ModelData$Education.Field...Highest.education..based.on.degree.issued. = as.numeric(ModelData$Education.Field...Highest.education..based.on.degree.issued.)
ModelData$Performance.Rating...3.if.new.joining.on.5.point.scale. = as.factor(ModelData$Performance.Rating...3.if.new.joining.on.5.point.scale.)
ModelData$Availed.Transport = as.numeric(as.factor(ModelData$Availed.Transport))
#ModelData = ModelData[,-29]
#ModelData1$Work.ex.prior.to.EY = as.numeric(ModelData1$Work.ex.prior.to.EY)
#ModelData1$Years.In.Current.RANK = as.numeric(ModelData1$Years.In.Current.RANK)
#ModelData$Performance.Rating...3.if.new.joining.on.5.point.scale. = as.numeric(ModelData$Performance.Rating...3.if.new.joining.on.5.point.scale.)
#ModelData1$Availed.Transport = as.factor(ModelData1$Availed.Transport)
##############
##############Imputation#############
na_count <-sapply(ModelData, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
Mode <- function(x){
  a = table(x) # x is a vector
  return(a[which.max(a)])
}
Mode(ModelData$Job.Involvement..Complexity.of.job.)
Mode(ModelData$Performance.Rating...3.if.new.joining.on.5.point.scale.)
#year1[is.na(year1[,3]), 3] <- mean(year1[,3],na.rm = T)
#ModelData[is.na(ModelData[,c(18,19,20,21,22,24,25,27)]),c(18,19,20,21,22,24,25,27)] <- mean(ModelData[,c(18,19,20,21,22,24,25,27)],na.rm = T)
ModelData[is.na(ModelData[,17]),17] <- mean(ModelData[,17],na.rm = T)#[17] "Environment.Satisfaction"
ModelData[is.na(ModelData[,18]),18] <- "Medium" #[18] "Job.Involvement..Complexity.of.job."
ModelData[is.na(ModelData[,19]),19] <- mean(ModelData[,19],na.rm = T) #[19] "Job.Satisfaction"
ModelData[is.na(ModelData[,20]),20] <- mean(ModelData[,20],na.rm = T) #[20] "Work.Life.Balance" 
ModelData[is.na(ModelData[,21]),21] <- mean(ModelData[,21],na.rm = T) #[21] "Relationship.Satisfaction..with.team."
ModelData[is.na(ModelData[,23]),23] <- mean(ModelData[,23],na.rm = T) #23 hike
ModelData[is.na(ModelData[,24]),24] <- 3 #Performance.Rating.
ModelData[is.na(ModelData[,25]),25] <- 0 #[25] "Availed.Transport" 
ModelData[is.na(ModelData[,26]),26] <- mean(ModelData[,26],na.rm = T) #[26] "Average.Learning.Hours"
ModelData[is.na(ModelData[,27]),27] <- median(ModelData[,27],na.rm = T) #[27] "Leave" 
#Salary imputation
imputeData = ModelData
imputeData = na.omit(imputeData)
imputeGroup = summaryBy(imputeData$New.Comp.Total.Cost.to.Company.+
                          imputeData$Charged.Hrs
                        ~imputeData$Job.Level...Like.65..64..63.etc.+imputeData$Job.Grade,
                        data=imputeData, FUN=c(mean))
colnames(imputeGroup) = c("Job.Level...Like.65..64..63.etc.","Job.Grade","CTCMean","ChargedHoursMean")
ModelData1 = join(ModelData, imputeGroup,
                  type = "inner",by = c("Job.Level...Like.65..64..63.etc.","Job.Grade"))
#Salary Imputation
for(i in 1:nrow(ModelData1)){
  #print(i)
  if(is.na(ModelData1[i,22]) == T){ #[22] "New.Comp.Total.Cost.to.Company."
    ModelData1[i,22] = ModelData1[i,32] #[32] "CTCMean"
  }
  if(is.na(ModelData1[i,15]) == T){ #[15] "Charged.Hrs" 
    ModelData1[i,15] = ModelData1[i,33]  #[33] "ChargedHoursMean"
  }
}

###################################Difference variables#############################
#Salary difference with grade
GroupByData = summaryBy(ModelData1$New.Comp.Total.Cost.to.Company.+
                          ModelData1$Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining.+
                          ModelData1$Job.Satisfaction+ModelData1$Work.Life.Balance+
                          ModelData1$Environment.Satisfaction
                        ~ModelData1$Job.Level...Like.65..64..63.etc.,
                        data=ModelData1, FUN=c(mean,max))
colnames(GroupByData) = c("Job.Level...Like.65..64..63.etc.","SalaryMean","SalaryHikeMean",
                          "JSatifactionMean","WLBalanceMean","ESatisfactionMean","SalaryMax",
                          "SalaryHikeMax","JSatifactionMax","WLBalanceMax","ESatisfactionMax")
#######################Merging static and yearly data####################
ModelData1 = join(ModelData1, GroupByData,
                  type = "inner")
ModelData1$AvgSalaryDiff = ((ModelData1$New.Comp.Total.Cost.to.Company.)-(ModelData1$SalaryMean))
ModelData1$MaxSalaryDiff = ((ModelData1$New.Comp.Total.Cost.to.Company.)-(ModelData1$SalaryMax))
ModelData1$AvgSalaryHikeDiff = ((ModelData1$Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining.)-(ModelData1$SalaryHikeMean))
ModelData1$MaxSalaryHikeDiff = ((ModelData1$Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining.)-(ModelData1$SalaryHikeMax))
ModelData1$AvgJSatisDiff = ((ModelData1$Job.Satisfaction)-(ModelData1$JSatifactionMean))
ModelData1$MaxJSatisDiff = ((ModelData1$Job.Satisfaction)-(ModelData1$JSatifactionMax))
ModelData1$AvgWLBalanceDiff = ((ModelData1$Work.Life.Balance)-(ModelData1$WLBalanceMean))
ModelData1$MaxWLBalanceDiff = ((ModelData1$Work.Life.Balance)-(ModelData1$WLBalanceMax))
ModelData1$AvgESatisfactionDiff = ((ModelData1$Environment.Satisfaction)-(ModelData1$ESatisfactionMean))
ModelData1$MaxESatisfactionDiff = ((ModelData1$Environment.Satisfaction)-(ModelData1$ESatisfactionMax))
ModelData1 = ModelData1[,c(-32:-43)]
#ModelData1$Number.of..Companies.Worked = as.numeric(ModelData1$Number.of..Companies.Worked)
#ModelData1$AvgDurCompany = (ModelData1$Total.Working.Years...total.experience.)/(ModelData1$Number.of..Companies.Worked)

#Salary difference with grade
GroupByDataGrade = summaryBy(ModelData1$New.Comp.Total.Cost.to.Company.+
                               ModelData1$Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining.+
                               ModelData1$Job.Satisfaction+ModelData1$Work.Life.Balance+
                               ModelData1$Environment.Satisfaction
                             ~ModelData1$Job.Level...Like.65..64..63.etc.+ModelData1$Job.Grade,
                             data=ModelData1, FUN=c(mean,max))
colnames(GroupByDataGrade) = c("Job.Level...Like.65..64..63.etc.","Job.Grade","SalaryMeanGrade","SalaryHikeMeanGrade",
                               "JSatifactionMeanGrade","WLBalanceMeanGrade","ESatisfactionMeanGrade","SalaryMaxGrade",
                               "SalaryHikeMaxGrade","JSatifactionMaxGrade","WLBalanceMaxGrade","ESatisfactionMaxGrade")
#######################Merging static and yearly data####################
ModelData1 = join(ModelData1, GroupByDataGrade,
                  type = "inner",by = c("Job.Level...Like.65..64..63.etc.","Job.Grade"))
ModelData1$AvgSalaryDiffGrade = ((ModelData1$New.Comp.Total.Cost.to.Company.)-(ModelData1$SalaryMeanGrade))
ModelData1$MaxSalaryDiffGrade = ((ModelData1$New.Comp.Total.Cost.to.Company.)-(ModelData1$SalaryMaxGrade))
ModelData1$AvgSalaryHikeDiffGrade = ((ModelData1$Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining.)-(ModelData1$SalaryHikeMeanGrade))
ModelData1$MaxSalaryHikeDiffGrade = ((ModelData1$Percent.Salary.Hike...in.last.appraisal.or...hike.given.if.new.joining.)-(ModelData1$SalaryHikeMaxGrade))
ModelData1$AvgJSatisDiffGrade = ((ModelData1$Job.Satisfaction)-(ModelData1$JSatifactionMeanGrade))
ModelData1$MaxJSatisDiffGrade = ((ModelData1$Job.Satisfaction)-(ModelData1$JSatifactionMaxGrade))
ModelData1$AvgWLBalanceDiffGrade = ((ModelData1$Work.Life.Balance)-(ModelData1$WLBalanceMeanGrade))
ModelData1$MaxWLBalanceDiffGrade = ((ModelData1$Work.Life.Balance)-(ModelData1$WLBalanceMaxGrade))
ModelData1$AvgESatisfactionDiffGrade = ((ModelData1$Environment.Satisfaction)-(ModelData1$ESatisfactionMeanGrade))
ModelData1$MaxESatisfactionDiffGrade = ((ModelData1$Environment.Satisfaction)-(ModelData1$ESatisfactionMaxGrade))
ModelData1 = ModelData1[,c(-42:-51)]
ModelData1 = ModelData1[,c(1:29,32:51,30,31)]
#####
#ModelData1$Number.of..Companies.Worked = as.numeric(ModelData1$Number.of..Companies.Worked)
ModelData1$AvgDurCompanyYearly = ((ModelData1$TotalExperience)/((ModelData1$Number.of..Companies.Worked)+1))
ModelData1$Performance.Rating...3.if.new.joining.on.5.point.scale. = as.numeric(ModelData1$Performance.Rating...3.if.new.joining.on.5.point.scale.)
ModelData1$Job.Involvement..Complexity.of.job. = as.numeric(ModelData1$Job.Involvement..Complexity.of.job.)
ModelData1 = ModelData1[,c(1:10,12:49,11,52,50,51)]
ModelData1 <- dummy.data.frame(ModelData1, names=c('City','Gender.Code',
                                                   'Job.Level...Like.65..64..63.etc.',
                                                   'Sub.Service.line','Promotion',
                                                   'progression',
                                                   'Education.Field...Highest.education..based.on.degree.issued.'
), sep="_")
ModelData1 = ModelData1[,c(-8,-9,-12,-99,-105,-121,-124)]
exclude = c("FY 2016-2017")
ScoreData = filter(ModelData1,(ModelData1$FiscalYear %in% exclude))
ModelData1 = filter(ModelData1,!(ModelData1$FiscalYear %in% exclude))
exclude1 = c("FY 2015-2016")
ModelData1 = filter(ModelData1,(ModelData1$FiscalYear %in% exclude1))
ScoreData = ScoreData[,-143]
ModelData1 = ModelData1[,-143]
#####
################Apply scaling##########
##Scaling code to normalize entire data
#scaling = function(x)
#{
#  x = (x-min(x))/(max(x)-min(x))
#  return(x)
#}

##Apply scaling for all data 
#ModelData1[,3:132] = apply(ModelData1[,3:132],2,scaling)


#######################Model Building########################
ModelDataId = unique(ModelData1$Unique.Emp.Number)
set.seed(100)
ind1 = sample(1:length(ModelDataId),round(0.68*length(ModelDataId)))
trainID1=ModelDataId[ind1]
testID1=ModelDataId[-ind1]
trainData = (filter(ModelData1, ModelData1$Unique.Emp.Number %in% trainID1))
testData = (filter(ModelData1, ModelData1$Unique.Emp.Number %in% testID1))
########################
##########Creating validation set
TestDataId = unique(testData$Unique.Emp.Number)
set.seed(100)
ind1 = sample(1:length(TestDataId),round(0.75*length(TestDataId)))
totalTestID1=TestDataId[ind1]
validationID1=TestDataId[-ind1]
totalTestData = (filter(testData, testData$Unique.Emp.Number %in% totalTestID1))
validationData = (filter(testData, testData$Unique.Emp.Number %in% validationID1))
########Creating 3 test set
TotalTestDataId = unique(totalTestData$Unique.Emp.Number)
ind1 = sample(1:length(TotalTestDataId),round(0.66*length(TotalTestDataId)))
TestSetID=TotalTestDataId[ind1]
TestSet2ID=TotalTestDataId[-ind1]
Test1Data = (filter(totalTestData, totalTestData$Unique.Emp.Number %in% TestSetID))
Test3Data = (filter(totalTestData, totalTestData$Unique.Emp.Number %in% TestSet2ID))
#test set 1& 2
TestsetDataId = unique(Test1Data$Unique.Emp.Number)
set.seed(100)
ind1 = sample(1:length(TestsetDataId),round(0.50*length(TestsetDataId)))
TestSetID=TestsetDataId[ind1]
TestSet2ID=TestsetDataId[-ind1]
Test2Data = (filter(Test1Data, Test1Data$Unique.Emp.Number %in% TestSetID))
Test4Data = (filter(Test1Data, Test1Data$Unique.Emp.Number %in% TestSet2ID))

##################Name test Data##############
TestSet1 = Test2Data
TestSet2 = Test3Data
TestSet3 = Test4Data
rm(Test2Data,Test3Data,Test4Data)
########################
t1 = trainData$Unique.Emp.Number %in% testData$Unique.Emp.Number == T
length(which(t1 == T))
t1 = TestSet1$Unique.Emp.Number %in% TestSet2$Unique.Emp.Number == T
length(which(t1 == T))
t1 = TestSet2$Unique.Emp.Number %in% TestSet3$Unique.Emp.Number == T
length(which(t1 == T))
t1 = TestSet1$Unique.Emp.Number %in% TestSet3$Unique.Emp.Number == T
length(which(t1 == T))
########Model building#########
#XGBOOST#
#Model Implementation
#set.seed(100)
set.seed(49)
#set.seed(0.01)
xgb <- xgboost(data = data.matrix(trainData[,c(-1,-2,-140,-142)]), 
               label = trainData[,142], 
               eta = 0.52,
               max_depth =  4, 
               nround=80, 
               subsample = 0.8,
               colsample_bytree = 0.7,
               seed = 1,
               eval_metric = "error",
               objective = "binary:logistic",
               min_child_weight = 0.475,
               lambda = 0.2,
               alpha = 0.4,
               gamma = 4
               #num_class = 12,
               #nthread = 3
)

#Predict the test data
y_pred1 <- predict(xgb, data.matrix(TestSet1[,c(-1,-2,-140,-142)]))
y_pred2 <- predict(xgb, data.matrix(TestSet2[,c(-1,-2,-140,-142)]))
y_pred3 <- predict(xgb, data.matrix(TestSet3[,c(-1,-2,-140,-142)]))
val_pred <- predict(xgb, data.matrix(validationData[,c(-1,-2,-140,-142)]))
#########
Prediction = function(x,y){
  testoutput1 = c()
  for(i in 1:length(x)){
    if(x[i] > 0.5){
      testoutput1 = c(testoutput1,1)
    }else{
      testoutput1 = c(testoutput1,0)
    }
  }
  tab = table(testoutput1,y$TargetVariable)
  #tab1 = table(testData$TargetVariable,testoutput1)
  cm = confusionMatrix(tab,positive = "1")
  return(cm)
  fs = (2*cm$byClass[1]*cm$byClass[3])/(cm$byClass[1]+cm$byClass[3])
  #return(fs)
}
Prediction(y_pred1,TestSet1)
Prediction(y_pred2,TestSet2)
Prediction(y_pred3,TestSet3)
Prediction(val_pred,validationData)

##########Scoring Prediction#########
score_Pred <- predict(xgb, data.matrix(ScoreData[,c(-1,-2,-140,-142)]))
Prediction(score_Pred,ScoreData)
#View(ScoreData)
#ScoreData = cbind(ScoreData,score_Pred)
#write.csv(ScoreData,"C:/Users/vigneshwar.thiyagara/Documents/DataScience/AttrtitionHrPrediction/Vigneshwar/Results/ScoreResults0204.csv",row.names = F)



