install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("maptree")
install.packages("gtools")
library(rpart)
library(rpart.plot)
library(dplyr)
library(maptree)
library(gtools)

set.seed(1)

maxError = 0.1^20

#mushroom dataset
mushroom <- read.table("agaricus-lepiota.data", sep=",", header=TRUE)
statArray <- matrix(0, length(names(mushroom)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(mushroom)
counter = 0
for(i in names(mushroom)){
  freq_data<-table(mushroom[[i]], mushroom$edible)
  freq_data
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 9)

#variabelen used by tree are the following['odor', 'spore.print.color']

#breast dataset
breast <- read.table("breast.txt", sep=" ")
breast[,2] <- NULL
colnames(breast) <- c('Class','Sample code number','Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion','Single Epithelial Cell Size','Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses')
breast$'Sample code number' <- NULL
statArray <- matrix(0, length(names(breast)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(breast)
counter = 0
for(i in names(breast)){
  freq_data<-table(breast[[i]], breast$Class)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 8)
#variabelen used by tree are the following['Uniformity of Cell Size', 'Bare Nuclei', 'Uniformity of Cell Shape']

#chess dataset
chess <- read.table("kr-vs-kp.data", sep=",")
colnames(chess) <- c('bkblk','bknwy','bkon8','bkona','bkspr','bkxbq','bkxcr','bkxwp','blxwp','bxqsq','cntxt','dsopp','dwipd','hdchk','katri','mulch','qxmsq','r2ar8','reskd','reskr','rimmx','rkxwp','rxmsq','simpl','skach','skewr','skrxp','spcop','stlmt','thrsk','wkcti','wkna8','wknck','wkovl','wkpos','wtoeg','white_win')
statArray <- matrix(0, length(names(chess)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(chess)
counter = 0
for(i in names(chess)){
  freq_data<-table(chess[[i]], chess$white_win)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 9)

#variabelen used by tree are the following['rimmx','wknck','bxqsq','wkna8', 'wkpos','bkxbq','katri', 'bkblk']

#adult dataset
adult <- read.table("a1a.txt", sep=" ", fill=TRUE)
adult[,16] <- NULL
colnames(adult) <- c('salery50k','age','workclass','fnlwgt','education','education-num','marital-status','occupation','relationship','race','sex','capital-gain','capital-loss','hours-per-week','native-country')
adultDf <- adult[!(is.na(adult$`native-country`) | adult$`native-country`==""), ]
statArray <- matrix(0, length(names(adult)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(adult)
counter = 0
for(i in names(adult)){
  freq_data<-table(adult[[i]], adult$salery50k)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 8)
#variabelen used by tree are the following['relationship', 'occupation', 'education', 'captial-gain','workclass']

#votes dataset
votes <- read.table("house-votes-84.data", sep=",", fill=TRUE)
colnames(votes) <- c('Class','handicapped-infants','water-project-cost-sharing','adoption-of-the-budget-resolution','physician-fee-freeze','el-salvador-aid','religious-groups-in-schools','anti-satellite-test-ban','aid-to-nicaraguan-contras','mx-missile','immigration','synfuels-corporation-cutback','education-spending','superfund-right-to-sue','crime','duty-free-exports','export-administration-act-south-africa')
statArray <- matrix(0, length(names(votes)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(votes)
counter = 0
for(i in names(votes)){
  freq_data<-table(votes[[i]], votes$Class)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 9)
#variabelen used by tree are the following['physician-fee-freeze']

#heloc dataset
heloc <- read.table("heloc_dataset_v1.csv", sep=",", fill=TRUE, header=TRUE)
start =0 
for (i in colnames(heloc)){
  if (start == 0) {
    start = 1
  }
  else {
    heloc[[i]] <- quantcut(heloc[[i]], q=10, na.rm=TRUE)
  }
}
set.seed(1)
statArray <- matrix(0, length(names(heloc)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(heloc)
counter = 0
for(i in names(heloc)){
  freq_data<-table(heloc[[i]], heloc$RiskPerformance)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 8)
#variabelen used by tree are the following ['ExternalRiskEstimate', 'MSinceMostRecentInqexcl7days']
#variabelen used by tree are the following ['ExternalRiskEstimate', 'MSinceMostRecentInqexcl7days', 'NumSatisfactoryTrades', 'NumTotalTrades']

#tictactoe dataset
tictactoe <- read.table("tic-tac-toe.data", sep=",", fill=TRUE)
colnames(tictactoe) <- cbind('top-left-square','top-middle-square','top-right-square','middle-left-square','middle-middle-square','middle-right-square','bottom-left-square','bottom-middle-square','bottom-right-square','Class')

statArray <- matrix(0, length(names(tictactoe)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(tictactoe)
counter = 0
for(i in names(tictactoe)){
  freq_data<-table(tictactoe[[i]], tictactoe$Class)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 8)

#variabelen used by tree are the following ['middle-middle-square','bottom-right-square','top-left-square','middle-right-square','middle-left-square','top-right-square','bottom-left-square','bottom-middle-square']

#monks dataset
monks <- read.table("monks-1.test", sep=" ", fill=TRUE)
colnames(monks) <- cbind('empty','class','a1','a2','a3','a4','a5','a6','Id')
monks$empty <- NULL
monks$Id <- NULL

statArray <- matrix(0, length(names(monks)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(monks)
counter = 0
for(i in names(monks)){
  freq_data<-table(monks[[i]], monks$class)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 9)

#variabelen used by tree are the following ['a1', 'a2', 'a4', 'a5']

#heart dataset
heart1 <- read.table("SPECT.test", sep=',', fill=TRUE)
heart2 <- read.table("SPECT.train", sep=',', fill=TRUE)
heart <- rbind(heart1, heart2)
colnames(heart) <- cbind('OVERALL_DIAGNOSIS','F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12','F13','F14','F15','F16','F17','F18','F19','F20','F21','F22')
statArray <- matrix(0, length(names(heart)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(heart)
counter = 0
for(i in names(heart)){
  freq_data<-table(heart[[i]], heart$OVERALL_DIAGNOSIS)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 8)
#variabelen used by tree are the following ['F13','F11','F16','F10','F22','F20']

#student dataset
student <- read.table("student-mat.csv", sep=';', row.names=NULL, header=TRUE)
student$Alcohol <- (student$Walc > 1) & (student$Dalc > 1)
student$Walc <- NULL
student$Dalc <- NULL
statArray <- matrix(0, length(names(student)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(student)
counter = 0
for(i in names(student)){
  freq_data<-table(student[[i]], student$Alcohol)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 8)
#variabelen used by tree are the following ['goout','famsize','reason','famsup','studytime','famsize','Mjob','activities','sex','famrel','G1']

#German dataset
german <- read.table("german.data", sep=' ', row.names=NULL, header=FALSE)
colnames(german) <- cbind('statusCheckAccount', 'duration', 'credHist', 'purpose', 'creditAmount', 'savings','employment', 'installrate', 'personalStatus', 'otherDebtors', 'presentResidence', 'property', 'age', 'otherInstallment', 'housing', 'existingCredit', 'job', 'peopleLiable', 'telephone', 'foreign', 'Class')
numericalGerman <- cbind('duration', 'creditAmount', 'installrate', 'presentResidence', 'age', 'existingCredit', 'peopleLiable')
for (i in numericalGerman){
  if (start == 0) {
    start = 1
  }
  else {
    german[[i]] <- quantcut(german[[i]], q=10, na.rm=TRUE)
  }
}

statArray <- matrix(0, length(names(german)), 2)
statFrame <- as.data.frame(statArray)
statFrame[,1] <- names(german)
counter = 0

counter = 0
for(i in names(german)){
  freq_data<-table(german[[i]], german$Class)
  chistat <- chisq.test(freq_data,)
  counter = counter + 1
  statFrame[counter, 2] <- chistat$statistic
}
statFrame <- statFrame[order(statFrame$V2),]
plot(statFrame$V2)
tail(statFrame, 8)

#control = list(maxdepth = 1)






