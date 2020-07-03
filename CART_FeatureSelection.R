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

#mushroom dataset
set.seed(1)
mushroom <- read.table("agaricus-lepiota.data", sep=",", header=TRUE)
sampleMushroom = sample_n(mushroom, round(0.9 * nrow(mushroom)) )
tree <- rpart(edible ~ . , data=sampleMushroom, method='class') 
rpart.plot(tree)
tree
#variabelen used by tree are the following['odor', 'spore.print.color']

#breast dataset
set.seed(1)
breast <- read.table("breast.txt", sep=" ")
breast[,2] <- NULL
colnames(breast) <- c('Class','Sample code number','Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion','Single Epithelial Cell Size','Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses')
sampleBreast = sample_n(breast, round(0.9 * nrow(breast)) )
sampleBreast$'Sample code number' <- NULL
tree <- rpart(Class ~ . , data=sampleBreast, method='class') 
rpart.plot(tree)
tree
#variabelen used by tree are the following['Uniformity of Cell Size', 'Bare Nuclei', 'Uniformity of Cell Shape']

#chess dataset
chess <- read.table("kr-vs-kp.data", sep=",")
colnames(chess) <- c('bkblk','bknwy','bkon8','bkona','bkspr','bkxbq','bkxcr','bkxwp','blxwp','bxqsq','cntxt','dsopp','dwipd','hdchk','katri','mulch','qxmsq','r2ar8','reskd','reskr','rimmx','rkxwp','rxmsq','simpl','skach','skewr','skrxp','spcop','stlmt','thrsk','wkcti','wkna8','wknck','wkovl','wkpos','wtoeg','white_win')
sampleChess = sample_n(chess, round(0.9 * nrow(chess)) )
tree <- rpart(white_win ~ . , data=sampleChess, method='class') 
rpart.plot(tree)
tree
#variabelen used by tree are the following['rimmx','wknck','bxqsq','wkna8', 'wkpos','bkxbq','katri', 'bkblk']

#adult dataset
adult <- read.table("a1a.txt", sep=" ", fill=TRUE)
adult[,16] <- NULL
colnames(adult) <- c('salery50k','age','workclass','fnlwgt','education','education-num','marital-status','occupation','relationship','race','sex','capital-gain','capital-loss','hours-per-week','native-country')
adultDf <- adult[!(is.na(adult$`native-country`) | adult$`native-country`==""), ]
sampleAdult = sample_n(adultDf, round(0.9 * nrow(adultDf)) )
tree <- rpart(salery50k ~ . , data=sampleAdult, method='class') 
rpart.plot(tree)
tree
#variabelen used by tree are the following['relationship', 'occupation', 'education', 'captial-gain','workclass']

#votes dataset
votes <- read.table("house-votes-84.data", sep=",", fill=TRUE)
colnames(votes) <- c('Class','handicapped-infants','water-project-cost-sharing','adoption-of-the-budget-resolution','physician-fee-freeze','el-salvador-aid','religious-groups-in-schools','anti-satellite-test-ban','aid-to-nicaraguan-contras','mx-missile','immigration','synfuels-corporation-cutback','education-spending','superfund-right-to-sue','crime','duty-free-exports','export-administration-act-south-africa')
votes[votes == '?'] <- NA
votes <-na.omit(votes)
sampleVotes <- sample_n(votes, round(0.9 * nrow(votes)) )
tree <- rpart(Class ~ . , data=sampleVotes, method='class') 
rpart.plot(tree)
tree
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
sampleHeloc <- sample_n(heloc, round(0.9 * nrow(heloc)) )
tree <- rpart(RiskPerformance ~ . , data=sampleHeloc, method='class', control = rpart.control(cp = 0.003)) 
rpart.plot(tree)
tree
#variabelen used by tree are the following ['ExternalRiskEstimate', 'MSinceMostRecentInqexcl7days']
#variabelen used by tree are the following ['ExternalRiskEstimate', 'MSinceMostRecentInqexcl7days', 'NumSatisfactoryTrades', 'NumTotalTrades']

#tictactoe dataset
tictactoe <- read.table("tic-tac-toe.data", sep=",", fill=TRUE)
colnames(tictactoe) <- cbind('top-left-square','top-middle-square','top-right-square','middle-left-square','middle-middle-square','middle-right-square','bottom-left-square','bottom-middle-square','bottom-right-square','Class')
sampleTictactoe <- sample_n(tictactoe, round(0.9 * nrow(tictactoe)) )
tree <- rpart(Class ~ . , data=sampleTictactoe, method='class') 
rpart.plot(tree)
tree
#variabelen used by tree are the following ['middle-middle-square','bottom-right-square','top-left-square','middle-right-square','middle-left-square','top-right-square','bottom-left-square','bottom-middle-square']

#monks dataset
monks <- read.table("monks-1.test", sep=" ", fill=TRUE)
colnames(monks) <- cbind('empty','class','a1','a2','a3','a4','a5','a6','Id')
monks$empty <- NULL
monks$Id <- NULL
monksSample <- sample_n(monks, round(0.9 * nrow(monks)) )
tree <- rpart(class ~ . , data=monksSample, method='class') 
rpart.plot(tree)
tree
#variabelen used by tree are the following ['a1', 'a2', 'a4', 'a5']

#heart dataset
set.seed(1)
heart1 <- read.table("SPECT.test", sep=',', fill=TRUE)
heart2 <- read.table("SPECT.train", sep=',', fill=TRUE)
heart <- rbind(heart1, heart2)
colnames(heart) <- cbind('OVERALL_DIAGNOSIS','F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12','F13','F14','F15','F16','F17','F18','F19','F20','F21','F22')
heartSample <- sample_n(heart, round(0.9 * nrow(heart)) )
tree <- rpart(OVERALL_DIAGNOSIS ~ . , data=heartSample, method='class') 
rpart.plot(tree)
tree
#variabelen used by tree are the following ['F13','F1','F7','F20','F22']

#student dataset
set.seed(1)
student <- read.table("student-mat.csv", sep=';', row.names=NULL, header=TRUE)
student$Alcohol <- (student$Walc > 1) & (student$Dalc > 1)
student$Walc <- NULL
student$Dalc <- NULL
studentSample <- sample_n(student, round(0.9 * nrow(student)) )
tree <- rpart(Alcohol ~ . , data=studentSample, method='class') 
rpart.plot(tree)
tree
#variabelen used by tree are the following ['goout','famsize','reason','famsup','studytime','famsize','Mjob','activities','sex','famrel','G1']

#control = list(maxdepth = 1)






