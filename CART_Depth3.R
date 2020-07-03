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
mushroom <- read.table("agaricus-lepiota.data", sep=",", header=TRUE)
smp_size <- floor(0.9 * nrow(mushroom))
set.seed(1)
train_ind <- sample(seq_len(nrow(mushroom)), size = smp_size)
trainMush <- mushroom[train_ind, ]
testMush <- mushroom[-train_ind, ]
tree <- rpart(edible ~ . , data=trainMush, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)
testPredMush <- predict(tree, testMush, type = "class")
trainPredMush <- predict(tree, trainMush, type = "class")
predAccMush <- sum(testPredMush == testMush$edible) / length(testPredMush)
trainAccMush <- sum(trainPredMush == trainMush$edible) / length(trainPredMush)
print(trainAccMush)
print(predAccMush)
#variabelen used by tree are the following['odor', 'spore.print.color']

#breast dataset
breast <- read.table("breast.txt", sep=" ")
breast[,2] <- NULL
colnames(breast) <- c('Class','Sample code number','Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion','Single Epithelial Cell Size','Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses')
breast$'Sample code number' <- NULL
smp_size <- floor(0.9 * nrow(breast))
set.seed(1)
train_ind <- sample(seq_len(nrow(breast)), size = smp_size)
trainBreast <- breast[train_ind, ]
testBreast <- breast[-train_ind, ]

tree <- rpart(Class ~ . , data=trainBreast, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)
testPredBreast <- predict(tree, testBreast, type = "class")
trainPredBreast <- predict(tree, trainBreast, type = "class")
predAccBreast <- sum(testPredBreast == testBreast$Class) / length(testPredBreast)
trainAccBreast <- sum(trainPredBreast == trainBreast$Class) / length(trainPredBreast)
print(predAccBreast)
print(trainAccBreast)
#variabelen used by tree are the following['Uniformity of Cell Size', 'Bare Nuclei', 'Uniformity of Cell Shape']

#chess dataset
chess <- read.table("kr-vs-kp.data", sep=",")
colnames(chess) <- c('bkblk','bknwy','bkon8','bkona','bkspr','bkxbq','bkxcr','bkxwp','blxwp','bxqsq','cntxt','dsopp','dwipd','hdchk','katri','mulch','qxmsq','r2ar8','reskd','reskr','rimmx','rkxwp','rxmsq','simpl','skach','skewr','skrxp','spcop','stlmt','thrsk','wkcti','wkna8','wknck','wkovl','wkpos','wtoeg','white_win')
smp_size <- floor(0.9 * nrow(chess))
set.seed(1)
train_ind <- sample(seq_len(nrow(chess)), size = smp_size)
trainChess <- chess[train_ind, ]
testChess <- chess[-train_ind, ]


tree <- rpart(white_win ~ . , data=trainChess, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)

testPredChess <- predict(tree, testChess, type = "class")
trainPredChess <- predict(tree, trainChess, type = "class")
predAccChess <- sum(testPredChess == testChess$white_win) / length(testPredChess)
trainAccChess <- sum(trainPredChess == trainChess$white_win) / length(trainPredChess)
print(predAccChess)
print(trainAccChess)

#variabelen used by tree are the following['rimmx','wknck','bxqsq','wkna8', 'wkpos','bkxbq','katri', 'bkblk']

#adult dataset
adult <- read.table("a1a.txt", sep=" ", fill=TRUE)
adult[,16] <- NULL
colnames(adult) <- c('salery50k','age','workclass','fnlwgt','education','education-num','marital-status','occupation','relationship','race','sex','capital-gain','capital-loss','hours-per-week','native-country')
adultDf <- adult[!(is.na(adult$`native-country`) | adult$`native-country`==""), ]

smp_size <- floor(0.9 * nrow(adultDf))
set.seed(1)
train_ind <- sample(seq_len(nrow(adultDf)), size = smp_size)
trainAdult <- adultDf[train_ind, ]
testAdult <- adultDf[-train_ind, ]
tree <- rpart(salery50k ~ . , data=trainAdult, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)

testPredAdult <- predict(tree, testAdult, type = "class")
trainPredAdult <- predict(tree, trainAdult, type = "class")
predAccAdult <- sum(testPredAdult == testAdult$salery50k) / length(testPredAdult)
trainAccAdult <- sum(trainPredAdult == trainAdult$salery50k) / length(trainPredAdult)
print(predAccAdult)
print(trainAccAdult)
#variabelen used by tree are the following['relationship', 'occupation', 'education', 'captial-gain','workclass']

#votes dataset
votes <- read.table("house-votes-84.data", sep=",", fill=TRUE)
colnames(votes) <- c('Class','handicapped-infants','water-project-cost-sharing','adoption-of-the-budget-resolution','physician-fee-freeze','el-salvador-aid','religious-groups-in-schools','anti-satellite-test-ban','aid-to-nicaraguan-contras','mx-missile','immigration','synfuels-corporation-cutback','education-spending','superfund-right-to-sue','crime','duty-free-exports','export-administration-act-south-africa')
votes[votes == '?'] <- NA
votes <-na.omit(votes)

smp_size <- floor(0.9 * nrow(votes))
set.seed(1)
train_ind <- sample(seq_len(nrow(votes)), size = smp_size)
trainVotes <- votes[train_ind, ]
testVotes <- votes[-train_ind, ]


tree <- rpart(Class ~ . , data=trainVotes, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)

testPredVotes <- predict(tree, testVotes, type = "class")
trainPredVotes <- predict(tree, trainVotes, type = "class")
predAccVotes <- sum(testPredVotes == testVotes$Class) / length(testPredVotes)
trainAccVotes <- sum(trainPredVotes == trainVotes$Class) / length(trainPredVotes)
print(predAccVotes)
print(trainAccVotes)
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

smp_size <- floor(0.9 * nrow(heloc))
set.seed(1)
train_ind <- sample(seq_len(nrow(heloc)), size = smp_size)
trainHeloc <- heloc[train_ind, ]
testHeloc <- heloc[-train_ind, ]

tree <- rpart(RiskPerformance ~ . , data=trainHeloc, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)
testPredHeloc <- predict(tree, testHeloc, type = "class")
trainPredHeloc <- predict(tree, trainHeloc, type = "class")
predAccHeloc <- sum(testPredHeloc == testHeloc$RiskPerformance) / length(testPredHeloc)
trainAccHeloc <- sum(trainPredHeloc == trainHeloc$RiskPerformance) / length(trainPredHeloc)
print(predAccHeloc)
print(trainAccHeloc)
#variabelen used by tree are the following ['ExternalRiskEstimate', 'MSinceMostRecentInqexcl7days']

#tictactoe dataset
tictactoe <- read.table("tic-tac-toe.data", sep=",", fill=TRUE)
colnames(tictactoe) <- cbind('top-left-square','top-middle-square','top-right-square','middle-left-square','middle-middle-square','middle-right-square','bottom-left-square','bottom-middle-square','bottom-right-square','Class')

smp_size <- floor(0.9 * nrow(tictactoe))
set.seed(1)
train_ind <- sample(seq_len(nrow(tictactoe)), size = smp_size)
trainTictactoe <- tictactoe[train_ind, ]
testTictactoe <- tictactoe[-train_ind, ]

tree <- rpart(Class ~ . , data=trainTictactoe, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)
testPredTictactoe <- predict(tree, testTictactoe, type = "class")
trainPredTictactoe <- predict(tree, trainTictactoe, type = "class")
predAccTictactoe <- sum(testPredTictactoe == testTictactoe$Class) / length(testPredTictactoe)
trainAccTictactoe <- sum(trainPredTictactoe == trainTictactoe$Class) / length(trainPredTictactoe)
print(predAccTictactoe)
print(trainAccTictactoe)
#variabelen used by tree are the following ['middle-middle-square','bottom-right-square','top-left-square','middle-right-square','middle-left-square','top-right-square','bottom-left-square','bottom-middle-square']

#monks dataset
monks <- read.table("monks-1.test", sep=" ", fill=TRUE)
colnames(monks) <- cbind('empty','class','a1','a2','a3','a4','a5','a6','Id')
monks$empty <- NULL
monks$Id <- NULL

smp_size <- floor(0.9 * nrow(monks))
set.seed(1)
train_ind <- sample(seq_len(nrow(monks)), size = smp_size)
trainMonks <- monks[train_ind, ]
testMonks <- monks[-train_ind, ]

tree <- rpart(class ~ . , data=trainMonks, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)
testPredMonks <- predict(tree, testMonks, type = "class")
trainPredMonks <- predict(tree, trainMonks, type = "class")
predAccMonks <- sum(testPredMonks == testMonks$class) / length(testPredMonks)
trainAccMonks <- sum(trainPredMonks == trainMonks$class) / length(trainPredMonks)
print(predAccMonks)
print(trainAccMonks)
#variabelen used by tree are the following ['a1', 'a2', 'a4', 'a5']

#heart dataset
heart1 <- read.table("SPECT.test", sep=',', fill=TRUE)
heart2 <- read.table("SPECT.train", sep=',', fill=TRUE)
heart <- rbind(heart1, heart2)
colnames(heart) <- cbind('OVERALL_DIAGNOSIS','F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12','F13','F14','F15','F16','F17','F18','F19','F20','F21','F22')

smp_size <- floor(0.9 * nrow(heart))
set.seed(1)
train_ind <- sample(seq_len(nrow(heart)), size = smp_size)
trainHeart <- heart[train_ind, ]
testHeart <- heart[-train_ind, ]

tree <- rpart(OVERALL_DIAGNOSIS ~ . , data=trainHeart, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)
testPredHeart <- predict(tree, testHeart, type = "class")
trainPredHeart <- predict(tree, trainHeart, type = "class")
predAccHeart <- sum(testPredHeart == testHeart$OVERALL_DIAGNOSIS) / length(testPredHeart)
trainAccHeart <- sum(trainPredHeart == trainHeart$OVERALL_DIAGNOSIS) / length(trainPredHeart)
print(predAccHeart)
print(trainAccHeart)
#variabelen used by tree are the following ['F13','F11','F16','F10','F22','F20']

#student dataset
student <- read.table("student-mat.csv", sep=';', row.names=NULL, header=TRUE)
student$Alcohol <- (student$Walc > 1) & (student$Dalc > 1)
student$Walc <- NULL
student$Dalc <- NULL

smp_size <- floor(0.9 * nrow(student))
set.seed(1)
train_ind <- sample(seq_len(nrow(student)), size = smp_size)
trainStudent <- student[train_ind, ]
testStudent <- student[-train_ind, ]

tree <- rpart(Alcohol ~ . , data=trainStudent, method='class', control = list(maxdepth = 3)) 
rpart.plot(tree)
testPredStudent <- predict(tree, testStudent, type = "class")
trainPredStudent <- predict(tree, trainStudent, type = "class")
predAccStudent <- sum(testPredStudent == testStudent$Alcohol) / length(testPredStudent)
trainAccStudent <- sum(trainPredStudent == trainStudent$Alcohol) / length(trainPredStudent)
print(predAccStudent)
print(trainAccStudent)
#variabelen used by tree are the following ['goout','famsize','reason','famsup','studytime','famsize','Mjob','activities','sex','famrel','G1']

#control = list(maxdepth = 1)






