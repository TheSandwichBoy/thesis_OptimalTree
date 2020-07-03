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
avgLeavesList <- c()
avgAccList <- c()

validate_ind <- sample(seq_len(nrow(mushroom)), size = floor(0.1*nrow(mushroom)))

validateMush <- mushroom[validate_ind, ]
trainTestMush <- mushroom[-validate_ind, ]

for (i in 1:5) {
  
  train_ind <- sample(seq_len(nrow(trainTestMush)), size = 600)
  trainMush <- trainTestMush[train_ind, ]
  testMush <- trainTestMush[-train_ind, ]
  
  tree <- rpart(edible ~ . , data=trainMush, method='class') 
  rpart.plot(tree)
  tree
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  
  testPredMush <- predict(tree, testMush, type = "class")
  predAccMush <- sum(testPredMush == testMush$edible) / nrow(testMush)
  
  print(predAccMush)
  
  validatePredMush <- predict(tree, validateMush, type = "class")
  ValidateAccMush <- sum(validatePredMush == validateMush$edible) / nrow(validateMush)
  
  print(ValidateAccMush)
  
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccMush)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)

#breast dataset
set.seed(1)
breast <- read.table("breast.txt", sep=" ")
breast[,2] <- NULL
colnames(breast) <- c('Class','Sample code number','Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion','Single Epithelial Cell Size','Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses')
breast$'Sample code number' <- NULL
avgLeavesList <- c()
avgAccList <- c()

validate_ind <- sample(seq_len(nrow(breast)), size = floor(0.1*nrow(breast)))

validateBreast <- breast[validate_ind, ]
trainTestBreast <- breast[-validate_ind, ]

for (i in 1:5) {
  train_ind <- sample(seq_len(nrow(trainTestBreast)), size = 600)
  trainBreast <- trainTestBreast[train_ind, ]
  testBreast <- trainTestBreast[-train_ind, ]
  
  tree <- rpart(Class ~ . , data=trainBreast, method='class')
  
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  
  testPredBreast <- predict(tree, testBreast, type = "class")
  predAccBreast <- sum(testPredBreast == testBreast$Class) / length(testPredBreast)
  
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccBreast)
  
  print(avgLeavesList)
  print(avgAccList)
  
  testPredBreastValidate <- predict(tree, validateBreast, type = "class")
  predAccBreastValidate <- sum(testPredBreastValidate == validateBreast$Class) / length(testPredBreastValidate)
  print(predAccBreastValidate)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)

#variabelen used by tree are the following['Uniformity of Cell Size', 'Bare Nuclei', 'Uniformity of Cell Shape']

#chess dataset
set.seed(1)
chess <- read.table("kr-vs-kp.data", sep=",")
colnames(chess) <- c('bkblk','bknwy','bkon8','bkona','bkspr','bkxbq','bkxcr','bkxwp','blxwp','bxqsq','cntxt','dsopp','dwipd','hdchk','katri','mulch','qxmsq','r2ar8','reskd','reskr','rimmx','rkxwp','rxmsq','simpl','skach','skewr','skrxp','spcop','stlmt','thrsk','wkcti','wkna8','wknck','wkovl','wkpos','wtoeg','white_win')
avgLeavesList <- c()
avgAccList <- c()

validate_ind <- sample(seq_len(nrow(chess)), size = floor(0.1*nrow(chess)))

validatechess <- chess[validate_ind, ]
trainTestchess <- chess[-validate_ind, ]


for (i in 1:5) {
  
  train_ind <- sample(seq_len(nrow(trainTestchess)), size = floor(0.9*nrow(trainTestchess)))
  trainChess <- trainTestchess[train_ind, ]
  testChess <- trainTestchess[-train_ind, ]
  
  tree <- rpart(white_win ~ . , data=trainChess, method='class')
  
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  
  testPredChess <- predict(tree, testChess, type = "class")
  predAccChess <- sum(testPredChess == testChess$white_win) / length(testPredChess)
  
  print(predAccChess)
  
  validatePredChess <- predict(tree, validatechess, type = "class")
  ValidateAccChess <- sum(validatePredChess == validatechess$white_win) / length(validatePredChess)
  
  print(ValidateAccChess)
  
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccChess)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)



#adult dataset
set.seed(1)
adult <- read.table("a1a.txt", sep=" ", fill=TRUE)
adult[,16] <- NULL
colnames(adult) <- c('salery50k','age','workclass','fnlwgt','education','education-num','marital-status','occupation','relationship','race','sex','capital-gain','capital-loss','hours-per-week','native-country')
adult <- adult[!(is.na(adult$`native-country`) | adult$`native-country`==""), ]
avgLeavesList <- c()
avgAccList <- c()

validate_ind <- sample(seq_len(nrow(adult)), size = floor(0.1*nrow(adult)))

validateadults <- adult[validate_ind, ]
trainTestadult <- adult[-validate_ind, ]

for (i in 1:5) {
  
  train_ind <- sample(seq_len(nrow(trainTestadult)), size = 600)
  trainAdult <- trainTestadult[train_ind, ]
  testAdult <- trainTestadult[-train_ind, ]
  
  tree <- rpart(salery50k ~ . , data=trainAdult, method='class')
  
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  
  testPredAdult <- predict(tree, testAdult, type = "class")
  predAccAdult <- sum(testPredAdult == testAdult$salery50k) / length(testPredAdult)
  
  print(predAccAdult)
  
  valiPredAdult <- predict(tree, validateadults, type = "class")
  valiAccAdult <- sum(valiPredAdult == validateadults$salery50k) / length(valiPredAdult)
  
  print(valiAccAdult)
  
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccAdult)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)


#votes dataset
set.seed(1)
votes <- read.table("house-votes-84.data", sep=",", fill=TRUE)
colnames(votes) <- c('Class','handicapped-infants','water-project-cost-sharing','adoption-of-the-budget-resolution','physician-fee-freeze','el-salvador-aid','religious-groups-in-schools','anti-satellite-test-ban','aid-to-nicaraguan-contras','mx-missile','immigration','synfuels-corporation-cutback','education-spending','superfund-right-to-sue','crime','duty-free-exports','export-administration-act-south-africa')
votes[votes == '?'] <- NA
votes <-na.omit(votes)
avgLeavesList <- c()
avgAccList <- c()

validate_ind <- sample(seq_len(nrow(votes)), size = floor(0.1*nrow(votes)))

validateVotes <- votes[validate_ind, ]
trainTestVotes <- votes[-validate_ind, ]

folds <- cut(seq(1,nrow(trainTestVotes)),breaks=5,labels=FALSE)

for (i in 1:5) {
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testVotes <- trainTestVotes[testIndexes, ]
  trainVotes <- trainTestVotes[-testIndexes, ]
  
  tree <- rpart(Class ~ . , data=trainVotes, method='class')
  
  testPredVotes <- predict(tree, testVotes, type = "class")
  predAccVotes <- sum(testPredVotes == testVotes$Class) / nrow(testVotes)
  
  print(predAccVotes)
  ValPredVotes <- predict(tree, validateVotes, type = "class")
  ValAccVotes <- sum(ValPredVotes == validateVotes$Class) / nrow(validateVotes)
  
  print(ValAccVotes)
  
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccVotes)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)




#heloc dataset
set.seed(1)
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


validate_ind <- sample(seq_len(nrow(heloc)), size = floor(0.1*nrow(heloc)))

validateHeloc <- heloc[validate_ind, ]
trainTestHeloc <- heloc[-validate_ind, ]


avgLeavesList <- c()
avgAccList <- c()
for (i in 1:5) {

  train_ind <- sample(seq_len(nrow(trainTestHeloc)), size = 600)
  trainHeloc <- trainTestHeloc[train_ind, ]
  testHeloc <- trainTestHeloc[-train_ind, ]
  
  tree <- rpart(RiskPerformance ~ . , data=trainHeloc, method='class') 
  
  testPredHeloc <- predict(tree, testHeloc, type = "class")
  predAccHeloc <- sum(testPredHeloc == testHeloc$RiskPerformance) / length(testPredHeloc)
  
  print(predAccHeloc)
  
  ValPredHeloc <- predict(tree, validateHeloc, type = "class")
  ValAccHeloc <- sum(ValPredHeloc == validateHeloc$RiskPerformance) / length(ValPredHeloc)
  
  print(ValAccHeloc)
  
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccHeloc)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)


#tictactoe dataset
set.seed(1)
tictactoe <- read.table("tic-tac-toe.data", sep=",", fill=TRUE)
colnames(tictactoe) <- cbind('top-left-square','top-middle-square','top-right-square','middle-left-square','middle-middle-square','middle-right-square','bottom-left-square','bottom-middle-square','bottom-right-square','Class')



avgLeavesList <- c()
avgAccList <- c()

validate_ind <- sample(seq_len(nrow(tictactoe)), size = floor(0.1*nrow(tictactoe)))

validatetictactoe <- tictactoe[validate_ind, ]
trainTesttictactoe <- tictactoe[-validate_ind, ]

for (i in 1:5) {
  
  train_ind <- sample(seq_len(nrow(trainTesttictactoe)), size = 600)
  trainTictactoe <- trainTesttictactoe[train_ind, ]
  testTictactoe <- trainTesttictactoe[-train_ind, ]
  
  tree <- rpart(Class ~ . , data=trainTictactoe, method='class')
  
  testPredTictactoe <- predict(tree, testTictactoe, type = "class")
  predAccTictactoe <- sum(testPredTictactoe == testTictactoe$Class) / length(testPredTictactoe)
  
  valPredTictactoe <- predict(tree, validatetictactoe, type = "class")
  valAccTictactoe <- sum(valPredTictactoe == validatetictactoe$Class) / length(valPredTictactoe)
  
  print(predAccTictactoe)
  print(valAccTictactoe)
  
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccTictactoe)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)

#monks dataset

monks <- read.table("monks-1.test", sep=" ", fill=TRUE)
colnames(monks) <- cbind('empty','class','a1','a2','a3','a4','a5','a6','Id')
monks$empty <- NULL
monks$Id <- NULL

avgLeavesList <- c()
avgAccList <- c()
set.seed(2)

validate_ind <- sample(seq_len(nrow(monks)), size = floor(0.1*nrow(monks)))

validatetictactoe <- monks[validate_ind, ]
trainTesttictactoe <- monks[-validate_ind, ]


folds <- cut(seq(1,nrow(trainTesttictactoe)),breaks=5,labels=FALSE)

for (i in 1:5) {
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testMonks <- trainTesttictactoe[testIndexes, ]
  trainMonks <- trainTesttictactoe[-testIndexes, ]

  
  tree <- rpart(class ~ . , data=trainMonks, method='class') 
  rpart.plot(tree)
  
  testPredMonks <- predict(tree, testMonks, type = "class")
  predAccMonks <- sum(testPredMonks == testMonks$class) / length(testPredMonks)
  print(predAccMonks)
  
  valiPredMonks <- predict(tree, validatetictactoe, type = "class")
  valiAccMonks <- sum(valiPredMonks == validatetictactoe$class) / length(valiPredMonks)
  print(valiAccMonks)
  
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccMonks)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)


#heart dataset
heart1 <- read.table("SPECT.test", sep=',', fill=TRUE)
heart2 <- read.table("SPECT.train", sep=',', fill=TRUE)
heart <- rbind(heart1, heart2)
colnames(heart) <- cbind('OVERALL_DIAGNOSIS','F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12','F13','F14','F15','F16','F17','F18','F19','F20','F21','F22')

avgLeavesList <- c()
avgAccList <- c()
set.seed(1)


validate_ind <- sample(seq_len(nrow(heart)), size = floor(0.1*nrow(heart)))

validateheart <- heart[validate_ind, ]
trainTestheart <- heart[-validate_ind, ]


folds <- cut(seq(1,nrow(trainTestheart)),breaks=5,labels=FALSE)

for (i in 1:5) {
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testHeart <- trainTestheart[testIndexes, ]
  trainHeart <- trainTestheart[-testIndexes, ]
  
  tree <- rpart(OVERALL_DIAGNOSIS ~ . , data=trainHeart, method='class') 
  rpart.plot(tree)
  testPredHeart <- predict(tree, testHeart, type = "class")
  predAccHeart <- sum(testPredHeart == testHeart$OVERALL_DIAGNOSIS) / length(testPredHeart)
  print(predAccHeart)
  valPredHeart <- predict(tree, validateheart, type = "class")
  valAccHeart <- sum(valPredHeart == validateheart$OVERALL_DIAGNOSIS) / length(valPredHeart)
  print(valAccHeart)
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccHeart)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)


#student dataset
student <- read.table("student-mat.csv", sep=';', row.names=NULL, header=TRUE)
student$Alcohol <- (student$Walc > 1) & (student$Dalc > 1)
student$Walc <- NULL
student$Dalc <- NULL

avgLeavesList <- c()
avgAccList <- c()
set.seed(1)

validate_ind <- sample(seq_len(nrow(student)), size = floor(0.1*nrow(student)))

validatestudent <- student[validate_ind, ]
trainTeststudent <- student[-validate_ind, ]


folds <- cut(seq(1,nrow(trainTeststudent)),breaks=5,labels=FALSE)

for (i in 1:5) {
  testIndexes <- which(folds==i,arr.ind=TRUE)
  trainStudent <- trainTeststudent[testIndexes, ]
  testStudent <- trainTeststudent[-testIndexes, ]

  tree <- rpart(Alcohol ~ . , data=trainStudent, method='class') 
  
  rpart.plot(tree)
  
  testPredStudent <- predict(tree, testStudent, type = "class")
  predAccStudent <- sum(testPredStudent == testStudent$Alcohol) / length(testPredStudent)
  print(predAccStudent)
  ValPredStudent <- predict(tree, validatestudent, type = "class")
  ValAccStudent <- sum(ValPredStudent == validatestudent$Alcohol) / length(ValPredStudent)
  print(ValAccStudent)
  
  leaveAmount <- sum(tree$frame$var == "<leaf>")
  avgLeavesList <- c(avgLeavesList, leaveAmount)
  avgAccList <- c(avgAccList, predAccStudent)
  
}
print(mean(avgLeavesList))
print(mean(avgAccList) * 100)





