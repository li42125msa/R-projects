library(C50)
churn
str(churnTrain);
churnTrain = churnTrain[,! names(churnTrain) %in% c("state", "area_code", "account_length") ]

set.seed(2)
ind = sample(2, nrow(churnTrain), replace = TRUE, prob=c(0.7, 0.3));ind;table(ind)
 trainset = churnTrain[ind == 1,]
 testset = churnTrain[ind == 2,]

 dim(trainset)
library(adabag) 
 set.seed(2)
churn.bagging = bagging(churn ~ ., data=trainset, mfinal=10)
 
library(ipred)

trace(utils:::unpackPkgZip, edit=TRUE)
;install.packages("ipred")
