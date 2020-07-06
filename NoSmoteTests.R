EDA<-read.csv("C:\\Users\\vidyasankar.sundar\\Desktop\\Big Data Project\\MCI_2014_to_2019.csv", header = TRUE)
EDANEW<-na.omit(EDA)
EDAfilter<-filter(EDANEW, occurrenceyear == "2014" | occurrenceyear == "2015" | occurrenceyear == "2016" | occurrenceyear == "2017" | occurrenceyear == "2018" | occurrenceyear == "2019")
EDAfilter$occurrencemonth<-factor(EDAfilter$occurrencemonth, levels=c("January","February","March","April","May", "June", "July", "August", "September", "October", "November", "December"))
EDAfilter$occurrencedayofweek = gsub(" ", "", EDAfilter$occurrencedayofweek)
EDAfilter$occurrencedayofweek<-factor(EDAfilter$occurrencedayofweek, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
EDAfilter$Neighbourhood<-as.factor(EDAfilter$Neighbourhood)
EDAfilter<-subset(EDAfilter, select = -c(1,2,3,4,6:14,22,23,27))
procdata<-EDAfilter
#remove occdate in unix format, occday of year also convert prem type and mci to factors
procdata$premisetype<-as.factor(procdata$premisetype)
procdata$MCI<-as.factor(procdata$MCI)
#usenumdata variable and convert everything to numeric
numdata<-procdata
save(numdata, file = "numdata.RData")
load("numdata.RData")
#Drop Year and Neighbourhood as those are not needed for training.
numdata<-subset(numdata, select = -c(2,8))
set.seed(123)
ind<-sample(2, nrow(numdata), replace = TRUE, prob=c(0.7,0.3))
trainset<-numdata[ind == 1,]
testset<-numdata[ind == 2,]
# J48 Decision Tree
set.seed(123)
dtree.J48<-J48(MCI~., data=trainset, control=Weka_control(M=100))
dtree.pred<-predict(dtree.J48, testset, type="class")
dtree.conf<-table(testset$MCI, dtree.pred, dnn=c("Actual", "Predicted"))
confusionMatrix(testset$MCI, dtree.pred)
evaluate_Weka_classifier(dtree.J48,  numFolds = 10) 
#implement KNN
set.seed(123)
knn.classifier<-IBk (MCI~., data=trainset, control=Weka_control(K=3, X = TRUE))
evaluate_Weka_classifier(knn.classifier, numFolds = 5)
knn.pred<-predict(knn.classifier, testset, type="class")
knn.conf<-table(testset$MCI, knn.pred, dnn=c("Actual", "Predicted"))
confusionMatrix(testset$MCI, knn.pred)
#implement Naive Bayes
set.seed(123)
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
b.classifier <- NB(MCI~., data=trainset)
evaluate_Weka_classifier(b.classifier, numFolds = 10)
b.pred<-predict(b.classifier, testset, type="class")
b.conf<-table(testset$MCI, b.pred, dnn=c("Actual", "Predicted"))
confusionMatrix(testset$MCI, b.pred)
#implement Random Forest
set.seed(123)
RF<-make_Weka_classifier("weka/classifiers/trees/RandomForest")
rf.classifier<- RF(MCI~., data=trainset)
evaluate_Weka_classifier(rf.classifier, numFolds = 5)
rf.pred<-predict(rf.classifier, testset, type="class")
rf.conf<-table(testset$MCI, rf.pred, dnn=c("Actual", "Predicted"))
confusionMatrix(testset$MCI, rf.pred)

