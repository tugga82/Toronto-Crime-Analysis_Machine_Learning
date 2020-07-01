library(ggplot2)
library(dplyr)
library(plyr)
library(Boruta)
crimedata<-read.csv("/Users/tugga/Downloads/MCI_2014_to_2019.csv", header = TRUE)
str(crimedata)
crimenew<-na.omit(crimedata)
EDAfilter<-filter(crimenew, occurrenceyear == 2014 | occurrenceyear == 2015 | occurrenceyear == 2016 | occurrenceyear == 2017 | occurrenceyear == 2018 | occurrenceyear == 2019)
EDAfilter$occurrencemonth<-factor(EDAfilter$occurrencemonth, levels=c("January","February","March","April","May", "June", "July", "August", "September", "October", "November", "December"))
EDAfilter$occurrencedayofweek = gsub(" ", "", EDAfilter$occurrencedayofweek)
EDAfilter$occurrencedayofweek<-factor(EDAfilter$occurrencedayofweek, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
#Dataframe contains 27 variables. Drop the variables (14) that are not meaningful. This includes the following
#Index, event unique id, occurrence date and reported date in unix formats,ucr_code, ucr_ext, offence, reported year, month, day, day of year, day of week, hour, and objectid.
EDAdrop<-subset(EDAfilter, select = -c(1,2,3,4,6,7,8:14,27))
set.seed(123)
ind<-sample(2, nrow(EDAdrop), replace = TRUE, prob=c(0.7,0.3))
trainset<-EDAdrop[ind == 1,]
testset<-EDAdrop[ind == 2,]
#Apply feature selection with the Boruta Algorithm
set.seed(456)
boruta<-Boruta(MCI~.,data=trainset, doTrace=2)
print(boruta)
#Plot the boruta variable importance chart
plot(boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta$ImpHistory),function(i)
  boruta$ImpHistory[is.finite(boruta$ImpHistory[,i]),i])
names(lz) <- colnames(boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(boruta$ImpHistory), cex.axis = 0.7)
getSelectedAttributes(boruta, withTentative = F)
boruta.df <- attStats(boruta)
print(boruta.df)
#Although Boruta confirmed all the attributes, based on the importance scores and what we know about the attributes based on the definitions, we can reject division, hood_id, and occurrenceyear.