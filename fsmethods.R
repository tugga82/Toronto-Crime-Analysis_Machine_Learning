#Feature Selection using information gain
library(FSelector)
set.seed(456)
#Using information gain filter on all the variables in the dataset did not provide appropriate variables for the model. Therefore, manually drop the variables that are not appropriate or relevant and then run the information gain to identify attributes that are important.
#Dataframe contains 27 variables. Drop the variables (14) that are not meaningful. This includes the following
#Index, event unique id, occurrence date and reported date in unix formats,ucr_code, ucr_ext, offence, reported year, month, day, day of year, day of week, hour, and objectid.
ind1<-sample(2, nrow(EDAfilter), replace = TRUE, prob=c(0.7,0.3))
featureselect<-EDAfilter[ind1 == 1,]
fstest<-EDAfilter[ind1 == 2,]
weights <- information.gain(MCI~., featureselect)
print(weights)
subset <- cutoff.k(weights, 2)
f <- as.simple.formula(subset, "MCI")
print(f)
#Applying information gain on the dataset after manually dropping several variables resulted in the following variables for applying the models. This includes premisetype, occurrencehour, division, neighbourhood, longitude and lattitude. 
set.seed(123)
ind2<-sample(2, nrow(EDAdrop), replace = TRUE, prob=c(0.7,0.3))
featureselect2<-EDAdrop[ind1 == 1,]
fstest2<-EDAdrop[ind1 == 2,]
weights2 <- information.gain(MCI~., featureselect2)
print(weights2)
subset2 <- cutoff.k(weights2, 2)
f2 <- as.simple.formula(subset2, "MCI")
print(f2)
#Run this on the 30% partitioned dataset and see how the information gain filter works
weights3 <- information.gain(MCI~., fstest2)
print(weights3)
subset3 <- cutoff.k(weights3, 2)
f3 <- as.simple.formula(subset3, "MCI")
print(f3)
#This run on the partitioned dataset also resulted in premisetype, occurrencehour, division, neighbourhood, longitude and lattitude as the most important variables. 
