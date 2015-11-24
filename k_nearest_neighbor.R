# This is a k-nearest-neighbor regression
# We'll use the training data to build a model
# with the neighbor number = 3. The knn.reg function
# will then choose a prediction value based on S(t-n) and S(t-(n+1))
# and the respective number of neighbors.

#To add a new station to the model just add its name on the vector below:
names = c("WNVH1", "SCSH1", "SCEH1", "SCBH1", "PLHH1", "MKRH1", "KTAH1", "KKRH1", "KFWH1", "C0875")
name = names[4] #set to the station you want to run

n = 1 #n is the number of hours ahead you want to predict

# Interpolate by getting the mean of the adjancet values #
interpolation = function(data)
{
  for (i in 1:nrow(data)) 
  {
    if(is.na(data[(i),]$SOLR)) data[(i),]$SOLR = mean(c(data[(i-1),]$SOLR, data[(i+1),]$SOLR), na.rm = TRUE)
  }
  data
}

# Clean the data, creating the necessary columns and getting only the hour interval we want #
clean = function(data)
{
  data$st1 = 0 #create new column for S(t - 1)
  data$st2 = 0 #create new column for S(t - 2)
  data = data[c("MON", "DAY", "YEAR", "HR", "SOLR", "st1", "st2")] #necessary columns for prediction
  rownames(data) = NULL #remove row names from data frame
  data
}

#Get the past data for the prediction. S(t-n) and S(t-(n+1)) #
getPast = function(data, n)
{
  for (t in (n+1):nrow(data)) 
  {
    data$st1[t] = data$SOLR[t-(n)] #get the n hour before solar irradiance
  }
  
  for (t in (n+2):nrow(data)) 
  {
    data$st2[t] = data$SOLR[t-(n+1)] #get the (n + 1) hour before solar irradiance
  }
  data
}

# # # # # # # # # # # # # # # # # # # # TRAINING DATA # # # # # # # # # # # # # # # # # # # # 
train1 = read.csv(paste(name, "-2008.csv", sep = "")) #first data frame
train2 = read.csv(paste(name, "-2012.csv", sep = "")) #second data frame
train3 = read.csv(paste(name, "-2013.csv", sep = "")) #third data frame
train = rbind(train1, train2, train3) #put the data frames together

train = clean(train) #clean train data frame
train = interpolation(train) #interpolate train data frame
train = getPast(train, n) #get the n hour before and the n + 1 hour before values
train.SOLR = train$SOLR #store the solar radiation
traindata = train[c("st1", "st2")] #create a new data frame containing only s(t-n) and s(t-(n+1))

# # # # # # # # # # # # # # # # # # # # TEST DATA # # # # # # # # # # # # # # # # # # # # 
test = read.csv(paste(name, "-2014.csv", sep = "")) #test data frame

test = clean(test) #test data frame
test = interpolation(test) #interpolate test data frame
test = getPast(test, n) #get the n hour before and the n + 1 hour before values
test.SOLR = test$SOLR #store the solar radiation
testdata = test[c("st1", "st2")] #create a new data frame containing only s(t-n) and s(t-(n+1))

# # # # # # # # # # # # # # # # # # # # BUILD KNN MODEL AND GET PREDICTIONS # # # # # # # # # # # # # # # # # # # # 
library(FNN)
test.knn <- knn.reg(traindata, test = testdata, y=train.SOLR, k=5);
test$pred = test.knn$pred
test$SOLR = test.SOLR
test = test[(test$HR >= 8 & test$HR <= 17),] #from 8am to 5pm

# # # # # # # # # # # # # # # # # # # # GENERATE PLOTS AND THE MEAN ABSOLUTE ERROR # # # # # # # # # # # # # # # # # # # # 
knn = test
knn$error = abs(knn$SOLR - knn$pred)
mean(knn$error, na.rm = TRUE)
hist(knn$error, main = "KNN", ylim=c(0, 900), xlim=c(0, 800), xlab="Error")
boxplot(error~HR,data=knn, main="KNN", 
        xlab="Hour", ylab="Error", ylim=c(0, 900))

# # # # # # # # # # # # # # # # # # # # GENERATES CSV IN THE NEEDED FORMAT # # # # # # # # # # # # # # # # # # # # 
knn = knn[c("YEAR", "MON", "DAY", "HR", "pred")]
colnames(knn)[5] <- "SOLR" #change the name of the predictions column to SOLR
write.csv(test, file = paste(name, "-f", n, "-KNN", ".csv", sep=""),row.names=FALSE) #generate the csv file