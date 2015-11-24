# This is an ordinary linear regression.
# First we get and clean the data, 
# then we give all this data to the lm function with generate 
# the predictions. 
# We are using 2 independent (S(t-n) & S(t-(n+1))) variables to get
# the value of one dependent value (SOLR). Therefore the linear equation is:
# Spred = w1 * S(t-n) + w2 * S(t-(n+1)) + w3
# The function will generate w1, w2 and w3 for us based on the training data.
# Then, it will generate the predicted values applying the function on the 
# test data.

#To add a new station to the model just add its name on the vector below:stations = c("WNVH1", "SCSH1", "SCEH1", "SCBH1", "PLHH1", "MKRH1", "KTAH1", "KKRH1", "KFWH1", "C0875")
names = c("WNVH1", "SCSH1", "SCEH1", "SCBH1", "PLHH1", "MKRH1", "KTAH1", "KKRH1", "KFWH1", "C0875")
name = names[4]  #set to the station you want to run

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

# Clean the data, creating the necessary columns and getting only the hour interval we want #
clean = function(data)
{
  data$st1 = NA #create new column for S(t - 1)
  data$st2 = NA #create new column for S(t - 2)
  data = data[c("MON", "DAY", "YEAR", "HR", "SOLR", "st1", "st2")] #necessary columns for prediction
  rownames(data) = NULL #remove row names from data frame
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

# # # # # # # # # # # # # # # # # # # # TEST DATA # # # # # # # # # # # # # # # # # # # # 
test = read.csv(paste(name, "-2014.csv", sep = "")) #test data frame

test = clean(test) #test data frame
test = interpolation(test) #interpolate test data frame
test = getPast(test, n) #get the n hour before and the n + 1 hour before values

# # # # # # # # # # # # # # # # # # # # BUILD MODEL AND GET PREDICTIONS # # # # # # # # # # # # # # # # # # # # 
model = lm(SOLR ~ st1 + st2, data = train) #builds the linear regression model using SOLR as the dependet variable and both st1 and st2 as the independent variables
test$pred = predict(model, test) #get the predctions and add to the test data frame
test = test[(test$HR >= 8 & test$HR <= 17),]

# # # # # # # # # # # # # # # # # # # # GENERATE PLOTS AND THE MEAN ABSOLUTE ERROR # # # # # # # # # # # # # # # # # # # # 
linearm2f = test
linearm2f$error = abs(linearm2f$SOLR - linearm2f$pred)
mean(linearm2f$error, na.rm= TRUE);
hist(linearm2f$error, main=paste("Linear Model -", n, "Hours", sep=" "), ylim=c(0, 900), xlim=c(0, 800), xlab="Error")
boxplot(error~HR,data=linearm2f, main=paste("Linear Model -", n, "Hours", sep=" "), 
        xlab="Hour", ylab="Error", ylim=c(0, 900))

# # # # # # # # # # # # # # # # # # # # GENERATES CSV IN THE NEEDED FORMAT # # # # # # # # # # # # # # # # # # # # 
linearm2f = test[c("YEAR", "MON", "DAY", "HR", "pred")] #creates a new data frame in the needed format
colnames(linearm2f)[5] <- "SOLR" #change the name of the pred column to SOLR
write.csv(linearm2f, file = paste(name, "-f", n, ".csv", sep=""),row.names=FALSE) #generate the csv file