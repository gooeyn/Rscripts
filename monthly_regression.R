# This is a monthly linear regression.
# We'll build this model exactly like 
# the other linear models but instead
# of using the lm function once and
# giving all the data to it, we are
# going to divide by month. So we'll
# have one model for each month of 
# the year. Then, we'll get the predictions
# and bind the 12 data frames
# so we have a yearly csv.

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

# Clean the data, creating the necessary columns and getting only the hour interval we want #
clean = function(data)
{
  data$st1 = NA #create new column for S(t - 1)
  data$st2 = NA #create new column for S(t - 2)
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
train = read.csv(paste(name, "-2013.csv", sep = "")) #get the csv data
train = clean(train) #clean train data frame
train = interpolation(train) #interpolate train data frame
train = getPast(train, n) #get the n hour before and the n + 1 hour before values

# # # # # # # # # # # # # # # # # # # # TEST DATA # # # # # # # # # # # # # # # # # # # # 
test = read.csv(paste(name, "-2014.csv", sep = "")) #get the csv data
test = clean(test) #test data frame
test = interpolation(test) #interpolate test data frame
test = getPast(test, n) #get the n hour before and the n + 1 hour before values

# # # # # # # # # # # # # # # # # # # # BUILD MONTHLY MODEL # # # # # # # # # # # # # # # # # # # # 
months = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") #new vector containing the months of the year
trainmonthly <- list() #new data frame list which will contain 12 data frames, representes the months of the year
testmonthly <- list() #new data frame list which will contain 12 data frames, representes the months of the year
modelmonthly <- list() #new data frame list which will contain 12 data frames, representes the months of the year
for (i in 1:length(months)) #for 1 to 12 (going through all the months)
{
  trainmonthly[[i]] = train[(train$MON == i),] #each data frame on the data frame list receives its respective data frame based on the month number
  testmonthly[[i]]= test[(test$MON == i),] #each data frame on the data frame list receives its respective data frame based on the month number
  modelmonthly[[i]] = lm(SOLR ~ st1 + st2, data = trainmonthly[[i]]) #build a linear model based on the current month
  testmonthly[[i]]$pred = predict(modelmonthly[[i]], testmonthly[[i]]) #get the predictions, using the same month as the one used to build the model
}
linearm2monthly = rbind(testmonthly[[1]], testmonthly[[2]], testmonthly[[3]], testmonthly[[4]], testmonthly[[5]], testmonthly[[6]], testmonthly[[7]], testmonthly[[8]], testmonthly[[9]], testmonthly[[10]], testmonthly[[11]], testmonthly[[12]]) #we bind the monthly data frame to build a data frame for the whole year
linearm2monthly = linearm2monthly[(linearm2monthly$HR >= 8 & linearm2monthly$HR <= 17),]

# # # # # # # # # # # # # # # # # # # # GENERATE PLOTS AND THE MEAN ABSOLUTE ERROR # # # # # # # # # # # # # # # # # # # # 
linearm2monthly$error = abs(linearm2monthly$SOLR - linearm2monthly$pred)
mean(linearm2monthly$error, na.rm= TRUE);
hist(linearm2monthly$error, main=paste("Linear Model -", n, "Hours", sep=" "), ylim=c(0, 900), xlim=c(0, 800), xlab="Error")
boxplot(error~HR,data=linearm2monthly, main=paste("Linear Model -", n, "Hours", sep=" "), 
        xlab="Hour", ylab="Error", ylim=c(0, 900))

# # # # # # # # # # # # # # # # # # # # GENERATES CSV IN THE NEEDED FORMAT # # # # # # # # # # # # # # # # # # # # 
linearm2monthly = linearm2monthly[c("YEAR", "MON", "DAY", "HR", "pred")] #creates a new data frame in the needed format
colnames(linearm2monthly)[5] <- "SOLR" #change the name of the predictions column to SOLR
linearm2monthly = linearm2monthly[(linearm2monthly$HR >= 8 & linearm2monthly$HR <= 17),] #from 8am to 5pm
write.csv(test, file = paste(name, "-f", n, "-MONTHLY", ".csv", sep=""),row.names=FALSE) #generate the csv file