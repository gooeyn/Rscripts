# This is a hourly deseasonalized linear regression
# We'll get the data from past year and get each hour mean. For example:
# Let's suppose the training data is 2011, 2012 and 2013 and the Solar Radiation for each year is
# respectively, 100, 200 and 300. We are going to deseasonalize the data by getting the mean, max or min
# of those values. If we deseasonalized by mean (200 in this case) we would have to subtract the mean by 
# each Solar Radiaton value, therefore we would have -100, 0 and 100 for the years above.
# after the deseasonalation we just run the linear regression as before, getting the s(t-1) and s(t-2)
# deseasonalized values as the independent variables and SOLR as the dependent.
# After we have the predict values we add the mean value we got before to get the real prediction.


#To add a new station to the model just add its name on the vector below:
names = c("WNVH1", "SCSH1", "SCEH1", "SCBH1", "PLHH1", "MKRH1", "KTAH1", "KKRH1", "KFWH1", "C0875")
name = names[4] #set to the station you want to run

n = 1 #n is the number of hours ahead you want to predict
m = 1 #1 - deseasonalize by MEAN
      #2 - deseasonalize by MAX
      #3 - deseasonalize by MIN

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
  data$MEAN = NA #create new column for the mean value
  data$DES = NA #create new column for the deseasonalized value
  data$st1 = NA #create new column for S(t - 1)
  data$st2 = NA #create new column for S(t - 2)
  data = data[c("MON", "DAY", "YEAR", "HR", "SOLR", "MEAN", "DES", "st1", "st2")] #necessary columns for prediction
  data = data[(data$HR >= 6 & data$HR <= 18),] #hour interval we want
  rownames(data) = NULL #remove row names from data frame
  data
}

#Get the past data for the prediction. S(t-n) and S(t-(n+1)) #
getPast = function(data, n)
{
  for (t in (n+1):nrow(data)) 
  {
    data$st1[t] = data$DES[t-(n)] #get the n hour before solar irradiance
  }
  
  for (t in (n+2):nrow(data)) 
  {
    data$st2[t] = data$DES[t-(n+1)] #get the (n + 1) hour before solar irradiance
  }
  data
}

#Get the deseasonalized value (SOLR - MEAN) #
getDES = function(data)
  {
      for (i in 1:nrow(data)) 
      {
        data$DES[i] = data$SOLR[i] - data$MEAN[i]
      }
      data
  }
  
# # # # # # # # # # # # # # # # # # # # TRAINING DATA # # # # # # # # # # # # # # # # # # # # 
train = list() #create a list of data frames
train[[1]] = read.csv(paste(name, "-2008.csv", sep = "")) #first data frame
train[[2]] = read.csv(paste(name, "-2012.csv", sep = "")) #second data frame
train[[3]] = read.csv(paste(name, "-2013.csv", sep = "")) #third data frame

# Clean all the data frames #
train[[1]] = clean(train[[1]])
train[[2]] = clean(train[[2]])
train[[3]] = clean(train[[3]])
 
# Interpolate all the data frames #
train[[1]] = interpolation(train[[1]])
train[[2]] = interpolation(train[[2]])
train[[3]] = interpolation(train[[3]])

# # # # # # # # # # # # # # # # # # # # TEST DATA # # # # # # # # # # # # # # # # # # # # 
test = read.csv(paste(name, "-2014.csv", sep = "")) #test data frame
test = clean(test) #test data frame
test = interpolation(test) #interpolate test data frame

# # # # # # # # # # # # # # # # # # # # GENERATE MEAN/MAX/MIN VALUE # # # # # # # # # # # # # # # # # # # # 
for (i in 1:nrow(test)) #goes through all the rows of the test data frame
{
  #get the respective row from each data set
  data1 = train[[1]][(train[[1]]$MON == test$MON[i] & train[[1]]$DAY == test$DAY[i] & train[[1]]$HR == test$HR[i]),] 
  data2 = train[[2]][(train[[2]]$MON == test$MON[i] & train[[2]]$DAY == test$DAY[i] & train[[2]]$HR == test$HR[i]),] 
  data3 = train[[3]][(train[[3]]$MON == test$MON[i] & train[[3]]$DAY == test$DAY[i] & train[[3]]$HR == test$HR[i]),] 
  
  # Treat missing values for the first data frame # (not handled by the interpolation because the ROWS are missing)
  if(nrow(data1) <= 0) # if the value is missing get the value before and the after
  {
    before1 = train[[1]][(train[[1]]$MON == test$MON[i] & train[[1]]$DAY == test$DAY[i] & train[[1]]$HR == test$HR[i]-1),] 
    after1 = train[[1]][(train[[1]]$MON == test$MON[i] & train[[1]]$DAY == test$DAY[i] & train[[1]]$HR == test$HR[i]+1),] 
    if(((nrow(before1)) > 0) &&  ((nrow(after1)) > 0)) #if the before and the after are not missing (the rows)
    {
      if((!is.na(before1$SOLR)) && (!is.na(after1$SOLR))) #if both of the values are not NAs
      {
        data1.SOLR = mean(c(before1$SOLR, after1$SOLR)) #set the missing value to the mean of the adjacent values
      }
      else #if one of the values is NA
      {
        data1.SOLR = NA #set the solar radiance to NA
      }
    } 
    else #if either before oe after value is missing
    {
      data1.SOLR = NA #set the solar radiance to NA
    }
  } 
  else #if the value is not missing
  {
    data1.SOLR = data1$SOLR #set the solar radiance to the solar radiance we got
  }
  # Treat missing values for the second data frame # (not handled by the interpolation because the ROWS are missing)
  if(nrow(data2) <= 0) # if the value is missing get the value before and the after
  {
    before2 = train[[2]][(train[[2]]$MON == test$MON[i] & train[[2]]$DAY == test$DAY[i] & train[[2]]$HR == test$HR[i]-1),] 
    after2 = train[[2]][(train[[2]]$MON == test$MON[i] & train[[2]]$DAY == test$DAY[i] & train[[2]]$HR == test$HR[i]+1),] 
    if(((nrow(before2)) > 0) &&  ((nrow(after2)) > 0)) #if the before and the after are not missing (the rows)
    {
      if((!is.na(before2$SOLR)) && (!is.na(after2$SOLR))) #if both of the values are not NAs
      {
        data2.SOLR = mean(c(before2$SOLR, after2$SOLR))  #set the missing value to the mean of the adjacent values
      }
      else #if one of the values is NA
      {
        data2.SOLR = NA #set the solar radiance to NA
      }
    }
    else #if either before oe after value is missing
    {
      data2.SOLR = NA #set the solar radiance to NA
    }
  } 
  else #if the value is not missing
  {
    data2.SOLR = data2$SOLR #set the solar radiance to the solar radiance we got
  }
  # Treat missing values for the third data frame # (not handled by the interpolation because the ROWS are missing)
  if(nrow(data3) <= 0) # if the value is missing get the value before and the after
  {
    before3 = train[[3]][(train[[3]]$MON == test$MON[i] & train[[3]]$DAY == test$DAY[i] & train[[3]]$HR == test$HR[i]-1),]
    after3 = train[[3]][(train[[3]]$MON == test$MON[i] & train[[3]]$DAY == test$DAY[i] & train[[3]]$HR == test$HR[i]+1),] 
    if(((nrow(before3)) > 0) &&  ((nrow(after3)) > 0)) #if the before and the after are not missing (the rows)
    {
      if((!is.na(before3$SOLR)) && (!is.na(after3$SOLR))) #if both of the values are not NAs
      {
        data3.SOLR = mean(c(before3$SOLR, after3$SOLR)) #set the missing value to the mean of the adjacent values
      }
      else #if one of the values is NA
      {
        data3.SOLR = NA #set the solar radiance to NA
      }
    }
    else #if either before oe after value is missing
    {
      data3.SOLR = NA #set the solar radiance to NA
    }
  } 
  else #if the value is not missing
  {
    data3.SOLR = data3$SOLR #set the solar radiance to the solar radiance we got
  }
  # Get the mean/max/min and insert it into the data frame #
  if (m == 1) test$MEAN[i] = mean(c(data1.SOLR, data2.SOLR, data3.SOLR), na.rm = TRUE)
  if (m == 2) test$MEAN[i] = max(data1$SOLR, data2$SOLR, data3$SOLR, na.rm = TRUE)
  if (m == 3) test$MEAN[i] = min(data1$SOLR, data2$SOLR, data3$SOLR, na.rm = TRUE)  
}

# # # # # # # # # # # # # # # # # # # # INSERTING THE VALUE(MEAN/MAX/MIN) INTO THE TRAINING DATA # # # # # # # # # # # # # # # # # # # # 
for (x in 1:length(train))
  {
    for (i in 1:nrow(train[[x]])) 
    {
      data = test[(test$MON == train[[x]]$MON[i] & test$DAY == train[[x]]$DAY[i] & test$HR ==train[[x]]$HR[i]),]
      
      if(nrow(data) <= 0)
      {
        data1 = train[[1]][(train[[1]]$MON == train[[x]]$MON[i] & train[[1]]$DAY == train[[x]]$DAY[i] & train[[1]]$HR == train[[x]]$HR[i]),] 
        data2 = train[[2]][(train[[2]]$MON == train[[x]]$MON[i] & train[[2]]$DAY == train[[x]]$DAY[i] & train[[2]]$HR == train[[x]]$HR[i]),] 
        data3 = train[[3]][(train[[3]]$MON == train[[x]]$MON[i] & train[[3]]$DAY == train[[x]]$DAY[i] & train[[3]]$HR == train[[x]]$HR[i]),] 
        if(nrow(data1) <= 0) 
        {
          before1 = train[[1]][(train[[1]]$MON == train[[x]]$MON[i] & train[[1]]$DAY == train[[x]]$DAY[i] & train[[1]]$HR == train[[x]]$HR[i]-1),] 
          
          after1 = train[[1]][(train[[1]]$MON == train[[x]]$MON[i] & train[[1]]$DAY == train[[x]]$DAY[i] & train[[1]]$HR == train[[x]]$HR[i]-1),] 
          
          if(((nrow(before1)) > 0) &&  ((nrow(after1)) > 0))
          {
            if((!is.na(before1$SOLR)) && (!is.na(after1$SOLR)))
            {
              data1.SOLR = mean(c(before1$SOLR, after1$SOLR))
            }
            else 
            {
              data1.SOLR = NA
            }
          }
          else 
          {
            data1.SOLR = NA
          }
        } 
        else 
        {
          data1.SOLR = data1$SOLR
        }
        
        if(nrow(data2) <= 0) 
        {
          before2 = train[[2]][(train[[2]]$MON == train[[x]]$MON[i] & train[[2]]$DAY == train[[x]]$DAY[i] & train[[2]]$HR == train[[x]]$HR[i]-1),] 
          
          after2 = train[[2]][(train[[2]]$MON == train[[x]]$MON[i] & train[[2]]$DAY == train[[x]]$DAY[i] & train[[2]]$HR == train[[x]]$HR[i]+1),] 
          
          if(((nrow(before2)) > 0) &&  ((nrow(after2)) > 0))
          {
            if((!is.na(before2$SOLR)) && (!is.na(after2$SOLR)))
            {
              data2.SOLR = mean(c(before2$SOLR, after2$SOLR))
            }
            else 
            {
              data2.SOLR = NA
            }
          }
          else 
          {
            data2.SOLR = NA
          }
        } 
        else 
        {
          data2.SOLR = data2$SOLR
        }
        
        if(nrow(data3) <= 0) 
        {
          before3 = train[[3]][(train[[3]]$MON == train[[x]]$MON[i] & train[[3]]$DAY == train[[x]]$DAY[i] & train[[3]]$HR == train[[x]]$HR[i]-1),] 
          
          after3 = train[[3]][(train[[3]]$MON == train[[x]]$MON[i] & train[[3]]$DAY == train[[x]]$DAY[i] & train[[3]]$HR == train[[x]]$HR[i]+1),] 
          
          if(((nrow(before3)) > 0) &&  ((nrow(after3)) > 0))
          {
            if((!is.na(before3$SOLR)) && (!is.na(after3$SOLR)))
            {
              data3.SOLR = mean(c(before3$SOLR, after3$SOLR))
            }
            else 
            {
              data3.SOLR = NA
            }
          }
          else 
          {
            data3.SOLR = NA
          }
        } 
        else 
        {
          data3.SOLR = data3$SOLR
        }
        if (m == 1) train[[x]]$MEAN[i] = mean(c(data1.SOLR, data2.SOLR, data3.SOLR), na.rm = TRUE)
      }  
      else train[[x]]$MEAN[i] = data$MEAN 
    }
  }

# Get the deseasonalized value #
train[[1]] = getDES(train[[1]])
train[[2]] = getDES(train[[2]])
train[[3]] = getDES(train[[3]])
test = getDES(test)

# Get the n hour before and the n + 1 hour before values #
train[[1]] = getPast(train[[1]], n)
train[[2]] = getPast(train[[2]], n)
train[[3]] = getPast(train[[3]], n)
test = getPast(test, n)

data = rbind(train[[1]], train[[2]], train[[3]]) #put the data frames together

# # # # # # # # # # # # # # # # # # # # BUILD MODEL AND GET PREDICTIONS # # # # # # # # # # # # # # # # # # # # 
model = lm(DES ~ st1 + st2, data = data) #builds the model using the deseasonalized value as the dependent variable. S(t-1) and S(t-2) as the independent variable.
test$DESpred = predict(model, test) #get the predctions for the deseasonalized value
for (i in 1:nrow(test)) 
{
  test$pred = test$DESpred + test$MEAN #sums the deseasonalized value with the mean value to get the real prediction
}

# # # # # # # # # # # # # # # # # # # # GENERATE PLOTS AND THE MEAN ABSOLUTE ERROR # # # # # # # # # # # # # # # # # # # # 
test = test[(test$HR >= 8 & test$HR <= 17),]
deshourlymean = test
deshourlymean$error = abs(deshourlymean$SOLR - deshourlymean$pred)
mean(deshourlymean$error, na.rm = TRUE)
hist(deshourlymean$error, main="Deseasonalized by mean", xlab="Error")
boxplot(error~HR,data=deshourlymean, main="Deseasonalized by mean", 
        xlab="Hour", ylab="Error", ylim=c(0, 900))

# # # # # # # # # # # # # # # # # # # # GENERATES CSV IN THE NEEDED FORMAT # # # # # # # # # # # # # # # # # # # # 
test = test[c("YEAR", "MON", "DAY", "HR", "pred")] #creates a new data frame in the needed format
colnames(test)[5] <- "SOLR" #change the name of the predictions column to SOLR
 #from 8am to 5pm
write.csv(test, file = paste(name, "-f", n, "-DES", ".csv", sep=""),row.names=FALSE) #generate the csv file