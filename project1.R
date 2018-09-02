
#For this project, I worked on a computer in the Economics Building at Penn State. 
#When submitting this project, the main parent directory is the folder called 'Project1'
#and the three sub directories are called Code, Data (with Raw and Intermediate folders), and Output.

#Parent Directory: Project1
#Subdirectories: Code, Data, Output
#Folders in Subdirectory 'Data': Raw and Intermediate

# rm(list=ls())


#package's that were installed:
#install.packages("BatchGetSymbols")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("xlsx")


#These are the libraries and packages that I am using throughout this test. 
library(BatchGetSymbols)
library(plyr)
library(dplyr)
library(tidyverse)
library(xlsx)


#1.

#Reading in the csv files from different directories was difficult for me. I was able to load the data in RStudio by 
#manually importing the data. The command below used the directory specific to the computer I was working with. 
indexsub = read.csv("C:/Users/EconGuest/Dropbox/Chicago Booth RA/Test/fama-miller-exam/index_subsample.csv")
#vector of unique tickers
tics = indexsub$co_tic 

#With the function 'BatchGetSymbols' I am downloading data with the vector of tickers from the 'index_subsample.csv' file. 
#This data is from Yahoo! Finance. 
mydata = BatchGetSymbols(tics, first.date = as.Date('1970-01-01'), last.date = as.Date('2017-01-01'), bench.ticker = "^GSPC",type.return = "arit") #the argument type.return gives the log returns of the asset
securityfile = mydata$df.tickers



#2. 

#Manually importing 'dates.csv' to RStudio. Please refer to Parent Directory where the data is stored to
#access this file. This data is located in the Data subdirectory and under the 'Raw' folder. The command below 
#is specific to the computer I was working on. 


dates <- read.csv("C:/Users/EconGuest/Dropbox/Chicago Booth RA/Exam/fama-miller-exam/dates.csv", sep="")

# In this step, I am converting the formatted SAS dates into R dates with a For loop. 
Rdates = as.data.frame(matrix(NA, nrow = nrow(dates), ncol = 1))
N = nrow(dates)
for (i in 1:N){
  Rdates[i]= as.Date(as.integer(dates$date[i]), origin="1960-01-01")
} 
dates$date[1]
Rdates = unique.data.frame(Rdates)


#I am making a formatted R dates vector
Rdates = t(Rdates) #transpose
Rdates = as.data.frame(Rdates, row.names = 1:nrow(Rdates))
colnames(Rdates) = "Dates"



returns = as.data.frame(matrix(NA,nrow = nrow(Rdates), ncol=2))
returns$Dates = Rdates$Dates
returns$average_returns = matrix(nrow = nrow(returns), ncol = 1)
cancel = c(1,2)
returns = returns[,-cancel]
returns

#I use securityfile data.frame from question 1 to calculate mean average returns for all stocks
T = nrow(returns)

for (i in 1:T){
  indexes = which(returns$Dates[i] == securityfile$ref.date)
  subset = securityfile[indexes,]
  returns$average_returns[i] = mean(subset$ret.adjusted.prices)
}

#There were several NaN's that were calculated with the for loop
finalreturns = na.omit(returns)
finalreturns
#Output
write.csv(finalreturns, file = "equal-weight-returns.csv")

#3.

#I am cleaning 'securifyfile' to create a column for year and month. I thought this would be an easier way to work.  
securityfile1 = securityfile
securityfile1$date = as.character(securityfile$ref.date)
securityfile1$date=strsplit(securityfile$ref.date, split = " ")
securityfile1$year = substring(securityfile1$date, 1, 4)
securityfile1$month = substring(securityfile1$date, 6,7)

#I am aggregating the daily dataset and grouping and my best guest aggregating daily to monthly return prices. These
#commands are done through the 'dplyr' library. 
monthlyData <- securityfile1 %>%
  group_by(year, month,ticker) %>%
  summarise(mean(ret.adjusted.prices)) 

#Aggregating monthly data of specified stocks and cleaned out NaN's that were calculated
monthlyData = na.omit(monthlyData)
monthlyData$month = as.numeric(monthlyData$month)

#b. 

#I separated the raw 'F-F_Research_Data_Factors.CSV" file into 2 intermediate excel files. FF contains the monthly data. 
#I manually imported these data files to Rstudio. These intermediate files are located in the the subdirectory of "Project1"
#under "Data" and then under the "Intermediate" folder. 
#Please change the working directory to have to the intermediate files attached in order to upload the data. 
#My commands below are specific to the computer I was working with. 

FF <- read_excel("C:/Users/EconGuest/Dropbox/Chicago Booth RA/fama-miller-exam/FF.xlsx")
FF2 <- read_excel("C:/Users/EconGuest/Dropbox/Chicago Booth RA/fama-miller-exam/Annual_Factors_Jan_Dec.xlsx")



#cleaning FF: extracting years and months into separate columns
class(FF$X__1)
FF$X__1 = as.character(FF$X__1)
FF$year = substring(FF$X__1, 1,4)
FF$year = as.numeric(FF$year)
FF$month = substring(FF$X__1,5,6)
FF$month = as.numeric(FF$month)
FF = FF[,-1]

#Merged datasets of monthly data and FF
rates = merge(x = monthlyData, y = FF, by = c("year", "month"), all.x = TRUE)

#Separate dataframe that only has data for the month of January. 
january = rates[rates$month == 1, ]
colnames(january)[4] = "returns"
colnames(january)[5] = "freeRate"
ticks = unique(rates$ticker)
ticks = as.data.frame(ticks)
ticks = as.data.frame(ticks)


#I think this was the most challenging part of the problems. This for loop was going to be used to make 5 individual vectors of risk-free market rates 
#for the past 5 years. The first loop runs through the unique ticker vector so that it goes through each
#stock and the second loop makes the individal vectors from T-1 to T-5. 'subs' is used as a subset of
#January that had all of the data from a particular stock. 

#I belive it stopped working when it looped through a ticker called 'CL' which was the 28 element in the 'tick' vector. Given my time constraints, 
#I had the for loop run through the first 27 unique stock tickers and make vectors of lagged observations of the risk free market rate. 
#In every iteration of the loop, I ask my program to print the resulting sub data frame to show examples of what I was trying to do. 
#I'd like to explain my though process for the remaining exercies. 



for (i in 1:27){
  subs =  january[january$ticker == ticks[i,],]
  for(j in 2:nrow(subs)){
   subs$yearminusone[j] = subs$freeRate[j-1]
   subs$yearminustwo[j] = subs$yearminusone[j-1]
   subs$yearminusthree[j] = subs$yearminustwo[j-1]
   subs$yearminusfour[j] = subs$yearminusthree[j-1]
   subs$yearmiusfive[j] = subs$yearminusfour[j-1]
   print(subs)
  }
 }


#The problem I encountered above was trying to eliminate the NA's that would appear in the new vectors. This prevented running the regression
#on the returns on the the risk free market rates. 



# In order to calculate the market B's I would have run this regression on the data and save the coefficient estimates. 
# B = lm(subs$returns~subs$yearminusone+subs$yearminustwo+subs$yearminusthree+subs$yearminusfour+subs$yearmiusfive)
# To save the coefficients I call the function of the linear model 'B'. To access the coefficients I would 
# write B$coefficients[2:6] and save it to a data frame and merge it with the January dataframe. Merging would match
# ticker names together in this process. 

#c.  

#The absolute value function is trivial within R. 
# To calculate the equal-weight reutns for each portfolio I would have run a for loop through the January just as I did in problem 2 with the
# equal-weight returns portion. 


#d. 

#I would have run another for loop to find the minimum and the maxiumum of the absolute values of the market B's and difference them and store them
#into a new vector within the January data frame. 

#e. 

#Finally I would have outputted the final January data frame as "write.csv(January, "portfolio-returns.csv")"
