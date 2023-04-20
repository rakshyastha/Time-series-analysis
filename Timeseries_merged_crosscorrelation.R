library(dplyr)
library(imputeTS)
library(xts)
library(forecast)
library(zoo)
library(Kendall)


#set working directory
path<-'C:/Users/14098/OneDrive - Lamar University/Desktop/Machine Learning/Timeseries'
setwd(path)
getwd()

#read the csv file and create dataframe
df1 <- read.csv('imputeddaycomal.csv',skip=0,header=TRUE)
head(df1, 10)

# telling that the column is date column 
df1$date <- as.Date(df1$date, format = "%Y-%m-%d")

#read the csv file and create dataframe
df2 <- read.csv('imputeddaysanmarcos.csv',skip=0,header=TRUE)
head(df2, 10)

# telling that the column is date column 
df2$date <- as.Date(df2$date, format = "%Y-%m-%d")

#read the csv file and create dataframe
df3 <- read.csv('imputeddaysj17.csv',skip=0,header=TRUE)
head(df3, 10)

# telling that the column is date column 
df3$date <- as.Date(df3$date, format = "%Y-%m-%d")

# merge the data frame and the sequence of dates
merged_df1 <- merge(df1,df2, by = "date", all = TRUE)
head(merged_df1, 10)

# merge the data frame and the sequence of dates
merged_df2 <- merge(merged_df1,df3, by = "date", all = TRUE)
head(merged_df2, 10)

#finding column names of df
colnames(merged_df2)

#cross-correlation between j17 & coral
Ccf(merged_df2$WaterLevelElevation,merged_df2$Discharge_cfs.x, main = "CrosscorrelationJ17_coral", lag.max = 100)

#cross-correlation between j17 & sanmarcos
Ccf(merged_df2$WaterLevelElevation,merged_df2$Discharge_cfs.y, main = "CrosscorrelationJ17_sanmarcos", lag.max = 100)

#cross-correlation between coral & sanmarcos
Ccf(merged_df2$Discharge_cfs.x,merged_df2$Discharge_cfs.y, main = "Crosscorrelationcoral_sanmarcos", lag.max = 100)


