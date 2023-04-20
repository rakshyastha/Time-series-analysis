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
df <- read.csv('j17.csv',skip=0,header=TRUE)
head(df, 10)

# Defining start and end dates
start_date <- as.Date("1932-12-11")
end_date <- as.Date("2023-04-09")

# Generate sequence of daily dates 
dummydate <- seq(start_date, end_date, by = "day")
dates_df <- data.frame(date = dummydate)
#dates_df$date <- as.Date(dates_df$date, format = "%Y/%m/%d")

# Convert the date column to a date object
df$DailyHighDate <- as.Date(df$DailyHighDate, format = "%m/%d/%Y")

# Change the format of the date to "yyyy/mm/dd"
df$date  <- format(df$DailyHighDate , "%Y-%m-%d")

# Print the updated data frame
df

#check if there is any missing values
x <-length(dummydate)
Y <-length(df$date)
if(x>Y)print("missing values")

#finding column names
colnames(df)


# merge the data frame and the sequence of dates
merged_df <- merge(dates_df, df, by = "date", all = TRUE)
head(merged_df, 40)

#finding column names
colnames(merged_df)

# Remove the unwanted column
merged_df1 <- subset(merged_df, select = c("date", "WaterLevelElevation"))

#groupby 
#merged_df1_grouped = merged_df1.groupby("date")["WaterLevelElevation"].sum()
merged_df1_grouped <- merged_df1 %>%group_by(date) %>%summarize(WaterLevelElevation = sum(WaterLevelElevation, na.rm = TRUE))

merged_df1_grouped[merged_df1_grouped == 0] <- NA

#plotting time series data of well depth
plot(merged_df1_grouped$date, merged_df1_grouped$WaterLevelElevation, xlab = "Date", ylab = "Water Level Elevation",type = "l", col = "black")

#checking the number of NA's 
summary(merged_df1_grouped)


# Use na_kalman to impute the missing values

merged_df1_grouped$WaterLevelElevation <- na_kalman(merged_df1_grouped$WaterLevelElevation,  model = "StructTS", smooth = TRUE, maxgap = Inf)

#checking summary statistics
summary(merged_df1_grouped)

#Finding na values in date column
date_na <- which(is.na(merged_df1_grouped$date))

#replace the missing value with a date
merged_df1_grouped$date[date_na] <- as.Date("1970-01-01")

#decomposing
dec_1<- ts(merged_df1_grouped$WaterLevelElevation, frequency = 600)
dec_11 <- decompose(dec_1)
plot(dec_11)

#converting dataframe to csv
#write.csv(merged_df1_grouped, "imputeddaysj17.csv")

#plotting time series data of well depth without na
plot(merged_df1_grouped$date, merged_df1_grouped$WaterLevelElevation, xlab = "Date", ylab = "Water Level Elevation",type = "l", col = "black")

#finding if there is any na left
sum(is.na(merged_df1_grouped))

#performing 10-day moving average 
merged_df1_grouped$waterlevel10d <- rollmean(merged_df1_grouped$WaterLevelElevation,10,fill = NA,align='right')

#plotting time series data of well depth without na
plot(merged_df1_grouped$date, merged_df1_grouped$waterlevel10d, xlab = "Date", ylab = "waterlevel10davg",type = "l", col = "black")

#plotting acf
acf(merged_df1_grouped$waterlevel10d, main = "waterlevel10d", lag.max = 500, na.action = na.omit)

#plotting pacf
pacf(merged_df1_grouped$waterlevel10d, main = "waterlevel10d", lag.max = 500, na.action = na.omit)

#mankendall
MannKendall(merged_df1_grouped$WaterLevelElevation)






