getwd()
setwd("C:/Users/Sourabh Reddy/Desktop/USF/DM")
getwd()
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(na.tools)
library(sqldf)


################################## Weather Data ######################################
weather <- read.csv("weather.csv")
head(weather)

weather$wtime = as.POSIXct(weather$time_stamp, origin="1970-01-01", tz = "America/New_York")

weather$wdate = date(weather$wtime)
weather$whour = hour(weather$wtime)
weather$wminute = minute(weather$wtime)
weather$wseconds = second(weather$wtime)
weather$wday = weekdays(weather$wtime)

#replace missing values in rain with median with rolling window of size 5

weather$rain = na.median(weather$rain, k = 5 , weighting = "simple")
################################## Cab Data ######################################
data =read.csv("cab_rides.csv")
data$time <- as.POSIXct(data$NEWTIME,origin="1970-01-01",tz = "America/New_York")
#deletemissingvalues
clean_data = na.omit(data)
clean_data$date = date(clean_data$time)
clean_data$hour = hour(clean_data$time)
clean_data$minute = minute(clean_data$time)
clean_data$seconds = second(clean_data$time)
clean_data$day = weekdays(clean_data$time)

######################### adding merge column to both the datasets##################
weather$merge = paste(weather$location, weather$wdate, weather$whour)
clean_data$merge = paste(clean_data$destination, clean_data$date, clean_data$hour)

colnames(weather)=tolower(make.names(colnames(weather)))
attach(weather)
weather$temp = weather$ï..temp

########################### selecting appropriate columns from weather dataset##############3
weatherDF <- sqldf("select AVG(temp) as temp_avg,AVG(clouds) as clouds_avg,AVG(pressure)
                   as pressure_avg,AVG(rain) as rain_avg,AVG(humidity) as humidity_avg,
                   AVG(wind) as wind_avg,merge from weather group by merge")

########################## merging the cab rides and weather data sets################
mergeDF = merge(clean_data, weatherDF, by ="merge")


########################### Removing unnecessary columns ############################
merge_DF = select(mergeDF,-c('merge','time_stamp', 'id', 'product_id', 'NEWTIME'))

#write.csv(merge_DF,"mergedDataFrame.csv")

######################### Sampling the data for plots and visualization #############

First_DF <- merge_DF[sample(nrow(merge_DF), 6000), ]

#day vs surgemultiplier on cab type
ggplot(First_DF, aes(x = day, y = surge_multiplier, color = cab_type))+
  geom_point()

#price vs rain_avg on cab type
ggplot(First_DF, aes(x = price  , y = rain_avg, color = cab_type))+
  geom_point()

#distance vs surgemultiplier on cab type
ggplot(First_DF, aes(x = distance  , y = surge_multiplier, color = cab_type))+
  geom_point()
#temp_avg vs price on cab name
ggplot(First_DF, aes(x = temp_avg  , y = price, color = cab_type))+
  geom_point()
#distance vs price on cab name
ggplot(First_DF, aes(x = distance  , y = price, color = name))+
  geom_point()
#distance vs price on cab type
ggplot(First_DF, aes(x = distance  , y = price, color = cab_type))+
  geom_point()
#day vs surge multiplier on whole dataset
ggplot(merge_DF, aes(x = day, y = surge_multiplier, color = cab_type))+
  geom_point()
#26th nov time vs price on cab type
DistanceDF = subset(merge_DF, date < '2018-11-27' & date>'2018-11-25')
sampleDistDF = DistanceDF[sample(nrow(DistanceDF), 500), ]
#surge_multiplier vs temperature on cab type

sampleDistDF %>% ggplot(aes(x=time,y=price,color=cab_type)) +
  geom_point() +  
  scale_x_datetime(labels = function(x) format(x,format = "%H:%M"),breaks=date_breaks("120 min")) +
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5)) +
  ggtitle("26th Nov")

###############################################################################################
#################################Boruta Feature Selection Technique############################
###############################################################################################
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

set.seed(111)
boruta_output <- Boruta(price ~ ., data=na.omit(MergeDF), doTrace=2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)
print(boruta_output)
plot(boruta_output, cex.axis=.5, las=2, xlab="",ylim = c(-5,50), main="Variable Importance")  # plot variable importance

plotImpHistory(boruta_output)

tent <- TentativeRoughFix(boruta_output)

attStats(boruta_output)