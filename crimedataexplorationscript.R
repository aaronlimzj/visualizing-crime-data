#Data exploration with Philadelphia crime dataset
#https://www.kaggle.com/mchirico/philadelphiacrimedata
# Take random sample (crimesmall) of 5000 observations from data set of 2 Mn+ obs

#prepare environment
setwd("C:\\Users\\aaronlimzj\\Desktop\\R")

#install and load relevant packages

list.of.packages <- c("ggplot2", "RColorBrewer", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = T)

#cleanup
rm(list = c("list.of.packages", "new.packages"))

#load dataset
crime <- read.csv(".//crimesmall.csv")

#clean dates
crime$Dispatch_Date_Time  <- strptime(crime$Dispatch_Date_Time, format = "%Y-%m-%d %H:%M:%S")
crime$Dispatch_Date  <- strptime(crime$Dispatch_Date, format = "%Y-%m-%d")
crime$Dispatch_Time  <- strptime(crime$Dispatch_Time, format = "%H:%M:%S")

#extract year
crime$Year  <- crime$Dispatch_Date$year+1900

#plot trend line across year months
yearMonthCounts  <- as.data.frame(table(crime$Month))
ggplot(yearMonthCounts, aes(x=Var1, y = Freq)) + geom_line(group = 1, color = "red") + xlab("Year-Month") + ylab("Number of dispatches") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Monthly crime over time") + scale_x_discrete(breaks = yearMonthCounts$Var1[seq(1, length(yearMonthCounts$Var1), 12)])

#there appears to be a seasonal trend, let's visualize it more clearly

#extract year and month in yearMonthCount
yearMonthCounts$Year  <- factor(substr(yearMonthCounts$Var1,1,4), ordered = T)
yearMonthCounts$Month  <- factor(substr(yearMonthCounts$Var1,6,7), ordered = T)

#plot freq vs months across years
ggplot(yearMonthCounts, aes(x= Month, y = Freq)) + geom_line(aes(group = Year, color = Year), size = 1) + scale_color_manual(values = brewer.pal(11, "PiYG")) + ylab("Number of dispatches")

#plot heatmap (probably a better visualization)
ggplot(yearMonthCounts, aes(x = Month, y = Year))+geom_tile(aes(fill = Freq)) + scale_fill_gradient(name ="Monthly Dispatches", low = "white",high = "red")

#nope, no clear seasonal trend. but it clearly shows that crime has been decreasing across the years

#add count for easy transformation with reshape
crime$Count  <- 1




