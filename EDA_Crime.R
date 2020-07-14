library(ggplot2)
library(gridExtra)
library(dplyr)
library(plyr)
library(viridis)
library(corrplot)
library(ggmap)
library(maps)
#Read the downloaded file in R and assign it to EDA
EDA<-read.csv("/Users/tugga/Downloads/MCI_2014_to_2019.csv", header = TRUE)
#Drop a few variables to clean up the dataset
EDA<-subset(EDA, select = -c(1,3,4,6,7,9:14,22,23,27))
#Check the dimensions
dim(EDA)
#Check for missing values in the dataset and omit the missing values - in this case we decide to omit the missing values because only 1000 rows have missing instances out of the 206000 rows
sum(complete.cases(EDA))
#The dataframe had duplicates, so we remove the duplicated rows
EDA_uniq<-unique(EDA)
EDANEW<-na.omit(EDA_uniq)
#The dataset has to be filtered for values from 2014-2019 as that is the period used for the analysis
EDAfilter<-filter(EDANEW, occurrenceyear == 2014 | occurrenceyear == 2015 | occurrenceyear == 2016 | occurrenceyear == 2017 | occurrenceyear == 2018 | occurrenceyear == 2019)
#Convert respective variables within the dataset to factors
EDAfilter$occurrencemonth<-factor(EDAfilter$occurrencemonth, levels=c("January","February","March","April","May", "June", "July", "August", "September", "October", "November", "December"))
EDAfilter$occurrencedayofweek = gsub(" ", "", EDAfilter$occurrencedayofweek)
EDAfilter$occurrencedayofweek<-factor(EDAfilter$occurrencedayofweek, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
EDAfilter$Neighbourhood<-as.factor(EDAfilter$Neighbourhood)
EDAfilter$premisetype<-as.factor(EDAfilter$premisetype)
EDAfilter$MCI<-as.factor(EDAfilter$MCI)
EDAfilter$Division<-as.factor(EDAfilter$Division)
#Group crimes by year
by_date <- EDAfilter %>% group_by(occurrenceyear) %>% dplyr::summarise(Total = n())
#1.Boxplot to see the distribution of crimes per year
#In Toronto, the average number of crime incidents is 31179 per year, 2647 per month, and 86 per day. 
ggplot(by_date, aes(x = "",Total, fill = Total)) + geom_boxplot(fill="orange") + xlab("Year") + ylab("Crime Counts per Year")
#Group crimes by month
by_month <- EDAfilter %>% group_by(occurrencemonth,occurrenceyear) %>% dplyr::summarise(Total = n())
#2.Boxplot to see the distribution of crimes per month
ggplot(by_month, aes(x = "",Total, fill = Total)) + geom_boxplot(fill="blue") + xlab("Month") + ylab("Crime Counts per Month")
#Group crimes by day
by_day <- EDAfilter %>% group_by(occurrenceyear,occurrencedayofyear) %>% dplyr::summarise(Total = n())
#3.Boxplot to see the distribution of crimes per day
#The graph of each day has an abnormal max value of 201 incidents, which is suspected as an outlier.
ggplot(by_day, aes(x = "",Total, fill = Total)) + geom_boxplot(fill="green") + xlab("Day") + ylab("Crime Counts per Day")
#4.Trend crimes by year
#Overall, crime has seen an increasing trend since 2014.
ggplot(by_date, aes(occurrenceyear, Total, color = occurrenceyear)) + geom_line()
#5.Understand crime trends by month, day, hour, and day of the week and plot them side by side
# Crime incidents are most during summer and fall (May-October), with frequencies peaking on the first of every month.Crimes are also most around Fridays and weekends. 
mon.bp <- ggplot(EDAfilter, aes(x = EDAfilter$occurrencemonth, fill=as.factor(EDAfilter$occurrencemonth))) +
  geom_bar(width=0.8, stat="count") + theme(legend.position="none") + scale_x_discrete(labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  ggtitle("Crime Records by Month of Year") + labs(x="Month", y="Total")
sdom.bp <- ggplot(EDAfilter, aes(x = EDAfilter$occurrenceday, fill=as.factor(EDAfilter$occurrenceday))) +
  geom_bar(width=0.8, stat="count") + theme(legend.position="none") +
  ggtitle("Crime Records by Day of Month") + labs(x="Day", y="Total")
shour.bp <- ggplot(EDAfilter, aes(x = EDAfilter$occurrencehour, fill=as.factor(EDAfilter$occurrencehour))) +
  geom_bar(width=0.8, stat="count") + theme(legend.position="none") +
  ggtitle("Crime Records by Hr")+ labs(x="Hour", y="Total")
dow.bp <- ggplot(EDAfilter, aes(x = EDAfilter$occurrencedayofweek, fill=as.factor(EDAfilter$occurrencedayofweek))) + geom_bar(width=0.8, stat="count") + theme(legend.position="none") + scale_x_discrete(labels = c('M', 'T', 'W', 'T', 'F', 'S', 'S')) +
  ggtitle("Crime Records by Day of Week") + labs(x="Day of Week", y="Total")
grid.arrange(sdom.bp, shour.bp, mon.bp, dow.bp)
#6.Crimes by hour and weekday - trends
#Crime peaks at noon, tapers off and gradually picks up steam to continously progress through evening and late into night.
ggplot(EDAfilter)+
  aes(x=EDAfilter$occurrencehour, colour=EDAfilter$occurrencedayofweek)+
  geom_line(stat="count")+
  scale_x_continuous(breaks = seq(0,23,1),limit=c(0,23))+
  scale_y_continuous(breaks = seq(400,3000,250),limit=c(200,2500))+
  labs(title="Crimes by Hour and Weekday",x="Hour",y="Number of Incidents")
#7.Heatmap - Crimes by Month and Day
#The first and the last week of any month is seen to have the most incidents. The peak observed is on the first day of every month.
crime_heatmap <- EDAfilter %>% group_by(occurrencemonth, occurrenceday) %>% dplyr::summarise(Total = n())
ggplot(crime_heatmap, aes(occurrencemonth, occurrenceday, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Crimes by Month and Day(2014-2019)")
#8.Heatmap - Crimes by Day and Hour
# During weekdays, peak is observed at noon and then starts to gradually build from 3PM. During weekends, peak is observed at midnight.
hour_heatmap <- EDAfilter %>% group_by(occurrencedayofweek, occurrencehour) %>% dplyr::summarise(Total = n())
ggplot(hour_heatmap, aes(occurrencedayofweek, occurrencehour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Crimes by Day and Hour(2014-2019)")
#9.Crimes by premisetype
# Most number of crimes happen outside followed by apartments and commercial establishments.
premise <- ggplot(EDAfilter, aes(x = EDAfilter$premisetype, fill=as.factor(EDAfilter$premisetype))) +
  geom_bar(width=0.8, stat="count") + theme(legend.position="none") +
  ggtitle("Crime Records by Premise Type")
premise
#10.Major crime incident categories
#Assault is the most prevalent form of crime in Toronto followed by home/commercial break and enter and auto theft.
mci<-EDAfilter %>% group_by(MCI) %>% dplyr::summarise(Total = n())
ggplot(mci, aes(reorder(MCI, Total), Total, fill = MCI)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + ggtitle("Crimes by Categories") + theme(legend.position="none") + 
  xlab("Crime Category") +  
  ylab("Total Crimes")
#11.MCI trends by year
# All crime types have seen an increasing trend since 2014 with the exception of robbery
mciyear<-EDAfilter %>% group_by(MCI,occurrenceyear) %>% dplyr::summarise(Total = n())
ggplot(mciyear, aes(occurrenceyear, Total, color = MCI)) + geom_line()
#12.MCI Heatmap by Year
ggplot(mciyear, aes(MCI, occurrenceyear, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("MCI by Year(2014-2019)")
#13.MCI Heatmap by Month
#Most assault incidents happen between May and August,autotheft between July and November while other types are fairly consistent across all months.
mcimonth<-EDAfilter %>% group_by(MCI,occurrencemonth) %>% dplyr::summarise(Total = n())
ggplot(mcimonth, aes(MCI, occurrencemonth, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("MCI by Month(2014-2019)")
#14.Trend MCI by Month
ggplot(mcimonth, aes(as.integer(occurrencemonth), Total, color = MCI)) + scale_x_continuous(name = " ", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) + geom_line() + labs(title="MCI Trends by Month",x="Month",y="Total")
#15.MCI Heatmap by Day of Week
#Assault peak is observed usually on weekends, while other types peak on Fridays.
mciweek <- EDAfilter %>% group_by(MCI, occurrencedayofweek) %>% dplyr::summarise(Total = n())
ggplot(mciweek, aes(MCI, occurrencedayofweek, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("MCI by Weekday(2014-2019)")
#16.MCI Heatmap by Day of Month
mciday <- EDAfilter %>% group_by(MCI, occurrenceday) %>% dplyr::summarise(Total = n())
ggplot(mciday, aes(MCI, occurrenceday, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("MCI by Day(2014-2019)")
#17.MCI Heatmap by Hour
# Assault, Break and Enter, Theftover peaks are observed at both midnight and noon, autotheft at 10PM, robbery at 9PM.
mcihour <- EDAfilter %>% group_by(MCI, occurrencehour) %>% dplyr::summarise(Total = n())
ggplot(mcihour, aes(MCI, occurrencehour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("MCI by Hour(2014-2019)")
#18.Crime Category by Premise Type
#Auto theft happens mostly outside and at houses while other crime types are common across all premise types
premci<-EDAfilter %>% group_by(MCI,premisetype) %>% dplyr::summarise(Total = n())
ggplot(premci, aes(reorder(premisetype,Total),Total,fill=MCI))+
  geom_bar(stat="identity")+
  coord_flip()+
  xlab("PremiseType") +
  ylab("Crime Count")+
  theme_minimal()
#19.Top offences
#Top offences within the assault category include - assault with weapon, bodily harm, and assault peace officer while top offences within robbery include - mugging, other, robbery with weapons, and robbery-business.
type<-EDAfilter %>% group_by(offence) %>% dplyr::summarise(Total = n())
ggplot(type, aes(reorder(offence, Total), Total)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = Total), stat = 'identity', data = type, hjust = -0.1, size = 2) +
  scale_y_continuous(breaks = seq(0,80000,10000)) + 
  coord_flip() + ggtitle("Crimes by Top Offences") + 
  xlab("Crime Offence Type") + 
  ylab("Total Crimes")
#20.Top Neighbourhoods for Crime in Toronto
#The most dangerous neighbourhood is the Waterfront. The other two most dangerous hoods are the Bay Street Corridor and Yonge-Church Corridor. 
df<-EDAfilter
df$Neighbourhood<-as.factor(df$Neighbourhood)
dfhood<-df %>% group_by(Neighbourhood) %>% dplyr::summarise(Total = n()) %>% dplyr::top_n(20) %>% dplyr::arrange(desc(Total))
ggplot(dfhood,aes(reorder(Neighbourhood,Total), Total))+
  geom_col(fill = "navy")+
  coord_flip()+
  ggtitle("Top 20 Neighbourhoods for Crime") +
  labs(x = NULL, y = NULL)
#21.Safe Neighbourhoods in Toronto
# The most safest hoods are Lambton Baby Point, Woodbine-Lumsden, and Guildwood
dfsafe<-df %>% group_by(Neighbourhood) %>% dplyr::summarise(Total = n()) %>% dplyr::top_n(-20) %>% dplyr::arrange(desc(Total))
ggplot(dfsafe,aes(reorder(Neighbourhood,Total), Total))+
  geom_col(fill = "navy")+
  coord_flip()+
  ggtitle("Top 20 Safe Neighbourhoods in Toronto")+
  labs(x = NULL, y = NULL)
#22.Top 20 Neighbourhoods by Crime Categories - shows the top neighbourhoods for assault, auto theft, and so on
#Besides assaults, Bay Street Corridor, Church-Yonge Corridor and Waterfront had the most break and enter crimes while West Humber-Clairville had the most vehicle stolen crimes
dfmci<-df %>% group_by(MCI,Neighbourhood) %>% dplyr::summarise(Total = n()) %>% dplyr::top_n(20) %>% dplyr::arrange(desc(Total))
ggplot(dfmci, aes(reorder(Neighbourhood,Total),Total,fill=MCI))+
  geom_bar(stat="identity")+
  coord_flip()+
  xlab("Neighbourhood") +
  ylab("Crime Count")+
  theme_minimal()
#23.Map of Toronto's crimes - simple visualization
#Assaults, and Break and Enter occur all over the city, with a concentration in the Waterfront areas. Other crimes, such as Auto Theft has more points on the west side than the east side. Robbery and Theft Over are primarily in the Waterfront areas.
lat <- df$Lat
lon <- df$Long
crimes <- df$MCI
to_map <- data.frame(crimes, lat, lon)
colnames(to_map) <- c('crimes', 'lat', 'lon')
sbbox <- make_bbox(lon = df$Long, lat = df$Lat, f = 0.01)
my_map <- get_map(location = sbbox, maptype = "roadmap", scale = 2, color="bw", zoom = 10)
ggmap(my_map) +
  geom_point(data=to_map, aes(x = lon, y = lat, color = "#27AE60"),
             size = 0.5, alpha = 0.03) +
  xlab('Longitude') +
  ylab('Latitude') +
  ggtitle('Location of Major Crime Indicators Toronto 2014-2019') +
  guides(color=FALSE)
ggmap(my_map) +
  geom_point(data=to_map, aes(x = lon, y = lat, color = "#27AE60"),
             size = 0.5, alpha = 0.05) +
  xlab('Longitude') +
  ylab('Latitude') +
  ggtitle('Location of Major Crime Indicators Toronto 2014-2019') +
  guides(color=FALSE) +
  facet_wrap(~ crimes, nrow = 2)

