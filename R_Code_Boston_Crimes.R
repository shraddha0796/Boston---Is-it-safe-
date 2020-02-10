#package installations and library read
install.packages("ggplot2")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("forecast")
install.packages("wordcloud")
install.packages("tidyverse")
install.packages("VIM")
install.packages("leaflet")
install.packages("plotly")
install.packages("lubridate")
install.packages("magrittr")
install.packages("wordcloud")

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(forecast)
library(wordcloud)
library(tidyverse)
library(VIM)
library(leaflet)
library(plotly)
library(lubridate)
library(magrittr)
library(wordcloud)



#initial steps- data reading and viewing
pdata<-read.csv(file.choose(), header = T) 
head(pdata)
pdata[][is.na(pdata[])] <- 0


pal = brewer.pal(9,"Blues")

crimes <- pdata#creating another variable for the crime data
names(crimes)
#Creating nice looking color palettes especially for thematic maps
pal = brewer.pal(9,"Blues")
#building a contingency table of the counts at each combination of factor levels
street_name <- as.tibble(table(Street_Name= pdata$STREET))
#naming the columns 
colnames(street_name) <- c("Street_Name", "Count")
#Integer vector, containing the random number generator 
set.seed(21)
#Plotiing the word Cloud, Which has minimum frequency 50, Range of font size  
wordcloud(street_name$Street_Name, street_name$Count, min.freq = 50, random.order = F, random.color = T, colors =c("black", "cornflowerblue", "darkred"), scale = c(2.4,.7))


#checking purity of data
aggr(pdata)

#checking uniqueness of variables in data
d<-unique(pdata$OFFENSE_CODE_GROUP)
d
o<-unique(pdata$REPORTING_AREA)
o
head(d,10)
head(o,10)
e<-unique(pdata$DISTRICT)
e
f<-unique(pdata$OFFENSE_CODE)
f
head(e,10)
head(f,10)

View(pdata) #view the data

#PERFORMING NECESSARY DATA VISUALIZATIONS AND EXPLORATORY DATA ANALYSIS (EDA)

#visualizations and EDA where required
#displaying the map of boston through leaflet package
leaflet() %>%
  setView(lng=-71.0892, lat=42.3398, zoom = 10) %>%  #using setView method to set the view of map (center and zoom level)
  addTiles() %>%
  addMarkers(lng=-71.0892, lat=42.3398, popup="Boston") 

#displaying the map of boston distributed among crime district by latitude and longitude
#crime mapping 
qplot(Long, Lat, data= pdata, color=DISTRICT, geom='point', xlim = c(-71.2,-70.95), ylim= c(42.22,42.4))+
  theme_bw(base_size=15)+
  geom_point(size = 2)



#number of crimes count per year
ggplot(pdata, aes(x=YEAR, fill= as.factor(YEAR))) +
  labs(x="year", y="counts", title = "number of crimes per year")+
  geom_bar()

#number of crimes count per month
ggplot(pdata, aes(x=MONTH, fill= as.factor(MONTH))) +
  labs(x="months", y="counts", title = "number of crimes per month")+
  geom_bar()

#number of crimes count per day of the week
ggplot(pdata, aes(x=DAY_OF_WEEK, fill= as.factor(DAY_OF_WEEK))) +
  labs(x="days of the week", y="counts", title = "number of crimes every week")+
  geom_bar()

#dividing the shift into 4 groups and generating six points of the day to bin the day into four equal segments
time_diff<- c("0","6","12","18","24") #breaking day into 6 interval period

table(pdata$time_diff) #displaying the crime counts as per hour shift



#plotting the crimes according to the timings
plot_shift = plot_ly(pdata, x= ~ time_diff, color = ~ time_diff) %>%
  add_histogram() %>%
  layout(
    title = "Total crimes as per hours shift",
    xaxis = list(title = "4 timings shifts",
                 yaxis = list(title = "Count"))
  )
plot_shift #displaying the crimes counts as per hours shift  




#counting street crimes
street_crime<- sort(table(pdata$STREET), decreasing = TRUE)
head(street_crime, 10)

#displaying the summary of the dataset
summary(pdata)

#renaming the month names
pdata$MONTH <- as.character(pdata$MONTH)
pdata$MONTH[pdata$MONTH =='1'] <- 'Jan'
pdata$MONTH[pdata$MONTH =='2'] <- 'Feb'
pdata$MONTH[pdata$MONTH =='3'] <- 'Mar'
pdata$MONTH[pdata$MONTH =='4'] <- 'Apr'
pdata$MONTH[pdata$MONTH =='5'] <- 'May'
pdata$MONTH[pdata$MONTH =='6'] <- 'Jun'
pdata$MONTH[pdata$MONTH =='7'] <- 'Jul'
pdata$MONTH[pdata$MONTH =='8'] <- 'Aug'
pdata$MONTH[pdata$MONTH =='9'] <- 'Sep'
pdata$MONTH[pdata$MONTH =='10'] <- 'Oct'
pdata$MONTH[pdata$MONTH =='11'] <- 'Nov'
pdata$MONTH[pdata$MONTH =='12'] <- 'Dec'
pdata$MONTH

l<-sort(table(pdata$STREET),decreasing = TRUE)[1:10] # displaying top 10 streets 
l #displaying the streets

m<-sort(table(pdata$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11] # displaying top 10 offense code group 
m #displaying the offense code group

n<-sort(table(pdata$REPORTING_AREA),decreasing=TRUE)[1:10] # displaying top 10 reporting areas
n #displying the reporting areas


headdata<-head(pdata,10)

headdata$newcolumn<-c(25773,22996,18363,17494,15247,15217,14807,13316,11037,10867) #values of highest crimes counts
headdata$newcolumn1<-c("Larceny", "Medical Assistance", "Investigate Person", "Other", "Simple Assault","Vandalism","Drug Violation", "Verbal Disputes", "Towed", "Investigate Property")

#displying top 10 offense count
ggplot(headdata, aes(x=newcolumn1, y=newcolumn, fill= newcolumn1))+
  geom_bar(stat = "identity") + 
  coord_flip()+ #flipping the cartesian coordinates
  labs(y = "Type of offense", x = "Count",title ="top 10 offense count")

headdata$newcolumn2<-c(14237,7156,7131,5146,4783,4528,4511,4386, 3899,3501 ) #values of crimes counts in streets 
headdata$newcolumn3<-c("WASHINGTON ST","BLUE HILL AVE","BOYLSTON ST","DORCHESTER AVE","TREMONT ST", "MASSACHUSETTS AVE", "HARRISON AVE","CENTRE ST","COMMONWEALTH AVE","HYDE PARK AVE")

#displaying crime counts of top areas involved 
ggplot(headdata,aes(x=newcolumn3,y=newcolumn2, fill=newcolumn3))+
  geom_bar(stat="identity")+
  coord_flip()+  #flipping the cartesian coordinates
  labs(y="count", x="area name",title="top areas involved")

#considering only 2016 data and showing highest offense codes reported
graph1<-filter(pdata,YEAR==2016) #filtering 2016 year offense codes
table(graph1$OFFENSE_CODE_GROUP) #displaying counts of all offense codes
ocg1<-sort(table(graph1$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11] #taking top 10 offense code group

headdata$newcolumn4<-c(7903,6978,5765,5538,5284,5063,4744,4100,3521,3360) 
headdata$newcolumn5<-c("larceny","medical assistance","Investigate Person","Other","Drug Violation", "Vandalism", "simple assault","verbal disputes","motor vehicle larency","investigate property")

#generating the bar chart of 2016 highest reported areas
ggplot(headdata,aes(x=newcolumn5,y=newcolumn4, fill=newcolumn5))+
  geom_bar(stat="identity")+
  coord_flip()+ #flipping the cartesian coordinates
  labs(y="area name", x="count",title="2016 highest reported areas")

sum(headdata$newcolumn4) #displaying the total sum of top 10 2016 offense codes

#generating the bar chart of 2017 highest reported areas

graph2<-filter(pdata,YEAR==2017) #filtering 2017 year offense codes 
table(graph2$OFFENSE_CODE_GROUP)
ocg2<-sort(table(graph2$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11] #taking top 10 offense code groups for 2017 year

headdata$newcolumn6<-c(7817,7812,6659,5323,4898,4838,4761,4437,3973,3951)
headdata$newcolumn7<-c("medical assistance","larceny","Investigate Person","Other","simple assault", "Vandalism", "drug violation","verbal disputes","investigate property","towed")
ggplot(headdata,aes(x=newcolumn7,y=newcolumn6, fill=newcolumn7))+
  geom_bar(stat="identity")+
  coord_flip()+ #flipping the cartesian coordinates
  labs(y="area name", x="count",title="2017 highest reported areas")

sum(headdata$newcolumn6) #displaying the total sum of top 10 2017 offense codes

#2018
graph3<-filter(pdata,YEAR==2018) #filtering 2018 year offense codes 
table(graph3$OFFENSE_CODE_GROUP)

ocg3<-sort(table(bbb$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11]#taking top 10 offense code groups for 2018 year

headdata$newcolumn8<-c(6292,5949,4372,4241,4013,3692,3433,3208,2847,2899)
headdata$newcolumn9<-c("medical assistance","larceny","Other","Investigate Person","simple assault","Drug Violation","verbal disputes", "Vandalism","investigate property","towed")

ggplot(headdata,aes(x=newcolumn9,y=newcolumn8, fill=newcolumn9))+
  geom_bar(stat="identity")+
  coord_flip()+ #flipping the cartesian coordinates
  labs(y="area name", x="frequency",title="2018 highest reported areas")


sum(headdata$newcolumn8) #displaying the total sum of top 10 2018 offense codes

#as we see larceny is occuring most often in all the years, we further dig deeper to see what kind of larceny happens the most for all the 3 years
ldff<-filter(pdata,OFFENSE_CODE_GROUP=='Larceny') #filtering the larcency offense group

#displaying the count of larceny offense description
ggplot(ldff, aes(x=OFFENSE_DESCRIPTION),fill=as.factor(OFFENSE_DESCRIPTION)) +
  coord_flip()+  #flipping the coordinates
  geom_bar()

#HYPOTHESIS TESTING PAIRED SAMPLE T-TEST

set.seed(7) #to set the sample selection
pdata.sample <- sample_n(pdata,30, replace = TRUE)#select random 30 samples

monday<-subset(pdata.sample,subset = DAY_OF_WEEK=="Monday")#subset table when day is monday
tuesday<-subset(pdata.sample,subset = DAY_OF_WEEK=="Tuesday")#subset table when day is tuesday
wednesday<-subset(pdata.sample,subset = DAY_OF_WEEK=="Wednesday")#subset table when day is wednesday
thursday<-subset(pdata.sample,subset = DAY_OF_WEEK=="Thursday")#subset table when day is thursday
friday<-subset(pdata.sample,subset = DAY_OF_WEEK=="Friday")#subset table when day is friday
saturday<-subset(pdata.sample,subset = DAY_OF_WEEK=="Saturday")#subset table when day is saturday
sunday<-subset(pdata.sample,subset = DAY_OF_WEEK=="Sunday")#subset table when day is sunday

monday_count <- count(monday)#count of the mondays
tuesday_count <- count(tuesday)#count of the tuesdays
wednesday_count <- count(wednesday)#count of the wednesdays
thursday_count <- count(thursday)#count of the thursdays
friday_count <- count(friday)#count of the fridays
saturday_count <- count(saturday)#count of the saturdays
sunday_count <- count(sunday)#count of the sundays

weekday.counts <- c(monday_count,tuesday_count,wednesday_count,thursday_count) #net weekdays values
weekend.counts <- c(friday_count, saturday_count, sunday_count) #net weekdends values

weekday.counts <- as.numeric(as.character(weekday.counts)) #weekdays numeric
weekend.counts <- as.numeric(as.character(weekend.counts)) #weekends numeric


test.paired <- t.test(weekday.counts, weekend.counts, mu=0, alternative = "two.sided", paired = F, conf.level = 0.99) #confidence level 99%
test.paired # t - test

#2 sample T-test conclusion

#Ho:  weekday crime = weekend crime
#Ha:  weekday crime != weekend crime
#p-value = 0.6131 > 0.05
#do not reject null hypothesis
#weekday crime = weekend crime with 99% confidence

#TIME SERIES ANALYSIS

crimes <- pdata#creating another variable for the crime data
names(crimes)

# Finding the class of column OCCURED_ON_DATE 
class(crimes$OCCURRED_ON_DATE)

# As the class of column OCCURED_ON_DATE is factor we are converting it into date formart (year-month-day)
crimes$OCCURRED_ON_DATE <- as.Date(crimes$OCCURRED_ON_DATE, format="%Y-%m-%d")

# The colum has both dates and times so now we are dividing it into only dates
dates<-cut(crimes$OCCURRED_ON_DATE, 'day')

# Now we are having the counts of each date which represents the number of times 
tab.dates<- table(dates)

#converting it into data frame with its frequency 
crimes.dates<-data.frame(Date=format(as.Date(names(tab.dates)), '%d/%m/%Y'),
                         Frequency=as.vector(tab.dates))
# Having a look at the data frame 
head(crimes.dates)

#converting it into the time series, starting at the year 2015-June and 167th day of the year with frequency 365 days
crimes.ts<- ts((crimes.dates$Frequency),start=c(2015,6,167),frequency =365 )

#having a look at the time series 
head(crimes.ts)

#ploting the time series graph 
plot(crimes.ts,main="Time Series of Crimes from 2015-2018")

# Decomposing the time series into 3 other components trend,seasonal & random. To find how the trend shifts 
plot(decompose(crimes.ts))

# Using auto arima to find the best order for the arima model with lowest aic value
# USing Tracr=TRUE reports the list of Arima models considered 
mymodel<- auto.arima(crimes.ts,ic="aic",trace = TRUE)
mymodel

#Using the best ARIMA model we are trying to forecast the next 365days trend in crime rate with 99% confidence interval
fut.crimes <- forecast(mymodel,level=c(99),h=365)

#plotting the graph of forecasting time series 
plot(fut.crimes)

# Computing Holt-Winters Filtering of a given time series
hw.crimes<- HoltWinters(crimes.ts)

#using predict to fuction from results of model fitting, we are predicting next 356 daily crime rates
hw<- predict(hw.crimes,n.ahead = 365)
#having a loot at the predicted values
head(hw)

# Ploting the predicted values of time series with the past years time series. with dotted line
ts.plot(crimes.ts,hw,col="blue",lty=1:3,main="Predicting Using HoltWinters Model")

#END

