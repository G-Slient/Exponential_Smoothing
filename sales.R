#Import libraries
library(ggplot2)  #Visualization
library(ggthemes) #visualization
library(caTools)
library(plyr)    
library(smooth)  #For Exponential Smoothing 
library(greybox)  
library(Mcomp)

#Import Data 
sale_Data<-read.csv("C:/Rise_Internship/Datasets/DayWiseSales_Ex1.csv",stringsAsFactors = FALSE)
sale_Data$SaleID<-factor(sale_Data$SaleID)
str(sale_Data)

#convert the SaleDate into Date format
sale_Data$SaleDate<-as.Date(sale_Data$SaleDate,"%d/%m/%Y")
str(sale_Data)

# #Splitting the date into day, month and year
# df <- data.frame(SaleDate = sale_Data$SaleDate,
#                  year = as.numeric(format(sale_Data$SaleDate, format = "%Y")),
#                  month = as.numeric(format(sale_Data$SaleDate, format = "%m")),
#                  day = as.numeric(format(sale_Data$SaleDate, format = "%d")))
# 
# dataset<-join(sale_Data,df,by="SaleDate")
# str(dataset)

# dataset$Netsales<-as.ts(dataset$Netsales)
 
#Spliting into Training and testing
train<-sale_Data[1:1276,]
test<-sale_Data[1277:1595,]

#Plot the data date by Netsales
ggplot(train,aes(x=Netsales,y=SaleDate))+
  theme_bw()+
  geom_line()+
  labs(x="NetSales of the Company",
       y="SaleDate")

##From the Graph below, it is clear that there is some seasonality and we can 
##observe that there is no trend.

#Plot the data date by Netsales
ggplot(train,aes(x=SaleDate,y=Netsales))+
  theme_bw()+
  geom_line()+
  labs(x="SaleDate",
       y="Netsales")

#Plot the boxplot for the netsales
ggplot(train,aes(y=Netsales,x=SaleDate))+
  theme_bw()+
  geom_boxplot()


# Sale_ts<-ts(sale_Data$Netsales,start=c(2012),end=c(2018),frequency = 365)
# 
library("xts")
sale_tt<-as.ts(sale_Data$Netsales,order.by = sale_Data$SaleDate)

# plot.ts(Sale_ts)


es(sale_Data$Netsales, h=18, holdout=TRUE, silent=FALSE)


es(sale_Data$Netsales, h=18, holdout=TRUE, intervals=TRUE, silent=FALSE)

ourModel <- es(sale_Data$Netsales, h=18, holdout=TRUE, silent="all")

es(sale_Data$Netsales, model=ourModel, h=18, holdout=FALSE, intervals="np", level=0.93)

modelType(ourModel)

es(sale_Data$Netsales, model=modelType(ourModel), h=18, holdout=FALSE, initial=ourModel$initial, silent="graph")

es(sale_Data$Netsales, model=modelType(ourModel), h=18, holdout=FALSE, persistence=ourModel$persistence, silent="graph")

es(sale_Data$Netsales, model=modelType(ourModel), h=18, holdout=FALSE, initial=1500, silent="graph")

es(sale_Data$Netsales, h=18, holdout=TRUE, cfType="aTMSE", bounds="a", ic="BIC", intervals=TRUE)

es(sale_Data$Netsales, model="CCN", h=18, holdout=TRUE, silent="graph")

es(sale_Data$Netsales, model=c("ANN","AAN","AAdN","ANA","AAA","AAdA"), h=18, holdout=TRUE, silent="graph")

x <- cbind(rnorm(length(sale_Data$Netsales),50,3),rnorm(sale_Data$Netsales),100,7)

es(sale_Data$Netsales, model="ZZZ", h=18, holdout=TRUE, xreg=x)

ourModel <- es(sale_Data$Netsales, model="ZZZ", h=18, holdout=TRUE, xreg=x, updateX=TRUE)


es(sale_Data$Netsales, model="ZZZ", h=18, holdout=TRUE, xreg=xregExpander(x), xregDo="select")

formula(ourModel)

etsModel <- forecast::ets(sale_Data$Netsales)
esModel <- es(sale_Data$Netsales, model=etsModel, h=18)

forecast(etsModel,h=18,level=0.95)

forecast(esModel,h=18,level=0.95)

es(sale_Data$Netsales, intervals=TRUE, silent=FALSE)




#Import libraries
library(ggplot2)  #Visualization
library(ggthemes) #visualization
#library(caTools)
#library(plyr)    
#library(smooth)  #For Exponential Smoothing 
#library(greybox)  
#library(Mcomp)
#library(fpp)

#Import Data 
sale_Data<-read.csv("C:/Rise_Internship/Datasets/DayWiseSales_Ex1.csv")
sale_Data$SaleID<-factor(sale_Data$SaleID)
str(sale_Data)

#convert the SaleDate into Date format
sale_Data$SaleDate<-as.Date(sale_Data$SaleDate,"%d/%m/%Y")
str(sale_Data)



#Removing the rows with negative Netsales
sale_Data<-sale_Data[sale_Data$Netsales>0,]

sale_Data<-sale_Data[c(3)]
str(sale_Data)

#sale_Data$SaleDate<-ts(sale_Data$SaleDate)
sale_tt<-ts(sale_Data$Netsales,start=2012,end=2018,frequency = 365)
str(sale_tt)

plot.ts(sale_tt)



#Data manipulation in case of seasonal or random fluctuations
sale_tt_log<-log(sale_tt)

plot.ts(sale_tt_log)

# #Simple Exponential Smoothing
# library(expsmooth)
# ccsmooth<- HoltWinters(sale_tt, beta=FALSE, gamma=FALSE)
# ccsmooth
# plot(ccsmooth$fitted)
# 
# Considering the seasonality
# ccsmooths<- HoltWinters(sale_tt, beta=FALSE, gamma=TRUE)
# ccsmooths
# plot(ccsmooths$fitted)



# #install.packages("TTR")
# library("TTR")
# 
# #In case fluctuations are present and you want to smooth the time series
# sale7<-SMA(sale_tt,7)
# str(sale7)
# plot.ts(sale7)
# 
# #Exponential smoothing
# sale_ema<-EMA(sale_tt,3,0.25)
# str(sale_ema)
# plot.ts(sale_ema)


library(fpp)
data("olympic")
olympic

str(olympic)

dput(olympic)
year <- seq.int(1896L, 1996L, by = 4L)
y <- rep.int(NA, length(year))
y[match(olympic$Year, year)] <- olympic$time









