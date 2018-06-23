#Import library
library(ggplot2)
library(ggthemes)
library(forecast)

sales<-read.csv("C:/Rise_Internship/Datasets/DayWiseSales_Ex1.csv")
str(sales)

#Removing the rows with negative Netsales
sales<-sales[sales$Netsales>0,]

#convert the SaleDate into Date format
sales$SaleDate<-as.Date(sales$SaleDate,"%d/%m/%Y")
str(sales)

head(sales,n=5)

#Plot the data date by Netsales
ggplot(sales,aes(x=SaleDate,y=Netsales))+
  theme_bw()+
  geom_line()+
  geom_smooth(method = "lm")
  labs(x="SaleDate",
       y="NetSales of the Company")
  
sale_ts<-ts(sales$Netsales,start=2012,end=2017,frequency = 365)
str(sale_ts)

#write.csv(sale_ts,"C:/Rise_Internship/Datasets/sale.csv")

salesDecomp <- decompose(sale_ts)
plot(salesDecomp)

# log transform time series data
salesLog <- log(sale_ts)
plot(salesLog)

#HoltWinters 
salesLogHW <- HoltWinters(salesLog)
str(salesLogHW)
head(salesLogHW$fitted,n=8)
tail(salesLogHW$fitted,n=8)

plot(salesLogHW)

#Looking at the plot above, we can see that the HoltWinters prediction(Red) is 
#close to the observed values.

# Forecast next 30 day's sales
nextthirtydaysSales <- forecast(salesLogHW, h=30)
# plot
plot(nextthirtydaysSales)

nextthirtydaysSales

#These are forecasted values fr next thirty days
tt<-exp(nextthirtydaysSales$mean)
tail(tt)
tt




