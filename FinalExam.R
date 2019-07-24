#Read in Online Retail File for Problem 1
library(readxl)
onlineRetail <- read_excel("Desktop/Penn State/Spring 2019/Foundations of Predictive Analytics/Assignments/Final Exam/Online_Retail.xlsx")
View(onlineRetail)   

#Explore Online Retail Data
summary(onlineRetail)
sapply(onlineRetail, typeof)
colSums(is.na(onlineRetail))
str(onlineRetail)

#Removes canceled orders
onlineRetail <- onlineRetail[!grepl("C", onlineRetail$InvoiceNo), ]

#Convert InvoiceNo column to a numeric vector
onlineRetail$InvoiceNo <- as.numeric(onlineRetail$InvoiceNo)

#View NAs in InvoiceNo column that were introduced due to canceled orders and bad debt adjustments
sum(is.na(onlineRetail$InvoiceNo))

#Remove rows that contain data for canceled orders and bad debt adjustments and store into new data frame
onlineRetail <- onlineRetail[!is.na(onlineRetail$InvoiceNo), ]

#Recheck to ensure NAs are removed
sum(is.na(onlineRetail$InvoiceNo))

#View summary statistics of new data frame
summary(onlineRetail)
View(onlineRetail)

#Identify the number of negative values in the Quantity Column
sum(onlineRetail$Quantity < 0)

#Identify the number of negative values in the Price column
sum(onlineRetail$UnitPrice < 0)

#Remove Negative numbers from Quantity Column
onlineRetail2 <- onlineRetail[onlineRetail$Quantity >= 0, ]

#Check for outliers
boxplot.default(onlineRetail2$Quantity, main = "Online Retail Quantity", ylab = "Quantity Sold Per Order" )
boxplot.default(onlineRetail2$UnitPrice, main = "Online Retail Unit Price", ylab = "Unit Price Per Order" )

#Partition Data to Online Display Invoice Data and Unit Price
library(plyr)
onlineRetail3 <- subset(onlineRetail2, select = c(5,6))
invoiceProfit <- onlineRetail3
dailyProfit <- invoiceProfit$UnitPrice

#Aggregate Daily data in months
invoiceProfit$InvoiceDate <- strptime(invoiceProfit$InvoiceDate,format="%m/%d/%Y %H:%M")
invoiceProfit$InvoiceDate <- trunc(onlineRetail3$InvoiceDate,"months")

#Calcuates profit per day
invoiceDs <- ddply(invoiceProfit,.(InvoiceDate),
                    summarize,
                    Profit = sum(UnitPrice)
)
#Stores profit as a variable
monthlyRevenue <- invoiceDs$Profit

# Moving average function
# Copyright https://github.com/cran/pracma/blob/master/R/movavg.R
movavg <- function(x, n, type=c("s", "t", "w", "m", "e", "r")) {
  stopifnot(is.numeric(x), is.numeric(n), is.character(type))
  if (length(n) != 1 || ceiling(n != floor(n)) || n <= 1)
    stop("Window length 'n' must be a single integer greater 1.")
  nx <- length(x)
  if (n >= nx)
    stop("Window length 'n' must be greater then length of time series.")
  y <- numeric(nx)
  if (type == "s") {         # simple
    for (k in 1:(n-1))  y[k] <- mean(x[1:k])
    for (k in n:nx)     y[k] <- mean(x[(k-n+1):k])
  } else if (type == "t") {  # triangular
    n <- ceiling((n + 1)/2)
    s <- movavg(x, n, "s")
    y <- movavg(s, n, "s")
  } else if (type == "w") {  # weighted
    for (k in 1:(n-1))  y[k] <- 2 * sum((k:1)*x[k:1]) / (k*(k+1))
    for (k in n:nx)     y[k] <- 2 * sum((n:1)*x[k:(k-n+1)]) / (n*(n+1))
  } else if (type == "m") {  # modified
    y[1] <- x[1]
    for (k in 2:nx)     y[k] <- y[k-1] + (x[k] - y[k-1])/n
  } else if (type == "e") {  # exponential
    a <- 2/(n+1)
    y[1] <- x[1]
    for (k in 2:nx)     y[k] <- a*x[k] + (1-a)*y[k-1]
  } else if (type == "r") {  # running
    a <- 1/n
    y[1] <- x[1]
    for (k in 2:nx)     y[k] <- a*x[k] + (1-a)*y[k-1]
  } else
    stop("The type must be one of 's', 't', 'w', 'm', 'e', or 'r'.")
  return(y)
}

#Create and Plot Timeseries 1
monthlyRevenueTs <- ts(monthlyRevenue)
xlimits = length(invoiceDs$InvoiceDate)
plot.ts(monthlyRevenueTs, type = "l", col = 1, xlim = c(0,xlimits), main = "Types of Moving Averages for Online Sales 
   (Total Earnings Per Month)", 
     sub = "December 2010 - December 2011 ", xlab = "Months ", ylab = "Profit Per Month")

y <- movavg(monthlyRevenue, 3, "s")
lines(y, col = 2)
y <- movavg(monthlyRevenue, 4, "s")
lines(y, col = 3)
y <- movavg(monthlyRevenue, 5, "s")
lines(y, col = 4)

y <- movavg(monthlyRevenue, 5, "w")
lines(y, col = 5)

y <- movavg(monthlyRevenue, 2, "e")
lines(y, col = 6)

grid()
legend(x = "topleft", 40, c("original data", "MV(3)", "MV(4)", "MV(5)", "weighted", "exponential"), 
       col = 1:7, lty = 1, lwd = 1, box.col = "gray", bg = "white", cex = 0.7)

###########
#Second time series plot
#Partition Data to Online Display Invoice Data and Order Quantity
library(plyr)
onlineRetail4 <- subset(onlineRetail2, select = c(5,4))
invoiceProfit2 <- onlineRetail4
dailyProfit2 <- invoiceProfit2$Quantity

#Aggregate Daily data in months
invoiceProfit2$InvoiceDate <- strptime(invoiceProfit2$InvoiceDate,format="%m/%d/%Y %H:%M")
invoiceProfit2$InvoiceDate <- trunc(onlineRetail4$InvoiceDate,"months")

#Calcuates profit per day
invoiceDs2 <- ddply(invoiceProfit2,.(InvoiceDate),
                   summarize,
                   Quantity = sum(Quantity)
)
#Stores Quantity as a variable
monthlyQuantity <- invoiceDs2$Quantity

#Create and Plot Timeseries 2 for Quantites
monthlyQuantityTs <- ts(monthlyQuantity)
xlimits2 = length(invoiceDs2$Quantity)
plot.ts(monthlyQuantityTs, type = "l", col = 1, xlim = c(0,xlimits2), main = "Types of Moving Averages for Online Sales
      (Total Quantites Sold Per Month)", 
        sub = "December 2010 - December 2011 ", xlab = "Months ", ylab = "Quantities Sold Per Month")

y <- movavg(monthlyQuantityTs, 3, "s")
lines(y, col = 2)
y <- movavg(monthlyQuantityTs, 4, "s")
lines(y, col = 3)
y <- movavg(monthlyQuantityTs, 5, "s")
lines(y, col = 4)

y <- movavg(monthlyQuantityTs, 5, "w")
lines(y, col = 5)

y <- movavg(monthlyQuantityTs, 2, "e")
lines(y, col = 6)

grid()
legend(x = "topleft", 40, c("original data", "MV(3)", "MV(4)", "MV(5)", "weighted", "exponential"), 
       col = 1:7, lty = 1, lwd = 1, box.col = "gray", bg = "white", cex = 0.7)

#Performance of TS Model for Unit Price
auto.arima(monthlyRevenueTs)

#Perfomance of TS Mode for Quantity
auto.arima(monthlyQuantityTs)

#ACF anaysis TS Model for Unit Price (Time Series Plot 1)
acf(monthlyRevenueTs, main = "ACF of Unit Price for Online Sales")

#ACF analysis TS Modell for quantity (Time Series Plo 2)
acf(monthlyQuantityTs, main = "ACF of Quanity for Online Sales")

#PACF analysis TS Model for Unit Price (Time Series Plot 1)
pacf(monthlyRevenueTs, main = "PACF of Unit Price for Online Sales")

#PACF analysis TS Model for Quantity (Time Series Plot 2)
pacf(monthlyQuantityTs, main = "PACF of Quantity for Online Sales")
