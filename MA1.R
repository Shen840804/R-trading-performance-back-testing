library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr, tcpl)
library(devtools)

testMA1 <- function () {

  #Variable with all data from csv excel file
  
  rio_stock <- import("C:/Users/user/Documents/GitHub/Thesis/test_20200307.csv")
  date <- rio_stock$date
  stockPrices <- rio_stock[,2]
  dailyepu <- rio_stock$daily_policy_index
  monthlyepu_0<- rio_stock$`Monthly EPU`
  monthlysenti_0 <- rio_stock$`Monthly Orthogonalized Sentiment Index`
  RF <- rio_stock$RF
  transactioncost = 0.0005
  
  #Read montly epu and sentiment data
  rio_epu_senti <- import("C:/Users/user/Documents/GitHub/Thesis/monthlyepu_sentiment.csv")
  monthlyepu_1<- rio_epu_senti$Three_Component_Index
  monthlysenti_1 <- rio_epu_senti$Sentiment
 
  #Get the Descriptive statistic of Monthly epu and sentiment 
  
  dailyepupivot = quantile(dailyepu,c(0.3333,0.6666))
  
  monthlyepupivot = quantile(monthlyepu_1,c(0.3333,0.6666))
  
  sentipivot = quantile(monthlysenti_1,c(0.3333,0.6666))
  
  #rio_csv <- import("C:/Users/aruna/Downloads/S&P500-198501-201812-test.csv")
  #stockPrices <- rio_csv$`Level of the S&P 500 Index`
  
  #rio_csv <- import("C:/Users/aruna/Downloads/DJIA-198501-200712-test.csv")
  #stockPrices <- rio_csv$`Industrial Close`
  
  #rio_csv <- import("C:/Users/aruna/Downloads/NASDAQ-198501-201812-test.csv")
  #stockPrices <- rio_csv$`Level of the Nasdaq Composite Index`

  #UnComment the below lines if you want to filter and take the values from excel with some date range 
  #and delete the stockPrices variable line, and change this stockPrices1 name to stockPrices
  #datasubset <- subset(rio_csv, rio_csv$date > "1999-01-02" & rio_csv$date < "2000-02-04")
  #stockPrices1 <- datasubset$`Level of the Nasdaq Composite Index`
  
  #Double data type Vector storing list of stock market prices read from the csv excel
  #stockPrices <- rio_csv$`Level of the Nasdaq Composite Index`
  
  #Integer Data Type Vector to store the q values (moving average calculation days)
  q <- c(2, 5, 10, 15, 20, 25, 50, 100, 150, 200, 250)
  
  #Double Data Type Vector to store the x values (percentage of increase/decrease in price that to be monitored)
  x <- c(0, 0.05, 0.1, 0.5, 1.0, 5.0)
  
  #Integer Data Type Array to store the d values (number of days for monitoring the marked price)
  d <- c(0, 2, 3, 4, 5)
  #Moving Price Total Value Storing Variable, used for Moving Average Price Calculation
  movTotValue <- 0.0
  #Moving Average Price Variable
  movTotAvg <- 0.0
  #Variable to store the Current Stock Price plus Increased Percentage value
  risept <- 0.0
  #Variable to count the number of days the Increased Percentage value is retained in stock market
  riseptdaysCt = 0
  #Variable to store the Current Stock Price minus decreased Percentage value
  secondpt <- 0.0
  #Variable to count the number of days the Decreased Percentage value is retained in stock market
  deconddaysCt <- 0
  #Variable to store the position of the stock market price, it is used in the calculation to subtract previous single value from the moving price total
  qTrackCt <- 1
  
  #Set the storage path
  setwd("C:/Users/user/Documents/Algorithm results")
  sink("OutputAlgorithmn1.txt")
  sink()

  #Loop for q values
  for (i in 1:length(q)) {
    #Local Variable to store the loops iterations current q value, which is used later	
    qLocal <- q[i]
    #Loop for x values
    for (j in 1:length(x)) {
      #Local Variable to store the loops iterations current x value, which is used later	
      xLocal <- x[j]
      #Loop for d values
      for (k in 1:length(d)) {
        #Local Variable to store the loops iterations current d value, which is used later	
        dLocal <- d[k]
        #Initializing all the below variables to default values
        riseptdaysCt <- 0
        deconddaysCt <- 0
        risept <- 0.0
        secondpt <- 0.0
        riseFlag <- F
        declineFlag <- F
        qfact <- 0
        movTotValue <- 0.0
        movTotAvg <- 0.0
        qTrackCt <- 1
        #Loop to iterate the stock prices
        for(value in 1:length(stockPrices)) {
          #Condition to filter out of range NA values iteration
            if(!(is.na(stockPrices[value]))) {
            #Variable to track the stock price days, incremented for the iteration of every stock price, considering each stock price is on different dates
            qfact <- qfact+1
            #Condition to increment the days count for buying price monitoring
            if(( riseptdaysCt > 0) && (stockPrices[value] >= risept)) {
              riseptdaysCt <- riseptdaysCt + 1
            }
            #Condition to reset the days count for buying price monitoring when the value of the stock price drops during the monitoring days period
            if(( riseptdaysCt > 0) && (stockPrices[value] < risept)) {
              riseptdaysCt <- 0
            }
            #Condition to increment the days count for selling price monitoring
            if(( deconddaysCt > 0) && (stockPrices[value] <= secondpt)) {
              deconddaysCt <- deconddaysCt+1
            }
            #Condition to reset the days count for buying price monitoring when the value of the stock price increases during the monitoring days period
            if(( deconddaysCt > 0) && (stockPrices[value] > secondpt)) {
              deconddaysCt <- 0
            }
            #Condition to calculate the moving average when the number of days (q days) are met
            if((qLocal > 1)&&(qfact == qLocal)) {
              movTotValue <- movTotValue+stockPrices[value]
              #Calculate Moving Average
              movTotAvg <- movTotValue/qLocal
              #Subtract the first value from the Total, so that during the next average calculation, it will contain 2nd and 3rd values and so on....
              movTotValue <- movTotValue-stockPrices[qTrackCt]
              #Reset the qfact stock days tracker to the value of the current q value -1 
              #and its incremented again at the starting of the loop at line 85,
              #so that the average calculation will be executed on all iterations once the q days are reached for the first time
              qfact <- qLocal-1
              #Counter to be used to keep track to subtract one by one old values from the total, during each iteration
              qTrackCt <- qTrackCt+1
            } 
            #This condition is to keep adding the stock prices to the Total till the number of q days are reached for the moving average calculation
            else {
              movTotValue <- movTotValue + stockPrices[value]
            }
            #Calculate the Increased Amount (rising percentage value + moving average value) for Buying
            if(movTotAvg > stockPrices[value]) {
                #Condition to Check that it should be a first time buying, if not then there is a selling done before
                if((riseptdaysCt == 0) && (!riseFlag)) {
                  risept <- movTotAvg + ((movTotAvg * xLocal) / 100)
                  riseptdaysCt <- riseptdaysCt+1
                }
            } 
            #Calculate the Decreased Amount (moving average value - decrease percentage value) for Selling
            if((riseFlag) && (movTotAvg > stockPrices[value])) {
                #Condition to Check that there is a open buying exists in order to sell
                if ((riseptdaysCt == 0) && (deconddaysCt == 0) && (!declineFlag)){
                  secondpt <- movTotAvg - ((movTotAvg * xLocal) / 100)
                  deconddaysCt <- deconddaysCt+1
                }
            }
            #Condition to check that the raised price continued for d days 
            #(as in the above loops logic, the day rising percentage calculated also considered as one day  instead of from the next days, here it has be subtracted 1)
            if(riseptdaysCt-1 == dLocal) {
              sink("OutputAlgorithmn1.txt", append = TRUE)
              cat(c("Buying date is:", date[value],"\n"),c("Buying Price is:", stockPrices[value],"\n"),
                  c("Transaction cost is :", stockPrices[value]*transactioncost,"\n"),
                  c("Risk free rate is :", RF[value],"\n"),
                  "q,x,d = ",qLocal, xLocal, dLocal,"\n")
            
              #Define the trading date is High or Low Daily EPU
              if(dailyepu[value] <= dailyepupivot[1]){
                cat(c("The Daily EPU is:", dailyepu[value],"/","Low Daily EPU period \n"))}
              else if(dailyepupivot[1] < dailyepu[value] && dailyepu[value] < dailyepupivot[2]  ){
                cat(c("The Daily EPU is:", dailyepu[value], "/","Medium Daily EPU period \n"))
              }else{
                cat(c("The Daily EPU is:", dailyepu[value], "/","High Daily EPU period \n"))
              }
              
              #Define the trading date is High or Low Monthly EPU
              if(monthlyepu_0[value] <= monthlyepupivot[1]){
                cat(c("The Monthly EPU is:" ,monthlyepu_0[value], "/","Low Monthly EPU period \n"))}
              else if(monthlyepupivot[1] < monthlyepu_0[value] && monthlyepu_0[value] < monthlyepupivot[2]){
                cat(c("The Monthly EPU is:" ,monthlyepu_0[value],"/","Medium Monthly EPU period \n"))
              }else{
                cat(c("The Monthly EPU is:" ,monthlyepu_0[value],"/","High Monthly EPU period \n"))
              }
              
              #Define the trading date is High or Low Monthly Sentiment
              if(monthlysenti_0[value] <=  sentipivot[1]){
                cat(c("The Monthly Sentiment is:" ,monthlysenti_0[value], "/","Low Monthly Sentiment period \n"))}
              else if(sentipivot[1] <monthlysenti_0[value] && monthlysenti_0[value] < sentipivot[2]){
                cat(c("The Monthly Sentiment is:" ,monthlysenti_0[value],"/","Medium Monthly Sentiment period \n"))
              }else{
                cat(c("The Monthly Sentiment is:" ,monthlysenti_0[value],"/","High Monthly Sentiment period \n"))
              }
              cat("\n")
              sink()
              riseptdaysCt = 0
              deconddaysCt = 0
              riseFlag = 1
              declineFlag = 0
            }
            #Condition to check that the decreased price continued for d days 
            #(as in the above loops logic, the day decreased percentage calculated also considered as one day  instead of from the next days, here it has be subtracted 1)
            if(deconddaysCt-1 == dLocal) {
              sink("OutputAlgorithmn1.txt", append = TRUE)
              cat(c("Selling date is:", date[value],"\n"),c("Selling Price is:", stockPrices[value],"\n"),
                  c("Transaction cost is :", stockPrices[value]*transactioncost,"\n"),
                  c("Risk free rate is :", RF[value],"\n"),
                  "q,x,d = ",qLocal, xLocal, dLocal,"\n")
              
              #Define the trading date is High or Low Daily EPU
              if(dailyepu[value] <= dailyepupivot[1]){
                cat(c("The Daily EPU is:", dailyepu[value],"/","Low Daily EPU period \n"))}
              else if(dailyepupivot[1] < dailyepu[value] && dailyepu[value] < dailyepupivot[2]  ){
                cat(c("The Daily EPU is:", dailyepu[value], "/","Medium Daily EPU period \n"))
              }else{
                cat(c("The Daily EPU is:", dailyepu[value], "/","High Daily EPU period \n"))
              }
              
              #Define the trading date is High or Low Monthly EPU
              if(monthlyepu_0[value] <= monthlyepupivot[1]){
                cat(c("The Monthly EPU is:" ,monthlyepu_0[value], "/","Low Monthly EPU period \n"))}
              else if(monthlyepupivot[1] < monthlyepu_0[value] && monthlyepu_0[value] < monthlyepupivot[2]){
                cat(c("The Monthly EPU is:" ,monthlyepu_0[value],"/","Medium Monthly EPU period \n"))
              }else{
                cat(c("The Monthly EPU is:" ,monthlyepu_0[value],"/","High Monthly EPU period \n"))
              }
              
              #Define the trading date is High or Low Monthly Sentiment
              if(monthlysenti_0[value] <=  sentipivot[1]){
                cat(c("The Monthly Sentiment is:" ,monthlysenti_0[value], "/","Low Monthly Sentiment period \n"))}
              else if(sentipivot[1] <monthlysenti_0[value] && monthlysenti_0[value] < sentipivot[2]){
                cat(c("The Monthly Sentiment is:" ,monthlysenti_0[value],"/","Medium Monthly Sentiment period \n"))
              }else{
                cat(c("The Monthly Sentiment is:" ,monthlysenti_0[value],"/","High Monthly Sentiment period \n"))
              }
              cat("\n")
              sink()
              riseptdaysCt = 0
              deconddaysCt = 0
              declineFlag = 1
              riseFlag = 0
            }
          }
        }
      }
    }
  }
}

testMA1()

sink.reset()
