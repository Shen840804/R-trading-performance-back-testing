library(knitr)
opts_chunk$set(tidy=FALSE,cache=FALSE,size='scriptsize',
               fig.path='figures/',fig.show='hide',fig.keep='last',
               fig.align='center',	fig.width=7,	fig.height=5,
               message=FALSE,warning=FALSE)

## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=81,continue=" ",digits=8)

library(quantstrat) # main package
library(quantmod) # retrieve symbols from yahoo finance, google finance, etc
library(IKTrading) # quantstrat extensions, mainly for asset allocation and order-sizing functions
library(doParallel) # for apply.paramset() and walk.forward()

suppressWarnings(try(rm(list=ls(FinancialInstrument:::.instrument),pos=FinancialInstrument:::.instrument),silent=TRUE))

#--------------------------Initial a currency and a stock instrument-----------------------------------------

currency("USD")

stock("DJI",currency = "USD", multiplier = 1)

ls(envir = FinancialInstrument:::.instrument)

ls(all=T)

#--------------------------Fetch historic data-------------------------------------------
initDate = '1997-12-31'
startDate = '1998-01-01'
endDate = '2014-06-30'
initEq = 1e6

Sys.setenv(TZ='UTC')
if(file.exists("DJI.RData"))
{
  load("DJI.RData")
} else {
  getSymbols('^DJI', from=startDate, to=endDate, index.class="POSIXct", adjust=T)
  save(list="DJI",file="DJI.RData")
}

#--------------------------Initialize portfolio and account-------------------------------

MA.strategy = "shen's"

rm.strat(MA.strategy) # remove strategy etc. if this is a re-run

initPortf(MA.strategy,"DJI",initDate = initDate)

initAcct(MA.strategy, portfolios = MA.strategy, initDate = initDate, initEq = initEq)

#Initialize orders and strategy

initOrders(portfolio = MA.strategy, initDate = initDate)

strategy(MA.strategy,store = TRUE)

#--------------------------Portfolio, account, orderbook, and strategy objects------------------------------

ls(all=T)
ls(.blotter)
ls(.strategy)

#--------------------------The quantstrat strategy object-------------------------

strat = getStrategy(MA.strategy)
class(strat)

summary(strat)

#--------------------------Add indicators to a strategy-----------------------------

add.indicator(strategy = MA.strategy, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")

add.indicator(strategy = MA.strategy, name= "SMA",
              arguments = list(x= quote(Cl(mktdata)),n=120), label="SMA120")


summary(getStrategy(MA.strategy))

#--------------------------Add signals to a strategy----------------------------

#Buy signal
add.signal(MA.strategy,name="sigCrossover",
           arguments = list(columns=c("SMA10","SMA120"),relationship="gt"),
           label="SMA10.gt.SMA120")
#Sell signal
add.signal(MA.strategy,name="sigCrossover",
           arguments = list(columns=c("SMA10","SMA120"),relationship="lt"),
           label="SMA10.lt.SMA120")

summary(getStrategy(MA.strategy))

#--------------------------Consider transaction cost---------------------------
buyCost <- 0.001425

buyFee <- function(TxnQty, TxnPrice, Symbol, ...)
{
  abs(TxnQty) * TxnPrice * -buyCost
}

sellCost <- 0.004425
sellFee <- function(TxnQty, TxnPrice, Symbol, ...)
{
  abs(TxnQty) * TxnPrice * -sellCost
}

#--------------------------Add rules to a strategy---------------------------

# long when quickMA > slowMA
add.rule(MA.strategy, name='ruleSignal',
         arguments = list(sigcol="SMA10.gt.SMA120",
                          sigval=TRUE,
                          orderqty=900,
                          ordertype='m00arket',
                          orderside='long',
                          TxnFees = "buyFee"),
         type='enter')

# exit when quickMA < slowMA
add.rule(MA.strategy, name='ruleSignal',
         arguments = list(sigcol="SMA10.lt.SMA120",
                          sigval=TRUE,
                          orderqty='all',
                          ordertype='market',
                          orderside='long',
                          TxnFees="sellFee"),
         type='exit')

#--------------------------Apply the strategy--------------------------------

applyStrategy(strategy = MA.strategy,portfolios = MA.strategy)

options(width=120)

getTxns(Portfolio = MA.strategy, Symbol = "DJI")

mktdata['2002']

#--------------------------Update portfolio, account, and equity--------------------------
options(width=81,digits=8)

updatePortf(MA.strategy)
updateAcct(MA.strategy)
updateEndEq(MA.strategy)

updateEndEq(qs.strategy)


#--------------------------Data integrity check---------------------------------------------------------
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}
checkBlotterUpdate(MA.strategy,MA.strategy)

#--------------------------Plot performance-------------------------

# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

# plot performance
chart.Posn(MA.strategy, Symbol = 'DJI', Dates = '1998::',theme=myTheme,
           TA='add_SMA(n=10,col=4, on=1, lwd=2);add_SMA(n=120,col=4, on=1, lwd=2)')

#--------------------------The order_book object----------------------

ob = getOrderBook(MA.strategy)

names(ob)

names(ob$`shen's`)

names(ob$`shen's`$DJI)

options(width=110)

ob$`shen's`$DJI[,1:5]

options(width=81)

head(ob$`shen's`$DJI[,6:11])

perTradeStats(MA.strategy)

#--------------------------MAE and MFE plots--------------------------

options(width=81)

chart.ME(Portfolio = MA.strategy, Symbol = 'DJI', type = 'MAE', scale = 'percent')

chart.ME(Portfolio = MA.strategy, Symbol = 'DJI', type = 'MFE', scale = 'percent')

#--------------------------Retrieving the account summary----------------------

options(width=105)

a = getAccount(MA.strategy)

last(a$summary,5)

library(lattice)

xyplot(a$summary,type='h',col=4)

#--------------------------Plot equity curve and performance chart-----------------------

options(width=81)

equity <- a$summary$End.Eq

plot(equity,main="MA Strategy Equity Curve")

ret <- Return.calculate(equity,method="log")

charts.PerformanceSummary(ret, colorset = bluefocus,
                          main="MA Strategy Performance")

#--------------------------Parameter optimization-----------------------------------

#quickMA
add.distribution(MA.strategy,
                 paramset.label = 'allParam',
                 component.type = 'indicator',
                 component.label = 'quickMA',
                 variable = list(threshold = 5:15),
                 label = 'quickMA')
#slowMA
add.distribution(MA.strategy,
                 paramset.label = 'allParam',
                 component.type = 'indicator',
                 component.label = 'slowMA',
                 variable = list(threshold = 60:200),
                 label = 'slowMA')
# Walk Forward Analysis
# paramset.label: set the range of the parameters
# k.training: training period
# k.testing: testing period
# nsamples: equal to 0 means all combinations
# obj.func: objective function. Default is based on the maximum rate of return,
#           if you want to change to Max drawdown or other conditions, you need to customize the function.
# anchored: moving time window y/n
# verbose: whether to print the transaction details

registerDoParallel(cores=detectCores())
resultsWFA <- walk.forward(
  strategy.st = MA.strategy,
  paramset.label='allParam',
  portfolio.st = MA.strategy,
  account.st = MA.strategy,
  period='years',
  k.training=4,
  k.testing=2,
  nsamples=0,
  audit.prefix='wfa',
  #obj.func=my.obj,
  #obj.args=my.args,
  anchored=TRUE,
  verbose=TRUE,
  include.insamples=TRUE
)