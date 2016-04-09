#library(lattice)
library(foreach)
#library(doSNOW)
#library(ggplot2)
library(PerformanceAnalytics)
#require(latticeExtra)
#require(grid)
#library(gridExtra)
#library(reshape)
library(quantstrat)                               
require(doMC)

#uncomment these lines and set appropriately to use multiple cores
cores=12
registerDoMC(cores)

#uncomment this line to use one core
#registerDoSEQ()

.strategy<- new.env()
.blotter<- new.env()                              

currency(c('USD', 'EUR'))
exchange_rate(primary_id="EURUSD", tick_size=0.0001)

data.location.r <- "processingtime_q_rsigfinance.csv"
symbol.data <- as.xts(read.zoo(data.location.r, sep=',', tz="",header=TRUE, format='%d/%m/%Y %H:%M', index.column = 1))
symbol.data <- symbol.data[symbol.data$VOLUME!=0,]                                                            
symbol.data[,c(1,2,3,4)] <- round(as.numeric(symbol.data[,c(1,2,3,4)]),abs(log10(0.0001)))
assign("EURUSD", symbol.data)

strategy.st <- "rsigfinance"
rm.strat(strategy.st)                                                      

initDate = as.character(as.Date(index(symbol.data[1])-1))                    
initPortf(strategy.st, "EURUSD", initDate=initDate, currency = "USD")
initAcct(strategy.st, portfolios=strategy.st, initDate=initDate, initEq=100000, currency = "USD")
initOrders(portfolio=strategy.st,initDate=initDate)                          
strategy(strategy.st,store=TRUE)
summary(getStrategy(strategy.st))                                            

positionSizeLong  =    round(100000 / as.numeric(symbol.data$CLOSE[1]),-2)
positionSizeShort =  - round(100000 / as.numeric(symbol.data$CLOSE[1]),-2)
txn.model <- 0                                                     
sltsltp.txn.fee <- 0

add.indicator(strategy.st,  
              name = "MACD", 
              arguments = list(x=Cl(eval(parse(text = "EURUSD")))), 
              label='macd') 

add.signal(strategy.st,name="sigCrossover",
           arguments = list(columns=c("macd.macd","signal.macd"),relationship="gt"),
           label="macd.gt.signal") 

add.signal(strategy.st,name="sigCrossover",
           arguments = list(columns=c("macd.macd","signal.macd"),relationship="lt"),
           label="macd.lt.signal")

add.rule(strategy.st,
         name='ruleSignal',
         arguments = list(sigcol="macd.gt.signal",
                          sigval=TRUE,
                          prefer="Open", 
                          orderqty= positionSizeLong, 
                          ordertype='market',
                          orderside='long',
                          orderset='ocolong',
                          TxnFees = txn.model),
         type='enter',
         label='longenter',
         enabled=TRUE
)

add.rule(strategy.st,
         name='ruleSignal',
         arguments = list(sigcol="macd.lt.signal",
                          sigval=TRUE,
                          prefer="Open", 
                          orderqty='all',
                          ordertype='market',
                          orderside='long',
                          orderset='ocolong',
                          TxnFees = txn.model),
         type='exit',
         label='longexit',
         enabled=TRUE
)

add.rule(strategy.st,name='ruleSignal',
         arguments = list( sigcol="macd.lt.signal", sigval=TRUE,
                           replace=FALSE,
                           orderside='long',
                           ordertype='stoplimit',
                           tmult=TRUE,
                           threshold=quote( longStopLossDistance ),
                           orderqty='all',
                           orderset='ocolong',
                           TxnFees = txn.model),
         type='chain', parent="longenter",
         label='StopLossLong',
         enabled=TRUE)

# add.rule(strategy.st, name = 'ruleSignal',
#          arguments=list(sigcol="macd.lt.signal" , sigval=TRUE,
#                         replace=FALSE,
#                         orderside='long',
#                         ordertype='stoptrailing',
#                         tmult=TRUE,
#                         threshold=quote(longTrailingStopDistance),
#                         orderqty='all',
#                         orderset='ocolong',
#                         TxnFees = txn.model),
#          type='chain', parent="longenter",
#          label='StopTrailingLong',
#          enabled=FALSE
# )

# add.rule(strategy.st, name = "ruleSignal",
#          arguments = list(sigcol="macd.lt.signal",
#                           sigval=TRUE,
#                           ordertype="limit",
#                           orderside="long",
#                           replace=FALSE,
#                           tmult=TRUE,
#                           threshold=quote(longTakeProfitDistance), 
#                           orderqty="all",
#                           orderset="ocolong",
#                           TxnFees = txn.model),
#          type = "chain", parent="longenter",
#          label = "takeProfitLong",
#          enabled = FALSE
# )

add.rule(strategy.st,
         name='ruleSignal',
         arguments = list(sigcol="macd.lt.signal",
                          sigval=TRUE,
                          prefer="Open", 
                          orderqty=positionSizeShort, 
                          ordertype='market',
                          orderside='short',
                          orderset='ocoshort',
                          TxnFees = txn.model),
         type='enter',
         label='shortenter',
         enabled=TRUE
)

add.rule(strategy.st,
         name='ruleSignal',
         arguments = list(sigcol="macd.gt.signal",
                          sigval=TRUE,
                          prefer="Open", 
                          orderqty='all',
                          ordertype='market',
                          orderside='short',
                          orderset='ocoshort',
                          TxnFees = txn.model),
         type='exit',
         label='shortexit',
         enabled=TRUE
)

add.rule(strategy.st,name='ruleSignal',
         arguments = list( sigcol="macd.gt.signal", sigval=TRUE,
                           replace=FALSE,
                           orderside='short',
                           ordertype='stoplimit',
                           tmult=TRUE,
                           threshold=quote( shortStopLossDistance ),
                           orderqty='all',
                           orderset='ocoshort',
                           TxnFees = txn.model),
         type='chain', parent="shortenter",
         label='StopLossShort',
         enabled=TRUE)

# add.rule(strategy.st, name = 'ruleSignal',
#          arguments=list(sigcol="macd.gt.signal" , sigval=TRUE,
#                         replace=FALSE,
#                         orderside='short',
#                         ordertype='stoptrailing',
#                         tmult=TRUE,
#                         threshold=quote( shortTrailingStopDistance),
#                         orderqty='all',
#                         orderset='ocoshort',
#                         TxnFees = txn.model),
#          type='chain', parent="shortenter",
#          label='StopTrailingShort',
#          enabled=FALSE
# )

# add.rule(strategy.st, name = "ruleSignal",
#          arguments = list(sigcol="macd.gt.signal",
#                           sigval=TRUE,
#                           ordertype="limit",
#                           orderside="short",
#                           replace=FALSE,
#                           tmult=TRUE,
#                           threshold=quote( -shortTakeProfitDistance), 
#                           orderqty="all",
#                           orderset="ocoshort",
#                           TxnFees = txn.model),
#          type = "chain", parent="shortenter",
#          label = "takeProfitShort",
#          enabled = FALSE
# )

# set up the distributions and ranges

macdFastMARange <- seq(2,17,by=5)
macdSlowMARange <- seq(5,35,by=10)
macdSignalRange <- seq(2,18,by=8)

StopLossDistanceRange <- seq(0.01,0.02,by=0.01)                               
TrailingDistanceRange <- seq(0.01,0.02,by=0.01)                               

positionSizeLong  =    round(100000 / as.numeric(symbol.data$CLOSE[1]),-2)
positionSizeShort =  - round(100000 / as.numeric(symbol.data$CLOSE[1]),-2)
txn.model <- 0                                                     
sltsltp.txn.fee <- 0

add.distribution(strategy.st,
                 paramset.label = "MACD_OPT",
                 component.type = 'indicator',
                 component.label = "macd",
                 variable = list( nFast = macdFastMARange ), 
                 label = "macdFastMARANGE")


add.distribution(strategy.st,
                 paramset.label = "MACD_OPT",
                 component.type = 'indicator',
                 component.label = "macd",
                 variable = list( nSlow = macdSlowMARange ),
                 label = "macdSlowMARANGE")

add.distribution(strategy.st,
                 paramset.label = "MACD_OPT",
                 component.type = 'indicator',
                 component.label = "macd",
                 variable = list( nSig = macdSignalRange ),
                 label = "macdSignalRANGE")

add.distribution.constraint(strategy.st,
                            paramset.label = 'MACD_OPT',
                            distribution.label.1 = 'macdFastMARANGE',
                            distribution.label.2 = 'macdSlowMARANGE',
                            operator = '<',
                            label = 'FastMA<SlowMA')

add.distribution(strategy.st,
                 paramset.label = "MACD_OPT",
                 component.type = "chain",
                 component.label = "StopLossLong",
                 variable = list( threshold = StopLossDistanceRange ),
                 label = "StopLossLONG")

# add.distribution(strategy.st,
#                  paramset.label = "MACD_OPT",
#                  component.type = "chain",
#                  component.label = "StopTrailingLong",
#                  variable = list( threshold = TrailingDistanceRange ),
#                  label = "StopTrailingLONG")

add.distribution(strategy.st,
                 paramset.label = "MACD_OPT",
                 component.type = "chain",
                 component.label = "StopLossShort",
                 variable = list( threshold = StopLossDistanceRange ),
                 label = "StopLossSHORT")

# add.distribution(strategy.st,
#                  paramset.label = "MACD_OPT",
#                  component.type = "chain",
#                  component.label = "StopTrailingShort",
#                  variable = list( threshold = TrailingDistanceRange ),
#                  label = "StopTrailingSHORT")

add.distribution.constraint(strategy.st,
                            paramset.label = "MACD_OPT",
                            distribution.label.1 = "StopLossLONG",
                            distribution.label.2 = "StopLossSHORT",
                            operator = "==",
                            label = "StoplossEquality")

# add.distribution.constraint(strategy.st,
#                             paramset.label = "MACD_OPT",
#                             distribution.label.1 = "StopTrailingLONG",
#                             distribution.label.2 = "StopTrailingSHORT",
#                             operator = "==",
#                             label = "TrailingStopEquality")
                       

summary(getStrategy(strategy.st))                                             



nFast <- 9
nSlow <- 24
nSignal <- 7
longStopLossDistance <- 0.01;longTrailingStopDistance <- 0.01;longTakeProfitDistance <- 0.01
shortStopLossDistance <- 0.01;shortTrailingStopDistance <- 0.01;shortTakeProfitDistance <- 0.01

paramsetenv<-new.env()

#turn off prescheduling to get better load balancing
mcoptions <- list(preschedule=FALSE)

start.t <- Sys.time()

# results <- applyStrategy( strategy=strategy.st , portfolios=strategy.st, 
#                           parameters=list(nFast=nFast, nSlow=nSlow, nSig=nSignal),verbose=TRUE)
#
# updatePortf(strategy.st)
# updateAcct(strategy.st)
# updateEndEq(strategy.st)

results <- apply.paramset(strategy.st,paramset.label="MACD_OPT",
                          portfolio=strategy.st, account=strategy.st,nsamples=0,verbose = FALSE,
                          audit=paramsetenv
                          , .options.multicore=mcoptions
                          )
 
finish.t <- Sys.time()
print(finish.t-start.t)
 
