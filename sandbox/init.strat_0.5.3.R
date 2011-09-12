###########################################
# Functions that allow you to rapidly 
# build (or remove) a quantstrat strategy 
# First draft.
###########################################

## Depends FinancialInstrument (>=0.6.5)
## Depends quantstrat (>=0.5.3)
require(quantstrat)


#' init strategy, portfolio, account, and orders, and download data (getSymbols)
#' @param symbols character names of instrument primary_ids
#' @param portfolio character name of portfolio 
#' @param initDate starting date
#' @param initEq initial portfolio value
#' @param account name of account. Not fully supported. Default is to use \code{portfolio}
#' @param env where to assign the strategy
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @author Garrett See
init.strat <- 
function(symbols,
        portfolio='default', 
        initDate = '2011-01-01', 
        initEq = 0, 
        account=portfolio, 
        currency='USD',
        env=.GlobalEnv,
        store=FALSE)
{
    #if the currency hasn't been defined yet, define it.
    if (!is.currency(currency)) currency <- currency(currency)
        
    #if any 'symbols' are not defined as instruments, we'll make a basic instrument
    getsyms <- NULL #symbols that aren't in .GlobalEnv that we'll have to get
    for (sym in symbols) {
        if(!is.instrument(getInstrument(sym,silent=TRUE))) {
            instrument.auto(sym, currency=currency)
        }   
        tmp <- try(get(sym,pos=env),silent=TRUE)
        #test for is.xts here?
        if (inherits(tmp, 'try-error')) getsyms <- c(getsyms, sym)
    }
    if (!is.null(getsyms)) getSymbols(getsyms,from=initDate) #get the data that didn't exist in env
    initPortf(name=portfolio, symbols=symbols, initPosQty=0, initDate=initDate)
    initAcct(name=account, portfolios=portfolio, initDate=initDate, initEq=initEq, currency=currency)
    initOrders(portfolio=portfolio, symbols=symbols, initDate=initDate)
    if (store)
        assign(portfolio, strategy(portfolio), env=env)
    else return(strategy(portfolio))    
    portfolio    
}


#' Remove objects associated with a strategy
#'
#' Remove the order_book, account, and portfolio 
#' @param name name of the portfolio.
rm.strat <- function(name='default') {
    if (is.strategy(name)) name <- name[['name']]
    try(rm(list=paste("order_book",name,sep="."), pos=.strategy))
    try(rm(list=paste(c("account", "portfolio"), name, sep="."), pos=.blotter))
}


#' Add sigCrossover signals and rules to your strategy
#' @param strategy name of a strategy or the strategy itself
#' @param pcolumns vector of names of price columns that need to cross indicator columns to trigger a signal.  First columns in \code{\link{sigCrossover}} columns argument
#' @param icolumns vecror of names of indicator columns that when crossed by price columns will trigger a signal.  Second columns in \code{\link{sigCrossover}} columns argument
#' @param relationships vector of same length as \code{pcolumns} indicating how to compare pcolumns and icolumns ('gt', 'lt', 'gte', 'lte' for greater than, less than, greater than or equal, less than or equal, respectively
#' @param ruletypes vector of same lenghth as \code{pcolumns} indicating type of rule ('enter', 'exit', etc.)
#' @param orderqty list of same length as \code{pcolumns} indicating or1der quantities
#' @param ordertypes vector of same length as \code{pcolumns} indicating type of order ('market', 'limit', etc')
#' @param df optional data.frame with columns named \code{pcolumns}, \code{icolumns}, \code{relationships},
#' \code{ruletypes}, \code{orderqty}, \code{ordertypes} to use instead of those respective arguments.
#' @param portfolio name of portfolio. if missing, it will become the same as the strategy name
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @author Garrett See
add.sigCrossovers <- 
function(strategy,
        pcolumns=     rep("Close",4), 
        icolumns=     c('dn','up','mavg','mavg'), 
        relationships=c('lt','gt','gt','lt'),
        ruletypes=    c('enter','enter','exit','exit'),
        orderqty=     list(100,-100,'all','all'),
        ordertypes=   rep("market",4),
        df= NULL,
        osFUN='osNoOp',
        portfolio,
        store = FALSE) {
    #if (is.strategy(strategy)) strategy <- strategy$name    
    if (!is.strategy(strategy)) strategy <- getStrategy(strategy)
    if (missing(portfolio)) portfolio <- strategy$name
    if (!is.null(df)) {
        if (!is.null(df$pcolumns)) pcolumns <- df$pcolumns
        if (!is.null(df$icolumns)) icolumns <- df$icolumns
        if (!is.null(df$relationships)) relationships <- df$relationships
        if (!is.null(df$ruletypes)) ruletypes <- df$ruletypes
        if (!is.null(df$orderqty)) orderqty <- df$orderqty
        if (!is.null(df$ordertypes)) ordertypes <- df$ordertypes
    }    
    lpc <- length(pcolumns)    
    if (lpc != length(icolumns) || lpc != length(relationships) 
            || lpc != length(ruletypes) || lpc !=length(ordertypes) 
            || lpc !=length(orderqty)) stop("all args except 'strategy' must be of the same length")    
    for (i in seq_along(pcolumns)) {
        rulename <- paste(pcolumns[[i]],relationships[[i]],icolumns[[i]],sep=".")
        strategy <- add.signal(strategy, name="sigCrossover", 
                    arguments = list(columns=c(pcolumns[[i]],icolumns[[i]]),relationship=relationships[[i]]),
                    label=rulename, store=FALSE)
        qty <- ifelse(orderqty[[i]] != 'all',as.numeric(orderqty[[i]]), 'all')
        strategy <- add.rule(strategy, 'ruleSignal',
                    arguments = list(sigcol=rulename, sigval=TRUE, 
                                    orderqty=orderqty[[i]],
                                    orderside=NULL, 
                                    threshold=NULL,
                                    pricemethod='opside',
                                    portfolio=portfolio,
                                    ordertype=ordertypes[[i]],
                                    osFUN=osFUN), 
                                    type=ruletypes[[i]],
                                    store=FALSE)
    }
    if (store) assign(strategy$name, strategy, pos='.strategy')
    else return(strategy)
    strategy$name
}

#' update portfolio, account, and ending equity
#' @param portfolio string identifying a portfolio
#' @param account string identifying an account. Same as \code{portfolio} by default
#' @param Symbols: character vector of names of symbols whose portfolios will be updated
#' @param Dates optional xts-style ISO-8601 time range to run updatePortf over, default NULL (will use times from Prices)
#' @param Prices optional xts object containing prices and timestamps to mark the book on, default NULL
#' @param showEq TRUE/FALSE should ending equity be printed to the screen with a call to \code{cat}. default TRUE
#' @param chart TRUE/FALSE if TRUE (default) a call will be made to \code{chart.Posn}
#' @seealso \code{\link{updatePortf}}, \code{\link{updateAcct}}, \code{\link{updateEndEq}}
#' @author Garrett See
update.strat <- 
function(portfolio='default', 
         account=portfolio, 
         Symbols=NULL, 
         Dates=NULL, 
         Prices=NULL,
         showEq=TRUE,
         chart=TRUE)
{
    out <- list()
    out[['portfolio.st']] <- updatePortf(Portfolio=portfolio, Symbols=Symbols, Dates=Dates, Prices=Prices)
    out[['account.st']] <- updateAcct(name=account,Dates=Dates) 
    updateEndEq(Account=account,Dates=Dates)
    if (showEq) cat('EndingEq: ', getEndEq(Account=account,Date=Sys.time()), '\n')
    if (chart) chart.Posn(Portfolio=portfolio, Symbol=names(getPortfolio(portfolio)$symbols)[[1]])
    if (out[[1]] == out[[2]]) out[[1]]
    else out
}

##########################################################################################################
##### Now for a demo.  These few lines will do what the bbands demo does.
#rm.strat('bbands')
strat <- init.strat(stock("IBM",currency("USD"),src='yahoo'), portfolio='bbands', initDate='2006-12-31')
strat <- add.indicator(strat, name='BBands', arguments=list(HLC=quote(HLC(mktdata)), maType='SMA'))
strat <- add.sigCrossovers(strat)
out<-try(applyStrategy(strat, strat$name, parameters=list(sd=2,n=20)) )
update.strat('bbands')
##### End bbands demo.
##########################################################################################################


#####
## Although it is recommended, it is not required to define the stock beforehand 
## (or in the call as we did above with stock("IBM",currency("USD"),src='yahoo')),
## The below would have worked by creating a basic instrument with multiplier=1 and type=NULL.
## it would make an 'instrument' and a portfolio called 'default'
#
# strat <- init.strat('IBM',initDate='2006-12-31') 
#####

#########################################################################################################
#### OR you can pass a df that contains a column for each pertinent argument.
#rm.strat()
strat <- init.strat('IBM', initDate='2010-01-01')
strat <- add.indicator(strat, name='BBands',arguments=list(HLC=quote(HLC(mktdata)),maType='SMA'))

sigrule.args <- data.frame(cbind(
    pcolumns=     c("Close","Close","High","Low"),
    icolumns=     c('dn','up','mavg','mavg'),
    relationships=c('lt','gt','gt','lt'),
    ruletypes=    c('enter','enter','exit','exit'),
    orderqty=     list(100,-100,'all','all'),
    ordertypes=   rep("market",4)
    ))

strat <- add.sigCrossovers(strat,df=sigrule.args)
out <- try(applyStrategy(strat, strat$name, parameters=list(sd=1.8,n=50)))
########################################################################################################


########################################################################################################
#make a break-out strategy with "PBands"
library(twsInstrument)
library(qmao)

rm.strat()
#rm(list=symb)
rm_futures()
future("VX",multiplier=1000,src='cfe',underlying_id=synthetic("VIX",currency("USD")))
symb <- future_series("VX_G07")
getSymbols(symb)
#start(VX_G07)
strat <- init.strat(symb,initDate='2006-04-01')
strat <- add.indicator(strat, name='PBands',arguments=list(prices=quote(Cl(mktdata)),maType='SMA'))
#breakout strategy. Buy when it crosses upper band exit at 'center'. 
#Sell at bottom band cross. exit at center
strat <- add.sigCrossovers(strat, 
            icolumns=c("up","dn","center","center"),
            relationships=c("gt", "lt", "lt", "gt"),
            orderqty=list(1,-1,'all','all'))
out<-try(applyStrategy(strat, strat$name, parameters=list(sd=2,n=20,fastn=2)) )
update.strat()
#########################################################################################################


##########################################################
# This is a proof-of-concept script.
# There is an uncommitted new version that does not require
# that all the lines begin with strat <- 
# but it requires a couple tweaks to quantstrat.
##
# Everything is subject to change including function and 
# argument names as well as usage, return values, 
# flexibility, and functionality.
##########################################################


