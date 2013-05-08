#' Chart to analyse walk.forward() objective function
#'
#' @param audit.filename name of .audit environment file as produced by walk.forward()
#'
#' @export

chart.forward.training <- function(audit.filename)
{
    if(!require(xtsExtra, quietly=TRUE)) stop('The "xtsExtra" package is required to use this function')

    .audit <- NULL

    load(audit.filename)

    # extract all portfolio names from the audit environment
    portfolios.st = ls(name=.audit, pattern='portfolio.*')
    n <- length(portfolios.st)

    # calculate Net.Trading.PL for each portfolio, one xts col per portfolio
    PL.xts <- xts()
    for(portfolio.st in portfolios.st)
    {
        p <- getPortfolio(portfolio.st, envir=.audit)

    	from <- index(p$summary[2])
    	
        #R <- cumsum(p$summary['2004-01-01/','Net.Trading.PL'])
        R <- cumsum(p$summary[paste(from, '/', sep=''),'Net.Trading.PL'])
        names(R) <- portfolio.st
        
        PL.xts <- cbind(PL.xts, R)
    }
    
    # add a column for the chosen portfolio, doubling it
    chosen.one <- .audit$param.combo.nr
    chosen.portfolio.st = ls(name=.audit, pattern=glob2rx(paste('portfolio', '*', chosen.one, sep='.')))

    R <- PL.xts[,chosen.portfolio.st]
    PL.xts <- cbind(PL.xts, R)
    
    PL.xts <- na.locf(PL.xts)

    # add drawdown columns for all portfolio columns
    CumMax <- cummax(PL.xts)
    Drawdowns.xts <- -(CumMax - PL.xts)
    data.to.plot <- as.xts(cbind(PL.xts, Drawdowns.xts))
    
    # now plot it
    dev.new()
    plot.xts(
        data.to.plot,
        screens=rep(1:2,each=n+1),
        col=c(rep('grey',n), 'blue'),
        minor.ticks=FALSE,
        main=NA
    )
    title(
        main='Walk Forward Analysis',
        sub=audit.filename
    )
    
    .audit <- NULL
}
