#' Chart to analyse walk.forward() objective function
#'
#' @param audit.filename name of .audit environment file as produced by walk.forward()
#'
#' @export

chart.forward <- function(audit.filename)
{
    if(!require(xtsExtra, quietly=TRUE)) stop('The "xtsExtra" package is required to use this function')

    #.audit <- NULL
    if(is.null(.audit)) stop ('You need to run a walk forward test first to create the .audit environment')
  
    if(!is.null(audit.filename))
      load(audit.filename)
    else 
      stop('You need to provide an audit.filename.')

    # extract all portfolio names from the audit environment, except wfa portfolio
    portfolios.st = ls(name=.audit, pattern='portfolio.*.[0-9]+')
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
    
    # now for the result portfolio
    portfolio.st <- 'portfolio.forex'
    p <- getPortfolio(portfolio.st, envir=.audit)
    from <- index(p$summary[2])
    R <- cumsum(p$summary[paste(from, '/', sep=''),'Net.Trading.PL'])
    names(R) <- portfolio.st

    #PL.xts <- na.locf(PL.xts)
    PL.xts <- na.locf(cbind(PL.xts, R))

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
