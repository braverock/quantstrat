#' Chart to analyse walk.forward() objective function
#'
#' The \code{\link{walk.forward}} function creates an audit environment, written 
#' out as a results file, which contains the out of sample results of the (chosen)
#' parameter set.  This function parses that file and generates a chart to 
#' compare the performance of all parameter sets against the optimal one. 
#'
#' \code{\link{chart.forward.training}} uses the audit environment for a single 
#' in-sample training period to draw a chart for that in-sample data.
#' 
#' Note that parameter \code{audit filename} may also be an audit environment 
#' which is already loaded in \R, for ease of development and debugging.  Little
#' checking is done to ensure the correct structure of this environment, so passing
#' an unsuitable environment (such as the .blotter environment) will result in
#' errors.
#' 
#' @param audit.filename name of .audit environment file as produced by walk.forward().
#'        Filename will match pattern [audit.prefix].results.RData.
#' 
#' @seealso \code{\link{walk.forward}}, \code{\link{chart.forward.training}}
#' @export
chart.forward <- function(audit.filename)
{
  if(is.environment(audit.filename)){
    .audit <- audit.filename
  } else {
    .audit <- NULL  # keep codetools happy
    # ensure correct file written by walk.forward() is provided
    if (!grepl("\\.results\\.RData$", audit.filename[1L])) {
       stop("'audit.filename' should match pattern:\n  [audit.prefix].results.RData")
    }
    if (file.exists(audit.filename)) {
        load(audit.filename)
    } else {
        stop("'audit.filename', ", audit.filename, " not found.")
    }
  }

    # extract all portfolio names from the audit environment,
    # except result portfolio (which doesn't end with a digit)
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
    
    # now for the result portfolio (which doesn't end with a digit)
    portfolio.st <- ls(name=.audit, pattern='portfolio.*[^.0-9]$')
    p <- getPortfolio(portfolio.st, envir=.audit)
    from <- index(p$summary[2])
    R <- cumsum(p$summary[paste(from, '/', sep=''),'Net.Trading.PL'])
    names(R) <- portfolio.st

    # add a column for the chosen portfolio, doubling it and
    # making it plot last (first column, per PerfA convention) 
    # so it's not over-plotted by other portfolios
    PL.xts <- cbind(R, PL.xts)
    
    PL.xts <- na.locf(PL.xts)
    
    # add drawdown columns for all portfolio columns
    CumMax <- cummax(PL.xts)
    Drawdowns.xts <- -(CumMax - PL.xts)
    data.to.plot <- as.xts(cbind(PL.xts, Drawdowns.xts))

    p <- plot(PL.xts, col=c("blue", rep("grey", n )), main="Walk Forward Analysis")
    # set on=NA so it is drawn on a new panel
    p <- lines(Drawdowns.xts, col=c("blue", rep("grey", n )), on=NA, main="Drawdowns")
    print(p)
}
