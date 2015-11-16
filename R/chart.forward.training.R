#' Chart to analyse walk.forward() objective function
#'
#' @param audit.filename name of .audit environment file as produced by walk.forward().
#'        Filename will match pattern [audit.prefix].[symbol].[start timestamp].[end timestamp].RData.
#'
#' @export

chart.forward.training <- function(audit.filename)
{
    .audit <- NULL  # keep codetools happy
    # ensure correct file written by walk.forward() is provided
    datePattern <- "[[:digit:]]{4}.[[:digit:]]{2}.[[:digit:]]{2}(.[[:digit:]]{2}.[[:digit:]]{2}.[[:digit:]]{2})?"
    if (!grepl(paste0(datePattern, "\\.", datePattern, "\\.RData$"), audit.filename[1L])) {
        stop("'audit.filename' should match pattern:\n  [audit.prefix].[symbol].[start timestamp].[end timestamp].RData.")
    }
    if (file.exists(audit.filename)) {
        load(audit.filename)
    } else {
        stop("'audit.filename', ", audit.filename, " not found.")
    }

    # extract all portfolio names from the audit environment
    # NB: training data only has portfolios that end in digits
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
    
    # .audit$param.combo.nr contains the rowname of the best portfolio
    chosen.one <- .audit$param.combo.nr[1L]
    chosen.portfolio.st = ls(name=.audit, pattern=glob2rx(paste('portfolio', '*', chosen.one, sep='.')))
    # add a column for the chosen portfolio, doubling it and
    # making it plot last, so it's not over-plotted by other portfolios
    R <- PL.xts[,chosen.portfolio.st]
    PL.xts <- cbind(PL.xts, R)
    
    PL.xts <- na.locf(PL.xts)

    # add drawdown columns for all portfolio columns
    CumMax <- cummax(PL.xts)
    Drawdowns.xts <- -(CumMax - PL.xts)
    data.to.plot <- as.xts(cbind(PL.xts, Drawdowns.xts))

    # based on the suggestion by Ross, note that the number of
    # lines is increased by 1 since the 'chosen' portfolio is added as the last one
    # and highlighted using the blue color
    p <- plot(PL.xts, col=c("blue", rep("grey", n)), main="Walk Forward Analysis")
    # set on=NA so it is drawn on a new panel
    p <- lines(Drawdowns.xts, col=c("blue", rep("grey", n)), on=NA, main="Drawdowns")
    print(p)
}
