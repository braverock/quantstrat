#' Chart to analyse walk.forward() objective function
#'
#' The \code{\link{walk.forward}} function creates an audit environment,  potentially written 
#' out as a results file, which contains the out of sample results of the (chosen)
#' parameter set.  This function parses that file  or environment and generates a chart to 
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
#' @param audit.filename name of .audit environment file as produced by \code{\link{walk.forward}}
#'        Filename will often match pattern [audit.prefix].results.RData. Alternately, an
#'        audit environment provided by the output of \code{\link{walk.forward}}
#' @param portfolio.st string defining which portfolio should be used for out of sample, default NULL
#' @seealso \code{\link{walk.forward}}, \code{\link{chart.forward.training}}
#' @export
chart.forward <- function(audit.filename, portfolio.st=NULL)
{
  if(is.environment(audit.filename)){
    .audit <- audit.filename
  } else {
    # .audit <- NULL  # keep codetools happy
    # ensure correct file written by walk.forward() is provided
    if (!grepl("\\.Results\\.RData$", audit.filename[1L])) {
       stop("'audit.filename' should match pattern:\n  [audit.prefix].results.RData")
    }
    if (file.exists(audit.filename)) {
        load(audit.filename)
      } else {
        stop("'audit.filename', ", audit.filename, " not found.")
        }
    if(is.environment(results)) {
      .audit <- get("results")  # potentially Jasen's dodgy fix, means results must always be the name of the returned environment
      } else {
        stop("no environment named 'results' when calling load(audit.filename)")
      }
  }

  if(length(ls(name=.audit,pattern='blotter'))){ 
    # post 0.12.0 audit environment
    
    #get performance from OOS result portfolio (which doesn't end in a digit)
    if(is.null(portfolio.st) && !is.null(.audit$portfolio.st)){
      portfolio.st <- .audit$portfolio.st
    } else {
      portfolio.st <- ls(name=.audit$blotter, pattern='portfolio.*[^.0-9]$')
    }
    
    if(length(portfolio.st)>1){ 
      stop('Returned vector of portfolio names, please select one from',portfolio.st)
    } 
    
    R <- cumsum(getPortfolio(paste0('test.',portfolio.st), envir = .audit$blotter)[['summary']][,'Net.Trading.PL']) 
    R <- R[-1,]
    names(R) <- portfolio.st
    
    #get the IS paramset cumPL from the environment
    # PL.xts <- audit.filename$insample.apply.paramset$cumPL
    PL.xts <- .audit$insample.apply.paramset$cumPL # potentially Jasen's dodgy fix, although it feels right to be calling .audit as opposed to audit.filename, since we passing audit.filename to .audit higher up when passing an env argument, or my new code assigning get("results") to .audit when passing filename to chart.forward()
    PL.xts <- PL.xts[-1,]
    
    
    n  <- ncol(PL.xts)
    el <- .audit$wf.subsets$testing.start
    el <- xts(as.character(el),order.by=el)
    
    # adjust to the mean of the training sets, which have a head start
    pstart <- mean(PL.xts[first(index(el))])
    R  <- R+pstart # adjust in-sample
    
  } else {
    # we have an old style audit environment
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
    p <- getPortfolio(paste0('test.',portfolio.st), envir=.audit$blotter)
    from <- index(p$summary[2])
    R <- cumsum(p$summary[paste(from, '/', sep=''),'Net.Trading.PL'])
    names(R) <- portfolio.st
    el<-NULL
  } # end if/else over audit environment
  
    # add a column for the chosen portfolio, doubling it and
    # making it plot last (first column, per PerfA convention) 
    # so it's not over-plotted by other portfolios
    PL.xts <- cbind(R, PL.xts)
    
    PL.xts <- na.locf(PL.xts)
    
    units <- periodicity(R)$units

    units <- switch(units,
                    seconds = 'hours',
                    minutes = 'hours',
                    hours = 'days',
                    days = 'months',
                    weeks = 'years',
                    months = 'years',
                    quarters = 'years',
                    years = 'years')
    
    if(units=='months' && nrow(R)>500) units <- 'years'
    
    # add drawdown columns for all portfolio columns
    CumMax <- cummax(PL.xts)
    Drawdowns.xts <- -(CumMax - PL.xts)

    p <- plot(PL.xts, 
              col=c("blue", rep("grey", n )),
              grid.ticks.on=units,
              minor.ticks=NULL,
              main="Walk Forward Analysis")
    # set on=NA so it is drawn on a new panel
    p <- lines(Drawdowns.xts, col=c("blue", rep("grey", n )), on=NA, main="Drawdowns")
    if(!is.null(el)){
      p <- addEventLines(el, offset=.4, pos=2, srt=90, on=1)
    }
    print(p)
}
