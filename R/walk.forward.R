###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: parameters.R 1218 2012-10-11 20:47:44Z opentrades $
#
###############################################################################
#
# Authors: Jan Humme
#
###############################################################################

max.Net.Trading.PL <- function(tradeStats.list)
{
        which(max(tradeStats.list$Net.Trading.PL) == tradeStats.list$Net.Trading.PL)
}

### exported functions ############################################################

#' walk.forward
#' 
#' walk.forward
#' 
#' @param strategy the name of the strategy object
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#'
#' @author Jan Humme
#' @export

walk.forward <- function(portfolio.st, strategy.st, paramset.label, on, k.training, nsamples=0, k.testing, objective=max.Net.Trading.PL, verbose=FALSE)
{
warning('walk.forward() is still under development! expect changes in arguments and results at any time JH')

        must.have.args(match.call(), c('portfolio.st', 'strategy.st', 'paramset.label', 'k.training'))

        strategy <- must.be.strategy(strategy.st)
        must.be.paramset(strategy, paramset.label)

        portfolio <- getPortfolio(portfolio.st)

        results <- list()

        for(symbol.st in names(portfolio$symbols))
        {
                symbol <- get(symbol.st)

                ep <- endpoints(symbol, on)

                k <- 1; while(TRUE)
                {
                        result <- list()

                        training.start <- ep[k] + 1
                        training.end  <- ep[k + k.training]

                        if(is.na(training.end))
                                break

                        result$training <- list()
                        result$training$start <- index(symbol[training.start])
                        result$training$end <- index(symbol[training.end])

                        print(paste('=== training', paramset.label, 'on', paste(result$training$start, result$training$end, sep='/')))

                        result$training$results <- apply.paramset(strategy.st=strategy.st, paramset.label=paramset.label, portfolio.st=portfolio.st, mktdata=symbol[training.start:training.end], nsamples=nsamples, verbose=verbose)

                        if(!missing(k.testing) && k.testing>0)
                        {
                                if(!is.function(objective))
                                        stop(paste(objective, 'unknown objective function', sep=': '))

                                testing.start <- ep[k + k.training] + 1
                                testing.end   <- ep[k + k.training + k.testing]

                                if(is.na(testing.end))
                                        break

                                result$testing <- list()
                                result$testing$start <- index(symbol[testing.start])
                                result$testing$end <- index(symbol[testing.end])

                                tradeStats.list <- result$training$results$tradeStats
                                param.combo.nr <- do.call(objective, list('tradeStats.list'=tradeStats.list))
                                result$testing$param.combo.nr <- param.combo.nr

                                last.param.combo.column.nr <- grep('Portfolio', names(tradeStats.list)) - 1
                                param.combo <- tradeStats.list[param.combo.nr,1:last.param.combo.column.nr]
                                result$testing$param.combo <- param.combo

                                strategy <- quantstrat:::install.param.combo(strategy, param.combo, paramset.label)
                                result$testing$strategy <- param.combo

                                print(paste('--- testing param.combo', param.combo.nr, 'on', paste(result$testing$start, result$testing$end, sep='/')))

#browser()
                                applyStrategy(strategy, portfolios=portfolio.st, mktdata=symbol[testing.start:testing.end])
                        }
                        results[[k]] <- result

                        k <- k + 1
                }
        }
        updatePortf(portfolio.st, Dates=paste('::',as.Date(Sys.time()),sep=''))

        results$portfolio <- portfolio
        results$tradeStats <- tradeStats(portfolio.st)

        return(results)
} 
