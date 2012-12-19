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

#' Rolling walk forward analysis
#' 
#' A wrapper for apply.paramset() and applyStrategy(), implementing a Rolling Walk Forward Analysis (WFA). It executes a strategy on a portfolio, while
#' rolling a re-optimization of one of the strategies parameter sets during a specified time period (training window), then selecting an optimal
#' parameter combination from the parameter set using an objective function, then applying the selected parameter combo to the next out-of-sample
#' time period immediately following the training window (testing window). Once completed,
#' the training window is shifted forward by a time period equal to the testing window size, and the process is repeated. 
#' WFA stops when there are insufficient data left for a full testing window. For a complete description, see Jaekle&Tomasini chapter 6.
#' 
#' @param portfolio.st the name of the portfolio object
#' @param strategy.st the name of the strategy object
#' @param paramset.label a label uniquely identifying within the strategy the paramset to be tested
#' @param period the period unit, as a character string, eg. 'days' or 'months'
#' @param k.training the number of periods to use for training, eg. '3' months
#' @param nsamples the number of sample param.combos to draw from the paramset for training; 0 means all samples (see also apply.paramset)
#' @param k.testing the number of periods to use for testing, eg. '1 month'
#' @param objective a user provided function returning the best param.combo from the paramset, based on training results; a default function is provided that returns the number of the param.combo that brings the highest Net.Trading.PL
#' @param verbose dumps a lot of info during the run if set to TRUE, defaults to FALSE
#'
#' @return a list consisting of a slot containing detailed results for each training + testing period, as well as the results of tradeStats() on the portfolio for the entire WFA over all testing periods
#'
#' @seealso applyStrategy apply.paramset endpoints tradeStats
#'
#' @examples
#' res <- walk.forward(strategy.st, paramset.label='SMA', portfolio.st=portfolio.st, on='months', k.training=3, k.testing=1, verbose=FALSE)
#'
#' @author Jan Humme
#'
#' @export

walk.forward <- function(portfolio.st, strategy.st, paramset.label, period, k.training, nsamples=0, k.testing, objective=max.Net.Trading.PL, verbose=FALSE)
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

		ep <- endpoints(symbol, on=period)

		k <- 1; while(TRUE)
		{
			result <- list()

			training.start <- ep[k] + 1
			training.end   <- ep[k + k.training]

			if(is.na(training.end))
				break

            training.timespan <- paste(index(symbol[training.start]), index(symbol[training.end]), sep='/')

			result$training.timespan <- training.timespan

			if(verbose) print(paste('=== training', paramset.label, 'on', training.timespan))

			result$apply.paramset <- apply.paramset(strategy.st=strategy.st, paramset.label=paramset.label,
                portfolio.st=portfolio.st, mktdata=symbol[training.timespan], nsamples=nsamples, verbose=verbose)

			if(missing(k.testing) || k.testing==0)
			{
                k <- k + 1
            }
            else
            {
				if(!is.function(objective))
					stop(paste(objective, 'unknown objective function', sep=': '))

				testing.start <- ep[k + k.training] + 1
				testing.end   <- ep[k + k.training + k.testing]

				if(is.na(testing.end))
					break

                testing.timespan <- paste(index(symbol[testing.start]), index(symbol[testing.end]), sep='/')

                tradeStats.list <- result$apply.paramset$tradeStats

				param.combo.nr <- do.call(objective, list('tradeStats.list'=tradeStats.list))
				param.combo <- tradeStats.list[param.combo.nr, 1:grep('Portfolio', names(tradeStats.list)) - 1]

				strategy <- quantstrat:::install.param.combo(strategy, param.combo, paramset.label)

                result$testing.timespan <- testing.timespan
				result$param.combo.nr <- param.combo.nr
				result$param.combo <- param.combo
				result$strategy <- strategy

				print(paste('--- testing param.combo', param.combo.nr, 'on', testing.timespan))

				applyStrategy(strategy, portfolios=portfolio.st, mktdata=symbol[testing.timespan])
			}
			results[[k]] <- result

			k <- k + k.testing
		}
	}
	updatePortf(portfolio.st, Dates=paste('::',as.Date(Sys.time()),sep=''))

	results$portfolio <- portfolio
	results$tradeStats <- tradeStats(portfolio.st)

	return(results)
} 
