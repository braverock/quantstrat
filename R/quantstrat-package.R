#' Quantitative Strategy Model Framework
#' 
#' Transaction-oriented infrastructure for constructing trading systems and
#' simulation.  Provides support for multi-asset class and multi-currency
#' portfolios for backtesting and other financial research.  Still in heavy
#' development.
#' 
#' quantstrat provides a generic infrastructure to model and backtest
#' signal-based quantitative strategies.  It is a high-level abstraction layer
#' (built on xts, FinancialInstrument, blotter, etc.) that allows you to build
#' and test strategies in very few lines of code.  quantstrat is still under
#' heavy development but is being used every day on real portfolios.  We
#' encourage you to send contributions and test cases to the project forums.
#' 
#' \emph{Generic Signal-Based Strategy Modeling}
#' 
#' A signal-based strategy model first generates indicators.  Indicators are
#' quantitative values derived from market data (e.g. moving averages, RSI,
#' volatility bands, channels, momentum, etc.).  Indicators should be applied
#' to market data in a vectorized (for fast backtesting) or streaming (for live
#' execution) fashion, and are assumed to be path-independent (i.e. they do not
#' depend on account / portfolio characteristics, current positions, or
#' trades).
#' 
#' The interaction between indicators and market data are used to generate
#' signals (e.g. crossovers, thresholds, multiples, etc.).  These signals are
#' points in time at which you may want to take some action, even though you
#' may not be able to.  Like indicators, signals may be applied in a vectorized
#' or streaming fashion, and are assumed to be path-independent.
#' 
#' Rules use market data, indicators, signals, and current account / portfolio
#' characteristics to generate orders.  Notice that rules about position
#' sizing, fill simulation, order generation / management, etc. are separate
#' from the indicator and signal generation process.  Unlike indicators and
#' signals, rules are generally evaluated in a path-dependent fashion
#' (path-independent rules are supported but are rare in real life) and are
#' aware of all prior market data and current positions at the time of
#' evaluation.  Rules may either generate new or modify existing orders (e.g.
#' risk management, fill, rebalance, entry, exit).
#' 
#' \emph{How quantstrat Models Strategies}
#' 
#' quantstrat uses FinancialInstrument to specify instruments (including their
#' currencies) and uses blotter to keep track of transactions, valuations, and
#' P&amp;amp;L across portfolios and accounts.
#' 
#' Indicators are often standard technical analysis functions like those found
#' in TTR; and signals are often specified by the quantstrat sig* functions
#' (i.e. sigComparison, sigCrossover, sigFormula, sigPeak, sigThreshold).
#' Rules are typically specified with the quantstrat ruleSignal function.
#' 
#' The functions used to specify indicators, signals, and rules are not limited
#' to those mentioned previously.  The name parameter to add.indicator,
#' add.signal, and add.rule can be any R function.  Because the supporting
#' toolchain is built using xts objects, custom functions will integrate most
#' easily if they return xts objects.
#' 
#' The strategy model is created in layers and makes use of delayed execution.
#' This means strategies can be applied--unmodified--to several different
#' portfolios.  Before execution, quantstrat strategy objects do not know what
#' instruments they will be applied to or what parameters will be passed to
#' them.
#' 
#' For example, indicator parameters such as moving average periods or
#' thresholds are likely to affect strategy performance.  Default values for
#' parameters may (optionally) be set in the strategy object, or set at
#' call-time via the parameters argument of applyStrategy (parameters is a
#' named list, used like the arguments lists).
#' 
#' quantstrat models orders, which may or may not become transactions.  This
#' provides a lot of extra ability to evaluate how the strategy is actually
#' working, not working, or could be improved.  For example, the performance of
#' strategies are often affected by how often resting limit orders are changed
#' / replaced / canceled.  An order book allows the quantitative strategist to
#' examine market conditions at the time these decisions are made. Also, the
#' order history allows for easy computation of things that are important for
#' many strategies, like order-to-fill ratios.
#' 
#' \emph{Argument Matching}
#' 
#' Many places in quantstrat apply arguments passed in the strategy
#' specification, the parameters list, or in \code{...} to an indicator,
#' signal, or rule function.  These arguments are matched in this order, with
#' the last math overriding. Specifically, this order is:
#' 
#' \enumerate{ \item the \code{arguments=list(...)} assigned to each indicator,
#' signal, or rule \item the \code{parameters=list{...}} applied when
#' \code{\link{applyStrategy}} is called \item any additional arguments passed
#' in \code{...} in the call to \code{applyStrategy} }
#' 
#' @name quantstrat-package
#' @aliases quantstrat-package quantstrat
#' @docType package
#' @author primary authors: Peter Carl, Brian G. Peterson, Joshua Ulrich, Jasen Mackie, Jan Humme 
#' 
#' Maintainer: Brian G. Peterson <brian@@braverock.com>
#' @seealso \code{\link[quantmod:quantmod-package]{quantmod}},
#' \code{\link[blotter:blotter-package]{blotter}},
#' \code{\link[FinancialInstrument:FinancialInstrument-package]{FinancialInstrument}},
#' \code{\link[blotter:blotter-package]{blotter}},
#' \code{\link[PerformanceAnalytics:PerformanceAnalytics-package]{PerformanceAnalytics}}
#' @keywords package
# @examples
#' 
#' @import blotter FinancialInstrument foreach methods quantmod xts zoo
#' @importFrom grDevices dev.new heat.colors
#' @importFrom graphics abline boxplot lines par plot
#' @importFrom methods hasArg
#' @importFrom stats as.formula coef end lm na.omit start time acf median pnorm pt qnorm qt rexp runif toeplitz var
#' @importFrom utils glob2rx installed.packages head globalVariables
NULL

#' sample spx daily OHLCVA data set 1970:1971
#' @name spx
#' @docType data 
#' @keywords data
NULL

#' sample portfolio output from running luxor demo in file data/luxor-p066.RData
#' @name portfolio.luxor
#' @docType data 
#' @keywords data
NULL

#' sample tradeStats output from running luxor demo in file data/luxor.parameters.1-10.30-55.RData
#' @name stats
#' @docType data 
#' @keywords data
NULL

#' sample audit environment output from running luxor demo in file data/luxor.wfa.ples.RData
#' @name luxoraudit
#' @rdname luxoraudit
#' @docType data 
#' @keywords data
NULL

# variables that will always be available if needed at run-time, 
# but need to be available at compile time for 
# R CMD check
utils::globalVariables(names=c('beanplot','Change','env','gamlss','hPlot',
                               'initDate','initEq','melt','mktdata','param.combo',
                               'Period','plotSimpleGamlss','price','redisClose',
                               'redisConnect','redisGetContext','timespan'))
