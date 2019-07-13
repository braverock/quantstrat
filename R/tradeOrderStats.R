#' get order information associated with closing positions
#' 
#' Combine perTradeStats output with closed order information.
#'
#' TODO: decide which of these columns are actually important
#' TODO: add option for opening order/trade pairing rather than closing
#' 
#' @param portfolio text name of the portfolio the order book is associated with
#' @param symbol text string defining the symbol to get trades and orders for
#' @param ... any other passthrough parameters
#' @return 
#' A \code{xts} object containing:
#' 
#' \describe{
#'      \item{Order.Qty}{}
#' 		\item{Order.Price}{}
#' 		\item{Order.Type}{}
#' 		\item{Order.Side}{}
#' 		\item{Order.Threshold}{}
#' 		\item{Order.Status}{should be 'closed' only}
#' 		\item{Order.StatusTime}{time of the closing trade, should match 'End' column}
#' 		\item{Prefer}{prefer argument for \code{getPrice}}
#' 		\item{Order.Set}{order set of the closing trade}
#' 		\item{Txn.Fees}{and fees applied to the closing trade}
#' 		\item{Rule}{the name of the rule that generated the order which closed the position}
#'      \item{Start}{the \code{POSIXct} timestamp of the start of the trade}
#'      \item{Init.Pos}{the initial position on opening the trade}
#'      \item{Max.Pos}{the maximum (largest) position held during the open trade}
#'      \item{Num.Txns}{ the number of transactions included in this trade}
#'      \item{Max.Notional.Cost}{ the largest notional investment cost of this trade}
#'      \item{Net.Trading.PL}{ net trading P&L in the currency of \code{Symbol}}
#'      \item{MAE}{ Maximum Adverse Excursion (MAE), in the currency of \code{Symbol}}
#'      \item{MFE}{ Maximum Favorable Excursion (MFE), in the currency of \code{Symbol}}
#'      \item{Pct.Net.Trading.PL}{ net trading P&L in percent of invested \code{Symbol} price gained or lost}
#'      \item{Pct.MAE}{ Maximum Adverse Excursion (MAE), in percent}
#'      \item{Pct.MFE}{ Maximum Favorable Excursion (MFE), in percent}
#'      \item{tick.Net.Trading.PL}{  net trading P&L in ticks}
#'      \item{tick.MAE}{ Maximum Adverse Excursion (MAE) in ticks}
#'      \item{tick.MFE}{ Maximum Favorable Excursion (MFE) in ticks} 
#' }
#' @export
tradeOrderStats <- function(portfolio, symbol, ...) {
	stats.table <- perTradeStats(Portfolio=portfolio,Symbol=symbol,...)
	stats.xts <- as.xts(stats.table, order.by=stats.table$End)
	orderbook <- getOrderBook(portfolio=portfolio)[[portfolio]][[symbol]]
	closed <- orderbook[which(orderbook$Order.Status=='closed'),]
	closed.xts <- xts(closed, as.POSIXct(as.character(closed$Order.StatusTime), tz=tzone(closed), format="%Y-%m-%d %H:%M:%OS"))
	merged.table <- merge(closed.xts,stats.xts)
	merged.closed <- merged.table[index(stats.xts)]
	return(merged.closed)
}


###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2015
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, 
# Jeffrey Ryan, Joshua Ulrich, and Garrett See 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: ruleOrderProc.R 1435 2013-04-16 15:23:34Z bodanker $
#
###############################################################################
