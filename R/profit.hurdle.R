###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2017
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

#' Profit Hurdle function - A Minimum Profitability Method for Proposed Trading Strategies 
#' 
#' Based on their 2015 JPM paper "Backtesting", Campbell Harvey and Yan Liu (HL) propose
#' and demonstrate three methods of adjusting for potential multiple testing bias.
#' Using their model they propose haircuts for Sharpe ratios returned by trading strategies
#' (see \code{\link{haircutSharpe}}).
#' HL pose another way of viewing the problem is to ascertain a minimum average monthly
#' return for a given significance level. This is replicated in \code{quantstrat} with the
#'  \code{profitHurdle} function.
#'
#' @param portfolios string name of portfolio, or optionally a vector of portfolios, see DETAILS
#' @param ... any other passthrough parameters
#' @param strategy optional strategy specification that would contain more information on the process, default NULL
#' @param trials optional number of trials,default NULL
#' @param alpha_sig Significance level e.g. 0.05 ie. 5\%, default 0.05
#' @param vol_annual Annual return volatility e.g. 0.10 ie. 10\%, default 0.10
#' @param RHO Assumed average correlation, default 0.2
#' @param audit optional audit environment containing the results of parameter optimization or walk forward, default NULL
#' @param env optional environment to find market data in, if required
#'
#' @author Jasen Mackie, Brian G. Peterson
#' @references
#' Harvey, Campbell R. and Yan Liu. 2015. Backtesting The Journal of Portfolio Management. 41:1 pp. 13-28. 
#' 
#' Harvey, Campbell R., Yan Liu, and Heqing Zhu. 2016. "... and the cross-section of expected returns." The Review of Financial Studies 29, no. 1 (2016): 5-68.
#' 
#' Mackie, Jasen. 2016. R-view: Backtesting - Harvey & Liu (2015). https://opensourcequant.wordpress.com/2016/11/17/r-view-backtesting-harvey-liu-2015/
#' @return
#' 
#' an object of type \code{profitHurdle} containing:
#' 
#' \itemize{
#'    \item{Significance Level: the significance level used to determine the minimum required average monthly return}
#'    \item{Number of Monthly Observations: the number of monthly observations in the strategy, converted from one of either daily, weekly, monthly, quarterly or yearly}
#'    \item{Annualized Return Volatility: the annualized volatility of the strategy returns}
#'    \item{Assumed Number of Tests: the number of tests assumed and needed to account for in multiple testing adjustment}
#'    \item{Assumed Average Correlation: assumed average level of correlation among strategy returns}
#'    \item{Independent: unadjusted return hurdle, implying single test significance}
#'    \item{Bonferroni: the Bonferroni adjusted return hurdle for the given level of significance}
#'    \item{Holm: the Holm adjusted return hurdle for the given level of significance}
#'    \item{BHY: the BHY adjusted return hurdle for the given level of significance}
#'    \item{Average for Multiple Tests: the average multiple testing return hurdle for the given level of significance}
#' }
#'
#' @seealso \code{\link{SharpeRatio.haircut}}
#' @aliases profit.hurdle
#' @export profit.hurdle
#' @export profitHurdle
#' 
profitHurdle <- profit.hurdle <- function( portfolios
                                          , ...
                                          , strategy=NULL
                                          , trials=NULL
                                          , alpha_sig=0.05
                                          , vol_annual=0.1
                                          , RHO=0.2 # When not specified, use the assumption from HLZ (and the Cross-Section of Expected Returns) p.32-33 Section 5.3
                                          , audit=NULL
                                          , env=.GlobalEnv)
{
  #check inputs
  if(length(portfolios==1)&&is.null(audit)){
    stop("Not enough information to calculate.  \n",
         "Need either \n",
         "  - multiple portfolios \n",
         "  - single portfolio plus audit environment \n")
  }
  
  if(is.null(strategy)&&is.null(trials)){
    stop("Not enough information to calculate.  \n",
         "Need either \n",
         "  - strategy object with trials slot \n",
         "  - explicit number of trials \n")
  }
  
  #initialize things we'll need:
  if(!is.null(strategy)){
    if(!is.strategy((strategy))){
      s<-try(getStrategy(strategy))
      if(inherits(s,"try-error"))
        stop ("You must supply an object of type 'strategy'.")
    } else {
      s <- strategy
    }
    s_trials<-s$trials
    if(!is.null(trials) && s_trials>trials){
      trials <- s_trials
    }
    if(is.null(trials)) trials <- s_trials
  }
  if(trials==0 || !is.numeric(trials))
    stop("You must supply a numeric number of trials or a strategy with trials included")
  
  #loop over portfolios
  dailySt<-list()
  for(portfolio in portfolios){
    if(!is.null(audit)){
      if(!is.environment(audit)){
        stop("audit parameter should be an environment containing trial portfolios")
      } else{
        # run dailyStats on all (matching) portfolios if there
        pvec <- ls(pattern = paste0('portfolio.',portfolio),name = audit)
        if(length(pvec)){
          if(length(pvec)>trials) trials <- trials + length(pvec)
          # run dailyStats on all (matching) portfolios if there
          dailySt <- c(dailySt,lapply(pvec, function(x){ dailyStats(x,perSymbol = FALSE, method='moment', envir=audit) }))
          dailySt <- do.call(rbind,dailySt)
        }
        # put target portfolio first
        dailySt <- rbind(dailyStats(portfolios[1],perSymbol = FALSE, method='moment'),dailySt)
        rownames(dailySt) <- c(portfolios[1],pvec)
      }
    } else {
      #if we don't have an audit environment, we just need stats on all the portfolios
      dailySt <- c(dailySt,lapply(portfolios, function(x){ dailyStats(x,perSymbol = FALSE, method='moment') }))
      dailySt <- do.call(rbind,dailySt)
      rownames(dailySt) <- portfolios
    }
  }
  
  ## construct inputs for the internal fn haircutSR
  # 1. 'num_tests': No. of tests one allows for in multiple tests
  # num_tests <- 300
  num_tests <- trials
  
  # 2. 'num_obs': No. of monthly observations for a strategy
  portfolio <- portfolios[1] #target portfolio is the first one
  p <- .getPortfolio(portfolio) 
  freq <- periodicity(p$summary)$scale
  sm_fre <- switch (freq,
                    "daily" = 1,
                    "weekly" = 2,
                    "monthly" = 3,
                    "quarterly" = 4,
                    "yearly" = 5)
  
  ### Number of monthly observations 'N' ###
  # num_obs <- 240
  num_obs <- nrow(p$summary)
  if(sm_fre ==1){ 
    num_obs <- floor(num_obs*12/360)
  } else if(sm_fre ==2){
    num_obs <- floor(num_obs*12/52)
  } else if(sm_fre == 3){
    num_obs <- floor(num_obs*12/12)
  } else if(sm_fre == 4){
    num_obs <- floor(num_obs*12/4)
  } else if(sm_fre == 5){
    num_obs <- floor(num_obs*12/1)
  }
  
  # 3. 'alpha_sig': Significance level (e.g., 5#)
  alpha_sig <- alpha_sig
  
  # 4. 'vol_annual': Annual return volatility (e.g., 0.05)
  vol_annual <- vol_annual # TODO: get this metric from dailySt
  
  # 5. Average correlation assumed
  RHO <- RHO # use the assumption from HLZ (and the Cross-Section of Expected Returns) p.32-33 Section 5.3
  
  
  result <- .profitHurdle(sm_fre, num_tests, num_obs, alpha_sig, vol_annual, RHO)
  result$call <- match.call()
  result
} # end haircut Sharpe wrapper

### Required returns due to testing multiplicity ------ Harvey and Liu
### (2014): "Backtesting", Duke University 

# Profit_Hurdle <- function (num_tests, num_obs, alpha_sig, vol_annual, RHO){
#' internal implementation of profit hurdle code, see \code{\link{profit.hurdle}}
#'
#' @param sm_fre Sampling frequency; [1,2,3,4,5] = [Daily, Weekly, Monthly, Quarterly, Annual]
#' @param num_tests No. of tests one allows for in multiple tests
#' @param num_obs No. of monthly observations for a strategy
#' @param alpha_sig Significance level (e.g., 5#)
#' @param vol_annual Annual return volatility (e.g., 0.05 or 5#)
#' @param RHO Assumed average correlation, default 0.2
#'
#' @rdname dotprofitHurdle
.profitHurdle <- function(  sm_fre
                          , num_tests
                          , num_obs
                          , alpha_sig
                          , vol_annual
                          , RHO)
{
  
  ###############################
  ####### Parameter inputs ######
  
  ### 'num_tests': No. of tests one allows for in multiple tests;
  ### 'num_obs': No. of monthly observations for a strategy;
  ### 'alpha_sig': Significance level (e.g., 5#);
  ### 'vol_annual': Annual return volatility (e.g., 0.05 or 5#).
  
  NN <- num_tests
  
  Obs <- num_obs
  alpha0 <- alpha_sig
  vol_anu <- vol_annual
  
  ###Independent test ####
  #B_ind = norminv( (1- alpha0/2),0,1);
  B_ind <- qnorm( (1- alpha0/2),0,1)
  
  ###Bonferroni ####
  p0_mat <- alpha0/NN
  t0_mat <- qnorm( (1-p0_mat/2),0,1)
  BF <- t0_mat
  
  
  ###Input for Holm and BHY ####
  ###Parameter input from Harvey, Liu and Zhu (2014) #######
  para0 <- matrix(c(0, 1295, 3.9660*0.1, 5.4995*0.001,
                    0.2, 1377, 4.4589*0.1, 5.5508*0.001,
                    0.4, 1476, 4.8604*0.1, 5.5413*0.001,
                    0.6, 1773, 5.9902*0.1, 5.5512*0.001,
                    0.8, 3109, 8.3901*0.1, 5.5956*0.001),
                  nrow = 5, ncol = 4, byrow = TRUE)
  ### Interpolated parameter values based on user specified level of correlation RHO %%%%%%%%%%   
  if (RHO >= 0 & RHO < 0.2){ 
    para_inter <- ((0.2 - RHO)/0.2)*para0[1,] + ((RHO - 0)/0.2)*para0[2,]
  } else if (RHO >= 0.2 & RHO < 0.4) {
    para_inter <- ((0.4 - RHO)/0.2)*para0[2,] + ((RHO - 0.2)/0.2)*para0[3,]
  } else if (RHO >= 0.4 & RHO < 0.6){
    para_inter <- ((0.6 - RHO)/0.2)*para0[3,] + ((RHO - 0.4)/0.2)*para0[4,]
  } else if (RHO >= 0.6 & RHO < 0.8){
    para_inter <- ((0.8 - RHO)/0.2)*para0[4,] + ((RHO - 0.6)/0.2)*para0[5,]
  } else if (RHO >= 0.8 & RHO < 1.0){
    para_inter <- ((0.8 - RHO)/0.2)*para0[4,] + ((RHO - 0.6)/0.2)*para0[5,]
  } else {
    ### Default: para_vec = [0.2, 1377, 4.4589*0.1, 5.5508*0.001,M_simu]
    para_inter <- para0[2,] ### Set at the preferred level if RHO is misspecified 
  }
  
  WW <- 2000;  ### Number of repetitions 
  
  ### Generate a panel of t-ratios (WW*Nsim_tests) ###
  Nsim_tests <- (floor(NN/para_inter[2]) + 1)*floor(para_inter[2]+1); # make sure Nsim_test >= num_tests
  t_sample <- sample_random_multests(para_inter[1], Nsim_tests, para_inter[3], para_inter[4], WW)
  
  ### Holm #####   
  HL_mat <- NULL
  
  for(ww in 1:WW){ 
    
    yy <-  t_sample[ww, 1:NN] ### Use the ww'th row of t-sample  ###
    
    
    p_sub <- 2*(1-pnorm(yy))
    p_new <- sort(p_sub)
    
    KK <- length(p_new)
    comp_vec <- NULL
    
    for(kk in 1:KK){
      comp_vec[kk] <- alpha0/(KK + 1-kk)
    }
    
    comp_res <- p_new > comp_vec
    
    comp_new <- cumsum(as.numeric(comp_res))
    
    if(sum(comp_new) == 0){
      HL <- 1.96
    } else {
      p0 <- p_new[comp_new == 1]
      HL <- qnorm((1 - p0/2),0,1)
    }
    
    HL_mat <- append(HL_mat, HL)
  }
  
  ### BHY ####
  BHY_mat <- NULL
  
  for(ww in 1:WW){ 
    
    yy <-  t_sample[ww, 1:NN] ### Use the ww'th row of t-sample  ###
    
    p_sub <- 2*(1-pnorm(yy)) 
    
    if(length(p_new) <= 1){
      BH00 <- 1.96
    } else {
      p_new11 <- sort(p_sub, decreasing = TRUE)
      
      KK <- length(p_new11)
      comp_vec0 <- NULL
      cons_vec <- 1:KK
      cons_norm <- sum(1/cons_vec)
      
      for(kk in 1:KK){
        comp_vec0[kk] <- (alpha0*kk)/(KK*cons_norm)
      }
      
      comp_vec <- sort(comp_vec0, decreasing = TRUE)
      
      comp_res11 <- as.numeric(p_new11 <= comp_vec)
      
      if(sum(comp_res11) == 0){
        BH00 <- 1.96;
      } else {
        p0 <- p_new11[comp_res11 ==1]
        
        b0 <- which(abs(p_new11 - p0[1]) == min(abs(p_new11 - p0[1])))
        
        if(b0 == 1){
          p1 <- p0[1]
        } else {
          p1 <- p_new11[(b0-1)]
        }
        
        BH00 <- qnorm((1 - (p0[1]+p1)/4),0,1)
      }
    }
    
    BHY_mat <- append(BHY_mat,BH00)
  }
  
  tcut_vec <- c(B_ind, BF, median(HL_mat), median(BHY_mat))
  
  ret_hur <- ((vol_anu/sqrt(12))/sqrt(Obs))*tcut_vec
  
  # structure and return
  result<-list()
  result$Independent <- round(ret_hur[1]*100,3)
  result$Bonferroni <- round(ret_hur[2]*100,3)
  result$Holm <- round(ret_hur[3]*100,3)
  result$BHY <- round(ret_hur[4]*100,3)
  result$Average <- round(mean(ret_hur[-1])*100,3)
  
  result$alpha_sig  <- alpha_sig
  result$num_obs <- num_obs
  result$vol_anu <- vol_anu
  result$num_tests <- num_tests
  result$RHO <- RHO
  result$inner_call <- match.call()
  
  return(structure(result, class='profitHurdle'))
  
}

#' print method for Harvey and Liu Haircut Sharpe Ratio
#'
#' @param x an object of type \code{profitHurdle} to be printed 
#' @param ... any other passthough parameters
#'
#' @seealso \code{\link{profit.hurdle}}
#' @method print profitHurdle
#' @export
print.profitHurdle <- function(x, ...){
  cat('\n',
      'Profit Hurdle Report: \n\n',
      'Significance Level = ', x$alpha_sig*100,'%','\n',
      'Number of Monthly Observations = ', x$num_obs,'\n',
      'Annualized Return Volatility = ', x$vol_anu*100,'%','\n',
      'Assumed Number of Tests = ', x$num_tests,'\n',
      'Assumed Average Correlation = ', x$RHO,'\n\n',
      'Minimum Average Monthly Return: \n\n',
      'Independent = ', x$Independent,'%','\n',
      'Bonferroni = ', x$Bonferroni,'%','\n',
      'Holm = ', x$Holm,'%','\n',
      'BHY = ', x$BHY,'%','\n',
      'Average for Multiple Tests = ', x$Average,'%','\n',sep = "")
  
  invisible(x)  
}