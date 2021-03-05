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

#' Haircut Sharpe Ratio to correct for number of trials and autocorrelation 
#'
#' In their 2015 JPM paper "Backtesting", Campbell Harvey and Yan Liu (HL) discuss
#' the common practice of adjusting or 'haircutting' a set of backtest results
#' to correct for assumed overfitting.  In the industry, this haircut is often 
#' assumed to be 50\% of reported performance.  They propose and demonstrate
#' three methods of adjusting for potential multiple testing bias by adapting 
#' three methods for adjusting confidence on multiple trials from the statstical
#' literature.
#' 
#' In multiple hypothesis testing the challenge is to guard against false 
#' discoveries. HL argue that the appropriate "haircut Sharpe ratio" is
#' non-linear, in that the highest Sharpe ratios (SR's) are only moderately
#' penalized whilst marginal SR's more so. The implication is that high SR's
#' are more likely true discoveries in a multiple hypothesis testing framework.
#' 
#' HL mention 5 caveats to their framework, namely;
#'
#' \itemize{
#'  \item{Sharpe ratios may not be appropriate metrics for strategies with negatively skewed expected payoffs, such as option strategies.}
#'  \item{Sharpe ratios normalize returns based on their volatility (ie. market risk), which may not be the most appropriate reflection of risk for a strategy.}
#'  \item{Determining the appropriate significance level for multiple testing (where in single tests 5\% is the normal cutoff).}
#'  \item{Which multiple testing method you choose could yield different conclusions. HL proposes 3 methods together with an average.}     
#'  \item{The number of trials used to adjust for multiple tests}
#' }
#'  
#' 
#' @section Linking Sharpe ratio with t-statistic:
#' 
#' To explain the link between the Sharpe ratio and the t-stat and the application 
#' of a multiple testing p-value adjustment, HL use the simplest case of an 
#' individual investment strategy. Assume a null hypothesis in which the 
#' strategy's mean return is significantly different from zero, therefore 
#' implying a 2-sided alternative hypothesis. A strategy can be regarded as 
#' profitable if its mean returns are either side of zero since investors can 
#' generally go long or short. Since returns will at least be asymptotically 
#' normally distributed (thanks to the Central Limit Theorem) a t-statistic 
#' following a t-distribution can be constructed and tested for significance. 
#' Due to the link between the Sharpe ratio and the t-stat it is possible to 
#' assess the significance of a strategy's excess returns directly using the 
#' Sharpe ratio. Assume \eqn{\hat{\mu}}{mu} denotes the mean of your sample of 
#' historical returns (daily or weekly etc) and \eqn{\hat{\sigma}}{sigma} denotes 
#' standard deviation, then:
#' 
#' \deqn{t-statistic = \frac{ \hat{\mu}}{(\frac{\hat{\sigma}}{\sqrt{T})}}}{tstatistic = mu/(sigma/sqrt(T))}
#' 
#' where \eqn{T-1} is degrees of freedom and since
#' 
#' \deqn{\widehat{SR} =  \frac{\hat{\mu}}{\hat{\sigma}}}{SR = mu/sigma}
#' 
#' it may be shown that
#' 
#' \deqn{\widehat{SR} = \frac{t-statistic}{\sqrt{T}}}{SR = tstatistic/sqrt(T)}
#' 
#' By implication a higher Sharpe ratio equates to a higher t-ratio, implying a 
#' higher significance level (lower p-value) for an investment strategy. If we 
#' denote p-value of the single test as \eqn{p^s} then we can present the p-value
#' for a single test as:
#' 
#' \deqn{{p^s} = Pr( |r| > t-ratio)}{p^s = Pr(|r|>t-ratio)}
#' 
#' or
#' 
#' \deqn{{p^s} = Pr( |r| > \widehat{SR}.\sqrt{T}}{p^s = Pr( |r| > SR * sqrt{T}}
#' 
#' If the researcher was exploring a particular economic theory then this p-value 
#' might make sense, but what if the researcher has tested multiple strategies 
#' and presents only the most profitable one? In this case the p-value of the 
#' single test may severely overstate the actual significance. A more truthful 
#' p-value would be an adjusted multiple testing p-value which assuming we denote 
#' as \eqn{p^m} which could be represented as:
#' 
#' \deqn{{p^m} = Pr( max{|{r_i}|, i = 1,...,N} > t-ratio)}{p^m = Pr( max(|r_i|, i = 1,...,N) > t-ratio)}
#' 
#' or
#' 
#' \deqn{ {p^m} = 1 - (1 - {p^s}{)^N} }{ p^m = 1 - (1 -p^s)^N }
#' 
#' By equating the p-value of a single test to a multiple test p-value we get 
#' the defining equation of \eqn{p^m} which is
#' 
#' \deqn{  {p^m} = Pr( {|{r_i}|} > \widehat{SR}.\sqrt{T})  }{ p^m = Pr(|r_i| > SR * sqrt(T))  }
#' 
#' where
#' 
#' \deqn{ p^m = 1-(1-{p^s}{)^N} }{p^m = 1 - (1-p^s)^N }
#' 
#' @section Multiple Testing Methods:
#' 
#' This function replicates the methods proposed by Harvey and Liu to adjust
#' an observed Sharpe Ratio for the number of trials performed, the
#' autocorrelation between the trials, the overall level of performance, and 
#' the presumed or observed correlation between trials.
#'
#' We will refer to these methods as:
#' 
#' 1. Bonferroni (BON)
#' 
#' 2. Holm
#'
#' 3. Benjamini, Hochberg and Yekutieli (BHY)
#' 
#' Full details on the calculations and adjustments should be found in 
#' Harvey and Liu (2015).  This documentation is just an overview to aid in 
#' easy use of the \R function.
#' 
#' HL mention 3 well known adjustment methods in the statistics literature, which
#' are originally prescribed in the paper "...and the Cross-Section of Expected
#' Returns" by Harvey, Liu and Zhu. These are Bonferroni, Holm, and Benjamini,
#' Hochberg, and Yekutieli (BHY).
#' 
#' 1. Bonferroni (BON)
#' 
#' \deqn{{p_{(i)}}^Bonferroni = min {|{M * p_i, 1}|}}{p_i^Bonferroni=min(|M*p_1,1|)}
#' 
#' Bonferroni applies the same adjustment to the p-value of each test, inflating
#' the p-value by the number of tests. The multiple testing p-value is the minimum
#' of each inflated p-value and 1 where 1 (or 100\% if you prefer) is the upper bound
#' of probability. HL use the example of p-values from 6 strategies where the p-values
#' are (0.005, 0.009, 0.0128, 0.0135, 0.045, 0.06). According to a 5\% significance
#' cutoff the first 5 tests would be considered significant. Using the p.adjust function
#' in R we can get the multiple adjusted p-values and according to Bonferroni only the
#' first test would be considered significant.
#' 
#' 2. Holm
#' 
#' p-value adjustments can be categorized into 2 categories, namely: single-step and
#' sequential. Single-step corrections equally adjust p-values as in Bonferroni.
#' Sequential adjustments are an adaptive procedure based on the distribution of p-values.
#' Sequential methods gained prominence after a seminal paper by Schweder & Spjotvoll (1982)
#' and section 7.3 of this paper gives a useful example of an application of multiple
#' testing hypothesis diagnostic plotting in R. Holm is an example of a sequential
#' multiple testing procedure. For Holm, the equivalent adjusted p-value is
#' 
#' \deqn{{p_{(i)}}^Holm = min[max((M - j + 1)*{p_{(j)}} ),1]}{p_i^Holm=min[max((M-j+1)*p_j),1]}
#' 
#' Bonferroni adjusts single tests equally, whereas Holm applies a sequential approach.
#' By conclusion it should not surprise you that adjusted Sharpe ratios under Bonferroni
#' will therefore be lower than for Holm. At this point it is useful to mention that both
#' Holm and Bonferroni attempt to prevent even 1 Type I error occurring, controlling what
#' is called the family-wise error rate (FWER). The next adjustment proposed by HL is BHY
#' and the main difference from the previous 2 adjustment methods is that BHY attempts to
#' control the false discovery rate (FDR), implying more lenience than Holm and Bonferroni
#' and therefore expected to yield higher adjusted Sharpe ratios.
#' 
#' 3. BHY
#' 
#' BHY's formulation of the FDR can be represented as follows. First all p-values are
#' sorted in descending order and the adjusted p-value sequence is defined by pairwise
#' comparisons.
#' 
#' TODO: BHY equation
#' 
#' We expect BHY to be more lenient as it controls the false discovery rate
#' whereas Holm and Bonferroni control the family-wise error rate, trying to
#' eliminate making even 1 false discovery. Bonferroni is more stringent than
#' Holm since it is a single-step adjustment versus the sequential approach of
#' Holm. With these 3 methods HL attempt to adjust p-values to account for
#' multiple testing and then convert these to haircut Sharpe ratios and in so
#' doing control for data mining. Both Holm and BHY require the empirical
#' distribution of p-values from previously tried strategies.
#' 
#' \strong{Empirical Study}
#' 
#' Harvey, Liu and Zhu (2016, HLZ) provides a large study of multiple testing 
#' bias by examining market anomalies or risk factors previously published in
#' major peer reviewed journals.  In constructing such a large study, they needed 
#' to correct for multiple potential issues in the analysis, including lack of 
#' complete data on all trials, lack of the number of failed trials, correlation
#' among the published trials, and data snooping or look ahead bias as later 
#' researchers learned features of the data from prior studies.   
#' 
#' HLZ model over 300 risk factors documented in the finance literature.
#' However, using this model for the distribution of p-values is not complete
#' since many tried strategies would not have been documented (referred to as
#' Publication Bias) plus they are potentially correlated thereby violating the
#' requirement for independence between tests. HLZ propose a new distribution to
#' overcome these shortfalls.
#' 
#' HLZ publish the list of resources they studied, over 300 factors for
#' explaining the cross section of return patterns. See
#' http://faculty.fuqua.duke.edu/~charvey/Factor-List.xlsx. There is a clear
#' pattern of increasing factor discovery with each decade (HLZ, Figure 2:
#' Factors and Publications, p.20). Assuming statistical and economic soundness
#' of published t-statistics, HLZ conduct the 3 multiple testing procedures
#' described earlier. Their conclusion, assuming all tried factors are published
#' is that an appropriate minimum threshold t-statistic for 5\% significance is
#' 2.8. This equates to a p-value of only 0.50\% for single tests. Of course the
#' assumption that all tried factors are published is not reasonable, and
#' therefore the analysis does suggest a minimum threshold for accepting the
#' significance of future tests, ie. less than or equal to 0.50\%.
#' 
#' HLZ limit their sample of factors to unique factors thereby minimizing test
#' dependence which is a requirement for the 3 multiple testing procedures they
#' propose. Since we know the requirements for being published are fairly
#' stringent, HLZ estimate that 71\% of tried tests are not published. See 
#' appendix B of HLZ for details. Using this number of tested factors together
#' with the 3 multiple testing procedures they propose a benchmark t-statistic
#' of 3.18. This required threshold is intuitively larger than the 2.8 threshold
#' generated assuming a lower number of tests.
#' 
#' Acknowledging the inevitable presence of test dependence and correlation 
#' among published test statistics (think of the many price multiple factors for
#' instance) HLZ propose a "direct modeling approach" in which only t-statistics
#' are required to account for this correlation. Correction for correlation in 
#' multiple testing procedures has only recently been documented in the 
#' statistics literature, and methods typically include simulating the entire 
#' time series to construct an empirical distribution for the range of test 
#' statistics (see e.g. \code{\link[blotter]{mcsim}} and
#' \code{\link[blotter]{txnsim}}). Of course the luxury of access to the 
#' entire dataset is not generally available to the risk factor researcher or
#' potential investor being presented with a backtest, so HLZ propose a
#' "Truncated Exponential Distribution" for modelling the t-statistic sample of
#' published and unpublished results. The intuitive reasoning for a
#' monotonically decreasing exponential distribution for modelling t-statistics
#' is that finding factors with small t-statistics should be easier than larger
#' ones.
#' 
#' HLZ conclude that threshold cutoffs are increasing through time, imposing
#' higher scrutiny to data mining today than to data mining in the past. Their
#' justification is summarized by 3 reasons:
#' 
#' 1. The easily discovered factors have already been discovered.
#' 
#' 2. In Finance there is a limited amount of data, compared with particle
#' physics for example where an experiment can create trillions of new
#' observaions.
#' 
#' 3. The relative costs of data mining in the past were much higher than they
#' are today, implying the more economically sound principle factors were likely
#' to be tested earlier.
#' 
#' 
#' @param portfolios string name of portfolio, or optionally a vector of portfolios, see DETAILS
#' @param ... any other passtrhrough parameters
#' @param strategy optional strategy specification that would contain more information on the process, default NULL
#' @param trials optional number of trials,default NULL
#' @param audit optional audit environment containing the results of parameter optimization or walk forward, default NULL
#' @param env optional environment to find market data in, if required.
#'
#' @author Jasen Mackie, Brian G. Peterson
#' @return 
#' 
#' an object of type \code{haircutSR} containing:
#' 
#' \describe{
#'    \item{Bonferroni}{a \code{data.frame} containing slots \code{haircut_SR},\code{adj_pvalue},and \code{pct_adj}}
#'    \item{Holm}{a \code{data.frame} containing slots \code{haircut_SR},\code{adj_pvalue},and \code{pct_adj}}
#'    \item{BHY}{a \code{data.frame} containing slots \code{haircut_SR},\code{adj_pvalue},and \code{pct_adj}}
#'    \item{average}{a \code{data.frame} containing slots \code{haircut_SR},\code{adj_pvalue},and \code{pct_adj}}
#'    \item{freq}{output frequency}
#'    \item{sm_fre}{Sampling frequency; [1,2,3,4,5] = [Daily, Weekly, Monthly, Quarterly, Annual]}
#'    \item{num_obs}{No. of observations in the frequency specified in the previous step}
#'    \item{SR}{observed Sharpe Ratio}
#'    \item{SR_ac_adj}{Observed Sharpe Ratio corrected for autocorrelation}
#'    \item{ind_an}{Indicator; if annulized, 'ind_an' = 1; otherwise = 0}
#'    \item{ind_aut}{Indicator; if adjusted for autocorrelations, 'ind_aut' = 0; otherwise = 1}
#'    \item{rho}{Autocorrelation coefficient at the specified frequency}
#'    \item{num_trials}{number or trials}
#'    \item{RHO}{Average correlation among contemporaneous strategy returns}
#'    \item{call}{call used for \code{\link{SharpeRatio.haircut}}}
#'    \item{inner_call}{call used to call \code{\link{.haircutSR}}}
#' }
#' 
#' @references 
#' Harvey, Campbell R. and Yan Liu. 2015. Backtesting The Journal of Portfolio Management. 41:1 pp. 13-28. 
#' 
#' Harvey, Campbell R., Yan Liu, and Heqing Zhu. 2016. "... and the cross-section of expected returns." The Review of Financial Studies 29, no. 1 (2016): 5-68.
#' 
#' @importFrom TTR ROC
#' @seealso \code{\link{SharpeRatio.deflated}}
#' @rdname SharpeRatio.haircut
#' @aliases haircutSharpe SharpeRatio.haircut
#' @export SharpeRatio.haircut
#' @export haircutSharpe
haircutSharpe <- SharpeRatio.haircut <- function( portfolios
                                                  , ...
                                                  , strategy=NULL
                                                  , trials=NULL
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
  # 1. sm_fre
  portfolio <- portfolios[1] #target portfolio is the first one
  p <- .getPortfolio(portfolio) 
  freq <- periodicity(p$summary)$scale
  sm_fre <- switch (freq,
                    "daily" = 1,
                    "weekly" = 2,
                    "monthly" = 3,
                    "quarterly" = 4,
                    "yearly" = 5)
  # 2. num_obs
  num_obs <- nrow(p$summary)
  
  # 3. Sharpe ratio of strategy returns
  SR <- dailySt[1,]$Ann.Sharpe #use the target portfolio, the first row, for this
  SR_index <- which(dailySt$Ann.Sharpe == SR) # potentially use later to determine which test yielded optimal SR
  
  # 4. SR annualized? (1=Yes)
  ind_an <- 1 # if these came from dailyStats, they are annualized
  # if(SR == max(stats$Ann.Sharpe, na.rm = TRUE)){ # Beware: Excessive annualized Sharpe ratios could be the result of short profitable backtests
  #   ind_an <- 1
  # } else {
  #   ind_an <- NULL # is Sharpe ratio takes from stats object returned in apply.paramsets, which is annualized?
  # }
  
  # 5. AC correction needed? (0=Yes), and per Brian for any quantstrat return series, autocorrelation adjustment will be required.
  ind_aut <- 0
  
  # 6. AC level
  roc <- ROC(cumsum(p$summary$Net.Trading.PL))
  ACF <- acf(na.omit(roc), plot = FALSE)
  rho <- ACF$acf[[2]] # assume autocorrelation coefficient for lag = 1 is suitable for now
  
  # 7. # of tests assumed
  num_test <- trials
  
  # 8. Average correlation assumed
  RHO <- 0.2 # use the assumption from HLZ (and the Cross-Section of Expected Returns) p.32-33 Section 5.3
  
    
  result <- .haircutSR(sm_fre, num_obs, SR, ind_an, ind_aut, rho, num_test, RHO)
  result$call <- match.call()
  result
} # end haircut Sharpe wrapper

#' The non-exported \code{.haircutSR} function is the internal implementation of
#' the haircut Sharpe Ratio calculations. It is based off of the code  written 
#' by Jasen Mackie for his blog post referenced below. It calcualtes an equivalent
#' adjusted Sharpe Ratio after taking the number and autocorrelation of the
#' trials into account.
#'
#' @param sm_fre Sampling frequency; [1,2,3,4,5] = [Daily, Weekly, Monthly, Quarterly, Annual] 
#' @param num_obs No. of observations in the frequency specified in the previous step
#' @param SR Sharpe ratio; either annualized or in the frequency specified in the previous step
#' @param ind_an Indicator; if annulized, 'ind_an' = 1; otherwise = 0
#' @param ind_aut Indicator; if adjusted for autocorrelations, 'ind_aut' = 0; otherwise = 1
#' @param rho Autocorrelation coefficient at the specified frequency
#' @param num_test 'num_test': Number of tests allowed. e.g. Harvey, Liu and Zhu (2014) find 315 published equity risk factors
#' @param RHO Average correlation among contemporaneous strategy returns
#'
#' @references 
#' Mackie, Jasen. 2016. R-view: Backtesting - Harvey & Liu (2015). https://opensourcequant.wordpress.com/2016/11/17/r-view-backtesting-harvey-liu-2015/
#' @rdname SharpeRatio.haircut
.haircutSR <- function(  sm_fre
                       , num_obs
                       , SR
                       , ind_an
                       , ind_aut
                       , rho
                       , num_test
                       , RHO)
{

  if(sm_fre == 1){
    fre_out <- 'Daily'
  } else if(sm_fre == 2){ 
    fre_out <- 'Weekly'
  } else if(sm_fre == 3){ 
    fre_out <- 'Monthly'
  } else if(sm_fre == 4){ 
    fre_out <- 'Quarterly'
  } else {
    fre_out <- 'Annual'
  }
  
  if(ind_an == 1){
    sr_out <- 'Yes'
  } else {
    sr_out <- 'No'
  }
  
  # adjust SR to correct for autocorrelation
  if(ind_an == 1 & ind_aut == 0){
    sr_annual <- SR
  } else if(ind_an ==1 & ind_aut == 1){
    if(sm_fre ==1){
      sr_annual <- SR*(1 + (2*rho/(1-rho))*(1- ((1-rho^(360))/(360*(1-rho)))))^(-0.5)
    } else if(sm_fre ==2){
      sr_annual <- SR*(1 + (2*rho/(1-rho))*(1- ((1-rho^(52))/(52*(1-rho)))))^(-0.5)
    } else if(sm_fre ==3){
      sr_annual <- SR*(1 + (2*rho/(1-rho))*(1- ((1-rho^(12))/(12*(1-rho)))))^(-0.5)
    } else if(sm_fre ==4){
      sr_annual <- SR*(1 + (2*rho/(1-rho))*(1- ((1-rho^(4))/(4*(1-rho)))))^(-0.5)
    } else if(sm_fre ==5){
      sr_annual <- SR
    }
  } else if(ind_an == 0 & ind_aut == 0){
    if(sm_fre ==1){ 
      sr_annual <- SR*sqrt(360)
    } else if(sm_fre ==2){
      sr_annual <- SR*sqrt(52)
    } else if(sm_fre ==3){
      sr_annual <- SR*sqrt(12)
    } else if(sm_fre ==4){
      sr_annual <- SR*sqrt(4)
    } else if(sm_fre ==5){
      sr_annual = SR
    }
  } else if(ind_an == 0 & ind_aut == 1){
    if(sm_fre ==1){ 
      sr_annual <- sqrt(360)*SR*(1 + (2*rho/(1-rho))*(1- ((1-rho^(360))/(360*(1-rho)))))^(-0.5)
    } else if(sm_fre ==2){
      sr_annual <- sqrt(52)*SR*(1 + (2*rho/(1-rho))*(1- ((1-rho^(52))/(52*(1-rho)))))^(-0.5)
    } else if(sm_fre ==3){
      sr_annual <- sqrt(12)*SR*(1 + (2*rho/(1-rho))*(1- ((1-rho^(12))/(12*(1-rho)))))^(-0.5);
    } else if(sm_fre ==4){
      sr_annual <- sqrt(4)*SR*(1 + (2*rho/(1-rho))*(1- ((1-rho^(4))/(4*(1-rho)))))^(-0.5)
    } else if(sm_fre ==5){
      sr_annual <- SR
    }
  }
  
  ### Number of monthly observations 'N' ###
  
  if(sm_fre ==1){ 
    N <- floor(num_obs*12/360)
  } else if(sm_fre ==2){
    N <- floor(num_obs*12/52)
  } else if(sm_fre == 3){
    N <- floor(num_obs*12/12)
  } else if(sm_fre == 4){
    N <- floor(num_obs*12/4)
  } else if(sm_fre == 5){
    N <- floor(num_obs*12/1)
  }
  
  ### Number of tests allowed ###
  M <- num_test;
  
  
  ############################################
  ########## Sharpe ratio adjustment #########
  
  m_vec <- 1:(M+1);
  c_const <- sum(1./m_vec);
  
  ########## Input for Holm & BHY ##########
  ### Parameter input from Harvey, Liu and Zhu (2014) 
  para0 <- matrix(c(0, 1295, 3.9660*0.1, 5.4995*0.001,
                    0.2, 1377, 4.4589*0.1, 5.5508*0.001,
                    0.4, 1476, 4.8604*0.1, 5.5413*0.001,
                    0.6, 1773, 5.9902*0.1, 5.5512*0.001,
                    0.8, 3109, 8.3901*0.1, 5.5956*0.001),
                  nrow = 5, ncol = 4, byrow = TRUE)
  ### Interpolated parameter values based on user specified level of correlation RHO
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
  
  WW <- 2000;  ### Number of repetitions (in HL on p.30 the authors suggest using 5k simulations, but in the exhibit they use 2k)
  
  ### Generate a panel of t-ratios (WW*Nsim_tests) ###
  Nsim_tests <- (floor(M/para_inter[2]) + 1)*floor(para_inter[2]+1); # make sure Nsim_test >= M
  
  t_sample <- sample_random_multests(para_inter[1], Nsim_tests, para_inter[3], para_inter[4], WW)
  
  # Sharpe Ratio, monthly
  sr <- sr_annual / (12 ^(1/2))
  T <- sr * N^(1/2)
  p_val <- 2 * (1 - pt(T, N - 1))
  
  # Drawing observations from the underlying p-value distribution; simulate a
  # large number (WW) of p-value samples
  p_holm <- rep(1, WW)
  p_bhy <- rep(1, WW)
  
  for(ww in 1:WW){
    
    yy <-  t_sample[ww, 1:M]
    t_value <- t(yy)
    
    p_val_sub <- 2*(1- pnorm(t_value,0,1));
    
    ### Holm
    p_val_all <-  append(t(p_val_sub), p_val)
    p_val_order <- sort(p_val_all)
    p_holm_vec <- NULL
    
    for(i in 1:(M+1)){
      p_new <- NULL
      for(j in 1:i){
        p_new <- append(p_new, (M+1-j+1)*p_val_order[j])
      }
      p_holm_vec <- append(p_holm_vec, min(max(p_new),1))
    }
    
    p_sub_holm <- p_holm_vec[p_val_order == p_val]
    p_holm[ww] <- p_sub_holm[1];
    
    ### BHY
    p_bhy_vec <- NULL
    p_0 <- Inf 
    for(i in 1:(M+1)){
      kk <- (M+1) - (i-1)
      if(kk == (M+1)){
        p_new <- p_val_order[M+1]
      } else {
        p_new <- min( (M+1)*(c_const/kk)*p_val_order[kk], p_0)
      }
      p_bhy_vec <- append(p_new, p_bhy_vec)
      p_0 <- p_new
    }
    
    p_sub_bhy <- p_bhy_vec[p_val_order == p_val]
    p_bhy[ww] <- p_sub_bhy[1]
    
  }
  
  ### Bonferroni ###
  p_BON <- min(M*p_val,1)
  ### Holm ###
  p_HOL <- median(p_holm)
  ### BHY ###
  p_BHY <- median(p_bhy)
  ### Average ###
  p_avg <- (p_BON + p_HOL + p_BHY)/3
  
  # Invert to get z-score
  z_BON <- qt(1 - p_BON/2, N - 1)
  z_HOL <- qt(1 - p_HOL/2, N - 1)
  z_BHY <- qt(1 - p_BHY/2, N - 1)
  z_avg <- qt(1- p_avg/2, N - 1)
  
  # Annualized Sharpe ratio
  sr_BON <- (z_BON/sqrt(N)) * sqrt(12)
  sr_HOL <- (z_HOL/sqrt(N)) * sqrt(12)
  sr_BHY <- (z_BHY/sqrt(N)) * sqrt(12)
  sr_avg <- (z_avg/sqrt(N))*sqrt(12)
  
  # Calculate haircut
  hc_BON <- (sr_annual - sr_BON)/sr_annual
  hc_HOL <- (sr_annual - sr_HOL)/sr_annual
  hc_BHY <- (sr_annual - sr_BHY)/sr_annual
  hc_avg <- (sr_annual - sr_avg)/sr_annual
  
  # structure and return
  result<-list()
  result$Bonferroni <- data.frame(haircut_SR=sr_BON,
                                  adj_pvalue=p_BON,
                                  pct_adj=hc_BON)
  result$Holm       <- data.frame(haircut_SR=sr_HOL,
                                  adj_pvalue=p_HOL,
                                  pct_adj=hc_HOL)
  result$BHY        <- data.frame(haircut_SR=sr_BHY,
                                  adj_pvalue=p_BHY,
                                  pct_adj=hc_BHY)
  result$average    <- data.frame(haircut_SR=sr_avg,
                                  adj_pvalue=p_avg,
                                  pct_adj=hc_avg)
  result$freq <- fre_out
  result$sm_fre  <- sm_fre
  result$num_obs <- num_obs
  result$SR <- SR
  result$SR_ac_adj <- sr_annual
  result$ind_an <- ind_an
  result$ind_aut <- ind_aut 
  result$rho <- rho
  result$num_test <- num_test
  result$RHO <- RHO
  result$inner_call <- match.call()
  
  return(structure(result, class='haircutSR'))
} # end internal fn .haircutSR

#' print method for Harvey and Liu Haircut Sharpe Ratio
#'
#' @param x an object of type \code{haircutSR} to be printed 
#' @param ... any other passthough parameters
#'
#' @seealso \code{\link{SharpeRatio.haircut}}
#' @method print haircutSR
#' @export
print.haircutSR <- function(x, ...){
  cat('\n',
      'Sharpe Ratio Haircut Report: \n\n',
      'Frequency =', x$freq, '\n',
      'Number of Observations =', x$num_obs, '\n',
      'Observed Sharpe Ratio:',x$SR,'\n',
      'Autocorrelation:',x$rho,'\n',
      'Sharpe Ratio corrected for autocorrelation:',x$SR_ac_adj,'\n',
      'Assumed number of tests:',x$num_test,'\n',
      'Assumed Average Correlation:',x$RHO,'\n\n',
      "Bonferroni Adjustment: \n",
      "Adjusted P-value =", x$Bonferroni$adj_pvalue,'\n',
      "Haircut Sharpe Ratio =", x$Bonferroni$haircut_SR,'\n',
      "Percentage Haircut =", x$Bonferroni$pct_adj,'\n\n',
      "Holm Adjustment: \n",
      "Adjusted P-value =", x$Holm$adj_pvalue,'\n',
      "Haircut Sharpe Ratio =", x$Holm$haircut_SR,'\n',
      "Percentage Haircut =", x$Holm$pct_adj,'\n\n',
      "BHY Adjustment: \n",
      "Adjusted P-value =", x$BHY$adj_pvalue,'\n',
      "Haircut Sharpe Ratio =", x$BHY$haircut_SR,'\n',
      "Percentage Haircut =", x$BHY$pct_adj,'\n\n',
      "Average Adjustment: \n",
      "Adjusted P-value =", x$average$adj_pvalue,'\n',
      "Haircut Sharpe Ratio =", x$average$haircut_SR,'\n',
      "Percentage Haircut =", x$average$pct_adj,'\n')
  
  invisible(x)  
}

#' Generate empirical p-value distributions
#' 
#' 
#' @param rho average correlation among returns
#' @param m_tot total number of trials
#' @param p_0 probability for a random factor to have a zero mean
#' @param lambda average of monthly mean returns for true strategies
#' @param M_simu number of simulations to perform
#'
#' @importFrom MASS mvrnorm
sample_random_multests <- function(rho, m_tot, p_0, lambda, M_simu){ 
  
  ###Parameter input from Harvey, Liu and Zhu (2014) ############
  ###Default: para_vec = [0.2, 1377, 4.4589*0.1, 5.5508*0.001,M_simu]###########
  
  p_0 <- p_0 ;  # probability for a random factor to have a zero mean   
  lambda <- lambda; # average of monthly mean returns for true strategies
  m_tot <- m_tot; # total number of trials
  rho <- rho; # average correlation among returns
  
  M_simu <- M_simu;  # number of rows (simulations) 
  
  sigma <- 0.15/sqrt(12); # assumed level of monthly vol
  N <- 240; #number of time-series (240 months ie. 20 years; see Harvey, Liu and Zhu (2014) Section 5.2 p.30)
  
  sig_vec <- c(1, rho*rep(1, m_tot-1))
  SIGMA <- toeplitz(sig_vec)
  MU <- rep(0,m_tot)
  shock_mat <- mvrnorm(MU, SIGMA*(sigma^2/N), n = M_simu)
  
  prob_vec <- replicate(M_simu, runif(m_tot,0,1))
  
  mean_vec <- t(replicate(M_simu, rexp(m_tot, rate=1/lambda)))
  m_indi <- prob_vec > p_0
  mu_nul <- as.numeric(m_indi)*mean_vec #Null-hypothesis
  tstat_mat <- abs(mu_nul + shock_mat)/(sigma/sqrt(N))
  
  tstat_mat 
} 