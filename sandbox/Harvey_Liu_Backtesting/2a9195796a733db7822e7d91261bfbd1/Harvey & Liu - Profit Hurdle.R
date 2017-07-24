### Required returns due to testing multiplicity ------ Harvey and Liu
### (2014): "Backtesting", Duke University 

Profit_Hurdle <- function (num_tests, num_obs, alpha_sig, vol_annual, RHO){


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

print('Inputs:')
print(paste('Significance Level = ',alpha0*100))
print(paste('Number of Observations = ', num_obs))
print(paste('Annualized Return Volatility = ', vol_anu*100))
print(paste('Assumed Number of Tests = ', NN))
print(paste('Assumed Average Correlation = ', RHO))

print('Outputs:')
print('Minimum Average Monthly Return:')
print(paste('Independent = ', ret_hur[1]*100))
print(paste('Bonferroni = ', ret_hur[2]*100))
print(paste('Holm = ', ret_hur[3]*100))
print(paste('BHY = ', ret_hur[4]*100))
print(paste('Average for Multiple Tests = ', mean(ret_hur[-1])*100))

}

Profit_Hurdle(300, 240, 0.05, 0.1, 0.4)
