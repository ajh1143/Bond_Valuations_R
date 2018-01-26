#----------------------------------
#Language: R
#Function: bond_price(par_val, coup_rate, ttm, yld)
#Inputs: par_val for par value, coup_rate for coupon rate, ttm for time to maturity, and yld for yield.
#Output: price
#----------------------------------

bond_price <- function(par_val, coup_rate, ttm, yld) {
  cf <- c(rep(par_val * coup_rate, ttm - 1), par_val * (1 + coup_rate))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + yld)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  sum(cf$pv)
}
