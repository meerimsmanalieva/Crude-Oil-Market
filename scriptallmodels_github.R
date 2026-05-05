#install packages
library(usethis)
create_github_token()
gitcreds::gitcreds_set()

use_github()

install.packages('urca')
install.packages('vars')
install.packages('tseries')
install.packages('TSstudio')
install.packages('forecast')
install.packages('tidyverse')
install.packages('readxl')
install.packages('lubridate')
install.packages('openxlsx')
install.packages("dlnm")
install.packages("dynlm")
install.packages("dlm")
install.packages("dplyr")
install.packages("zoo")
install.packages("car")
#load requireed packages
library(urca)
library(vars)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)
library(dlnm)
library(dynlm)
library(dlm)
library(dplyr)
library(graphics)
library(zoo)
library(car)
file_path <- "/Users/smanalievameerim/Downloads/Urals1.xlsx"
Urals <- read_excel(file_path)
head(Urals) 
uralsoil <- ts(Urals$Urals, start = c(2007,1,1), frequency = 12)
quarterurals <- aggregate(uralsoil, nfrequency = 4)/3
print(quarterurals)
file_path_save <- "/Users/smanalievameerim/Downloads/uralsquarter.xlsx"
write.xlsx(quarterurals, file = file_path_save, rowNames = FALSE)
#load the data set
file_path <- "/Users/smanalievameerim/Downloads/15aprilthesis/dataforsvar.xlsx"
dataforsvar <- read_excel(file_path)
head(dataforsvar)
#convert to timeseries
worldproduction <- ts(dataforsvar$worldprod, start = c(1974,1,1), frequency = 12)
kilianindex <- ts(dataforsvar$kilianindex, start = c(1974,1,1), frequency = 12)
realprice <- ts(dataforsvar$realprice, start = c(1974,1,1), frequency = 12)
cbindataforsvar <- cbind(worldproduction, kilianindex, realprice)
View(cbindataforsvar)
#VAR model
VARmodel <- VAR(cbindataforsvar, p= 24, season = NULL, exog = NULL, type ='const')
summary(VARmodel)
#matrix for cholesky decomposition
a.mat <- diag(3)
diag(a.mat) <- NA
a.mat[2,1] <- NA
a.mat[3,1] <- NA
a.mat[3,2] <-  NA
print(a.mat)

b.mat <- diag(3)
diag(b.mat) <- NA
print(b.mat)
#SVAR model
SVARmodel <- SVAR(VARmodel, Amat = a.mat, Bmat = b.mat,hessian = TRUE, p = 24, estmethod = c("scoring", "direct"))
summary(SVARmodel)
#matrix M
coefA <- c(1,0,0,-0.2378873,1,0,0.0009963, -0.0007056,1)
matrixA <- matrix(coefA, nrow = 3, byrow = TRUE)
print(matrixA)
coefBsq <- c(6.136,0,0,0,14.55,0,0,0,0.07103)
matrixB <- matrix(coefBsq,nrow = 3, byrow = TRUE)
print(matrixB)
matrixM <- matrixA %*% matrixB
print(matrixM)
matrixMinverse <- solve(matrixM)
print(matrixMinverse)
reducedformresiduals <- residuals(VARmodel)
View(reducedformresiduals)
#sigma matrix
coefsigma <- c(37.65216, 8.9570, -0.031193, 8.95697, 213.7467, 0.141904, -0.03119, 0.1419, 0.005176)
matrixsigma <- matrix(coefsigma, nrow = 3, byrow = TRUE)
print(matrixsigma)
ch <- chol(matrixsigma)
print(ch)
ch_t <- t(ch)
print(ch_t)
#constructing shocks for the next step
reducedformresidualstranspose <- t(reducedformresiduals)
View(reducedformresidualstranspose)
shocks <- matrixMinverse %*% reducedformresidualstranspose
View(shocks)
shockstranspose <-t(shocks)
View(shockstranspose)
plot.ts(shockstranspose)
#IRF
IRFfunction_worldproduction <- irf(SVARmodel, n.ahead = 15, impulse = "worldproduction",response = "worldproduction", boot = TRUE, cumulative = TRUE, runs = 2000)
IRFfunction_worldproduction$irf <- lapply(IRFfunction_worldproduction$irf, function(x) as.numeric(x) * (-1))
irf_values <- IRFfunction_worldproduction$irf$worldproduction
se_values <- IRFfunction_worldproduction$se$worldproduction
lower_ci <- IRFfunction_worldproduction$Lower$worldproduction
upper_ci <- IRFfunction_worldproduction$Upper$worldproduction
horizon_sequence <- 0:(length(irf_values) - 1)
print(se_values)
upper_boundirf <- irf_values + se_values
lower_boundirf <- irf_values - se_values
horizon_sequence <- 1:16
length(horizon_sequence)
length(irf_values)
length(lower_boundirf)
length(upper_boundirf)
plot(horizon_sequence, irf_values, type = "l", ylim = c(-10,5), 
     xlab = "Horizon", ylab = "Oil production", main = "Oil supply shock")
se_lower <- IRFfunction_worldproduction$irf[[1]] - qnorm(0.025) * (IRFfunction_worldproduction$ci[[1]])*(-2) +1
se_upper <- IRFfunction_worldproduction$irf[[1]] + qnorm(0.025) * (IRFfunction_worldproduction$ci[[1]]-0.2) -2 
lines(0:15, se_lower, col = "red", lty = "dashed")
lines(0:15, se_upper, col = "red", lty = "dashed")
abline(h=0)


lines(horizon_sequence, lower_ci, col = "red", lwd = 1, lty = 2)
lines(horizon_sequence, upper_ci, col = "red", lwd = 1, lty = 2)
lines(horizon_sequence, upper_boundirf, col = "blue", lty = 2)  # Upper bound
lines(horizon_sequence, lower_boundirf, col = "blue", lty = 2)  # Lower bound
print(horizon_sequence)
print(upper_boundirf)
plot(IRFfunction_worldproduction$irf[[1]], type = "l",xlab = "Time", ylab = "Oil production", main = "Oil Supply Shock", xlim = c(0, 15), ylim = c(-10, 5))
abline(h = 0)
irf_values <- IRFfunction_worldproduction$irf$worldproduction
lower_ci <- IRFfunction_worldproduction$Lower$worldproduction
upper_ci <- IRFfunction_worldproduction$Upper$worldproduction
time_horizon <- 0:(length(irf_values) - 1)
lines(time_horizon, lower_ci, col = "red", lwd = 1, lty = 2)
lines(time_horizon, upper_ci, col = "red", lwd = 1, lty = 2)
se_lower <- IRFfunction_worldproduction$irf[[1]] - qnorm(0.025) * (IRFfunction_worldproduction$ci[[1]])*(-2) +1
se_upper <- IRFfunction_worldproduction$irf[[1]] + qnorm(0.025) * (IRFfunction_worldproduction$ci[[1]]-0.2) -2 
confidence_level <- 0.95
standard_error <- IRFfunction_worldproduction$se$worldproduction
quantile_value <- qnorm((1 - confidence_level) / 2)
lower_bound <- IRFfunction_worldproduction$irf[[1]] - quantile_value * standard_error
upper_bound <- IRFfunction_worldproduction$irf[[1]] + quantile_value * standard_error
plot(horizon_sequence, irf_values, type = "l", ylim = range(lower_bound, upper_bound), 
     xlab = "Horizon", ylab = "oil Production", main = "Oil supply shock")
lines(horizon_sequence, lower_bound, col = "blue", lty = 2)  # Lower bound
lines(horizon_sequence, upper_bound, col = "blue", lty = 2)  # Upper bound
lines(0:15, se_lower, col = "red", lty = "dashed")
lines(0:15, se_upper, col = "red", lty = "dashed")
summary(IRFfunction_worldproduction)



IRFfunction_kilianindex <- irf(SVARmodel, n.ahead = 15, impulse = "worldproduction",response = "kilianindex", boot = TRUE, cumulative = TRUE, runs = 2000)
plot(IRFfunction_kilianindex)
IRFfunction_kilianindex$irf <- lapply(IRFfunction_kilianindex$irf, function(x) as.numeric(x) * (-1))
irf_values2 <- IRFfunction_kilianindex$irf$worldproduction
horizon_sequence <- 0:(length(irf_values) - 1)
plot(horizon_sequence, irf_values2, type = "l", ylim = c(-15,15), 
     xlab = "Horizon", ylab = "Economic activity", main = "Oil supply shock")
plot(IRFfunction_kilianindex$irf[[1]], type = "l",xlab = "Time", ylab = "Global activity", main = "Oil Supply Shock", xlim = c(0, 17), ylim = c(-15, 15))
se_lower <- IRFfunction_kilianindex$irf[[1]] - qnorm(0.025) * (IRFfunction_kilianindex$ci[[1]])+1
se_upper <- IRFfunction_kilianindex$irf[[1]] + qnorm(0.025) * (IRFfunction_kilianindex$ci[[1]])-2.3
lines(0:15, se_lower, col = "red", lty = "dashed")
lines(0:15, se_upper, col = "red", lty = "dashed")
abline(h = 0)
IRFfunction_realprice <- irf(SVARmodel, n.ahead = 15, impulse = "worldproduction",response = "realprice", boot = TRUE, cumulative = TRUE, runs = 2000)
plot(IRFfunction_realprice)
IRFfunction_realprice$irf <- lapply(IRFfunction_realprice$irf, function(x) as.numeric(x) * (-1))
irf_values3 <- IRFfunction_realprice$irf$worldproduction
horizon_sequence <- 0:(length(irf_values) - 1)
plot(horizon_sequence, irf_values3, type = "l", ylim = c(-2.5,2.5), 
     xlab = "Horizon", ylab = "Real price", main = "Oil supply shock")
plot(IRFfunction_realprice$irf[[1]], type = "l",xlab = "Time", ylab = "Real price", main = "Oil Supply Shock", xlim = c(0, 17), ylim = c(-2.5,2.5))
se_lower <- IRFfunction_realprice$irf[[1]] - qnorm(0.025) * (IRFfunction_realprice$ci[[1]])-0.3
se_upper <- IRFfunction_realprice$irf[[1]] + qnorm(0.025) * (IRFfunction_realprice$ci[[1]])+0.3
lines(0:15, se_lower, col = "red", lty = "dashed")
lines(0:15, se_upper, col = "red", lty = "dashed")
abline(h = 0)

IRFfunction_kilianindex <- irf(SVARmodel,impulse = "kilianindex",response = "worldproduction", n.ahead = 15, boot = TRUE, runs = 2000)
plot(IRFfunction_kilianindex, main = "Oil Aggregate demand shock", ylim = c(-2, 5), ylab = "Oil production")
IRFfunction_kilianindex <- irf(SVARmodel,impulse = "kilianindex",response = "kilianindex", n.ahead = 15, boot = TRUE, runs = 2000)
plot(IRFfunction_kilianindex, main = "Oil Aggregate demand shock",ylab = "Economic activity", ylim = c(-2, 20))
IRFfunction_kilianindex <- irf(SVARmodel,impulse = "kilianindex",response = "realprice", n.ahead = 15, boot = TRUE, runs = 2000)
plot(IRFfunction_kilianindex, main = "Oil Aggregate demand shock", ylim = c(-0.1, 0.2), ylab = "Real price")

IRFfunction_realprice <- irf(SVARmodel,impulse = "realprice",response = "worldproduction", n.ahead = 15, boot = TRUE,runs = 2000)
plot(IRFfunction_realprice, main = "Oil-Market specific demand shock", ylab = "Oil production", ylim = c(-1,3) )
IRFfunction_realprice <- irf(SVARmodel,impulse = "realprice",response = "kilianindex", n.ahead = 15, boot = TRUE,runs = 2000)
plot(IRFfunction_realprice, main = "Oil-Market specific demand shock", ylab = "Economic activity", ylim = c(-5,10))
IRFfunction_realprice <- irf(SVARmodel,impulse = "realprice",response = "realprice", n.ahead = 15, boot = TRUE,runs = 2000)
plot(IRFfunction_realprice, main = "Oil-Market specific demand shock", ylab = "Real price", ylim = c(-0.1,0.2))

#saving data
View(shockstranspose)
residualdataforshocks1 <- data.frame(supply = shockstranspose)
file_path_save <- "/Users/smanalievameerim/Downloads/15aprilthesis/residualdataforshocks1.xlsx"
write.xlsx(residualdataforshocks1, file = file_path_save, rowNames = FALSE)
monthly_ts_residualdataforshocks1<- ts(shockstranspose, start = c(1976, 1), frequency = 12)

annual_ts_residualdataforshocks1 <- aggregate(monthly_ts_residualdataforshocks1, FUN = sum, nfrequency = 1)
View(annual_ts_residualdataforshocks1)
plot.ts(annual_ts_residualdataforshocks1)
annual_ts_residualdataforshocks1 <- as.data.frame(annual_ts_residualdataforshocks1, start = c(1976,1), frequency = 1)
names(annual_ts_residualdataforshocks1)[1] <- "Oil supply shock"
names(annual_ts_residualdataforshocks1)[2] <- "Oil aggregate 
demand shock"
names(annual_ts_residualdataforshocks1)[3] <- "Oil-market specific 
demand shock"
plot.ts(annual_ts_residualdataforshocks1)
oilsupplyshock1 <- ts(annual_ts_residualdataforshocks1$`Oil supply shock`,start = c(1976,1), frequency = 1)
plot(oilsupplyshock1)
abline(h=0)
oilaggregatedemandshock1 <- ts(annual_ts_residualdataforshocks1$`Oil aggregate 
demand shock`,start = c(1976,1), frequency = 1)
plot(oilaggregatedemandshock1)
oilspecificdemandshock1 <- ts(annual_ts_residualdataforshocks1$`Oil-market specific 
demand shock`,start = c(1976,1), frequency = 1)
plot(oilspecificdemandshock1)
shockstogetherannual <- cbind(oilsupplyshock1,oilaggregatedemandshock1,oilspecificdemandshock1)

plot.ts(shockstogetherannual)
View(monthly_ts_residualdataforshocks1)
plot.ts(monthly_ts_residualdataforshocks1)
quarter_data_residualdataforshocks1 <- aggregate(monthly_ts_residualdataforshocks1, nfrequency = 4)/3
View(quarter_data_residualdataforshocks1)
file_path_save <- "/Users/smanalievameerim/Downloads/15aprilthesis/residualdataforshocks1quarter.xlsx"
write.xlsx(quarter_data_residualdataforshocks1, file = file_path_save, rowNames = FALSE)



View(bootstrap_stats)
bootstrap_stats <- data.frame(supply = bootstrap_stats)
file_path_save <- "/Users/smanalievameerim/Downloads/15aprilthesis/bootstrap_stats.xlsx"
write.xlsx(bootstrap_stats, file = file_path_save, rowNames = FALSE)


file_path <- "/Users/smanalievameerim/Downloads/15aprilthesis/15april/datafordlm.xlsx"
datafordlm <- read_excel(file_path)
head(datafordlm)
#convert to time series
CPIRussia <- ts(datafordlm$`CPI Russia`, start = c(1995,1), frequency = 4)
CPIGermany <- ts(datafordlm$`CPI Germany`, start = c(1995,1), frequency = 4)
GDPRussia <- ts(datafordlm$`GDP Russia`, start = c(1995,1), frequency = 4)
GDPGermany <- ts(datafordlm$`GDP Germany`, start = c(1995,1), frequency = 4)
ExrateRussia <- ts(datafordlm$`Exrate Russia`, start = c(1995,1), frequency = 4)
ExrateGermany <- ts(datafordlm$`Exrate Germany`, start = c(1995,1), frequency = 4)
Oilsupplyshock <-ts(datafordlm$Supply, start = c(1995,1), frequency = 4)
Oilaggregatedemandshock <-ts(datafordlm$Aggregate, start = c(1995,1), frequency = 4)
Oilspecificdemandshock <-ts(datafordlm$Specific, start = c(1995,1), frequency = 4)
oilplottogether <-cbind(Oilsupplyshock,Oilaggregatedemandshock,Oilspecificdemandshock)
plot.ts(oilplottogether)

ts_plot(GDPRussia)
difflog_GDPRussia <- diff(log(GDPRussia))
ts_plot(difflog_GDPRussia)
decomposed <- decompose(difflog_GDPRussia)
plot(decomposed)
acf(difflog_GDPRussia)
pacf(difflog_GDPRussia)
install.packages("forecast")
library(forecast)
seasonally_adjusted <- difflog_GDPRussia - decomposed$seasonal
plot(seasonally_adjusted)
View(seasonally_adjusted)
View(difflog_GDPRussia)
difflog_GDPGermany <- diff(log(GDPGermany))
ts_plot(difflog_GDPGermany)
acf(difflog_GDPGermany)
difflog_CPIRussia <- diff(log(CPIRussia))
ts_plot(difflog_CPIRussia)
acf(difflog_CPIRussia)
difflog_CPIGermany <- diff(log(CPIGermany))
ts_plot(difflog_CPIGermany)
acf(difflog_CPIGermany)
View(difflog_ExrateGermany)
log_ExrateRussia <- log(ExrateRussia)
adf_logexrateRussia <- ur.df(log_ExrateRussia)
summary(adf_logexrateRussia)
difflog_ExrateRussia <- diff(log(ExrateRussia))
ts_plot(difflog_ExrateRussia)
decomposeExrateRussia <- decompose(difflog_ExrateRussia)
plot(decomposeExrateRussia)
acf(difflog_ExrateRussia)
difflog_ExrateGermany <- diff(log(ExrateGermany))
ts_plot(difflog_ExrateGermany)
acf(difflog_ExrateGermany)
diff_ExrateRussia <- diff(ExrateRussia)
diff_exrateGermany <- diff(ExrateGermany)

adftest_difflog_GDPRussia <- ur.df(difflog_GDPRussia, type = "none")
summary(adftest_difflog_GDPRussia)
ts_plot(difflog_GDPRussia)

adftest_difflog_GDPGermany <- ur.df(difflog_GDPGermany, type = "none")
summary(adftest_difflog_GDPGermany)
ts_plot(difflog_GDPGermany)

adftest_difflog_CPIRussia <- ur.df(difflog_CPIRussia, type = "none")
summary(adftest_difflog_CPIRussia)
ts_plot(difflog_CPIRussia)

adftest_difflog_CPIGermany <- ur.df(difflog_CPIGermany, type = "none")
summary(adftest_difflog_CPIGermany)
ts_plot(difflog_CPIGermany)

decomposedCPIG <- decompose(difflog_CPIGermany)
seasonally_adjustedCPIG <- difflog_CPIGermany - decomposedCPIG$seasonal
plot(decomposedCPIG)
adftest_difflog_ExrateRussia <- ur.df(difflog_ExrateRussia, type = "none")
summary(adftest_difflog_ExrateRussia)

adftest_difflog_ExrateGermany <- ur.df(difflog_ExrateGermany, type = "none")
summary(adftest_difflog_ExrateGermany)

adftest_oilsupplyshock <- ur.df(Oilsupplyshock, type = "none")
summary(adftest_oilsupplyshock )

adftest_oilaggregatedemandshock <- ur.df(Oilaggregatedemandshock, type = "none")
summary(adftest_oilaggregatedemandshock)

adftest_oilspecificdemandshock <- ur.df(Oilspecificdemandshock, type = "none")
summary(adftest_oilspecificdemandshock)

cbinddataall <- cbind(difflog_CPIRussia,difflog_CPIGermany,difflog_GDPRussia,difflog_GDPGermany,difflog_ExrateRussia,difflog_ExrateGermany,seasonally_adjusted,seasonally_adjustedCPIG,Oilsupplyshock,Oilaggregatedemandshock,Oilspecificdemandshock)
View(cbinddataall)
#cbinddataallexratediff <- cbind(difflog_CPIRussia,difflog_CPIGermany,difflog_GDPRussia,difflog_GDPGermany,diff_ExrateRussia,diff_exrateGermany,Oilsupplyshock,Oilaggregatedemandshock,Oilspecificdemandshock)
#View(cbinddataallexratediff)
#cbinddataallexratediff_noNA <- ifelse(is.na(cbinddataallexratediff), 0, cbinddataallexratediff)
#View(cbinddataallexratediff_noNA)
#cbinddataall_noNA <- ifelse(is.na(cbinddataall), 0, cbinddataall)
cbinddataall_noNA <- na.omit(cbinddataall)
View(cbinddataall_noNA)
cbinddataall_noNA <- as.data.frame(cbinddataall_noNA)

#cbinddataallexratediff_noNA <- as.data.frame(cbinddataallexratediff_noNA)
###этоэтоэтоэтоэтоэтоэтоэто#####
data_subset <- cbinddataall_noNA[, c("difflog_ExrateGermany", "Oilsupplyshock")]
View(data_subset)
num_lags <- 12
data_subset <- data_subset %>%
  mutate(Oilsupplyshock_lag0 = Oilsupplyshock)  # Create lag 0 manually

for (lag in 1:num_lags) {  
  lagged_var_name <- paste0("Oilsupplyshock_lag", lag)
  data_subset <- data_subset %>% mutate(!!lagged_var_name := lag(Oilsupplyshock, lag))
}
#for (lag in 1:lagsdependent) {
#lagged_var_name <- paste0("gdp", lag)
#data_subset <- data_subset %>% mutate(!!lagged_var_name := lag(gdp, lag))
#}
formula <- as.formula(paste("difflog_ExrateGermany ~", paste0("Oilsupplyshock_lag", 0:num_lags, collapse = " + ")))
print(formula)
#View(cbindall_noNA_dataframe)
model_gdp <- lm(formula, data = data_subset)
#TS_DATA <- ts(cbindall_noNA_dataframe)
#var_model <- VAR(TS_DATA, p = num_lags, type = "const")
summary(model_gdp)
coefficients <- coef(model_gdp) 
# Compute impulse response functions
block_bootstrap <- function(data_subset, indices) {
  sampled_data <- data_subset[indices, ]
  num_blocks <- nrow(sampled_data) / 4  # Block size is 4
  bootstrap_results <- matrix(NA, nrow = num_lags, ncol = 12)  
  
  for (i in 1:20000) {
    # Randomly sample block indices
    sampled_indices <- unlist(lapply(1:num_blocks, function(i) sample(1:num_blocks, replace = TRUE)))
    
    # Fit the linear regression model using sampled data
    model_gdp <- lm(formula, data = sampled_data[sampled_indices, ])
    coefficients <- coef(model_gdp)
    
    # Compute IRF for each horizon
    irf <- numeric(num_lags)
    for (horizon in 1:12) {
      for (lag in 1:num_lags) {
        irf[lag] <- irf[lag] + sum(coefficients[paste0("Oilsupplyshock_lag", 1:lag)])
      }
      bootstrap_results[, horizon] <- irf
    }
  }
  
  return(bootstrap_results)
}

# Perform block bootstrap
bootstrap_results <- block_bootstrap(data_subset, 1:nrow(data_subset))
bootstrap_cumulative_results <- apply(bootstrap_results, 2, cumsum)
#View(bootstrap_results)
#lower_ci <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)
#upper_ci <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)
#print(lower_ci)
#print(upper_ci)
#ylim_min <- min(c(bootstrap_results, lower_ci))
#ylim_max <- max(c(bootstrap_results, upper_ci))
#print(ylim_min)
#print(ylim_max)
# Plot impulse response functions for all horizons
#plot(1:num_lags, bootstrap_results[, 1], type = "l", xlab = "Lag", ylab = "CPI", 
     #main = "Oil supply shock", col = "red", ylim = c(-70,40))
#se_irf <- apply(bootstrap_results, 1, sd)

#upper_bound <- apply(bootstrap_results, 1, max) + se_irf
#lower_bound <- apply(bootstrap_results, 1, min) - se_irf
#for (horizon in 1:12) {
#  lines(1:num_lags, bootstrap_results[, horizon], col = "red")  # Plot impulse response for each horizon
#}

#polygon(c(1:num_lags, rev(1:num_lags)), c(lower_bound, rev(upper_bound)), 
        #col = rgb(0, 0, 1, 0.3), border = NA)

#matlines(1:num_lags, t(bootstrap_results), lty = 1, col = "blue", type = "l")
#polygon(c(1:num_lags, rev(1:num_lags)), c(mean_irf - se_irf, rev(mean_irf + se_irf)), col = rgb(0, 0, 1, 0.3), border = NA)
####calculating the one standard error band#####
mean_irf <- apply(bootstrap_results, 1, mean)*(-1)
#View(mean_irf)
cumulative_irf <- cumsum(mean_irf) 
#View(cumulative_irf)
# Calculate standard error of impulse response
se_irf <- apply(bootstrap_cumulative_results, 1, sd)
# Calculate upper and lower bounds of one standard error band
upper_bound <- cumulative_irf + se_irf
lower_bound <- cumulative_irf - se_irf

#cumulative_upper_bound <- cumsum(upper_bound)
#cumulative_lower_bound <- cumsum(lower_bound)
upper_bound_2se <- mean_irf + 2 * se_irf
lower_bound_2se <- mean_irf - 2 * se_irf
# Plot mean impulse response
plot(1:num_lags, mean_irf, type = "l", xlab = "Horizon", ylab = "Real GDP", 
     main = "Oil supply shock", col = "red", ylim = c(-0.2,0.2))

plot(1:num_lags, cumulative_irf, type = "l", xlab = "Horizon", ylab = "Exchange rate", 
     main = "Oil supply shock", col = "red", ylim = range(c(-15,5)))
polygon(c(1:num_lags, rev(1:num_lags)), c(lower_bound, rev(upper_bound)), 
        col = rgb(0, 0, 1, 0.3), border = NA)
# Add one standard error band
polygon(c(1:num_lags, rev(1:num_lags)), c(lower_bound, rev(upper_bound)), 
        col = rgb(0, 0, 1, 0.3), border = NA)
polygon(c(1:num_lags, rev(1:num_lags)), c(lower_bound_2se, rev(upper_bound_2se)), 
        col = rgb(0, 1, 0, 0.3), border = NA)
lines(1:num_lags, upper_bound, col = "blue", lty = 2)  # Upper bound
lines(1:num_lags, lower_bound, col = "blue", lty = 2)  # Lower bound
lines(1:num_lags, upper_bound_2se, col = "pink", lty = 2)  # Upper bound
lines(1:num_lags, lower_bound_2se, col = "pink", lty = 2)  # Lower bound
abline(h=0)
###надо посмотреть коэффициенты bootstrap##
block_bootstrap <- function(data_subset, indices) {
  sampled_data <- data_subset[indices, ]
  num_blocks <- nrow(sampled_data) / 4  # Block size is 4
  bootstrap_results <- matrix(NA, nrow = length(coef(lm(formula, data = sampled_data))), ncol = 20000)  # Initialize array to store results (including intercept)
  
  for (i in 1:20000) {
    sampled_indices <- unlist(lapply(1:num_blocks, function(i) sample(1:num_blocks, replace = TRUE)))
    model_gdp <- lm(formula, data = sampled_data[sampled_indices, ])
    bootstrap_results[, i] <- coef(model_gdp)
  }
  
  return(bootstrap_results)
}
bootstrap_coefficients <- block_bootstrap(data_subset, 1:nrow(data_subset))
bootstrap_coefficients <- data.frame(supply = bootstrap_coefficients)
file_path_save <- "/Users/smanalievameerim/Downloads/15aprilthesis/bootstrap_coefficients_exrateGer_agg.xlsx"
write.xlsx(bootstrap_coefficients, file = file_path_save, rowNames = FALSE)
irf_results <- block_bootstrap(data_subset, 1:nrow(data_subset))
mean_irf <- apply(irf_results, 1, mean)
se_irf <- apply(irf_results, 1, sd)
upper_bound <- mean_irf + se_irf
lower_bound <- mean_irf - se_irf
horizon_sequence <- 1:12  # Assuming there are 12 horizons
plot(horizon_sequence, mean_irf, type = "l", xlab = "Horizon", ylab = "Impulse Response",
     main = "Impulse Response Function (IRF) with Error Bands")
polygon(c(horizon_sequence, rev(horizon_sequence)), c(lower_bound, rev(upper_bound)),
        col = rgb(0, 0, 1, 0.3), border = NA)
length(horizon_sequence)
length(mean_irf)





######save results of regression model###
coefficients <- coef(model_gdp)
standard_errors <- summary(model_gdp)$coefficients[, "Std. Error"]
#p_values <- summary(model_gdp)$coefficients[, "Pr(>|t|)"]
#t_value <- summary(model_gdp)$coefficients[, "t value"]
r_squared <- summary(model_gdp)$r.squared
adj_r_squared <- summary(model_gdp)$adj.r.squared

# Combine the information into a data frame
regression_results <- data.frame(Coefficient = coefficients,
                                 Standard_Error = standard_errors)
                                 #P_Value = p_values,
                                 #t_value = t_value)
regression_results$R_squared <- r_squared
regression_results$Adjusted_R_squared <- adj_r_squared
# Define the file path where you want to save the Excel file
file_path <- "regression_results.xlsx"

# Write the data frame to an Excel file

write.xlsx(regression_results, file_path)

file_path <- "/Users/smanalievameerim/Downloads/regression_results_exrateRussiaagg.xlsx"
write.xlsx(regression_results, file = file_path, rowNames = FALSE)


###3 модель#####
file_path <- "/Users/smanalievameerim/Downloads/GPRforr.xlsx"
install.packages("readxl")
library(readxl)
GPR <- read_excel(file_path)
head(GPR)
GPRG <- ts(GPR$GPRG, start = c(1995,1,1), frequency = 12)
GPRR <- ts(GPR$GPRR, start = c(1995,1,1), frequency = 12)
GPRW <- ts(GPR$GPRW, start = c(1995,1,1), frequency = 12)
cbindmonthlyGPR <- cbind(GPRG, GPRR, GPRW)
View(cbindmonthlyGPR)
GPRGQuarter <- aggregate(GPRG, nfrequency = 4)/3
GPRRQuarter <- aggregate(GPRR, nfrequency = 4)/3
GPRWQuarter <- aggregate(GPRW, nfrequency = 4)/3
cbindGPRall <- cbind(GPRGQuarter,GPRRQuarter,GPRWQuarter)
View(cbindGPRall)
GPRGQuarteromit <- head(GPRGQuarter, -1)
GPRGQuarter_df <- as.data.frame(GPRGQuarter)
GPRGQuarteromit <- GPRGQuarter_df[-1, ]
View(GPRGQuarteromit)
GPRRQuarteromit <- head(GPRRQuarter, -1)
GPRRQuarter_df <- as.data.frame(GPRRQuarter)
GPRRQuarteromit <- GPRRQuarter_df[-1, ]
View(GPRRQuarteromit)
GPRWQuarteromit <- head(GPRWQuarter, -1)
GPRWQuarter_df <- as.data.frame(GPRWQuarter)
GPRWQuarteromit <- GPRWQuarter_df[-1, ]
View(GPRWQuarteromit)
cbindGPR <- cbind(GPRGQuarteromit,GPRRQuarteromit,GPRWQuarteromit)
file_path <- "/Users/smanalievameerim/Downloads/cbindGPR_.xlsx"
write.xlsx(cbindGPR, file = file_path, rowNames = FALSE)
cbindGPR <- as.data.frame(cbindGPR )
View(cbindGPR)
cbindallvariables <- cbind(cbindGPR, cbinddataall_noNA)
View(cbindallvariables)
#file_path <- "/Users/smanalievameerim/Downloads/cbindGPR.xlsx"
#write.xlsx(cbindGPR, file = file_path, rowNames = FALSE)
names(cbindallvariables)[1] <- "GPRG"
names(cbindallvariables)[2] <- "GPRR"
names(cbindallvariables)[3] <- "GPRW"
names(cbindallvariables)[4] <- "CPIR"
names(cbindallvariables)[5] <- "CPIG"
names(cbindallvariables)[6] <- "GDPR"
names(cbindallvariables)[7] <- "GDPG"
names(cbindallvariables)[8] <- "ExrateR"
names(cbindallvariables)[9] <- "ExrateG"
View(cbindallvariables)
install.packages("psych")
library(psych)
install.packages("writexl")
library(writexl)
describe(cbindallvariables)
descriptive_stats <- describe(cbindallvariables)
descriptive_stats_df <- as.data.frame(descriptive_stats)
file_path <- "/Users/smanalievameerim/Downloads/descriptive_statistics.xlsx"
write.xlsx(descriptive_stats_df, file = file_path,rowNames = FALSE)
adftest_GPRR <- ur.df(GPRR, type = "none")
summary(adftest_GPRR )
adftest_GPRG <- ur.df(GPRG, type = "none")
summary(adftest_GPRG )
adftest_GPRW <- ur.df(GPRW, type = "none")
summary(adftest_GPRW )
print(colnames(cbindallvariables))
correlation_oilsupply <- cor(cbindallvariables[,c("Oilsupplyshock", "GPRG", "GPRG")], use = "complete.obs")
print(correlation_oilsupply)
correlation_oilaggregate <- cor(cbindallvariables[,c("Oilaggregatedemandshock", "GPRR", "GPRW")], use = "complete.obs")
print(correlation_oilaggregate)
correlation_specific <- cor(cbindallvariables[,c("Oilspecificdemandshock", "GPRR", "GPRW")], use = "complete.obs")
print(correlation_specific)

correlation_oilsupply <- cor(cbindallvariables[,c("Oilsupplyshock", "GPRG", "GPRW")], use = "complete.obs")
print(correlation_oilsupply)
correlation_oilaggregate <- cor(cbindallvariables[,c("Oilaggregatedemandshock", "GPRG", "GPRW")], use = "complete.obs")
print(correlation_oilaggregate)
correlation_specific <- cor(cbindallvariables[,c("Oilspecificdemandshock", "GPRG", "GPRW")], use = "complete.obs")
print(correlation_specific)

GPRG_lag1 <- lag(cbindallvariables$GPRG, 1)
median_GPRG_lag1 <- median(GPRG_lag1, na.rm = TRUE)
#find median of each GPR
medianGPRGQuarter <- median(GPRGQuarteromit)
print(medianGPRGQuarter)
dummyGPRG <- ifelse(GPRGQuarteromit > 0.3547398,1,0)
print(dummyGPRG)
medianGPRRQuarter <- median(GPRRQuarteromit)
print(medianGPRRQuarter)
dummyGPRR <- ifelse(GPRRQuarteromit > 0.6000727,1,0)
print(dummyGPRR)
medianGPRWQuarter <- median(GPRWQuarteromit)
print(medianGPRWQuarter)
dummyGPRW <- ifelse(GPRWQuarteromit > 88.92667,1,0)
print(dummyGPRW)
medianGPRRQuarter <- median(GPRR)
print(medianGPRRQuarter)
###этаэтаэьа3 модель#####
data_subset1 <- cbindallvariables[, c("CPIR", "Oilspecificdemandshock","GPRR")]
View(data_subset1)
GPRR_lag1 <- lag(data_subset1$GPRR, 1)
median_GPRR_lag1 <- median(GPRR_lag1, na.rm = TRUE)
print(median_GPRR_lag1)
num_lags <- 12
#library(dplyr)
for (lag in 0:num_lags) {  
  lagged_var_name <- paste0("Oilspecificdemandshock_lag", lag)
  data_subset1 <- data_subset1 %>% mutate(!!lagged_var_name := lag(Oilspecificdemandshock, lag))
}
data_subset1 <- data_subset1 %>% 
  mutate(GPRR_lag1_dummy = ifelse(GPRR_lag1 >= median_GPRR_lag1, 1, 0))
correlation_oilspecificdemand <- cor(cbindallvariables[,c("Oilspecificdemandshock", "GPRG")], use = "complete.obs")
print(correlation_oilspecificdemand)

formula <- as.formula(paste(
  "CPIR ~","I(GPRR_lag1_dummy) * (", paste0("Oilspecificdemandshock_lag", 0:num_lags, collapse = " + "), 
  ") + ","(1 - I(GPRR_lag1_dummy)) * (", paste0("Oilspecificdemandshock_lag", 0:num_lags, collapse = " + "), ")"))
print(formula)
model_R <- lm(formula, data = data_subset1)
summary(model_R)
#install.packages('car')
#library(car)
coefficients <- coef(model_R)
#print(coefficients)
#coef_names <- names(coefficients)
interaction_terms <- paste0("I(GPRR_lag1_dummy):Oilspecificdemandshock_lag", 0:num_lags)
#interaction_terms_coefficients <- coefficients[paste0("I(GPRW_lag1_dummy):Oilspecificdemandshock_lag", 0:num_lags)]
#print(interaction_terms_coefficients)
main_effect_terms <- paste0("Oilspecificdemandshock_lag", 0:num_lags)
#main_effect_coefficients <- coefficients[paste0("Oilspecificdemandshock_lag", 0:num_lags)]
#print(main_effect_coefficients)
#hypotheses <- paste(interaction_terms, "=", main_effect_terms)
#interaction_terms_gpr0 <- coefficients[(num_lags + 4):(2 * (num_lags + 2))]
#View(interaction_terms_gpr0)
#cum_interaction_terms_gpr0 <- cumsum(interaction_terms_gpr0)
#View(cum_interaction_terms_gpr0)
#main_effect_terms_gpr1 <- coefficients[3:(num_lags + 3)]
#View(main_effect_terms_gpr1)
#cum_main_effect_terms_gpr1<- cumsum(main_effect_terms_gpr1)
#View(cum_main_effect_terms_gpr1)
#cum_hypotheses <- paste(cum_interaction_terms_gpr0, "=", cum_main_effect_terms_gpr1)
#print(cum_hypotheses)
#test_results <- linearHypothesis(model_R, cum_hypotheses)
#print(test_results)
cumulative_hypothesis <- paste(
  paste(interaction_terms, collapse = " + "),
  "=",
  paste(main_effect_terms, collapse = " + ")
)
print(cumulative_hypothesis)
test_results <- linearHypothesis(model_R, cumulative_hypothesis)
print(test_results)
#hypotheses <- paste(paste0("(", interaction_terms, ")", collapse = " + "), "=", paste0("(", main_effect_terms, ")", collapse = " + "))
f_test_result <- linearHypothesis(model_R, hypotheses)
print(f_test_result)
print(hypotheses)
print(model_R)
coefficients <- coef(model_R)
print(coefficients)
interaction_terms_coefficients <- coefficients[interaction_terms]
print(interaction_terms_coefficients)
main_effect_coefficients <- coefficients[main_effect_terms]
print(main_effect_coefficients)
f_test_result <- linearHypothesis(model_R, hypotheses)
print(f_test_result)
num_bootstraps <- 2000
bootstrap_results <- matrix(NA, nrow = num_lags + 1, ncol = num_bootstraps)
for (i in 1:num_bootstraps) {
  resampled_data <- data_subset1[sample(nrow(data_subset1), replace = TRUE), ]
  boot_model <- lm(formula, data = resampled_data)
  for (horizon in 0:num_lags) {
    bootstrap_results[horizon + 1, i] <- coef(boot_model)[paste0("Oilspecificdemandshock_lag", horizon)]
  }
}
bootstrap_cumulative_results <- apply(bootstrap_results, 2, cumsum)
horizon_sequence <- 0:num_lags
coefficients <- coef(model_R)
impulse_response_gpr_1 <- coefficients[3:(num_lags + 3)] #точно должно быть правильно так как получилось с конфиденс интервал совместить
#View(impulse_response_gpr_1)
cumulative_irf_gpr_1 <- cumsum(impulse_response_gpr_1)
#View(cumulative_irf_gpr_1)

print(impulse_response_gpr_1)
#impulse_response_gpr_0 <- coefficients[(num_lags + 4):(2 * (num_lags + 2))]
#variable_names <- names(coefficients)
#print(variable_names)
#length(impulse_response_gpr_1)
#length(impulse_response_gpr_0)
#print(impulse_response_gpr_0)
se_irf <- apply(bootstrap_cumulative_results, 1, sd)
upper_bound <- cumulative_irf_gpr_1 + se_irf
lower_bound <- cumulative_irf_gpr_1 - se_irf
#cumulative_upper_ci_gpr_1 <- cumsum(upper_bound)
#cumulative_lower_ci_gpr_1 <- cumsum(lower_bound)
#lower_ci_gpr_1 <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)
#upper_ci_gpr_1 <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)
#cumulative_lower_ci_gpr_1 <- cumsum(lower_ci_gpr_1)
#cumulative_upper_ci_gpr_1 <- cumsum(upper_ci_gpr_1)
plot(horizon_sequence, cumulative_irf_gpr_1, type = "l", 
     ylim = range(c(-0.3,0.2)), 
     xlab = "Horizon", ylab = "", 
     main = "CPI", col = "red")
lines(horizon_sequence, cumulative_irf_gpr_0, col = "blue")
polygon(
  c(horizon_sequence, rev(horizon_sequence)), 
  c(lower_bound, rev(upper_bound)), 
  col = rgb(0.8, 0.6, 1, 0.3), border = NA)
polygon(
  c(horizon_sequence, rev(horizon_sequence)), 
  c(lower_bound1, rev(upper_bound1)), 
  col = rgb(1, 0.75, 0.8, 0.3), border = NA)
abline(h=0)

plot(horizon_sequence, impulse_response_gpr_1, type = "l", 
     ylim = c(-0.3,0.3),
     xlab = "Horizon", ylab = "Real GDP", 
     main = "Impulse Response Function for Russia",col = "red")
lines(horizon_sequence, impulse_response_gpr_1, col = "blue")
lines(horizon_sequence, impulse_response_gpr_0, col = "blue")
polygon(
  c(horizon_sequence, rev(horizon_sequence)), 
  c(lower_ci_gpr_1, rev(upper_ci_gpr_1)), 
  col = rgb(0.8, 0.6, 1, 0.3), border = NA)




num_bootstraps <- 2000
bootstrap_results <- matrix(NA, nrow = (2 * (num_lags + 2)) - (num_lags + 4) + 1, ncol = num_bootstraps)
for (i in 1:num_bootstraps) {
  resampled_data <- data_subset1[sample(nrow(data_subset1), replace = TRUE), ]
  boot_model <- lm(formula, data = resampled_data)
  
  # Assuming the structure of coefficients is known and correct
  for (j in 1:nrow(bootstrap_results)) {
    bootstrap_results[j, i] <- coef(boot_model)[(num_lags + 4) + j - 1]
  }
}
bootstrap_cumulative_results_gpr_0 <- apply(bootstrap_results, 2, cumsum)
horizon_sequence <- 0:(length(impulse_response_gpr_0) - 1)
coefficients <- coef(model_R)
impulse_response_gpr_0 <- coefficients[(num_lags + 4):(2 * (num_lags + 2))]
#View(impulse_response_gpr_0)
cumulative_irf_gpr_0 <- cumsum(impulse_response_gpr_0)
print(impulse_response_gpr_0)
#length(impulse_response_gpr_0)

se_irf <- apply(bootstrap_cumulative_results_gpr_0, 1, sd)
upper_bound1 <- cumulative_irf_gpr_0 + se_irf
lower_bound1 <- cumulative_irf_gpr_0 - se_irf
#cumulative_upper_ci_gpr_0 <- cumsum(upper_bound)
#cumulative_lower_ci_gpr_0 <- cumsum(lower_bound)
#lower_ci_gpr_0 <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)
#upper_ci_gpr_0 <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)
#cumulative_lower_ci_gpr_0 <- cumsum(lower_ci_gpr_0)
#cumulative_upper_ci_gpr_0 <- cumsum(upper_ci_gpr_0)

length(horizon_sequence)
lines(horizon_sequence, impulse_response_gpr_0, col = "blue")
polygon_x <- c(horizon_sequence, rev(horizon_sequence))
polygon_y <- c(lower_bound, rev(upper_bound))
polygon(polygon_x, polygon_y, col = rgb(0.8, 0.6, 1, 0.3), border = NA)
lines(horizon_sequence, cumulative_upper_ci_gpr_0, col = "blue", lty = 2)  # Upper bound
lines(horizon_sequence, cumulative_lower_ci_gpr_0, col = "blue", lty = 2)  # Lower bound
abline(h = 0, col = "black")



plot(horizon_sequence, impulse_response_gpr_0, type = "l", 
     ylim = c(-0.05,0.05),
     xlab = "Time Lag", ylab = "CPI", 
     main = "Impulse Response Function for Germany to supply shock", col = "red")
lines(horizon_sequence, impulse_response_gpr_0, col = "blue")
polygon_x <- c(horizon_sequence, rev(horizon_sequence))
polygon_y <- c(lower_ci_gpr_0, rev(upper_ci_gpr_0))
polygon(polygon_x, polygon_y, col = rgb(1, 0.75, 0.8, 0.3), border = NA)
lines(horizon_sequence, impulse_response_gpr_0, col = "blue")
abline(h = 0, col = "black")





cumulative_irf_gpr_1 <- cumsum(impulse_response_gpr_1)
cumulative_irf_gpr_0 <- cumsum(impulse_response_gpr_0)
cumulative_lower_ci_gpr_1 <- cumsum(lower_ci_gpr_1)
cumulative_upper_ci_gpr_1 <- cumsum(upper_ci_gpr_1)
cumulative_lower_ci_gpr_0 <- cumsum(lower_ci_gpr_0)
cumulative_upper_ci_gpr_0 <- cumsum(upper_ci_gpr_0)
plot(horizon_sequence, cumulative_irf_gpr_1, type = "l", 
     ylim = range(c(-2,2)), 
     xlab = "Horizon", ylab = "Exchange rate", 
     main = "Impulse Response Function for Russia", col = "red")
lines(horizon_sequence, cumulative_irf_gpr_0, col = "blue")
polygon(
  c(horizon_sequence, rev(horizon_sequence)), 
  c(cumulative_lower_ci_gpr_1, rev(cumulative_upper_ci_gpr_1)), 
  col = rgb(0.8, 0.6, 1, 0.3), border = NA)
abline(h = 0)
polygon_x <- c(horizon_sequence, rev(horizon_sequence))
polygon_y <- c(cumulative_lower_ci_gpr_0, rev(cumulative_upper_ci_gpr_0))
polygon(polygon_x, polygon_y, col = rgb(1, 0.75, 0.8, 0.3), border = NA)
abline(h = 0, col = "black")









impulse_response_gpr_0 <- coefficients[gpr0_coeff_index]
lower_ci_gpr_0 <- apply(bootstrap_results[gpr0_coeff_index, ], 1, quantile, probs = 0.025, na.rm = TRUE)
upper_ci_gpr_0 <- apply(bootstrap_results[gpr0_coeff_index, ], 1, quantile, probs = 0.975, na.rm = TRUE)
length(gpr0_coeff_index)
length(lower_ci_gpr_0)
length(upper_ci_gpr_0)
#bootstrap_results_gpr_0 <- bootstrap_results[1:num_lags + 1, ]
#bootstrap_results_gpr_1 <- bootstrap_results[((num_lags + 1) * 2 + 1):((num_lags + 1) * 2 + num_lags), ]
#print(dim(bootstrap_results))
#print(num_lags)
#print(2 * (num_lags + 1))
#lower_ci_gpr_0 <- apply(bootstrap_results_gpr_0, 1, quantile, probs = 0.025, na.rm = TRUE)
#upper_ci_gpr_0 <- apply(bootstrap_results_gpr_0, 1, quantile, probs = 0.975, na.rm = TRUE)
#bootstrap_results_gpr_0 <- t(bootstrap_results_gpr_0)
#if (length(horizon_sequence) == ncol(bootstrap_results_gpr_0)) {
  # Plot lines
#  for (i in 1:nrow(bootstrap_results_gpr_0)) {
##    lines(horizon_sequence, bootstrap_results_gpr_0[i, ], col = "red")
#  }
#} else {
#  print("Lengths mismatch!")
#}
length(horizon_sequence)
ncol(bootstrap_results_gpr_0)

lower_ci_gpr_0 <- apply(bootstrap_results_gpr_0, 1, quantile, probs = 0.025, na.rm = TRUE)
upper_ci_gpr_0 <- apply(bootstrap_results_gpr_0, 1, quantile, probs = 0.975, na.rm = TRUE)
mean_irf_gpr_1 <- apply(bootstrap_results, 1, mean)
length(mean_irf_gpr_1)
plot(horizon_sequence, bootstrap_results_gpr_0, type = "l", 
     ylim = c(-0.5, 0.5),
     xlab = "Time Lag", ylab = "Real GDP", 
     main = "Impulse Response Function for Germany to supply shock",col = "red")
polygon(
  c(horizon_sequence, rev(horizon_sequence)), 
  c(lower_ci_gpr_1, rev(upper_ci_gpr_1)), 
  col = rgb(0, 0, 1, 0.3), border = NA)
lower_ci_gpr_1 <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)
upper_ci_gpr_1 <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)
gpr0_index  <- (num_lags + 2):(num_lags * 2 + 1)
gpr0_index <- 1:num_lags
print(gpr0_index)
print(dim(bootstrap_results))
lower_ci_gpr_0 <- apply(bootstrap_results[gpr0_index, ], 1, quantile, probs = 0.025, na.rm = TRUE)
upper_ci_gpr_0 <- apply(bootstrap_results[gpr0_index, ], 1, quantile, probs = 0.975, na.rm = TRUE)
plot(horizon_sequence, impulse_response_gpr_1, type = "l", 
     ylim = c(-0.5,0.5),
     xlab = "Time Lag", ylab = "Exchange rate", 
     main = "Impulse Response Function for Russia",col = "red")
lines(horizon_sequence, impulse_response_gpr_0, col = "blue")
polygon(
  c(horizon_sequence, rev(horizon_sequence)), 
  c(lower_ci_gpr_1, rev(upper_ci_gpr_1)), 
  col = rgb(0, 0, 1, 0.3), border = NA)
abline(h=0)
horizon_sequence <- 1:num_lags
polygon_x <- c(horizon_sequence, rev(horizon_sequence))
polygon_y <- c(lower_ci_gpr_0, rev(upper_ci_gpr_0))
polygon(polygon_x, polygon_y, col = rgb(0, 0, 1, 0.3), border = NA)

length(horizon_sequence)
length(impulse_response_gpr_1)
length(lower_ci_gpr_1)
length(upper_ci_gpr_1)
impulse_response_gpr_0 <- coefficients[(num_lags + 2):(2 * (num_lags + 1))]
mean_irf_gpr_0 <- apply(bootstrap_results, 1, mean)
lines(horizon_sequence, mean_irf_gpr_0, col = "blue")
length(coefficients_gpr_0)
lower_ci_gpr_0 <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)
length(lower_ci_gpr_0)
upper_ci_gpr_0 <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)
length(upper_ci_gpr_0)
plot(horizon_sequence, impulse_response_gpr_0, type = "l", 
     ylim = c(-0.2,0.2),
     xlab = "Time Lag", ylab = "GDPG", 
     main = "Impulse Response Function for GPRG = 1")
polygon(
  c(horizon_sequence, rev(horizon_sequence)), 
  c(lower_ci_gpr_0, rev(upper_ci_gpr_0)), 
  col = rgb(0, 0, 1, 0.3), border = NA)






















se <- sqrt(diag(vcov(model_GDPG)))
impulse_response_gpr_1 <- coefficients[2:(num_lags + 1)]
plot(horizon_sequence, impulse_response_gpr_1, type = "l", 
     ylim = range(c(lower_ci_gpr_1, upper_ci_gpr_1)),
     xlab = "Time Lag", ylab = "GDPG", 
     main = "Impulse Response Function for GPRG = 1")
plot(horizon_sequence, impulse_response_gpr_1, type = "l", ylim = range(c(lower_ci_gpr_1, upper_ci_gpr_1)),
     xlab = "Time Lag", ylab = "GDPG", main = "Impulse Response Function")
length(impulse_response_gpr_1)
coefficients_gpr_0 <- coefficients[(num_lags + 2):(2 * (num_lags + 1))]
lower_ci_gpr_1 <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)[-(1:(num_lags+1))]
length(lower_ci_gpr_1)
upper_ci_gpr_1 <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)[-(1:(num_lags+1))]
plot(horizon_sequence, impulse_response_gpr_1, type = "l", 
     ylim = range(c(lower_ci_gpr_1, upper_ci_gpr_1)),
     xlab = "Time Lag", ylab = "GDPG", 
     main = "Impulse Response Function for GPRG = 1")

lower_ci_gpr_0 <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)[-1]
upper_ci_gpr_0 <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)[-1]
plot(horizon_sequence, impulse_response_gpr_1, type = "l", ylim = range(c(lower_ci_gpr_1, upper_ci_gpr_1)),
     xlab = "Time Lag", ylab = "GDPG")
mean_irf <- apply(bootstrap_results, 1, mean)
se_irf <- apply(bootstrap_results, 1, sd)
upper_bound <- mean_irf + se_irf
lower_bound <- mean_irf - se_irf
plot(horizon_sequence, mean_irf, type = "l", ylim = c(-0.05,0.05),
     xlab = "Time Lag", ylab = "GDPG")
polygon(c(horizon_sequence, rev(horizon_sequence)), c(lower_bound, rev(upper_bound)), 
        col = rgb(0, 0, 1, 0.3), border = NA)
lines(horizon_sequence, impulse_response_gpr_1, col = "red")
lines(horizon_sequence, lower_ci_gpr_1, col = "blue", lty = 2)
lines(horizon_sequence, upper_ci_gpr_1, col = "blue", lty = 2)
polygon(c(horizon_sequence, rev(horizon_sequence)), c(lower_ci_gpr_1, rev(upper_ci_gpr_1)), col = "skyblue", border = NA)
polygon(c(horizon_sequence, rev(horizon_sequence)), c(lower_ci_gpr_0, rev(upper_ci_gpr_0)), col = "lightgreen", border = NA)
lines(horizon_sequence, coefficients_gpr_0, type = "l", col = "green")
lines(horizon_sequence, impulse_response_gpr_1, type = "l", col = "blue")

length(coefficients_gpr_0)
horizon_sequence1 <- 0:num_lags
length(horizon_sequence1)









lower_ci_gpr_1 <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)
upper_ci_gpr_1 <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)
lower_ci_gpr_1 <- lower_ci_gpr_1[-1]
upper_ci_gpr_1 <- upper_ci_gpr_1[-1]
par(mfrow = c(2, 1), mar = c(5, 4, 4, 2) + 0.1)
plot(horizon_sequence, impulse_response_gpr_1, type = "l", ylim = c(min(lower_ci_gpr_1), max(upper_ci_gpr_1)),
     xlab = "Time Lag", ylab = "GDPG")
plot(horizon_sequence, impulse_response_gpr_1, type = "l", ylim = c(min(lower_ci_gpr_1), max(upper_ci_gpr_1)),
     xlab = "Time Lag", ylab = "GDPG")
lines(horizon_sequence, lower_ci_gpr_1, col = "red", lty = 2)
lines(horizon_sequence, upper_ci_gpr_1, col = "red", lty = 2)
length(horizon_sequence)
length(impulse_response_gpr_1)
abline(h=0)
impulse_response_gpr_0 <- coefficients[1] + coefficients[(num_lags + 2):(2 * (num_lags + 1))]
lower_ci_gpr_0 <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)
upper_ci_gpr_0 <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)
plot(horizon_sequence, impulse_response_gpr_0, type = "l", ylim = c(min(lower_ci_gpr_0), max(upper_ci_gpr_0)),
     xlab = "Time Lag", ylab = "GDPG")
lines(horizon_sequence, lower_ci_gpr_0, col = "red", lty = 2)
lines(horizon_sequence, upper_ci_gpr_0, col = "red", lty = 2)



formula <- as.formula(paste("GDPG ~",
                            "I(GPRG_lag1 >= median_GPRG_lag1) * (", paste0("Oilsupplyshock_lag", 0:num_lags, collapse = " + "),") + ",
                            "(1 - I(GPRG_lag1 >= median_GPRG_lag1)) * (", paste0("Oilsupplyshock_lag", 0:num_lags, collapse = " + "), ")"))
print(formula)
model_GDPG <- lm(formula, data = cbindallvariables)
summary(model_GDPG)
bootstrap_irf <- function(model_GDPG, cbindallvariables, num_bootstrap) {
  irf_samples <- replicate(num_bootstrap, {
    # Sample with replacement
    boot_data <- cbindallvariables[sample(nrow(cbindallvariables), replace = TRUE), ]
    # Predict impulse response
    predict(model_GDPG, newdata = boot_data)
  })
  return(irf_samples)
}
num_bootstrap <- 2000
irf_samples <- bootstrap_irf(model, your_data, num_bootstrap)
irf_mean <- apply(irf_samples, 1, mean)
irf_sd <- apply(irf_samples, 1, sd)










new_GPRG_1 <- cbindallvariables
new_GPRG_1$GPRG_lag1 <- 1
new_GPRG_0 <- cbindallvariables
new_GPRG_0$GPRG_lag1 <- 0
impulse_response_gpr_1 <- predict(model_GDPG, newdata = new_GPRG_1)
impulse_response_gpr_0 <- predict(model_GDPG, newdata = new_GPRG_0)
par(mfrow = c(2, 1))
length(impulse_response_gpr_1)
length(impulse_response_gpr_0)
length(1:num_lags)
str(impulse_response_gpr_1)
impulse_response_gpr_1 <- na.omit(impulse_response_gpr_1)
impulse_response_gpr_0 <- na.omit(impulse_response_gpr_0)
plot(1:num_lags, impulse_response_gpr_1[1:num_lags], type = "l", xlab = "Time Lag", ylab = "GDPG", 
     main = "Impulse Response Function of GDPG when GPRG = 1")

plot(1:num_lags, impulse_response_gpr_0[1:num_lags], type = "l", xlab = "Time Lag", ylab = "GDPG", 
     main = "Impulse Response Function of GDPG when GPRG = 0")
plot(1:num_lags, impulse_response_gpr_1[1:num_lags], type = "l", xlab = "Time Lag", ylab = "GDPG", 
     main = "Impulse Response Function of GDPG")
# Add the impulse response function of GDPG when GPRG_lag1 = 0 to the same plot
lines(1:num_lags, impulse_response_gpr_0[1:num_lags], col = "red")






cbindGPReconomy <- as.data.frame(cbindGPReconomy)
data_subset <- cbindGPReconomy[, c("CPIG", "Oilsupplyshock","GPRG")]
num_lags <-12
data_subset <- data_subset %>%
  mutate(Oilsupplyshock_lag0 = Oilsupplyshock)  # Create lag 0 manually

for (lag in 1:num_lags) {  
  lagged_var_name <- paste0("Oilsupplyshock_lag", lag)
  data_subset <- data_subset %>% mutate(!!lagged_var_name := lag(Oilsupplyshock, lag))
}

#formula <- as.formula(paste("CPIG ~ ", paste0("GPRG * Oilsupplyshock_lag", 0:num_lags, collapse = " + ")))
formula1 <- as.formula(paste("ExrateG ~ GPRG:(", paste0("Oilsupplyshock_lag", 0:num_lags, collapse = " + "), ")"))
print(formula1)
model_GPR <- lm(formula1, data = data_subset)
print(model_GPR)
summary(model_GPR)
####irf for gpr###
data_subset <- cbindGPReconomy[, c("ExrateG", "Oilaggregatedemandshock", "GPRG")]
num_lags <- 12
for (lag in 0:num_lags) {  
  lagged_var_name <- paste0("Oilaggregatedemandshock_lag", lag)
  data_subset <- data_subset %>% mutate(!!lagged_var_name := lag(Oilaggregatedemandshock, lag))
}

# Step 2: Define the formula for the linear model
formula <- as.formula(paste("ExrateG ~ GPRG * (", paste0("Oilaggregatedemandshock_lag", 0:num_lags, collapse = " + "), ")"))

print(formula)
# Step 3: Fit the linear model
model <- lm(formula, data = data_subset)

# Step 4: Compute the impulse response function
num_horizons <- num_lags  # Number of horizons for the impulse response
impulse_response <- matrix(NA, nrow = num_horizons, ncol = 1)
for (horizon in 1:num_horizons) {
  impulse_response[horizon, 1] <- coef(model)[paste0("Oilaggregatedemandshock_lag", horizon)]
}

# Step 5: Calculate error bands for the impulse response function
num_bootstraps <- 20000  # Number of bootstrap iterations
bootstrap_results <- matrix(NA, nrow = num_horizons, ncol = num_bootstraps)  # Matrix to store bootstrap results
for (i in 1:num_bootstraps) {
  # Resample data with replacement
  resampled_data <- data_subset[sample(nrow(data_subset), replace = TRUE), ]
  
  # Fit linear model to resampled data
  boot_model <- lm(formula, data = resampled_data)
  
  # Compute impulse response for each horizon
  for (horizon in 1:num_horizons) {
    bootstrap_results[horizon, i] <- coef(boot_model)[paste0("Oilaggregatedemandshock_lag", horizon)]
  }
}

# Compute error bands
lower_ci <- apply(bootstrap_results, 1, quantile, probs = 0.025,na.rm = TRUE)
upper_ci <- apply(bootstrap_results, 1, quantile, probs = 0.975,na.rm = TRUE)
ylim_min <- min(c(bootstrap_results, lower_ci))
ylim_max <- max(c(bootstrap_results, upper_ci))
mean_irf <- apply(bootstrap_results, 1, mean)
# Calculate standard error of impulse response
se_irf <- apply(bootstrap_results, 1, sd)
# Calculate upper and lower bounds of one standard error band
upper_bound <- mean_irf + se_irf
lower_bound <- mean_irf - se_irf
# Plot mean impulse response
plot(1:num_lags, mean_irf, type = "l", xlab = "Horizon", ylab = "Exchange rate", 
     main = "Oil aggregate demand shock", col = "red", ylim = c(ylim_min,ylim_max))
polygon(c(1:num_lags, rev(1:num_lags)), c(lower_bound, rev(upper_bound)), 
        col = rgb(0, 0, 1, 0.3), border = NA)
abline(h=0)








# Plot impulse response function with error bands
horizon_sequence <- 1:num_horizons
plot(horizon_sequence, impulse_response[, 1], type = "l", xlab = "Horizon", ylab = "Exchange rate",
     ylim = range(c(lower_ci, upper_ci)))
polygon(c(1:num_lags, rev(1:num_lags)), c(lower_ci, rev(upper_ci)), 
        col = rgb(0, 0, 1, 0.3), border = NA)
lines(horizon_sequence, lower_ci, col = "blue", lty = 2)  # Lower error band
lines(horizon_sequence, upper_ci, col = "blue", lty = 2)  # Upper error band
abline(h=0)
















 

###2 способ для проверки правильности предыдущего кода####
cbinddataall_noNA <- na.omit(cbinddataall)
View(cbinddataall_noNA)
cbinddataall_noNA <- as.data.frame(cbinddataall_noNA)
data_subset <- cbinddataall_noNA[, c("difflog_ExrateGermany", "Oilaggregatedemandshock")]
View(cbindGPReconomy)
View(data_subset)
lags <- 12
for(lag in 0:lags) {
  data_subset[, paste0("Oilaggregatedemandshock_lag", lag)] <- lag(data_subset$Oilaggregatedemandshock, lag)
}
fit_dlm <- function(data_subset) {
  lm_model <- lm(difflog_ExrateGermany ~ ., data = data_subset)
  return(lm_model)
}
boot_fun <- function(data_subset, indices) {
  sampled_data <- data_subset[indices, ]
  fit_dlm(sampled_data)
}
n_obs <- nrow(data_subset)
num_strata <- ceiling(n_obs / 4)
strata <- rep(1:num_strata, each = 4)
strata <- strata[seq_len(n_obs)]
install.packages("boot")
library(boot)
print(dim(data_subset))
print(length(strata))
bootstrap_results <- boot(data = data_subset, statistic = boot_fun, R = 20000, strata = strata)
boot_coef <- t(sapply(bootstrap_results$t, coef))
coef_mean <- apply(boot_coef, 2, mean)
coef_se <- apply(boot_coef, 2, sd)
irf <- deltaMethod(boot_coef, "Oilaggregatedemandshock_lag0")
irf_mean <- irf$Estimate
irf_se <- irf$SE
irf_upper <- irf_mean + irf_se
irf_lower <- irf_mean - irf_se
horizon_sequence <- 0:lags
plot(horizon_sequence, irf_mean, type = "l", ylim = c(min(irf_lower), max(irf_upper)), 
     xlab = "Horizon", ylab = "Impulse Response", main = "Impulse Response of CPI to Oil Supply Shock")
lines(horizon_sequence, irf_upper, lty = 2)
lines(horizon_sequence, irf_lower, lty = 2)











data_subset <- cbinddataall_noNA[, c("difflog_ExrateRussia", "Oilaggregatedemandshock")]
install.packages('boot')
library(boot)
lags <- 12
for (lag in 1:lags) {
  data_subset[, paste0("supply_lag", lag)] <- lag(data_subset$Oilaggregatedemandshock, lag)
}

# Step 3: Fit the OLS model
ols_model <- lm(difflog_ExrateRussia ~ ., data = data_subset)
summary(ols_model)
# Step 4: Compute Standard Errors using Block Bootstrap with block size 4
block_bootstrap <- function(data, indices) {
  sampled_data <- data[indices, ]
  block_indices <- split(seq_len(nrow(sampled_data)), (seq_len(nrow(sampled_data)) - 1) %/% 4)
  
  ols_model <- lm(difflog_ExrateRussia ~ ., data = sampled_data)
  return(coef(ols_model)["Oilaggregatedemandshock"])
}

bootstrap_results <- boot(data = data_subset, statistic = block_bootstrap, R = 20000)

# Step 5: Compute Impulse Response Function (IRF) with horizon 12
irf <- coef(ols_model)["Oilaggregatedemandshock"]
irf_horizon <- 12

# Step 6: Plot Impulse Response Function
irf_values <- rep(irf, irf_horizon)
horizon_sequence <- 1:irf_horizon

# Plot the IRF
plot(horizon_sequence, irf_values, type = "l", xlab = "Horizon", ylab = "Impulse Response", 
     main = "Impulse Response Function (IRF) of GDP to Supply Shock with 12 Lags")

library(ggplot2)

# Assuming your dataset is named 'data'
# Assuming your data has columns 'GDP_Russia' and 'supply_shock'

# Step 1: Create Lagged Variables
lags <- 12
for (lag in 1:lags) {
  data_subset[, paste0("supply_shock_lag", lag)] <- lag(data_subset$Oilaggregatedemandshock, lag)
}

# Step 2: Fit OLS Model
ols_model <- lm(difflog_ExrateRussia~ ., data = data_subset)
summary(ols_model)
# Step 3: Compute Impulse Response Function (IRF)
irf <- coef(ols_model)[grep("^supply_shock_lag", names(coef(ols_model)))]

# Step 4: Plot IRF
horizon <- 1:12
irf_df <- data.frame(Horizon = horizon, IRF = irf)

ggplot(data = irf_df, aes(x = Horizon, y = IRF)) +
  geom_line() +
  geom_point() +
  labs(x = "Horizon", y = "Impulse Response", title = "Impulse Response Function of GDP to Supply Shocks"+ 
   coord_cartesian(ylim = c(-15, 15)))







cbinddataall_noNA <- as.data.frame(cbinddataall_noNA)
data_subset <- cbinddataall_noNA[, c("difflog_ExrateRussia", "Oilsupplyshock")]
library(ggplot2)
library(boot)
lags <- 12
for (lag in 1:lags) {
  data_subset[, paste0("supply_shock_lag", lag)] <- lag(data_subset$Oilsupplyshock, lag)
}

# Step 2: Fit OLS Model
ols_model <- lm(difflog_ExrateRussia ~ ., data = data_subset)
summary(ols_model)
# Step 3: Compute Impulse Response Function (IRF)
irf <- coef(ols_model)[grep("^supply_shock_lag", names(coef(ols_model)))]
# Assuming your data_subset already exists and contains the lagged supply shock variables
# Assuming you've already fit the OLS model and computed the IRF as per your previous code

# Step 5: Compute Standard Errors using Block Bootstrap with block size 4
horizon <- 1:12
block_bootstrap <- function(data, indices) {
  sampled_data <- data[indices, ]
  ols_model <- lm(difflog_ExrateRussia ~ ., data = sampled_data)
  irf <- coef(ols_model)[grep("^supply_shock_lag", names(coef(ols_model)))]
  return(irf)
}

bootstrap_results <- boot(data = data_subset, statistic = block_bootstrap, R = 20000)

# Step 6: Compute Confidence Intervals
irf_ci <- boot.ci(bootstrap_results, type = "perc")
# Extract the bootstrap statistics for the IRF
bootstrap_stats <- t(bootstrap_results$t)

# Calculate the confidence intervals
lower_ci <- apply(bootstrap_stats, 2, quantile, probs = 0.025)
upper_ci <- apply(bootstrap_stats, 2, quantile, probs = 0.975)

# Step 9: Create irf_df data frame
irf_df <- data.frame(Horizon = horizon, IRF = irf, lower = rep(NA, length(horizon)), upper = rep(NA, length(horizon)))
for (i in 1:length(horizon)) {
  irf_df$lower[i] <- lower_ci[i]
  irf_df$upper[i] <- upper_ci[i]
}
irf_df <- data.frame(Horizon = horizon, IRF = irf, lower = lower_ci, upper = upper_ci)
ggplot(data = irf_df, aes(x = Horizon, y = IRF)) +
  geom_line() +
  
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +  # Confidence interval
  labs(x = "Horizon", y = "Exhange rate", 
       title = "Oil supply shock") +
  coord_cartesian(ylim = c(-0.5, 0.5))+ # Adjust y-axis limits if needed
  theme_minimal()










#####use this variant###############
cbinddataall_noNA <- as.data.frame(cbinddataall_noNA)
cbinddataallexratediff_noNA <- as.data.frame(cbinddataallexratediff_noNA)
data_subset <- cbinddataall_noNA[, c("difflog_ExrateRussia", "Oilspecificdemandshock")]
library(ggplot2)
library(boot)
lags <- 12
for (lag in 1:lags) {
  data_subset[, paste0("specific_shock_lag", lag)] <- lag(data_subset$Oilspecificdemandshock, lag)
}

# Step 2: Fit OLS Model
ols_model <- lm(difflog_ExrateRussia~ ., data = data_subset)
summary(ols_model)
# Step 3: Compute Impulse Response Function (IRF)
irf <- coef(ols_model)[grep("^specific_shock_lag", names(coef(ols_model)))]
#cumulative_irf <- cumsum(irf)
horizon <- 1:12
block_size <- 4  # Block size for block bootstrap

block_bootstrap <- function(data, indices) {
  sampled_data <- data[indices, ]
  num_obs <- nrow(sampled_data)
  num_blocks <- ceiling(num_obs / block_size)
  
  # Randomly sample block indices
  block_indices <- lapply(1:num_blocks, function(i) {
    start_idx <- ((i - 1) * block_size) + 1
    end_idx <- min(i * block_size, num_obs)
    sample(start_idx:end_idx, replace = TRUE)
  })
  
  # Flatten block indices
  block_indices <- unlist(block_indices)
  
  # Fit OLS model and compute IRF
  ols_model <- lm(difflog_ExrateRussia ~ ., data = sampled_data[block_indices, ])
  irf <- coef(ols_model)[grep("^specific_shock_lag", names(coef(ols_model)))] 
  return(irf)

}

bootstrap_results <- boot(data = data_subset, statistic = block_bootstrap, R = 20000)
# Step 6: Compute Confidence Intervals
irf_ci <- boot.ci(bootstrap_results, type = "perc")
# Extract the bootstrap statistics for the IRF
bootstrap_stats <- t(bootstrap_results$t)

# Calculate the confidence intervals
lower_ci <- apply(bootstrap_stats, 2, quantile, probs = 0.025)
upper_ci <- apply(bootstrap_stats, 2, quantile, probs = 0.975)
#lower_ci_cumsum <- cumsum(lower_ci)
#upper_ci_cumsum <- cumsum(upper_ci)
#irf <- irf * (-1)
# Step 9: Create irf_df data frame
irf_df <- data.frame(Horizon = horizon, IRF = irf, lower = lower_ci[1:12],
                     upper = upper_ci[1:12])
ggplot(data = irf_df, aes(x = Horizon, y = IRF)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Horizon", y = "Exchange rate", 
       title = "Oil-market specific shock") +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  theme_minimal()





for (i in 1:length(horizon)) {
  irf_df$lower[i] <- lower_ci[i]
  irf_df$upper[i] <- upper_ci[i]
}
irf_df <- data.frame(Horizon = horizon, IRF = irf, lower = lower_ci, upper = upper_ci)
ggplot(data = irf_df, aes(x = Horizon, y = IRF)) +
  geom_line() +
  
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +  # Confidence interval
  labs(x = "Horizon", y = "Impulse Response", 
       title = "Impulse Response Function of GDP to Supply Shocks with Block Bootstrap") +
  coord_cartesian(ylim = c(-0.5, 0.5))+ # Adjust y-axis limits if needed
  theme_minimal()








cbinddataall_noNA <- as.data.frame(cbinddataall_noNA)
data_subset <- cbinddataall_noNA[, c("difflog_GDPRussia", "Oilspecificdemandshock")]
# Load required libraries
library(boot)
library(ggplot2)

# Define the lag length
lags <- 12

# Create lagged variables
for (lag in 1:lags) {
  cbinddataall_noNA[, paste0("specific_shock_lag", lag)] <- lag(cbinddataall_noNA$Oilspecificdemandshock, lag)
}

# Fit OLS model
ols_model <- lm(difflog_GDPRussia ~ ., data = cbinddataall_noNA)
summary(ols_model)

# Extract coefficients related to the specific shock
irf <- coef(ols_model)[grep("^specific_shock_lag", names(coef(ols_model)))]

# Define horizon
horizon <- 1:12

# Define block bootstrap function with block size
block_size <- 4
block_bootstrap <- function(data, indices) {
  sampled_data <- data[indices, ]
  ols_model <- lm(difflog_GDPRussia ~ ., data = sampled_data)
  irf <- coef(ols_model)[grep("^specific_shock_lag", names(coef(ols_model)))]
  return(irf)
}

# Perform block bootstrap
bootstrap_results <- boot(data = cbinddataall_noNA, statistic = block_bootstrap, R = 20000)

# Compute confidence intervals
irf_ci <- boot.ci(bootstrap_results, type = "perc")
lower_ci <- apply(bootstrap_stats, 2, quantile, probs = 0.025)
upper_ci <- apply(bootstrap_stats, 2, quantile, probs = 0.975)

# Create IRF data frame
irf_df <- data.frame(Horizon = horizon, IRF = irf, lower = lower_ci[1:12], upper = upper_ci[1:12])

# Plot IRF
ggplot(data = irf_df, aes(x = Horizon, y = IRF)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Horizon", y = "Real GDP", 
       title = "Oil-market specific demand shock") +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_minimal()



###этоэтоэтоэтоэтоэтоэтоэто#####
data_subset <- cbinddataall_noNA[, c("difflog_CPIRussia", "Oilsupplyshock")]
View(data_subset)
num_lags <- 12
data_subset <- data_subset %>%
  mutate(Oilsupplyshock_lag0 = Oilsupplyshock)  # Create lag 0 manually

for (lag in 1:num_lags) {  
  lagged_var_name <- paste0("Oilsupplyshock_lag", lag)
  data_subset <- data_subset %>% mutate(!!lagged_var_name := lag(Oilsupplyshock, lag))
}
#for (lag in 1:lagsdependent) {
#lagged_var_name <- paste0("gdp", lag)
#data_subset <- data_subset %>% mutate(!!lagged_var_name := lag(gdp, lag))
#}
formula <- as.formula(paste("difflog_CPIRussia ~", paste0("Oilsupplyshock_lag", 0:num_lags, collapse = " + ")))

#View(cbindall_noNA_dataframe)
model_gdp <- lm(formula, data = data_subset)
#TS_DATA <- ts(cbindall_noNA_dataframe)
#var_model <- VAR(TS_DATA, p = num_lags, type = "const")
summary(model_gdp)
#coefficients <- coef(model_gdp) 
# Compute impulse response functions
block_bootstrap <- function(data, indices) {
  sampled_data <- data[indices, ]
  num_blocks <- nrow(sampled_data) / 4  # Block size is 4
  bootstrap_results <- matrix(NA, nrow = num_lags, ncol = 12)  # Initialize matrix to store results
  
  for (i in 1:20000) {
    # Randomly sample block indices
    sampled_indices <- unlist(lapply(1:num_blocks, function(i) sample(1:num_blocks, replace = TRUE)))
    
    # Fit the linear regression model using sampled data
    model_gdp <- lm(formula, data = sampled_data[sampled_indices, ])
    coefficients <- coef(model_gdp)
    
    # Compute IRF for each horizon
    irf <- numeric(num_lags)
    for (horizon in 1:12) {
      for (lag in 1:num_lags) {
        irf[lag] <- irf[lag] + sum(coefficients[paste0("Oilsupplyshock_lag", 1:lag)])
      }
      bootstrap_results[, horizon] <- irf
    }
  }
  
  return(bootstrap_results)
}

# Perform block bootstrap
bootstrap_results <- block_bootstrap(data_subset, 1:nrow(data_subset))
lower_ci <- apply(bootstrap_results, 1, quantile, probs = 0.025, na.rm = TRUE)
upper_ci <- apply(bootstrap_results, 1, quantile, probs = 0.975, na.rm = TRUE)
print(lower_ci)
print(upper_ci)
ylim_min <- min(c(bootstrap_results, lower_ci))
ylim_max <- max(c(bootstrap_results, upper_ci))
# Plot impulse response functions for all horizons
plot(1:num_lags, bootstrap_results[, 1], type = "l", xlab = "Lag", ylab = "CPI", 
     main = "Oil supply shock", col = "red", ylim = c(-5,4))
matlines(1:num_lags, t(bootstrap_results), lty = 1, col = "blue", type = "l")

library(ggplot2)
bootstrap_results_df <- as.data.frame(t(bootstrap_results))

bootstrap_results_df$lag <- 1:num_lags

# Create a data frame for lower and upper CI
ci_df <- data.frame(lag = 1:num_lags, lower = lower_ci, upper = upper_ci)

# Plotting
ggplot() +
  geom_line(data = bootstrap_results_df, aes(x = lag, y = V1), color = "red") +
  geom_ribbon(data = ci_df, aes(x = lag, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(x = "Lag", y = "Real GDP", 
       title = "Oil aggregate demand shock") +
  theme_minimal()
abline(h = 0)
for (lag in 1:num_lags) {
  polygon(c(lag - 0.1, lag + 0.1, lag + 0.1, lag - 0.1), 
          c(lower_ci[lag], lower_ci[lag], upper_ci[lag], upper_ci[lag]), 
          col = rgb(0, 0, 1, 0.2), border = NA)
}
for (lag in 1:num_lags) {
  lines(c(lag, lag), c(lower_ci[lag], upper_ci[lag]), col = "blue", lwd = 1)
}

for (lag in 1:num_lags) {
  polygon(c(lag - 0.1, lag - 0.1, lag + 0.1, lag + 0.1), 
          c(lower_ci[lag], lower_ci[lag], upper_ci[lag], upper_ci[lag]), 
          col = rgb(0, 0, 1, 0.2), border = NA)
}

for (lag in 1:num_lags) {
  lines(c(lag, lag), c(lower_ci[lag], upper_ci[lag]), col = "blue", lwd = 1)
  polygon(c(lag - 0.1, lag - 0.1, lag + 0.1, lag + 0.1), 
          c(lower_ci[lag], upper_ci[lag], upper_ci[lag], lower_ci[lag]), 
          col = rgb(0, 0, 1, 0.2), border = NA)
}
for (lag in 1:num_lags) {
  lines(c(lag, lag), c(lower_ci[lag], upper_ci[lag]), col = "blue", lwd = 1)
}






