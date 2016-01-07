#######################################################################
##  Filename: overfitting_sim.r
##  Purpose: Plots to demonstate how boosting works and CART fit
##  Uses data from: Randomly generated data 
##  Assumes packages: stringr, rpart
##  Output to: plots/boosting_sim.pdf; plots/cart_func.pdf.pdf
##  Last Edited: 06 January 2016
##  Christopher Boylan, Penn State University
#######################################################################
## Load Packages and Prepare Directory
#######################################################################
rm(list=ls()) ## remove objects from working environment
set.seed(1804) ## set seed
library(stringr); library(rpart) ## load packages
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "R", "../", "./") ## set directory 
#######################################################################
## Generate data
#######################################################################
n=500 ## number of observations
x=sort(runif(n, max=2)*2*pi) ## generate x values
y=sin(x)+rnorm(n,0,.2) ## response values
df=data.frame(x = x,r = y) ## initialize residuals = y
yp <- rep(0, nrow(df)) ## initialize g(x) = 0
YP = yp ## YP matrix to collect predictions from each iteration
FX = yp ## FX matrix to collect evolving function after each iteration
R = df$r ## R matrix to collect residuals after each iteration
#######################################################################
## Boosting simulation
#######################################################################
## Simulation
for(t in 1:50){ ## 50 iterations
  fit = rpart(r~x, data = df, control = rpart.control(maxdepth = 1)) ## fit cart
  yp = predict(fit) ## get predicted values
  df$r = df$r - yp ## subtract predicted values from residuals
  R = cbind(R, df$r) ## add updated values for residuals to R matrix
  YP = cbind(YP, yp) ## add updated predicted values to YP matrix
  FX = cbind(FX, apply(YP, 1, sum)) ## add updated overall function to YP matrix
}
## Plot evolution of fit
iter <- c(1,6,11,51)
pdf(paste0(dir, 'plots/boosting_sim.pdf'), width = 10, height = 6.5)
par(mfcol = c(nrows = 2, ncols = 4)) ## panel for plots
for (i in 1:length(iter)){
plot(df$x,R[,1], xlab = 'X', ylab = 'Y', ylim = c(-1.5, 1.5),col = 'gray',
     main = paste('t: ', iter[i]-1)) ## scatterplot of data
lines(df$x, FX[, iter[i]], type = 'l',col = 'blue',lwd = 1.5) ## blue line for overall fit
plot(df$x, R[, iter[i]], xlab ='X', ylab = 'Residuals', ## scatterplot of residuals
     col='gray', ylim = c(-1.5,1.5))
lines(df$x, YP[, iter[i]], type = 'l',col = 'orange',lwd = 1.5) ## orange line for fit t
}
dev.off()
#######################################################################
## CART fit
#######################################################################
fit1=rpart(y~x,data = df,control = rpart.control(maxdepth = 1)) ## two region model
yp1=predict(fit1) ## predicted values
fit2=rpart(y~x,data = df,control = rpart.control(maxdepth = 5)) ## ten region model
yp2=predict(fit2) ## predicted values
pdf(paste0(dir, 'plots/cart_func.pdf'), width = 10, height = 6.5)
plot(df$x, y, xlab = 'X', ylab = 'Y', ylim = c(-1.5,1.5),col = 'gray') ## data
lines(df$x, yp1, type = 'l',lty = 1,lwd = 1.5,col = 'blue') ## blue = two region model
lines(df$x, yp2, type = 'l',lty = 1,lwd = 1.5,col = 'orange')## orange = ten region model
legend('topright', c("J=2", "J=10" ), text.col = c('blue', 'orange'), bty = "n", 
       cex = 1.5) ## add legend
dev.off()