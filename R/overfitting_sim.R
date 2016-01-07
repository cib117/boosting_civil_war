#######################################################################
##  Filename: overfitting_sim.r
##  Purpose: Graphically demonstrate concept of overfitting
##  Uses data from: Randomly generated data 
##  Assumes packages: stringr
##  Output to: plots/overfitting.pdf
##  Last Edited: 06 January 2016
##  Christopher Boylan, Penn State University
#######################################################################
## Load Packages and Prepare Directory
#######################################################################
rm(list=ls()) ## remove objects from working environment
set.seed(1804) ## set seed
library(stringr)
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "R", "../", "./") ## set directory 
#######################################################################
## Prepare Data
#######################################################################
x <- c(2, 5, 7, 10, 15, 20, 30, 40, 50) # generate feature values
y = log(x) ## outcome data
y <- y + rnorm(length(x), .1, .1) ## add random noise
#######################################################################
## Run Models
#######################################################################
mod1 <- lm(y ~ x) ## fit linear model
mod2 <- lm(y ~ poly(x, 2, raw  = TRUE)) ## fit first order polynomial model
mod3 <- lm(y ~ poly(x, 6, raw = TRUE)) ## fit sixth order polynomial model
#######################################################################
## Make Plot
#######################################################################
pdf(paste0(dir, 'plots/overfitting.pdf'), width = 10, height = 5) ## plot data
par(mfrow = c(1, 3)) ## One row, three columns
plot(x, y, main = 'Linear') ## Plot linear fit
abline(mod1, col = 'blue') ## black points and blue line
plot(x, y, main = 'Second Order Polynomial') ## Plot second order fit
points(x, predict(mod2), type = 'l', col = 'blue') ## black points and blue line
plot(x, y, main = 'Sixth Order Polynomial') ## Plot sixth order fit
points(x, predict(mod3), type = 'l', col = 'blue') ## black points and blue line
dev.off()