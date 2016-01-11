#######################################################################
##  Filename: partial_dependence.R
##  Purpose: Demonstrate partial dependence plots in mlr
##  Uses data from: data/SambnisImp.csv
##                  From Muchlinski et al. (2015)              
##  Assumes packages: mlr, parrallelMap, stringr
##  Output to: plots/pdplot.pdf
##  Last Edited: 06 January 2016
##  Christopher Boylan, Penn State University
#######################################################################
## Load Packages and Prepare Directory
#######################################################################
rm(list=ls()) ## remove objects from working environment
set.seed(1804) ## set seed
library(mlr); library(parallelMap); library(stringr)
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "R", "../", "./") ## set directory 
#######################################################################
## Prepare Data
#######################################################################
data <- read.csv(file=paste0(dir,"data/SambnisImp.csv")) ## import Muchlinski data
## subset data
data <- data[,c("warstds", "ager", "agexp", "anoc", "army85", "autch98", "auto4",
                   "autonomy", "avgnabo", "centpol3", "coldwar", "decade1", "decade2",
                   "decade3", "decade4", "dem", "dem4", "demch98", "dlang", "drel",
                   "durable", "ef", "ef2", "ehet", "elfo", "elfo2", "etdo4590",
                   "expgdp", "exrec", "fedpol3", "fuelexp", "gdpgrowth", "geo1", "geo2",
                   "geo34", "geo57", "geo69", "geo8", "illiteracy", "incumb", "infant",
                   "inst", "inst3", "life", "lmtnest", "ln_gdpen", "lpopns", "major", "manuexp", "milper",
                   "mirps0", "mirps1", "mirps2", "mirps3", "nat_war", "ncontig",
                   "nmgdp", "nmdp4_alt", "numlang", "nwstate", "oil", "p4mchg",
                   "parcomp", "parreg", "part", "partfree", "plural", "plurrel",
                   "pol4", "pol4m", "pol4sq", "polch98", "polcomp", "popdense",
                   "presi", "pri", "proxregc", "ptime", "reg", "regd4_alt", "relfrac", "seceduc",
                   "second", "semipol3", "sip2", "sxpnew", "tnatwar", "trade",
                   "warhist", "xconst")]
## convert warstds to factor
data$warstds<-factor(
  data$warstds,
  levels = c(0,1),
  labels = c("peace", "war"))
## rename varaibles to be plotted
names(data)[names(data) == "sxpnew"] <- 'Primary_Commodity_Exports'
names(data)[names(data) == "illiteracy"] <- 'Illiteracy'
names(data)[names(data) == "agexp"] <- 'Agricultural_Exports'
#######################################################################
## Estimate Models
#######################################################################
parallelStart("multicore") ## Use multiple cores
class.task <- makeClassifTask(id = "cv", data = data, ## classification task
                              target = "warstds", positive='war')
lrn <- makeLearner("classif.gbm", predict.type = "prob") ## Use gbm
fit <- train(lrn, class.task) ## train model
rdesc <- makeResampleDesc("CV", iters = 10L) ## use ten fold cv
pred <- resample(lrn, class.task, rdesc, auc) ## run model
#######################################################################
## Plot partial dependence
#######################################################################
## Select predictors to plot
features <- c('Agricultural_Exports', 'Illiteracy', 'Primary_Commodity_Exports')
## Generate pd data
pd <- generatePartialPredictionData(fit, class.task, features,interaction=F) 
## Generate pd plot
p <- plotPartialPrediction(pd) + theme_bw() + ylab('Probability of Civil War Onset')
## Save plot
ggsave(paste0(dir, "plots/pdplot.pdf"), p, height = 6, width = 8)
#######################################################################
## Search for Interaction
#######################################################################
## Select predictors to plot
features <- c('Agricultural_Exports', 'Illiteracy')
## Generate pd data
pd <- generatePartialPredictionData(fit, class.task, features,interaction=T) 
## Generate pd plot
p <- plotPartialPrediction(pd) + theme_bw() + ylab('Probability of Civil War Onset')
## Save plot
ggsave(paste0(dir, "plots/pdinteraction.pdf"), p, height = 6, width = 8)