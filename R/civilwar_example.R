#######################################################################
##  Filename: civil_war.R
##  Purpose: Demonstrate partial dependence plots in mlr
##  Uses data from: data/SambnisImp.csv
##                  From Muchlinski et al. (2015)              
##  Assumes packages: caret, parrallelMap, stringr, ggplot2
##  Output to: plots/pdplot.pdf
##  Last Edited: 07 January 2016
##  Christopher Boylan, Penn State University
#######################################################################
## Load Packages, Prepare Directory, Set Number of Cores
#######################################################################
rm(list=ls())
library(caret); library(doMC); library(stringr); library(ggplot2) ## load packages
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "code", "../", "./") ## set directory 
cores <- detectCores() ## detect number of cores
registerDoMC(cores=cores) # distributing workload over multiple cores for faster computaiton
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
#######################################################################
## Estimate models
#######################################################################
## Set seeds for cv for reproducibility
set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:10) seeds[[i]] <- sample.int(1000, 5)
seeds[[11]] <- sample.int(1000, 1)
## Set resampling parameters
tc<-trainControl(method="cv", 
                 number=10,#creates CV folds - 10 for this data
                 summaryFunction=twoClassSummary, # provides ROC summary stats in call to model
                 classProb=T, ## return class probabilities
                 seeds=seeds) ## set seeds
####################
## Boosting
####################
## set hyperparameters for grid search
gbmGrid <-  expand.grid(interaction.depth = c(2,4,6,8), ## allow depth to vary
                                   n.trees = (1:50)*100, ## allow number of trees to vary 
                                   shrinkage = 0.01, ## set shrinkage to 0.01
                                   n.minobsinnode = 5) ## set number of min obs
## perform grid search for boosting
set.seed(1804)
model.gbm <-train(as.factor(warstds)~., 
                  metric="ROC", method="gbm", ## stochastic gradient boosting model
                  trControl=tc,
                  verbose=T,
                  tuneGrid = gbmGrid,
                  data=data)
## plot boosting performance at different hyperparameter values
p <- ggplot(model.gbm) +
  theme_bw() +
  ylab('Cross Validated ROC-AUC') +
  xlab('Number of Trees')
      
ggsave(paste0(dir, "plots/gbmtuning.pdf"), p, height= 6, width=9)
####################
## Random forest
####################
set.seed(1804)
model.rf<-train(as.factor(warstds)~., 
                metric="ROC", method="rf", ## random forest model
                importance=T, 
                proximity=F,
                trControl=tc, 
                data=data)
model.rf
####################
## Regularized logit
####################
set.seed(1804)
model.log <-train(as.factor(warstds)~., 
                  metric="ROC", 
                  method="glmnet", ## regularized logit
                  trControl=tc, 
                  data=data)
model.log
####################
## CART
####################
set.seed(1804)
model.cart <-train(as.factor(warstds)~., 
                  metric="ROC",
                  method="rpart", ## CART
                  trControl=tc, 
                  data=data)
model.cart

## Combine performance of different methods for comparison
resamps <- resamples(list(Boosting = model.gbm,
                          Random.Forest = model.rf,
                          Regularized.Logit = model.log,
                          CART = model.cart))

## Plot comparison of model performance
trellis.par.set(caretTheme())
pdf(paste0(dir, 'plots/modelcomparison.pdf'))
dotplot(resamps, metric = "ROC")
dev.off()

## Extract variable importance stats for gbm
imp <- (varImp(model.gbm)$importance) ## get importance
imp$names <- rownames(imp) ## get names
imp <- arrange(imp, desc(Overall)) ## sort in descending order
imp <- imp[1:10,] ## only use top 10
imp$varnames <- c('Primary Commodity Exports/GDP', 'Agricultural Raw Materials Exports',
                  'Manufactures Exports', 'New State', 'Illiteracy Rate', 'Fuel Exports',
                  'Military Personnel', 'GDP Growth', 
                 'Trade/GDP', 'Population Density')
## plot 10 most important variables
p <- ggplot(imp, aes(y = Overall, x = reorder(varnames, Overall)))+
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab('Relative Influence') +
  xlab('Predictor') +
  theme_bw()
## save plot
ggsave(paste0(dir, "plots/varimportance.pdf"), p, height = 6, width = 8)

