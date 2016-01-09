#######################################################################
##  Filename: cart_plot.R
##  Purpose: Plots to demonstate how CART works
##  Uses data from: data/SambnisImp.csv
##                  From Muchlinski et al. (2015)   
##  Assumes packages: stringr, party, ggplot2
##  Output to: plots/cartpartition.pdf; plots/carttree.pdf
##  Last Edited: 07 January 2016
##  Christopher Boylan, Penn State University
#######################################################################
## Load Packages and Prepare Directory
#######################################################################
rm(list=ls()) ## remove objects from working environment
set.seed(1809) ## set seed
library(party); library(ggplot2); library(stringr) ## load packages
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "R", "../", "./") ## set directory 
#######################################################################
## Prepare data
#######################################################################
df<- read.csv(paste0(dir, "data/SambnisImp.csv")) ## import Muchlinski data
df <- df[,c('warstds', 'gdpgrowth', 'ln_gdpen')] ## subset data
## convert warstds to factor
df$warstds<-factor(
  df$warstds,
  levels = c(0, 1),
  labels = c("Peace", "War"))
## select sample of data, deliberately oversampling civil war onsets
rows <- c(sample(as.numeric(rownames(df[df$warstds == "War",])), size = 10),
              sample(as.numeric(rownames(df[df$warstds == "Peace",])), size = 50))
## only keep sample
df <- df[rows,]
## rename variables
colnames(df) <- c('warstds', 'growth', 'gdp.pc')
#######################################################################
## Estimate CART model
#######################################################################
## Estimate a modified model for demonstration purposes
cart.mod <- ctree(warstds ~ growth + gdp.pc, 
                  controls = ctree_control(minsplit = 10,
                                          mincriterion = 0.75),
                  data = df)
#######################################################################
## Plot splits in feature space and tree
#######################################################################
## Get split values
split1 <- cart.mod@tree$psplit$splitpoint
split2 <- cart.mod@tree$right$psplit$splitpoint
## Create classification variable for plot
df$class <- factor(ifelse(((df$gdp.pc >= split1 & df$warstds == "Peace" & df$growth >= split2) |
                  (df$gdp.pc >= split1 & df$warstds == "Peace" & df$growth < split2) |
                  (df$gdp.pc <= split1 & df$warstds == "Peace")),
                    1, 0))
## Two dimensional feature space plot
p <- ggplot(df, aes(gdp.pc, growth, shape = warstds, col = class))+
  theme_bw() + ## bw theme
  geom_point(size = 4) + ## size of points
  scale_x_continuous(limits = c(-2, 3), expand = c(0, 0)) + ## x-axis 
  scale_shape_manual(values = c("P", "W"), labels = c("No Onset", "Onset"),
                     name = "Civil War Onset?") + ## P=peace, W=war
  scale_color_manual(values = c("orange", "royalblue1"), name = 'Correctly Classified?',
                     labels = c("Misclassified", "Correctly Classified")) + ## orange=correct
  xlab('GDP per capita (logged)') + ## x-axis label
  ylab('Growth')+  ## y-axis label
  geom_vline(aes(xintercept = split1), size = .5,  color = "black") +  ## split 1
  geom_segment(aes(x = split1, xend = 3, y = split2,yend = split2),
               size = .5,  color = "black") ## split 2
ggsave(paste0(dir, "plots/cartpartition.pdf"), p, height = 6, width = 8) ## save plot
## Tree plot
pdf(paste0(dir, "plots/carttree.pdf"), height = 6, width = 8)
tree.plot <- plot(cart.mod, inner_panel = node_inner(cart.mod, pval = FALSE),
     terminal_panel = node_barplot(cart.mod, beside = FALSE, id = FALSE))
dev.off()