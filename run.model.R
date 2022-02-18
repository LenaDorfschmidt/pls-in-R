library(plsdepot) # I am using plsdepot 0.1.17
library(ggplot2) # I am using ggplot2 3.3.2
library(here) # I am using here 1.0.0
# =======================================================
#                         Prepare Data               
# =======================================================
X = read.csv(here('data/X.csv'))
Y = read.csv(here('data/Y.csv'))

nboot = 10 # Define number of permutation iterations
nboot = 10 # Define number of bootstrap iterations

source('plot_variance_explained.R')
source('plot_perm_results.R')
source('pls.perm.boot.R')

pls.res <- pls.perm.boot(X,Y,nperm=10,nboot=10, col_names = colnames(X))

pls.res <- pls.perm.boot(X,Y,nperm=10,nboot=10, col_names = colnames(X))




