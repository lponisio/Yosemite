#!/usr/bin/env bash

## All paths are relative to the analysis folder within the github
## repo

## install the necessary packages. Analysis were run in R 3.5.3
bash analysis/packages.sh

## prep data
## only needed for original analysis, the saved .Rdata files should
## all in in github
# Rscript analysis/data/dataPrep.R

##***************************************************************
## network metrics
##***************************************************************
## pollinator partner turnover
Rscript analysis/network/1metrics.R 999
## plot R outputs
Rscript analysis/network/plotting/1metrics.R

##***************************************************************
## Community resistance: to species loss
##***************************************************************
Rscript analysis/network/2resistance.R
## plot model output
Rscript analysis/network/plotting/2resistance.R

##***************************************************************
## interaction flexibility
##***************************************************************
## pollinator partner turnover
Rscript analysis/variability/1nulls.R "pol" 999
Rscript analysis/variability/2partner.R "pol" "abund"
## pollinator role variability
Rscript analysis/variability/3pca.R

##***************************************************************
## population resistance
##***************************************************************
Rscript analysis/variability/4deltaAbund.R
## plot model output
Rscript analysis/variability/plotting/1deltaAbund.R

## for the hedgerow log ratio comparisons
Rscript analysis/variability/5deltaAbund_HR.R
