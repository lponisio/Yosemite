#!/usr/bin/env bash

## All paths are relative to the analysis folder within the github
## repo

## install the necessary packages. Analysis were run in R 3.5.1
bash analysis/packages.sh

## prep data
## only needed for original analysis, the saved .Rdata files should
## all in in github

Rscript analysis/data/dataPrep.R

##***************************************************************
## interaction flexibility
##***************************************************************
## pollinator partner turnover
Rscript analysis/variability/nulls.R "pols" 999
Rscript analysis/variability/partner.R "pols" "abund"
## pollinator role variability
Rscript analysis/variability/pca.R

##***************************************************************
## delta abund and persistence models
##***************************************************************
Rscript analysis/variability/deltaAbund.R
## plot model output
Rscript analysis/variability/plotting/deltaAbund_mods.R

##***************************************************************
## Robustness/resistence to species loss
##***************************************************************
Rscript analysis/networkLevel/robustness.R
## plot model output
Rscript analysis/networkLevel/plotting/robustness.R
