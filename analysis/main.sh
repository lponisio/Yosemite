#!/usr/bin/env bash

## All paths are relative to the analysis folder within the github
## repo

## install the necessary packages. Analysis were run in R 3.5.3
bash analysis/packages.sh

## prep data
## only needed for original analysis, the saved .Rdata files should
## all in in github
Rscript analysis/data/dataPrep.R

##***************************************************************
## network metrics
##***************************************************************
## pollinator partner turnover
Rscript analysis/networkLevel/metrics.R 999
## plot R outputs
Rscript analysis/networkLevel/plotting/networkLevel_mods.R

##***************************************************************
## Community resistance: to species loss
##***************************************************************
Rscript analysis/networkLevel/robustness.R
## plot model output
Rscript analysis/networkLevel/plotting/robustness.R

##***************************************************************
## interaction flexibility
##***************************************************************
## pollinator partner turnover
Rscript analysis/variability/nulls.R "pol" 999
Rscript analysis/variability/partner.R "pol" "abund"
## pollinator role variability
Rscript analysis/variability/pca.R

##***************************************************************
## population resistance
##***************************************************************
Rscript analysis/variability/deltaAbund.R
## plot model output
Rscript analysis/variability/plotting/deltaAbund_mods.R
