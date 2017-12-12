# Welfare Retrenchments and Government Support: Evidence from a Natural Experiment
---

### Description and data sources

Replication material for 'Welfare Retrenchments and Government Support' published in the European Sociological Review. This repository contains all files required to produce the figures, tables and numerical information provided in the manuscript and supplementary material.

Data for the European Social Survey is publicly available at: http://www.europeansocialsurvey.org/download.html?file=ESS6DK&c=DK&y=2012 (free login required). Data for the Danish National Election Study can be ordered at: http://dda.dk/catalogue/27067 (required in order to produce Figure A.1).

### Author/contact

 - Erik Gahner Larsen, School of Politics and International Relations, University of Kent, E.G.Larsen@kent.ac.uk.

### Repository content

- `01-data.R` = R script used to create the datasets used for the analysis, `data-dnes.csv` and `data-ess.csv`
    - Requires `ElectionStudy-2011_da_F1.dta` and `ESS6DK.dta`
- `02-analysis` = R script used for all analyses in the article
- `data-dnes.csv` = Data from the Danish National Election Study (required to create Figure A.1)
- `dnes-ess.csv`= Data from the European Social Survey (required to estimate everything beyond Figure A.1)
- `sessionInfo.txt` = Output from sessionInfo() in R

### Session info

The analyses were made with [RStudio](http://www.rstudio.com/) (Version 1.0.136) with the following R session:

```
## R version 3.3.1 (2016-06-21)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.13.1 (unknown)

## locale:
## [1] da_DK.UTF-8/da_DK.UTF-8/da_DK.UTF-8/C/da_DK.UTF-8/da_DK.UTF-8

## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     

## other attached packages:
##  [1] lm.beta_1.5-1   rms_5.1-1       Hmisc_4.0-3     Formula_1.2-2   survival_2.41-3
##  [6] lattice_0.20-35 RItools_0.1-15  SparseM_1.77    stargazer_5.2   rdrobust_0.98  
## [11] MatchIt_3.0.1   gridExtra_2.3   ggplot2_2.2.1   tidyr_0.7.2     rio_0.5.5      

## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.13        mvtnorm_1.0-6       zoo_1.8-0           assertthat_0.2.0   
##  [5] digest_0.6.12       R6_2.2.2            cellranger_1.1.0    plyr_1.8.4         
##  [9] backports_1.1.1     acepack_1.4.1       MatrixModels_0.4-1  rlang_0.1.2        
## [13] lazyeval_0.2.0      curl_3.0            svd_0.4.1           multcomp_1.4-7     
## [17] readxl_1.0.0        data.table_1.10.4-2 rpart_4.1-11        Matrix_1.2-11      
## [21] checkmate_1.8.5     labeling_0.3        splines_3.3.1       stringr_1.2.0      
## [25] foreign_0.8-69      htmlwidgets_0.9     munsell_0.4.3       pkgconfig_2.0.1    
## [29] base64enc_0.1-3     htmltools_0.3.6     tidyselect_0.2.2    nnet_7.3-12        
## [33] tibble_1.3.4        htmlTable_1.9       codetools_0.2-15    dplyr_0.7.4        
## [37] MASS_7.3-47         grid_3.3.1          nlme_3.1-131        polspline_1.1.12   
## [41] xtable_1.8-2        gtable_0.2.0        magrittr_1.5        scales_0.5.0       
## [45] stringi_1.1.5       optmatch_0.9-7      bindrcpp_0.2        latticeExtra_0.6-28
## [49] sandwich_2.4-0      openxlsx_4.0.17     TH.data_1.0-8       RColorBrewer_1.1-2
## [53] tools_3.3.1         forcats_0.2.0       glue_1.1.1          purrr_0.2.4        
## [57] abind_1.4-5         colorspace_1.3-2    cluster_2.0.6       bindr_0.1          
## [61] knitr_1.17          haven_1.1.0         quantreg_5.33      
```
