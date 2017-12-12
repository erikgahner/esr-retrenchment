## Title:     Welfare Retrenchments and Government Support:
##            Evidence from a Natural Experiment
## 
##            Erik Gahner Larsen
##            E.G.Larsen@kent.ac.uk
##        
## Data:      Publicly available at: http://www.europeansocialsurvey.org/download.html?file=ESS6DK&c=DK&y=2012
##            (free login required)
##            Data for the Danish National Election Study can be ordered at: http://dda.dk/catalogue/27067
##            (required in order to produce Figure A.1)

# Load packages
library("rio")
library("tidyr")

# Load data from ESS, round 6, DK sample
ess <- import("ESS6DK.dta")

# Recodings
## Outcome: Government satisfaction (code missing values)
ess$stfgov[ess$stfgov > 50] <- NA

## Create reform variable
ess$reform <- NA
ess$reform[ess$inwmme < 2] <- 0
ess$reform[ess$inwmme == 2 & ess$inwdde < 19] <- 0
ess$reform[ess$inwmme == 2 & ess$inwdde > 19] <- 1
ess$reform[ess$inwmme > 2] <- 1

## Create education status variable
ess$education <- ifelse(ess$edctn == 1, 1, 0)

## Create covariates
### Male
ess$male <- ifelse(ess$gndr == 1, 1, 0)
ess$age <- ess$agea

### Education level
ess$edulevel <- ess$eisced
ess$edulevel[ess$eisced == 88 | ess$eisced == 99] <- NA

### Subjective class
ess$society <- ess$plinsoc
ess$society[ess$plinsoc == 88 | ess$plinsoc == 99] <- NA

### Political interest
ess$polinterest <- ess$polintr
ess$polinterest[ess$polintr == 8 | ess$polintr == 9] <- NA
ess$polinterest <- (ess$polinterest - 5)*-1

### Political news
ess$tv <- ess$tvpol
ess$tv[ess$tvpol == 66 | ess$tvpol == 88 | ess$tvpol == 99] <- NA

### Religious
ess$religious <- ess$rlgdgr
ess$religious[ess$rlgdgr == 88] <- NA

### Ideology
ess$ideology <- ess$lrscale
ess$ideology[ess$lrscale == 88 | ess$lrscale == 99] <- NA

## Create placebo measures
ess$stflife[ess$stflife > 50] <- NA
ess$stfeco[ess$stfeco > 50] <- NA
ess$stfdem[ess$stfdem > 50] <- NA 

## Create date variables
ess$date <- format(as.Date(c(paste(ess$inwyys, ess$inwmms, ess$inwdds, sep="-")), by = "days"))
ess$date <- as.Date(ess$date)

### Distance in days
ess$difdate <- ess$date - as.Date("2013-02-19")
ess$difdate <- as.numeric(ess$difdate)

### Absolute distance in days
ess$absdate <- abs(ess$difdate)

## Create alternative proximity measures
### Family proximity
ess$family <- 0
ess$family[ess$yrbrn2 < 1996 & ess$yrbrn2 > 1984] <- 1
ess$family[ess$yrbrn3 < 1996 & ess$yrbrn3 > 1984] <- 1
ess$family[ess$yrbrn4 < 1996 & ess$yrbrn4 > 1984] <- 1
ess$family[ess$yrbrn5 < 1996 & ess$yrbrn5 > 1984] <- 1
ess$family[ess$yrbrn6 < 1996 & ess$yrbrn6 > 1984] <- 1
ess$family[ess$yrbrn7 < 1996 & ess$yrbrn7 > 1984] <- 1
ess$family[ess$yrbrn8 < 1996 & ess$yrbrn8 > 1984] <- 1
ess$family[ess$edctnp == 1] <- 1

### Prospective proximity
ess$prosp <- 0
ess$prosp[ess$yrbrn2 > 1996 & ess$yrbrn2 < 2013] <- 1
ess$prosp[ess$yrbrn3 > 1996 & ess$yrbrn3 < 2013] <- 1
ess$prosp[ess$yrbrn4 > 1996 & ess$yrbrn4 < 2013] <- 1
ess$prosp[ess$yrbrn5 > 1996 & ess$yrbrn5 < 2013] <- 1
ess$prosp[ess$yrbrn6 > 1996 & ess$yrbrn6 < 2013] <- 1
ess$prosp[ess$yrbrn7 > 1996 & ess$yrbrn7 < 2013] <- 1
ess$prosp[ess$yrbrn8 > 1996 & ess$yrbrn8 < 2013] <- 1

### Risk proximity
ess$unempl <- ifelse(ess$uempla + ess$uempli == 0, 0, 1)

# DNES
## Import data
dnes <- import("ElectionStudy-2011_da_F1.dta")

## Create variables
dnes$defense <- dnes$v149
dnes$healthcare <- dnes$v150
dnes$education <- dnes$v151
dnes$pension <- dnes$v152
dnes$environment <- dnes$v153
dnes$culture <- dnes$v154
dnes$childcare <- dnes$v155
dnes$unemployment <- dnes$v156
dnes$cashbenefits <- dnes$v157
dnes$foreignaid <- dnes$v158
dnes$immigration <- dnes$v159
dnes$homecare <- dnes$v160
dnes$highways <- dnes$v161
dnes$police <- dnes$v162

## Reshape from long to wide
dnes.wide <- gather(dnes, policy, spending, defense, healthcare, education, pension, 
                    environment, culture, childcare, unemployment, cashbenefits,
                    foreignaid, immigration, homecare, highways, police)
dnes.wide <- dnes.wide[,c("policy", "spending")]

## Export data-dnes.csv and data-ess.csv
export(dnes.wide, "data-dnes.csv")
export(ess, "data-ess.csv")