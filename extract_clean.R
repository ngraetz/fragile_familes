library(haven)
library(data.table)
library(ggplot2)
library(naniar)
library(mice)
library(survey)
setwd('C:/Users/ngraetz/Documents/repos/fragile_familes/')
process_raw_data <- TRUE

## Function to apply codebook and keep only those clean extracted variables.
pull_wave_dta <- function(file, in_dir, cb) {
  ## Load and kill Stata, the heart of all evil.
  d <- read_dta(paste0(in_dir, '/', file))
  d <- as.data.table(as_factor(d))
  ## Keep and rename all variables in codebook.
  keep_vars <- names(d)[names(d) %in% cb[, var]]
  d <- d[, c('idnum',keep_vars), with=FALSE]
  for(v in keep_vars) setnames(d, v, cb[var==v, var_rename])
  ## Recode any variables that need recoding.
  this_cb <- cb[var %in% keep_vars, ]
  for(v in this_cb[, var_rename]) {
    for(val in 1:5) {
      this_val <- this_cb[var_rename==v, get(paste0('value_',val))]
      ## Split up these values if provided as strings (":") or numeric range ("_").
      if(grepl(':',this_val)) {
        this_val <- unlist(strsplit(this_val, ':'))
      } else if(grepl('_',this_val)) {
        this_val <- as.numeric(unlist(strsplit(this_val, '_')))
        this_val <- as.character(this_val[1]:this_val[2])
      }
      ## Grab recode value.
      this_recode <- this_cb[var_rename==v, get(paste0('recode_',val))]
      ## Do the recode.
      if(this_val[1] != '') {
        message(paste0('Recoding "', this_val, '" to ', this_recode))
        d[get(v) %in% this_val, (v) := this_recode] 
      }
    }
  }
  ## Return cleaned dataset.
  return(d)
}

## Load codebook and apply to all raw datasets.
cb <- fread("./raw_public_data/clean_cb.csv")
if(process_raw_data) {
files <- c('ffdadbspv3_stata/ffdadbspv3.dta','ffmombspv3_stata/ffmombspv3.dta', ## Baseline
           'ffdad1ypv2_stata/ffdad1ypv2.dta','ffmom1ypv2_stata/ffmom1ypv2.dta', ## Year 1
           'ffmom3ypv2_stata/ffmom3ypv2.dta','ffdad3ypv2_stata/ffdad3ypv2.dta', ## Year 3
           'ffmom5ypv1_stata/ffmom5ypv1.dta','ffdad5ypv1_stata/ffdad5ypv1.dta', ## Year 5
           'ff_y9_pub1_stata/ff_y9_pub1.dta', ## Year 9
           'FF_Y15_pub_stata/FF_Y15_pub.dta', ## Year 15
           'inhome3yr_stata/InHome3yr.dta', ## For Year 3, DV saved separately.
           'Inhome5yr2011_stata/inhome5yr2011.dta') ## For Year 5, DV saved separately.
files <- lapply(files, pull_wave_dta, in_dir <- './raw_public_data', cb=cb)
## Save in repo so I don't have to compile every time
saveRDS(files, 'C:/Users/ngraetz/Documents/repos/fragile_familes/all_waves.RDS')
}

## Load all cleaned datasets.
files <- readRDS('C:/Users/ngraetz/Documents/repos/fragile_familes/all_waves.RDS')

## I want one dataset long by id, age, year.
wave1 <- merge(files[[1]], files[[2]], by='idnum')
wave1[, wave := 1]
wave2 <- merge(files[[3]], files[[4]], by='idnum')
wave2[, wave := 2]
wave3 <- merge(files[[5]], files[[6]], by='idnum')
wave3[, wave := 3]
wave4 <- merge(files[[7]], files[[8]], by='idnum')
wave4[, wave := 4]
wave5 <- files[[9]]
wave5[, wave := 5]
wave6 <- files[[10]]
wave6[, wave := 6]
all_waves <- rbind(wave1, wave2, wave3, wave4, wave5, wave6, fill=TRUE)
ind_race_option <- FALSE

## Manually infer some variables across waves
# If father not in jail at baseline but reported "ever in jail" at Year 1, change "ever in jail" at baseline to yes. "Ever in jail" not included in baseline.
# Clean job type based on hours worked (make job type "none" if job==0, make sure job==1 for "part" and "full")
# For now, treat age for child in years at baseline (0), and just add 1, 3, 5, 9, 15 in observed data at each wave.

## Use codebook to copy forward across waves all baseline variables that are time-invariant.

## Label people who are censored using relationship variable ("not in wave" = '-9 Not in wave').
all_waves[m_relation=='-9 Not in wave', censor := 1]
all_waves[is.na(censor), censor := 0]

## After coding censoring, replace 

## Multiple imputation with mice and then collapse with mitools.
## There are arguments to MI your DVs, especially if you do enough imputations and your DV missingness might predict your IV missingness.
## Especially revelvant in g-formula context where IVs are explicitly a function of DVs as well over time.
## https://modeling.uconn.edu/wp-content/uploads/sites/1188/2016/05/Don%E2%80%99t-be-Fancy.-Impute-Your-Dependent-Variables.pdf

## Save for merging with restricted data by id and running g-formula.
