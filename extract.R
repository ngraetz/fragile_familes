library(haven)
library(data.table)
library(ggplot2)

pull_wave_dta <- function(file, in_dir, cb) {
  d <- read_dta(paste0(in_dir, '/', file))
  d <- as.data.table(as_factor(d))
  keep_vars <- names(d)[names(d) %in% cb[, var]]
  d <- d[, c('idnum',keep_vars), with=FALSE]
  for(v in keep_vars) setnames(d, v, cb[var==v, var_rename])
  return(d)
}
cb <- fread("C:/Users/Nick/Documents/repos/fragile_familes/codebook.csv")
files <- c('ffdadbspv3_stata/ffdadbspv3.dta','ffmombspv3_stata/ffmombspv3.dta',
           'ffdad1ypv2_stata/ffdad1ypv2.dta','ffmom1ypv2_stata/ffmom1ypv2.dta',
           'ffmom3ypv2_stata/ffmom3ypv2.dta','ffdad3ypv2_stata/ffdad3ypv2.dta',
           'ffmom5ypv1_stata/ffmom5ypv1.dta','ffdad5ypv1_stata/ffdad5ypv1.dta',
           'ff_y9_pub1_stata/ff_y9_pub1.dta',
           'FF_Y15_pub_stata/FF_Y15_pub.dta')
files <- lapply(files, pull_wave_dta, in_dir <- 'C:/Users/Nick/Downloads', cb=cb)

## For now, I think I want this long by child, wave.
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
all_waves <- rbind(wave1, wave2, wave3, wave4, files[[9]], files[[10]], fill=TRUE)

## Pull all waves (0, 1, 3, 5, 9, 15) of parental race, age, poverty, education, depression, father incarceration, relationship

## Reconcile mother OR father variables for Wave 6 based on relationship reported between primary care-giver (PCG) and child.
pcg_vars <- c('age','hh_income','poverty','lib_depression','con_depression','education','father_in_jail')
for(pcg_var in pcg_vars) {
  all_waves[wave==6 & pcg_relation_to_ind=='1 Biological mother', (paste0('mother_',pcg_var)) := get((paste0('pcg_',pcg_var)))]
  all_waves[wave==6 & pcg_relation_to_ind=='2 Biological father', (paste0('father_',pcg_var)) := get((paste0('pcg_',pcg_var)))]
}

## For now, keep only children with info in Year 15 (Wave 6).
#all_waves <- all_waves[pcg_relation_to_ind != '-9 Not in wave', ]

## Create unweighted descriptives across wave/race.
all_waves[wave==6 & ind_race=='2 Black/Af. American only, non-hispanic', race := 'black']
all_waves[wave==6 & ind_race=='1 White only, non-hispanic', race := 'white']
ind_race <- all_waves[wave==6, c('idnum','race')]
all_waves[, race := NULL]
all_waves <- merge(all_waves, ind_race, by='idnum')

## Create dummies for variables we want in descriptive table.
all_waves[idnum=='0003', c('idnum','wave','race','mother_race','father_race','ind_race',
                           'mother_education','mother_poverty','mother_con_depression',
                           'father_education','mother_father_in_jail','father_father_in_jail','mother_relation_father')]

## Lowercase everything
dummy_vars <- c('mother_education','mother_poverty','mother_con_depression',
                'father_education','mother_father_in_jail','father_father_in_jail','mother_relation_father')
for(v in dummy_vars) all_waves[, (v) := tolower(get(v))]

## Manual recoding of variables for now: add to codebook later.
all_waves[mother_education=='4 coll or grad', mother_education := '4 college or grad']
all_waves[mother_education=='1 less hs', mother_education := '1 less than hs']
all_waves[father_education=='4 coll or grad', father_education := '4 college or grad']
all_waves[father_education=='1 less hs', father_education := '1 less than hs']
all_waves[mother_poverty=='300%+', mother_poverty := '5 300%+']
all_waves[mother_poverty=='1 <49%', mother_poverty := '1 0-49%']
all_waves[mother_con_depression=='0', mother_con_depression := '0 no']
all_waves[mother_con_depression=='1', mother_con_depression := '1 yes']
all_waves[mother_father_in_jail=='2 no', mother_father_in_jail := '0 no']
all_waves[mother_relation_father %in% c("5 sep/wid/div","5 sep/ div/ widow",
                                        "5 sep/div/widow","5 sep/div/wid", "5 hardly talk"),
          mother_relation_father := 'separated']

## poverty = '2 50-99%'
## extreme_poverty = '1 0-49%'
make_dummies <- function(vars, d) {
  for(x in vars) {
    d[, (x) := tolower(get(x))]
    values <- unique(d[, get(x)])
    for(v in values) {
      d[get(x)==v, (paste0(x,'_',v)) := 1]
      d[get(x)!=v, (paste0(x,'_',v)) := 0]
    }
    d[is.na(get(x)), (paste0(x,'_',v)) := NA]
  }
  return(d)
}
all_waves <- make_dummies(vars=dummy_vars, d=all_waves)

names(all_waves)[grep('mother_relation_father',names(all_waves))]

## Dummies to collapse means over 0/1:
collapse_vars <- c("mother_education_1 less than hs", "mother_education_2 hs or equiv", "mother_education_3 some coll, tech", "mother_education_4 college or grad",
"father_education_1 less than hs", "father_education_2 hs or equiv", "father_education_3 some coll, tech", "father_education_4 college or grad",
"mother_poverty_1 0-49%", "mother_poverty_2 50-99%", "mother_poverty_3 100-199%", "mother_poverty_4 200-299%", "mother_poverty_5 300%+",
"mother_con_depression_1 yes", "mother_father_in_jail_1 yes", "mother_relation_father_1 married", "mother_relation_father_separated")
all_waves[, N := 1]
sample_size <- all_waves[!is.na(race), list(N=sum(N)), by=c('wave','race')]
collapse_waves <- all_waves[, lapply(.SD, mean, na.rm=TRUE), by=c('wave','race'), .SDcols=collapse_vars]
collapse_waves <- collapse_waves[!is.na(race), ]
collapse_waves <- merge(collapse_waves, sample_size, by=c('wave','race'))
collapse_waves <- melt(collapse_waves, id.vars = c('wave','race'), measure.vars = c(collapse_vars,'N'))
collapse_waves <- dcast(collapse_waves, variable ~ wave + race, value.var = 'value')
collapse_waves[variable=='mother_con_depression_1 yes', variable := 'Mother depressed']
collapse_waves[variable=='mother_father_in_jail_1 yes', variable := 'Father incarcerated']
collapse_waves[, variable := gsub(paste(dummy_vars,collapse="|"), '', variable)]
collapse_waves[, variable := gsub(paste(c('_1 ','_2 ','_3 ','_4 ','_5 ','_'),collapse="|"), '', variable)]
saveRDS(collapse_waves, 'C:/Users/Nick/Documents/repos/fragile_familes/table_1.RDS')

## -1 through -9 are missing codes that relate to various reasons that information is missing.

wave5[!grep(paste(as.character(-9:-1),collapse="|"), cp6yagey), cp6yagey := 'NA']
unique(wave5$cp6yagey)


val_labels(wave5$p6b33_4)
wave5$test <- labelled(x = wave5$p6b33_4, labels = val_labels(wave5$p6b33_4))
