library(haven)
library(data.table)
library(ggplot2)

wave5 <- read_dta("C:/Users/ngraetz/Downloads/FF_Y15_pub_stata/FF_Y15_pub.dta")
wave5 <- as.data.table(as_factor(wave5))
wave4 <- read_dta("C:/Users/ngraetz/Downloads/ff_y9_pub1_stata/ff_y9_pub1.dta")
wave4 <- as.data.table(as_factor(wave4))

## -1 through -9 are missing codes that relate to various reasons that information is missing. 

wave5[!grep(paste(as.character(-9:-1),collapse="|"), cp6yagey), cp6yagey := 'NA']
unique(wave5$cp6yagey)


val_labels(wave5$p6b33_4)
wave5$test <- labelled(x = wave5$p6b33_4, labels = val_labels(wave5$p6b33_4))

