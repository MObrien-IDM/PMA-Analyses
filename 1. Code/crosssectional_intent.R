## PMA cross-sectional

rm(list = ls())
require(readstata13)
require(plyr); require(dplyr)
require(ggplot2)
require(ggstance)
require(svyweight)
require(pollster)
require(gdata)
require(ipumsr)
dir <- "C:/Users/michelleob/OneDrive - Bill & Melinda Gates Foundation/Documents/PMA/data/"
setwd(dir)
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("pma_00002.xml")
data <- read_ipums_micro(ddi)

pma_cs <- as.data.frame(data)

### Replicate code for near-term intent from longitudinal analysis
pma_cs$near_term_intent <- NA
pma_cs$near_term_intent[pma_cs$FPPLANWHEN == 01 & pma_cs$FPPLANVAL <= 12] <- "Near-term intent" ## less than or equal 12 months
pma_cs$near_term_intent[pma_cs$FPPLANWHEN == 02 & pma_cs$fp_start_value <= 1] <- "Near-term intent" ## less than or equal 1 year
pma_cs$near_term_intent[pma_cs$FPPLANWHEN == 03] <- "Near-term intent"
pma_cs$near_term_intent[pma_cs$FPPLANWHEN == 04] <- "Some time in future"
pma_cs$near_term_intent[pma_cs$FPPLANWHEN == 97] <- "Some time in future"
pma_cs$near_term_intent[pma_cs$FPUSPLAN == 00] <- "No intent to use in future"
pma_cs$near_term_intent[pma_cs$FPUSPLAN == 99] <- "Not in universe"
pma_cs$near_term_intent[pma_cs$FPUSPLAN == 98] <- "No response/missing"

## Weighted descriptives 
library(questionr)
wtd.table(x = pma_cs$near_term_intent, weights = pma_cs$FQWEIGHT, useNA = "ifany")

## country specific 
bf <- pma_cs %>% filter(COUNTRY == 01)
wtd.table(x = bf$near_term_intent, weights = bf$FQWEIGHT)%>% prop.table()
drc <- pma_cs %>% filter(COUNTRY == 02)
wtd.table(x = drc$near_term_intent, weights = drc$FQWEIGHT)%>% prop.table()
eth <- pma_cs %>% filter(COUNTRY == 03)
wtd.table(x = eth$near_term_intent, weights = eth$FQWEIGHT)%>% prop.table()
ken <- pma_cs %>% filter(COUNTRY == 07)
wtd.table(x = ken$near_term_intent, weights = ken$FQWEIGHT)%>% prop.table()
ngr <- pma_cs %>% filter(COUNTRY == 08)
wtd.table(x = ngr$near_term_intent, weights = ngr$FQWEIGHT)%>% prop.table()
nga <- pma_cs %>% filter(COUNTRY == 09)
wtd.table(x = nga$near_term_intent, weights = nga$FQWEIGHT)%>% prop.table()
uga <- pma_cs %>% filter(COUNTRY == 10)
wtd.table(x = uga$near_term_intent, weights = uga$FQWEIGHT) %>% prop.table()
cdi <- pma_cs %>% filter(COUNTRY == 11)
wtd.table(x = cdi$near_term_intent, weights = cdi$FQWEIGHT) %>% prop.table()

