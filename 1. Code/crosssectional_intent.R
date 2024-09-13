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
dir <- "PMA/data/" ## replace with own dir for data
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
bf_tab <- as.data.frame(wtd.table(x = bf$near_term_intent, weights = bf$FQWEIGHT)%>% prop.table())
bf_tab$country <- "BF"

drc <- pma_cs %>% filter(COUNTRY == 02)
drc_tab <- as.data.frame(wtd.table(x = drc$near_term_intent, weights = drc$FQWEIGHT)%>% prop.table())
drc_tab$country <- "DRC"

eth <- pma_cs %>% filter(COUNTRY == 03)
eth_tab <- as.data.frame(wtd.table(x = eth$near_term_intent, weights = eth$FQWEIGHT)%>% prop.table())
eth_tab$country <- "ETH"

ken <- pma_cs %>% filter(COUNTRY == 07)
ken_tab <- as.data.frame(wtd.table(x = ken$near_term_intent, weights = ken$FQWEIGHT)%>% prop.table())
ken_tab$country <- "KEN"

ngr <- pma_cs %>% filter(COUNTRY == 08)
ngr_tab <- as.data.frame(wtd.table(x = ngr$near_term_intent, weights = ngr$FQWEIGHT)%>% prop.table())
ngr_tab$country <- "Niger"

nga <- pma_cs %>% filter(COUNTRY == 09)
nga_tab <- as.data.frame(wtd.table(x = nga$near_term_intent, weights = nga$FQWEIGHT)%>% prop.table())
nga_tab$country <- "Nigeria"


uga <- pma_cs %>% filter(COUNTRY == 10)
uga_tab <- as.data.frame(wtd.table(x = uga$near_term_intent, weights = uga$FQWEIGHT) %>% prop.table())
uga_tab$country <- "UGA"

cdi <- pma_cs %>% filter(COUNTRY == 11)
cdi_tab <- as.data.frame(wtd.table(x = cdi$near_term_intent, weights = cdi$FQWEIGHT) %>% prop.table())
cdi_tab$country <- "CDI"

## bind all the prop tables together 
int_prop <- rbind(bf_tab, drc_tab, eth_tab, ken_tab, ngr_tab, nga_tab, uga_tab, cdi_tab)
names(int_prop) <- c("Intent", "Prop", "Country")

nti <- int_prop %>% filter(Intent=="Near-term intent")
fivenum(nti$Prop)

write.csv(nti, file = "near-term intent PMA.csv")


#projection to # WITU
pop_WITU <- as.data.frame(wtd.table(x = pma_cs$COUNTRY, pma_cs$near_term_intent, weights = pma_cs$POPWT, useNA = "ifany"))
write.csv(pop_WITU, file = "pop size WITU PMA.csv")
