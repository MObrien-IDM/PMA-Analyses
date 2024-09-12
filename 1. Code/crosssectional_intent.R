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

### Replicate code for near-term intent from longitudinal analysis
pma$near_term_intent <- NA
pma$near_term_intent[pma$fp_start == "1. X months" & pma$fp_start_value <= 12] <- "Near-term intent"
pma$near_term_intent[pma$fp_start == "2. X years" & pma$fp_start_value <= 1] <- "Near-term intent"
pma$near_term_intent[pma$fp_start == "-88. Do not know"] <- "Desire to use in future"
pma$near_term_intent[pma$fp_start == "3. Soon/now"] <- "Near-term intent"
pma$near_term_intent[pma$fp_start == "4. After the birth of this child"] <- "Desire to use in future"
pma$near_term_intent[pma$future_user_not_current == "0. No"] <- "No intent to use in future"
pma$near_term_intent[pma$current_user == "1. Yes"] <- "Current user"
pma$near_term_intent[pma$pregnant == "1. Yes"] <- "Pregnant"

