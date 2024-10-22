##Examining intent to use descriptives in the PMA

#setup
rm(list = ls())
require(readstata13)
require(plyr); require(dplyr)
require(ggplot2)
require(ggstance)
require(svyweight)
require(pollster)
require(gdata)
dir <- "C:/Users/michelleob/OneDrive - Bill & Melinda Gates Foundation/Documents/PMA/data/"
setwd(dir)

# ## 1 READ all data at once 
# temp = list.files(pattern="*.dta") #list all files in the working directory with extension
# for (i in 1:length(temp)) assign(temp[i], read.dta13(temp[i]))  #bring in all files in folder with selected variables
# 
# dfs = sapply(.GlobalEnv, is.data.frame) #list all dataframes in the environment
# temp <- do.call(rbind.fill, mget(names(dfs)[dfs])) #bind all the dataframes in environment, missing columns filled in

## 2 SUBSET data to women with complete records
# pma <- temp %>% filter((HHQ_result == "1. Completed" | HHQ_result_cc == "1. Completed") & 
#          (FRS_result == "1. Completed" | FRS_result_cc == "1. Completed") & last_night == "1. Yes") 

## if 1 and 2 already completed - saves appx 10 min runtime
#write.csv(pma, file="pma.csv")
pma <- read.csv("pma.csv")


## Descriptive statistics 
table(pma$country, pma$phase)
table(pma$future_user_not_current, pma$future_user_pregnant, useNA="ifany")
## recode intent to use x current use
pma$intent_x_use <- "No intention to use"
pma$intent_x_use[pma$current_user == "1. Yes"] <- "Current user"
pma$intent_x_use[pma$future_user_not_current == "1. Yes"] <- "Future user"
pma$intent_x_use[pma$future_user_pregnant == "1. Yes"] <- "Future user"
table(pma$country, pma$intent_x_use, pma$phase)

pma$near_term_intent <- NA
pma$near_term_intent[pma$fp_start == "1. X months" & pma$fp_start_value <= 12] <- "Near-term intent"
pma$near_term_intent[pma$fp_start == "2. X years" & pma$fp_start_value <= 1] <- "Near-term intent"
pma$near_term_intent[pma$fp_start == "-88. Do not know"] <- "Desire to use in future"
pma$near_term_intent[pma$fp_start == "3. Soon/now"] <- "Near-term intent"
pma$near_term_intent[pma$fp_start == "4. After the birth of this child"] <- "Desire to use in future"
pma$near_term_intent[pma$future_user_not_current == "0. No"] <- "No intent to use in future"
pma$near_term_intent[pma$current_user == "1. Yes"] <- "Current user"
pma$near_term_intent[pma$pregnant == "1. Yes"] <- "Pregnant"

### IF DMPA-SC QUESTIONS, RECODE CURRENT USER
pma$fp_method <- pma$current_method

library(questionr)
wtd.table(x = pma$near_term_intent, weights = pma$FQweight)

##restrict to baseline for easier baseline tables and figures
pma1 <- pma %>% filter(phase==1)
##country-specific work
cdi <- pma %>% filter(country=="Cotedivoire")
cdi1 <- pma1 %>% filter(country=="Cotedivoire")
wtd.table(x = pma1$near_term_intent, weights = pma1$FQweight)

## QUick visz

int_prop <- as.data.frame(wtd.table(x = pma1$country, y = pma1$near_term_intent, weights = pma1$FQweight) %>% prop.table(margin=1))
ggplot(int_prop, aes(x = Var2, y=Freq, group=Var2, colour=Var2)) + 
  geom_point() +
  facet_wrap(~ Var1) + 
  xlab("Survey phase") + ylab("Proportion") + 
  theme(legend.title=element_blank())

int_time <- as.data.frame(wtd.table(x = pma$phase, y = pma$near_term_intent, weights = pma$FQweight) %>% prop.table(margin=1))
ggplot(int_time, aes(x = Var2, y=Freq, 
                     group=Var2, colour=Var2, fill=Var2)) + 
  geom_bar(stat="identity")+
  facet_wrap(~ Var1) + 
  xlab("") + ylab("Proportion") + 
  theme(legend.title=element_blank()) + 
  coord_flip() +
geom_text(aes(label=round(Freq,digits=2), hjust=-0.5)) +
  ylim(0,0.7)

## country specific over time
cs_country = "Cotedivoire"
cs <- NULL
cs <- pma %>% filter(pma$country==cs_country)
int_time_cs <- as.data.frame(wtd.table(x = cs$phase, y = cs$near_term_intent, weights = cs$FQweight) %>% prop.table(margin=1))
ggplot(int_time_cs, aes(x = Var2, y=Freq, 
                     group=Var2, colour=Var2, fill=Var2)) + 
  geom_bar(stat="identity")+
  facet_wrap(~ Var1) + 
  xlab("") + ylab("Proportion") + 
  theme(legend.title=element_blank()) + 
  coord_flip() +
  geom_text(aes(label=round(Freq,digits=2), hjust=-0.5)) +
  ylim(0,0.7) + 
  ggtitle(cs_country)

# multinom(intent_x_use ~ age + marital _status + phase, data=pma, weights=FQweight)
# 
# #code for survey weighted tables
# crosstab(df = pma, x = country, y=intent_x_use, weight=FQweight, n=TRUE)











