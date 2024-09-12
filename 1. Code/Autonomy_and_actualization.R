### PMA contraceptive autonomy --> actualization

## RQ - Does contraceptive autonomy facilitate women's ability to overcome
## barriers to actualizing their intent to use contraception

#setup
rm(list = ls())
require(readstata13)
require(plyr); require(dplyr)
require(ggplot2)
require(ggstance)
require(svyweight)
require(pollster)
require(gdata)
dir <- "C:/Users/michelleob/OneDrive - Bill & Melinda Gates Foundation/Documents/PMA/data"
setwd(dir)

## 1 READ all data at once 
temp = list.files(pattern="*.dta") #list all files in the working directory with extension
for (i in 1:length(temp)) assign(temp[i], read.dta13(temp[i]))  #bring in all files in folder with selected variables

dfs = sapply(.GlobalEnv, is.data.frame) #list all dataframes in the environment
temp <- do.call(rbind.fill, mget(names(dfs)[dfs])) #bind all the dataframes in environment, missing columns filled in

## 2 SUBSET data to women with complete records
pma <- temp %>% filter((HHQ_result == "1. Completed" | HHQ_result_cc == "1. Completed") & 
                         (FRS_result == "1. Completed" | FRS_result_cc == "1. Completed") & last_night == "1. Yes") 

## recode intent to use x current use
pma$intent_x_use <- "No intention to use"
pma$intent_x_use[pma$current_user == "1. Yes"] <- "Current user"
pma$intent_x_use[pma$future_user_not_current == "1. Yes"] <- "Future user"
pma$intent_x_use[pma$future_user_pregnant == "1. Yes"] <- "Future user"
table(pma$country, pma$intent_x_use, pma$phase)

int_prop <- as.data.frame(table(pma$country, pma$intent_x_use, pma$phase) %>% prop.table(margin=c(1,3)))
ggplot(int_prop, aes(x = Var3, y=Freq, group=Var2, colour=Var2)) + 
  geom_point() + geom_line() +
  facet_wrap(~ Var1) + 
  xlab("Survey phase") + ylab("Proportion") + 
  theme(legend.title=element_blank())

## construction of actualization variable 
## memberID 
sum(duplicated(pma$memberID))

## construction of contraceptive autonomy variable - use SDG definition

## bivariate association between CA and actualization

## progressive modeling of the relationship between CA and actualization 



