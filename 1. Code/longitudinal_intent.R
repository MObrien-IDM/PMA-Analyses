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
kano20 <- readstata13::read.dta13("Kano2020.dta", generate.factors = T)
kano21 <- readstata13::read.dta13("Kano2021.dta", generate.factors = T)
kano22 <- readstata13::read.dta13("Kano2022.dta", generate.factors = T)


