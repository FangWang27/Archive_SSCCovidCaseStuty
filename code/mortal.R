# R code for Covid Case Study
# Created on October 24, 2020
# Student name: Fang Wang
# Student number : 1003432760
# Email : warng.wang@mail.utoronto.ca
# Program and Department: MsC statistics, 1st year; Department of Statistical Science


require("dplyr")
require("ggplot2")
# import data
# mortality data for years by age 
dfMortal_raw <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/13100709.csv")

# data_cleaning
dfData_1 <- dfMortal_raw %>% 
select(sex = Sex, died = VALUE, age =Age.at.time.of.death)  %>% 
slice(-(1:3))