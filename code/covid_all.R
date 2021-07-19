# R code for Covid Case Study
# data cleaning for the combined covid data of Canada
# Created on October 24, 2020
# Student name: Fang Wang
# Student number : 1003432760
# Email : warng.wang@mail.utoronto.ca
# Program and Department: MsC statistics, 1st year; Department of Statistical Science

require("dplyr")
require("ggplot2")
require("tidyverse")
require("mgcv")

# import data
#  source : https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310078101 on October 24
dfCovid_raw <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/13100781.csv")
# save(dfCovid_raw, file ='dfCovid_raw.RData')
dfCovid_raw <- load("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/dfCovid_raw.RData")



# data cleaning

dfCovid <- dfCovid_raw %>%
  select(case_id = Case.identifier.number, case_info = Case.information, value = VALUE) %>%
    filter(case_info %in% c("Episode week", "Age group", "Hospital status", "Recovered", "Death", "Recovery week", "Occupation", "Region")) %>%
  spread(case_info, value)

table(as.numeric(dfCovid$`Episode week`))
 head(as.numeric(dfCovid$Episode.week))
names(dfCovid)


# weekly death count
df_death <- dfCovid %>%
    filter(Death == 1) %>%
    select(-c("Death", "case_id", "Recovery week", "Recovered")) %>%
    rename(age = 'Age group', time_week = 'Episode week', hospital = 'Hospital status') %>%
    group_by(time_week) %>%
    summarize(count = n())

head(df_death)


View(df_death)

# weekly death count by age group
df_death_age <- dfCovid %>%
    filter(Death == 1) %>%
    select(-c("Death", "case_id", "Recovery week", "Recovered")) %>%
    rename(age = 'Age group', time_week = 'Episode week', hospital = 'Hospital status') %>%
    group_by(time_week, age) %>%
    summarize(counts = n()) %>%
    spread("age", "counts") %>%
    replace(is.na(.), 0)
View(df_death_age)


# weekly death by Hospital Status
df_death_hospital <- dfCovid %>%
    filter(Death == 1) %>%
    select(-c("Death", "case_id", "Recovery week", "Recovered")) %>%
    rename(age = 'Age group', time_week = 'Episode week', hospital = 'Hospital status') %>%
    group_by(time_week, hospital) %>%
    summarize(counts = n()) %>%
    spread("hospital", "counts") %>%
    replace(is.na(.), 0)
View(df_death_hospital)

#weekly death by Occupation

df_death_occupation <- dfCovid %>%
    filter(Death == 1) %>%
    select(-c("Death", "case_id", "Recovery week", "Recovered")) %>%
    rename(age = 'Age group', time_week = 'Episode week', hospital = 'Hospital status') %>%
    group_by(time_week, Occupation) %>%
    summarize(counts = n()) %>%
    spread("Occupation", "counts") %>%
    replace(is.na(.), 0)
View(df_death_occupation)

df_case_occupation <- dfCovid %>%
    filter(Death == 2) %>%
    select(-c("Death", "case_id", "Recovery week", "Recovered")) %>%
    rename(age = 'Age group', time_week = 'Episode week', hospital = 'Hospital status') %>%
    group_by(time_week, Occupation) %>%
    summarize(counts = n()) %>%
    spread("Occupation", "counts") %>%
    replace(is.na(.), 0)
View(df_case_occupation)




df_case_longterm <- dfCovid %>%
    filter(Death == 2) %>%
    select(-c("Death", "case_id", "Recovery week", "Recovered")) %>%
    rename(age = 'Age group', time_week = 'Episode week', hospital = 'Hospital status') %>%
    transmute(time_week, is_longterm = (Occupation == 9)) %>%
    group_by(time_week, is_longterm) %>%
    summarize(counts = n()) %>%
 spread("is_longterm","counts")  %>% 
replace(is.na(.), 0)

View(df_case_longterm)
# replace value by the average of three preceding weeks
weeks <- seq_along(df_case_longterm)

precede_avg <- function(s, n) {
  # take a sequence seq and replace
  # each ith value  by the mean of n preceding value for all i>n
  #does not change if i<= n
  # s: numeric sequence
  # n : # of preceding value for which mean is calculated to replace
  N <- length(s)
  if(n==1){
    return(c(s[1], s[1:(N-1)]))
  }
  mat_seqs <- do.call(rbind, lapply(1:(N - 1), function(i) { c(s[i:N], rep(0, i)) }))
  avg <- colMeans( as.matrix(mat_seqs[(1:n),]))
  return(c(s[1:n], avg[1:(N - n)]))
}

#weekly average case

df_case_longterm_avg <- df_case_longterm
df_case_longterm_avg[,2:3] <- apply(case_longterm_avg[,2:3] ,2,function(s){ precede_avg(s,3)})








df_death_region <- dfCovid %>%
    filter(Death == 1) %>%
    select(-c("Death", "case_id", "Recovery week", "Recovered")) %>%
    rename(age = 'Age group', time_week = 'Episode week', hospital = 'Hospital status') %>%
    group_by(time_week, Region) %>%
    summarize(counts = n()) %>%
    spread("Region", "counts") %>%
    replace(is.na(.), 0)
View(df_death_region)

colSums(df_death_region)




