library(TSA)
require("dplyr")
require("ggplot2")

data_raw <- read.csv("C:\\Users\\wangf\\OneDrive - Seneca College of Applied Arts & Technology\\Current\\Covid\\Data\\covid19dataexport.csv")


data_dead <- data_raw %>% 
filter(Case.status == "Died") %>%
select("Date.reported", "Age.group")

data_active <- data_raw %>% 
filter(Case.status == "Active") %>%
select("Date.reported", "Age.group")

table(data_dead$Age.group)
table(data_active$Age.group)



age_group <- apply(data_raw, 1, function(ag) {
  age <- ag[5]
  child <- c("Under 1 year", "1-4 years", "5-9 years", "10-19 years")
  young <- c("20-29 years", "30-39 years", "40-49 years")
  old <- c("50-59 years", "60-69 years", "70-79 years", "80+ years")
  if (age %in% child) {
    return("child")
  } else if (age %in% young) {
    return("young")
  } else if (age %in% old) {
    return("old")
  } else {
    return("unknown")
  }
})

table(age_group)

names(table(data_raw$Age.group))
# check group and do the data cleaning and transform to counting data
data_regroup <- data_raw
data_regroup$Age.group <- age_group

df_active <- data_regroup %>%
filter(Case.status == "Active") %>%
select("Date.reported", "Age.group") %>%
transmute(report_date = as.Date(Date.reported), age_group = as.factor(Age.group)) %>%
group_by(report_date, age_group) %>%
mutate(count = n())

gg_active <- ggplot(df_active, aes(x = report_date, y = count)) +
geom_line(aes(color = age_group))


# died
df_died <- data_regroup %>%
filter(Case.status == "Died") %>%
select("Date.reported", "Age.group") %>%
transmute(report_date = as.Date(Date.reported), age_group = as.factor(Age.group)) %>%
group_by(report_date, age_group) %>%
mutate(count = n())

gg_died <- ggplot(df_died, aes(x = report_date, y = count)) +
geom_line(aes(color = age_group))


