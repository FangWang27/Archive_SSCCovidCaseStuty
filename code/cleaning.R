# R code for data cleaning for the Covid Project
# Last updated on December 27,2020
# Student name: Fang Wang
# Student number : 1003432760
# Email : warng.wang@mail.utoronto.ca
# Program and Department: MsC statistics, 1st year; Department of Statistical Science



# raw_cases <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/cases.csv")
raw_ltc_summary <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/ltccovidsummary.csv")
#raw_ltc_active <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/activeltcoutbreak.csv")
raw_outbreaks <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/outbreak_cases.csv")
total_death <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/klehhlbyj.csv")
total_case <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/cuwms1og.csv")
ltc_bf <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/ltc_data_bf.csv")



require("tidyverse")
require("ggplot2")



# function handling negative count
replace_neg <- function(x) {
  # replace value of x that is negative by the mean of consecutive entry
  # the negative number can not be the fist or the last
  ind <- which(x < 0)
  sapply(ind, function(i) mean(c(x[i - 1], x[i + 1])))
}
precede_avg <- function(s, n) {
  # take a sequence seq and replace
  # each ith value  by the mean of n preceding value for all i>n
  #does not change if i<= n
  # s: numeric sequence
  # n : # of preceding value for which mean is calculated to replace
  N <- length(s)
  if (n == 1) {
    return(c(s[1], s[1:(N - 1)]))
  }
  mat_seqs <- do.call(rbind, lapply(1:(N - 1), function(i) { c(s[i:N], rep(0, i)) }))
  avg <- colMeans(as.matrix(mat_seqs[(1:n),]))
  return(c(s[1:n], avg[1:(N - n)]))
}
############################################

# daily death/ case data manually obtained for April
colnames(ltc_bf) <- c("date", "case", "death")

ltc_daily <- as.data.frame(ltc_bf) %>%
mutate(date = as.Date(date, "%d-%b"), death = as.numeric(death), case = as.numeric(case)) %>%
mutate(death = death - lag(death, 1), case = case - lag(case, 1)) %>%
na.omit %>%
mutate(death = replace(death, death < 0, replace_neg(death))) %>%
mutate(case = replace(case, case < 0, replace_neg(case)))

# total daily death count
df_death <- total_death %>%
select(date = category, death = New.deaths) %>%
mutate(date = substring(date, 5)) %>%
mutate(date = tolower(gsub(" ", "/", date))) %>%
mutate(date = as.Date(date, "%b/%d/%y"))



# ltc death count aftere April 25
death_ltc <- raw_ltc_summary %>%
select(date = Report_Data_Extracted, death = Total_LTC_Resident_Deaths) %>%
mutate(date = as.Date(date, "%Y-%m-%d")) %>%
mutate(death = death - lag(death, 1)) %>%
slice(-1) %>%
mutate(death = replace(death, death < 0, replace_neg(death)))

# death_ltc without cleaning
death_ltc_raw <-  raw_ltc_summary %>%
select(date = Report_Data_Extracted, death = Total_LTC_Resident_Deaths) %>%
mutate(date = as.Date(date, "%Y-%m-%d")) %>%
mutate(death = death - lag(death, 1)) %>% 
na.omit


# combine them
date_start <- as.Date("2020-04-06")
date_end_1 <- as.Date("2020-04-24")
date_end <- as.Date("2020-12-05")

death_total <- subset(df_death, difftime(date, date_start) >= 0 & difftime(date, date_end) <= 0)
death_ltc1 <- subset(ltc_daily, difftime(date, date_start) >= 0 & difftime(date, date_end_1) <= 0, select = death)


# the death count obtained by combining two data sets
death_combined <- data.frame(date = death_total$date,
ltc = c(death_ltc1$death, death_ltc$death),
total = death_total$death
)

# data cleaning and organizing
death_cleaned <- death_combined %>%
mutate(non_ltc = total - ltc) %>%
mutate(non_ltc_pos = replace(non_ltc, non_ltc < 0, mean(non_ltc[non_ltc > 0])))






# the case numbers-------------------

df_outbreak <- raw_outbreaks %>%
select(date, type = outbreak_subgroup, case = TOTAL_CASES) %>%
mutate(is_ltc = type == "Long-Term Care Home") %>%
select(date, is_ltc, case) %>%
group_by(date, is_ltc) %>%
summarise(across(c(case), sum)) %>%
mutate(date = as.Date(date, "%m/%d/%Y"))

# case count-----------------------------

ggplot(death_cleaned, aes(x = date)) +
geom_line(aes(y =ltc), col = "green") +
geom_line(aes(y = total), col = "blue")




# plotting---------------------------------
# plot cleaned death data 
df_death_plt <- death_cleaned %>% 
select(date, total, ltc) %>% 
gather(key = "type", value = "count", -date) %>%
ggplot( aes(x = date)) +
geom_line(aes(y = count, col = type))


# death from death_ltc, data after April 25
ggplot(death_ltc) +
geom_line(aes(x = date, y = death))

# brute force obtained data

ggplot(death_ltc_raw) +
geom_line(aes( x= date, y = death))



#plot case number
df_outbreak$date <- as.Date(df_outbreak$date)
ggplot(df_outbreak, aes(x = date, y = case)) +
 geom_line(aes(color = is_ltc))



save(death_cleaned, precede_avg, ltc_bf, ltc_daily,df_outbreak, file = "cleaned_death.RData")
save(df_outbreak, file = "case.RData")


ls()