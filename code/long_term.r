


require("dplyr")
require("ggplot2")
require("tidyverse")
require("mgcv")


# import raw data
# dfCovid_raw <- read.csv("C:/Users/wangf/OneDrive - Seneca College of Applied Arts & Technology/Current/Covid/Data/13100781.csv")
dfCovid <- dfCovid_raw %>%
  select(case_id = Case.identifier.number, case_info = Case.information, value = VALUE) %>%
    filter(case_info %in% c("Episode week", "Age group", "Hospital status", "Recovered", "Death", "Recovery week", "Occupation", "Region")) %>%
  spread(case_info, value)


# case count for long-term caring home

df_case_longterm <- dfCovid %>%
    filter(Death == 2) %>%
    select(-c("Death", "case_id", "Recovery week", "Recovered")) %>%
    rename(age = 'Age group', time_week = 'Episode week', hospital = 'Hospital status') %>%
    transmute(time_week, is_longterm = (Occupation == 3)) %>%
    group_by(time_week, is_longterm) %>%
    summarize(counts = n()) %>%
 spread("is_longterm", "counts") %>%
replace(is.na(.), 0)



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

#weekly average case

# replace value by the average of three preceding weeks
df_case_longterm_avg <- df_case_longterm
df_case_longterm_avg[, 2:3] <- apply(df_case_longterm[, 2:3], 2, function(s) { precede_avg(s, 3) })

View(df_case_longterm_avg)

# the format easy to work with
df_case_longterm_avg_final <- df_case_longterm_avg %>%
 gather(key = "is.longterm", value = "count", 2:3)

View(df_case_longterm_avg_final)
# weekly death

df_death_longterm <- dfCovid %>%
    filter(Death == 1) %>%
    select(-c("Death", "case_id", "Recovery week", "Recovered")) %>%
    rename(age = 'Age group', time_week = 'Episode week', hospital = 'Hospital status') %>%
    transmute(time_week, is_longterm = (Occupation == 3)) %>%
    group_by(time_week, is_longterm) %>%
    summarize(counts = n()) %>%
# spread("is_longterm","counts")  %>% 
replace(is.na(.), 0)

View(df_death_longterm)
save.image(file = "long_term.RData")
