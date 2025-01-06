#Muye Day 1: IFLS Awal
library(haven)
library(dplyr)
bk_ar1 <- read_dta("C:/Users/Muflih/Downloads/hh14_all_dta/bk_ar1.dta")
View(bk_ar1)

#Generate unique id of the fifth wave of IFLS (IFLS 2014) ----
#Check the length of hhid14 & pid14 digits
check_hhid14 <- bk_ar1 %>%
  mutate(check = nchar(hhid14)) #To check the length of the variable character
mean(check_hhid14$check)
max(check_hhid14$check)

check_pid14 <- bk_ar1 %>%
  mutate(check = nchar(pid14)) #To check the length of the variable character
mean(check_pid14$check)
max(check_pid14$check)

#Create new variable
bk_ar1 <- bk_ar1 |>
  mutate(id_new = paste(hhid14, sprintf("%02d", pid14), sep = ""))

#Validation check, if all id is unique or otherwise
validation_check <- bk_ar1 |>
  summarize(
    distinct_count = n_distinct(id_new),  # Count of unique `id_new`
    total_count = n()                     # Total count of rows
  ) |>
  mutate(difference = total_count - distinct_count)  # Calculate the difference

ifelse(validation_check$difference == 0, "all unique", "not unique")

print(validation_check)