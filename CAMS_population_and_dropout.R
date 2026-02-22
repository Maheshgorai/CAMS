library(dplyr)
library(data.table)
library(tidyr)
library(writexl)
library(purrr)

NSS <- fread("D:/Sch_edu/UDISE+/NSS79CAMS_Member.txt")
View(NSS)

dropout_basee <- NSS %>%
  mutate(
    final_weight = V57/100,
    Dropout = if_else(V21==2, "Yes", "No"),
    Age = as.integer(V19)  
  ) %>%
  filter(Dropout == "Yes", between(Age, 6, 18))


age_groups <- list(
  "Age 6-13" = 6:13,
  "Age 6-14" = 6:14,
  "Age 14-18" = 14:18,
  "Age 15-18" = 15:18
)

# Calculate population sums independently for each age group (allows overlaps to be handled correctly)
dropoutt <- map_dfr(names(age_groups), ~{
  range <- age_groups[[.x]]
  dropout_basee %>%
    filter(Age %in% range) %>%
    summarise(Population = sum(final_weight, na.rm = TRUE)) %>%
    mutate(Age_group = .x)
}) %>%
  pivot_wider(
    names_from = Age_group,
    values_from = Population,
    values_fill = 0
  )

# Add India_Total as the sum for the full 6-18 range (non-overlapping total)
dropoutt <- dropoutt %>%
  mutate(India_Total = dropout_basee %>%
           summarise(Population = sum(final_weight, na.rm = TRUE)) %>%
           pull(Population))

View(dropoutt)
write_xlsx(




