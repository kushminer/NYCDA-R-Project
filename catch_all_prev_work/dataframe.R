
# ----------------- > COST PER VARIABLE
## *** results are different : review *** ##

# cost per var daily mean
combined_data %>%
  spread(variable, value) %>%
  mutate(CPI = `Spend (USD)` / Impressions,
         CPR = `Spend (USD)` / Reach,
         CPC = `Spend (USD)` / `Website Clicks`,
         CPS = `Spend (USD)` / Searches,
         CPV = `Spend (USD)` / `View Content`,
         CPA = `Spend (USD)` / `Add to Cart`,
         CPP = `Spend (USD)` / Purchase) %>%
  select(group, CPR,CPC,CPP) %>%
  group_by(group) %>%
  summarise(across(where(is.numeric), mean)) %>%
  arrange(group)

# cost per var derived from total
combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  spread(variable, sum) %>%
  mutate(CPI = `Spend (USD)` / Impressions,
         CPR = `Spend (USD)` / Reach,
         CPC = `Spend (USD)` / `Website Clicks`,
         CPS = `Spend (USD)` / Searches,
         CPV = `Spend (USD)` / `View Content`,
         CPA = `Spend (USD)` / `Add to Cart`,
         CPP = `Spend (USD)` / Purchase) %>%
  select(group, `Spend (USD)`, CPI,CPC,CPP)

# ----------------- > DAILY MEAN PER VARIABLE

combined_data %>%
  spread(variable, value)  %>%
  group_by(group) %>%
  summarise(across(where(is.numeric), mean)) %>%
  select(group, `Spend (USD)`,Reach)

# ----------------- > TOTAL PER VARIABLE

# Total derived multiplying mean by number of days
combined_data %>%
  spread(variable, value)  %>%
  group_by(group) %>%
  summarise(across(where(is.numeric), mean)) %>%
  mutate(across(where(is.numeric), ~ . * 29)) %>%
  select(group, `Spend (USD)`,Reach)

# Total derived from sum of all values per group
combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value))  %>%
  spread(variable, sum) %>%
  select(group, Impressions, `Website Clicks`, Purchase)

# ----------------- > LENGTH OF TEST

combined_data %>%
  distinct(Date)  %>%
  nrow()

