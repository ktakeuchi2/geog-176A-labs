#Kiki Takeuchi
#8/12/20
#Lab 02

# Question 1: COVID Cases in California

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)

library(knitr)
dat = covid %>%
  filter(state == "California") %>%
  group_by(county) %>%
  mutate(newCases = cases - lag(cases)) %>%
  ungroup() %>%
  filter(date == max(date))

most_cases = dat %>%
  group_by(county) %>%
  summarise(sum_cases = sum(cases)) %>%
  arrange(-sum_cases) %>%
  slice_max(sum_cases, n = 5) %>%
  select(county, sum_cases)

knitr::kable(most_cases,
             caption = "Most Cumulative Cases in California",
             col.names = c("County", "Cases")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

most_new_cases = dat %>%
  slice_max(newCases, n = 5) %>%
  select(county, newCases)

knitr::kable(most_new_cases,
             caption = "Most New Cases in California",
             col.names = c("County", "New Cases")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

# Number of Cases in the Last 14 Days Per 100,000 People
# Using Population Estimates Data

library(readxl)
PopulationEstimates <- read_excel("data/PopulationEstimates.xls",
                                  skip = 2)
pop = PopulationEstimates %>%
  select(fips = FIPStxt, pop19 = POP_ESTIMATE_2019, state = State, county = Area_Name) %>%
  group_by(state) %>%
  slice_max(pop19, n=1) %>%
  right_join(dat, pop, by = "FIPStxt") %>%
  group_by(county) %>%
  mutate(new_cases_per_cap = sum(most_new_cases)/pop19) %>%
  ungroup()

most_cases_per_cap = pop %>%
  group_by(county) %>%
  summarize(cases_per_cap = sum(cases)/pop19) %>%
  slice_max(cases_per_cap, n = 5) %>%
  select(county, cases_per_cap)

knitr::kable(most_cases_per_cap,
             caption = "Most Cumulative Cases Per Capita",
             col.names = c("County", "Cases Per Capita")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

most_new_cases_per_cap = pop %>%
  slice_max(new_cases_per_cap, n = 5) %>%
  select(county, new_cases_per_cap)

knitr::kable(most_new_cases_per_cap,
             caption = "Most New Cases Per Capita",
             col.names = c("County", "New Cases Per Capita")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

pop %>%
  filter(date == max(date)-13) %>%
  group_by(county, pop19) %>%
  summarize(tot_cases = sum(cases) / 100000) %>%
  mutate(newCases = cases - lag(cases)) %>%
  summarize(new_Cases = sum(newCases) / 100000) %>%
  ungroup() %>%
  count(new_Cases)

# The total number of cases is ,
# the total number of new cases is ,
# and the total number of safe counties (where there are less than 100 new cases
# per 100,000 residents over the past 14 days) is .

# Question 2: COVID Cases in New York, California, Louisiana, and Florida

library(zoo)

covid %>%
  group_by(state) %>%
  filter(state == "NY", "CA", "LA", "FL") %>%
  group_by(date) %>%
  summarize(cases = sum(cases)) %>%
  mutate(newCases = cases - lag(cases),
         roll7 = rollmean(newCases, 7, fill = NA, align = "right")) %>%
  ggplot(aes (x = date)) +
  geom_col(aes(y = newCases), col = state, fill = state) +
  geom_line(aes(y = roll7, col = state, size = 1)) +
  facet_wrap(date~state, scales = "free_y") +
  labs(title = "Daily New Cases and 7-Day Rolling Mean for New York, California, Louisiana, and Florida",
       x = "Date",
       y = "Cases",
       subtitle = "Data Source: NY Times",
       caption = "Lab 02 Question 02") +
  theme_linedraw()




