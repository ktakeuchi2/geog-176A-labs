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
  filter(state == "CA") %>%
  group_by(county) %>%
  slice_max(pop19, n=1) %>%
  right_join(dat, pop, by = "fips") %>%
  mutate(cases_pcap = (cases/pop19) * 100000, newcases_pcap = (cases/pop19) * 100000) %>%
  ungroup() %>%
  filter(date == max(date))

cases_pcapita = pop %>%
  slice_max(cases_pcap, n = 5) %>%
  select(county.y, cases_pcap)

knitr::kable(cases_pcapita,
             caption = "Most Cumulative Cases Per Capita",
             col.names = c("County", "Cases Per Capita")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

newcases_pcapita = pop %>%
  slice_max(newcases_pcap, n = 5) %>%
  select(county.y, newcases_pcap)

knitr::kable(newcases_pcapita,
             caption = "Most New Cases Per Capita",
             col.names = c("County", "New Cases Per Capita")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

# covid data to last 14 days
# total number of cases per county
# total number of new cases per county
# number of safe counties (less than 100 new cases in past 14 days per 100,000 residents)
# filter(date == max(date) - 13) %>%

last_14 = pop %>%
  filter(date == max(date) - 13) %>%
  select(county = county.y, newCases, pop19, date) %>%
  group_by(county, pop19) %>%
  summarize(tot_cases = sum(newCases)) %>%
  ungroup() %>%
  mutate(newcasespcap = (tot_cases)/(pop19/100000))

tot_state_cases = pop %>%
  summarise(tot)

safe_county = last_14 %>%
  filter(newcasespcap < 100) %>%
  pull(county)



  summarize(safe = tot_casespcap < 100 == TRUE)

# The total number of cases is ,
# the total number of new cases is ,
# and the total number of safe counties (where there are less than 100 new cases
# per 100,000 residents over the past 14 days) is .

# Question 2: COVID Cases in New York, California, Louisiana, and Florida

library(zoo)

covid %>%
  filter(state == "New York"| state == "California"| state == "Louisiana" | state =="Florida") %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases)) %>%
  ungroup(state, date) %>%
  group_by(state) %>%
  mutate(newCases = cases - lag(cases),
         roll7 = rollmean(newCases, 7, fill = NA, align = "right")) %>%
  ggplot(aes (x = date)) +
  geom_col(aes(y = newCases), color = NA, fill = "lightblue3") +
  geom_line(aes(y = roll7, color = "blue", size = 0.5)) +
  labs(title = "Daily New Cases and 7-Day Rolling Mean",
       x = "Date",
       y = "Cases",
       subtitle = "Data Source: NY Times",
       caption = "Lab 02 Question 02") +
  facet_wrap(~state, scales = "free_y") +
  theme_linedraw() +
  ggsave(file = "img/lab02question02.png")

state_pop = pop %>%
  filter(state %in% c("NY", "CA", "LA", "FL")) %>%
  group_by(state.y) %>%
  slice_max(pop19, n = 1) %>%
  right_join(state_dat, by = "fips")
group_by(state.y, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(newCases = cases - lag(cases)) %>%
  filter(newCases > 0)




state_data <- inner_join(state_dat, state_pop, by = "state") %>%
  group_by(state) %>%
  mutate(cpc = sum(newCases)/(pop19)) %>%
  mutate(roll7 = rollmean(newCases, 7, fill = NA, align = "right")) %>%
  ungroup() %>%
  filter(newCases > 0)


