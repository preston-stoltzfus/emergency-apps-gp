install.packages("tidyverse")
install.packages("skimr")
install.packages("hms")
library(tidyverse)
library(skimr)
library(hms)

vet_hosp_apptimes <- read_csv("vet_hosp_apptimes_june2022_june2023.csv")

# getting more familiar with my data.View() & head() help me get my bearings and skim() to help find errors that need cleaned
View(vet_hosp_apptimes)
head(vet_hosp_apptimes)
skim(vet_hosp_apptimes)

# start cleaning

# formatting appt_date to date format & making sure app_time is in time format
vet_hosp_apptimes$appt_date <- as.Date(vet_hosp_apptimes$appt_date, format = "%m/%d/%Y")
vet_hosp_apptimes$appt_time <- as_hms(vet_hosp_apptimes$appt_time)

# making characters lowercase in "reason_desc" 
vet_hosp_apptimes <- vet_hosp_apptimes %>% 
  mutate(reason_desc = tolower(reason_desc))

# remove app times that are inaccurate (hosp. only open 7am-8pm)
vet_hosp_apptimes <- vet_hosp_apptimes %>% 
  filter(appt_time >= as_hms("07:00:00") & appt_time <= as_hms("20:00:00"))

# looking at unique values in "reason_desc", no misspelling or variances of "emergency" found
reason_desc_unique_values <- vet_hosp_apptimes %>% 
  count(reason_desc)

# select out only columns needed for analysis
vet_hosp_apptimes_select <- vet_hosp_apptimes %>% 
  select(appt_date, appt_time, reason_desc)

# filtering out only emergency apps
vet_hosp_apptimes_emergency <- vet_hosp_apptimes_select %>% 
  filter(reason_desc == "emergency")

# finished cleaning
# start analysis

# plot of emergency app time vs date, no correlation found
ggplot(vet_hosp_apptimes_emergency, aes(x = appt_date, y = appt_time)) + geom_point(color = "red") +
  labs(title = "Time of emergency appointments", subtitle = "June 2022 - 2023", x = "Date", y = "Time of Day") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(vet_hosp_apptimes_emergency$appt_date), y = min(vet_hosp_apptimes_emergency$appt_time),
           label = "Data collected from IDEXX Cornerstone Software", hjust = 1, vjust = 2)

# saving the file
ggsave(filename = "time_of_emergency_appointments_scatter_plot.png",  scale = 2, dpi = 600)


# plot of any appointment type just for curiosity sake.
ggplot(vet_hosp_apptimes_select, aes(x = appt_date, y = appt_time)) + geom_point() + facet_wrap(~reason_desc)
  labs(title = "Time of appointments", subtitle = "Data from June 1st 2022 - June 1st 2023", x = "Date", y = "Time of Day")
