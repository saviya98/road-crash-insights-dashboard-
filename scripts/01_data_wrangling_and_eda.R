#wrangling
library(readr)
df <- read_csv("traffic_accidents.csv") 

library(dplyr)    
library(stringr)  
library(forcats)   
library(lubridate)

#fix data types
df1 <- df %>% 
  mutate(
    crash_dt = parse_datetime(crash_date,
                              format = "%m/%d/%y %H:%M",
                              locale = locale(tz = "Pacific/Auckland")),
    intersection = intersection_related_i == "Y",
    damage = factor(damage,
                    levels = c("$500 OR LESS",
                               "$501 - $1,500",
                               "OVER $1,500"),
                    ordered = TRUE)
  )

names(df1)
View(df1)

#remove duplicates
df1 <- df1 %>% distinct()
View(df1)

#check for Unknown,Other,NA values
na_words <- c("UNKNOWN", "OTHER" ,"NOT APPLICABLE")

df1 %>% 
  summarise(across(
    where(is.character),
    ~sum(.x %in% na_words, na.rm = TRUE),
    .names = "n_{col}"
  ))

# remove NAs
df1 <- df1 %>% 
  mutate(across(where(is.character),
                ~replace(.x, .x %in% na_words, NA)))

View(df1)
# trim spaces
df1 <- df1 %>% 
  mutate(across(where(is.character),
                ~str_trim(.x) %>% str_squish() %>% str_to_title()))

dim(df1)
#creating new columns

df1 <- df1 %>% 
  mutate(
    year       = year(crash_dt),
    month      = month(crash_dt, label = TRUE, abbr = TRUE),
    weekday    = wday(crash_dt, label = TRUE),
    is_weekend = weekday %in% c("Sat", "Sun"),
    hour       = hour(crash_dt),
    is_night   = hour %in% c(0:5, 20:23),
    precip     = weather_condition %in%
      c("RAIN", "FREEZING RAIN/DRIZZLE",
        "SLEET/HAIL", "SNOW"),
    peak_hour  = hour %in% c(7:9, 16:18)
  )

df1 <- df1 %>% 
  mutate(
    severity = case_when(
      injuries_fatal                > 0 ~ "Fatal",
      injuries_incapacitating       > 0 ~ "Severe",
      injuries_non_incapacitating   > 0 ~ "Moderate",
      injuries_reported_not_evident > 0 ~ "Possible",
      injuries_no_indication        > 0 ~ "No-Injury",
      TRUE                          ~ "Unknown"
    ),
    severity = factor(severity,
                      levels = c("Fatal","Severe",
                                 "Moderate","Possible",
                                 "No-Injury","Unknown"),
                      ordered = TRUE)
  )

dim(df1)
View(df1)

r_miss <- sapply(df1, \(x) mean(is.na(x)))
print(r_miss)
#saving to csv file
write_csv(df1,"cleaned_accident_data.csv")

#EDA
library(tidyverse)
install.packages("naniar")
library(naniar) 
install.packages("GGally")
library(GGally) 

dim(df1)

sapply(df1, class)
table(is.na(df1))

#central tendency of crash count per month
monthly <- df1 %>%                           
  mutate(year_month = floor_date(crash_dt, "month")) %>%  
  count(year_month, name = "crashes")

mean(monthly$crashes)       
median(monthly$crashes)
sd(monthly$crashes)

summary(df1$num_units)   #  Min 1,  Median 2,  95th % = 3
table(df1$lighting_condition)


#Crashes by hour
ggplot(df1, aes(hour)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Crashes by Hour", x = "Hour", y = "Count")

#crashes by weather condition
df1 %>% 
  count(weather_condition, sort = TRUE) %>% 
  slice_head(n = 10) %>%
  ggplot(aes(reorder(weather_condition, n), n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Crashes by Weather Condition", x = "", y = "Crashes")


#severity of crashes
df1 %>% 
  count(severity) %>% 
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(severity, pct)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Crashes by injury severity",
       x = "", y = "Percent of crashes")


#severity by weather
ggplot(df1, aes(weather_condition, fill = severity)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Severity by Weather", y = "Percent")


#Hourly crash trend
df1 %>% 
  count(hour) %>% 
  ggplot(aes(hour, n)) +
  geom_line() +
  labs(title = "Hourly crash trend", x = "Hour", y = "Crashes")

acc_num <- df1 %>% select(where(is.numeric))
ggcorr(acc_num, label = TRUE)

#Crashes by Day and Night
df1 %>% 
  count(weekday, is_night) %>% 
  ggplot(aes(weekday, n, fill = is_night)) +
  geom_col(position = "dodge") +
  labs(title = "Crashes by Day and Night", x = "", y = "Count")


#vehicles involved by injury severity
ggplot(df1, aes(severity, num_units)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.1) +
  scale_y_continuous(breaks = 0:8) +
  labs(title = "Vehicles involved by injury severity",
       x = "Injury level", y = "Number of vehicles")



acc_num <- df1 %>% select(where(is.numeric))
ggcorr(acc_num, label = TRUE)  


ggplot(df1 %>% filter(!is.na(roadway_surface_cond)),
       aes(roadway_surface_cond, fill = severity)) +
  geom_bar(position = "fill") + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Severity share by Road Surface", x = "", y = "Percent")


# count crashes and fatals by hour
hour_summary <- df1 %>% 
  count(hour, wt = (severity %in% c("Fatal","Severe")), name = "fatal_severe") %>% 
  full_join(df1 %>% count(hour, name = "total"), by = "hour") %>% 
  mutate(rate = fatal_severe / total)

ggplot(hour_summary, aes(hour, total)) +
  geom_col(fill = "red") +
  geom_line(aes(y = rate * max(total)), size = 1) +
  scale_y_continuous(
    name = "Crash count",
    sec.axis = sec_axis(~ . / max(hour_summary$total),
                        labels = scales::percent,
                        name = "Fatal–Severe rate")) +
  labs(title = "Crash volume and harm rate by hour",
       x = "Hour of day")

summary(df1$severity)
str(df1)
cor(df1,use = "pair")
count(df1, lighting_condition, sort = TRUE)


ggplot(df1 %>% filter(!is.na(roadway_surface_cond)),
       aes(roadway_surface_cond, fill = severity)) +
  geom_bar(position = "fill") + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Severity share by Road Surface", x = "", y = "Percent")


install.packages("slider")
library(slider)


library(lubridate)
library(ggplot2)

#crashes according to month over years
monthly <- df1 %>% 
  filter(year(crash_dt) >= 2020) %>% 
  mutate(year_month = as.Date(floor_date(crash_dt, "month"))) %>%  # make Date
  count(year_month, name = "crashes")

ggplot(monthly, aes(year_month, crashes)) +
  geom_line(size = 0.2) +
  geom_point() +
  geom_text(aes(label = format(year_month, "%b")),
            vjust = -0.5, size = 3) +
  scale_x_date(date_breaks = "12 month", date_labels = "%Y") +
  labs(title = "Monthly crash count",
       x = "Month", y = "Number of crashes") +
  theme_minimal() 


