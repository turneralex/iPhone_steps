library(tidyverse)
library(lubridate)
library(ggmosaic)
library(ggridges)
library(ggpmisc)
library(RcppRoll)
library(XML)
library(rlang)

# data prep

health_xml <- xmlParse("export.xml")

health <- health_xml %>% xmlToList()

health %>% str(list.len = 5)

types <- map(health, "type") %>% 
  unlist() %>% 
  unique()

types

steps_index <- health %>% 
  map_lgl(~ types[3] %in% .)

steps <- health[steps_index]

steps[1]

step_counts <- steps %>% 
  map("value") %>% 
  as.integer() %>% 
  unname()

step_counts[1:5]

steps_time_stamps <- steps %>%  
  map_chr("endDate") %>% 
  unname() %>% 
  ymd_hms(tz = "Australia/Melbourne")

steps_time_stamps[1] 

steps_df <- tibble(
  time_stamp = steps_time_stamps,
  date = time_stamp %>% as_date(),
  step_counts = step_counts
)

steps_df

distances_index <- health %>% 
  map_lgl(~ types[4] %in% .)

distances <- health[distances_index]

distances[1]

distance_covered <- distances %>% 
  map("value") %>%
  as.numeric() %>% 
  unname()

distance_covered[1:5]

distances_time_stamps <- distances %>% 
  map_chr("endDate") %>% 
  unname() %>% 
  ymd_hms(tz = "Australia/Melbourne")

distances_time_stamps[1]

distances_df <- tibble(
  time_stamp = distances_time_stamps,
  date = time_stamp %>% as_date(),
  distance_covered = distance_covered
)

distances_df

steps_summary <- steps_df %>% 
  group_by(date) %>% 
  summarise(total_steps = step_counts %>% sum()) %>% 
  mutate(year = date %>% year() %>% factor(),
         month = date %>% month() %>% factor(),
         day_of_week = date %>% wday(label = T, week_start = 1) %>% factor(ordered = F), 
         weekday_weekend = if_else(day_of_week %in% c("Sat", "Sun"), "Weekend", "Week Day") %>% factor())

steps_summary

distances_summary <- distances_df %>% 
  group_by(date) %>% 
  summarise(total_distance = distance_covered %>% sum()) %>% 
  mutate(year = date %>% year() %>% factor(),
         month = date %>% month() %>% factor(),
         day_of_week = date %>% wday(label = T, week_start = 1) %>% factor(ordered = F), 
         weekday_weekend = if_else(day_of_week %in% c("Sat", "Sun"), "Weekend", "Week Day") %>% factor())

distances_summary

steps_summary %>% 
  group_by(year) %>% 
  count()

all_dates <- seq.Date(steps_summary$date[1], steps_summary$date[nrow(steps_summary)], by = 1) %>% 
  enframe(name = NULL) %>% 
  rename(date = value)

missing_dates <- anti_join(all_dates, steps_summary, by = "date")

missing_dates

missing_dates <- missing_dates %>% 
  mutate(day_before = date - days(1),
         day_after = date + days(1))

steps_summary %>% 
  filter(date %in% missing_dates$day_before | date %in% missing_dates$day_after)

steps_summary <- steps_summary %>% 
  mutate(
    total_steps = if_else(
      date %in% (missing_dates %>% pull(day_before)) | date %in% (missing_dates %>% pull(day_after)),
      steps_summary$total_steps %>% mean() %>% as.integer(),
      total_steps
    )
  ) 

missing_date_data <- missing_dates %>% 
  select(date) %>% 
  mutate(total_steps = steps_summary$total_steps %>% mean() %>% as.integer(),
         year = date %>% year() %>% factor(),
         month = date %>% month() %>% factor(levels = 1:12),
         day_of_week = date %>% wday(label = T, week_start = 1) %>% factor(ordered = F),
         weekday_weekend = if_else(day_of_week %in% c("Sat", "Sun"), "Weekend", "Week Day") %>% factor())

steps_summary <- steps_summary %>% 
  bind_rows(missing_date_data) %>% 
  arrange(date)

steps_summary %>% 
  group_by(year) %>% 
  count()

update_index <- steps_summary$date %>% 
  map_lgl(~ . %in% missing_dates$date | . %in% missing_dates$day_before | . %in% missing_dates$day_after) 

update_values <- map_int(
  steps_summary$total_steps[update_index], 
  ~ (. + sample(-sd(steps_summary$total_steps):sd(steps_summary$total_steps), 1)) %>% 
    as.integer()
) 

steps_summary <- steps_summary %>% 
  mutate(total_steps = replace(total_steps, update_index, update_values))

# summaries

options(scipen = 999)

steps_summary$total_steps %>% mean()

steps_summary$total_steps %>% sd()

summary_fun <- function(variable) {
  quo_variable <- enquo(variable)
  
  steps_summary %>% 
    group_by(!!quo_variable) %>% 
    summarise(mean = mean(total_steps),
              sd = sd(total_steps)) 
}

var_names <- colnames(steps_summary %>% select(year:weekday_weekend)) %>% 
  syms()

var_names %>% map(summary_fun) 

bar_plot <- function(variable) {
  quo_variable <- enquo(variable)
  quo_text <- quo_text(quo_variable) %>% tools::toTitleCase()
  
  steps_summary %>% 
    group_by(!!quo_variable) %>% 
    summarise(mean_steps = total_steps %>% mean()) %>% 
    ggplot(aes(!!quo_variable, mean_steps)) +
    geom_col(position = "dodge") +
    labs(title = paste("Mean Steps by", quo_text),
         x = quo_text,
         y = "Total Steps")
}

var_names %>% map(bar_plot)

# years

steps_summary %>% 
  ggplot(aes(total_steps)) +
  geom_histogram(binwidth = 200) +
  geom_vline(xintercept = mean(steps_summary$total_steps), colour = "dodgerblue", size = 2)

steps_summary %>%
  ggplot(aes(total_steps)) +
  geom_histogram(binwidth = 200) +
  geom_vline(xintercept = mean(steps_summary$total_steps), colour = "dodgerblue", size = 2) +
  facet_grid(year ~ .) +
  geom_vline(data = var_summary[[1]], aes(xintercept = mean), colour = "red", size = 2)

steps_summary %>%
  ggplot(aes(total_steps, fill = weekday_weekend)) +
  geom_histogram(binwidth = 200) +
  geom_vline(xintercept = mean(steps_summary$total_steps), size = 2) +
  scale_fill_brewer(palette = "Dark2") +
  facet_grid(year ~ .) 

# steps_summary %>% 
#   group_by(year, weekday_weekend) %>% 
#   summarise(above = sum(total_steps > mean(steps_summary$total_steps)),
#             prop_above = round(above / n(), 2)) %>% 
#   ggplot(aes(year, prop_above, fill = weekday_weekend)) +
#   geom_col(position = "dodge") +
#   scale_fill_brewer(palette = "Dark2")

steps_summary %>% 
  group_by(year, weekday_weekend) %>% 
  summarise(mean = mean(total_steps)) %>% 
  ggplot(aes(year, mean, group = weekday_weekend, colour = weekday_weekend)) +
  geom_line(size = 2) +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 8000))

# months

month_ridge_1 <- steps_summary %>% 
  filter(month %in% 1:6) %>% 
  ggplot(aes(total_steps, fct_rev(month))) +
  geom_density_ridges(stat = "binline", bins = 50, size = 0.5, scale = 1, fill = "seagreen1") +
  geom_vline(xintercept = mean(steps_summary$total_steps), linetype = "dashed", size = 1) 


month_ridge_2 <- steps_summary %>% 
  filter(month %in% 7:12) %>% 
  ggplot(aes(total_steps, fct_rev(month))) +
  geom_density_ridges(stat = "binline", bins = 50, size = 0.5, scale = 1, fill = "seagreen1") +
  geom_vline(xintercept = mean(steps_summary$total_steps), linetype = "dashed", size = 1) 

cowplot::plot_grid(month_ridge_1, month_ridge_2)

steps_summary %>% 
  filter(year != 2015) %>% 
  ggplot(aes(month, total_steps, fill = year)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2")

# day of week

steps_summary %>% 
  ggplot(aes(total_steps, fct_rev(day_of_week))) +
  geom_density_ridges(stat = "binline", bins = 80, size = 0.5, scale = 1, fill = "lightcoral") +
  geom_vline(xintercept = mean(steps_summary$total_steps), linetype = "dashed", size = 1) 

steps_summary %>% 
  filter(year != 2015) %>% 
  ggplot(aes(day_of_week, total_steps, fill = year)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2")

# other

rolling_mean <- RcppRoll::roll_mean(steps_summary$total_steps, n = 30)

diff <- nrow(steps_summary) - length(rolling_mean)

rolling_mean <- c(rolling_mean, rep(rolling_mean[length(rolling_mean)], diff))

steps_summary %>% 
  ggplot(aes(date, total_steps)) +
  geom_line() +
  geom_line(data = steps_summary %>% mutate(y = rolling_mean),
            aes(date, y),
            colour = "dodgerblue",
            size = 2) +
  scale_y_continuous(breaks = seq(0, 20000, by = 1000)) 

steps_summary %>% 
  inner_join(distances_summary, by = "date") %>%
  filter(!(date %in% missing_dates$date | date %in% missing_dates$day_before | date %in% missing_dates$day_after)) %>% 
  ggplot(aes(total_steps, total_distance)) +
  geom_point(size = 3, alpha = 0.25) +
  geom_smooth(method = lm) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = T)  

year_day_xtab <- xtabs(
  total_steps ~ year + day_of_week,
  data = steps_summary
) %>% 
  as_tibble() %>% 
  filter(year != 2015) %>% 
  mutate(year = year %>% factor(),
         day_of_week = day_of_week %>% factor(levels = steps_summary$day_of_week %>% levels()))

year_day_xtab %>% 
  ggplot(aes(year, fct_rev(day_of_week), fill = n)) +
  geom_raster() +
  scale_fill_gradient(low = "white", 
                      high = "red",
                      limits = c(0, max(year_day_xtab$n)))

steps_summary %>%
  filter(year != 2015) %>% 
  ggplot(aes(day_of_week, total_steps, colour = day_of_week)) +
  geom_jitter(width = 0.2) +
  facet_wrap(~ year) +
  guides(colour = F)

year_day_xtab %>% 
  ggplot() +
  geom_mosaic(aes(x = product(day_of_week), weight = n, fill = year))

