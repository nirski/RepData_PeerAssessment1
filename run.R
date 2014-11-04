library(xts)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
# library(devtools)
# install_github("jrnold/ggthemes")
library(ggthemes)
library(pander)

activity <- read.csv("data/activity.csv", stringsAsFactors = FALSE) %>%
    tbl_df %>%
    mutate(
        hour = sprintf("%04d", interval),
        hour.posix = ymd_hm(paste("1900-01-01", hour)),
        time = ymd_hm(paste(date, hour)),
        weekday = wday(time)
    )

activity.daily <- activity %>%
    group_by(date) %>%
    summarise(steps = sum(steps))

activity.hourly <- activity %>%
    group_by(hour.posix) %>%
    summarise(steps = mean(steps, na.rm = TRUE))

activity %>% filter(!complete.cases(activity)) %>% tally
summary(activity)
summary(activity.daily)
table(is.na(activity$steps))

activity.daily %>%
    ggplot() +
    geom_histogram(aes(steps), binwidth = 2000)

activity.hourly %>%
    ggplot() +
    geom_line(aes(hour.posix, steps)) +
    scale_x_datetime(labels = date_format("%H:%M")) +
    theme_pander() +
    scale_colour_pander()

activity.xts <- with(activity, xts(steps, time))
activity.xts.subset <- activity.xts["2012-10-01/2012-10-07"]
# activity.xts.subset <- activity.xts["T08:00/T16:00"]

activity.xts.daily <- apply.daily(
    activity.xts,
    sum
)
activity.xts.subset.daily <- apply.daily(
    activity.xts.subset,
    sum
)
activity.xts.subset.hourly <- period.apply(
    activity.xts.subset,
    endpoints(activity.xts.subset, "hours", 1),
    sum
)

plot(activity.xts.subset)
plot(activity.xts.subset.hourly)

activity.xts.subset.hourly %>%
#     na.approx %>%
    fortify %>%
    ggplot() +
    geom_line(aes(Index, activity.xts.subset.hourly)) +
    scale_x_datetime(breaks = date_breaks("1 day"))

activity.xts %>%
    na.approx %>%
    as.data.frame %>%
    tbl_df

autoplot(activity.xts.daily)
autoplot(activity.xts.daily %>% na.approx)
autoplot(activity.xts.daily %>% na.aggregate)
autoplot(activity.xts.daily %>% na.locf)
