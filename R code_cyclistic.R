library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(readr)
# importing the combined csv file of 12 month data (202103-202202)
processed_tripdata <- read_csv("202103-202202_processed_data.csv")
View(processed_tripdata)
# Inspecting dataset
str(processed_tripdata)
colnames(processed_tripdata)
# processing dataset
processed_tripdata$started_at <- ymd_hms(processed_tripdata$started_at)
processed_tripdata$ended_at <- ymd_hms(processed_tripdata$ended_at)
processed_tripdata$duration <- as.numeric(difftime(processed_tripdata$ended_at, processed_tripdata$started_at, units="min"))
processed_tripdata$month <- format(processed_tripdata$started_at, format="%B")
processed_tripdata$day_of_week <- format(processed_tripdata$started_at, format="%A")
processed_tripdata$hour <- format(processed_tripdata$started_at, format="%H")
glimpse(processed_tripdata)
processed_tripdata %>% 
  filter(duration <= 0) %>%
  count()
processed_tripdata <- processed_tripdata %>%
  filter(duration > 0)
glimpse(processed_tripdata)
head(processed_tripdata)
# Analyze dataset
# Customer time spent
ggplot(processed_tripdata, aes(x=duration, fill=member_casual)) +
  geom_histogram(binwidth = 1, color="white") +
  xlim(0,100)+
  xlab("Duration (Minutes)") +
  ylab("No. of Trips") +
  theme_linedraw() +
  ggtitle("Customer Time Spent on Bike Trip Distribution") + 
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5),
          legend.justification=c(1,0),
          legend.position=c(0.8,0.5)
        ) +
  guides(fill=guide_legend(title="Customer Type")) +
  scale_fill_manual(values=c('#FF992D','#066CCD'), limits = c("member", "casual")) +
  coord_flip()
# monthly bike trip 
processed_tripdata %>%
  select(month, member_casual) %>%
  group_by(month, member_casual) %>%
  count() %>%
  ggplot(aes(x=factor(month, level=c('January', 'February', 'March', 'April', 'May', 
                                     'June', 'July', 'August', 'September', 'October', 'November', 'December')), 
             y=n, fill=member_casual)) +
  geom_bar(stat="identity", position=position_dodge())+
  coord_flip()+
  scale_fill_manual(values=c('#FF992D','#066CCD'), limits = c("member", "casual")) +
  xlab("Month") +
  ylab("No. of Trips") +
  theme_linedraw() +
  ggtitle("Monthly Bike Trip Distribution") +
  guides(fill=guide_legend(title="Customer Type")) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.justification = c(1,0),
        legend.position = c(0.8,0.1)
  )
# hours bike trip 
df <- processed_tripdata %>%
  select(day_of_week, hour, member_casual) %>%
  filter(day_of_week == 'Saturday' | day_of_week == 'Sunday') %>%
  group_by(hour, member_casual) %>%
  count()
df$weekend_weekday = 'weekends'

df1 <- processed_tripdata %>%
  select(day_of_week, hour, member_casual) %>%
  filter(day_of_week != 'Saturday' & day_of_week != 'Sunday') %>%
  group_by(hour, member_casual) %>%
  count()

df1$weekend_weekday = 'weekdays'

df <- rbind(df, df1)

df %>%
  ggplot(aes(x=hour, y=n, fill=member_casual)) +
  geom_bar(stat="identity", position=position_dodge())+
  coord_flip()+
  scale_fill_manual(values=c('#FF992D','#066CCD'), limits = c("member", "casual")) +
  xlab("Hour of a Day") +
  ylab("No. of Trips") +
  theme_linedraw() +
  ggtitle("Hourly Bike Trip Distribution") +
  guides(fill=guide_legend(title="Customer Type")) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.justification = c(1,0),
        legend.position = c(0.97,0.1)
  ) +
  facet_wrap(~weekend_weekday)
# Bike type among customers and members
df <- processed_tripdata %>%
  select(member_casual, bike_type) %>%
  filter(member_casual == "casual") %>%
  group_by(bike_type, member_casual) %>%
  count()

df1 <- processed_tripdata %>%
  select(member_casual, bike_type) %>%
  filter(member_casual == "member") %>%
  group_by(bike_type, member_casual) %>%
  count() 

df2 <- rbind((df%>%mutate(countT= sum(df$n)) %>%
                group_by(bike_type, add=TRUE) %>%
                mutate(per=n/countT, per_label=paste0(round(100*n/countT,2),"%"))), 
             (df1%>%mutate(countT= sum(df1$n)) %>%
                group_by(bike_type, add=TRUE) %>%
                mutate(per=n/countT, per_label=paste0(round(100*n/countT,2),"%"))))

ggplot(df2, aes(x="", y=per, fill=bike_type)) +
  geom_col() +
  coord_polar(theta = "y") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = per_label), position = position_stack(vjust = 0.5), color="white") +
  scale_fill_manual(values=c('#FF992D','#066CCD','#AAAAAA'), limits = c("classic", "docked", "electric")) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 0), 
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
  ggtitle("Bike Type Distribution Among Casual Customers and Members") +
  guides(fill=guide_legend(title="Bike Type")) +
  facet_wrap(~member_casual)
# Day of week bike
processed_tripdata %>%
  select(day_of_week, bike_type, member_casual) %>%
  group_by(day_of_week, bike_type, member_casual) %>%
  count() %>% 
  ggplot(aes(x=factor(day_of_week, level=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 
                                           'Friday', 'Saturday', 'Sunday')), 
             y=n, fill=bike_type)) +
  geom_bar(stat="identity", position=position_dodge())+
  coord_flip()+
  scale_fill_manual(values=c('#FF992D','#066CCD','#AAAAAA'), limits = c("classic","docked","electric")) +
  xlab("Day of Week") +
  ylab("No. of Trips") +
  theme_linedraw() +
  ggtitle("Day of Week Bike Type Distribution") +
  guides(fill=guide_legend(title="Bike Type")) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.justification = c(1,0),
        legend.position = c(0.97,0.1)
  ) +
  facet_wrap(~member_casual)
# Monthly bike type
processed_tripdata %>%
  select(month, bike_type, member_casual) %>%
  group_by(month, bike_type, member_casual) %>%
  count() %>% 
  ggplot(aes(x=factor(month, level=c('January', 'February', 'March', 'April', 'May', 
                                     'June', 'July', 'August', 'September', 'October', 'November', 'December')), 
             y=n, fill=bike_type)) +
  geom_bar(stat="identity", position=position_dodge())+
  coord_flip()+
  scale_fill_manual(values=c('#FF992D','#066CCD','#AAAAAA'), limits = c("classic", "docked", "electric")) +
  xlab("Month") +
  ylab("No. of Trips") +
  theme_linedraw() +
  ggtitle("Monthly Bike Type Distribution") +
  guides(fill=guide_legend(title="Bike Type")) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.justification = c(1,0),
        legend.position = c(0.97,0.1)
  ) +
  facet_wrap(~member_casual)



