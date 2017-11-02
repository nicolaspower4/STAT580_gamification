###
### Exploratory Analysis of Cleaned Database Data
###
library(plyr)
dat_clean <- read_csv("C:/Users/ckell/OneDrive/Penn State/2017-2018/580/Gamification/stats consultant/Database data/database_cleaned.csv")

hist(dat_clean$visits)
hist(dat_clean$course_total)
hist(dat_clean$total_assm)
hist(dat_clean$total_time)
hist(dat_clean$avg_time)


count(dat_clean, 'female')
count(dat_clean, 'game')
count(dat_clean, 'teacher')
count(dat_clean, 'eng_class')

cor(dat_clean$total_assm, dat_clean$total_time)
cor(dat_clean$total_assm, dat_clean$avg_time)
cor(dat_clean$total_time, dat_clean$avg_time)
cor(dat_clean$total_time, dat_clean$course_total)

dat_clean$avg_assm <- dat_clean$total_assm/dat_clean$visits
dat_clean$avg_time2 <- dat_clean$total_time/dat_clean$visits
cor(dat_clean$avg_assm, dat_clean$avg_time)
cor(dat_clean$avg_time2, dat_clean$avg_assm)


plot(dat_clean$total_assm, dat_clean$avg_time)

#first table 3xx and second tale 264
