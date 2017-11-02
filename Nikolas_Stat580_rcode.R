#################################
##     REAL ROCK STAT580       ##
#################################
#This is my draft code
data=read.csv('database_cleaned.csv', header=T)
attach(data)
avg_assm=data$total_assm/data$m30_n


total_time_1=data$total_time[1:167]
total_time_0=data$total_time[168:342]
total_assm_1=data$total_assm[1:167]
total_assm_0=data$total_assm[168:342]
avg_time_1=data$avg_time[1:167]
avg_time_0=data$avg_time[168:342]
avg_assm_1=avg_assm[1:167]
avg_assm_0=avg_assm[168:342]

#p-value<0.05 i.e. not from normal
shapiro.test(data$total_time[168:342])
shapiro.test(data$total_assm)
shapiro.test(data$avg_time)
shapiro.test(avg_assm)

#Non parametric test
wilcox.test(total_time_1,total_time_0,correct=FALSE) #there is different
wilcox.test(total_assm_1,total_assm_0,correct=FALSE) #no different
wilcox.test(avg_time_1,avg_time_0,correct=FALSE) #there is different
wilcox.test(avg_assm_1,avg_assm_0,correct=FALSE) #there is different

#t-test
t.test(total_time_1,total_time_0,alternative="greater") #there is different
t.test(total_assm_1,total_assm_0) #no different
t.test(avg_time_1,avg_time_0) #there is different
t.test(avg_assm_1,avg_assm_0) #there is different

#This is the related reference for outliers.
#https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
#Actually, use the Tukey's method to identify the outliers ranged above and below the 1.5*IQR.

source("http://goo.gl/UUyEzD")
outlierKD(data, total_time)
source("http://goo.gl/UUyEzD")
outlierKD(data, total_assm)
source("http://goo.gl/UUyEzD")
outlierKD(data, avg_time)
source("http://goo.gl/UUyEzD")
outlierKD(data, avg_assm)

#After these commands, we should run and the first commands again.

#When we detect outliers and remove them, the results are the same except from the total_assm 
#for which now there is a difference

#Before running the following, clean the previous stuff.
#Now, about males and females.
#About males:
data_male=read.csv('database_male.csv', header=T)
attach(data_male)

avg_assm_m=data_male$total_assm/data_male$m30_n
total_time_0m=data_male$total_time[1:50]
total_time_1m=data_male$total_time[51:92]
total_assm_0m=data_male$total_assm[1:50]
total_assm_1m=data_male$total_assm[51:92]
avg_time_0m=data_male$avg_time[1:50]
avg_time_1m=data_male$avg_time[51:92]
avg_assm_0m=avg_assm_m[1:50]
avg_assm_1m=avg_assm_m[51:92]

par(mfrow=c(2,2))
boxplot(total_time_0m,total_time_1m)
boxplot(total_assm_0m,total_assm_1m)
boxplot(avg_time_0m,avg_time_1m)
boxplot(avg_assm_0m,avg_assm_1m)

wilcox.test(total_time_1m,total_time_0m,correct=FALSE) #no different
wilcox.test(total_assm_1m,total_assm_0m,correct=FALSE) #no different
wilcox.test(avg_time_1m,avg_time_0m,correct=FALSE) #no different
wilcox.test(avg_assm_1m,avg_assm_0m,correct=FALSE) #no different

source("http://goo.gl/UUyEzD")
outlierKD(data_male, total_time)
source("http://goo.gl/UUyEzD")
outlierKD(data_male, total_assm)
source("http://goo.gl/UUyEzD")
outlierKD(data_male, avg_time)
source("http://goo.gl/UUyEzD")
outlierKD(data_male, avg_assm_m)

#When we detect outliers and remove them, the results are the same.


#About females:
data_female=read.csv('database_female.csv', header=T)
attach(data_female)

avg_assm_f=data_female$total_assm/data_female$m30_n
total_time_0f=data_female$total_time[1:125]
total_time_1f=data_female$total_time[126:250]
total_assm_0f=data_female$total_assm[1:125]
total_assm_1f=data_female$total_assm[126:250]
avg_time_0f=data_female$avg_time[1:125]
avg_time_1f=data_female$avg_time[126:250]
avg_assm_0f=avg_assm_f[1:125]
avg_assm_1f=avg_assm_f[126:250]

par(mfrow=c(2,2))
boxplot(total_time_0f,total_time_1f)
boxplot(total_assm_0f,total_assm_1f)
boxplot(avg_time_0f,avg_time_1f)
boxplot(avg_assm_0f,avg_assm_1f)

wilcox.test(total_time_1f,total_time_0f,correct=FALSE) #no different
wilcox.test(total_assm_1f,total_assm_0f,correct=FALSE) #no different
wilcox.test(avg_time_1f,avg_time_0f,correct=FALSE) #There is different
wilcox.test(avg_assm_1f,avg_assm_0f,correct=FALSE) #no different

source("http://goo.gl/UUyEzD")
outlierKD(data_female, total_time)
source("http://goo.gl/UUyEzD")
outlierKD(data_female, total_assm)
source("http://goo.gl/UUyEzD")
outlierKD(data_female, avg_time)
source("http://goo.gl/UUyEzD")
outlierKD(data_female, avg_assm_f)

#When we detect outliers and remove them, the results are the same.



