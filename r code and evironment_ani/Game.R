d=data.frame(d)


#### Subsetting data

d1=data.frame(d[which(d$game==1),])
d0=data.frame(d[which(d$game==0),])


######## t test

#total ass
var.test(d1$total_assm,d0$total_assm)
shapiro.test(d1$total_assm)
shapiro.test(d0$total_assm)
boxplot(d1$total_assm,d0$total_assm)
c=d1$total_assm[d1$total_assm < 170]
d=d0$total_assm[d0$total_assm < 130]
boxplot(c,d)
t.test(d1$total_assm,d0$total_assm,var.equal = TRUE,alternative = "greater") #no difference
t.test(c,d,var.equal = TRUE,alternative = "greater") ### post outlier trt high

#total time
var.test(d1$total_time,d0$total_time)
shapiro.test(d1$total_time)
shapiro.test(d0$total_time)
boxplot(d1$total_time,d0$total_time)

t.test(d1$total_time,d0$total_time,var.equal =FALSE,alternative="greater")#trt grp more time
c=d1$total_time[d1$total_time < 9500]
d=d0$total_time[d0$total_time < 7000]
boxplot(c,d)
t.test(c,d,var.equal = TRUE,alternative = "greater") ### SAME



#avg ass using stay_30m
var.test(d1$stay_30m,d0$stay_30m)
shapiro.test(d1$stay_30m)
shapiro.test(d0$stay_30m)
boxplot(d1$stay_30m,d0$stay_30m)


t.test(d1$stay_30m,d0$stay_30m,var.equal = FALSE,alternative = "greater")#trt grp more time
c=d1$stay_30m[d1$stay_30m < 19]
d=d0$stay_30m[d0$stay_30m < 19]
boxplot(c,d)
var.test(c,d)
t.test(c,d,var.equal = FALSE,alternative = "greater") ###SAME

#avg ass using avgass
var.test(d1$m_avgass,d0$m_avgass)
shapiro.test(d1$m_avgass)
shapiro.test(d0$m_avgass)
boxplot(d1$m_avgass,d0$m_avgass)



t.test(d1$m_avgass,d0$m_avgass,var.equal = FALSE,alternative = "greater")#trt grp more time

c=d1$m_avgass[d1$m_avgass < 19]
d=d0$m_avgass[d0$m_avgass < 19]
boxplot(c,d)
var.test(c,d)
t.test(c,d,var.equal = TRUE,alternative = "greater") ###No diff



#avg time using m_avgtime
var.test(d1$m_avgtime,d0$m_avgtime)
shapiro.test(d1$m_avgtime)
shapiro.test(d0$m_avgtime)
boxplot(d1$m_avgtime,d0$m_avgtime)

t.test(d1$m_avgtime,d0$m_avgtime,var.equal = FALSE,alternative="greater")#trt grp more time
c=d1$m_avgtime[d1$m_avgtime < 1150]
d=d0$m_avgtime[d0$m_avgtime < 1150]
boxplot(c,d)
var.test(c,d)
t.test(c,d,var.equal = TRUE,alternative = "greater") ###SAME


#total score
var.test(d1$course_total,d0$course_total)
shapiro.test(d1$course_total)
shapiro.test(d0$course_total)
boxplot(d1$course_total,d0$course_total)

t.test(d1$course_total,d0$course_total,var.equal = FALSE,alternative = "greater")#no diff

c=d1$course_total[d1$course_total <100]
d=d0$course_total[d0$course_total>55]
boxplot(c,d)
t.test(c,d,var.equal = TRUE,alternative = "greater") ###SAME


#max level

var.test(d1$maxLevel,d0$maxLevel)
shapiro.test(d1$maxLevel)
shapiro.test(d0$maxLevel)
boxplot(d1$course_total,d0$course_total)
t.test(d1$maxLevel,d0$maxLevel,var.equal = TRUE,alternative = "greater")#trt high




#final level

var.test(d1$finalLevel,d0$finalLevel)
shapiro.test(d1$finalLevel)
shapiro.test(d0$finalLevel)
boxplot(d1$finalLevel,d0$finalLevel)
t.test(d1$finalLevel,d0$finalLevel,var.equal = TRUE,alternative = "greater")#trt high

#no t test has normality



###visits
var.test(d1$m30_n,d0$m30_n)
shapiro.test(d1$m30_n)
shapiro.test(d0$m30_n)
boxplot(d1$m30_n,d0$m30_n)
t.test(d1$m30_n,d0$m30_n,var.equal = TRUE,alternative = "greater")#no diff




####ANOVA

fit1=lm(course_total~ as.factor(teacher)+as.factor(female),d) ###both
summary(fit1) 
anova(fit1)



fit2=lm(total_assm~ as.factor(teacher)+as.factor(female),d) ###both 
summary(fit2)
anova(fit2)


fit3=lm(total_time~ as.factor(teacher)+as.factor(female),d) ###both
summary(fit3)
anova(fit3)

fit4=lm(m_avgtime~ as.factor(teacher)+as.factor(female),d) #only teacher
summary(fit4)
anova(fit4)


fit5=lm(m_avgass~ as.factor(teacher)+as.factor(female),d) ## only teacher
summary(fit5)
anova(fit5)

### if you check female separately then its alays significant



############ model for final score
fit6=lm(course_total~as.factor(female)+as.factor(teacher)+
          maxLevel+(m_avgass)+as.factor(game)*as.factor(female)*maxLevel,d)
summary(fit6)
anova(fit6)
res=fit6$resid
hist(res)
yhat=fit6$fitted.values
plot(yhat,y=res)
abline(h=0,col="red")
qqnorm(res)
qqline(res)
cv=sum(((predict(fit6)-d$course_total)^2)/length(d$course_total))



########### survey
s=s
s1=data.frame(s[which(d$game==1),])
s0=data.frame(s[which(d$game==0),])

t.test(s1$mastApp,s0$mastApp)
t.test(s1$mastAvo,s0$mastAvo)
t.test(s1$perfApp,s0$perApp) #####significant
t.test(s1$perfAvo,s0$perfAvo)


boxplot(s1$sysCI_9,s0$sysCI_9)
boxplot(s1$sysCI_14,s0$sysCI_14)
boxplot(s1$sysCI_15,s0$sysCI_15)
boxplot(s1$sysCI_10,s0$sysCI_10)
boxplot(s1$sysCI_17,s0$sysCI_17)
boxplot(s1$sysCI_18,s0$sysCI_18)
boxplot(s1$sysCI_19,s0$sysCI_19)
boxplot(s1$sysCI_20,s0$sysCI_20)
boxplot(s1$sysCI_21,s0$sysCI_21)
boxplot(s1$sysCI_11_re,s0$sysCI_11_re)



t.test(s1$sysCI_9,s0$sysCI_9, alternative = "greater") ##signi Q 1
t.test(s1$sysCI_14,s0$sysCI_14)
t.test(s1$sysCI_15,s0$sysCI_15)
t.test(s1$sysCI_10,s0$sysCI_10)
t.test(s1$sysCI_17,s0$sysCI_17,alternative = "less") ###significant Q 5
t.test(s1$sysCI_18,s0$sysCI_18)
t.test(s1$sysCI_19,s0$sysCI_19)
t.test(s1$sysCI_20,s0$sysCI_20)
t.test(s1$sysCI_21,s0$sysCI_21)
t.test(s1$sysCI_11_re,s0$sysCI_11_re)


t.test(s$sysCI_9,mu=3.5,alternative = "greater")
t.test(s$sysCI_14,mu=3.5,alternative = "greater")
t.test(s$sysCI_15,mu=3.5,alternative = "greater")
t.test(s$sysCI_10,mu=3,alternative = "greater")
t.test(s$sysCI_17,mu=3,alternative = "greater")
t.test(s$sysCI_18,mu=3,alternative = "greater")
t.test(s$sysCI_19,mu=3,alternative = "greater")
t.test(s$sysCI_20,mu=3,alternative = "greater")
t.test(s$sysCI_21,mu=3.5,alternative = "greater")
t.test(s$sysCI_11_re,mu=3)

####all the above one sample t test is rejected beisdes the last one



######### logistic and lm didnot work for any survey wrt game


t.test(s$mastApp,s$mastAvo) ### no diff
t.test(s$perfApp,s$perfAvo,alternative = "greater") ### students want to perform better


t.test(s$mastApp,s$perfApp) ## no diff
t.test(s$mastApp,s$perfAvo,alternative = "greater")
t.test(s$mastAvo,s$perfApp,alternative = "greater")
t.test(s$mastAvo,s$perfAvo,alternative = "greater")


pwr.t2n.test(n1 = 150,n2=150,d=.5 , sig.level =.05 )
#power analysis just tells if you the right numbers of sample,alpha and power

