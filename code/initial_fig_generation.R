library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(viridisLite)
library(gridExtra)

install.packages("googlesheets4")
library(googlesheets4)


# read in data ------------------------------------------------------------
### read in data and data prep
#all <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/mar2022.csv")
#all$weight.g <- as.numeric(all$weight.g)

data <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/mar2022.csv")
data <- data %>% filter(days.since.3rd <= 15) #the numbers look bad after 13 days tbh bc of all the drops in weight
wands.data <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/wands.csv")


# make numeric
data$weight.g <- as.numeric(data$weight.g) # for some reason this column isnt numberic

wands.data$days.died <- as.numeric(wands.data$days.died)
wands.data$days.em <- as.numeric(wands.data$days.em)
wands.data$days.pup <- as.numeric(wands.data$days.pup)
wands.data$days.wand <- as.numeric(wands.data$days.wand)

# filtering and cleaning ------------------------------------------------------------
# FILTERING
# removing nat paras that wander, slow growers, died as pre-wandering larva (injected) or before em (nat para)
#data.rm1 <- data %>% filter(unique != "R1-para.d0-05", 
#                            unique != "R1-para.d1-04",
#                            unique != "R1-25-09",
#                            unique != "R1-10-07", unique != "R1-10-10",
#                            unique != "R1-1-06")

## USE THIS ONE
# removing the bad injections and same as above
data.rm2 <- data %>% filter(unique != "R1-para.d0-03", unique != "R1-para.d0-05", 
                            unique != "R1-para.d1-04",
                            unique != "R1-25-01", unique != "R1-25-02", unique != "R1-25-04", unique != "R1-25-09", unique != "R1-25-10",
                            unique != "R1-10-01", unique != "R1-10-02", unique != "R1-10-04", unique != "R1-10-07", unique != "R1-10-08", unique != "R1-10-09"  #keeping 10-10 in
) %>%
  filter((round == "R1" & treatment != "1") | round == "R2")

# for test data. omits 0.25-0.01 treatments
data.test <- data %>% filter(treatment == "0.5" | treatment == "1.0" | treatment == "2.0" | treatment == "para" | treatment == "PBS") %>%
  filter(unique != "R1-para.d0-03", unique != "R1-para.d0-05", 
         unique != "R1-para.d1-04",
         unique != "test-50.d1-02")


wands.rm <- wands.data %>% filter(unique != "R1-para.d0-03", unique != "R1-para.d0-05", 
                                  unique != "R1-para.d1-04",
                                  unique != "R1-25-01", unique != "R1-25-02", unique != "R1-25-04", unique != "R1-25-09", unique != "R1-25-10",
                                  unique != "R1-10-01", unique != "R1-10-02", unique != "R1-10-04", unique != "R1-10-07", unique != "R1-10-08", unique != "R1-10-09"  #keeping 10-10 in
) %>%
  filter((round == "R1" & treatment != "1") | round == "R2")

data.rm2.para <- data.rm2 %>% filter(treatment == "para")



# data subsets
data.para <- data.rm2 %>% filter(unique != "R1-para.d0-03", unique != "R1-para.d0-05", 
                                 unique != "R1-para.d1-04", 
                                 treatment == "para")

data.nopara <- data.rm2 %>% filter(treatment != "para")

data.R1 <- data.rm2 %>% filter(round == "R1")

data.R2 <- data.rm2 %>% filter(round == "R2")



wands.nopara <- wands.rm %>% filter(treatment != "para")
wands.para <- wands.rm %>% filter(treatment == "para")


# statistics ------------------------------------------------------------
### calculate statistics

# all
#summary <- data %>% group_by(treatment, days.since.3rd) %>% 
#  summarise(avg = mean(weight.g, na.rm=TRUE),
#            se = sd(weight.g, na.rm=TRUE)/length(weight.g),
#            n = n())

#summary.rm1 <- data.rm1 %>% group_by(treatment, days.since.3rd) %>% 
#  summarise(avg = mean(weight.g, na.rm=TRUE),
#            se = sd(weight.g, na.rm=TRUE)/length(weight.g))

##USE
summary.rm2 <- data.rm2 %>% group_by(treatment, days.since.3rd) %>% 
  summarise(avg = mean(weight.g, na.rm=TRUE),
            se = sd(weight.g, na.rm=TRUE)/length(weight.g),
            n = n())

summary.wands <- wands.nopara %>% group_by(treatment) %>%
  summarise(avg.wand.wt = mean(weight.wand, na.rm=TRUE),
            se.wand.wt = sd(weight.wand, na.rm=TRUE)/length(weight.wand),
            time.to.wand = days.wand-date.inj,
            avg.wand.time = mean(time.to.wand, na.rm=TRUE),
            se.wand.time = sd(time.to.wand, na.rm=TRUE)/length(time.to.wand),
            time.to.pup = days.pup-days.wand,
            avg.pup.time = mean(time.to.pup, na.rm=TRUE),
            se.pup.time = sd(time.to.pup, na.rm=TRUE)/length(time.to.pup),
            n = n())

summary.ems <- wands.para %>% group_by(treatment.2) %>%
  summarise(time.to.em = days.em-date.inj,
            avg.em.time = mean(time.to.em, na.rm = TRUE),
            se.em.time = sd(time.to.em, na.rm=TRUE)/length(time.to.em),
            n = n())


# treatment subsets
summary.bypara <- data.para %>% group_by(treatment.2, days.since.3rd) %>% 
  summarise(avg = mean(weight.g, na.rm=TRUE),
            se = sd(weight.g, na.rm=TRUE)/length(weight.g),
            n = n())

summary.nopara <- data.nopara %>% group_by(treatment, days.since.3rd) %>% 
  summarise(avg = mean(weight.g, na.rm=TRUE),
            se = sd(weight.g, na.rm=TRUE)/length(weight.g))

summary.rm2.para <- data.rm2.para %>% group_by(treatment.2, days.since.3rd) %>% 
  summarise(avg = mean(weight.g, na.rm=TRUE),
            se = sd(weight.g, na.rm=TRUE)/length(weight.g))

# round subsets
summary.R1 <- data.R1 %>% group_by(treatment, days.since.3rd) %>% 
  summarise(avg = mean(weight.g, na.rm=TRUE),
            se = sd(weight.g, na.rm=TRUE)/length(weight.g),
            round = round)

summary.R2 <- data.R2 %>% group_by(treatment, days.since.3rd) %>% 
  summarise(avg = mean(weight.g, na.rm=TRUE),
            se = sd(weight.g, na.rm=TRUE)/length(weight.g),
            round = round)

# to look at "test"
summary.rounds <- data.test %>% group_by(round, treatment, days.since.3rd) %>%
  summarise(avg = mean(weight.g, na.rm=TRUE),
            se = sd(weight.g, na.rm=TRUE)/length(weight.g),
            round = round)

#summary2 <- data %>% group_by(days.since.3rd, treatment) %>% 
#  summarise(avg = mean(weight.g, na.rm=TRUE),
#              se = sd(weight.g, na.rm=TRUE)/length(weight.g))




# plots ------------------------------------------------------------
### plots
# uncommented out plots are okay to use

# all rounds and treatments
## might be okay
#ggplot(data = data, aes(x = days.since.3rd, color = treatment)) +
#  #geom_point(aes(y=weight.g), alpha = 0.3) +
#  geom_line(data = summary, aes(y = avg)) +
#  geom_errorbar(data = summary, aes(ymin=avg-se, ymax=avg+se), width=0.2) +
#  geom_vline(xintercept = 3.5, lty = "dashed", alpha = 0.3) +
#  geom_vline(xintercept = 7, lty = "dashed", alpha = 0.3) +
#  ylim(0, 13) +
#  scale_color_brewer(palette = "Set1")

# this isnt that useful lol basically looks like plot 1
#ggplot(data = data.rm1, aes(x = days.since.3rd, color = treatment)) +
#  #geom_point(aes(y=weight.g), alpha = 0.3) +
#  geom_line(data = summary.rm1, aes(y = avg)) +
#  geom_errorbar(data = summary.rm1, aes(ymin=avg-se, ymax=avg+se), width=0.2) +
#  ylim(0, 16) +
#  scale_color_brewer(palette = "Set1")

## use me!!!!!!
ggplot(data = data.rm2) +
  #geom_point(aes(y=weight.g), alpha = 0.3) +
  geom_line(data = summary.rm2, aes(x = days.since.3rd, color = treatment, y = avg)) +
  geom_errorbar(data = summary.rm2, aes(ymin=avg-se, ymax=avg+se, x = days.since.3rd, color = treatment), width=0.2) +
  #geom_line(data = summary.rm2.para, aes(y = avg, x = days.since.3rd, color = treatment.2), lty = "dotted") +
  #geom_errorbar(data = summary.rm2.para, aes(ymin=avg-se, ymax=avg+se, x = days.since.3rd, color = treatment.2), width=0.2) +
  geom_vline(xintercept = 3.5, lty = "dashed", alpha = 0.3) +
  geom_vline(xintercept = 7, lty = "dashed", alpha = 0.3) +
  ylim(0, 13) +
  scale_color_brewer(palette = "Set1")


# just paras
ggplot(data = data.para, aes(x = days.since.3rd, color = treatment.2)) +
  geom_point(aes(y=weight.g), alpha = 0.3) +
  geom_line(data = summary.bypara, aes(y = avg)) +
  geom_errorbar(data = summary.bypara, aes(ymin=avg-se, ymax=avg+se), width=0.2) +
  ylim(0, 16) +
  scale_color_brewer(palette = "Set1")

# by round
ggplot(data = data.R2, aes(x = days.since.3rd, color = treatment)) +
  geom_point(aes(y=weight.g), alpha = 0.3) +
  geom_line(data = summary.R2, aes(y = avg)) +
  #geom_errorbar(data = summary, aes(ymin=avg-se, ymax=avg+se), width=0.2) +
  ylim(0, 16) +
  scale_color_brewer(palette = "Set1")

# for looking at "test"
ggplot(data = data.rm2, aes(x = days.since.3rd, color = treatment, lty = round)) +
  #geom_point(aes(y=weight.g), alpha = 0.3) +
  geom_line(data = summary.rounds, aes(y = avg)) +
  #geom_errorbar(data = summary, aes(ymin=avg-se, ymax=avg+se), width=0.2) +
  ylim(0, 16) +
  scale_color_brewer(palette = "Set2")

# separated out the paras
### sort of useable... not rlly
ggplot() +
  #geom_point(data = data.nopara, aes(y=weight.g, x = days.since.3rd, color = treatment), alpha = 0.3) +
  #geom_point(data = data.para, aes(y = weight.g, x = days.since.3rd, color = treatment.2), alpha = 0.3) +
  #geom_point(data = all, aes(y = weight.g, x = days.since.3rd, color = treatment), alpha = 0.3) +
  geom_line(data = summary, aes(y = avg, x = days.since.3rd, color = treatment)) +
  geom_errorbar(data = summary, aes(ymin=avg-se, ymax=avg+se, x = days.since.3rd, color = treatment), width=0.2) +
  geom_line(data = summary.bypara, aes(y = avg, x = days.since.3rd, color = treatment.2), lty = "dotted") +
  geom_errorbar(data = summary.bypara, aes(ymin=avg-se, ymax=avg+se, x = days.since.3rd, color = treatment.2), width=0.2) +
  ylim(0, 16) +
  scale_color_brewer(palette = "Set1")






## sanity checks!
# R1 vs R2
ggplot() +
  geom_line(data = summary.R1, aes(y=avg, x = days.since.3rd, color = treatment, lty=round), alpha = 0.5, size = 1) +
  geom_line(data = summary.R2, aes(y=avg, x=days.since.3rd, color = treatment, lty=round)) +
  geom_vline(xintercept = 3.5, lty = "dashed", alpha = 0.3) +
  geom_vline(xintercept = 7, lty = "dashed", alpha = 0.3)


#weights at wandering
ggplot(data = summary.wands, aes(x = treatment, y = avg.wand.wt)) +
  geom_point() +
  geom_errorbar(aes(ymin=avg.wand.wt-se.wand.wt, ymax=avg.wand.wt+se.wand.wt))

# time to wander
ggplot(data = summary.wands, aes(x = treatment, y = avg.wand.time)) +
  geom_point() +
  geom_errorbar(aes(ymin=avg.wand.time-se.wand.time, ymax=avg.wand.time+se.wand.time))

# time to pup
ggplot(data = summary.wands, aes(x = treatment, y = avg.pup.time)) +
  geom_point() +
  geom_errorbar(aes(ymin=avg.pup.time-se.pup.time, ymax=avg.pup.time+se.pup.time))

# time to em
ggplot(data = summary.ems, aes(x = treatment.2, y = avg.em.time)) +
  geom_point() +
  geom_errorbar(aes(ymin=avg.em.time-se.em.time, ymax=avg.em.time+se.em.time))


### apr data

#apr <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/apr-test.csv") #need to clean
apr <- read.csv("~/Desktop/apr2.csv")

apr <- apr %>% filter(unique != "40-0.5-3") %>% select(-unique)
apr$X17.Apr <- as.numeric(apr$'X17.Apr') # force NAs
apr$X1.May <- as.numeric(apr$'X1.May') # force NAs

long.apr <- apr %>% pivot_longer(cols = 6:20, names_to = c('date'), names_prefix = "X", values_to = 'weight.g') 
# "unique" breaks the pivot :\
write_csv(long.apr, file = '~/Desktop/longapr.csv')

#long.apr <- long.apr %>% mutate(days.since.3rd = case_when(date = '17.Apr' ~ '3'))

#long$date <- as.character(long$date)
# fix this later -.-

apr_summary <- long %>%
  group_by(temp, dose, date) %>%
  summarise(avg.weight = mean(weight.g),
            se = sd(weight.g, na.rm=TRUE)/length(weight.g),
            count = n())

ggplot(data = apr_summary, aes(x = date, y = avg.weight, color = dose)) +
  #geom_point(aes(x = date, y = avg.weight)) +
  geom_line(aes(group=dose)) +
  #geom_point(aes(shape=as.factor(temp)))
  #geom_errorbar(aes(ymin = se, ymax = se)) + # hmmmmm not right
  facet_wrap(~temp)

ggplot(data = long, aes(x = date, y = weight.g, color = dose)) + #breaking
  #geom_point(aes(y=weight.g), alpha = 0.3) +
  geom_point(aes(as.factor(temp))) 
#geom_errorbar(aes(ymin = se, ymax = se)) +
facet_wrap(~temp) +
  scale_color_brewer(palette = "Set1")



# (old) combined wands -----------------------------------------------------------------

all_wands <- read.csv("~/Desktop/all_wands.csv")

summary.all_wands <- all_wands %>% group_by(temp, treatment) %>%
  summarise(avg.wand.weight = mean(wand.weight, na.rm=TRUE),
            n = n(),
            wand.se = sd(wand.weight, na.rm=TRUE)/length(wand.weight))

#summary.all_wands.pupal <- all_wands %>% group_by(temp, treatment, sex) %>%
#  summarise(avg.pupal.weight = mean(pupal.weight, na.rm=TRUE),
#            n = n(),
#            pup.se = sd(pupal.weight, na.rm=TRUE)/length(pupal.weight))

ggplot(data = summary.all_wands, aes(x=treatment, y = avg.wand.weight, color = as.factor(temp))) +
  geom_point() +
  geom_errorbar(aes(ymin = avg.wand.weight-wand.se, ymax = avg.wand.weight+wand.se))

# (use!) development data -------------------------------------------------------------
all_orig <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/all_devdata.csv")
all <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/all_devdata_rm.csv")
# took out the bad ones. too lazy to repeat all the typing

# fix column types
all[, c(14,17,20,23,26,29,36:40)] <- lapply(all[, c(14,17,20,23,26,29,36:40)], 
                                            FUN = function(y){as.numeric(y)})

# take out some treatments that are messy (we=2.00, non) 
#all <- all %>%
#  filter(para.type != "2.00" & para.type != "non")
all <- all %>%
  filter(para.type != "2" & para.type != "non")



### SUMMARY STATS

stats <- all %>% group_by(heatshock, para.type) %>%
  summarise(n = n(),
            avg.3rd = mean(mass.3rd, na.rm=T),
            avg.4th = mean(mass.4th, na.rm=T),
            avg.5th = mean(mass.5th, na.rm=T),
            avg.wand = mean(mass.wand,na.rm=T),
            avg.duration3rd = mean(duration.3rd, na.rm=T),
            avg.duration4th = mean(duration.4th,na.rm=T),
            avg.timetowand = mean(timeto.wand, na.rm=T),
            avg.timetopup = mean(timeto.pup, na.rm=T),
            se.3rd = sd(mass.3rd, na.rm=TRUE)/length(mass.3rd),
            se.4th = sd(mass.4th, na.rm=TRUE)/length(mass.4th),
            se.5th = sd(mass.5th, na.rm=TRUE)/length(mass.5th),
            se.wand = sd(mass.wand, na.rm=TRUE)/length(mass.wand),
            se.duration3rd = sd(duration.3rd,na.rm=TRUE)/length(duration.3rd),
            se.duration4th = sd(duration.4th,na.rm=TRUE)/length(duration.4th),
            se.duration5th = sd(duration.5th,na.rm=TRUE)/length(duration.5th),
            se.timetowand = sd(timeto.wand,na.rm=TRUE)/length(timeto.wand),
            se.timetopup = sd(timeto.pup,na.rm=TRUE)/length(timeto.pup))

# to handle treated/untreated 4ths
stats.untr4th <- all %>% filter(round == "R3" | round == "R4") %>%
  group_by(heatshock, para.type) %>%
  summarise(n = n(),
            avg.4th = mean(mass.4th, na.rm=T),
            avg.duration4th = mean(duration.4th,na.rm=T),
            se.4th = sd(mass.4th, na.rm=TRUE)/length(mass.4th),
            se.duration4th = sd(duration.4th,na.rm=TRUE)/length(duration.4th))

stats.tr4th <- all %>% filter(round == "R1" | round == "R1") %>%
  group_by(heatshock, para.type) %>%
  summarise(n = n(),
            avg.4th = mean(mass.4th, na.rm=T),
            avg.duration4th = mean(duration.4th,na.rm=T),
            se.4th = sd(mass.4th, na.rm=TRUE)/length(mass.4th),
            se.duration4th = sd(duration.4th,na.rm=TRUE)/length(duration.4th))


# pupae stats
stats_bysex <- all %>% group_by(para.type, heatshock, sex) %>%
  filter(sex!="na") %>%
  summarise(n = n(),
            avg.pup = mean(mass.pupa, na.rm=T),
            se.pup = sd(mass.pupa, na.rm=T)/length(mass.pupa))



### PLOTTING

## masses
plot.avg3rd <- ggplot(data=stats, aes(x = para.type, color = as.factor(heatshock))) +
  geom_point(aes(y=avg.3rd)) +
  geom_errorbar(aes(ymin=avg.3rd-se.3rd, ymax=avg.3rd+se.3rd))

plot.avg4th <- ggplot(data=stats, aes(x = para.type, color = as.factor(heatshock))) +
  geom_point(aes(y=avg.4th)) +
  geom_errorbar(aes(ymin=avg.4th-se.4th, ymax=avg.4th+se.4th))

plot.avg5th <- ggplot(data=stats, aes(x = para.type, color = as.factor(heatshock))) +
  geom_point(aes(y=avg.5th)) +
  geom_errorbar(aes(ymin=avg.5th-se.5th, ymax=avg.5th+se.5th))

#grid.arrange(plot.avg3rd, plot.avg4th, plot.avg5th, ncol=3)

plot.avgwand <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.wand)) +
  geom_errorbar(aes(ymin=avg.wand-se.wand, ymax=avg.wand+se.wand))


plot.avgpup <- ggplot(data=stats_bysex, aes(x=para.type, shape=as.factor(heatshock), color = sex)) +
  geom_point(aes(y=avg.pup)) +
  geom_errorbar(aes(ymin=avg.pup-se.pup, ymax=avg.pup+se.pup)) +
  scale_shape_manual(values = c(1:2))


## duration
plot.dur3rd <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.duration3rd)) +
  geom_errorbar(aes(ymin=avg.duration3rd-se.duration3rd, ymax=avg.duration3rd+se.duration3rd))

plot.dur4th <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.duration4th)) +
  geom_errorbar(aes(ymin=avg.duration4th-se.duration4th, ymax=avg.duration4th+se.duration4th))

plot.timetowand <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.timetowand)) +
  geom_errorbar(aes(ymin=avg.timetowand-se.timetowand, ymax=avg.timetowand+se.timetowand))

plot.timetopup <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.timetopup)) +
  geom_errorbar(aes(ymin=avg.timetopup-se.timetopup, ymax=avg.timetopup+se.timetopup))


## 4ths
plot.avg4th.untr4th <- ggplot(data=stats.untr4th, aes(x = para.type, color = as.factor(heatshock))) +
  geom_point(aes(y=avg.4th)) +
  geom_errorbar(aes(ymin=avg.4th-se.4th, ymax=avg.4th+se.4th)) +
  ylab("avg4th.untreated4th")

plot.avg4th.tr4th <- ggplot(data=stats.tr4th, aes(x = para.type, color = as.factor(heatshock))) +
  geom_point(aes(y=avg.4th)) +
  geom_errorbar(aes(ymin=avg.4th-se.4th, ymax=avg.4th+se.4th)) +
  ylab("avg4th.treated4th")

plot.dur4th.untr4th <- ggplot(data=stats.untr4th, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.duration4th)) +
  geom_errorbar(aes(ymin=avg.duration4th-se.duration4th, ymax=avg.duration4th+se.duration4th)) +
  ylab("dur4th.untreated4th")

plot.dur4th.tr4th <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.duration4th)) +
  geom_errorbar(aes(ymin=avg.duration4th-se.duration4th, ymax=avg.duration4th+se.duration4th)) +
  ylab("dur4th.treated4th")


### plots
# mass
plot.avg3rd 
plot.avg4th # this has treated and untreated 4ths...
plot.avg5th # prob the better one to use
plot.avgwand
plot.avgpup
# paras: there is a bit of distinction bt d0 and d1 injections i think but didnt look into it

# development time
plot.dur3rd
plot.dur4th
plot.timetowand
plot.timetopup

# 4ths
plot.avg4th.untr4th
plot.avg4th.tr4th
plot.dur4th.untr4th
plot.dur4th.tr4th

# looking at sample sizes
view(all %>% group_by(round, heatshock, para.type) %>% count())
view(all %>% group_by(round, para.type) %>% count())
view(all %>% group_by(round) %>% count())
view(all %>% group_by(heatshock, para.type) %>% count()) ### use this


# 220704 dev data revisited ----------------------------------------------------

# basically i should toss out R1 and R2 bc thats when i was freezing the virus like a knucklehead 
# so virus was prob dead!!!! lawl

# READ DATA
all_orig <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/all_devdata.csv")
all <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/all_devdata_rm.csv")
# took out the bad ones. too lazy to repeat all the typing (based on "filtering and cleaning" section above)

# FIX COLUMN TYPES
all[, c(14,17,20,23,26,29,36:40)] <- lapply(all[, c(14,17,20,23,26,29,36:40)], 
                                            FUN = function(y){as.numeric(y)})

# FILTER R1 & R2

all <- all %>% filter((round == "R1" & (para.type == "PBS" | para.type == "para")) |
                        (round == "R2" & (para.type == "PBS" | para.type == "para")) |
                        round == "R4") %>%
  filter(unique != "R1-25-para_d0-3", # remove wandering paras
         unique != "R1-25-para_d0-5")


# SUMMARY STATS (isolate by stage)

stats <- all %>% group_by(heatshock, para.type) %>%
  summarise(n = n(),
            avg.3rd = mean(mass.3rd, na.rm=T),
            avg.4th = mean(mass.4th, na.rm=T),
            avg.5th = mean(mass.5th, na.rm=T),
            avg.wand = mean(mass.wand,na.rm=T),
            avg.duration3rd = mean(duration.3rd, na.rm=T),
            avg.duration4th = mean(duration.4th,na.rm=T),
            avg.timetowand = mean(timeto.wand, na.rm=T),
            avg.timetopup = mean(timeto.pup, na.rm=T),
            se.3rd = sd(mass.3rd, na.rm=TRUE)/length(mass.3rd),
            se.4th = sd(mass.4th, na.rm=TRUE)/length(mass.4th),
            se.5th = sd(mass.5th, na.rm=TRUE)/length(mass.5th),
            se.wand = sd(mass.wand, na.rm=TRUE)/length(mass.wand),
            se.duration3rd = sd(duration.3rd,na.rm=TRUE)/length(duration.3rd),
            se.duration4th = sd(duration.4th,na.rm=TRUE)/length(duration.4th),
            se.timetowand = sd(timeto.wand,na.rm=TRUE)/length(timeto.wand),
            se.timetopup = sd(timeto.pup,na.rm=TRUE)/length(timeto.pup))
# dont need to separate out treated/untreated 4ths here.
# only "treated" 4ths" (i.e. treated at 4th are the paras)

stats_bysex <- all %>% group_by(para.type, heatshock, sex) %>%
  filter(sex!="na") %>%
  summarise(n = n(),
            avg.pup = mean(mass.pupa, na.rm=T),
            se.pup = sd(mass.pupa, na.rm=T)/length(mass.pupa))

# PLOTTING (isolate by stage)
## PLOT AVG MASS

plot.avg4th <- ggplot(data=stats, aes(x = para.type, color = as.factor(heatshock))) +
  geom_point(aes(y=avg.4th)) +
  geom_errorbar(aes(ymin=avg.4th-se.4th, ymax=avg.4th+se.4th))

plot.avg5th <- ggplot(data=stats, aes(x = para.type, color = as.factor(heatshock))) +
  geom_point(aes(y=avg.5th)) +
  geom_errorbar(aes(ymin=avg.5th-se.5th, ymax=avg.5th+se.5th))

plot.avgwand <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.wand)) +
  geom_errorbar(aes(ymin=avg.wand-se.wand, ymax=avg.wand+se.wand))

plot.avgpup <- ggplot(data=stats_bysex, aes(x=para.type, color=as.factor(heatshock), shape=sex)) +
  geom_point(aes(y=avg.pup), size=2) +
  geom_errorbar(aes(ymin=avg.pup-se.pup, ymax=avg.pup+se.pup))


plot.avg4th #these are mostly just starting weights for expt bc not much is treated
plot.avg5th
plot.avgwand
plot.avgpup

## PLOT AVG DURATION (isolate by stage)

plot.dur3rd <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.duration3rd)) +
  geom_errorbar(aes(ymin=avg.duration3rd-se.duration3rd, ymax=avg.duration3rd+se.duration3rd))

plot.dur4th <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.duration4th)) +
  geom_errorbar(aes(ymin=avg.duration4th-se.duration4th, ymax=avg.duration4th+se.duration4th))

plot.timetowand <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.timetowand)) +
  geom_errorbar(aes(ymin=avg.timetowand-se.timetowand, ymax=avg.timetowand+se.timetowand))

plot.timetopup <- ggplot(data=stats, aes(x=para.type, color=as.factor(heatshock))) +
  geom_point(aes(y=avg.timetopup)) +
  geom_errorbar(aes(ymin=avg.timetopup-se.timetopup, ymax=avg.timetopup+se.timetopup))

plot.dur3rd
plot.dur4th
plot.timetowand
plot.timetopup


# STATS (isolate by time)
## there is prob a more efficient way to do this but idk it

stats_bytime <- function(data, wasp_eq){ # i should rename this function tbh
  mean.growth <- data %>% filter(para.type == wasp_eq) %>%
    group_by(heatshock) %>%
    summarise("3" = mean(mass.3rd, na.rm=T),
              "4" = mean(mass.4th, na.rm=T),
              "5" = mean(mass.5th, na.rm=T),
              wand = mean(mass.wand, na.rm=T),
              pup = mean(mass.pupa, na.rm=T)) %>%
    mutate(#measurement = "mass",
      para.type = wasp_eq)
  
  se.growth <- data %>% filter(para.type == wasp_eq) %>%
    group_by(heatshock) %>%
    summarise("3" = sd(mass.3rd, na.rm=TRUE)/length(mass.3rd),
              "4" = sd(mass.4th, na.rm=TRUE)/length(mass.4th),
              "5" = sd(mass.5th, na.rm=TRUE)/length(mass.5th),
              wand = sd(mass.wand, na.rm=TRUE)/length(mass.wand)) %>%
    mutate(#measurement = "mass",
      para.type = wasp_eq)
  
  mean.dur <- data %>% filter(para.type == wasp_eq) %>%
    group_by(heatshock) %>%
    summarise("3" = mean(duration.3rd, na.rm=T),
              "4" = mean(duration.4th, na.rm=T),
              wand = mean(timeto.wand, na.rm=T),
              pup = mean(timeto.pup, na.rm=T)) %>%
    mutate(#measurement = "avg.duration",
      para.type = wasp_eq)
  
  se.dur <- data %>% filter(para.type == wasp_eq) %>%
    group_by(heatshock) %>%
    summarise("3" = sd(duration.3rd,na.rm=TRUE)/length(duration.3rd),
              "4" = sd(duration.4th,na.rm=TRUE)/length(duration.4th),
              wand = sd(timeto.wand,na.rm=TRUE)/length(timeto.wand),
              pup = sd(timeto.pup,na.rm=TRUE)/length(timeto.pup)) %>%
    mutate(#measurement = "se.duration",
      para.type = wasp_eq)
  
  long.mean.growth <- mean.growth %>% gather(key = "instar", value = "avg.mass", c(2:6))
  long.se.growth <- mean.growth %>% gather(key = "instar", value = "se.mass", c(2:6))
  stats.growth <- merge(long.mean.growth, long.se.growth)
  
  
  long.mean.dur <- mean.dur %>% gather(key = "instar", value = "avg.dur", c(2:5))
  long.se.dur <- se.dur %>% gather(key = "instar", value = "se.dur", c(2:5))
  stats.dur <- merge(long.mean.dur, long.se.dur)
  
  stats.wasp_eq <- merge(stats.growth, stats.dur, all=T) # want to add a mutate here to count n()
  
  stats.wasp_eq <- stats.wasp_eq %>% mutate_all(~ifelse(is.nan(.), NA, .)) # convert NaNs to NA
  
  return(stats.wasp_eq)
}

#### 0.5 we testing ####
mean.05growth <- all %>% filter(para.type == "0.5") %>%
  group_by(heatshock) %>%
  summarise("3" = mean(mass.3rd, na.rm=T),
            "4" = mean(mass.4th, na.rm=T),
            "5" = mean(mass.5th, na.rm=T),
            wand = mean(mass.wand, na.rm=T),
            pup = mean(mass.wand, na.rm=T)) %>%
  mutate(#measurement = "mass",
    para.type = "0.5")

se.05growth <- all %>% filter(para.type == "0.5") %>%
  group_by(heatshock) %>%
  summarise("3" = sd(mass.3rd, na.rm=TRUE)/length(mass.3rd),
            "4" = sd(mass.4th, na.rm=TRUE)/length(mass.4th),
            "5" = sd(mass.5th, na.rm=TRUE)/length(mass.5th),
            wand = sd(mass.wand, na.rm=TRUE)/length(mass.wand)) %>%
  mutate(#measurement = "mass",
    para.type = "0.5")

mean.05dur <- all %>% filter(para.type == "0.5") %>%
  group_by(heatshock) %>%
  summarise("3" = mean(duration.3rd, na.rm=T),
            "4" = mean(duration.4th, na.rm=T),
            wand = mean(timeto.wand, na.rm=T),
            pup = mean(timeto.wand, na.rm=T)) %>%
  mutate(#measurement = "avg.duration",
    para.type = "0.5")

se.05dur <- all %>% filter(para.type == "0.5") %>%
  group_by(heatshock) %>%
  summarise("3" = sd(duration.3rd,na.rm=TRUE)/length(duration.3rd),
            "4" = sd(duration.4th,na.rm=TRUE)/length(duration.4th),
            wand = sd(timeto.wand,na.rm=TRUE)/length(timeto.wand),
            pup = sd(timeto.pup,na.rm=TRUE)/length(timeto.pup)) %>%
  mutate(#measurement = "se.duration",
    para.type = "0.5")

long.mean.05growth <- mean.05growth %>% gather(key = "instar", value = "avg.mass", c(2:6))
long.se.05growth <- mean.05growth %>% gather(key = "instar", value = "se.mass", c(2:6))
stats.05growth <- merge(long.mean.05growth, long.se.05growth)


long.mean.05dur <- mean.05dur %>% gather(key = "instar", value = "avg.dur", c(2:5))
long.se.05dur <- se.05dur %>% gather(key = "instar", value = "se.dur", c(2:5))
stats.05dur <- merge(long.mean.05dur, long.se.05dur)

stats.05 <- merge(stats.05growth, stats.05dur)
#### end ####


#test05 <- stats_bytime(all, "0.5")  ##YAYYYYY
# sort of... code is breaking somewhere....

stats.05 <- stats_bytime(all, 0.5)
stats.1 <- stats_bytime(all, 1)
stats.para <- stats_bytime(all, "para")
stats.pbs <- stats_bytime(all, "PBS")


# PLOTTING

plotting <- function(data, x, y, factor){ #y_se
  ggplot(data = data, aes(x = {{x}}, color = as.factor({{factor}}))) +
    geom_point(aes(y={{y}})) #+
  #geom_errorbar(aes(ymin={{y}}-{{y_se}}, ymax={{y}}+{{y_se}}))
}

plot.05mass <- plotting(data=stats.05, x=instar, y=avg.mass, factor=heatshock)
plot.1mass <- plotting(stats.1, instar, avg.mass, heatshock)
plot.paramass <- plotting(stats.para, instar, avg.mass, heatshock)
plot.pbsmass <- plotting(stats.pbs, instar, avg.mass, heatshock)

plot.05mass
plot.1mass
plot.paramass
plot.pbsmass

plot.05dur <- plotting(data=stats.05, x=instar, y=avg.dur, factor=heatshock)
plot.1dur <- plotting(stats.1, instar, avg.dur, heatshock)
plot.paradur <- plotting(stats.para, instar, avg.dur, heatshock)
plot.pbsdur <- plotting(stats.pbs, instar, avg.dur, heatshock)

plot.05dur
plot.1dur
plot.paradur
plot.pbsdur







# daily growth (lab) ------------------------------------------------------------

# read in data
growth_trimmed <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/daily.csv", header=T)
names(growth_trimmed) <- sub("^X", "", names(growth_trimmed)) # remove leading X's


# d&b 1993 data (guessing)

dnb <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/db1993_data.csv")
names(dnb) <- sub("^X", "", names(dnb))



# filter & remove bad samples
clean <- growth_trimmed %>% select(-c(41:65, 37:39))# remove junk columns

growth <- clean %>%
  filter(!(unique == "R4-25-PBS-3")) %>% # remove super slow growers
  filter(!(unique == "R1-25-0.01-6")) %>%
  filter(!(para.type == "para" & fate == "wand")) %>% # removes unsuccesful paras
  filter(!(para.type == "para" & fate == "pup")) %>%
  filter(!(fate == "L" | fate == "na")) %>% #remove things that died as larvae (but keep dead pupa?)
  filter(!(para.type == "non")) #idk what to do w this

growth_new <- growth %>%
  filter(!(round == "R1" | round == "R2"))
# R3 and R4 only


# convert to long form
#days <- sprintf("%02d",growth_new$days.post.hatch)
# fix the missing leading 0's

long_new <- growth_new %>% 
  gather(key = "days.post.hatch", value = "mass", c(13:36))

long_all <- growth %>%
  gather(key = "days.post.hatch", value = "mass", c(13:36))

dnb_long <- dnb %>%
  gather(key="para.type", value="avg", c(3:6)) %>%
  mutate(heatshock = "25",
         src = "dnb")



# summary stats
stats.sub <- long_new %>% group_by(para.type, heatshock, days.post.hatch) %>% 
  dplyr::summarise(avg = mean(mass, na.rm=TRUE), 
                   se = sd(mass, na.rm=TRUE)/length(mass),
                   n = n())
stats.sub$days.post.hatch <- as.numeric(stats.sub$days.post.hatch) # fixes days column lol

stats.all <- long_all %>% group_by(para.type, heatshock, days.post.hatch) %>% 
  dplyr::summarise(avg = mean(mass, na.rm=TRUE), 
                   se = sd(mass, na.rm=TRUE)/length(mass),
                   n = n())
stats.all$days.post.hatch <- as.numeric(stats.all$days.post.hatch) # fixes days column lol

#ggplot(data=stats.sub, aes(y=avg, x=days.post.hatch, color=as.factor(para.type), shape=as.factor(heatshock))) + # heatshock is breaking!!!
#  geom_point() +
#  #geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
#  #xlim(0, 25) +
#  scale_color_brewer(palette = "Set1")

#facet by heatshock
ggplot(data=stats.sub, aes(y=avg, x=days.post.hatch, color=as.factor(para.type))) +
  geom_line(group=1, lwd=1) +
  facet_wrap(~as.factor(heatshock)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
  xlim(5, 25) +
  theme_bw() +
  scale_color_manual(values = c("PBS" = "#bdd7e7",
                                "0.50" = "#6baed6",
                                "1.00" = "#3182bd",
                                "2.00" = "#08519c",
                                "para" = "#fed98e"))
# should overlap older data on this

# overlapping old data...
ggplot(data = stats.sub, aes(y=avg, x=days.post.hatch, color=as.factor(para.type))) +
  geom_line(group=1, lwd=1) +
  geom_line(data=dnb_long, lty="dashed", lwd=1) +
  facet_wrap(~as.factor(heatshock)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
  xlim(5, 25) +
  theme_bw() +
  scale_color_manual(values = c("PBS" = "#c6dbef",
                                "uninj" = "#c6dbef",
                                "0.10" = "#9ecae1",
                                "0.50" = "#6baed6",
                                "1.00" = "#3182bd",
                                "2.00" = "#08519c",
                                "para" = "#fed98e"))




# ctrls only
ctrl_lab <- stats.sub %>% filter(para.type == "PBS" | para.type ==  "para") %>%
  mutate(src = "lab")
ctrl_dnb <- dnb_long %>% filter(para.type == "uninj" | para.type == "para")
ctrl_dnb$heatshock <- as.numeric(ctrl_dnb$heatshock)

ctrls <-rbind(ctrl_lab, ctrl_dnb)


ggplot(data = ctrls, aes(y=avg, x=days.post.hatch, shape=as.factor(heatshock), color=para.type)) +
  geom_line(aes(lty=src), lwd=1) +
  geom_point(group=1, size=2) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
  xlim(5, 25) +
  theme_bw() +
  scale_color_manual(values = c("PBS" = "#c6dbef",
                                "uninj" = "#c6dbef",
                                "para" = "#fed98e")) +
  scale_linetype_manual(values = c("lab" = "solid",
                                   "dnb" = "dashed")) +
  scale_shape_manual(values = c("25" = 1,
                                "40" = 2))


# facet by para
ggplot(data=stats.sub, aes(y=avg, x=days.post.hatch, color=as.factor(heatshock))) +
  geom_line(group=1) +
  facet_wrap(~as.factor(para.type)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
  xlim(5, 25) +
  theme_bw() +
  scale_color_manual(values = c("25" = "blue",
                                "40" = "red"))

ggplot(data=stats.sub, aes(y=avg, x=days.post.hatch, color=as.factor(para.type), lty=as.factor(heatshock))) +
  geom_line() +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
  xlim(5, 25) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") # this is so hard to read



#ggplot(data=stats.all, aes(y=avg, x=days.post.hatch, color=as.factor(para.type), lty=as.factor(heatshock))) + # heatshock is breaking!!!
#  geom_line() +
#  geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
#  xlim(5, 25) +
#  scale_color_brewer(palette = "Set1")

#ggplot(data=stats.all, aes(y=avg, x=days.post.hatch, color=as.factor(para.type))) +
#  geom_line() +
#  facet_wrap(~heatshock) +
#  geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
#  xlim(5, 25) +
#  scale_color_brewer(palette = "Set1")






# field data (manual) -----------------------------------------------------


field <- read.csv("~/Desktop/fielddump.csv")
field_se <- read.csv("~/Desktop/fielddump_se.csv")
names(field) <- sub("^X", "", names(field))
names(field_se) <- sub("^X", "", names(field_se))

long <- field %>% gather(key = "days.post.inject", value = "avg.mass", c(3:15))
longse <- field_se %>% gather(key = "days.post.inject", value = "se", c(3:15))
long$days.post.inject <- as.numberic(long$days.post.inject)
longse$days.post.inject <- as.numberic(longse$days.post.inject)

fieldall <- cbind(long, "se"=longse[,4])

ggplot(fieldall, aes(y=avg.mass, x=days.post.inject, color=as.factor(dose))) +
  geom_line(aes(group=dose)) +
  #geom_errorbar(aes(ymax=avg.mass+se, ymin=avg.mass-se)) +
  facet_wrap(~HS) +
  scale_color_manual(values = c("PBS" = "#c6dbef",
                                "0.5" = "#6baed6",
                                "1" = "#08519c",
                                "para" = "#fed98e"))

#scale_linetype_manual(values = c("25" = "solid",
#                                 "40" = "dashed"))



# field data 2 ------------------------------------------------------------

#install.packages("lubridate")
#library(lubridate)

field <- read.csv("~/Desktop/fielding2.csv")
names(field) <- sub("^X", "", names(field))
long <- field %>% gather(key = "date", value = "mass", c(9:51)) #update this as needed for dates

# convert dates to julian
# need to write a function for this zzz
#long$date <- paste0("22.", long$date) # add year
long$date <- as.Date(long$date, format = "%d.%b") # reformat dates
long$jdate <- format(long$date, "%j")
#long$jdate <- yday(long$date)

#long$date.inject <- paste0("22-", long$date.inject) # add year
long$date.inject <- as.Date(long$date.inject, format = "%d-%b") # reformat dates
long$jdate.inject <- format(long$date.inject, "%j")

long$jdate <- as.numeric(long$jdate)
long$jdate.inject <- as.numeric(long$jdate.inject)
long <- long %>% 
  #select(-c(10,13:14)) %>% 
  mutate(days.since.inject = jdate-jdate.inject)

## stats
#some cleaning
#long$mass <- as.numeric(long$mass) #sometimes u need this...
long$is.sup <- sub("^$", "no", long$is.sup) # fill empties w "no"

# mutate in later steps breaks when doing stats if the "?" are kept in the date.died & died.as columns
#long$date.died <- sub("\\?\\?", "idk", long$date.died) 
#long$died.as <- gsub("\\?", "q", long$died.as)
#long$extraction.date <- gsub("\\?", "q", long$extraction.date)

long <- long %>% select(-c(1, 9)) # select out thigns w ?

fieldstats <- long %>% # separates sups
  filter(species == "MS",
         days.since.inject >= 0,
         date.pmd == "") %>%
  group_by(days.since.inject, temp, dose, is.sup, species) %>%
  dplyr::summarise(avg.mass = mean(mass, na.rm=TRUE),
                   se.mass = sd(mass, na.rm=TRUE)/length(mass),
                   n = n())

fieldstats_all <- long %>% # all together
  filter(species == "MS",
         days.since.inject >= 0,
         date.pmd == "") %>%
  group_by(days.since.inject, temp, dose) %>%
  dplyr::summarise(avg.mass = mean(mass, na.rm=TRUE),
                   se.mass = sd(mass, na.rm=TRUE)/length(mass),
                   n = n())

fieldstats <- fieldstats %>% mutate_all(~ifelse(is.nan(.), NA, .)) # get rid of NaNs

MQ <- long %>%
  filter(species == "MQ",
         days.since.inject >= 0,
         date.pmd == "") %>%
  group_by(days.since.inject, temp, dose, is.sup) %>%
  dplyr::summarise(avg.mass = mean(mass, na.rm=TRUE),
                   se.mass = sd(mass, na.rm=TRUE)/length(mass),
                   n = n())

lab_MS <- long %>%
  filter(species == "lab MS",
         days.since.inject >= 0,
         date.pmd == "") %>%
  group_by(days.since.inject, temp, dose) %>%
  dplyr::summarise(avg.mass = mean(mass, na.rm=TRUE),
                   se.mass = sd(mass, na.rm=TRUE)/length(mass),
                   n = n())

dnb <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Y1/virus/db1993_data.csv")
names(dnb) <- sub("^X", "", names(dnb))
dnb_long <- dnb %>%
  gather(key="dose", value="mass", c(3:6)) %>%
  mutate(temp = "25",
         is.sup = "no",
         src = "dnb")

# (lab data)
# u have to read in the old data first tho (scroll up)
stats.sub <- long_new %>% group_by(para.type, heatshock, days.post.hatch) %>% 
  dplyr::summarise(avg = mean(mass, na.rm=TRUE), 
                   se = sd(mass, na.rm=TRUE)/length(mass),
                   n = n())
stats.sub$days.post.hatch <- as.numeric(stats.sub$days.post.hatch) # fixes days column lol

oldnames = c("para.type", "heatshock", "days.post.hatch", "avg", "se")
newnames = c("dose", "temp", "days.since.inject", "avg.mass", "se.mass")
stats.sub <- stats.sub %>% rename_at(vars(oldnames), ~ newnames)
stats.sub <- stats.sub %>%
  mutate(is.sup = "no",
         src = "lab",
         species = "MS",
         days.since.inject2 = days.since.inject-7) # need to check the math here :\

stats.sub$dose <- sub("0.50", "0.5", stats.sub$dose)
stats.sub$dose <- sub("1.00", "1", stats.sub$dose)
stats.sub <- stats.sub %>% mutate_all(~ifelse(is.nan(.), NA, .)) # get rid of NaNs

## wooo plots
# 2x2
# would be nice if some avg day of molting data could be slapped on here
ggplot(data=fieldstats, aes(x=days.since.inject, y=avg.mass, color=as.factor(dose))) +
  geom_line(aes(group=dose)) +
  geom_errorbar(aes(ymax=avg.mass+se.mass, ymin=avg.mass-se.mass)) +
  facet_grid(is.sup~temp, scales="free") +
  scale_color_manual(values = c("PBS" = "#9ecae1",
                                #"0.1" = "#6baed6",
                                "0.5" = "#2171b5",
                                "1" = "#08306b",
                                #"2.00" = "#08306b",
                                "uninj" = "#41ab5d",
                                "para" = "#de2d26")) +
  #scale_color_manual(values = c("PBS" = "#9ecae1",
  #"0.1" = "#6baed6",
  #"0.5" = "#4292c6",
  #"1" = "#084594",
  #"2.00" = "#2171b5",
  #"uninj" = "#a1d99b",
  #"para" = "#e34a33")) +
  labs(color="dose") +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = ">5 instars?", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "temperature", breaks = NULL, labels = NULL)) +
  geom_line(data=lab_MS, lwd=0.8)  +
  geom_errorbar(data=lab_MS, aes(ymax=avg.mass+se.mass, ymin=avg.mass-se.mass)) +
  #MQ stuff
  geom_line(data=MQ, lty="dashed") +
  geom_errorbar(data=MQ, aes(ymax=avg.mass+se.mass, ymin=avg.mass-se.mass)) 
#scale_linetype_manual(values=c("MS"= "solid", "MQ" = "dotdash", "lab_MS" = "dashed"))  # idky this doesnt work
# dnb data
#geom_line(data=dnb_long, aes(y=mass), lty="dotdash")
# this looks terrible bc dnb =/= field lol
# lab data
#geom_line(data=stats.sub, aes(x=days.since.inject2), lty="dashed")


# 2x1 (not as good)
ggplot(data=fieldstats_all, aes(x=days.since.inject, y=avg.mass, color=as.factor(dose))) +
  geom_line(aes(group=dose)) +
  geom_errorbar(aes(ymax=avg.mass+se.mass, ymin=avg.mass-se.mass)) +
  facet_grid(~temp) +
  scale_color_manual(values = c("PBS" = "#9ecae1",
                                "0.5" = "#4292c6",
                                "1" = "#084594",
                                "para" = "#e34a33"))
# MQ
ggplot(data=MQ, aes(x=days.since.inject, y=avg.mass, color=as.factor(dose))) +
  geom_line(aes(group=dose)) +
  geom_errorbar(aes(ymax=avg.mass+se.mass, ymin=avg.mass-se.mass)) +
  facet_wrap(~temp) +
  scale_color_manual(values = c("PBS" = "#9ecae1",
                                "0.5" = "#4292c6",
                                "1" = "#084594",
                                "para" = "#e34a33"))




# look at some dev stats
#library(googlesheets4)

#fielddev <- read_sheet("https://docs.google.com/spreadsheets/d/1CY0MjcwMakfAZfDzsRhdEtmfmv5TBragb6Ku0oFBCeY/edit#gid=1835142623", "july dev")

fielddev <- read_csv("~/Desktop/fielding_dev.csv")

# cleaning (add nas and julians)
fielddev <- fielddev %>% mutate_at((17), ~replace_na(.,"no"))

fielddev$date.inject <- as.Date(fielddev$date.inject, format = "%d-%b")
fielddev$jdate.inject <- as.numeric(fielddev$date.inject, format = "%j")
fielddev$date.4th <- as.Date(fielddev$date.4th, format = "%d-%b")
fielddev$jdate.4th <- as.numeric(format(fielddev$date.4th, "%j"))
fielddev$date.5th <- as.Date(fielddev$date.5th, format = "%d-%b")
fielddev$jdate.5th <- as.numeric(format(fielddev$date.5th, "%j"))
fielddev$date.6th <- as.Date(fielddev$date.6th, format = "%d-%b")
fielddev$jdate.6th <- as.numeric(format(fielddev$date.6th, "%j"))
fielddev$date.wander <- as.Date(fielddev$date.wander, format = "%d-%b")
fielddev$jdate.wander <- as.numeric(format(fielddev$date.wander, "%j"))

fielddev$mass.4th <- as.numeric(fielddev$mass.4th)
fielddev$mass.5th <- as.numeric(fielddev$mass.5th)
# idk how to do these all at once lol



somestats <- fielddev %>%
  filter(dose != "" & dose != "none",
         species == "MS") %>%
  group_by(temp, dose, is.sup, species) %>%
  dplyr::summarise(avg.inject.mass = mean(mass.inject),
                   se.inject.mass = sd(mass.inject, na.rm=TRUE)/length(mass.inject),
                   #avg.4th.mass = mean(mass.4th),
                   #se.4th.mass = sd(mass.4th, na.rm=TRUE)/length(mass.4th),
                   avg.5th.mass = mean(mass.5th),
                   se.5th.mass = sd(mass.5th, na.rm=TRUE)/length(mass.5th),
                   avg.6th.mass = mean(mass.6th),
                   se.6th.mass = sd(mass.6th, na.rm=TRUE)/length(mass.6th),
                   avg.wand.mass = mean(mass.wander),
                   se.wand.mass = sd(mass.wander, na.rm=TRUE)/length(mass.wander),
                   avg.wand.time = mean(jdate.inject-jdate.wander),
                   se.wand.time = sd((jdate.inject-jdate.wander), na.rm=TRUE)/length(jdate.inject-jdate.wander),
                   avg.4th.dur = mean(jdate.5th-jdate.4th),
                   se.4th.dur = sd((jdate.5th-jdate.4th), na.rm=TRUE)/length(jdate.5th-jdate.4th),
                   avg.5th_nos.dur = mean(jdate.wander-jdate.5th),
                   se.5th_nos.dur = sd((jdate.wander-jdate.5th), na.rm=TRUE)/length(jdate.wander-jdate.5th),
                   avg.5th_sup.dur = mean(jdate.6th-jdate.5th),
                   se.5th_sup.dur = sd((jdate.6th-jdate.5th), na.rm=TRUE)/length(jdate.6th-jdate.5th),
                   avg.6th.dur = mean(jdate.wander-jdate.6th),
                   se.6th.dur = sd((jdate.wander-jdate.6th), na.rm=TRUE)/length(jdate.wander-jdate.6th),
                   #avg.wand.nos.dur = mean(jdate.pupa-jdate.wander),
                   #se.wand.nos.dur = sd((jdate.6th-jdate5th), na.rm=TRUE)/length(jdate.6th-jdate5th),
                   #avg.wand.sup.dur = mean(jdate.6th-jdate5th),
                   #se.wand.sup.dur = sd((jdate.6th-jdate5th), na.rm=TRUE)/length(jdate.6th-jdate5th),
                   n = n())

somestats2 <- fielddev %>%
  filter(dose != "" & dose != "none",
         species == "MS") %>%
  group_by(temp, dose, is.sup) %>%
  dplyr::summarise(avg.inject.mass = mean(mass.inject),
                   se.inject.mass = sd(mass.inject, na.rm=TRUE)/length(mass.inject),
                   #avg.4th.mass = mean(mass.4th),
                   #se.4th.mass = sd(mass.4th, na.rm=TRUE)/length(mass.4th),
                   avg.5th.mass = mean(mass.5th),
                   se.5th.mass = sd(mass.5th, na.rm=TRUE)/length(mass.5th),
                   avg.6th.mass = mean(mass.6th),
                   se.6th.mass = sd(mass.6th, na.rm=TRUE)/length(mass.6th),
                   avg.wand.mass = mean(mass.wander),
                   se.wand.mass = sd(mass.wander, na.rm=TRUE)/length(mass.wander),
                   avg.wand.time = mean(jdate.inject-jdate.wander),
                   se.wand.time = sd((jdate.inject-jdate.wander), na.rm=TRUE)/length(jdate.inject-jdate.wander),
                   n = n())

longstats <- somestats %>% gather(key = "stat", value = "value", c(5:23))

# plots
# for is.sup: color is better than facet_grid!
# to do: should look at where the sups are

ggplot(data=somestats, aes(y=avg.inject.mass, x=as.factor(dose), color=is.sup)) +
  facet_wrap(~temp) +
  geom_point() +
  geom_errorbar(aes(ymax=avg.inject.mass+se.inject.mass, ymin=avg.inject.mass-se.inject.mass))
# i think you need to look at injections w/o knowing sup or not!

ggplot(data=somestats, aes(y=avg.4th.mass, x=as.factor(dose), color=is.sup)) +
  facet_wrap(~temp) +
  geom_point() +
  geom_errorbar(aes(ymax=avg.4th.mass+se.4th.mass, ymin=avg.4th.mass-se.4th.mass))

ggplot(data=somestats, aes(y=avg.5th.mass, x=as.factor(dose), color=is.sup)) +
  facet_wrap(~temp) +
  geom_point() +
  geom_errorbar(aes(ymax=avg.5th.mass+se.5th.mass, ymin=avg.5th.mass-se.5th.mass))
# does the weird rise compare w R4 results?

ggplot(data=somestats, aes(y=avg.6th.mass, x=as.factor(dose), color=is.sup)) +
  facet_wrap(~temp) +
  geom_point() +
  geom_errorbar(aes(ymax=avg.6th.mass+se.6th.mass, ymin=avg.6th.mass-se.6th.mass))

ggplot(data=somestats, aes(y=avg.wand.mass, x=as.factor(dose), color=is.sup)) +
  facet_wrap(~temp) +
  geom_point() +
  geom_errorbar(aes(ymax=avg.wand.mass+se.wand.mass, ymin=avg.wand.mass-se.wand.mass))

ggplot(data=somestats, aes(y=avg.4th.dur, x=as.factor(dose), color=is.sup)) +
  facet_wrap(~temp) +
  geom_point() +
  geom_errorbar(aes(ymax=avg.4th.dur+se.4th.dur, ymin=avg.4th.dur-se.4th.dur))

ggplot(data=somestats, aes(y=avg.5th_nos.dur, x=as.factor(dose),color=is.sup)) +
  facet_wrap(~temp) +
  geom_point() +
  geom_errorbar(aes(ymax=avg.5th_nos.dur+se.5th_nos.dur, ymin=avg.5th_nos.dur-se.5th_nos.dur))

ggplot(data=somestats, aes(y=avg.5th_sup.dur, x=as.factor(dose),color=is.sup)) +
  facet_wrap(~temp) +
  geom_point() +
  geom_errorbar(aes(ymax=avg.5th_sup.dur+se.5th_sup.dur, ymin=avg.5th_sup.dur-se.5th_sup.dur))

ggplot(data=somestats, aes(y=avg.5th_sup.dur, x=as.factor(dose),color=is.sup)) +
  facet_wrap(~temp) +
  geom_point() +
  geom_errorbar(aes(ymax=avg.5th_sup.dur+se.5th_sup.dur, ymin=avg.5th_sup.dur-se.5th_sup.dur))






# plots w/ other axes

endpoints <- read_csv("~/Desktop/endpoints.csv")
endpoints <- endpoints %>% mutate_at((12), ~replace_na(.,"<5"))

endstats <- endpoints %>%
  filter(dose != "" & dose != "none",
         species == "MS") %>%
  group_by(temp, dose, is.sup) %>%
  dplyr::summarise(avg.wand.mass = mean(mass.wander),
                   se.wand.mass = sd(mass.wander, na.rm=TRUE)/length(mass.wander),
                   avg.wand.time = mean(time.to.wand),
                   se.wand.time = sd(time.to.wand, na.rm=TRUE)/length(time.to.wand),
                   n = n())

ggplot(data=endstats, aes(y=avg.wand.mass, x=avg.wand.time,color=dose)) +
  facet_wrap(is.sup~temp) +
  geom_point(size=3) +
  geom_errorbar(aes(ymax=avg.wand.mass+se.wand.mass, ymin=avg.wand.mass-se.wand.mass)) +
  geom_errorbarh(aes(xmax=avg.wand.time+se.wand.time, xmin=avg.wand.time-se.wand.time))
