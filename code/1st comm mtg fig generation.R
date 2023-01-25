library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)


# lab ---------------------------------------------------------------------

### read in data
lab <- read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Documents/projects/ccbv_effects/data/lab_daily-growth.csv", header=T)

names(lab) <- sub("^X", "", names(lab)) # remove leading X's



### filter and remove bad samples, prep data to wrangle
lab_clean2 <- lab %>% select(-c(41:65, 37:39)) %>% # remove junk columns
  filter(!(unique == "R4-25-PBS-3")) %>% # remove super slow growers
  filter(!(unique == "R1-25-0.01-6")) %>%
  filter(!(para.type == "para" & fate == "wand")) %>% # removes unsuccesful paras
  filter(!(para.type == "para" & fate == "pup")) %>%
  filter(!(fate == "L" | fate == "na")) %>% #remove things that died as larvae (but keep dead pupa?)
  filter(!(para.type == "non")) #idk what to do w this

# keep R3 and R4 only
lab_new <- filter(lab_clean2, !(round == "R1" | round == "R2"))

lab_new <- lab_new[,-37] # remove "note" column at the end

# reshape df
lab_new.long <- lab_new %>% gather(key = "days.post.hatch", value = "mass", c(13:36))



### summary stats
stats.sub <- lab_new.long %>% group_by(para.type, heatshock, days.post.hatch) %>% 
  dplyr::summarise(avg = mean(mass, na.rm=TRUE), 
                   se = sd(mass, na.rm=TRUE)/length(mass),
                   n = n())

stats.sub$days.post.hatch <- as.numeric(stats.sub$days.post.hatch) # fixes days column lol



### plotting
lab_avgstats <- ggplot(data=stats.sub, aes(y=avg, x=days.post.hatch, 
                                           #color=as.factor(para.type), 
                                           color=as.factor(heatshock)
))

lab_avgstats + geom_point() + geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
  facet_wrap(~para.type) +
  xlim(0, 25) + scale_color_brewer(palette = "Set1")


# taking out some irrelevant stuff for committee mtg
stats.sub2 <- filter(stats.sub, 
                     !(para.type == "0.50" | para.type == "2.00" ))

lab_avgstats2 <- ggplot(data=stats.sub2, aes(y=avg, x=days.post.hatch, 
                                             #color=as.factor(para.type), 
                                             color=as.factor(heatshock)
))

lab_avgstats2 + geom_line() + geom_errorbar(aes(ymin=avg-se, ymax=avg+se)) +
  facet_wrap(~para.type) +
  xlim(0, 25) + 
  scale_color_manual(values = c("25" = "blue", "40" = "red")) +
  #scale_color_brewer(palette = "Set1") +
  labs(x = "days after hatching", y = "average mass (g)", title = "average mass gain (lab)") +
  guides(color = guide_legend(title = "heatshock"))




# field -------------------------------------------------------------------

### read in data
field <- read.csv("data/fielding2.csv")


### fixing & wrangle prep
# fix column names
names(field) <- sub("^X", "", names(field))
#field_long <- field %>% gather(key = "date", value = "mass", c(9:50)) # gather all date info
# for this code just keep using "long" df -- convert later

# convert "date...." columns to julian
long$date <- as.Date(long$date, format = "%d.%b")
long$jdate <- format(long$date, "%j")

long$date.inject <- as.Date(long$date.inject, format = "%d-%b")
long$jdate.inject <- format(long$date.inject, "%j")

long$jdate <- as.numeric(long$jdate) #jdate no longer julian
long$jdate.inject <- as.numeric(long$jdate.inject)

# create new "days since injection" column
long <- long %>% 
  #select(-c(10,13:14)) %>% #idky lol. probably old trouble columns
  mutate(days.since.inject = jdate-jdate.inject)



### stats
# some more cleaning to avoid breaking things
long$is.sup <- sub("^$", "no", long$is.sup) # fill empties w "no"
long <- long[,-1] # get rid of the ?s in the 1st column

field_MS <- long %>% # separates sups
  filter(species == "MS",
         days.since.inject >= 0,
         date.pmd == "",
         !(dose == "0.5" | dose == "uninj")) %>% # match up w/ lab graphs
  group_by(days.since.inject, temp, dose, is.sup, species) %>%
  dplyr::summarise(avg.mass = mean(mass, na.rm=TRUE),
                   se.mass = sd(mass, na.rm=TRUE)/length(mass),
                   n = n())




### plot
fieldplot <- field_MS %>% #filter(is.sup == "no") %>% # remove sup for committee mtg so x-axis isnt cropped
  ggplot(aes(y=avg.mass, x=days.since.inject, 
             #color=as.factor(para.type), 
             color=as.factor(temp)
  ))

fieldplot + geom_line() + geom_errorbar(aes(ymin=avg.mass-se.mass, ymax=avg.mass+se.mass)) +
  facet_wrap(is.sup~dose) +
  xlim(0, 25) + 
  scale_color_manual(values = c("25" = "blue", "40" = "red")) +
  #scale_color_brewer(palette = "Set1") +
  labs(x = "days after injection", y = "average mass (g)", title = "average mass gain (field)") +
  guides(color = guide_legend(title = "heatshock"))


