################################
### Rec Data sum stats ######
##############################

### Clear memory
rm(list = ls())

install.packages(pacman)
library(pacman)
p_load(apollo, tidyverse, haven, car, rmdcev, readxl, ggplot2,
       install = TRUE)

#load data, select and rename vars
df <- read_csv("Data/CanadaRecData.csv")##load data
df = df%>%
  select(id, quant, costs, activity, indig, imigrant, male, income, age, ageindex, university, college, urban, cabin, web_sample)# keep only these variables

## removing instances where expenditure>income ... need to identify the individuals and remove their data
## e.g., row 326 is individual 20, so remove all their data. remove all rows where expenditure is greater than income, sorting by ID

df2 <- df %>% group_by(id, income) %>% summarise(xsum = sum(costs * quant))
removals <- df2$id[df2$xsum >= df2$income]
df <- filter(df, !id %in% removals)
head(df)
sapply(df, function(x) length(unique(x))) 
summary(df)
all_coll <- df %>%
  filter(college==1)
sapply(all_coll, function(x) length(unique(x)))
all_uni <- df %>%
  filter(university==1)
sapply(all_uni, function(x) length(unique(x)))
all_urban <- df %>%
  filter(urban==1)
sapply(all_urban, function(x) length(unique(x)))
all_cabin<- df %>%
  filter(cabin==1)
sapply(all_cabin, function(x) length(unique(x)))
all_websample <- df %>%
  filter(web_sample==1)
sapply(all_websample, function(x) length(unique(x)))

### summarize

male<-df %>% filter(male==1)
summary(male)# income

sapply(male, function(x) length(unique(x))) 
male_coll <- male %>%
  filter(college==1)
sapply(male_coll, function(x) length(unique(x)))
male_uni <- male %>%
  filter(university==1)
sapply(male_uni, function(x) length(unique(x)))
male_urban <- male %>%
  filter(urban==1)
sapply(male_urban, function(x) length(unique(x)))
male_cabin <-male %>%
  filter(cabin==1)
sapply(male_cabin, function(x) length(unique(x)))
male_web <-male %>%
  filter(web_sample==1)
sapply(male_web, function(x) length(unique(x)))


## FEMALE
female<-df %>% filter(male==0)
summary(female)
sapply(female, function(x) length(unique(x)))
female_coll <- female %>%
  filter(college==1)
sapply(female_coll, function(x) length(unique(x)))
female_uni <- female %>%
  filter(university==1)
sapply(female_uni, function(x) length(unique(x)))
female_urban <- female %>%
  filter(urban==1)
sapply(female_urban, function(x) length(unique(x)))
female_cabin <-female %>%
  filter(cabin==1)
sapply(female_cabin, function(x) length(unique(x)))
female_web <-female %>%
  filter(web_sample==1)
sapply(female_web, function(x) length(unique(x)))

## INDIGENOUS
indig<-df %>% filter(indig==1, 
                     imigrant==0)
summary(indig)
sapply(indig, function(x) length(unique(x)))
indig_coll <- indig %>%
  filter(college==1)
sapply(indig_coll, function(x) length(unique(x)))
indig_uni <- indig %>%
  filter(university==1)
sapply(indig_uni, function(x) length(unique(x)))
indig_urban <- indig %>%
  filter(urban==1)
sapply(indig_urban, function(x) length(unique(x)))
ind_cabin <-indig %>%
  filter(cabin==1)
sapply(ind_cabin, function(x) length(unique(x)))
ind_web <-indig %>%
  filter(web_sample==1)
sapply(ind_web, function(x) length(unique(x)))

## Immigrant
imi<-df %>% filter(imigrant==1,
                   indig==0)
summary(imi)
sapply(imi, function(x) length(unique(x)))
imi_coll <- imi %>%
  filter(college==1)
sapply(imi_coll, function(x) length(unique(x)))
imi_uni <- imi %>%
  filter(university==1)
sapply(imi_uni, function(x) length(unique(x)))
imi_urban <- imi %>%
  filter(urban==1)
sapply(imi_urban, function(x) length(unique(x)))
im_cabin <-imi %>%
  filter(cabin==1)
sapply(im_cabin, function(x) length(unique(x)))
im_web <-imi %>%
  filter(web_sample==1)
sapply(im_web, function(x) length(unique(x)))

## Neither

other<-df %>% filter(imigrant==0,
                     indig==0)
summary(other)
sapply(other, function(x) length(unique(x)))
other_coll <- other %>%
    filter(college==1)
sapply(other_coll, function(x) length(unique(x)))
other_uni <- other %>%
  filter(university==1)
sapply(other_uni, function(x) length(unique(x)))
other_urban <- other %>%
  filter(urban==1)
sapply(other_urban, function(x) length(unique(x)))
oth_cabin <-other %>%
  filter(cabin==1)
sapply(oth_cabin, function(x) length(unique(x)))
oth_web <-other %>%
  filter(web_sample==1)
sapply(oth_web, function(x) length(unique(x)))

