library(readr)
library(knitr)
library(dplyr)
library(magrittr)
library(lubridate)
library(systemfonts)
library(tidyr)
library(tidyverse)

data <- read_csv('broadwaydata.csv')

data <- data %>% arrange(Attend)
View(data)

data$Attend %>% mean()

data

data$Attend %>% barplot(xlab="Show")

data %>% summarise(
              Min = min(Attend,na.rm = TRUE),
              Q1 = quantile(Attend,probs = .25,na.rm = TRUE),
              Median = median(Attend, na.rm = TRUE),
              Q3 = quantile(Attend,probs = .75,na.rm = TRUE),
              Max = max(Attend,na.rm = TRUE),
              IQR = IQR(Attend,na.rm = TRUE),
              Range = max(Attend,na.rm=TRUE)-min(Attend,na.rm=TRUE),
              Mean = mean(Attend, na.rm = TRUE),
              SD = sd(Attend, na.rm = TRUE))

data %>% group_by(Type) %>% summarise(n = n(),
                                      Min = min(Attend,na.rm = TRUE),
                                      Q1 = quantile(Attend,probs = .25,na.rm = TRUE),
                                      Median = median(Attend, na.rm = TRUE),
                                      Q3 = quantile(Attend,probs = .75,na.rm = TRUE),
                                      Max = max(Attend,na.rm = TRUE),
                                      IQR = IQR(Attend,na.rm = TRUE),
                                      Range = max(Attend,na.rm=TRUE)-min(Attend,na.rm=TRUE),
                                      Mean = mean(Attend, na.rm = TRUE),
                                      SD = sd(Attend, na.rm = TRUE))


boxplot(data$Attend ~ data$Type,
        main = "Boxplot of Attendance in Musicals vs Plays",
        xlab = "Type",
        ylab = "Attendance" )



Plays <- data[data$Type == "Play",]
Musicals <- data[data$Type == "Musical",]

a <- Plays %>% select(Show, Type, Attend)
b <- Plays %>% select(Show, Type, `AttendPrev Week`)
colnames(b)
names(b)[names(b) == "AttendPrev Week"] <- "Attend"
colnames(b)
twp <- bind_rows(a,b)
twp

c <- Musicals %>% select(Show,Type, Attend)
d <- Musicals %>% select(Show, Type,`AttendPrev Week`)
colnames(d)
names(d)[names(d) == "AttendPrev Week"] <- "Attend"
colnames(d)
twm <- bind_rows(c,d)
twm


twp$Attend %>% hist(breaks=30)

twm$Attend %>% hist(breaks=30)

all_shows <- bind_rows(twp,twm)
all_shows

all_shows$Attend <- as.numeric(all_shows$Attend)

all_shows$Attend %>% hist(breaks=30)

all_shows$Attend %>% boxplot()

all_shows %>% summarise(
  Min = min(Attend,na.rm = TRUE),
  Q1 = quantile(Attend,probs = .25,na.rm = TRUE),
  Median = median(Attend, na.rm = TRUE),
  Q3 = quantile(Attend,probs = .75,na.rm = TRUE),
  Max = max(Attend,na.rm = TRUE),
  IQR = IQR(Attend,na.rm = TRUE),
  Range = max(Attend,na.rm=TRUE)-min(Attend,na.rm=TRUE),
  Mean = mean(Attend, na.rm = TRUE),
  SD = sd(Attend, na.rm = TRUE))

library(lattice)
library(ggplot2)
all_shows %>% histogram(~Attend| Type, data = .,breaks=30,layout=c(1,2))


## Confidence interval for mean attendance based on all types 

mean <- mean(all_shows$Attend,na.rm = TRUE)
sd <- sd(all_shows$Attend,na.rm = TRUE)
n <- length(all_shows$Attend,na.rm = TRUE)

## write percentage as a decimal 
q <- 0.95

## assign p and df based on stats 
p <- (1-q)/2
df <- n - 1

## solve confidence interval formula
cutie <- qt(p, df, lower.tail = FALSE)
sn <- sqrt(n)

upper <- mean + (cutie*(sd/sn))

lower <- mean - (cutie*(sd/sn))  

upper 

lower

## Confidence interval for mean attendance based musical only 

mean <- mean(twm$Attend,na.rm = TRUE)
sd <- sd(twm$Attend,na.rm = TRUE)
n <- length(twm$Attend,na.rm = TRUE)

## write percentage as a decimal 
q <- 0.95

## assign p and df based on stats 
p <- (1-q)/2
df <- n - 1

## solve confidence interval formula
cutie <- qt(p, df, lower.tail = FALSE)
sn <- sqrt(n)

upper <- mean + (cutie*(sd/sn))

lower <- mean - (cutie*(sd/sn))  

upper 

lower



## Confidence interval for mean attendance based plays only 

mean <- mean(twp$Attend,na.rm = TRUE)
sd <- sd(twp$Attend,na.rm = TRUE)
n <- length(twp$Attend,na.rm = TRUE)

## write percentage as a decimal 
q <- 0.95

## assign p and df based on stats 
p <- (1-q)/2
df <- n - 1

## solve confidence interval formula
cutie <- qt(p, df, lower.tail = FALSE)
sn <- sqrt(n)

upper <- mean + (cutie*(sd/sn))

lower <- mean - (cutie*(sd/sn))  

upper 

lower

library(lubridate)
library(stringr)
library(magrittr)
library()

# WRANGLING


glimpse(data)

data$`Week End` %<>% mdy()
data$Show %<>% str_to_title()
data$Type %<>% factor(levels = c("Play", "Musical"))
data$Grosses %<>% str_remove_all("[$,]") %>% as.numeric()
data$`GrossesPrev Week` %<>% str_remove_all("[$,]") %>% as.numeric() 
data$`GG%GP` %<>% str_remove_all("%") %>% as.numeric() 
data$`% Cap` %<>% str_remove_all("%") %>% as.numeric() 



