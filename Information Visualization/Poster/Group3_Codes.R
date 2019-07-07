##Load all necessary libraries
library(plyr)
library(dplyr)
library(tidyr)

library(ggplot2)
library(plotly)
library(knitr)

library(ggthemes)
library(tidytext)
library(highcharter)
options(highcharter.theme = hc_theme_538())
library(rworldmap)

library(plot3D)
library(readxl)

##Female to male ratio map
#Load developer data from https://www.kaggle.com/hackerrank/developer-survey-2018#HackerRank-Developer-Survey-2018-Values.csv
setwd("/Users/erincali/documents/IST719_finalproject")
hack <- read.csv("HackerRank-Developer-Survey-2018-Values.csv")
options(highcharter.theme = hc_theme_538())
hack1 <- hack %>% mutate(CountryNumeric = CountryNumeric2)

##Map
#Create a data set with the Country, number of female coders, number of male coders, and female to male ratio
F2M_countries <- hack1 %>%  group_by(CountryNumeric) %>% mutate(count = n()) %>% 
  
  filter(count > 50) %>% 
  
  filter(q3Gender %in% c('Male','Female')) %>% 
  
  group_by(CountryNumeric,q3Gender) %>% count() %>% 
  
  spread(q3Gender, n) %>% mutate(F2M = Female/Male) %>% arrange(desc(F2M))

#Create a palette of corals to match our poster
colors <- c("#34347F",
            "#5D6DA0",
            "#C7EFE6",
            "#C7EFE6",
            "#FF948A",
            "#FF948A",
            "#FF6666"
)

#Map the female to male ratios for each country
map <- joinCountryData2Map(as.data.frame(F2M_countries), joinCode = "NAME", nameJoinColumn="CountryNumeric" )
mapcountry <- mapCountryData(map, nameColumnToPlot = "F2M", colourPalette = colors, oceanCol= "#808080", missingCountryCol="white", mapTitle = "Women Developers are under-represented globally.", addLegend = FALSE)
do.call(addMapLegend, c(mapcountry, 
                        legendLabels="all", 
                        legendWidth=0.5, 
                        legendIntervals="data", 
                        legendMar = 2))
##Donut plot
#check the summary of Gender
summary(hack$q3Gender)

#subset dataset when gender = female and Country = US
female <- hack[hack$q3Gender == "Female",] 

#check the summary of CountryName
summary(female$CountryNumeric2)

#subset
us_female <- female[female$CountryNumeric2 == "United States",]

#build a table of Industry count
df <- data.frame(table(us_female$q10Industry))

#Rename columns
names(df) <- c("industry", "Freq")

#first two entries are "" and #NULL!. remove them
df <- df[-(1:2),]

#Name industry as "Other" if the count is less than 35
df$industry <- as.character(df$industry)
df$industry <- ifelse(df$Freq < 35, "Other", df$industry)

#Reorganize table
df$industry <- as.factor(df$industry)
df1 <- aggregate(df$Freq, by=list(df$industry), FUN=sum)

#Rename columns
names(df1) <- c("industry", "Freq")

#Add addition columns, needed for drawing with geom_rect.
df1$fraction = df1$Freq / sum(df1$Freq)
df1 = df1[order(df1$fraction), ]
df1$ymax = cumsum(df1$fraction)
df1$ymin = c(0, head(df1$ymax, n=-1))

# Make the plot
p1 = ggplot(df1, aes(fill=industry, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  #theme(legend.position="top") +
  theme_void()
p1

##Hiring managers expectations in the US
# filter data to US and working adults
hack2 <- hack %>%
  filter(q8JobLevel != 'Student') %>%
  filter(CountryNumeric2 == 'United States') %>%
  filter(q3Gender %in% c('Female', 'Male'))

# plot 2 - one dimension
cols_q20 <- names(hack2)[which(grepl('q20', names(hack2)))]
cand_list <- list()

for(i in 1:length(cols_q20)){
  cand_list[[i]] <- hack2 %>% 
    group_by_(cols_q20[i]) %>% 
    count() %>% 
    rename('q20' = !!cols_q20[i])
}

cand_list <- bind_rows(cand_list) %>% 
  filter(! q20 %in% c('','#NULL!' )) %>%
  arrange(desc(n))

barplot(cand_list$n, names.arg = cand_list$q20, ylim = c(0, 1500))

##Gender and Decade histogram
hackerrank_modified <- read_excel("hackerrank_modified.xlsx", col_types = c("skip", "date", "date", "text", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text"))

#rename
HackerRank <- hackerrank_modified

# View dataset
View(HackerRank)

#dataset structure
str(HackerRank)

#create backup dataset
HackerRank1 <- HackerRank

#filter for United States only
HR_US <- HackerRank1 %>% filter(HackerRank1$CountryNumeric2 == "United States")

#filter for Male and Female only
HR_US <- HR_US %>% filter(HR_US$q3Gender != "Non-Binary")

#View Dataset
View(HR_US)

#Create barplot
##Response by Gender and Decade Started
counts <- table(HR_US$q3Gender,HR_US$Decade)
head(counts)
barplot(counts, col=c("pink","blue"),  main="Responses by Gender and Decade", space=0.9, ylim = c(0,2000), ylab = "Responses")

##Age men and women began coding
#read in data
countries <- read.csv('Country-Code-Mapping.csv')
codebook <- read.csv('HackerRank-Developer-Survey-2018-Values.csv')
numeric <- read.csv('HackerRank-Developer-Survey-2018-Numeric.csv')
numeric_mapping <- read.csv('HackerRank-Developer-Survey-2018-Numeric-Mapping.csv')
values <- read.csv('HackerRank-Developer-Survey-2018-Values.csv')

#explore the data
head(countries)
head(codebook)
head(numeric)
head(numeric_mapping)
head(values)
dim(values)

#data cleaning
#get column names for values table
colnames(values)

#Look at the distribution of ages
t <- as.data.frame(table(values$q2Age[values$q2Age != '#NULL!']))
t <- t %>% filter(Var1 != '#NULL!') %>% dplyr::rename(Age.Group = Var1)
t$Age.Group <- factor(t$Age.Group, levels = c("Under 12 years old",
                                              "12 - 18 years old",
                                              "18 - 24 years old",
                                              "25 - 34 years old",
                                              "35 - 44 years old",
                                              "45 - 54 years old",
                                              "55 - 64 years old",
                                              "65 - 74 years old",
                                              "75 years or older"))
t

#create barplot
ggplot(t, aes(x = Age.Group, y = Freq)) + geom_bar(stat = 'identity', fill = "slateblue4") + coord_flip() + theme_bw() + xlab("Age Group") + ylab("Frequency") + ggtitle("Developers by Age Range") + theme(plot.title = element_text(hjust = 0.5))

#Look at the distribution of ages for beginning coding
c <- as.data.frame(table(values$q1AgeBeginCoding[values$q1AgeBeginCoding != '#NULL!']))
c <- c %>% filter(Var1 != '#NULL!') %>% dplyr::rename(Age.Group = Var1)
c$Age.Group <- factor(c$Age.Group, levels = c("5 - 10 years old",
                                              "11 - 15 years old",
                                              "16 - 20 years old",
                                              "21 - 25 years old",
                                              "26 - 30 years old",
                                              "31 - 35 years old",
                                              "36 - 40 years old",
                                              "41 - 50 years old",
                                              "50+ years or older"))

c

#create barplot
ggplot(c, aes(x = Age.Group, y = Freq)) + geom_bar(stat = 'identity', fill = "seagreen2") + coord_flip() + theme_bw() + xlab("Age Group") + ylab("Frequency") + ggtitle("Age Developers Began Coding") + theme(plot.title = element_text(hjust = 0.5))

#Look at developers by gender
g <- as.data.frame(table(values$q3Gender[values$q3Gender != '#NULL!']))
g <- g %>% filter(Var1 != '#NULL!')
g

#Look at gender and ages of developers
ga <- values %>% select(q2Age, q3Gender)
ga <- as.data.frame(table(ga))
ga <- ga %>% filter(q3Gender != '#NULL!', q2Age != '#NULL!') %>% dplyr::rename(Age.Group = q2Age, Gender = q3Gender)
ga$Age.Group <- factor(ga$Age.Group, levels = c("Under 12 years old",
                                                "12 - 18 years old",
                                                "18 - 24 years old",
                                                "25 - 34 years old",
                                                "35 - 44 years old",
                                                "45 - 54 years old",
                                                "55 - 64 years old",
                                                "65 - 74 years old",
                                                "75 years or older"))
ga

ggplot(ga, aes(x = Age.Group, y = Freq, fill = Gender)) + geom_bar(stat = 'identity') + theme_bw() + xlab("Age Group") + ylab("Frequency") + ggtitle("Age Developers Began Coding") + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = c(0.88, 0.68))

chart3 <- filter(c3, !is.na(q3Gender)) %>% ggplot() + 
  geom_hline(yintercept=seq(0,1,.25), colour="white") +
  geom_bar(aes(x = q1AgeBeginCoding, fill = q3Gender), position = "fill") + xlab("Age Began Coding") + ylab("Percentage") +  scale_y_continuous(labels = scales::percent) + ggtitle("Women Beging Coding at Older Ages than Men") + scale_fill_manual(breaks = c("Female", "Male"), values = c("#FF6666", "#34347F")) + theme(legend.position = "bottom", axis.ticks.y=element_blank(),  legend.text = element_text(size = 8), legend.title = element_blank(), axis.text.x = element_text(angle = 0, size = 7), axis.title.x = element_text(size = 9), plot.background = element_rect(fill = "#808080"), legend.background = element_rect(fill = "#808080"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.background = element_rect(fill = "#808080"))
chart3
#ggsave("Chart3.pdf", width=9, height=4)

#isolate job roles to just Junior and Senior developers for easier comparison, remove NULL values, just use ages between 18 and 64
c4 <- values %>% select(q2Age, q8JobLevel, q3Gender) %>% filter(q8JobLevel == 'Level 1 developer (junior)' | q8JobLevel == 'Senior developer') %>% filter(q3Gender != '#NULL!' & q3Gender != "#NULL" & q2Age != '#NULL!' & q3Gender != 'Non-Binary' & q2Age %in% c("18 - 24 years old","25 - 34 years old","35 - 44 years old", "45 - 54 years old", "55 - 64 years old"))
c4$q8JobLevel <- mapvalues(c4$q8JobLevel, from = c("Level 1 developer (junior)", "Senior developer"), to = c("Junior Developer", "Senior Developer"))
c4$q8JobLevel <- factor(c4$q8JobLevel, levels = c("Junior Developer", "Senior Developer"))
table(c4$q8JobLevel)

#merge Age and Gender into one column for graphing
c4$AgeGender <- paste(c4$q3Gender, c4$q2Age, sep = " ")
#remove "years old" from AgeGender column
c4$AgeGender <- gsub(' years old', '', c4$AgeGender)
#remove age and Gender columns
c4 <- c4 %>% select(q8JobLevel, AgeGender) %>% rename(JobLevel = q8JobLevel)
#set AgeGender to ordered factor variable
c4$AgeGender <- factor(c4$AgeGender, levels = c("Female 18 - 24", "Male 18 - 24","Female 25 - 34", "Male 25 - 34", "Female 35 - 44", "Male 35 - 44", "Female 45 - 54", "Male 45 - 54"))
#view table
table(c4$AgeGender)

c4 %>% group_by(JobLevel, AgeGender) %>% filter(!is.na(AgeGender)) %>% tally() %>% arrange(AgeGender)

chart4 <- filter(c4, !is.na(AgeGender)) %>% ggplot() + 
  geom_hline(yintercept=seq(0,1,.25), colour="white") + geom_bar(aes(x = AgeGender, fill = JobLevel), position = "fill") +  scale_y_continuous(labels = scales::percent) + ylab("Percentage") + xlab("Age Gender Group") + ggtitle("Women hold more junior positions than men at all age categories") + theme(plot.title = element_text(size=12)) + scale_fill_manual(breaks = c("Junior Developer", "Senior Developer"), values = c("#c9e2d0", "#29c452")) + theme(legend.position = "bottom", axis.ticks.y=element_blank(),  legend.text = element_text(size = 8), legend.title = element_blank(), axis.text.x = element_text(angle = 0, size = 7), axis.title.x = element_text(size = 9), plot.background = element_rect(fill = "#808080"), legend.background = element_rect(fill = "#808080"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.background = element_rect(fill = "#808080"))
chart4
ggsave("Chart4.pdf", width=9, height=4)

