#importing our libraries
library(tidyverse)
library(dplyr)
library(forcats)

# to see available datasets in Tidyverse package
data()

#i'll be using the midwest dataset that comes in tidyverse package
View(midwest)

#returns metadata on our dataframe
?midwest

#returns structure of our df
str(midwest)

#returns names of variables/columns
names(df)

#selecting columns we need
df <- midwest %>%
  select(county, state, area, starts_with("pop"), perchsd:inmetro)

#taking a look at our datatypes
glimpse(df)

#we will change data type of county, state and inmetro to factor datatype
df$county <- as.factor(df$county)
df$state <- as.factor(df$state)
df$inmetro <- as.factor(df$inmetro)

#summary stats for state and inmetro variable
summary(df$state)
summary(df$inmetro)

#checking how many unique states we have in our df
unique(df$state)
#we have 6 states in our df, all of which the names are abbreviated

#recoding the state variable to show the full state name
df <- df %>%
  select(county : inmetro) %>%
  mutate(state= recode(state, "IL" = "Illinois", "IN" = "Indiana", 
                       "MI" = "Michigan", "OH" = "Ohio", "WI" = "Wisconsin"))


#checking duplicate values in our dataframe
df[duplicated(df), ]

#checking for missing values
df %>%
  select(county:inmetro) %>%
  filter(!complete.cases(.))
#we don't have missing values in our df


#looking at the spread of poptotal variable
summary(df$poptotal)
# the mean > median which means the data is right skewed
#in instances such as this we can use the median as central tendency

#lets visaulize it with a box plot
df %>%
  ggplot(aes(poptotal)) + 
  geom_boxplot(outlier.color= "blue", outlier.size = 1)+
  theme_bw() +
  scale_x_log10()

#plotting a histogram of our poptotal variable
df %>%
  ggplot(aes(poptotal)) +
  geom_histogram() +
  theme_bw() +
  scale_x_log10()

#we can locate outliers in our df using this method
iqr <- quantile(df$poptotal, 0.75) - quantile(df$poptotal, 0.25)
lower_threshold <- quantile(df$poptotal, 0.25) - 1.5 * iqr
upper_threshold <- quantile(df$poptotal, 0.75) + 1.5 * iqr

#we filter our df to show only the values below and above the thresholds
df %>%
  filter(poptotal < lower_threshold | poptotal> upper_threshold) %>%
  select(county, state:inmetro) %>%
  View()


#correlation between area and population total
cor(log(df$poptotal), df$area)
#r= 0.101718, this means there is no relationship between area and population total 

#lets visualize it with a scatter plot
df %>%
  ggplot(aes(area, poptotal)) +
  geom_point() +
  geom_smooth(method= "lm", se= FALSE) +
  theme_bw() +
  scale_y_log10()

#returns the county in each state with the highest population
df %>%
  select(county, state, poptotal) %>%
  group_by(state) %>%
  top_n(1, poptotal)
  
  
#total population in each state
df %>%
  select(state, poptotal) %>%
  group_by(state) %>%
  summarise(total_pop_state= sum(poptotal)) %>%
  arrange(desc(total_pop_state))

#lets visaulize it
df %>%
  select(state, poptotal) %>%
  group_by(state) %>%
  summarise(total_pop_state= sum(poptotal)) %>%
  arrange(desc(total_pop_state)) %>%
  ggplot(aes(forcats::fct_reorder(state, total_pop_state, .desc=TRUE), 
             total_pop_state)) +
  geom_col(fill= "#02121E") +
  theme_bw() +
  labs(x= "State",
       y= "Total Population",
       title= "Total Population by State")
  

#lets calculate the:
#1. number of children below poverty line
#2. number of adults below poverty line
#3. number of elderly below poverty line
pov <- df %>%
  mutate(no_of_chd_belowpoverty = round(percchildbelowpovert/100 * 
                                          poppovertyknown,0),
         no_of_adultbelowpoverty= round(percadultpoverty/100 * 
                                          poppovertyknown,0),
         no_of_elderlybelowpoverty = round(percelderlypoverty/100 * 
                                             poppovertyknown,0)) %>%
  group_by(state) %>%
  summarize(chd_belowpoverty= sum(no_of_chd_belowpoverty),
            adultbelowpoverty= sum(no_of_adultbelowpoverty),
            elderlybelowpoverty= sum(no_of_elderlybelowpoverty))
View(pov)

#lets visualize the various poverty categories
pov %>%
  ggplot(aes(forcats::fct_reorder(state, chd_belowpoverty),
             chd_belowpoverty)) +
  geom_col(fill= "#02121E") +
  coord_flip() +
  theme_bw() +
  labs(x= "State",
       y= "No of Children below Poverty Line",
       title= "Children Below Poverty Line in Each State")

pov %>%
  ggplot(aes(forcats::fct_reorder(state, adultbelowpoverty), 
             adultbelowpoverty)) +
  geom_col(fill= "#02121E") +
  coord_flip() +
  theme_bw() +
  labs(x = "State",
       y= "No of Adult below Poverty Line",
       title= "Adult Below Poverty Line in Each State")

pov %>%
  ggplot(aes(forcats::fct_reorder(state, elderlybelowpoverty), 
             elderlybelowpoverty)) +
  geom_col(fill= "#02121E") +
  coord_flip() +
  theme_bw() +
  labs(x= "State", 
       y= "No of Elderly People below Poverty Line",
       title= "Elderly People Below Poverty Line in Each State")

