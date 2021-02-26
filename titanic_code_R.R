####Libraries
library(ggplot2)
library(dplyr)
library(scales)

#####Read data
raw_data <- read.csv(file.choose(new = FALSE))

###Exploratory analysis
str(raw_data)  #891 obs, 12 vars
summary(raw_data) #177 NAs in age, 38% survived
sum(is.na(raw_data)) #confirms 177 NAs in dataset
sum(is.na(raw_data))/nrow(raw_data) #~20% of Age values have missing values, likely to have an important impact on survival rate

#Checking duplicate passengers 
raw_data %>%
  group_by(PassengerId) %>%
  count() %>% filter(n > 1) #no duplicate passenger IDs
  

###Data Cleaning
#Convert Survived to factor
raw_data$Survived <- factor(raw_data$Survived, labels = c('Died', 'Survived'))
#Convert Pclass to factor
raw_data$Pclass <- factor(raw_data$Pclass)
str(raw_data$Pclass)

#Fixing NAs
#Looking at the sample of NAs
raw_data %>% filter(is.na(Age) == TRUE) %>% summary()
raw_data %>% filter(is.na(Age) == FALSE) %>% summary()

#Distribution of Sibsp
#Proving that you cannot use either Parch or SibSp vars as singular determinants
raw_data %>%
  ggplot(aes(factor(SibSp))) +
  geom_bar()

raw_data %>%
  ggplot(aes(factor(SibSp), Age)) + 
  geom_boxplot(aes(fill = factor(SibSp))) #+ facet_wrap(~Parch)

#Create SibSp groups
raw_data <- raw_data %>%
  mutate(SibSp_gr = case_when(SibSp == 0 | SibSp == 1 ~ "0-1",
                              SibSp == 2 | SibSp == 3 | SibSp == 4 ~ "2-4", 
                              SibSp > 4 ~ ">4"
                              ))
                                                                         

#Average/Median age of passengers by demographic variables, where is.na == FALSE
group_df <- raw_data %>% 
              filter(is.na(Age) == FALSE) %>%
              group_by(Sex, Pclass, Parch, SibSp_gr) %>%
              summarise(n_passengers = n(),
                        Mean_age = mean(Age),
                        med_age = median(Age))

raw_data %>% 
  filter(is.na(Age) == TRUE) %>%
  #group_by(Sex, Pclass, Parch, SibSp) %>%
  summary()


#Updating missing ages with median Age of grouped demographic info
uptd_data <-raw_data %>%
              left_join(group_df, by = c('Sex' = 'Sex', 'Pclass' = 'Pclass', 'Parch' = 'Parch', 'SibSp_gr' = 'SibSp_gr')) %>%
              mutate(Age = ifelse(is.na(Age) == TRUE, med_age, Age)) 

summary(uptd_data) #No missing values

###Uni-variable exploration, understanding the dataset
pal <- choose_palette()
#Died vs Survived overall
uptd_data %>%
  group_by(Survived) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup() %>%
  mutate(pct = count/sum(count)) %>%
  ggplot() +
  aes(x = '', y = pct, fill = Survived) +
  geom_bar(width = 1, stat = 'identity') +
  geom_text(aes(y = pct,
                label = round(pct*100,1)),
            color = 'white',
            position = position_stack(vjust = 0.5),
            size = 15) +
  scale_fill_manual(values = pal(2), name = 'Survival Outcome') +
  coord_polar('y',start = 0) +
  theme_void() +
  ggtitle('Survival Outcome Ratio Among Titanic Passengers',
          subtitle = '% of passengers\n\n')

#Sex
uptd_data %>%
  ggplot(aes(Sex)) +
  geom_bar(aes(fill = Sex)) +
  scale_y_continuous(breaks = seq(0,600,100)) +
  scale_fill_manual(values = pal(2), name = 'Sex') +
  geom_text(stat = 'count', aes(label = ..count..),
            color = 'white',
            position = position_stack(vjust = 0.5),
            size = 15) +
  labs(y = 'Count') +
  ggtitle('Sex of Titanic Passengers',
          subtitle = 'count of passengers\n\n') +
  theme_bw()

#SES
uptd_data %>%
  ggplot(aes(Pclass)) +
  geom_bar(aes(fill = Pclass)) +
  aes(y=stat(count)/sum(stat(count))) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.6,.1)) +
  scale_x_discrete(limits = rev(levels(uptd_data$Pclass))) +
  coord_flip()+
  geom_text(stat = 'count', aes(label = scales::percent(..count../sum(..count..))),
            color = 'black',
            position = position_stack(vjust = 0.5),
            size = 10) +
  labs(x = 'Ticket Class', y = 'Percent', 
       fill = 'Ticket class') +
  ggtitle('Ticket Class Percent Breakdown Among Titanic Passengers',
          subtitle = 'Ticket class is a proxy for Socio-economic status\n\n') +
  scale_fill_manual(values = pal(3), label =  c('1st', '2nd', '3rd')) + 
  theme_bw()

#Parch and Sibsp
uptd_data %>%
  group_by(Parch) %>%
  summarise(count = n(), .groups = 'drop')

#Sex and SES
uptd_data %>%
  group_by(Sex, Pclass) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup() %>%
  mutate(pct = count/sum(count)) %>%
  ungroup() %>%
  
  ggplot() +
  aes(x = Sex, y = Pclass, fill = pct) +
  geom_raster() +
  geom_text(aes(label = round(pct*100,1)),
            size = 12,
            color = 'white') +
  theme_minimal()
  
  

#Age, comparison
age_miss <-raw_data %>%
              ggplot(aes(Age)) + 
              geom_histogram(binwidth = 5, boundary = 0, closed = "left", fill = 'red', color = 'black') +
              aes(y=stat(count)/sum(stat(count))) + 
              scale_y_continuous(labels = scales::percent, limits = c(0,.25)) +
              scale_x_continuous(breaks = seq(0,80,5)) +
              ggtitle('Histogram of Titanic Passengers Age - Missing Values') +
              theme_bw()

age_no_miss <- uptd_data %>%
                ggplot(aes(Age)) + 
                geom_histogram(binwidth = 5, boundary = 0, closed = "left", fill = 'blue', color = 'black') +
                aes(y=stat(count)/sum(stat(count))) + 
                scale_y_continuous(labels = scales::percent, limits = c(0,.25)) +
                scale_x_continuous(breaks = seq(0,80,5)) +
                ggtitle('Histogram of Titanic Passengers Age - Non-missing values') +
                theme_bw()

ggpubr::ggarrange(age_miss, age_no_miss, heights = c(1, 0.5))

###Multivate exploratory
#Age vs died - not much relationship
uptd_data %>%
  ggplot() +
  aes(x = Survived, y = Age) +
  geom_boxplot()

#Loop through factored variables
library(gridExtra)
library(grid)

graphs <- list()
i = 1

var_list <- names(uptd_data %>%
                      select(Pclass, Sex, Parch, SibSp))

graph_func <- function(df, var) {
  p <- df %>%
    select(var, 'Survived') %>%
    group_by_(var, 'Survived') %>%
    summarise(count = n(), .groups = 'drop') %>%
    ungroup() %>%
    group_by_(var) %>%
    mutate(pct = count/sum(count)) %>%
    rename(Q = var)
  
  p <<- p %>%
    ggplot() +
    aes(x = factor(Q), y= pct, fill = Survived) +
    geom_bar(stat = 'identity', position = 'stack') +
    geom_text(aes(label = round(pct*100,0)),
              size = 8,
              color = 'white',
              position = position_stack(vjust = 0.5)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = pal(2), name = 'Survival Status') +
    labs(y = 'Percent',
         x = ifelse(var == 'Pclass', 'Ticket Class', 
                    ifelse(var == 'Sex','Sex',
                           ifelse(var=='Parch','Parent/Child #','Sibling/Spouse #')))) +
    theme_bw() +
    ggtitle(ifelse(var == 'Pclass', 'Ticket Class', 
                   ifelse(var == 'Sex','Sex',
                          ifelse(var=='Parch','Parent/Child #','Sibling/Spouse #'))),
            subtitle = '% passengers of group')
  
  
  
  #return(p)

}

for (v in var_list) {
  graph_func(uptd_data, v)
  graphs[[i]] <- p
  i = i + 1
}

txt <- textGrob('Survival Rate by Different Demographic Variables',
                gp = gpar(fontface = 'bold',
                          fontsize = 30))

g3 <- arrangeGrob(grobs = graphs, ncol = 2)
g3 <- arrangeGrob(txt,
                  g3,
                  ncol = 1,
                  heights = c(1,10))
#Plot final grid
grid.arrange(g3)





#Pclass vs Survived - seemingly strong relationship
#Gender by Survival - strong relationship

#Ticket class by gender vs Survived - not as much of a strong relationship
uptd_data %>%
  mutate(pclass_gender = outer(Sex, "_", Pclass, FUN=paste0)) %>%
  group_by(pclass_gender, Survived) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(pct = count/sum(count)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = Survived, y = pclass_gender, fill = pct) +
  geom_raster() + 
  geom_text(aes(label = round(pct*100,1)),
            color = 'white',
            size = 12) +
  scale_fill_gradient(high = pal(4),
                      low = pal(4)) +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle('Gender by Ticket Class Survival Rate ',
          subtitle = '% of passengers\n\n')


#Sex by Age vs Survived (boxplot)
uptd_data %>%
  ggplot() +
  aes(x = Sex, y = Age, fill = Survived) +
  geom_boxplot() + 
  theme_bw() +
  scale_fill_manual(values = pal(2)) + 
  ggtitle('Age Range by Gender and Survival Status Among Passengers')
#Ticket class by age vs Survived (boxplot)
uptd_data %>%
  ggplot() +
  aes(x = Pclass, y = Age, fill = Survived) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = pal(2)) + 
  ggtitle('Age Range by Ticket Class and Survival Status Among Passengers')


  

uptd_data %>%
  ggplot(aes(Age)) + 
  geom_histogram(binwidth = 10, boundary = 0, closed = "left", aes(fill = Survived), color = 'black') +
  aes(y=stat(count)/sum(stat(count))) + 
  #scale_y_continuous(labels = scales::percent) +
  #scale_x_continuous(breaks = seq(0,80,10)) +
  facet_wrap(~Survived) +
  labs(title = ('Histogram of Titanic Passengers Age by Survival Outcome')) +
  theme_bw()

age_died <-uptd_data %>% filter(uptd_data$Survived == 'Died') %>%
            ggplot(aes(Age)) + 
            geom_histogram(binwidth = 10, boundary = 0, closed = "left", fill = 'red', color = 'black', position = 'dodge') +
            aes(y=stat(count)/sum(stat(count))) + 
            scale_y_continuous(labels = scales::percent, limits = c(0,.45)) +
            scale_x_continuous(breaks = seq(0,80,10)) +
            theme_bw()


age_surv <- uptd_data %>% filter(uptd_data$Survived == 'Survived') %>%
              ggplot(aes(Age)) + 
              geom_histogram(binwidth = 10, boundary = 0, closed = "left", fill = 'blue', color = 'black', position = 'dodge') +
              aes(y=stat(count)/sum(stat(count))) + 
              scale_y_continuous(labels = scales::percent, limits = c(0,.45)) +
              scale_x_continuous(breaks = seq(0,80,10)) +
              #facet_wrap(~Survived) +
              theme_bw()

ggpubr::ggarrange(age_died, age_surv, heights = c(1, 0.5))





