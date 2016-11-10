#-----------------------------------------------------------------
#               Visual Analysis using ggplot2                     |
#        Exploratory analysis of Bank customers data (Stay/Exit)  |
#  (keep in mind that ususally these kind of problems comes with  |
#    imbalanced data ==> while modeling, care has to be taken )   |
#-----------------------------------------------------------------

setwd('G:/DATASCIENCE/DS-PROJECTS/15_Visual_Analytics/Banking/')
rm(list=ls())
#-----------------------------------------------------------------
library('dplyr')
library('ggplot2')
library('gridExtra')
library('grid')
library('scales')
library('corrplot')
library('mlr')
#-----------------------------------------------------------------
data <- read.csv('Churn-Modelling.csv', na.strings = c('',' ', '?', 'NA'), stringsAsFactors = T)
# As I am not intended to do predictive modeling here, read only the input data

str(data)
# Target : Exited

data$HasCrCard      <- ifelse(data$HasCrCard == 0, 'NO',"YES")
data$IsActiveMember <- ifelse(data$IsActiveMember == 0, 'NO','YES')
data$Exited         <- ifelse(data$Exited == 0, 'Stayed', 'Exited')

data$Tenure <- as.factor(data$Tenure)
data$NumOfProducts <- as.factor(data$NumOfProducts)
data$HasCrCard <- as.factor(data$HasCrCard)
data$IsActiveMember <- as.factor(data$IsActiveMember)
data$Exited <- as.factor(data$Exited)


#-----------------------------------------------------------------------
# Data Summarizations and Tabulations
#------------------------------------------------------------------
# Though I prefer 'dplyr' , base R's prop.table() is providing the 
# cleaner output --> I have sticked to prop.table() in the analysis 
#---------------------------------------------------------------------------
# data %>% group_by(Geography) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
# data %>% group_by(Gender) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
# data %>% group_by(Geography,Gender) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
# data %>% group_by(Exited) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
# data %>% group_by(Geography,Exited) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
# data %>% group_by(HasCrCard,Exited) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
# data %>% group_by(Gender,Exited) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
# data %>% group_by(Geography,Gender,Exited) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
#-------------------------------------------------------------------
prop.table(table(data$Geography))
prop.table(table(data$Gender))
prop.table(table(data$Tenure))
prop.table(table(data$NumOfProducts))
prop.table(table(data$HasCrCard))
prop.table(table(data$IsActiveMember))
prop.table(table(data$Exited))

prop.table(table(data$Geography,data$Exited),1)
prop.table(table(data$Gender,data$Exited),1)
prop.table(table(data$Tenure,data$Exited),1)
prop.table(table(data$NumOfProducts,data$Exited),1)
prop.table(table(data$HasCrCard,data$Exited),1)
prop.table(table(data$IsActiveMember,data$Exited),1)
#------------------------- Univariate Exploration ----------------------
summary(data)

cred.sco_hist <- ggplot(data,aes(CreditScore)) + geom_histogram(binwidth=1.0,fill=alpha('blue',0.4)) + 
  scale_x_continuous(limits=c(300,820)) + labs(title='CreditScore_histogram')
  
a2 <- ggplot(data,aes(CreditScore)) + geom_histogram(binwidth=0.1,fill=alpha('blue',0.4)) + 
  scale_x_sqrt() # this is also better
a3 <- ggplot(data,aes(CreditScore)) + geom_histogram(binwidth=0.002) + scale_x_log10()
#----------------------
age_hist <- ggplot(data,aes(Age, ..density..)) + geom_histogram(binwidth=0.6,fill=alpha('blue',0.4)) + 
  geom_density(color='red') + labs(title='Age_histogram')

#ggplot(data,aes(Age)) + geom_freqpoly(binwidth=1.0)
#----------------------
# ggplot(data,aes(Balance)) + geom_histogram() + scale_x_sqrt()
b <- ggplot(data,aes(Balance)) + geom_histogram(binwidth = 0.01) + scale_x_log10()
# Though log10 helped, instead cap the small values
balancee_hist <- ggplot(data,aes(Balance)) + geom_histogram(binwidth = 1000,fill=alpha('blue',0.4)) + 
  scale_x_continuous(limits=c(9000,260000)) + labs(title='Balance_histogram')
#----------------------
Est_sal <- ggplot(data,aes(EstimatedSalary)) + geom_histogram(binwidth = 1000, fill=alpha('blue',0.4)) +
  labs(title='Estimated_salary_histogram')
#-----------------------------------------------------------------------
grid.arrange(a2,age_hist,balancee_hist,Est_sal,nrow=2)
#-----------------------------------------------------------------------
# -- 'Credit Score'     : did applied a 'square root' transformation
# -- 'Age'              : is somewhat rigth skewed
# -- 'Balance'          : applied some scale restictions 
# -- 'Estimated Salary' : (not well distributed)
#-----------------------------------------------------------------------
#-------------------------- Categorical Variables ----------------------
# Geograpy
Geography <- ggplot(data, aes(Geography,fill=Geography)) + geom_bar(aes(y=(..count../sum(..count..)))) +
  geom_text(aes(y=(..count../sum(..count..)),label=scales::percent((..count..)/sum(..count..))),
  stat='count', vjust=1.0) + scale_y_continuous(labels=percent_format()) +
  labs(title='Geography',y='Count_Percent', x='') +  theme(legend.position='none') 
# Gender
Gender <- ggplot(data, aes(Gender,fill=Gender)) + geom_bar(aes(y=(..count../sum(..count..)))) +
  geom_text(aes(y=(..count../sum(..count..)),label=scales::percent((..count..)/sum(..count..))),
            stat='count', vjust=1.0) + scale_y_continuous(labels=percent_format()) +
  labs(title='Gender',y='Count_Percent',x='') +  theme(legend.position='none') 
# Tenure
Tenure <- ggplot(data, aes(Tenure,fill=Tenure)) + geom_bar(aes(y=(..count../sum(..count..)))) +
  geom_text(size=2.5,aes(y=(..count../sum(..count..)),label=scales::percent((..count..)/sum(..count..))),
            stat='count', vjust=1.0) + scale_y_continuous(labels=percent_format()) +
  labs(title='Tenure',y='Count_Percent',x='') +  theme(legend.position='none') 
# NumOfProducts
No.Prods <- ggplot(data, aes(NumOfProducts,fill=NumOfProducts)) + geom_bar(aes(y=(..count../sum(..count..)))) +
  geom_text(aes(y=(..count../sum(..count..)),label=scales::percent((..count..)/sum(..count..))),
            stat='count', vjust=0.5) + scale_y_continuous(labels=percent_format()) +
  labs(title='NumOfProducts',y='Count_Percent',x='') +  theme(legend.position='none') 
# HasCrCard
Crcard <- ggplot(data, aes(HasCrCard,fill=HasCrCard)) + geom_bar(aes(y=(..count../sum(..count..)))) +
  geom_text(aes(y=(..count../sum(..count..)),label=scales::percent((..count..)/sum(..count..))),
            stat='count', vjust=1.0) + scale_y_continuous(labels=percent_format()) +
  labs(title='HasCrCard',y='Count_Percent',x='') +  theme(legend.position='none') 
# IsActiveMember
Active <- ggplot(data, aes(IsActiveMember,fill=IsActiveMember)) + geom_bar(aes(y=(..count../sum(..count..)))) +
  geom_text(aes(y=(..count../sum(..count..)),label=scales::percent((..count..)/sum(..count..))),
            stat='count', vjust=1.0) + scale_y_continuous(labels=percent_format()) +
  labs(title='IsActiveMember',y='Count_Percent',x='') +  theme(legend.position='none') 
# Exited
Ex_stay <- ggplot(data, aes(Exited,fill=Exited)) + geom_bar(aes(y=(..count../sum(..count..)))) +
  geom_text(aes(y=(..count../sum(..count..)),label=scales::percent((..count..)/sum(..count..))),
            stat='count', vjust=1.0) + scale_y_continuous(labels=percent_format()) +
  labs(title='Exited',y='Count_Percent',x='') +  theme(legend.position='none') 

# Tenure is not included -- probably needs to regroup them

grid.arrange(Geography,No.Prods,nrow=2)
grid.arrange(Gender,Crcard,Active,Ex_stay, nrow=2)
grid.arrange(Tenure,nrow=1)
#-----------------------------------------------------------------------
# 'Geography'   -- Overall, 50% of the bank custormer's are from France
#                           approx. 25% each are from Germany and Spain
# 'NumOfProds'  -- approx. 51% of customer's are having 1 product and 
#                          46% are having 2 products with the bank
#             (Generally, the more the products, they are less likely to exit)
# 'Gender'      -- The male customers are dominant in the bank (55%)
# 'HasCrCard'   -- nearly 71% of the customers have credit cards 
#             (Generally, customers with credit cards are less likely to exit)
# 'IsActive'    -- they are almost equal in proportions (approx. 50%)
# 'Exited'      -- 20% of the customers are exited
#-------------------------- BiVariate Analysis -------------------------
# Role of Geograpy on Exit
prop.table(table(data$Geography,data$Exited),1)
geo_ext <- ggplot(data, aes(x=Geography,fill=Exited)) + geom_bar(position='fill') + 
  scale_y_continuous(labels=percent_format()) +
  geom_hline(yintercept=0.2) +
  annotate('text',x=1,y=c(0.10,0.60),label=c('16%','84%'),size=5) +
  annotate('text',x=2,y=c(0.10,0.60),label=c('32%','68%'),size=5) +
  annotate('text',x=3,y=c(0.10,0.60),label=c('17%','83%'),size=5) +
  scale_fill_manual(values=alpha(c('red','blue'),.5)) + 
  labs(title = "Geography", y = "% of Total Number of Records", x='') +
  theme_classic()

# Exit rate is quite high in Germany (32%, infact, its double compared to 
# France and Spain)
#-----------------------------------------------------------------------
# Role of Gender on Exit
prop.table(table(data$Gender,data$Exited),1)
gender_ext <- ggplot(data, aes(x=Gender,fill=Exited)) + geom_bar(position='fill') + 
  scale_y_continuous(labels=percent_format()) +
  geom_hline(yintercept=0.2) +
  annotate('text',x=1,y=c(0.13,0.60),label=c('25%','75%'),size=5) +
  annotate('text',x=2,y=c(0.13,0.60),label=c('16%','84%'),size=5) +
  scale_fill_manual(values=alpha(c('red','blue'),.5)) +
  labs(title = "Gender", y = "% of Total Number of Records", x='') +
  theme_classic()

# Percentage of male customer's leaving the bank (16%) is less compared to the 
# female customers(25%)  ==> female customer's are more likely to exit (when 
# all other things are held constant) and infact, their exit rate is higher than 
# the overall exit rate (20% -- shown with a hotizontal line)of the bank

# This is a kind of statitstical A/B test
#-----------------------------------------------------------------------
# Tenure
prop.table(table(data$Tenure,data$Exited),1)
tenure_ext <- ggplot(data, aes(x=Tenure,fill=Exited)) + geom_bar(position='fill') + 
  scale_y_continuous(labels=percent_format()) +
  geom_hline(yintercept = 0.2) +
  annotate('text',x=1,y=c(0.13,0.60),label=c('23%','77%'),size=3.5) +
  annotate('text',x=2,y=c(0.13,0.60),label=c('22%','78%'),size=3.5) +
  annotate('text',x=3,y=c(0.13,0.60),label=c('19%','81%'),size=3.5) +
  annotate('text',x=4,y=c(0.13,0.60),label=c('21%','79%'),size=3.5) +
  annotate('text',x=5,y=c(0.13,0.60),label=c('20%','80%'),size=3.5) +
  annotate('text',x=6,y=c(0.13,0.60),label=c('20%','80%'),size=3.5) +
  annotate('text',x=7,y=c(0.13,0.60),label=c('20%','80%'),size=3.5) +
  annotate('text',x=8,y=c(0.13,0.60),label=c('17%','83%'),size=3.5) +
  annotate('text',x=9,y=c(0.13,0.60),label=c('19%','81%'),size=3.5) +
  annotate('text',x=10,y=c(0.13,0.60),label=c('22%','78%'),size=3.5) +
  annotate('text',x=11,y=c(0.13,0.60),label=c('21%','79%'),size=3.5) +
  scale_fill_manual(values=alpha(c('red','blue'),.5)) +
  labs(title = "Tenure", y = "% of Total Number of Records",x='') +
  theme_classic()

# Tenure might not provide much insight as the exit percentages are 
# almost the same across the Tenure years
#-----------------------------------------------------------------------
# NumOfProducts
prop.table(table(data$NumOfProducts,data$Exited),1)
prods_ext <- ggplot(data, aes(x=NumOfProducts,fill=Exited)) + geom_bar(position='fill') + 
  scale_y_continuous(labels=percent_format()) +
  geom_hline(yintercept = 0.2) +
  annotate('text',x=1,y=c(0.05,0.90),label=c('27%','73%'),size=4) +
  annotate('text',x=2,y=c(0.05,0.90),label=c('8%','92%'),size=4) +
  annotate('text',x=3,y=c(0.05,0.90),label=c('82%','18%'),size=4) +
  annotate('text',x=4,y=c(0.05),label=c('100%'),size=4) +
  scale_fill_manual(values=alpha(c('red','blue'),.5)) +
  labs(title = "NumOfProducts", y = "% of Total Number of Records",x='') +
  theme_classic()

# Looks like there are some anamolies -- generally, we expect that if there
# are more number of products, they are less likely to leave. This is true
# with customers having 1 and 2 products (exit percentages: 27% and 8%),
# however, the exit percentages of customers with 3 and 4 products are quite
# high (82% and 100% respectively). Though it is unusal, the total no. of
# customers in those categories are also very less (266 and 60).
#-----------------------------------------------------------------------
# HasCrCard
prop.table(table(data$HasCrCard,data$Exited),1)
card_ext <- ggplot(data, aes(x=HasCrCard,fill=Exited)) + geom_bar(position='fill') + 
  scale_y_continuous(labels=percent_format()) +
  geom_hline(yintercept=0.2) +
  annotate('text',x=1,y=c(0.15,0.60),label=c('21%','79%'),size=4) +
  annotate('text',x=2,y=c(0.15,0.60),label=c('20%','80%'),size=4) +
  scale_fill_manual(values=alpha(c('red','blue'),.5)) +
  labs(title = "HasCrCard", y = "% of Total Number of Records", x='') +
  theme_classic()

# Looks like the Credit card has less (or no) impact on the exit rates
# May be not an important feature in this particular case
#-----------------------------------------------------------------------
# IsActiveMember
prop.table(table(data$IsActiveMember,data$Exited),1)
active_ext <- ggplot(data, aes(x=IsActiveMember,fill=Exited)) + geom_bar(position='fill') + 
  scale_y_continuous(labels=percent_format()) +
  geom_hline(yintercept = 0.2) +
  annotate('text',x=1,y=c(0.12,0.60),label=c('27%','73%'),size=4) +
  annotate('text',x=2,y=c(0.12,0.60),label=c('14%','86%'),size=4) +
  scale_fill_manual(values=alpha(c('red','blue'),.5)) +
  labs(title = "IsActiveMember", y = "% of Total Number of Records", x='') +
  theme_classic()

# Customer's who are not acitve - of them, 27% are left during the period of observation 
# ==> bank needs to make their customer's to be active to keep them stay
#-----------------------------------------------------------------------
tenure_ext
grid.arrange(geo_ext,prods_ext,nrow=2)
grid.arrange(gender_ext,card_ext,active_ext,nrow=2)
#-----------------------------------------------------------------------
#--------------- categorizing Numerical Variables ----------------------

data$age_cat <- cut(data$Age, breaks=seq(15,90, by=5),include.lowest = TRUE) 

age_hist <- ggplot(data,aes(age_cat)) + geom_bar(aes(y=(..count..)/sum(..count..)),fill='orange') + 
  geom_text(size=2.9,aes(y=((..count..)/sum(..count..)), 
  label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_x_discrete(labels=seq(15,90,by=5)) +
  scale_y_continuous(labels=percent_format()) + labs(y='Count_Percent', title='Age distribution',x='')
  # +
  # theme(axis.text=element_text(size=10,face='bold'),
  #       axis.title=element_text(size=14,face='bold')) 

# Almost 73% of the customers are in the age group of 25-40 ==> the bank has 
# good number of younger customers (perhaps the reason why there are more people
# with less number of products)

prop.table(table(data$age_cat,data$Exited),1)
age_hist3 <- ggplot(data,aes(age_cat,fill=Exited)) + geom_bar(aes(y=(..count..)/sum(..count..)),position='fill') + 
  #geom_text(size=2.9,aes(y=((..count..)/sum(..count..)), 
  #label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  geom_hline(yintercept=0.2) +
  scale_x_discrete(labels=seq(15,90,by=5)) +
  scale_y_continuous(labels=percent_format()) + labs(y='Count_Percent',x='Age_bin')+
  scale_fill_manual(values=alpha(c('orange','blue'),.5)) +
  annotate('text',x=1,y=c(0.60),label=c('94%'),size=3) +
  annotate('text',x=2,y=c(0.60),label=c('92%'),size=3) +
  annotate('text',x=3,y=c(0.60),label=c('92%'),size=3) +
  annotate('text',x=4,y=c(0.60),label=c('91%'),size=3) +
  annotate('text',x=5,y=c(0.60),label=c('85%'),size=3) +
  annotate('text',x=6,y=c(0.60),label=c('73%'),size=3) +
  annotate('text',x=7,y=c(0.60),label=c('53%'),size=3) +
  annotate('text',x=8,y=c(0.60),label=c('41%'),size=3) +
  annotate('text',x=9,y=c(0.60),label=c('47%'),size=3) +
  annotate('text',x=10,y=c(0.60),label=c('60%'),size=3) +
  annotate('text',x=11,y=c(0.60),label=c('82%'),size=3) +
  annotate('text',x=12,y=c(0.60),label=c('89%'),size=3) +
  annotate('text',x=13,y=c(0.60),label=c('100%'),size=3) +
  annotate('text',x=14,y=c(0.60),label=c('89%'),size=3) +
  annotate('text',x=15,y=c(0.60),label=c('94%'),size=3) +
  annotate('text',x=16,y=c(0.60),label=c('100%'),size=3) +
  theme(axis.text=element_text(size=10,face='bold'),axis.title=element_text(size=14,face='bold')) +
  theme_classic() + theme(legend.position='none')

# people in the age group of 40 - 60 (middle age group) are more prone
# to exit the bank (might be having better offers from other banks or
# might be a serious financial crisis or else the programs at the bank 
# are more benefitial to the age groups between 15-35) ==> The bank 
# might need to focus on these age groups


#age_hist
#age_hist2
grid.arrange(age_hist,age_hist3,nrow=2)
#------------------------------------------------------------------------------------
# the dual plot provides quite interesting insight:
# we have more number of customers from 25-40 age group (73%), but the customers 
# from 40-60 are more intended to exit. Also there are very less number of customers
# in the age groups of 75-90 ==> the observed anamolies
#------------------------------------------------------------------------------------
data$bal_cat <- cut(data$Balance, breaks=seq(0,260000, by=10000),include.lowest = TRUE) 

bal_hist <- ggplot(data,aes(bal_cat)) + geom_bar(aes(y=(..count..)/sum(..count..)),fill='brown') + 
  geom_text(size=2.9,aes(y=((..count..)/sum(..count..)), 
  label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_x_discrete(labels=seq(0,26,by=1)) +
  scale_y_continuous(labels=percent_format()) + labs(y='Count_Percent',x='',title='Balance distribution')
# +
#   theme(axis.text=element_text(size=10,face='bold'),
#         axis.title=element_text(size=14,face='bold'))

# Its a nice distribution except at 0 position (we can cap/ransform it though !!!). It 
# clearly indicates there are more people with zero balances 

prop.table(table(data$bal_cat,data$Exited),1)
bal_hist2 <- ggplot(data,aes(bal_cat,fill=Exited)) + geom_bar(aes(y=(..count..)/sum(..count..)),position='fill') + 
  #geom_text(size=2.9,aes(y=((..count..)/sum(..count..)), 
  #label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  geom_hline(yintercept = 0.2) +
  scale_x_discrete(labels=seq(0,26,by=1)) +
  scale_y_continuous(labels=percent_format()) + labs(y='Count_Percent')+
  scale_fill_manual(values=alpha(c('brown','blue'),.5)) +
  annotate('text',x=1,y=c(0.70),label=c('86%'),size=3) +
  annotate('text',x=2,y=c(0.70),label=c('67%'),size=3) +
  annotate('text',x=3,y=c(0.70),label=c('37%'),size=3) +
  annotate('text',x=4,y=c(0.70),label=c('77%'),size=3) +
  annotate('text',x=5,y=c(0.70),label=c('67%'),size=3) +
  annotate('text',x=6,y=c(0.70),label=c('77%'),size=3) +
  annotate('text',x=7,y=c(0.70),label=c('76%'),size=3) +
  annotate('text',x=8,y=c(0.70),label=c('82%'),size=3) +
  annotate('text',x=9,y=c(0.70),label=c('81%'),size=3) +
  annotate('text',x=10,y=c(0.70),label=c('79%'),size=3) +
  annotate('text',x=11,y=c(0.70),label=c('74%'),size=3) +
  annotate('text',x=12,y=c(0.70),label=c('71%'),size=3) +
  annotate('text',x=13,y=c(0.70),label=c('75%'),size=3) +
  annotate('text',x=14,y=c(0.70),label=c('75%'),size=3) +
  annotate('text',x=15,y=c(0.70),label=c('77%'),size=3) +
  annotate('text',x=16,y=c(0.70),label=c('78%'),size=3) +
  annotate('text',x=17,y=c(0.70),label=c('80%'),size=3) +
  annotate('text',x=18,y=c(0.70),label=c('77%'),size=3) +
  annotate('text',x=19,y=c(0.70),label=c('74%'),size=3) +
  annotate('text',x=20,y=c(0.70),label=c('75%'),size=3) +
  annotate('text',x=21,y=c(0.70),label=c('43%'),size=3) +
  annotate('text',x=22,y=c(0.70),label=c('55%'),size=3) +
  annotate('text',x=23,y=c(0.70),label=c('50%'),size=3) +
  annotate('text',x=24,y=c(0.70),label=c('0%'),size=3) +
  annotate('text',x=25,y=c(0.70),label=c('0%'),size=3) +
 # annotate('text',x=26,y=c(0.60),label=c('0%'),size=3) +
  theme(axis.text=element_text(size=10,face='bold'),axis.title=element_text(size=14,face='bold')) +
  theme_classic() + theme(legend.position='none')

bal_hist
bal_hist2
#grid.arrange(bal_hist,bal_hist2,norw=2)

# Customers who have low and high balances are leaving more
# compared to the customers with medium range balances

#--------------------------------------------------------------------------

