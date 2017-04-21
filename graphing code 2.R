############# Coding Walkthrough #########

# things to cover:
# cleaning data
# histograms and boxplots and normality
# EDA with ggplot
# correlations
# regression assumptions
# regression
# interaction term - interaction plot
# regression transformations
# different regressions
# notes for next time
# exploration on classification and cluster analysis
# via proabbility



# Hi Everyone, this is a much cleaner version of what I have done
# Please feel free to recycle some of the code, tools, 
# packages, and what not for your analysis.

## First thing we're gonna do is load up some of the packages
# think of packages as open source add ons for R. Someone wrote these
# packages to do a function and released it for others to build upon.

# You only need to install the packages once, then to load it up you 
# type the command 
# require(package) 
# OR
# library(package)
# to first install the package typing 
# install.packages("packagename")

# 
# #### installations
# install.packages("stringr") # this helps with replacing strings in the data set
# install.packages("dplyr") # this helps with restructuring data
# install.packages("tidyr") # similair function to dplyr
# install.packages("doBy") # helps with 
# install.packages("DataCombine") # AMAZING pacakge for cleaning
# install.packages("readr")
# install.packages("data.table")
# install.packages("dplyr")
# 
# # data graphing
# install.packages("ggplot2") # helps with graphing
# install.packages("vioplot") # allows you to do violin plots
# install.packages("corrplot") # helps create plots for correlations
# install.packages("qgraph")
# 
# # data analytics
# install.packages("car") # for vif
# install.packages("PerformanceAnalytics") # helps with the overall correltion
# install.packages("Hmisc") # gets significance for the correlations 
# install.packages("lawstat") # helps verify some metrics of a LM
# install.packages("lmtest") # for LM
# install.packages("gvlma") # helps with checking assumptions of LM
# install.packages("stats") #LM
# install.packages("stats4") #LM
# install.packages("MASS") # lM
# install.packages("mvtnorm")
# install.packages("caret") #
# install.packages("Metrics")


# now to load up the package
############ packages #########

# data manipulation
library(stringr) # this helps with replacing strings in the data set
library(dplyr) # this helps with restructuring data
library(tidyr) # similair function to dplyr
library(doBy) # helps with 
library(DataCombine) # AMAZING pacakge for cleaning
library(readr)
library(data.table)
library(dplyr)

# data graphong
library(ggplot2) # helps with graphing
library(vioplot) # allows you to do violin plots
library(corrplot) # helps create plots for correlations

# data analytics
library(car) # for vif
library(PerformanceAnalytics) # helps with the overall correltion
library(Hmisc) # gets significance for the correlations 
library(lawstat) # helps verify some metrics of a LM
library(lmtest) # for LM
library(gvlma) # helps with checking assumptions of LM
library(stats) #LM
library(stats4) #LM
library(MASS) # lM
library(mvtnorm)
library(caret) # 
library(Metrics) #Measures of prediction error:mse, mae

############ loading data #########
df1 <- read.csv("cleaned.csv")
df1$X <- NULL
# adding rev.emp below - revenue/employees
df1$rev.emp <- df1$bq_revenue/df1$bq_current_employees_plan
# adding proft.margin - ebitda/revenue
df1$prof.margin <- df1$bq_ebitda/df1$bq_revenue


########## Defining what we have ##############
#independent variables (want to predict): 
#Employment, Revenue, EBITDA, BQ Growth Score, 
#BQ Risk Score, BQ Profit Score, BQ Growth Total Contribution to pension (proxy for revenue),
#BQ Growth Company Contribution to pension (profitability), bq emp growth rate

#dependent variables [dependent variable] (things you know or can find out) 
#global rank, country, country rank, 
#Web Registration info, Creation year, cateogry, category rank
#Public, and Industry


######### Look at what we got ########
names(df1)
# looking at this we are not gonna be using many of these
# tell it to extract the columns [rows, columns]
df1.5 <- df1[,c(1,3,15,17:29,35:37,40:44)]

######## Side note, you can also use the select function from dplyer
#df1.4 <- select(df1, name1, name2, name3, )
#### end of side note

############ Quickly Checking Data #########
# i like to view the data and play with the tabs to see if everything is good
summary(df1.5)

################## WEBSITE CHECKING ###########
# look at resutls, 
#you got some off things, 
#for one look at bq)_website
head(sort(table(df1.5$bq_website), decreasing = TRUE), 130)

# when I was originally wrote the data cleaning script, I did not account 
# for companies with different divisions but having the same name.
# looking at the data, there are a couple of things that need bto be fixxed
# 1) get rid of NANA 
# 2) there are companies that are parent of another.
# 3) 

############ NANA PROBLEM ###############

# first doing the NANA, you got 2262 of them in there. 
#original <- readRDS("originaldata.RDS")
# looking at original data set
# checked on Biz QUalify end, they are clean
# okay looking at the original dataset, it seems like this is a software, excel problem and it just got carried over
# because this repeats for the correct address companies 
# I'll have to go back and make some edits to the script.
# For the most part this was harmless


# to do this gotta change it back into character
df1.5$bq_website <- as.character(df1.5$bq_website)
sum(str_detect(df1.5$bq_website, "NANA"), na.rm = TRUE)
sum(str_detect(df1.5$bq_website, "http:NANA"), na.rm = TRUE) 
# so 10 have NANA but not http:NANA
# I'll have to look into those

for(i in 1:length(df1.5$bq_website)){
  df1.5$bq_website[i] <- str_replace(df1.5$bq_website[i], "http:NANA", "http:")}

sum(str_detect(df1.5$bq_website, "NANA"), na.rm = TRUE)
#funny enough those are https:NANA

for(i in 1:length(df1.5$bq_website)){
  df1.5$bq_website[i] <- str_replace(df1.5$bq_website[i], "https:NANA", "https:")}
# seems a pitty doing a for loop for 10 of them


######## Duplicate company sites #########
head(sort(table(df1.5$bq_website), decreasing = TRUE), 130) 
# sorted it out the tabled names 
# head returns the first many of something, in this case, I asked for 130


sum(sort(table(df1.5$bq_website), decreasing = TRUE)[1:128]) # this shows the number of the duplicated
sum(duplicated(df1.5$bq_website)) # This is counting how many are duplicated
length(unique(df1.5$bq_website)) # This matches the duplicate 
# so there are 262 names repeated 1951 times throughout this dataset?
# this is weird... Let me investigate
# okay found out that it counted the NA's as duplicates
# let's check this out
sum(is.na(df1.5$bq_website)) # 1818 duplicates because of NA
# there should be 133 left
# half of 262 ~ 133 so were good, had one straggle, its fine


# create the names 
rep.names <- names(sort(table(df1.5$bq_website), decreasing = TRUE)[1:128]) # this shows the names of the duplicated
rep.df <- df1.5[df1.5$bq_website %in% rep.names,]
# View(rep.df)
# so what this does is it puts all the names of the repeated in a list, 
# and creates a new variable called rep.df that has only those repeated companies from 
# the original dataframe


# So looking at the data set, many of them are the same.
# R couldn't pick it up the first time because of name specific things like
# llc vs inc. vs regular. R thought they were all different companies


# what i am going to do is to create a loop
# for every website with similair links, I am gonna tell it to create a subet
# find the one with the minimum global rank, and take that one. 
# I choose minimum global rank as a reference, because if you have companies
# that are within each other the one with the minimum global rank 
# should be more successful therefore, own the other. 
# i admit this logic is not fullproff, but it does give a way to clean some of 
# of the repeated data. Some companies will be lost because of this, but I think 
# it would be worth it to get rid of some of these excess companies that are subgroups of another

cleaned <- c() # empty data frame
for(i in 1:length(rep.names)){
  disposable.subset <- subset(rep.df, bq_website == rep.names[i])
  number.keep <- which.min(disposable.subset$global.rank) 
  cleaned <- rbind.data.frame(cleaned, disposable.subset[number.keep,]) # keep adding it to the list
}
dim(cleaned) # 124


names.keep <- !(df1.5$bq_website %in% rep.names) # keep ones not in duplicated names
# 10393 - 10131 = 262 So this makes sense cause we were working with 262 repeated
# adding the cleaned back in 
df1.7 <- rbind.data.frame(df1.5[names.keep,], cleaned)
df1.7$bq_website <- as.factor(df1.7$bq_website)

##### Exploring assets  ######
df2 <- df1.7 # just gonna do whole numbers from now on
summary(df2)
# couple of quick adjustments

df2$Public <- as.factor(df2$Public)
df2$web.private <- as.factor(df2$web.private)

# legal name good
# industry good, we'll have to group them up later on
head(sort(summary(df2$bq_industry_name)), 10) # these are too small on own

# website good
# current employees is exxtremely skewed
# but that is expected, WMT employees 1.5 Million, MCD employees 420K
# https://en.wikipedia.org/wiki/List_of_largest_employers_in_the_United_States


# growth rate looks normal
# profiability score looks slightly skewed left
# risk score looks normal
# bq_net_assets_pens_eoy: how can a company have negative retirement plan?

# Retirement plan net assets == bq_net_assets_pens_eoy
#View(df2[which(df2$bq_net_assets_pens_eoy < 0),])
df2[which(df2$bq_net_assets_pens_eoy < 1),] # show the zeros also
# See Jose for explanation

######### no earnings ########
summary(df2)

# select --> which columns
# also the reason its dplyr::select( is because another package has
# select and this way specify which package

df2 %>% filter(bq_revenue == 0) # just company with 0 revenue

df2 %>% filter(bq_revenue == 0) %>% # filter --> says what condition
  dplyr::select(bq_company_legal_name, bq_industry_name, Public)

df2 %>% dplyr::select(bq_company_legal_name, bq_industry_name, bq_revenue, bq_ebitda, rev.emp, prof.margin) %>% arrange(bq_industry_name)
(df2 %>% dplyr::mutate(proxy = bq_growth_tot_contrib_pens_amt_a/bq_current_employees_plan) %>% dplyr::select(bq_company_legal_name, bq_industry_name, bq_current_employees_plan, bq_revenue, bq_ebitda, rev.emp, prof.margin, Public) %>% arrange(bq_industry_name)) 
#View(df2 %>% dplyr::mutate(proxy = bq_growth_tot_contrib_pens_amt_a/bq_current_employees_plan) %>% dplyr::select(bq_company_legal_name, bq_industry_name, bq_current_employees_plan, bq_growth_tot_contrib_pens_amt_a, bq_revenue, bq_ebitda, rev.emp, prof.margin, Public, proxy) %>% arrange(bq_industry_name)) 
(df2 %>% dplyr::mutate(proxy = bq_growth_tot_contrib_pens_amt_a/bq_current_employees_plan) %>% dplyr::select(bq_company_legal_name, bq_industry_name, bq_current_employees_plan, bq_growth_tot_contrib_pens_amt_a, bq_revenue, bq_ebitda, rev.emp, prof.margin, Public, proxy) %>% arrange(desc(proxy)) )

# anyway, looking at the data, we can see 4/5 0 revenue companies are
# phamaceutical last one is telecom
# the drug companies can probably be explained by research
# telecom is hard to understand, based off its negative vairables
# in other columns in seems to be doing pretty badly 
# anyway, how to alter this for dataset...

summary(df2$bq_revenue) # get summary
summary(filter(df2,bq_revenue > 0) %>% dplyr::select(bq_revenue)) 
# summary looks kinda normal if take out these 5 companies
hist(filter(df2,bq_revenue > 0) %>% dplyr::select(bq_revenue))
# original skeweness
hist(log(filter(df2,bq_revenue > 0) %>% dplyr::select(bq_revenue)))
# with log, distribution looks normal 


logged.rev <- log(filter(df2, bq_revenue > 0) %>% dplyr::select(bq_revenue))
# gonna run the shapiro test to check if its normal
shapiro.test(sample(unlist(logged.rev), 5000)) # can only go up to 5000
# if it's singificant that means not normal
boxplot(logged.rev) # yes, this shows that not normal
sum(logged.rev > 22) # want to see how many above 22 because of IQR
sum(logged.rev < 15) # want to see how many below 15 because of IQR
# have 217 above 22
# have 29 below it 


# this is a problem....

######## SIde Notes ############


df2 %>% dplyr::select(bq_company_legal_name, bq_industry_name, bq_revenue, bq_ebitda, rev.emp, prof.margin) %>% arrange(bq_industry_name)
(df2 %>% dplyr::mutate(proxy = bq_growth_tot_contrib_pens_amt_a/bq_current_employees_plan) %>% dplyr::select(bq_company_legal_name, bq_industry_name, bq_current_employees_plan, bq_revenue, bq_ebitda, rev.emp, prof.margin, Public) %>% arrange(bq_industry_name)) 
#View(df2 %>% dplyr::mutate(proxy = bq_growth_tot_contrib_pens_amt_a/bq_current_employees_plan) %>% dplyr::select(bq_company_legal_name, bq_industry_name, bq_current_employees_plan, bq_growth_tot_contrib_pens_amt_a, bq_revenue, bq_ebitda, rev.emp, prof.margin, Public, proxy) %>% arrange(bq_industry_name)) 
(df2 %>% dplyr::mutate(proxy = bq_growth_tot_contrib_pens_amt_a/bq_current_employees_plan) %>% dplyr::select(bq_company_legal_name, bq_industry_name, bq_current_employees_plan, bq_growth_tot_contrib_pens_amt_a, bq_revenue, bq_ebitda, rev.emp, prof.margin, Public, proxy) %>% arrange(desc(proxy)) )


#### Age Oldest Plan ######
summary(df2)
# the oldest plan is weird, looking at it, you have some company that is 114
df2 %>% filter(bq_age_oldest_plan > 100)
# manually looked up the public ones, only Verizon doesn't belong
# has some mistakes

####### Pension and Public #######
df2$bq_comp_contribute_pens_ind <- as.factor(df2$bq_comp_contribute_pens_ind)
summary(df2 %>% dplyr::select(bq_comp_contribute_pens_ind, Public))

##### TOTAL Contribution and COmpany contribution #####
summary(df2 %>% dplyr::select(bq_growth_tot_contrib_pens_amt_a, bq_growth_co_contrib_pens_amt_a))
# bq_growth_tot_contrib_pens_amt_a 
# The 3-year CAGR growth rate of employee plus company contributions to retirement plans, which is an indicator of revenue.
# bq_growth_co_contrib_pens_amt_a
# The 3-year CAGR growth rate of company contributions to retirement plans, which is an indicator of profitability.

# these two are in percentages, don't forget. I mean it looks right
# As for normalizing it, its pretty skewed 
# Your gonna half to group them into factors for bq_growth_tot_contrib_pens_amt_a 
# like -100 to -50, -50 to 0, 0 to 50, 50 to 100, 100 +

# Similairly with bq_growth_co_contrib_pens_amt_a 
# like -100 to -50, -50 to 0, 0 to 50, 50 to 100, 100 +

# How to cut https://www.youtube.com/watch?v=EWs1Ordh8nI
df2$factor.total <- cut(df2$bq_growth_tot_contrib_pens_amt_a, breaks = c(-100,-50,0,50,100, 14270), labels = c("-100:-50", "-50:0", "0:50", "0:100", "100+"))
table(df2$factor.total)

max(df2$bq_growth_co_contrib_pens_amt_a, na.rm =TRUE)
df2$factor.co <- cut(df2$bq_growth_co_contrib_pens_amt_a, breaks = c(-100,-50,0,50,100, 5925), labels = c("-100:-50", "-50:0", "0:50", "0:100", "100+"))
table(df2$factor.co)




###### Ranks #######
summary(df2$global.rank)
inverse.rank <- 1/df2$global.rank
summary(inverse.rank)
hist(inverse.rank)
boxplot(inverse.rank) # now that looking at it, you can see there is a pretty steep penalty
# if your the tenth most visited site your gonna be in 0.1
# possibly log it?

sum(df2$global.rank < 100, na.rm = TRUE) # there's only 15 under 
# I can also try to split it by percentiles. I can try to have 8 - 10 % to keep robustness

library(lsr)
df2$quant.global.rank <- quantileCut(df2$global.rank, 5) # you can do this


# I personally like breaking it up by 
# so from log 0-5, log 5 - 10, 10-15, 15 - 20

df2$g.rank.cat <- cut(log(df2$global.rank), breaks = c(0,9,11,13,15,20), labels = c("exp[0:9]", "exp(9:11]", "exp(11,13]", "exp(13,15]", "exp(15+)"))
table(df2$g.rank.cat)

df2$c.rank.cat <- cut(log(df2$country.rank), breaks = c(0,5,7,9,12,20), labels = c("exp[0:5]", "exp(5:7]", "exp(7:9]", "exp(9:12]", "exp(12+)"))
table(df2$c.rank.cat)

###### testing wondering ########
summary(df2)
df2$inverse.rank <- 1/df2$global.rank

############# Self note: this would be a perfect random forest problem, NN, ORdinal Regression ###############
chisq.test(df2$g.rank.cat,df2$bq_industry_name)
# try multinomial, random forest, nn for prediction
# things that might be useful: rank, industry, year, country
############# for nn, you have rankings, industry, possibly year, country #########


############ playing with profiability score - cutting ################
df2$factor.profit <- cut(df2$bq_profitability_score , breaks = c(0,250,500,750,1000), labels = c("0:250", "250:500", "500:750", "750:1000"))
test.df2 <- c()
#test.df2$factor.profit <- cut(test.df2$bq_profitability_score , breaks = c(0,250,500,750,1000), labels = c("0:250", "250:500", "500:750", "750:1000"))

table(df2$factor.profit)
#chisq.test(test.df2$factor.profit, test.df2$bq_industry_name)

# try multinomial, random forest, nn for prediction
# things that might be useful: rank, industry, year, country
#(test.df2$factor.profit ~ test.df2$bq_industry_name*test.df2$g.rank.cat)
######### END ############
######## wrapping up the cleaning #######
summary(df2)

####### Doing the industries ######### 
summary(df2)
# so I didn't label each one from last time, I think it would be best if I used dput
tabled.result <- data.frame(table(df2$bq_industry_name))
tabled.result %>% arrange(Freq)

# I'll just arbitrarily cut it at 70
names.want <- tabled.result %>% filter(Freq > 70) 
df2.reduced <- data.frame(c())
names.want <- as.character(names.want[,1])
for(i in 1:length(names.want)){
  add <- df2[which(df2$bq_industry_name == names.want[i]),]
  df2.reduced <- rbind.data.frame(df2.reduced, add)
}
str(df2.reduced) # about 1000 dropped

######## Classification #######

#par(mar=c(4,4.5,2,1))
#par(oma=c(0,0,0,0))
# get can save your life with shiny
barplot(table(df2$Public, df2$factor.profit), beside=T, cex.names=0.7, args.legend=list(x=12,y=25,cex=0.8), col=c("pink","light blue"))
barplot(table(df2$Public, df2$factor.total), beside=T, cex.names=0.7, args.legend=list(x=12,y=25,cex=0.8), col=c("pink","light blue"))
chisq.test(df2$factor.profit, df2$g.rank.cat) # possible divide by % percentiles
# or by industry
chisq.test(df2$factor.profit, df2$quant.global.rank)
table(df2$factor.profit, df2$quant.global.rank)

df2.reduced$bq_industry_name <-  droplevels(df2.reduced$bq_industry_name)
#ggplot(data = df2, aes(x = bq_current_employees_plan, y = bq_profitability_score, col = quant.global.rank))
#ggplot(data = df2, aes(x = log(global.rank), y = bq_profitability_score, col = g.rank.cat)) + geom_point()
ggplot(data = df2.reduced, aes(x = log(global.rank), y = bq_profitability_score, col = bq_industry_name)) + geom_point()

quick.data <- df2.reduced
quick.data$orig.bq_industry_name <- quick.data$bq_industry_name <- as.character(quick.data$bq_industry_name)
for(i in 1:length(quick.data$bq_industry_name)){
  if(str_detect(quick.data$bq_industry_name[i], "car") == TRUE){
    quick.data$bq_industry_name[i] <- "car.dealers"
  }else if(str_detect(quick.data$bq_industry_name[i], "clothing") == TRUE){
    quick.data$bq_industry_name[i] <- "clothing"
  }else if(str_detect(quick.data$bq_industry_name[i], "manufacturing") == TRUE){
    quick.data$bq_industry_name[i] <- "manufacturing"
  }else if(str_detect(quick.data$bq_industry_name[i], "restuar") == TRUE){
    quick.data$bq_industry_name[i] <- "restuarant"
  }
}
for(i in 1:length(quick.data$bq_industry_name)){
  if((quick.data$bq_industry_name[i] %in% 
      c("activities related to credit intermediation (including loan brokers check clearing & money transmitting)",
        "all other nondepository credit intermediation", "banking",
        "credit card issuing", "direct insurance (except life health & medical) carriers", 
        "commodity contracts brokerage", "credit unions",
        "commodity contracts dealing", "insurance", "investment banking & securities dealing", 
        "offices of bank holding companies","other financial investment activities (including portfolio management & investment advice)",
        "securities brokerage", "insurance agencies & brokerages", "commercial banking")) == TRUE)
  {quick.data$bq_industry_name[i] <- "finance"
  }else if((quick.data$bq_industry_name[i] %in% c("beauty salons", "car.dealers", "clothing", "cosmetics beauty supplies & perfume stores",
                                                  "department stores", "electronics stores (including audio video computer and camera stores)", "motorcycle atv and all other motor vehicle dealers",
                                                  "food.stores", "restuarants", "motor vehicle & motor vehicle parts & supplies", "jewelry stores", "fullservice restaurants", "supermarkets and other grocery (except convenience) stores")) == TRUE)
  {quick.data$bq_industry_name[i] <- "brick.and.mortar"
  }else if((quick.data$bq_industry_name[i] %in% c("footwear manufacturing (including rubber & plastics)", "fruit & vegetable preserving & specialty food manufacturing", 
                                                  "glass & glass product manufacturing", "hard.ware.manufacturing", "mechanical.manufact", "medical equipment & supplies manufacturing",
                                                  "other chemical product & preparation manufacturing", "other food manufacturing (including coffee tea flavorings & seasonings)", "other miscellaneous manufacturing", "pharmaceutical & medicine manufacturing", 
                                                  "soap cleaning compound & toilet preparation manufacturing", 
                                                  "soft drink & ice manufacturing", "sugar & confectionery product manufacturing")) == TRUE)
  {quick.data$bq_industry_name[i] <- "manufacturing"
  }else if((quick.data$bq_industry_name[i] %in% c("architectural services", "management scientific & technical consulting services", "marketing research & public opinion polling",
                                                  "offices of certified public accountants", "offices of lawyers", "special food services (including food service contractors & caterers)", 
                                                  "specialized design services (including interior industrial graphic & fashion design)", "tax preparation services", "investigation & security services", "scientific research & development services", "all other professional scientific & technical services")) == TRUE)
  {quick.data$bq_industry_name[i] <- "service"
  }else if((quick.data$bq_industry_name[i] %in% c("computer services", "data processing hosting & related services", "software publishers", "telecommunications (including paging cellular satellite cable & other program distribution resellers other telecommunications & internet service providers)", "other computer related services","electronic shopping & mailorder houses", 
                                                  "custom computer programming services", "computer systems design services", "computer facilities management services")) == TRUE)
  {quick.data$bq_industry_name[i] <- "tech"
  }else if((quick.data$bq_industry_name[i] %in% c("casino hotels", "educational services (including schools colleges & universities)", "hospitals", "hotels (except casino hotels) & motels", "real estate property managers", "religious grantmaking civic professional & similar organizations (including condominium and homeowners associations)")) == TRUE)
  {quick.data$bq_industry_name[i] <- "facilities"
  }else if((quick.data$bq_industry_name[i] %in% c("sound recording industries", "motion picture & video industries (except video rental)", "newspaper publishers", "other information services (including news syndicates libraries internet publishing & broadcasting)", "radio & television broadcasting", "advertising & related services")) == TRUE)
  {quick.data$bq_industry_name[i] <- "media"
  }else if((quick.data$bq_industry_name[i] %in% c("oil & gas extraction","electric power generation transmission & distribution", "petroleum refineries (including integrated)")) == TRUE)
  {quick.data$bq_industry_name[i] <- "power"
  }else if((quick.data$bq_industry_name[i] %in% c("wineries", "beer wine & distilled alcoholic beverages")) == TRUE)
  {quick.data$bq_industry_name[i] <- "alcohol"
  }
}

######## GGPLOTTING THE SUBGROUPS ##########

#View(table(quick.data$bq_industry_name))# 10 or more places 
table(quick.data$bq_industry_name)
# I would go so far as to drop the performing arts, air transportation, and power. That way only have 500+
quick.data$orig.bq_industry_name <- as.factor(quick.data$orig.bq_industry_name)
group.data <- subset(quick.data, bq_industry_name %in% c("alcohol", "power", "media", "facilities", "tech", "service", "manufacturing", "finance", "brick.and.mortar", "air transportation", "performing arts companies"))
group.data <- subset(quick.data, bq_industry_name %in% c( "media", "facilities", "tech", "service", "manufacturing", "finance", "brick.and.mortar"))
# want more than 500

table(group.data$bq_industry_name)

ggplot(data = group.data, aes(x = log(global.rank), y = bq_profitability_score, col = bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = group.data, aes(x = log(global.rank), y = log(bq_current_employees_plan), col = bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = group.data, aes(x = log(global.rank), y = bq_growth_score, col = bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = group.data, aes(x = log(global.rank), y = bq_risk_score, col = bq_industry_name)) + geom_point() +  geom_smooth()
ggplot(data = group.data, aes(x = log(global.rank), y = log(bq_emp_growth_rate), col = bq_industry_name)) + geom_point() + geom_smooth()

# VS top ones in original

original.subset <- subset(quick.data, orig.bq_industry_name %in% c("advertising & related services", "commercial banking",
                                                                   "other information services (including news syndicates libraries internet publishing & broadcasting)",
                                                                   "data processing hosting & related services", "electronic shopping & mailorder houses", 
                                                                   "other financial investment activities (including portfolio management & investment advice)"))

head(sort(table(quick.data$orig.bq_industry_name), decreasing = TRUE), 15)
table(original.subset$bq_industry_name)

ggplot(data = original.subset, aes(x = log(global.rank), y = bq_profitability_score, col = orig.bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = original.subset, aes(x = log(global.rank), y = bq_profitability_score, col = bq_industry_name)) + geom_point() + geom_smooth()

# finance one is very interesting
ggplot(data = original.subset, aes(x = log(global.rank), y = log(bq_current_employees_plan), col = bq_industry_name)) + geom_point() + geom_point() + geom_smooth()
ggplot(data = original.subset, aes(x = log(global.rank), y = log(bq_current_employees_plan), col = orig.bq_industry_name)) + geom_point() + geom_point() + geom_smooth()


ggplot(data = original.subset, aes(x = log(global.rank), y = bq_growth_score, col = bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = original.subset, aes(x = log(global.rank), y = bq_growth_score, col = orig.bq_industry_name)) + geom_point() + geom_smooth()

ggplot(data = original.subset, aes(x = log(global.rank), y = bq_risk_score, col = bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = original.subset, aes(x = log(global.rank), y = bq_risk_score, col = orig.bq_industry_name)) + geom_point() + geom_smooth()

ggplot(data = original.subset, aes(x = log(global.rank), y = log(bq_emp_growth_rate), col = bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = original.subset, aes(x = log(global.rank), y = log(bq_emp_growth_rate), col = orig.bq_industry_name)) + geom_point() + geom_smooth()


####
####
####
####
### one can also instead take a random sample from each subgroup, so that not to overpopulate from
# first map, I will try that
sample.size <- 400
set.seed(1)
sample.finance.pop <- subset(group.data, bq_industry_name == "finance")
sample.finance <- sample.finance.pop[sample(1:nrow(sample.finance.pop), sample.size),]

sample.manufacturing.pop <- subset(group.data, bq_industry_name == "manufacturing")
sample.manufacturing <- sample.manufacturing.pop[sample(1:nrow(sample.manufacturing.pop), sample.size),]

sample.media.pop <- subset(group.data, bq_industry_name == "media")
sample.media <- sample.media.pop[sample(1:nrow(sample.media.pop), sample.size),]

sample.service.pop <- subset(group.data, bq_industry_name == "service")
sample.service <- sample.service.pop[sample(1:nrow(sample.service.pop), sample.size),]

sample.tech.pop <- subset(group.data, bq_industry_name == "tech")
sample.tech <- sample.tech.pop[sample(1:nrow(sample.tech.pop), sample.size),]

sample.data.pop <- rbind.data.frame(sample.finance, sample.manufacturing, sample.media, sample.service, sample.tech)

ggplot(data = sample.data.pop, aes(x = log(global.rank), y = bq_profitability_score, col = bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = sample.data.pop, aes(x = log(global.rank), y = log(bq_current_employees_plan), col = bq_industry_name)) + geom_point() + geom_point() + geom_smooth()
ggplot(data = sample.data.pop, aes(x = log(global.rank), y = bq_growth_score, col = bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = sample.data.pop, aes(x = log(global.rank), y = bq_risk_score, col = bq_industry_name)) + geom_point() + geom_smooth()
ggplot(data = sample.data.pop, aes(x = log(global.rank), y = log(bq_emp_growth_rate), col = bq_industry_name)) + geom_point() + geom_smooth()

######## Privacy #######
table(df2$web.creation.year)
#ggplot(data = df2, aes(x = web.creation.year, y = table(df2$web.creation.year), col = web.private)) + geom_point() 
ggplot(as.data.frame(table(df2$web.creation.year)), aes(x=Var1, y = Freq)) + geom_point()

####### Correlations  ####
df3 <- quick.data %>% filter(bq_revenue > 0) %>% filter(bq_net_assets_pens_eoy >= 0)
names(df3) <- str_replace_all(names(df3), "bq_", "")
df3$log.rank <- log(df3$global.rank)
dput(names(df3))
names(df3) <- c("company.name", "industry.name", "website", "employees", 
                "growth.score", "profit.score", "risk.score", "net_assets_pens_eoy", 
                "revenue", "ebitda", "age", "comp_contribute_pens_ind", 
                "Public", "growth_tot_contrib_pens", "growth_co_contrib_pens", 
                "emp_growth_rate", "global.rank", "country", "country.rank", 
                "web.creation.year", "web.reg.org", "web.private", "rev.emp", 
                "prof.margin", "factor.total", "factor.co", "quant.global.rank", "g.rank.cat", "c.rank.cat", 
                "inverse.rank", "factor.profit", "orig.industry_name", "log.rank")

chart.Correlation(df3[complete.cases(df3),c(4:11,16:17, 19, 20, 32)], histogram=TRUE, method = "pearson", pch = 19, main = "Pearson for All")
chart.Correlation(df3[complete.cases(df3),c(4:11,14:17, 19, 20, 32)], histogram=TRUE, method = "spearman", pch = 19, main = "Spearman for All")

df3.public.co <- df3 %>% filter(Public == 1) %>% filter(revenue > 0)
df3.private.co <- df3 %>% filter(Public == 0) %>% filter(revenue > 0)

chart.Correlation(df3.public.co[complete.cases(df3.public.co),c(4:11,16:17, 19, 20, 32)], histogram=TRUE, method = "pearson", pch = 19, cex.label = .3, main = "Pearson for Public")
chart.Correlation(df3.public.co[complete.cases(df3.public.co),c(4:11,14:17, 19, 20, 32)], histogram=TRUE, method = "spearman", pch = 19, cex.label = .3, main = "Spearman for Public")

chart.Correlation(df3.private.co[complete.cases(df3.private.co),c(4:11,16:17, 19, 20, 32)], histogram=TRUE, method = "pearson", pch = 19, cex.label = .3, main = "Pearson for Private")
chart.Correlation(df3.private.co[complete.cases(df3.private.co),c(4:11,14:17, 19, 20, 32)], histogram=TRUE, method = "spearman", pch = 19, cex.label = .3, main = "Spearman for Private")


df3.public.co <- df3 %>% filter(Public == 1) %>% filter(revenue > 0)
df3.private.co <- df3 %>% filter(Public == 0) %>% filter(revenue > 0)

reg.public.rev <- lm(df3.public.co$revenue ~ df3.public.co$log.rank*df3.public.co$orig.industry_name); summary(reg.public.rev); plot(reg.public.rev)
reg.private.rev <- lm(df3.private.co$revenue ~ df3.private.co$employees*df3.private.co$orig.industry_name); summary(reg.private.rev)
reg.public.ebitda <- lm(df3.public.co$ebitda ~ df3.public.co$employees*df3.public.co$orig.industry_name); summary(reg.public.ebitda)
reg.private.ebitda <- lm(df3.private.co$ebitda ~ df3.private.co$employees*df3.private.co$orig.industry_name); summary(reg.private.ebitda)

log.reg.public.rev <- lm(log(df3.public.co$revenue) ~ df3.public.co$log.rank + df3.public.co$orig.industry_name); summary(log.reg.public.rev); plot(log.reg.public.rev)
log.reg.private.rev <- lm(log(df3.private.co$revenue) ~ df3.private.co$log.rank*df3.private.co$orig.industry_name); summary(log.reg.private.rev)
log.reg.public.ebitda <- lm(log(df3.public.co$ebitda) ~ df3.public.co$log.rank*df3.public.co$orig.industry_name); summary(log.reg.public.ebitda)
log.reg.private.ebitda <- lm(log(df3.private.co$ebitda) ~ df3.private.co$log.rank*df3.private.co$orig.industry_name); summary(log.reg.private.ebitda)


old <- lm(df3$age_oldest_plan ~ df3$web.creation.year*df3$orig.industry_name)
summary(old)

vif(log.reg.public.rev)

##### Checking different linear models #######

growth.lm <- lm(df3$growth.score ~ df3$log.rank + df3$web.creation.year); summary(growth.lm); plot(growth.lm)
profit.lm <- lm(df3$profit.score ~ df3$log.rank + df3$web.creation.year); summary(profit.lm); plot(profit.lm)
employees.lm <- lm(log(df3$employees) ~ df3$log.rank + df3$web.creation.year); summary(employees.lm); plot(employees.lm)
risk.lm <- lm(df3$risk.score ~ df3$log.rank + df3$web.creation.year); summary(risk.lm); plot(risk.lm)
df3.no.zero <- df3 %>% filter(net_assets_pens_eoy > 0) %>% filter(net_assets_pens_eoy < 1e+10)
net.asset.lm <- lm(log(df3.no.zero$net_assets_pens_eoy) ~ df3.no.zero$log.rank*df3.no.zero$orig.industry_name + df3.no.zero$web.creation.year); summary(net.asset.lm); plot(net.asset.lm)
emp.growth.lm <- lm(df3$emp_growth_rate ~ df3$log.rank + df3$web.creation.year); summary(emp.growth.lm); plot(emp.growth.lm)
age.lm <- lm(df3$age_oldest_plan ~ df3$web.creation.year); summary(age.lm); plot(age.lm)

gvlma(emp.growth.lm)

chart.Correlation(df3.public.co[complete.cases(df3.public.co),c(4:11,16:17, 19, 20, 32)], histogram=TRUE, method = "pearson", pch = 19, cex.label = .3, main = "Pearson for Public")
chart.Correlation(df3.public.co[complete.cases(df3.public.co),c(4:11,14:17, 19, 20, 32)], histogram=TRUE, method = "spearman", pch = 19, cex.label = .3, main = "Spearman for Public")

chart.Correlation(df3.private.co[complete.cases(df3.private.co),c(4:11,16:17, 19, 20, 32)], histogram=TRUE, method = "pearson", pch = 19, cex.label = .3, main = "Pearson for Private")
chart.Correlation(df3.private.co[complete.cases(df3.private.co),c(4:11,14:17, 19, 20, 32)], histogram=TRUE, method = "spearman", pch = 19, cex.label = .3, main = "Spearman for Private")




rev.ebitda.emp.list <- c("advertising & related services", # 25-50
"all other professional scientific & technical services", # 25-50
"data processing hosting & related services", # 25-50
"electronic shopping & mailorder houses", # 25+
"direct life health & medical insurance carriers", # 25+
"footwear manufacturing (including rubber & plastics)", #25+
"medical equipment & supplies manufacturing", # 25+
"other chemical product & preparation manufacturing", # 25+
"other information services (including news syndicates libraries internet publishing & broadcasting)", # 25+
"other financial investment activities (including portfolio management & investment advice)", # 25 +
"fullservice restaurants", # 25+
"management scientific & technical consulting services", # 25+
"semiconductor & other electronic component manufacturing", # 25 +
"securities brokerage", # 25 +
"other computer related services", # 25 + 
"software publishers", # 25 +
"commercial banking", # 50+
"computer & peripheral equipment manufacturing", # 50+
"computer systems design services", # 50+
"credit unions", # 50+ 
"direct insurance (except life health & medical) carriers", # 50+ 
"offices of certified public accountants", # 50+
"offices of lawyers", # 50+
"soap cleaning compound & toilet preparation manufacturing", # 50+
"supermarkets and other grocery (except convenience) stores",  # 50+
"telecommunications (including paging cellular satellite cable & other program distribution resellers other telecommunications & internet service providers)", # 50+
"women's clothing stores" # 50 +
)



assets.list <- c("all other professional scientific & technical services", # .50+
            "commercial banking", #50+
            "computer & peripheral equipment manufacturing", #50+
            "computer systems design services", # 50+
            "credit unions", # 50+
            "offices of certified public accountants", # 50 +
            "offices of lawyers", # 50 + 
            "telecommunications (including paging cellular satellite cable & other program distribution resellers other telecommunications & internet service providers)", # 50+
            "supermarkets and other grocery (except convenience) stores", # 50+
            "soap cleaning compound & toilet preparation manufacturing", # 50+
            "medical equipment & supplies manufacturing", #50 +
            "educational services (including schools colleges & universities)", # 50+
            "women's clothing stores", # 50+
            "direct insurance (except life health & medical) carriers", # 25+ 
            "direct life health & medical insurance carriers", # 25+
            "hotels (except casino hotels) & motels", #25+
            "management scientific & technical consulting services", #25+
            "other computer related services", # 25+
            "other financial investment activities (including portfolio management & investment advice)", # 25 + 
            "semiconductor & other electronic component manufacturing", # 25 +
            "software publishers" # 25 +
            )

risk.profit.list <- c( 
"all other professional scientific & technical services", #25+
"commercial banking", # 25+
"computer & peripheral equipment manufacturing", #25+
"credit unions", #25+
"educational services (including schools colleges & universities)", # 25+
"hotels (except casino hotels) & motels", # 25+
"motion picture & video industries (except video rental)", #25 + 
"semiconductor & other electronic component manufacturing", # 25 +
"soap cleaning compound & toilet preparation manufacturing", # 25 +
"software publishers", # 25 +
"supermarkets and other grocery (except convenience) stores", # 25 +
"telecommunications (including paging cellular satellite cable & other program distribution resellers other telecommunications & internet service providers)" #25+
)


finance.borrow.list <- c("credit unions", # 50+ 
                         "direct insurance (except life health & medical) carriers", # 50+ 
                         "commercial banking", # 50+
                         "securities brokerage", # 25 +
                         "other financial investment activities (including portfolio management & investment advice)", # 25 +
                         "direct life health & medical insurance carriers" # 25+
                         )


brick.and.mortar.borrow.list <- c("women's clothing stores", "supermarkets and other grocery (except convenience) stores",
                                  "fullservice restaurants", # 25+
                                  "hotels (except casino hotels) & motels", #25+
                                  "educational services (including schools colleges & universities)"
                                  )

manufacturing.borrow.list <- c("soap cleaning compound & toilet preparation manufacturing",
                               "semiconductor & other electronic component manufacturing", # 25 +
                               "other chemical product & preparation manufacturing", # 25+
                               "medical equipment & supplies manufacturing", # 25+
                               "footwear manufacturing (including rubber & plastics)", #25+
                               "computer & peripheral equipment manufacturing")

service.borrow.list <- c("offices of certified public accountants", # 50+
                         "offices of lawyers", # 50+
                         "management scientific & technical consulting services", # 25+
                         "all other professional scientific & technical services" # 25-50
                         )

tech.borrow.list <- c("telecommunications (including paging cellular satellite cable & other program distribution resellers other telecommunications & internet service providers)",
                      "computer & peripheral equipment manufacturing", # 50+
                      "computer systems design services", # 50+
                      "other computer related services", # 25 + 
                      "software publishers", # 25 +
                      "data processing hosting & related services" # 25-50
                      )

media.borrow.list <- c("other information services (including news syndicates libraries internet publishing & broadcasting)", # 25+
                       "electronic shopping & mailorder houses", # 25+
                       "advertising & related services", # 25-50,
                       "motion picture & video industries (except video rental)")

df3.gg.original <- df3 %>% filter(orig.industry_name != "all")
combined.list <- c(finance.borrow.list, brick.and.mortar.borrow.list, 
                   manufacturing.borrow.list, service.borrow.list, tech.borrow.list,
                   media.borrow.list)
df3.gg.chosen <- df3.gg.original[df3.gg.original$orig.industry_name %in% combined.list,]

for(i in 1:length(df3.gg.chosen$orig.industry_name)){
  if((df3.gg.chosen$orig.industry_name[i] %in% 
      finance.borrow.list) == TRUE){df3.gg.chosen$industry.name[i] <- "finance"
  }else if((df3.gg.chosen$orig.industry_name[i] %in% brick.and.mortar.borrow.list) == TRUE)
  {df3.gg.chosen$industry.name[i] <- "brick.and.mortar"
  }else if((df3.gg.chosen$orig.industry_name[i] %in% manufacturing.borrow.list) == TRUE)
  {df3.gg.chosen$industry.name[i] <- "manufacturing"
  }else if((df3.gg.chosen$orig.industry_name[i] %in% service.borrow.list) == TRUE)
  {df3.gg.chosen$industry.name[i] <- "service"
  }else if((df3.gg.chosen$orig.industry_name[i] %in% tech.borrow.list) == TRUE)
  {df3.gg.chosen$industry.name[i] <- "tech"
  }else if((df3.gg.chosen$orig.industry_name[i] %in% media.borrow.list) == TRUE)
  {df3.gg.chosen$industry.name[i] <- "media"}
}
df3.gg.chosen <- droplevels(df3.gg.chosen)
df3.gg.chosen <- df3.gg.chosen %>% filter(ebitda > 1)


df3.gg.chosen$log.rev <- log(df3.gg.chosen$revenue)
df3.gg.chosen$log.ebitda <- log(df3.gg.chosen$ebitda)
df3.gg.chosen$log.asset <- log(df3.gg.chosen$net_assets_pens_eoy)
df3.gg.chosen$log.emp <- log(df3.gg.chosen$employees)

df3.gg.chosen <- df3.gg.chosen %>% dplyr::select(industry.name, employees:ebitda, global.rank, orig.industry_name, log.rank, Public, log.rev, log.ebitda, log.asset, log.emp) 
df3.gg.chosen[which(df3.gg.chosen$net_assets_pens_eoy == 0),"net_assets_pens_eoy"] <- 1 # for plotting purposes don't want to get NaN
df3.gg.chosen <- df3.gg.chosen %>% filter(net_assets_pens_eoy < 1e8) %>% filter(employees < 10000) %>% filter(revenue < 1e10) 
### END OF BEFORE 2ND GG #######

library(shiny)
ui2 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("choose.ranks", "Choose the X Axis", c("global.rank", "log.rank")),
      radioButtons("y.ggplot", "Choose the Y Axis", names(df3.gg.chosen)[c(2:8,13:16)]),
      checkboxGroupInput("industry.list", "Check the Industries Want",
            choices = names(table(df3.gg.chosen$orig.industry_name)))
        ),
  mainPanel(
    tabsetPanel(
      tabPanel("Gplot",
               plotOutput("GGplot"),
               plotOutput("ggplot.group")),
      tabPanel("Violin Plot",
               plotOutput("vioplot"),
               plotOutput("boxplot")),
      tabPanel("Growth.Profit.Risk.Scores",
               plotOutput("one.plot"),
               plotOutput("two.plot"))
    )
  )
)
)

server2 <- function(input, output){
  output$GGplot <- renderPlot({ 
    library(ggplot2)
    df3.gg.chosen <- df3.gg.chosen[(df3.gg.chosen$orig.industry_name %in% input$industry.list),]
    ggplot(data = df3.gg.chosen, aes(x = get(input$choose.ranks), y = get(input$y.ggplot), col = orig.industry_name)) + geom_point() + geom_smooth() + ylab(input$y.ggplot) + xlab(input$choose.ranks)
  })
  
  
  output$vioplot <- renderPlot({ 
    library(ggplot2)
    library(vioplot)
    df3.gg.chosen <- df3.gg.chosen[(df3.gg.chosen$orig.industry_name %in% input$industry.list),]
    ggplot(data = df3.gg.chosen, aes(x = orig.industry_name, y = get(input$y.ggplot))) + geom_violin(aes(fill = orig.industry_name)) + geom_point() + ylab(input$y.ggplot) + xlab(input$industry.list)
    #ggplot(data = df3.gg.chosen, aes(x = orig.industry_name, y = log.rank, col = orig.industry_name)) + geom_violin() + geom_point()  
    })
  
  output$boxplot <- renderPlot({ 
    library(ggplot2)
    library(vioplot)
    df3.gg.chosen <- df3.gg.chosen[(df3.gg.chosen$orig.industry_name %in% input$industry.list),]
    ggplot(data = df3.gg.chosen, aes(x = orig.industry_name, y = get(input$y.ggplot), fill = orig.industry_name)) + geom_boxplot() + ylab(input$y.ggplot) + xlab(input$industry.list)
    #ggplot(data = df3.gg.chosen, aes(x = orig.industry_name, y = log.rank, col = orig.industry_name)) + geom_violin() + geom_point()  
  })
  
  output$ggplot.group <- renderPlot({ 
    library(ggplot2)
    df3.gg.chosen <- df3.gg.chosen[(df3.gg.chosen$orig.industry_name %in% input$industry.list),]
    ggplot(data = df3.gg.chosen, aes(x = get(input$choose.ranks), y = get(input$y.ggplot), col = industry.name)) + geom_point() + geom_smooth() + ylab(input$y.ggplot) + xlab(input$choose.ranks)
  })
  
  output$two.plot <- renderPlot({ 
    library(ggplot2)
    df3.gg.chosen <- df3.gg.chosen[(df3.gg.chosen$orig.industry_name %in% input$industry.list),]
    df3.gg.chosen <- df3.gg.chosen %>% dplyr::select(growth.score:risk.score, industry.name)
    two.plot.data <- melt(df3.gg.chosen, id = "industry.name")
    ggplot(data = two.plot.data, aes(x = variable, y = value, col = industry.name)) + geom_boxplot() 
  })
  
  output$one.plot <- renderPlot({ 
    library(ggplot2)
    df3.gg.chosen <- df3.gg.chosen[(df3.gg.chosen$orig.industry_name %in% input$industry.list),]
    df3.gg.chosen <- df3.gg.chosen %>% dplyr::select(growth.score:risk.score, orig.industry_name)
    one.plot.data <- melt(df3.gg.chosen, id = "orig.industry_name")
    ggplot(data = one.plot.data, aes(x = variable, y = value, col = orig.industry_name)) + geom_boxplot() 
  })
  
  
  }

shinyApp(ui2, server2)

####### END OF SHINY ###########