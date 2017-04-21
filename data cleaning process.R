####### packages #######
library(plyr) # helping with reconfiguration
library(dplyr) # helping with reconfiguration
library(tidyr) # helping with reconfiguration
library(stringr) # help with string
library(rworldmap) # helps with countries
library(tictoc) # timer
library(ggplot2) # data visualization

tic()
######## DOWNLOADING DATA ######
excel <- read.csv("big.test.csv") # download excel
excel <- unique(excel) # so this gets rid of duplicates, also there is a few rows
# where the header is repeated this gets rid of duplicates
excel <- excel[-14716,] # this gets rid of original header that got duplicated in set
excel <- excel[,-(43:56)] # i don't know what these are for but i don't like them


##### fixing global rank - fixed.global.rank ########
table(excel$global_rank) 

# so if you look around you can see some weird stuff
# you can see commas in data, weird e, and xca.... this is bad
# want to Cleaning global rank
# to undertsnad how to use function type help(str_replace)
fixed.global.rank <- excel$global_rank %>% str_replace("\\\xca","") %>%  str_replace("\\\xca\\Z","") %>% # ^ means at the beggining and then 
  str_replace_all("\\ξ","") %>% # %>% means to pipe in in, so that don't have to keep creating new variables
  str_replace_all("\\,","") %>% as.character() %>% as.numeric()

###### countries prep - PREP #####
data(countryExData) # this loads the data for all teh countries
countries <- countryExData$Country
countries[145] <- "Vietnam"
countries <- str_to_lower(countries)
countries <- trimws(countries)
countries[150] <- "hong kong"
countries[151] <- "singapore"
countries[152] <- "puerto rico"
countries[153] <- NA
countries[154] <- "qatar"
countries[155] <- "guam"
countries[156] <- "korea"

# what you did right now is get a list of countries with the correct name
# it will come in handy later
########### Cleaning Country - country ###########
country <- excel$country
table(country)
country[17185] <- "united states"
country <- trimws(country)
country <- str_to_lower(country) # puts in all in lower case
country <- str_replace_all(country,"\\ξ","")  # You see how I am creating a variable for each one
country <- str_replace_all(country,"\\`","")  # an easier way to keep it going is with the %>% (pipe)
country <- str_replace_all(country,"\\_", " ") 
country <- str_replace(country,"statesa","states")  
country <- str_replace(country,"united'states","united states")  
country <- str_replace(country,"u.s.","united states")  
country <- str_replace(country,"u.s","united states")  
country <- str_replace(country,"united kindom","united kingdom")  
country <- str_replace(country,"state$","states")  
country[which(country == 21969)] <- NA
country[which(country == "")] <- NA
country[which(country == "unknown")] <- NA
country <- str_replace(country,"ustatesk","united states")  
country <- str_replace(country,"nstates","united states")  
country <- str_replace(country,"netherland$","netherlands")  
country <- str_replace(country,"gb","uk")  
country <- str_replace(country, "indian", "india")  
country <- str_replace(country, "kanpan", "japan")  
country <- str_replace(country, "south korea", "korea")  
country <- str_replace(country, "uk", "united kingdom")  
country <- str_replace(country, "u.k.", "united kingdom")
country <- str_replace(country, "u.s", "united states")
country <- str_replace(country, "janpan", "japan")  

# as you can probably tell the above method was taking too long
# one of my friends actually told me a neat trick which is below
# remember our countries from earlier
# we have all the countires and so were gonna see which one fits in the list
# the rest we put sort in a place called messed.up.ones and then sort it and label it
table(!(country %in% countries)) # have 106 ones not in list
messed.up.ones <- sort(country[!(country %in% countries)])
# just go few lines down
country <- country %>% str_replace("autralia", "australia") %>% str_replace("tunited kingdomy", "united kingdom") %>% str_replace("solvenia", "slovenia") %>% str_replace("united staties", "united states") %>% str_replace("isreal", "israel")

messed.up.ones <- sort(country[!(country %in% countries)])
solo <- names(table(messed.up.ones)) # you can see that the ones 5-8 are probably canada

country[which(country == "ca")] <- "canada"
country[which(country == "can")] <- "canada"
country[which(country == "cananda")] <- "canada"
country[which(country == "cn")] <- "canada"

messed.up.ones <- sort(country[!(country %in% countries)]) # update messed up ones
table(messed.up.ones) # see problems
solo <- names(table(messed.up.ones))
country[which(country == "jp")] <- "japan"
country[which(country == "jpn")] <- "japan"
country[which(country == "trinidad and tobago")] <- "trinidad & tobago"
country[which(country == "london")] <- "united kingdom"
country[which(country == "na")] <- NA
country[which(country == "n.a")] <- NA

messed.up.ones <- sort(country[!(country %in% countries)])
solo <- names(table(messed.up.ones))

# so as you can see were left with these 
#half half weird ones and trash
# so for this you can clean out the data points, probably get 30 properly
table(messed.up.ones)
# i just decided to make it all into na

country[which(country == "aunited statestralia")] <- "united states"

for(book in 1:length(solo)){ # decided to use book as the holder
  country[which(country == solo[book])] <- NA
}
messed.up.ones <- sort(country[!(country %in% countries)])
solo <- names(table(messed.up.ones)); solo

# country complete
# country main variable of interest
str(country)
table(country)
# END OF THIS

########### CLEANING Country rank - country.rank2 #############
country.rank2 <- excel$country_rank %>% str_replace("^\\\xca","") %>% 
  str_replace("\\\xca$","") %>% 
  str_replace_all("\\ξ","") %>% 
  str_replace_all("\\,","")

table(country.rank2) # how do you have a 0 in this set? what is 10049 ?
country.rank2[which(country.rank2 == 0)] <- NA
which(str_detect(country.rank2, "10049")) # this one ahs a weird thing
which(str_detect(country.rank2, "11413"))
country.rank2[17404] <- NA
country.rank2[17264] <- NA

country.rank2 <- as.numeric(as.character(country.rank2))
str(country.rank2)


######### for web creation year - creation.year #############
creation.year <- excel$web_creation_year
table(creation.year) # so couple of errosr
# You have dates in weird format, person puts wrong wording, you have dates that are completely off
# how is a website created at 2018 and 2019? did someone get the years confused
# so couple of errors
# want the years that are between 1983 - 2018
# so there are dates badly formateed years between, but I am not gonna go through extreme length
# to save one data point
# i would much rather throw everything not in those group away

# so the way I am gonna do this is create a list with all the numeric indexes
# of values that match years want.
# Then subtract that index from original and turn the rest into NA

year.names <- names(table(creation.year)) # so couple of errors

index.track.year <- c() # this is the index of numbers that are useful
years.want <- 1983:2017
for(i in 1:length(years.want)){ 
  dispose <- which(creation.year == years.want[i]) # find out which one are equal to dates want
  index.track.year <- c(index.track.year, dispose) # combine all the indexes of interest
} 

creation.year[-index.track.year] <- NA # make all the ones not with numbers 1983:2017 into NA
table(creation.year)
creation.year <- droplevels(creation.year)
table(as.numeric(as.character(creation.year))) # looks good
creation.year <- as.numeric(as.character(creation.year))


############ privacy - pro.pri ##############
pro.pri <- excel$privacy_protect_indic
table(pro.pri)
# want ones and zeros
wanted0and1 <- c(which(pro.pri == 0), which(pro.pri == 1))
pro.pri[-wanted0and1] <- NA
pro.pri <- droplevels(pro.pri)
pro.pri <- as.numeric(as.character(pro.pri))


checking <- data.frame(fixed.global.rank, country, country.rank2, creation.year, pro.pri)
for(panda in 1:ncol(checking)){
  print(str(checking[,panda]))
} # perfect, so all of them have same length as the unique excel and looks like they are in correct form
print(length(creation.year))

########## END OF NEW DATA #########


####### Formating the old data ######
for(panda in 1:ncol(excel)){
  print(paste(names(excel)[panda], str(excel[,panda])))
} # seeing the structure of all the columns

excel <- droplevels(excel) # drop excess levels your not using

##### OLD DATA - NUMBERS TO MAKE NUMERIC#####
colnames(excel)[c(17:29)] # this should be adjusted to numeric
for(panda in 17:29){
  print(paste(names(excel)[panda], str(excel[,panda])))
}
table(excel$bq_age_oldest_plan) # want to see how it is

# want to change these into numeric, lets create a for loop

# 17 gets you in trouble because of that rascial

table(excel[,17]) # ah so little rascal is 157256\xca
# whoever wrote \xca in the excel sheet, has caused me alot of pain
excel[,17] <- str_replace_all(excel[,17],"\\\xca","") # get rid of it at end
excel[,17] <- trimws(excel[,17]) # get rid of white space
excel[,17] <- as.numeric(as.character(excel[,17])) # turn this into numeric

counter.change <- 18
for(change in 18:29){
  cute <- as.numeric(as.character(excel[,change]))
  counter.change <- counter.change + 1
  print(names(excel)[change])
  str(cute)
} # this did trick for 18:29

for(change in 18:29){
  excel[,change] <- as.numeric(as.character(excel[,change]))
} # this did trick for 18:29

#### final frame ####

final.result <- data.frame(excel[,1:34], global.rank = fixed.global.rank, country = country, country.rank = country.rank2, excel$category, category.rank = as.numeric(as.character(excel$category_rank)), web.creation.year = creation.year, web.reg.org = excel[,41], web.private = pro.pri)
write.csv(final.result, "cleaned.csv")
hope.it.works <- read.csv("cleaned.csv")
str(hope.it.works) # it works

######### or so i thought .... ##########

######### DATA IMPORT ################

a <- read.csv("cleaned.csv") # import the cleaned one
no.name <- which(a$bq_company_legal_name == "#NAME?") 
# a few companies got their name changed to no name during the 50 student collection process. One of their sites was broken and the other was okay
# just do not want to deal with headache so deleting the two
a <- a[-no.name,] # telling it to delete it
b <- a # creating copy just in case

######### String Replacement - company name ################

# So I noticed that there are multiple compaanies in there, which I did not clean...
# These slipped through the cracks because the reptitions were changed with each iteration...
# either bizqualify data shifted or more often the collected alexa and whois data changed
# i think the human error was due to misunderstanding the links....
length(names(table(a$bq_company_legal_name))) # 11041 unique names originally

# want to strip companies of their commas, this is because companies missing commas get registered as different
# can apply same thing to hypened and . and also make them all lowercase
a$bq_company_legal_name <- trimws(str_replace_all(a$bq_company_legal_name, "\\,", "")) # want to get rid of ,
a$bq_company_legal_name <- trimws(str_replace_all(a$bq_company_legal_name, "\\-", ""))
a$bq_company_legal_name <- trimws(str_replace(a$bq_company_legal_name, "\\.", ""))
a$bq_company_legal_name <- str_to_lower(a$bq_company_legal_name) # this saved 2
length(names(table(a$bq_company_legal_name))) #10398 vs 11041 names. about 43 had gramatical problems in them

######### String Replacement - industry name ################
names(table(a$bq_industry_name)) # see what have
a$bq_industry_name[which(a$bq_industry_name == "other imformation service")] <- "Other Information Services (including news syndicates libraries internet publishing & broadcasting)" # clear misnomer
a$bq_industry_name <- a$bq_industry_name %>% str_replace_all("\\,", "") %>% str_replace_all("\\-", "") %>% str_replace("\\\xca","") %>% 
str_replace_all("\\ξ","") %>% str_to_lower() %>% trimws()

names(table(a$bq_industry_name))

######### Frequencies ################
corrected.names <- a$bq_company_legal_name

unique.names <- names(sort(table(corrected.names), decreasing = TRUE))
unique.names.occurances <- as.numeric(sort(table(corrected.names), decreasing = TRUE))
#View(data.frame(unique.names, unique.names.occurances))
# uncomment above to see the tabled frequencies of each

names.and.occurances <- data.frame(unique.names, unique.names.occurances)
dim(names.and.occurances) # 10398 unique names
sum(names.and.occurances[,2]) # 17887 including repeated ones
# 17887 - 10398 => 7489 over occurances :(


########## seeing how many are in there once vs 2+ ################
length(table(corrected.names)[which(table(corrected.names) == 1)]) 
# 7409 companies only in there once
# there should be 10398 - 7409 = 2991 companies in there multiple times

length(table(corrected.names)[which(table(corrected.names) > 1)])# matches our math... 
# so 2989 companies are unique but somehow duplicated 7487 times smore than wanted....


########## Thoughts on 1 vs 2+ ################
# Personally I think it might be better to use the data that there are just one of as there is no known conflicting info
# Assuming that people wrote these correct, 7409 companies to play around with should be fine
# there should be an object later on with just these. 
# I will go through the process of getting the other 2989 but note that accuracy may be lost through the process.
# Personally, I think that there are alot of mistakes in the data even with corrections so I would take everything
# with a grain of salt


########## Thoughts cleaning 2+ ################
# How do you want to deal with this?
# note there are also differing Biz qualify data as well as fetched data....
# Ways to tackle ----------------------------------
# A: so we can manually go through each one and see which one is best (time consuming and living hell)
# B. Find out the most recent --> don't know which one most recent
# C. Create a function that gets the mode of the variables and the minimum of our whois and alex data. 
# Mode is that one most people got, going off majority put for character values.
# for values, minimum is selected because the correct site will probably have the most minimum out of the multiple data
# i personally still do not trust the data, but for the sake of moving on I think this is fine.
# D. go fetch new data from bizqualify and then transplant the best judged from 2+ onto it
# D woudl have worked.... :(

########## Names and Indexes ################
two.plus.names <- names((table(corrected.names))[which(table(corrected.names) > 1)]); length(two.plus.names) # names of all copany repeated more than once
just.one.names <- names((table(corrected.names))[which(table(corrected.names) == 1)]); length(just.one.names) # names of all company just once in data
# 7409 sane ones, you can run your analysis somehwat sanely with these.

###### pulling ones and 2+ from original data #############
one.co.from.original.index <- which(a$bq_company_legal_name %in% just.one.names) # find out which just.one.names are in the a$legal.names
one.co.from.original <- a[one.co.from.original.index,]; dim(one.co.from.original)
more.than.one.co.original <- a[-one.co.from.original.index,]; dim(more.than.one.co.original)
# so there are 2990 original names in more.than.one.co.original 
# but total data is 10480
length(unique(more.than.one.co.original$bq_company_legal_name)) # verify number of unique ones

############ RECAP ##############
# essentially you've seperated the original data into ones that repeat more than ocne and 2+ times
# 2+ times (more.than.one.co.original)
# once (one.co.from.original)
############ RECAP ##############

############ Checking for 2+ ##############
## Looking for anything that is not in our list but in data ##################
weird.twos <- which(!(more.than.one.co.original$bq_company_legal_name %in% two.plus.names)) # of course 2 NA's
more.than.one.co.original <- more.than.one.co.original[-weird.twos,] # getting rid of NA's

length(which(more.than.one.co.original$bq_company_legal_name %in% two.plus.names)) # 10478
dim(more.than.one.co.original) #- everything we have in two.plus.names we have in our dataframe 
# we good!!!
length(unique(more.than.one.co.original$bq_company_legal_name)) # indicates how many unique in this mix


############ Checking for 1 ##############
weird.ones <- which(!(one.co.from.original$bq_company_legal_name %in% just.one.names))
weird.ones # want this to be zero, this indicates how many are labeled wrong

length(unique(more.than.one.co.original$bq_company_legal_name)) # the extra one is because of NA looking good

############ RECAP ##############
# what have you done so far
# one.co.from.original <- data that includes one company
# more.than.one.co.original <- data that includes 2+ company
# just.one.names <- names of the one company
# two.plus.names <- names of the 2+ company
############ END ##############

############ MODE FUNCTION ##############
Mode <- function(x, na.rm = FALSE) { # function to get mode of something in R
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
########## Following code executes the following ###############
# you are telling R to go find out which ones match

########## FUNCTION TO execute plan ###############
first <- length(two.plus.names) # want the lenght of the this string
rebrand <- data.frame(c()) # empty to put stuff in
biggy <- t(data.frame(rep(NA,43))) # empty row that can build upon
colnames(biggy) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", 
                     "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", 
                     "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28", 
                     "V29", "V30", "V31", "V32", "V33", "V34", "V35", "V36", "V37", 
                     "V38", "V39", "V40", "V41", "V42", "V43")
#the column names of function, helps with binding, we can always change names later
count <- 0 # helps with debugging
for(i in 1:first){ 
  count <- count + 1
  rebrand <- data.frame(c()) # cleaning for next
  index.matches <- which(more.than.one.co.original$bq_company_legal_name == two.plus.names[i]) # find index that matches
  rows.of.interest <- more.than.one.co.original[index.matches,] #pull rows that match
  second <- ncol(rows.of.interest) # want number of rows
  for(z in 1:second){ # for each row
    keeping.row.one <- Mode(rows.of.interest[,z]) # go find the mode
    rebrand[1, z] <- keeping.row.one 
  }
  for(z in 36:43){ # for each row
    keeping.row.one <- Mode(rows.of.interest[,z], na.rm = TRUE) # go find the mode
    rebrand[1, z] <- keeping.row.one 
  }
  inf.36 <- which(!is.na(rows.of.interest[,36]))
  rebrand[1, 36] <- min(rows.of.interest[inf.36, 36])
  inf.38 <- which(!is.na(rows.of.interest[,38]))
  rebrand[1, 38] <- min(rows.of.interest[inf.38, 38])
  inf.40 <- which(!is.na(rows.of.interest[,40]))
  rebrand[1, 40] <- min(rows.of.interest[inf.40, 40])
  inf.41 <- which(!is.na(rows.of.interest[,41]))
  rebrand[1, 41] <- min(rows.of.interest[inf.41, 41])
  #rebrand[1, 36] <- min(rows.of.interest[,36][which(complete.cases(rows.of.interest[,36]))])
  #rebrand[1, 38] <- min(rows.of.interest[,38][which(complete.cases(rows.of.interest[,38]))])
  #rebrand[1, 36] <- min(rows.of.interest[,36]) # change to correct rank
  #rebrand[1, 38] <- min(rows.of.interest[,38]) # change to correct 
  biggy <- rbind.data.frame(biggy, rebrand) # add the next cleaned one over
}
names(biggy) <- names(a)
biggy[,36][which(biggy[,36] == "Inf")] <- NA
biggy[,38][which(biggy[,38] == "Inf")] <- NA
biggy[,40][which(biggy[,40] == "Inf")] <- NA
biggy[,41][which(biggy[,41] == "Inf")] <- NA
biggy <- biggy[-1,]

#View(biggy) 

# so biggy is our dataset of 2+ reduced to one

finally.cleaned <- rbind.data.frame(one.co.from.original, biggy)
dim(finally.cleaned) 

colnames(finally.cleaned) <- c("Corresponding index", "bq_company_legal_name", "parent_comp", "bq_industry_name", 
                               "bq_business_code", "subj_industry", "bq_company_contact_name", 
                               "company_ein", "bq_company_address1_line_1", "bq_company_address1_line_2", 
                               "bq_company_address1_city", "bq_company_address1_state", "bq_company_address1_zip5", 
                               "bq_comp_fgn_address1_cntry", "bq_company_phone", "bq_website", 
                               "correct_web", "bq_current_employees_plan", "bq_growth_score", 
                               "bq_profitability_score", "bq_risk_score", "bq_net_assets_pens_eoy", 
                               "bq_revenue", "bq_ebitda", "bq_age_oldest_plan", "bq_comp_contribute_pens_ind", 
                               "Public", "bq_growth_tot_contrib_pens_amt_a", "bq_growth_co_contrib_pens_amt_a", 
                               "bq_emp_growth_rate", "bq_naics_code", "bq_sic_code", "bq_naics_description", 
                               "bq_sic_description", "bq_ticker", "global.rank", "country", 
                               "country.rank", "excel.category", "category.rank", "web.creation.year", 
                               "web.reg.org", "web.private")

finally.cleaned$`Corresponding index` <- NULL

View(finally.cleaned) # for both repeated and non-repeated
str(finally.cleaned)
finally.cleaned$bq_company_legal_name <- as.factor(finally.cleaned$bq_company_legal_name)
finally.cleaned$bq_industry_name <- as.factor(finally.cleaned$bq_industry_name)
finally.cleaned$bq_company_address1_zip5 <- as.factor(finally.cleaned$bq_company_address1_zip5)
finally.cleaned$company_ein <- as.factor(finally.cleaned$company_ein)
finally.cleaned$bq_business_code <- as.factor(finally.cleaned$bq_business_code)
finally.cleaned$web.private <- as.factor(finally.cleaned$web.private) #0 for public, 1 for private
finally.cleaned$Public <- as.factor(finally.cleaned$Public) # 0 for private, 1 for public
finally.cleaned$bq_comp_contribute_pens_ind <- as.factor(finally.cleaned$bq_comp_contribute_pens_ind) # this is binary
str(finally.cleaned)

######## last stuff #########
rownames(finally.cleaned) <- 1:nrow(finally.cleaned)

finally.cleaned$web.reg.org <- str_replace(finally.cleaned$web.reg.org, "\\,", "") %>% str_replace("\\.", "") %>% str_to_lower() %>%trimws()
# 
dput(names(table(finally.cleaned$web.reg.org[which(str_detect(finally.cleaned$web.reg.org, "priv"))])))
dput(names(table(finally.cleaned$web.reg.org[which(str_detect(finally.cleaned$web.reg.org, "domain"))])))
private.name.indicator <- c("_perfect privacy llc", "_yinsi baohu yi kaiqi (hidden by whois privacy protection service)", 
                            "contact privacy inc", "contact privacy inc customer 0112012615", 
                            "contact privacy inc customer 0118798206", "contact privacy inc customer 0118852529", 
                            "contact privacy inc customer 011977376", "contact privacy inc customer 0120627619", 
                            "contact privacy inc customer 0121071610", "contact privacy inc customer 0124278834", 
                            "contact privacy inc customer 0127976134", "contact privacy inc customer 0128140817", 
                            "contact privacy inc customer 0128888285", "contact privacy inc customer 0129605284", 
                            "contact privacy inc customer 013063117", "contact privacy inc customer 0133246678", 
                            "contact privacy inc customer 0133252934", "contact privacy inc customer 0134344902", 
                            "contact privacy inc customer 0137192649", "contact privacy inc customer 0137830188", 
                            "contact privacy inc customer 0141291320", "contact privacy inc customer 0141725274", 
                            "contact privacy inc customer 0143321602", "contact privacy inc customer 0144323080", 
                            "contact privacy inc customer 0144542907", "contact privacy inc customer 015164673", 
                            "contact privacy inc customer 124847692", "contact privacy inc_", 
                            "delta air lines inc / privacy protected deltaprivatejets.com", 
                            "digital privacy corporation", "domain name proxy service inc privacy id# 1155541", 
                            "domain name proxy service inc privacy id# 11674090", "domain name proxy service inc privacy id# 12656569", 
                            "domain name proxy service inc privacy id# 630802", "domain name proxy service inc privacy id# 983901", 
                            "domain privacy service fbo registrant", "easyspace privacy", 
                            "global domain privacy", "moniker privacy services", "myprivacynet ltd.", 
                            "perfect privacy", "perfect privacy llc", 
                            "perfect privcy llc", "privacy protected", "privacy protection service inc d/b/a privacyprotectorg", 
                            "private registration", "registration private", "see privacyguardianorg", 
                            "super privacy service c/o dynadot", "whois  privacy protection service inc", 
                            "whois  privacy service pty ltd", "whois privacy protection service by onamaecom", 
                            "whois privacy protection service inc", "whois privacy service", ## Deleted paladin private security, boston private bank from string
                            "whois privacy services pty ltd", "wstgtcom private registrant", "1&1 internet inc") # got all this beacuse had priv string in it
private.name.indicator <- c(private.name.indicator, "domains by proxy llc", "network solutions llc", "whoisguard inc", "markmonitor inc", 
                            "na", "registercom inc.", "na na blank", "dnstination inc", "proxy llc", "hugedomainscom", "domain professionals llc", "buydomainscom", "a happy dreamhost customer", "valued customer",
                            "godaddy, llc", "godaddycom llc") 

na.strings <- c("no online presence", "na", NA)


for(i in 1:length(finally.cleaned$web.reg.org)){
  if(sum(private.name.indicator %in% finally.cleaned$web.reg.org[i]) > 0){
    finally.cleaned$web.private[i] <- 1
    finally.cleaned$web.reg.org[i] <- "private.domain"
  } else if (sum(private.name.indicator %in% finally.cleaned$web.reg.org[i]) < 1){
    finally.cleaned$web.private[i] <- 0 # this second command overshadows students works... :(
  } 
  if (sum(na.strings %in% finally.cleaned$web.reg.org[i]) > 0){
    finally.cleaned$web.private[i] <- NA
    # finally.cleaned$web.reg.org[i] <- NA
  }
}

# reminder the code does not account for divisions in different departments 
# example cummins 


############## cleanup ################
# few slipped through the cracks
# mainly different divisions or company that slightly spelled differently 
# manually hardcoded these
head(sort(table(finally.cleaned$correct_web), decreasing = TRUE), 20)
head(sort(table(finally.cleaned$country.rank), decreasing = TRUE), 20)

finally.cleaned <- finally.cleaned[-which(finally.cleaned$parent_comp == "DigitalGlobe Inc."),] 
finally.cleaned <- finally.cleaned[-which(finally.cleaned$bq_company_legal_name =="full compass ssystems ltd"),]
finally.cleaned <- finally.cleaned[-which(finally.cleaned$bq_company_legal_name =="odyssey logistics and technology corporation"),]
finally.cleaned <- finally.cleaned[-which(finally.cleaned$bq_company_legal_name =="workfront"),]
finally.cleaned <- finally.cleaned[-which(finally.cleaned$bq_company_legal_name =="all nippon airways co ltd."),]
finally.cleaned$excel.category <- str_replace_all(finally.cleaned$excel.category, "\\ξ", "")
finally.cleaned$parent_comp <- str_replace_all(finally.cleaned$excel.category, "\\ξ", "")
# honestly there like 30 of these, and I would be okay with slipping these through cracks
# unless someone wants ago at these

finally.cleaned$web.reg.org[which(finally.cleaned$web.reg.org == "")] <- NA

finally.cleaned[which(finally.cleaned$category.rank == 0), 39:40] <- NA


############## Completing ################
str(finally.cleaned)
finally.cleaned$bq_company_legal_name <- as.factor(finally.cleaned$bq_company_legal_name)
finally.cleaned$bq_industry_name <- as.factor(finally.cleaned$bq_industry_name)
finally.cleaned$bq_company_address1_zip5 <- as.factor(finally.cleaned$bq_company_address1_zip5)
finally.cleaned$company_ein <- as.factor(finally.cleaned$company_ein)
finally.cleaned$bq_business_code <- as.factor(finally.cleaned$bq_business_code)
finally.cleaned$web.private <- as.factor(finally.cleaned$web.private)
finally.cleaned$excel.category <- as.factor(finally.cleaned$excel.category)
finally.cleaned <- droplevels(finally.cleaned)

str(finally.cleaned)
just.one.case <- finally.cleaned[which(finally.cleaned$bq_company_legal_name %in% one.co.from.original$bq_company_legal_name),]
# just.one.case is the variables in it just one time

write.csv(finally.cleaned, "cleaned.csv")
downloaded <- read.csv("cleaned.csv")

toc()
