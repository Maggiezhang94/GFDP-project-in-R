################## STEP 0 ###############
# SO I wrote this script to help with data collection
# I find it easier to have data to check then go out and search from scratch

################## STEP 1: PACKAGES ###############
# install.packages("readxl")
# install.packages("RCurl")
# install.packages("tictoc")
# install.packages("rvest")
# install.packages("httr")
# install.packages("dplyr")

# packages you will be using, it would be easiest to just open them up all at once
library(readxl) # read excel
library(RCurl) # this is a package to help with url's
library(tictoc) # helps with keeping track of time... 
library(rvest) # helps with scraping
library(httr) # this package will help bring out the real url
library(dplyr) # helps with data manipulation

################## STEP 2: READING DATA ###############

library(readxl) # read excel
getwd() # first get working directory, move file into working directory

#excel <- read.csv("file.csv") # if csv
excel <- read_excel("/Users/Garo/Downloads/filter.prospect.xls") # download excel
# Storing your excel sheet in an object called excel
#excel <- read.csv("~/Downloads/big.test.csv") # download excel

website <- excel
# making a copy of the excel sheet in an object called website. 
# This is mainly to play around with the data while still having an original copy.

################## STEP 2.5: WARNINGS ###############

####### Before we automate, I would like to bring about some warnings...
# Automated website scraping can do alot of things but it should not be taken for granted
# There can be alot of things that go wrong. I say use this script, but with caution...

# So main concerns with scraping/automation for website.
# (1) find out if Co has website -
# If there isn't a website listed on the excel, there isn't much we can work with.
# so for the automation to work (pull the info from who is), update the excel with a correct site and run the script.
# I've also added a script that googles the business names and pulls the first link, but for fully correct results, 
# please update the excel with the correct website if it is missing.

# (2) Given a website, you would like to see if the website is clickable. 
# (can click on link and pop up page) eg.) See if link broken or good.
# in the example Jose used he pulled Uber. Uber had uberregios listed as its website. 
# well uberregios.com does not exist. The script will help verify if 
# you can open it up on your webpage.

# (3) if click on link do you get redirected? 
# ex) if click on "broadcast.com" you will be redirected to "yahoo.com"
# the script will help correct that. 

# (4) Link works and on correct page, but it is not the company's main page. 
# Ex) Alphabet's website is https://abc.xyz but correct website want is google.com 
# For this the code cannot help you with and you will have to use your brain.

# Code is able to reveal 1-3. (4) is alluding...

################## STEP 3: Extracting URL (see if works) ###############

library(RCurl) # this is a package to help with url's
library(httr) # this is package to help verify site
# now gonna try to check if website is legit
limiter <- nrow(website) # assingns a value of numeber of rows of our excel sheet (labeled website) to something called limiter
url.extracted <- c() # create empty place to store URL's will get
count <- 0 # start count just in case... tool for depugging # not really necessary 
tic() # start timer
for(aim in 1:limiter){ # for 1 to end of length limiter run the function below
  
  tryCatch({ # so tryCatch here tells our code to continue despite any error, 
    # This will be useful for N/A urls
    count <- count + 1 # this line is again not necessary, it just helped me with debugging
    names(GET(website$bq_website[aim])) # telling to go get the websites for each one 
    url.extracted[aim] <- GET(website$bq_website[aim])$url}, error = function(e){}) 
    # previous line of code return the url associated with the website, 
    # this does two things, first it tells us if a website is real. 
    #Meaning you can type it on your browser and go to a valid page
    # Second, solves case (3) and tells you if the site redirects you to another site. 
    # example) broadcast.com --> yahoo.com
}
url.extraction.toc <- toc(); url.extraction.toc # how long it took... 
View(url.extracted)
# whole point is to get variable *url.extracted*
#View(cbind(Site.from.BQ = website$bq_website, URL.extracted.from.BQ = url.extracted))
# This uncomment code above and you should get a dataframe with the site from BQ and then the extracted URL

# if you get a NA in the second column, that means link is broken or does not exist.
# in which case you should go find the correct link of the site
# if you get something else in second column, 
#that is the link you are redirected to when you put that in browser (real website)
# most of time it is correct but 
#these links should be investigated as some website link might take you to a broken page instead (rarely but possible)
# luckily most of time you can recognize it, if it went wrong
# good example this case is http://medical-marketing-service.com --> https://en.wordpress.com/typo/?subdomain=medical-marketing-service

############ Concern #########
#remember, despite automation - Concern (4) remains possible
# I have not found fullproof solution for this, but moving on...

###### Filling in NA's #############
# so looking at this you can see that the url.extracted has a lot of NA's
# you can ignore move on or you can search them
# The next couple of lines searches the companies with a missing site and pulls up 
# the first google link

################## STEP 3 (optional): Googling Companies ###############
#In this exaple, I fill the NA portion with the first link from a google searches
# you can change it so that it can pull the first google link from each company

############# google script ##########
google.filled <- url.extracted # store previous variable url.extracted into google.filled
interest <- which(is.na(url.extracted)) # find out which variables have NA
google.tic <- tic(); google.tic # set up timer
seq <- 1
for(fill in interest){ # telling R for every empty go out and do below
  url <- URLencode(paste0("https://www.google.com/search?q=", website$bq_company_legal_name[interest[seq]]))
  # this tells it to go search for this term
  # below tells it to bring back links
  list.of.pages <- read_html(url) %>% 
    html_nodes("cite") %>% # Get all links
    html_text()
  # this tells it to replace the missing with the top of the page of that search
  google.filled[fill] <- as.character(list.of.pages[1])
  seq <- seq + 1
}
google.toc <- toc(); google.toc
View(cbind(Companyname = website$bq_company_legal_name, BQ.site = website$bq_website, BQ.site.real = url.extracted, google.filled))
# look for discripencies
# So here we have a company name, website registered, test to see if website real or not, and then a google search using domain name.
# In this case the googled searches are filled into those that have NA
# to improve accuracy you might want to compare the url.extracted with everything searched by google
# you can do this by changing interest (line 102) to length(url.extracted) and then cbinding them

# REMEMBER THERE IS ERROR, PLEASE CORRECT. THIS IS NOT FULLPROOF BUT IT WILL SAVE TIME

################## STEP 4 With links, scrape Who is ###############

######## Choose data set ######
# Whichever set you feel more comfortable with take it to the next level

# site4rvest <- website$bq_website # original data
#site4rvest <- url.extracted # one with refined terms from BQ
# site4rvest <- google.filled # one with google terms added
# REMEMBER YOUR GONNA GET SOME ERRORS HERE, PLEASE GO BACK AND FIX THEM
# THE CODE IS HERE TO HELP SAVE TIME BUT YOU STILL GOTTA DO THE CLEANING JOB
site4rvest <- url.extracted
#site4rvest <- google.filled
# if you use url.extracted you will get less extracted but more correctly extracted
# if you use google.filled you will get more extracted but also more uncorrectly extracted
# to help overall, I would suggest using url.extracted and one whole google search in the program.... but up to you
# default is filling url.extracted with google
######## Scrape Site ######
library(rvest) # package that helps with scraping
registration.date <- c() # empty set 4 registration date
registrant <- c() # empty set for registrant
count2 <- 0 # meaningless here, but helped me perfect script
tic() # start counting
for(go in 1:limiter){ # remember repeating sequence for this long
  tryCatch({ # continue loop even if encounter error
    count2 <- count2 + 1 # no real value, helped with debugging
    beggining <- "http://www.whois.com/whois/" # first half of site
    site <- paste0(beggining, site4rvest[go]) # second half of site pasted
    # you are telling to paste the half of real website that we found onto whois
    html.of.site <- read_html(site) # read the html site from previous line
    registration.date[go] <- html.of.site %>% 
      html_node(".whois_heading+ .df-block .df-row:nth-child(4) .df-value") %>%
      html_text() 
    # so this looks intimidating at first but its basic 
    # so you are storing all this in an object called html of site
    # first you are reading the html of the whois + your company link
    # the %>% (pipe) function connects it with the next function
    # the next function pulls the CSS line that want from page. 
    # You would have to use selector gadget chrome extension to find out what to put in the quotes to pull what want.
    # great tutorial can be found @ https://stat4701.github.io/edav/2015/04/02/rvest_tutorial/
    #lastly html text tells it put in text format that can understand
    registrant[go] <- html.of.site %>% html_node(".df-block:nth-child(6) .df-row:nth-child(3) .df-value") %>%
      html_text()}, error = function(e){})}
  # so this last bit is telling to store info in registrant and registration.date
  # the last part past the html text is part of the function to continue going even though error
time4scrape <- toc(); time4scrape #end time


#####################NOTE ###############################
# as you can see this is not completely accurate, if you go back and 
# change the website address correctly on excel, the algo can fetch correctly
#####################NOTE #########################


########## OPTIONAL CLEANING (YEAR ONLY) #########
#cleaning registration date to only year, put this here because years vs dates were originally asked
library(stringr) # package to help with strings
registration.year <- c() # create empty string to puts years in
split.stuff <- c() # vector will toss
for(chain in 1:length(registration.date)){
  split.stuff <- str_split(registration.date, "-")
  registration.year[chain] <- split.stuff[[chain]][1]
  registration.year[chain] <- as.numeric(registration.year[chain])
}
#registration.year
############### END of optional route

########## STEP 5 PATCHES AND PRIVACY #########
########## PRIVACY #########
# Now you can combine the variables of interest into a new file.
# this file will can help you to take a look at see if everything is okay.
# you'll notice on the right hand side some repeated dates and registrants... we'll get to that
sort(table(registrant)) # see is everything looks good
# putting in the privacy code
privacy <- rep(0, length(registrant))
private.words <- c("12808 Gran Bay Parkway West",
                   "Domains By Proxy, LLC", # these are all words you know 
                   "5800 Airport Boulevard", # indicate privacy,
                   "UDomain.NET", "12808 Gran Bay Pkwy West", "2626 Warrenville Rd")
# if the above words show up, give mark of 1
temp.box <- c() # using it to store for each loop
private.numbers <- c() # using it to store what we ultimately want
for(fellow in 1:length(registrant)){
  temp.box <- which(registrant == private.words[fellow])
  private.numbers <- c(private.numbers, temp.box)
}
privacy[private.numbers] <- 1 # telling all the numbers to get 1

############### FIXING UP PATCHES (NA) ############
numeric.setup <- which(is.na(site4rvest))
# NA's kinda ran amock here, the next couple of lines will make sure they don't do much damage
for(na.fix in numeric.setup){
  registration.year[na.fix] <- NA
  registrant[na.fix] <- NA
  registration.date[na.fix] <- NA
  privacy[na.fix] <- NA
} # because earlier you told the algo to continue even if get NA
# as a result for those it gave teh date 2000-02-23 & 12808 Gran Bay Parkway West and what not..
# these aren't true privacy indicators....
# fixing them up by putting NA

############# PATCHES FOR GOOGLE - used only if site4rvest is set to google.filled ##########
sort(table(registrant)) # sorting all the wrong google stuff
google.mistake.registrants <- c("Bloomberg Finance L.P.", "Wikimedia Foundation, Inc.", "The Nasdaq Stock Market, Inc")
goog.mis.length <- length(google.mistake.registrants)
real.bloomberg <- grep("loomberg", website$bq_company_legal_name, fixed = TRUE) # another mistake would probably be glassdoors
box2toss <- c()
google.mis.numbers <- c()
for(mistake.number in 1:goog.mis.length){
  box2toss <- which(registrant == google.mistake.registrants[mistake.number])
  google.mis.numbers <- c(google.mis.numbers, box2toss)
}

if(length(real.bloomberg) > 0){
if(real.bloomberg %in% google.mis.numbers){
  for(a in 1:length(real.bloomberg)){
    google.mis.numbers <- google.mis.numbers[-which(real.bloomberg[a] == google.mis.numbers)]
    }
  }
}

  registration.year[google.mis.numbers] <- NA
  registrant[google.mis.numbers] <- NA
  registration.date[google.mis.numbers] <- NA
  privacy[google.mis.numbers] <- NA
  site4rvest[google.mis.numbers] <- NA

# for the most part it has done the job, but when you examine the data you will find some odd stuff.
# For example) if there is an address for the registrant than that means that privacy is on and that is what who is returns
# if NA is on real.url, that means that you can't automate it, you'll have to find the real site for the company

final <- cbind(website, co.name = website$bq_company_legal_name, sites.used = site4rvest, registration.date, registration.year, registrant, privacy); View(final)
# Your essentially piecing everything together for the final...
# this is prelimary but this can be done with basic knowledge,
# if you correct the websites on the excel sheet and run the code it should return the values want. 

###### Complete CSV ##########

write.csv(final, "automated.csv")
##### please do not take for face value and do check everything...
### Point of this is to save time but also not sacrifice quality


