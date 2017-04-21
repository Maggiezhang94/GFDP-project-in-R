### import the data (either click import or use the function "read.csv"...) and name it "dat"

# deal with non-numeric value (same procedure for other variables...)
dat$employment <- as.numeric(as.character(dat$bq_current_employees_plan))
dat$employment <- as.numeric(as.character(dat$global_rank))
dat=dat[1:100,] #I only used the first 100 observations, but you shouldn't do this

### import the sector list file and add a new column of sector to each companies
sector <- Sector[,c('Sector','Industry_name')]

library(data.table)
sector.dt=data.table(sector)
dat.dt=data.table(dat)

setkey(sector.dt,'Industry_name')
setkey(dat.dt,'bq_industry_name')
# join two tables and create the new data "dat.new"
dat.new=sector.dt[dat.dt]

# remove the missing rows
dat.new=dat.new[,.(bq_current_employees_plan, country_rank)]
dat.new[,Sector:=as.factor(as.character(Sector))]

# show the first 5 rows
head(dat.new)
table(dat.new$Sector)

# we calculate the correlation for each sector:

# employment vs global_rank (I used method "pairwise complete obs", but please delete it and change to "method="spearman")
cor.emp.gr=dat.new[,.(corr=cor(bq_current_employees_plan,global_rank,use='pairwise.complete.obs')),by='Sector']
setorder(cor.emp.gr,'corr') #rearrange the order of my outcome
# According to my result, it's Finance and Insurance sector 

# revenue vs global_rank
cor.rev.gr=dat.new[,.(corr=cor(revenue,global_rank,use='pairwise.complete.obs')),by='Sector']
setorder(cor.rev.gr,'corr')
# it's Administrative and Support and Waste Management and Remediation Services sector 

# employment vs country_rank
cor.emp.cr=dat.new[,.(corr=cor(bq_current_employees_plan,country_rank,use='pairwise.complete.obs')),by='Sector']
setorder(cor.emp.cr,'corr')
# it's Finance and Insurance sector

# revenue vs country_rank
cor.rev.cr=dat.new[,.(corr=cor(revenue,country_rank,use='pairwise.complete.obs')),by='Sector']
setorder(cor.rev.cr,'corr')
# it's mining sector

##for general correlation test, use the function "cor.test(var1,var2,method="spearman")


#### make regression
# we consider the interaction between sector and each independent variable

# dummy encoding
dat1=model.matrix(~bq_current_employees_plan+Sector*global_rank-1,data=dat.new)
emp.gr.lm=lm(bq_current_employees_plan~.,data=data.frame(dat1))
#emp.gr.lm=dat.new[,lm(employment~global_rank*Sector)]
summary(emp.gr.lm)


dat2=model.matrix(~revenue+Sector*global_rank-1,data=dat.new)
rev.gr.lm=lm(bq_revenue~.,data=data.frame(dat2))
#rev.gr.lm=dat.new[,lm(revenue~global_rank*Sector)]
summary(rev.gr.lm)

dat3=model.matrix(~bq_current_employees_plan+Sector*country_rank-1,data=dat.new)
emp.cr.lm=lm(bq_current_employees_plan~.,data=data.frame(dat3))
#emp.cr.lm=dat.new[,lm(employment~country_rank*Sector)]
summary(emp.cr.lm)

dat4=model.matrix(~bq_revenue+Sector*country_rank-1,data=dat.new)
rev.cr.lm=lm(bq_revenue~.,data=data.frame(dat4))
#rev.cr.lm=dat.new[,lm(revenue~country_rank*Sector)]
summary(rev.cr.lm)