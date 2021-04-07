#KRat pedigree/subpop data
#Gina Lamka - Started Spring 2021

"Main Question: How to migrants contribute to genetic diversity of subpopulations?"
"First steps:
  1. find who the migrants are and what populations they moved from > to
  2. find out if the migrants added offspring to the new population
    a. If they did not add offspring, they would be 'vacationers' and would not affect overall genetic diversity
    b. If they did add offspring, Check these out! This is what we are interested in!
    
    J-proportionally, are there immmediate fitness costs of moving? what are the differences are
    
  3. identify if the migrants affected the genetic diversity of the subpopulation
    a. compare offspring of migrants vs offpsring of non-migrants in the next generation J--YES
    b. compare inbreeding estimates of the overall population before and after the moves J--YES --- can use F from pedigree or kinship
    J-before the m ove, what is the averge kinship before and after the move
    J affected by the size of the subpopulation in the move
    J fig with population size over time- use this for rough idea of pop size at any given time
    J looks like 50-75 except for one time with 10
    c. compare longevity of migrants vs non-migrants
      i. ideally, longevity = # of captures. 
      ii. potential issue-- these are not precise birth/death rates, and longevity may be too similar to detect differences
      J- weak but significant in inbreed and longevity in Heredity paper-- check supplementary
  4. other comparisons to check out:
    a. are all migrants of the same sex? and what is the sex ratio of the year they migrated and are the ratios different in first and second subpop?
    b. about how far did migrants travel?
      i. do this by looking at territories. this may or may not be simple to do- idk
      J - coorediantes are in meters and lat/long so you can literally use euclidian distance to estimate dispersal distance
      J - lat/long in field dataset are in meters from the same point. double check by plotting that the points are correct for lat/long
    c. did migrants have more potential mates?
    J-restructure:: once moved did they have more mates in next year rather than comparing to other individuals
      i. issue- what does this mean if yes? due to higher fitness/better genes or higher instance of encountering mates?
    
QUESTION: would it be better to study the migrants or their offspring in terms of genetic diversity?
QUESTION: if using offspring, how many generations? 
"

## open file with data
setwd("C:/Users/HP/Box/New Computer/Auburn/Data") #set working directory in the Rstudio folder on my computer and box drive

field = read.table("KRATP.csv", header=T, sep=",")
data  = read.table("krat_pedigree_plus.csv", header=T, sep=",")
kins  = read.table("allkins.csv", sep=",", header=TRUE)
sub   = read.table("krat_pedinfo_years.csv", header=TRUE, sep=",")

setwd("/Users/jannawilloughby/GDrive/Willoughby lab/kangaroo rat migration/")
field = read.table("mound_locs/KRATP.csv", header=T, sep=",")
data  = read.table("krat_pedigree_plus.csv", header=T, sep=",")
kins  = read.table("mound_locs/allkins.csv", sep=",", header=TRUE)
sub   = read.table("krat_pedinfo_years.csv", header=TRUE, sep=",")

"
THIS FAILED but see below for something that worked!

#find the migrants
indvs = unique(sub$id)
sub$freq = freq(sub$id) #failed

library(data.table)
setDT[, count := uniqueN(subpop), by = id]
"


library(dplyr)
sub2 <- sub %>%
  group_by(id) %>%
  mutate(moves = n_distinct(subpop)-1)
#write sub2 dataframe with the column "moves" that defines the number of times an individual moved subpopulations
sum(sub2$moves)
#we now know there are 78 rows of individuals that have moved

##to see the column for every individual to see how many subpopulations they have do this:
#sub %>%
#  group_by(id) %>%
#  summarise(count = n_distinct(subpop))
##this works BUTTTT summarise spits it out, we want to make a column

hist(sub2$moves)
hist(sub2$moves, breaks=seq(0,4,1))
table(sub2$moves) #shows most did not move, but there are 78 rows of data of individuals who moved. 
#note 78 is not the actual number of individuals, as these have at least 2 capture periods
field[field$id==1062,]#IDNUMB is the number for the ID the square brackets are subsetting row, column so all rows with this field ID
field[field$id==919,]
field[field$id==1635,]
field[field$id==1907,]
sub2[sub2$id==1907,]
sub2[sub2$id==4074,]
sub2[sub2$id==4072,]

" FAILED -- see below for attempt #2
OUT = migrant = NULL #these are creating varilables and setting them to null. take the out variable and add to it at every iteration of the loop. rbind takes what is in out already, add with writeout, and rbind adds another rowm, cbind adds column. so sum and restart loop cuz at the end. null creates variables with nothing in it
indvs = unique(sub2$id)
for(i in 1:length(indvs)){ #iterate 1 to the length of the number of indv in the list
  t = sub2[sub2$id==as.numeric(as.character(indvs[i])),,drop=F] #t is temperatry (convention). a subset of the datafram I am iterating over. so pull out all of rows in field data with thei [articular indiv caputered]. easier to have ind as numeric rather than character. the drop=F is critical. if you subset a datafra,e in R and teh result is just 1 row, R will conver to a list. if you add drop=FALSE, treate as a one row datafra,/one row matrix
  if(length(t$subpop[t$moves==1])>0){ #looking within t, when offspring =1. if a baby at the time, 1 is yes. if adult, offpring=0. find any rows where temperatry t, where it was captured as offpring, and if it is there, save as "natal". and saving the name of teh territory there. if natal=0, not caputred at reproductive age.
    migrant = as.character(t$subpop[t$moves==1])
  }else(
    migrant=0
  )
  yes = nrow(t[t$moves==1,,drop=F]) #find all rows when they were adults. therefore this is the number of captured as adult
  nsubpops = length(unique(t$subpop[t$moves==1])) ##how many territories as an adult (offspring=0) and look at how many unique, what is the length. so how many mounds did the indv occupt as adult
  writeout = c(indvs[i], migrant, nsubpops) #this is the info to put in new datafra,e. concatenated into a list and addd writeout to end of the out variable
  OUT = rbind(OUT, writeout)
}

rownames(OUT) = seq(1,nrow(OUT),1) #change row names of out to consecutibe numbers in the sequence. if in huge matrix, more convenient as dataframe, so taking column and putting in name that is understandable later
mounds = data.frame(id=as.numeric(as.character(OUT[,1])), natal=as.character(OUT[,2]), nadultmounds=as.numeric(as.character(OUT[,3])), nadultsubpops=as.numeric(as.character(OUT[,4])))

table(mounds$nadultmounds)
table(mounds$nadultsubpops)
table(mounds$nadultmounds-mounds$nadultsubpops)"

###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##UPdate-- does NOT work.. out of bounds ---- THIS WORKS BUT THE COLUMNS ARE MESSED UP-- TRY CHANGING SO THAT IT WORKS
OUT = nonmigrant = NULL #these are creating varilables and setting them to null. take the out variable and add to it at every iteration of the loop. rbind takes what is in out already, add with writeout, and rbind adds another rowm, cbind adds column. so sum and restart loop cuz at the end. null creates variables with nothing in it
indvs = unique(sub2$id)
for(i in 1:length(indvs)){ #iterate 1 to the length of the number of indv in the list
  t = sub2[sub2$id==as.numeric(as.character(indvs[i])),,drop=F] #t is temperatry (convention). a subset of the datafram I am iterating over. so pull out all of rows in field data with thei [articular indiv caputered]. easier to have ind as numeric rather than character. the drop=F is critical. if you subset a datafra,e in R and teh result is just 1 row, R will conver to a list. if you add drop=FALSE, treate as a one row datafra,/one row matrix
  if(length(t$terr[t$moves==1])>0){ #looking within t, when offspring =1. if a baby at the time, 1 is yes. if adult, offpring=0. find any rows where temperatry t, where it was captured as offpring, and if it is there, save as "natal". and saving the name of teh territory there. if natal=0, not caputred at reproductive age.
    migrant = length(as.character(t$terr[t$moves==1]))
  }else(
    migrant=0
  )
  ncap = nrow(t[t$moves==0,,drop=F]) #find all rows when they were adults. therefore this is the number of captured as adult
  nsubpop = length(unique(t$subpop[t$moves==0])) ##how many territories as an adult (offspring=0) and look at how many unique, what is the length. so how many mounds did the indv occupt as adult
  writeout = c(indvs[i], nonmigrant, ncap, nsubpop) #this is the info to put in new datafra,e. concatenated into a list and addd writeout to end of the out variable
  OUT = rbind(OUT, writeout)
}
rownames(OUT) = seq(1,nrow(OUT),1) #change row names of out to consecutibe numbers in the sequence. if in huge matrix, more convenient as dataframe, so taking column and putting in name that is understandable later
MIG =    data.frame(id=as.numeric(as.character(OUT[,1])), migrant=as.character(OUT[,2]), nmig=as.numeric(as.character(OUT[,3])), nsubpops=as.numeric(as.character(OUT[,4])))
NONMIG = data.frame(id=as.numeric(as.character(OUT[,1])), nonmigrant=as.character(OUT[,2]), ncap=as.numeric(as.character(OUT[,3])), nsubpop=as.numeric(as.character(OUT[,4])))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#THIS IS WHAT IT SHOULD BE

OUT = migrant = NULL #these are creating varilables and setting them to null. take the out variable and add to it at every iteration of the loop. rbind takes what is in out already, add with writeout, and rbind adds another rowm, cbind adds column. so sum and restart loop cuz at the end. null creates variables with nothing in it
indvs = unique(sub2$id)
for(i in 1:length(indvs)){ #iterate 1 to the length of the number of indv in the list
  t = sub2[sub2$id==as.numeric(as.character(indvs[i])),,drop=F] #t is temperatry (convention). a subset of the datafram I am iterating over. so pull out all of rows in field data with thei [articular indiv caputered]. easier to have ind as numeric rather than character. the drop=F is critical. if you subset a datafra,e in R and teh result is just 1 row, R will conver to a list. if you add drop=FALSE, treate as a one row datafra,/one row matrix
  if(length(t$terr[t$moves==1])>0){ #looking within t, when offspring =1. if a baby at the time, 1 is yes. if adult, offpring=0. find any rows where temperatry t, where it was captured as offpring, and if it is there, save as "natal". and saving the name of teh territory there. if natal=0, not caputred at reproductive age.
    migrant = length(as.character(t$terr[t$moves==1]))
  }else(
    migrant=0
  )
  nmig = nrow(t[t$moves==1,,drop=F]) #find all rows when they were adults. therefore this is the number of captured as adult
  nsubpops = length(unique(t$subpop[t$moves==1])) ##how many territories as an adult (offspring=0) and look at how many unique, what is the length. so how many mounds did the indv occupt as adult
  writeout = c(indvs[i], migrant, nmig, nsubpops) #this is the info to put in new datafra,e. concatenated into a list and addd writeout to end of the out variable
  OUT = rbind(OUT, writeout)
}
rownames(OUT) = seq(1,nrow(OUT),1) #change row names of out to consecutibe numbers in the sequence. if in huge matrix, more convenient as dataframe, so taking column and putting in name that is understandable later
MIG = data.frame(id=as.numeric(as.character(OUT[,1])), migrant=as.character(OUT[,2]), nmig=as.numeric(as.character(OUT[,3])), nsubpops=as.numeric(as.character(OUT[,4])))
#"nmig" is the number of captures and "nsubpops" is correct, but only when moves=0. So all the migrants where moves > 0, nmig and nsubpops =0.

#+~+~+~+~+~+~++~+~+~~+~+~+~+~+~+~+~++~+~+~+~+~+~+~++~+~+~++~+~+~+~+~+~++~+~+~+~+~+~++~
#MIG is the dataframe with data only for migrants
#SUBS is the dataframe with data for migrants and non-migrants
#migrant is the value taken out of the dataframe sub2, 
##default: 0 is non-migrant, n is the number of territories for migrants
#ncap is the number of captures for non-migrants
#ck - default: 1 for non-migrant, 0 for migrant. 
## this is the check to make sure that the individuals were subdvided correctly
#nyrs is the number of years a migrant was captured (1 capture per year)
#nsubpops is the number of subpopulations a migrant inhabited
#~+~+~+~+~+~~++~+~+~+~+~++~+~+~+~+~+~+~+~++~+~+~+~+~+~+~+~+~+~+~++~+~+~+~+~+~++~+~+~+~

OUT = migrant = nyrs = ncap = ck = nsubpops = NULL #these are creating varilables and setting them to null. take the out variable and add to it at every iteration of the loop. rbind takes what is in out already, add with writeout, and rbind adds another rowm, cbind adds column. so sum and restart loop cuz at the end. null creates variables with nothing in it
indvs = unique(sub2$id)
for(i in 1:length(indvs)){ #iterate 1 to the length of the number of indv in the list
  t = sub2[sub2$id==as.numeric(as.character(indvs[i])),,drop=F] #t is temperatry (convention). a subset of the datafram I am iterating over. so pull out all of rows in field data with thei [articular indiv caputered]. easier to have ind as numeric rather than character. the drop=F is critical. if you subset a datafra,e in R and teh result is just 1 row, R will conver to a list. if you add drop=FALSE, treate as a one row datafra,/one row matrix
  if(length(t$terr[t$moves==1])>0){ #looking within t, when offspring =1. if a baby at the time, 1 is yes. if adult, offpring=0. find any rows where temperatry t, where it was captured as offpring, and if it is there, save as "natal". and saving the name of teh territory there. if natal=0, not caputred at reproductive age.
    migrant = length(as.character(t$terr[t$moves==1]))
  }else(
    migrant=0
  )
  ncap = nrow(t[t$moves==0,,drop=F]) #find number of captures for nonmigrants
  ck = length(unique(t$subpop[t$moves==0]))
  nyrs = nrow(t[t$moves==1,,drop=F]) #find all rows when they were adults. therefore this is the number of captured as adult
  nsubpops = length(unique(t$subpop[t$moves==1])) ##how many territories as an adult (offspring=0) and look at how many unique, what is the length. so how many mounds did the indv occupt as adult
  writeout = c(indvs[i], migrant, nyrs, nsubpops, ncap, ck) #this is the info to put in new datafra,e. concatenated into a list and addd writeout to end of the out variable
  OUT = rbind(OUT, writeout)
}
rownames(OUT) = seq(1,nrow(OUT),1) #change row names of out to consecutibe numbers in the sequence. if in huge matrix, more convenient as dataframe, so taking column and putting in name that is understandable later
MIG = data.frame(id=as.numeric(as.character(OUT[,1])), migrant=as.character(OUT[,2]), nyrs=as.numeric(as.character(OUT[,3])), nsubpops=as.numeric(as.character(OUT[,4])), ncap=as.numeric(as.character(OUT[,5])), ck=as.numeric(as.character(OUT[,6])))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
##################################################################################3
#just some messin around with the data

#note: syntax is (dependent ~ independent, data)

"what is the relationship between migration and inbreeding?"
inb <- lm(inb ~ moves, data = sub2)
summary(inb)

"what is the relationship between migration and number of mates?"
mate <- lm(nmates ~ moves, data = sub2)
summary(mate)

"what is the relationship between migration and kinship?"
kins <- lm(pkins ~ moves, data = sub2)
summary(kins)
plot(kins)
hist(kins)

kins <- lm(moves ~ (pkins | id) + sex, data = sub2)
summary(kins)

kins <- lm(pkins ~ subpop, data = sub2)
summary(kins)


boxplot(pkins~subpop,sub2)
boxplot(inb~subpop, sub2)
summary(aov(pkins~moves,sub2))


shapiro.test(residuals(lm(pkins~moves,sub2))) #checks to see if data are normal
bartlett.test(pkins~moves,sub2) #checks to see if variances are equal
summary(aov(pkins~moves,sub2))
TukeyHSD(aov(pkins~moves,sub2))
hist(pkins~moves,sub2)

library(lme4)
library(lmerTest)
model = lmer(pkins ~ moves + (1|id), data = sub2, REML = TRUE)
anova(model)

#######################################################################################3

