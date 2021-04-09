#KRat pedigree/subpop data
#Gina Lamka - Started Spring 2021

"Main Question: How do migrants contribute to genetic diversity of subpopulations?"

"Background information
  I have a 17 generation pedigree for banner-tailed kangaroo rats from southeast Arizona.
  The population is subdivided into 4 distinct subpopulations distributed along a volcanic cinder cone.
  I am interested in understanding how migrants affect the fitness and genetic diversity of these subpopulations"

"First steps:
  1. Identify subpopulation mounds
  2. Find which individuals migrated from one subpopulation to another
  3. Compare migrant and non-migrant genomes and reproductive fitness
  
Questions I want to answer with these analyses
 Are there immmediate fitness costs (offspring, mates, longevity) of moving? 
 Do migrant genotypes lead to increased offspring fitness?
 Do migrants disproporionately contribute to subpopulation genetic diversity (decrease inbreeding)?
"

## open file with data
setwd("C:/Users/HP/Box/New Computer/Auburn/Data") #set working directory in the Rstudio folder on my computer and box drive

field = read.table("KRATP.csv", header=T, sep=",")
data  = read.table("krat_pedigree_plus.csv", header=T, sep=",")
kins  = read.table("allkins.csv", sep=",", header=TRUE)
sub   = read.table("krat_pedinfo_years.csv", header=TRUE, sep=",")

#adjust measurements to zero to 1200
field$adjlat  = field$lat - min(field$lat, na.rm=TRUE)
field$adjlong = field$long - min(field$long, na.rm=TRUE)

#assign subpopulation 
field$subpop = rep(0, nrow(field))
field$subpop[field$adjlong<425] = "R2"
field$subpop[field$adjlong<660 & field$adjlong>425] = "SSW"
field$subpop[field$adjlong>660 & field$adjlat<690] = "R1W"
field$subpop[field$adjlat>=690] = "R1E"
plot(-100,-100, xlim=c(0,1200), ylim=c(0,1200))
points(field$adjlat[field$subpop=="R2"],  field$adjlong[field$subpop=="R2"],  pch=19, col="darkorchid3")
points(field$adjlat[field$subpop=="SSW"], field$adjlong[field$subpop=="SSW"], pch=19, col="chartreuse3")
points(field$adjlat[field$subpop=="R1W"], field$adjlong[field$subpop=="R1W"], pch=19, col="dodgerblue3")
points(field$adjlat[field$subpop=="R1E"], field$adjlong[field$subpop=="R1E"], pch=19, col="firebrick3")
abline(v=c(700), h=c(425, 660))

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

#IDNUMB is the number for the ID the square brackets are subsetting row, column so all rows with this field ID
field[field$id==919,]
field[field$id==1062,]
field[field$id==1635,]
field[field$id==1907,]
sub2[sub2$id==1907,]
sub2[sub2$id==4074,]
sub2[sub2$id==4072,]

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

OUT = migrant = NULL #these are creating variables and setting them to null. 
#takes the variable and adds to it at every iteration of the loop. 
indvs = unique(sub2$id)
for(i in 1:length(indvs)){ #iterate 1 to the length of the number of indv in the list
  t = sub2[sub2$id==as.numeric(as.character(indvs[i])),,drop=F] #t is temporary and is lost at the end of the function
  #defines a subset of the dataframe I am iterating over.
  #in this case, it pulls out all rows for each individual.
  #the drop=F is critical to make sure R treats as a matrix rather than one long list.
  if(length(t$terr[t$moves==1])>0){ #looking within t, finds when moves = 1
    migrant = length(as.character(t$terr[t$moves==1])) #prints number of rows of migrants
  }else(
    migrant=0
  )
  nmig = nrow(t[t$moves==1,,drop=F]) #find the number of times captured as adult
  nsubpops = length(unique(t$subpop[t$moves==1])) ##how many subpopulations did the indv occupy as an adult
  writeout = c(indvs[i], migrant, nmig, nsubpops) #this is the info to put in the new dataframe. 
  OUT = rbind(OUT, writeout)
  #rbind takes what is in out already, add with writeout
}
rownames(OUT) = seq(1,nrow(OUT),1) 
#change row names of OUT to consecutive numbers in the sequence. 
#if in huge matrix, more convenient as dataframe, so taking column and putting in a header that is more understandable later
MIG = data.frame(id=as.numeric(as.character(OUT[,1])), migrant=as.character(OUT[,2]), nmig=as.numeric(as.character(OUT[,3])), nsubpops=as.numeric(as.character(OUT[,4])))

#"nmig" is the number of captures and "nsubpops" is correct, but only when moves>0. So all the migrants where moves = 0, nmig and nsubpops =0.

table(MIG$nmig)
table(MIG$nsubpops)
table(MIG$migrant)

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

OUT = migrant = nyrs = ncap = ck = nsubpops = NULL 
indvs = unique(sub2$id)
for(i in 1:length(indvs)){ 
  t = sub2[sub2$id==as.numeric(as.character(indvs[i])),,drop=F] 
  if(length(t$terr[t$moves==1])>0){ 
    migrant = length(as.character(t$terr[t$moves==1]))
  }else(
    migrant=0
  )
  ncap = nrow(t[t$moves==0,,drop=F]) #find number of captures for non-migrants
  ck = length(unique(t$subpop[t$moves==0])) #another check to make sure migrants and non-migrants are classified correctly
  nyrs = nrow(t[t$moves==1,,drop=F]) #number of years captured for migrants
  nsubpops = length(unique(t$subpop[t$moves==1])) #number of subpops for migrants
  writeout = c(indvs[i], migrant, nyrs, nsubpops, ncap, ck) 
  OUT = rbind(OUT, writeout)
}
rownames(OUT) = seq(1,nrow(OUT),1) 
ALL = data.frame(id=as.numeric(as.character(OUT[,1])), migrant=as.character(OUT[,2]), nyrs=as.numeric(as.character(OUT[,3])), nsubpops=as.numeric(as.character(OUT[,4])), ncap=as.numeric(as.character(OUT[,5])), ck=as.numeric(as.character(OUT[,6])))

table(ALL$nsubpops)
table(ALL$migrant)
table(ALL$ncap)
table(ALL$ck)
table(ALL$nyrs)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
##################################################################################3
#just some messin' around with the data using a random effects models

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


kins <- lm(moves ~ (pkins | id) + sex, data = sub2)
summary(kins)

kins <- lm(pkins ~ subpop, data = sub2)
summary(kins)


boxplot(pkins~subpop,sub2)
boxplot(inb~subpop, sub2)
summary(aov(pkins~moves,sub2))


shapiro.test(residuals(lm(pkins~moves,sub2))) #checks to see if data are normal
bartlett.test(pkins~moves,sub2) #checks to see if variances are equal
summary(aov(pkins~moves,sub2)) #runs an ANOVA


library(lme4)
library(lmerTest)
model = lmer(pkins ~ moves + (1|id), data = sub2, REML = TRUE)
anova(model)

#######################################################################################3

