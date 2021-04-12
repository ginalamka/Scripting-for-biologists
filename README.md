# Project Proposal

For my class project, I would like to create a script to analyze pedigree data on a population of kangaroo rats in Arizona. Banner-tailed kangaroo rat demography and dispersal have been monitored since 1979 on a 64-ha study site in the Chihuahuan desert. I plan to use R to examine a 17-generation pedigree, so that I can understand the fitness consequences of dispersal behavior among the 4 metapopulations studied. I plan to use this project, with additional analyses, as a chapter in my dissertation.

For more information on the study system, see [doi: 10.1111/j.1365-294X.2011.05002.x](https://onlinelibrary.wiley.com/doi/10.1111/j.1365-294X.2011.05002.x)

## Main Question: How do migrants contribute to genetic diversity of subpopulations?

### Background information
  I have a 17 generation pedigree for banner-tailed kangaroo rats from southeast Arizona.
  
  The population is subdivided into 4 distinct subpopulations distributed along a volcanic cinder cone.
  
  I am interested in understanding how migrants affect the fitness and genetic diversity of these subpopulations

### First steps:
  1. Identify subpopulation mounds
  2. Find which individuals migrated from one subpopulation to another
  3. Compare migrant and non-migrant genomes and reproductive fitness
  
### Questions I want to answer with these analyses
 Are there immmediate fitness costs (offspring, mates, longevity) of moving? 
 
 Do migrant genotypes lead to increased offspring fitness?
 
 Do migrants disproporionately contribute to subpopulation genetic diversity (decrease inbreeding)?

### The code

First, R requires that you set a working directory on your local system, and open the file with the data
```setwd("Drive:folder/folder/folder")```

In my case, I used
```
setwd("C:/Users/HP/Box/New Computer/Auburn/Data") #set working directory in the Rstudio folder on my computer and box drive
```

Then, you must add in the data that will be used for the analyses. The syntax is ```data_name = read.table("FileName.csv", header=T, sep=",")```
The ```header = T``` occurs when you have column headers in your dataset.

In this case of the kangaroo rat pedigree, I used these data
```
field = read.table("KRATP.csv", header=T, sep=",")
data  = read.table("krat_pedigree_plus.csv", header=T, sep=",")
kins  = read.table("allkins.csv", sep=",", header=TRUE)
sub   = read.table("krat_pedinfo_years.csv", header=TRUE, sep=",")
```

As an artifact of the latitude and longitude data collected, I had to adjust them, which I used the code
```
field$adjlat  = field$lat - min(field$lat, na.rm=TRUE)
field$adjlong = field$long - min(field$long, na.rm=TRUE)
```
This, however, may not be necessary depending on how the data are collected.

Notice the ```data$variable``` syntax is used to select a column within the dataset.


Then, I want to assign the subpopulation. Depending on the subpopulation structure, these data will be changed. 
In this case, I knew the subpopulation division, so I was able to classify the mounds into four subpopulations: R2, SSW, R1W, R1E
```
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
```

Next, I needed to identify the migrants. Here, I used the ```dplyr``` package. 
You can install packages in R using the ```Install``` function within the ```packages``` tab. 
Then, they can be recalled within the script using the function ```library(package)```

In my case, I used
```
library(dplyr)
```
Then, within that package, I was able to use the ```mutate``` function to identify the number of distinct subpopulations using the function
```
sub2 <- sub %>%
  group_by(id) %>%
  mutate(moves = n_distinct(subpop)-1)
```
This writes the ```sub2``` dataframe with the column ```moves``` that groups the function by ```id``` and defines the number of times an individual moved subpopulations. 

You can then add that to know how many rows of data have individuals that have moved
```
sum(sub2$moves)
```
Notice you can use the ```summarise``` function to spit out the column in the console rather than create a new dataframe.

Similarly, you can create a table with the same information using the syntax ```table(data$variable)```
```
table(sub2$moves)
```

You can also extract all data for a particular individual using the code
```
sub2[sub2$id==919,]
#or
field[field$id==1062,]
```

Then, we want to create a new dataframe using information from the pedigree. 
```OUT = variable = NULL``` creates a variable in the new dataset with no information. Using a for loop, we can
calculate the data we need and put it into a matrix using ```OUT = rbind(OUT, writeout)```

For more information on writing for loops in R, see [these instructions](https://r-coder.com/for-loop-r/#:~:text=For%20loop%20in%20R%201%20For%20loop%20R,for%20loop.%20...%207%20Parallel%20for%20loop.%20)

```
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
```
When looking at the ```MIG``` dataframe we just created, we can see that the ```nmig``` is the number of captures and ```nsubpops``` is correct for the number of subpopulations captured in, however that is only true when moves > 0, and these variables = 0 when moves = 0. 

Instead, I would like information on both migrants and non-migrants, so I use the code 
```
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
```
Notice 

```MIG``` is the dataframe with data only for migrants

```ALL``` is the dataframe with data for migrants and non-migrants

```migrant``` is the value taken out of the dataframe ```sub2```, 
  default: 0 is non-migrant, n is the number of territories for migrants
  
```ncap``` is the number of captures for non-migrants

```ck``` - default: 1 for non-migrant, 0 for migrant. 
  this is the check to make sure that the individuals were subdvided correctly
  
```nyrs``` is the number of years a migrant was captured (1 capture per year)

```nsubpops``` is the number of subpopulations a migrant inhabited


Then, these data can be understood and checked within a table
```table(ALL$nsubpops)
table(ALL$migrant)
table(ALL$ncap)
table(ALL$ck)
table(ALL$nyrs)
```

### More messing around with the data
Then, using the data that were collected, we can examine relationships between variables using random effects models. 
The syntax is ```name <- lm(dependent ~ independent, data)```
The results can be recalled using ```summary(name)```


Some examples within this dataset include
```
"what is the relationship between migration and inbreeding?"
inb <- lm(inb ~ moves, data = sub2)
summary(inb)

"what is the relationship between migration and number of mates?"
mate <- lm(nmates ~ moves, data = sub2)
summary(mate)

"what is the relationship between migration and kinship?"
kins <- lm(pkins ~ moves, data = sub2)
summary(kins)
```

The data can also be visualized using ```boxplot(dependent ~ independent, data)```
For example
```
boxplot(pkins~subpop,sub2)
boxplot(inb~subpop, sub2)
```
