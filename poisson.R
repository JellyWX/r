library(ggplot2)
library(gridExtra)

gen_pois <- function(){ # function that generates 300 poisson numbers with a mean of 2.5
  pois <- rpois(300,2.5)

  table(factor(pois,levels=0:11)) # puts the numbers into a frequency table which contains levels from 0 to 11
}

total_pois <- function(freq){ # function to calculate the total clinical illnesses in a frequency table
  total <- 0 # sets the total to zero (assignment operators are =, <- and ->)

  rep <- 0
  for(j in freq){
    total <- total + (j * rep) # total gets the original total plus 'j' (the number of colds from the table) times by 'rep' (the number of colds that people in that group experienced)
    rep <- rep + 1 # no ++ or += in R, so to add 1 to a value you must do it like this
  }

  total # instead of using a return method, R has a built-in way to return values simply by typing out the variable with no logical/assignment operators
}

total_pois_4 <- function(freq){ # function to calculate the total of colds, but treating above 4 as 4
  total_a4 <- 0 # variable (a4 for above 4)
  rep <- 0
  for(k in freq){
    total_a4 <- total_a4 + (k * rep)
    if(rep < 4){ # all similar as above, except the 'rep' variable is capped off at 4.
      rep <- rep + 1
    }
  }

  total_a4 # returns
}

virustypes <- function(t){
  retval = c(0,0)
  for(v in 1:t){
    r <- runif(1)
    if(r >= 0.5){
      retval[1] <- retval[1] + 1
      r <- runif(1)
      if(r <= 0.65){
        retval[2] <- retval[2] + 1
      }
    }
  }

  retval
}

e <- as.numeric(gen_pois()) # as.numeric() returns just the frequencies from the table, and not the reference numbers

total <- total_pois(e) # initialise the total variable
total_0 <- e[1] # intialise the total number of 0 colds variable (indexing starts at 1)
total_4 <- sum(e[5:11]) # initialise the total number of 4+ colds variable (indexing using a range; sum adds up all the numbers)
total_a4 <- total_pois_4(e) # initialise the total_a4 variable
groups <- matrix(0,ncol=2,nrow=20) # groups is the variable which contains the sample sizes
colnames(groups) <- c('PLACEBO','DRUG') # add names to the columns

groups.positive <- matrix(0,ncol=2,nrow=20) # groups is the variable which contains the sample sizes
colnames(groups.positive) <- c('PLACEBO','DRUG') # add names to the columns

d <- data.frame( # data for plotting
  x = 0:11,
  y = e
)

ti.placebo.clinical <- c()
ti.drug.clinical <- c() # intialise 2 empty time variables
for(j in 1:total_pois_4(e)){
  r <- runif(1) # runif(1) generates a single random decimal between 0 and 1
  if(r >= 0.75){ # 1 in 4
    r = runif(1)
    if(r <= 0.25){
      groups[1,1] <- groups[1,1] + 1 # increase the first group by 1
      ti.placebo.clinical <- c(ti.placebo.clinical,sample(1:200,1)) # sample(1:200,1) generates 1 random integer in the range 1:200. add it to the time variable for the placebos
    }
  }else{
    r = runif(1)
    if(r <= 0.0625){
      groups[1,2] <- groups[1,2] + 1 # increase the second group by 1
      ti.drug.clinical <- c(ti.drug.clinical,sample(1:200,1))
    }
  }
}
ti.placebo.clinical <- as.numeric(table(factor(ti.placebo.clinical,levels=0:200))) # turn the times into a list of frequencies by feeding it through many functions
ti.drug.clinical <- as.numeric(table(factor(ti.drug.clinical,levels=0:200)))


e <- virustypes(total_pois_4(e))

print(e)
print(e[2])

ti.placebo.positive <- c()
ti.drug.positive <- c() # intialise 2 empty time variables
for(j in 1:e[2]){
  r <- runif(1) # runif(1) generates a single random decimal between 0 and 1
  if(r >= 0.75){ # 1 in 4
    r = runif(1) # generate another float to check if they get an infection
    if(r <= 0.25){
      groups.positive[1,1] <- groups.positive[1,1] + 1 # increase the first group by 1
      ti.placebo.positive <- c(ti.placebo.positive,sample(1:200,1)) # sample(1:200,1) generates 1 random integer in the range 1:200. add it to the time variable for the placebos
    }
  }else{
    r = runif(1)
    if(r <= 0.0625){
      groups.positive[1,2] <- groups.positive[1,2] + 1 # increase the second group by 1
      ti.drug.positive <- c(ti.drug.positive,sample(1:200,1))
    }
  }
}
ti.placebo.positive <- as.numeric(table(factor(ti.placebo.positive,levels=0:200))) # turn the times into a list of frequencies by feeding it through many functions
ti.drug.positive <- as.numeric(table(factor(ti.drug.positive,levels=0:200)))

d.t.drug.positive <- data.frame(
  x = 0:200, # range 0 to 200 for the x axis
  y = cumsum(ti.drug.positive), # cumulative sum of the time, then times by 0.25 to get an approximation for the amount of infections
  grp = 'drug' # name the group 'placebo' (this appears on the side of the step chart)
)

d.t.placebo.positive <- data.frame(
  x = 0:200, # range 0 to 200 for the x axis
  y = cumsum(ti.placebo.positive), # cumulative sum of the time, then times by 0.25 to get an approximation for the amount of infections
  grp = 'placebo' # name the group 'placebo' (this appears on the side of the step chart)
)

d.t.drug.clinical <- data.frame(
  x = 0:200, # range 0 to 200 for the x axis
  y = cumsum(ti.drug.clinical), # cumulative sum of the time, then times by 0.25 to get an approximation for the amount of infections
  grp = 'drug' # name the group 'placebo' (this appears on the side of the step chart)
)

d.t.placebo.clinical <- data.frame(
  x = 0:200,
  y = cumsum(ti.placebo.clinical),
  grp = 'placebo'
)

p <- ggplot(d,aes(x,y)) + # assign data (d) and aesthetics (x,y) to the graph
  geom_point() + # point graph
  expand_limits(x=0,y=0) + # start the axis at exactly 0,0
  scale_x_continuous(expand=c(0,0),limits=c(0,10.5)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,100)) +
  ggtitle('Simulated number of clinical illnesses per family') + # plot title
  xlab('Number of clinical illnesses') + # plot labels
  ylab('Families')

p.t <- ggplot(d.t.placebo.clinical,aes(x,y)) +
  expand_limits(x=0,y=0) +
  scale_x_continuous(expand=c(0,0),limits=c(0,200.5)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,100)) +
  ggtitle('Simulated increase in clinical illness over time') +
  xlab('Days') +
  ylab('Cases of clinical illness')

p.t2 <- ggplot(d.t.placebo.clinical,aes(x,y)) + # this graph is rendered immediately
  expand_limits(x=0,y=0) +
  scale_x_continuous(expand=c(0,0),limits=c(0,200.5)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,100)) +
  ggtitle('Simulated increase in clinical illness over time') +
  xlab('Days') +
  ylab('Cases of clinical illness')

p.t2 <- p.t2 + geom_step(data=d.t.placebo.clinical,aes(x=x,y=y,color=grp)) + geom_step(data=d.t.drug.clinical,aes(x=x,y=y,color=grp))
pdf(file='clinical_illness1.pdf') # open pdf of this name
print(p.t2) # print to pdf
dev.off() # close file

p.t3 <- ggplot(d.t.placebo.positive,aes(x,y)) + # this graph is rendered immediately
  expand_limits(x=0,y=0) +
  scale_x_continuous(expand=c(0,0),limits=c(0,200.5)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,100)) +
  ggtitle('Simulated increase in positive rhinovirus/coronavirus over time') +
  xlab('Days') +
  ylab('Cases of rhinovirus/coronavirus')

p.t3 <- p.t3 + geom_step(data=d.t.placebo.positive,aes(x=x,y=y,color=grp)) + geom_step(data=d.t.drug.positive,aes(x=x,y=y,color=grp))
pdf(file='positive_illness1.pdf') # open pdf of this name
print(p.t3) # print to pdf
dev.off() # close file


for(i in 2:20){ # repeats x times and draws to plot
  e <- as.numeric(gen_pois())

  ## reassigning variables: ##
  d2 <- data.frame(
    x = 0:11,
    y = e
  )

  total_a4 <- (total_a4 + total_pois_4(e)) / 2
  total_4 <- c(total_4, sum(e[5:11]))
  total_0 <- c(total_0, e[1]) # 'c' is the function for a vector or a list of numbers
  total <- (total + total_pois(e)) / 2 # averages
  d <- (d + d2) / 2 # averages
  p <- p + geom_point(data=d2) # add points to the graph for the new data
  old_ti.placebo.positive <- ti.placebo.positive
  old_ti.drug.positive <- ti.drug.positive
  old_ti.placebo.clinical = ti.placebo.clinical
  old_ti.drug.clinical = ti.drug.clinical
  ti.placebo.clinical = c()
  ti.drug.clincal = c()

  for(j in 1:total_pois_4(e)){ # group assigning and time sampling
    r <- runif(1)
    if(r >= 0.75){
      r = runif(1) # generate another float to check if they get an infection
      if(r <= 0.25){
        groups[i,1] <- groups[i,1] + 1
        ti.placebo.clinical <- c(ti.placebo.clinical,sample(1:200,1))
      }
    }else{
      r = runif(1)
      if(r<=0.0625){
        groups[i,2] <- groups[i,2] + 1
        ti.drug.clinical <- c(ti.drug.clinical,sample(1:200,1))
      }
    }
  }
  ti.placebo.clinical <- as.numeric(table(factor(ti.placebo.clinical,levels=0:200)))
  ti.drug.clinical <- as.numeric(table(factor(ti.drug.clinical,levels=0:200)))

  ti.placebo.clinical <- (ti.placebo.clinical + old_ti.placebo.clinical) / 2
  ti.drug.clinical <- (ti.drug.clinical + old_ti.drug.clinical) / 2 # averaging

  e <- virustypes(total_pois_4(e))

  ti <- c()
  ti2 <- c() # give these variables different names and then recreate them as empty

  for(j in 1:e[2]){ # group assigning and time sampling
    r <- runif(1)
    if(r >= 0.75){
      r = runif(1)
      if(r <= 0.25){
        groups.positive[i,1] <- groups.positive[i,1] + 1
        ti.placebo.positive <- c(ti.placebo.positive,sample(1:200,1))
      }
    }else{
      r = runif(i)
      if(r <= 0.0625){
        groups.positive[i,2] <- groups.positive[i,2] + 1
        ti.drug.positive <- c(ti.drug.positive,sample(1:200,1))
      }
    }
  }
  ti.placebo.positive <- as.numeric(table(factor(ti.placebo.positive,levels=0:200)))
  ti.drug.positive <- as.numeric(table(factor(ti.drug.positive,levels=0:200)))

  ti.placebo.positive <- (ti + old_ti.placebo.positive) / 2
  ti.drug.positive <- (ti2 + old_ti.drug.positive) / 2 # averaging

}

p <- p + geom_smooth(data=d,method='loess') # make a smooth curve
#p <- p + geom_histogram(data=d,stat='identity',alpha=0.15) # unhash this line to draw a histogram

d.t <- data.frame(
  x = 0:200,
  y = cumsum(ti.placebo.clinical), # cumulative sums and set this up properly
  grp = 'placebo'
)

d.t2 <- data.frame(
  x = 0:200,
  y = cumsum(ti.drug.clinical), # times down for the expected effectiveness in the drug group
  grp = 'drug'
)

d.tt <- data.frame(
  x = 0:200,
  y = d.t2$y + d.t$y,
  grp = 'all'
)

p.t <- p.t + geom_step(data=d.t,aes(x=x,y=y,color=grp)) # add a step plot for the palcebo
p.t <- p.t + geom_step(data=d.t2,aes(x=x,y=y,color=grp)) # add a step plot for the drug
p.t <- p.t + geom_step(data=d.tt,aes(x=x,y=y,color=grp)) # add a step plot for the total

total <- format(total,digits=4,nsmall=1) # finalise the totals (limit the digits, force it to 1 d.p)
total_a4 <- format(total_a4,digits=4,nsmall=1)

placebo = groups[,1] # break the groups into 2 variables for printing
drug = groups[,2]

positive.placebo = groups.positive[,1] # break the groups into 2 variables for printing
positive.drug = groups.positive[,2]

# summarise

sink('summary.txt',append=F,split=T) # text file summary table

t <- matrix('',nrow=13,ncol=1) # new matrix (2d list) for the summary
colnames(t) <- c('SUMMARY')
rownames(t) <- c('','','','','','','','','','','','','')

close = which(abs(d.tt$y-mean(d.tt$y))==min(abs(d.tt$y-mean(d.tt$y)))) # pick out the value from the cumulative days that's closest to the mean
day = which(d.tt==close) # pick the day that the closest occured on

close_l = which(abs(d.tt$y-mean(d.tt[1:day,2]))==min(abs(d.tt$y-mean(d.tt[1:day,2])))) # same again but split in half further
day_l = which(d.tt==close_l)

close_h = which(abs(d.tt$y-mean(d.tt[day:200,2]))==min(abs(d.tt$y-mean(d.tt[day:200,2]))))
day_h = which(d.tt==close_h)

quart = (close_h - close_l) / 2 # deviation

# 'paste' is R's way of joining 2 strings. add these pasted strings to the matrix 't'
t[1,] <- paste('The predicted total of families that get no colds is',format(median(total_0),nsmall=1),'±',format((quantile(total_0)[4]-quantile(total_0)[2])/2,nsmall=3))
t[2,] <- paste('The predicted total of families that get more than 4 colds is',format(median(total_4),nsmall=1),'±',format((quantile(total_4)[4]-quantile(total_4)[2])/2,nsmall=3))
t[3,] <- paste('The predicted total number of colds is',total)
t[4,] <- paste('The predicted total of colds, treating above 4 as 4, is',total_a4)
t[5,] <- paste('The predicted clinical illnesses in the placebo group is',format(median(placebo)),'±',format((quantile(placebo)[4]-quantile(placebo)[2])/2))
t[6,] <-paste('The predicted clinical illnesses in the drug group is',format(median(drug)),'±',format((quantile(drug)[4]-quantile(drug)[2])/2))
t[7,] <- paste('The predicted clinical illnesses in total is',format(median(drug+placebo)),'±',format((quantile(drug+placebo)[4]-quantile(drug+placebo)[2])/2))
t[8,] <- paste('The predicted rhino/corona virus positive in the placebo group is',format(median(positive.placebo)),'±',format((quantile(positive.placebo)[4]-quantile(positive.placebo)[2])/2))
t[9,] <- paste('The predicted rhino/corona virus positive in the drug group is',format(median(positive.drug)),'±',format((quantile(positive.drug)[4]-quantile(positive.drug)[2])/2))
t[10,] <- paste('The predicted rhino/corona virus positive total is',format(median(positive.drug+positive.placebo)),'±',format((quantile(positive.placebo+positive.drug)[4]-quantile(positive.placebo+positive.drug)[2])/2))
t[11,] <- paste('The predicted time for half the clinical illnesses to occur is',day,'days ±',quart)
t[12,] <- paste('The predicted time from half to all infections is',200-day,'days')
t[13,] <- '20 iterations were ran in this simulation'

# commandline prints

print('+--------------------------------------------------------------------------------------------+')
print('|                                          SUMMARY                                           |')
print('|                        results are mean/median ± interquartile range                       |')
print('|--------------------------------------------------------------------------------------------|')
print(paste('|',format(t[1,],width=90,justify='c'),'|'))
print(paste('|',format(t[2,],width=90,justify='c'),'|'))
print('|--------------------------------------------------------------------------------------------|')
print(paste('|',format(t[3,],width=90,justify='c'),'|'))
print(paste('|',format(t[4,],width=90,justify='c'),'|'))
print('|--------------------------------------------------------------------------------------------|')
print(paste('|',format(t[5,],width=90,justify='c'),'|'))
print(paste('|',format(t[6,],width=90,justify='c'),'|'))
print(paste('|',format(t[7,],width=90,justify='c'),'|'))
print('|--------------------------------------------------------------------------------------------|')
print(paste('|',format(t[8,],width=90,justify='c'),'|'))
print(paste('|',format(t[9,],width=90,justify='c'),'|'))
print(paste('|',format(t[10,],width=90,justify='c'),'|'))
print('|--------------------------------------------------------------------------------------------|')
print(paste('|',format(t[11,],width=90,justify='c'),'|'))
print(paste('|',format(t[12,],width=90,justify='c'),'|'))
print('+--------------------------------------------------------------------------------------------+')
print(t[13,])

pdf(file='illness_frequencies.pdf') # draw to plot
print(p)
dev.off()

pdf(file='illness_times.pdf')
print(p.t)
dev.off()

pdf(file='summary.pdf') # pdf summary table
grid.table(t) # add the matrix 't' to a pdf file
dev.off()
