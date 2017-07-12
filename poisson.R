library(ggplot2)
library(gridExtra)

gen_pois <- function(){
  pois <- rpois(300,2.5)

  table(factor(pois,levels=0:11))
}

total_pois <- function(freq){
  total <- 0

  rep <- 0
  for(j in freq){
    total <- total + (j * rep)
    rep <- rep + 1
  }

  total
}

total_pois_4 <- function(freq){
  total_a4 <- 0
  rep <- 0
  for(k in freq){
    total_a4 <- total_a4 + (k * rep)
    if(rep < 4){
      rep <- rep + 1
    }
  }

  total_a4
}

grouping <- function(freq,rep){
  for(j in 1:total_pois_4(freq)){
    r <- runif(1)
    if(r >= 0.75){
      groups[rep,1] <- groups[rep,1] + 1
    }else{
      groups[rep,2] <- groups[rep,2] + 1
    }
  }
}

allocate_time <- function(freq){
  t <- c()
  for(k in 2:length(freq)){
    for(l in 1:freq[k]){
      r <- runif(k - 1)
      r <- sort(r,decreasing=T)
      r <- r[1:4]
      r <- r[!is.na(r)]
      r <- round(r * 200)
      t <- c(t,r)
    }
  }

  t.2 <- table(factor(t,levels=0:200))
  t.2 <- cumsum(as.numeric(t.2))
  t.2
}

e <- as.numeric(gen_pois())

total <- total_pois(e)
total_0 <- e[1]
total_4 <- sum(e[5:11])
total_a4 <- total_pois_4(e)
groups <- matrix(0,ncol=2,nrow=20)
colnames(groups) <- c('PLACEBO','DRUG')

d <- data.frame(
  x = 0:11,
  y = e
)

ti <- data.frame(
  x = 0:200,
  y = allocate_time(e)
)

for(j in 1:total_pois_4(e)){
  r <- runif(1)
  if(r >= 0.75){
    groups[1,1] <- groups[1,1] + 1
  }else{
    groups[1,2] <- groups[1,2] + 1
  }
}

p <- ggplot(d,aes(x,y)) +
  geom_point() +
  expand_limits(x=0,y=0) +
  scale_x_continuous(expand=c(0,0),limits=c(0,10.5)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,100)) +
  ggtitle('Simulated number of clinical illnesses per family') +
  xlab('Number of clinical illnesses') +
  ylab('Families')

p.t <- ggplot(ti,aes(x,y)) +
  expand_limits(x=0,y=0) +
  scale_x_continuous(expand=c(0,0),limits=c(0,200.5)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1000)) +
  ggtitle('Simulated increase in clinical illness over time') +
  xlab('Days') +
  ylab('Cases of clinical illness')

for(i in 2:20){ # repeats x times and draws to plot
  e <- as.numeric(gen_pois())

  d2 <- data.frame(
    x = 0:11,
    y = e

  )

  ti2 <- data.frame(
    x = 0:200,
    y = as.numeric(allocate_time(e))
  )

  total_a4 <- (total_a4 + total_pois_4(e)) / 2
  total_4 <- c(total_4, sum(e[5:11]))
  total_0 <- c(total_0, e[1])
  total <- (total + total_pois(e)) / 2
  d <- (d + d2) / 2
  ti <- (ti + ti2) / 2
  p <- p + geom_point(data=d2)

  for(j in 1:total_pois_4(e)){
    r <- runif(1)
    if(r >= 0.75){
      groups[i,1] <- groups[i,1] + 1
    }else{
      groups[i,2] <- groups[i,2] + 1
    }
  }
}

p <- p + geom_smooth(data=d,method='loess')
#p <- p + geom_histogram(data=d,stat='identity',alpha=0.15) # unhash this line to draw a histogram

p.t <- p.t + geom_step(data=ti)

total <- format(total,digits=4,nsmall=1)
total_a4 <- format(total_a4,digits=4,nsmall=1)

groups[,1] <- groups[,1] * 0.25 # directly quarters the placebo group
groups[,2] <- groups[,2] * 0.0625 # splits the treatment group

placebo = groups[,1]
drug = groups[,2]

# summarise

sink('summary.txt',append=F,split=T) # text file summary table

t <- matrix('',nrow=7,ncol=1)
colnames(t) <- c('SUMMARY')
rownames(t) <- c('','','','','','','')

t[1,] <- paste('The predicted total of families that get no colds is',format(median(total_0),nsmall=1),'±',format((quantile(total_0)[4]-quantile(total_0)[2])/2,nsmall=3))
t[2,] <- paste('The predicted total of families that get more than 4 colds is',format(median(total_4),nsmall=1),'±',format((quantile(total_4)[4]-quantile(total_4)[2])/2,nsmall=3))
t[3,] <- paste('The predicted total number of colds is',total)
t[4,] <- paste('The predicted total of colds, treating above 4 as 4, is',total_a4)
t[5,] <- paste('The predicted clinical illnesses in the placebo group is',format(median(placebo),digits=4,nsmall=2),'±',format((quantile(placebo)[4]-quantile(placebo)[2])/2,nsmall=5,digits=6))
t[6,] <- paste('The predicted clinical illnesses in the drug group is',format(median(drug),digits=4,nsmall=2),'±',format((quantile(drug)[4]-quantile(drug)[2])/2,nsmall=5,digits=6))
t[7,] <- '20 iterations were ran in this simulation'

# commandline prints

print('+-----------------------------------------------------------------------------+')
print('|                                   SUMMARY                                   |')
print('|-----------------------------------------------------------------------------|')
print(paste('|',format(t[1,],width=75,justify='c'),'|'))
print(paste('|',format(t[2,],width=75,justify='c'),'|'))
print('|-----------------------------------------------------------------------------|')
print(paste('|',format(t[3,],width=75,justify='c'),'|'))
print(paste('|',format(t[4,],width=75,justify='c'),'|'))
print('+-----------------------------------------------------------------------------+')
print(paste('|',format(t[5,],width=75,justify='c'),'|'))
print(paste('|',format(t[6,],width=75,justify='c'),'|'))
print('+-----------------------------------------------------------------------------+')
print(t[7,])

pdf(file='illness_frequencies.pdf') # draw to plot
print(p)
dev.off()

pdf(file='illness_times.pdf')
print(p.t)
dev.off()

pdf(file='summary.pdf') # pdf summary table
grid.table(t)
dev.off()
