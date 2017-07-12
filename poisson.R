library(ggplot2)

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

e <- as.numeric(gen_pois())
total <- total_pois(e)
total_0 <- e[1]
total_4 <- sum(e[5:11])
total_a4 <- total_pois_4(e)

d <- data.frame(
  x = 0:11,
  y = e
)

p <- ggplot(d,aes(x,y)) +
  geom_point() +
  expand_limits(x=0,y=0) +
  scale_x_continuous(expand=c(0,0),limits=c(0,10.5)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,100))

for(i in 1:20){ # repeats x times and draws to plot
  e <- as.numeric(gen_pois())

  d2 <- data.frame(
    x = 0:11,
    y = e
  )

  total_a4 <- (total_a4 + total_pois_4(e)) / 2
  total_4 <- c(total_4, sum(e[5:11]))
  total_0 <- c(total_0, e[1])
  total <- (total + total_pois(e)) / 2
  d <- (d + d2) / 2
  p <- p + geom_point(data=d2)
}

p <- p + geom_smooth(data=d,method='loess')
#p <- p + geom_histogram(data=d,stat='identity',alpha=0.15) # unhash this line to draw a histogram

total <- format(total,digits=4,nsmall=1)
total_a4 <- format(total_a4,digits=4,nsmall=1)

# summarise
print('+-----------------------------------------------------------------------------+')
print('|                                   SUMMARY                                   |')
print('|-----------------------------------------------------------------------------|')
print(paste('|The predicted total of families that get no colds is',median(total_0),'±',format((quantile(total_0)[4]-quantile(total_0)[2])/2,nsmall=1),'               |'))
print(paste('|The predicted total of families that get more than 4 colds is',median(total_4),'±',format((quantile(total_4)[4]-quantile(total_4)[2])/2,nsmall=1),'      |'))
print('|-----------------------------------------------------------------------------|')
print(paste('|The predicted total number of colds is',total,'                                |'))
print(paste('|The predicted total of colds, treating above 4 as 4, is',total_a4,'               |'))
print('+-----------------------------------------------------------------------------+')

pdf(file='plot.pdf') # draw to plot
print(p)
dev.off()
