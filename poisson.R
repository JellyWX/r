library(ggplot2)

gen_pois <- function(){
  pois <- rpois(300,2.5)
  a <- c(0,0,0,0,0,0,0,0,0,0,0)
  b <- 0

  for(i in pois){
    if(i > 0){
      a[i] <- a[i] + 1
    }else{
      b <- b + 1
    }
  }

  c(b,a)
}

e = gen_pois()

d <- data.frame(
  y = e,
  x = 0:(length(e) - 1)
)

p <- ggplot(d,aes(x,y)) +
  geom_point() +
  expand_limits(x=0,y=0) +
  scale_x_continuous(expand=c(0,0),limits=c(0,10.5)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,100))

for(i in 1:10){ # repeats x times and draws to plot
  e = gen_pois()

  d2 <- data.frame(
    y = e,
    x = 0:(length(e) - 1)
  )

  d <- (d + d2) / 2
  p <- p + geom_point(data=d2)
}

p <- p + geom_smooth(data=d,method='loess')

pdf(file='plot.pdf')
print(p)
dev.off()
