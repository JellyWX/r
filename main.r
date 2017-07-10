library(MASS)
library(reshape2)

d <- data.frame(
  player = c('Allen','Clara','Henry','Barry','Andy','Edward'),
  fav = c('Six Siege','CS:GO','Fallout 4','Rust','CS:GO','PU:BG'),
  id = c(1:6),
  kd = c(0.85,0.33,1.03,0.99,1.08,0.12)
)

print(d)

d$platform <- c('Xbox','PC','Xbox','PC','PC','PC')

print(d)

d.kd <- d[order(d$kd),]

print(d.kd)

d.kd.mean <- mean(d$kd)
d.kd.mean <- format(d.kd.mean,digits=3)

print(paste('Avergae k/d is',d.kd.mean))

pie(d$)