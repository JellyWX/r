pois = rpois(300,2.5)

pdf(file='hist.pdf')
hist(pois)
dev.off()

pdf(file='plot.pdf')

plot(pois)
