data <- read.csv('data.csv',header=TRUE,sep=';')

data <- data[data$platform=='PC',]
data <- data[data$role=='Defender',]

data$skillrank <- NULL
data$dateid <- NULL
data$secondarygadget <- NULL
data$secondaryweapon <- NULL
data$platform <- NULL
data$role <- NULL

ad <- data[order(data$nbpicks),]

ad$kd <- format(ad$nbkills / ad$nbdeaths,digits=2)
ad$wl <- format(ad$nbwins / (ad$nbpicks - ad$nbwins),digits=2)

ad$nbwins <- NULL
ad$nbpicks <- NULL
ad$nbkills <- NULL
ad$nbdeaths <- NULL

ad <- ad[order(ad$kd),]

#print(ad)

jtf2 <- ad[grepl('JTF',ad$operator),]
spetznas <- ad[grepl('SPETSNAZ',ad$operator),]
sat <- ad[grepl('SAT',ad$operator),]
swat <- ad[grepl('SWAT',ad$operator),]
gign <- ad[grepl('GIGN',ad$operator),]
sas <- ad[grepl('SAS',ad$operator),]
geo <- ad[grepl('G.E.O',ad$operator),]
gsg9 <- ad[grepl('GSG9',ad$operator),]


print(nrow(jtf2))