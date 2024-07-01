########################################################################################################################
## INQUISITION IN GIAVENO (1335)
## (6) Saturation graph
## R script written by Jose Luis Estevez (Vaestoliitto)
## Date: Nov 1st, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(data.table);library(ggplot2);library(survival)

# DATA LOADING 
rm(list=ls())
load('data/data5.RData')

rm(list=setdiff(ls(),c('denunciations','incriminated_nodes','N_sample')))

########################################################################################################################

# First, let's remove those cases when the same person reported the same target more than once
denunciations[,tie := paste(source,target,sep='-')]
denunciations <- denunciations[order(time,decreasing = FALSE)]
denunciations <- denunciations[!duplicated(tie)]

#################################

# Now, the first task is to classify the denunciations reported as either "first", "second", or "third or more" mention
# For this, let's create a matrix with each person as row, and each column as a day
ids <- incriminated_nodes[,id] # these are the rows
ids <- ids[order(ids)] # order the IDs

days <- range(denunciations[,time]) # these are the first and last day
days[1] <- days[1] - 24*60*60 # for convenience, let's subtract a day to the first date
days <- seq(min(days),max(days),by='days') # now let's obtain all the days between the first minus one to the last day

# Matrix object, filled up with zeroes
mtx <- matrix(0,nrow=length(ids),ncol=length(days),
              dimnames = list(ids,as.character(days)))

# Now, let's dispose the denunciations reported in the matrix (remember, the rows are the denunciation targets)
denunciations[,day := as.character(time)] # turn days into a character class 

for(i in 1:nrow(denunciations)){
  target <- denunciations$target[i]
  day <- denunciations$day[i]
  mtx[target,day] <- mtx[target,day] + 1
}

# Now that we have the denunciations disposed in a matrix, let's obtained the cumulative sum by row
for(i in 1:nrow(mtx)){
  mtx[i,] <- cumsum(mtx[i,]) # the cumulative sum
}

#################################

# Using this, we can classify the denunciations based on the number of mentions to the same target the day before
denunciations[,daybefore := as.character(time - 24*60*60)] # day previous to the deposition
denunciations[,prevmentions := NA] # vector with previous mentions

for(i in 1:nrow(denunciations)){
  target <- denunciations$target[i]
  prevday <- denunciations$daybefore[i]
  denunciations$prevmentions[i] <- mtx[target,prevday]
}

denunciations[,type := ifelse(prevmentions == 0,'First mention',
                               ifelse(prevmentions == 1,'Second mention','Third or subsequent mention'))]
# Turn it into a factor
denunciations[,type := factor(type,levels=c('First mention','Second mention','Third or subsequent mention'))]

# Let's have a visual look
theme_set(theme_bw())

ggplot(data=denunciations) +
  geom_bar(aes(x=time,fill=type),color='black') +
  labs(x='',y='Denunciations',fill='') +
  scale_x_continuous(breaks = denunciations$time) +
  scale_fill_manual(values=c('orangered2','khaki1','navyblue')) +
  theme(legend.position="top",legend.justification="center",
        axis.text.x = element_text(angle=90, vjust =0.5, hjust=1))

########################################################################################################################

# EVENT-HISTORY framework: hazard probabilities of mention, by type of mention

# All combinations with the individuals in the sample
dat <- data.table(sender = rep(ids,each=length(ids)),
                  target = rep(ids,times=length(ids)))
# Let's remove the cases where sender and target are the same person
dat <- dat[sender != target] # N = 269*268

# Only the 110 individuals deposed could send denunciations
dat <- dat[sender %in% N_sample$id] # 29,480 ties possible

# Only targets reported as heretics (239)
dat <- dat[target %in% incriminated_nodes[accused == 1]$id] # 26,208 ties possible

# And let's add the start date, the first day
dat[,start := days[1]]

# Let's bring the actual denunciations in this new object
# Object for the merge
den <- denunciations[,.(source,target,time,type)]
names(den) <- c('sender','target','end','type')
dat <- merge(dat,den,by=c('sender','target'),all.x = TRUE)
rm(den)

# If the denunciation never happened, let's "truncate" at last day of the trial
dat[is.na(end)]$end <- days[length(days)]

# The event in question is the denunciation
dat[,event := ifelse(!is.na(type),1,0)]

# For the survival object, we need to turn the dates into differences in dates
dat[,outset := 0]
dat[,diff := as.numeric(end-start)]

datlong <- survSplit(data=dat,
                     cut=1:max(dat$diff),
                     start='outset',end='diff',event='event')

datlong <- as.data.table(datlong) # as data table; object of a bit less than a million observations
datlong[,length(sender),by=outset] # checked

#################################

# Now, we need to introduce censoring/truncation. 

# Find the last date at which individuals deposed, and truncate observations from that person after that date
deposdate <- N_sample[order(date,decreasing = TRUE)][!duplicated(id)][,.(id,date)]
names(deposdate) <- c('sender','deposition')
datlong <- merge(datlong,deposdate,by='sender',all.x=TRUE)

# Finally, we need to translate this deposition date into a number from 0 to 33
datlong[,deposday := as.numeric(deposition - days[1])]
# datlong <- datlong[is.na(deposday) | diff <= deposday] # A sample of around 1,88 mill
datlong <- datlong[diff <= deposday] # A sample of around 600,000

########################################################################################################################

# DESCRIPTIVE VISUALIZATION

ts.fir <- survfit(Surv(outset,diff,event)~1,
                  conf.type='log',conf.int=.95,type="kaplan-meier",error='greenwood',
                  data=datlong[!(type %in% c('Second mention','Third or subsequent mention'))])
summary(ts.fir)
ts.sec <- survfit(Surv(outset,diff,event)~1,
                  conf.type='log',conf.int=.95,type="kaplan-meier",error='greenwood',
                  data=datlong[!(type %in% c('First mention','Third or subsequent mention'))])
summary(ts.sec)
ts.thi <- survfit(Surv(outset,diff,event)~1,
                  conf.type='log',conf.int=.95,type="kaplan-meier",error='greenwood',
                  data=datlong[!(type %in% c('First mention','Second mention'))])
summary(ts.thi)

# Extract the necessary information
hazarddata <- data.table(
  type = rep(levels(datlong$type),each=33),
  time = rep(days[-1],times=3),
  risk = c(ts.fir$n.risk,ts.sec$n.risk,ts.thi$n.risk),
  event = c(ts.fir$n.event,ts.sec$n.event,ts.thi$n.event),
  survival = c(ts.fir$surv,ts.sec$surv,ts.thi$surv),
  lower = c(ts.fir$lower,ts.sec$lower,ts.thi$lower),
  upper = c(ts.fir$upper,ts.sec$upper,ts.thi$upper)
)

hazarddata[,hazard := event/risk] # hazards: event / risk set
hazarddata[,se.h := sqrt(hazard*(1-hazard)/risk)] # SE of the hazard

# Plotting
tiff(filename="Fig2.tiff",
     width=30, height=18,units="cm", 
     compression="lzw",
     bg="white",
     res=1000
)

ggplot() +
  geom_bar(data=denunciations,aes(x=time,fill=type),color='grey40',alpha=.75) +
  geom_smooth(data=hazarddata,aes(time,hazard*10000,color=type),method='loess',se=FALSE,linewidth=1.15) +
  scale_y_continuous(name="Denunciations",
                     sec.axis = sec_axis(~./100, name = "Smoothed hazard probability (%)")) + # Create secondary y-axis
  scale_fill_manual(values=c('darkred','gold','navyblue')) +
  scale_color_manual(values=c('darkred','gold','navyblue')) +
  scale_x_continuous(breaks = denunciations$time) +
  xlab("Timeline") + ylab("") + labs(color='') +
  guides(fill='none', shape = guide_legend()) +
  theme(legend.position="top",legend.justification="center",
        axis.text.x = element_text(angle=90, vjust =0.5, hjust=1))
dev.off()

########################################################################################################################

# MOST POPULAR TARGETS

poptargets <- as.data.table(denunciations[,table(target)])
nrow(poptargets) # 239 unique targets
nrow(poptargets[N >= 3]) # 77 individuals received 3 or more denunciations

ggplot(data=poptargets) +
  geom_bar(aes(x=N),color='black',fill='grey') +
  labs(x='Denunciations received',y='Count')

# Super-popular (ten or more denunciations)
supoptargets <- poptargets[N >= 10]
# Add labels to know who these people were
supoptargets <- merge(supoptargets,incriminated_nodes[,.(id,label)],
                      by.x='target',by.y='id',all.x=TRUE)

supoptargets

########################################################################################################################