########################################################################################################################
## INQUISITION IN GIAVENO (1335)
## (4) DyNAM analyses
## R script written by Jose Luis Estevez (Vaestoliitto)
## Date: Oct 5th, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(data.table);library(ggplot2);library(ggpubr);library(igraph);library(goldfish)
library(broom);library(pixiedust);library(patchwork)

# DATA LOADING 
rm(list=ls())
load('data/data4.RData')

########################################################################################################################

# CREATION OF GOLDFISH OBJECTS: NODES, NETWORKS, AND EVENTS

# 1) NODES
actors <- incriminated_nodes[order(incriminated_nodes$id),] # Let's order nodes alphabetically first
actors <- actors[,c('id','sex','dead')] # I am isolating only those attributes useful for analyses
actors$present <- TRUE # Actors are always present
# Redefine variables as binary: 1 or 0
actors$sex <- ifelse(actors$sex == 'f',1,0)
actors <- actors[,c(1,4,2,3)] # reorder the variable
names(actors) <- c('label','present','woman','dead') # rename variables

# Eigenvector centrality in the congregation network
congCent <- graph_from_adjacency_matrix(congGraph,mode='undirected')
centralities <- data.table(id = names(degree(congCent)),
                           congEigen = eigen_centrality(congCent,directed=FALSE)$vector)
actors <- merge(actors,centralities,by.x='label',by.y='id')
# Check the correlation between in-degree in denunciation ntw and eigenvector centrality in heretic network
centralities <- merge(centralities,data.table(denunciations[,table(target)]),by.x='id',by.y='target')
centralities[,N := sqrt(N)] # We use the square root version

corrval <- cor.test(centralities$N,centralities$congEigen)
corrval$estimate

# Visualization
plot1 <- ggplot(centralities,aes(x=N,y=congEigen)) + 
  geom_point(size=3,alpha=1/3) + 
  stat_smooth(method = "loess",color='grey10',linetype='dashed') +
  annotate("text",x=2,y=0.9,label=parse(text = "rho == 0.88")) +
  labs(x='In-degree (denunciation network) (sqrt)',y='Eigenvector centrality (heretic network)') +
  theme_bw()
dens1 <- ggplot(centralities,aes(x=N)) + 
  geom_density(alpha = 0.4,fill='grey',adjust=.5) +
  theme_void() + 
  theme(legend.position = "none")
dens2 <- ggplot(centralities,aes(x=congEigen)) + 
  geom_density(alpha = 0.4,fill='grey',adjust=.5) + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_flip()

tiff(filename="FigC1.tiff",
     width=20, height=14,units="cm", 
     compression="lzw",
     bg="white",
     res=1000
)
dens1 + plot_spacer() + plot1 + dens2 + plot_layout(ncol=2,nrow=2,widths=c(4,1),heights=c(1,4))
dev.off()

# The following 6 attributes (denunciations received, summoned, deposed, re-deposed, centrality, denouncesRec, denouncesSent, tortured) are zero at the start
actors$tortured <- actors$denouncesRec <- actors$redeposed <- actors$deposed <- actors$summoned <- 0 
actors <- defineNodes(nodes=actors) # Turn into a DyNAM object

# 2) NETWORKS
# 2.1) DENUNCIATION NETWORK (empty at the start)
denunciationNetwork <- defineNetwork(nodes=actors,directed=TRUE)

# 2.2) KINSHIP NETWORK
kinshipNetwork <- graph_from_edgelist(as.matrix.data.frame(kinships[,c('V1','V2')]),directed = FALSE)
# Let's add all people involved in the sample as isolates
kinshipNetwork <- igraph::add_vertices(kinshipNetwork,
                                       length(actors$label[!(actors$label %in% V(kinshipNetwork)$name)]),
                                       name=actors$label[!(actors$label %in% V(kinshipNetwork)$name)])
# Let's remove family ties with those beyond our sample
kinshipNetwork <- igraph::delete_vertices(kinshipNetwork,!(V(kinshipNetwork)$name %in% actors$label))
# Let's extract the matrix from the igraph object
kinshipNetwork <- as.matrix(as_adjacency_matrix(kinshipNetwork))
# And reorder the rows and columns
kinshipNetwork <- kinshipNetwork[order(rownames(kinshipNetwork)),order(colnames(kinshipNetwork))]
kinshipNetwork <- defineNetwork(matrix=kinshipNetwork,nodes=actors,directed=FALSE) # Turn into a goldfish object

# 2.3) CONGREGATION NETWORK
congregationNetwork <- defineNetwork(matrix=congGraph,nodes=actors,directed=FALSE)

# 3) EVENTS
# 3.0) Before anything, let's express time in days instead of actual dates
# Let's homogeneize all the date
N_sample$date <- as.Date(N_sample$date)
castellario$keyevents$time <- as.Date(castellario$keyevents$time)
denunciations$time <- as.Date(denunciations$time)
castellario$edges$time <- as.Date(castellario$edges$time)

# First day: 19th of January
minday <- min(c(N_sample$date,castellario$keyevents$time,denunciations$time)) - 1
N_sample$date <- as.numeric(N_sample$date - minday)
castellario$keyevents$time <- as.numeric(castellario$keyevents$time - minday)
denunciations$time <- as.numeric(denunciations$time - minday)
castellario$edges$time <- as.numeric(castellario$edges$time - minday)

# 3.1) DEPOSITION AND RE-DEPOSITION EVENTS (linked as node's attributes)
# Deposition dates
depositionDates <- N_sample[N_sample$redeposition == 0,c('date','id')]
depositionDates <- depositionDates[order(depositionDates$date),]
names(depositionDates) <- c('time','node')
depositionDates$replace <- 1
# Re-deposition dates
redepositionDates <- N_sample[N_sample$redeposition == 1,c('date','id')]
redepositionDates <- redepositionDates[order(redepositionDates$date),]
names(redepositionDates) <- c('time','node')
redepositionDates$replace <- 1

# Let's lag re-depositions one day  to capture the effect on those denouncing
redepositionDates$time <- redepositionDates$time - 1 # a day lag
# Link (re)deposition events to the nodes' attributes
actors <- linkEvents(x=actors,changeEvents=depositionDates,attribute='deposed') |> 
  linkEvents(changeEvents=redepositionDates,attribute='redeposed')

# 3.2) SUMMONS 
# We know the exact date of summons for those in public calls
summoned <- castellario$keyevents
summoned <- summoned[summoned$type == 'summons',c('time','id')]
names(summoned) <- c('time','node')

# But more individuals were summoned before deposing (and the date is uncertain)
summonDates <- castellario$edge[,c('time','source','summoned')]
names(summonDates) <- c('time','node','replace')
summonDates <- summonDates[!is.na(summonDates$replace) & !is.na(summonDates$time) & summonDates$replace == 1,]
summonDates <- summonDates[!duplicated(summonDates),]

summonDates <- merge(summonDates,summoned,by='node',all=TRUE)
length(unique(summonDates$node)) # in total 90 individuals were summoned
sum(unique(summonDates$node) %in% depositionDates$node) # of whom, 85 were deposed (5 people did not deposed?)

# Difference in time between summon and deposition (in days)
summonDates <- summonDates[order(summonDates$time.x),]
as.numeric(mean(summonDates[!duplicated(summonDates$node),]$time.x - 
                  summonDates[!duplicated(summonDates$node),]$time.y,na.rm=TRUE))  # mean = 0.64 day
as.numeric(range(summonDates[!duplicated(summonDates$node),]$time.x - 
                   summonDates[!duplicated(summonDates$node),]$time.y,na.rm=TRUE))  # between 0 and 5 days max

# Since the ave difference is 0.64 day, when the date of summon is missing, we consider it: date of deposition - 1 day
for(i in 1:nrow(summonDates)){
  if(is.na(summonDates$time.y[i])){
    summonDates$time.y[i] <- summonDates$time.x[i] - 1 # a day lag
  }
}

# Link summons to the nodes' attributes
summonDates <- summonDates[,c('time.y','node','replace')]
names(summonDates) <- c('time','node','replace')
summonDates$replace <- 1
summonDates <- summonDates[!duplicated(summonDates),] # let's remove duplicates
summonDates <- summonDates[order(summonDates$time),] # and order by time
actors <- linkEvents(x=actors,changeEvents=summonDates,attribute='summoned')

# 3.3) DENUNCIATION EVENTS (linked to the denunciation network)
denunciationsAll <- denunciations[,c('time','source','target')]
names(denunciationsAll) <- c('time','sender','receiver')
denunciationsAll <- denunciationsAll[order(denunciationsAll$time),] # order by time
denunciationsAll$replace <- 1 # the value that will replace 0 in denunciationNetwork
# Link denunciation events to the denunciation network
denunciationNetwork <- linkEvents(x=denunciationNetwork,changeEvent=denunciationsAll,nodes=actors)

# 3.5) DEPENDENT EVENT
denunciationDependent <- defineDependentEvents(events=denunciationsAll,nodes=actors,defaultNetwork=denunciationNetwork)
denunciationDependent

# 3.6) TORTURED
tortureDates <- castellario$keyevents[castellario$keyevents$type == 'torture',c('time','id')]
names(tortureDates) <- c('time','node')
tortureDates$replace <- 1
# Let's delay torture a day too to capture the effect on depositions under torture
tortureDates$time <- tortureDates$time - 1 # a day-lag
actors <- linkEvents(x=actors,changeEvents=tortureDates,attribute='tortured')

# 3.7) ABSCONDED FROM THE TRIAL
# People who were summoned but never deposed
summoned[!(summoned$node %in% depositionDates$node),]
(absconded <- unique(summoned$node[!(summoned$node %in% depositionDates$node)])) # 5 cases
# Who were these?
incriminated_nodes[id %in% absconded,.(id,label)]
# Let's have an indicator for those who absconded the trial
actors$absconded <- ifelse(actors$label %in% absconded,1,0) 

########################################################################################################################

# ESTIMATION PART (DYNAMs)

# 1) ALL DENUNCIATIONS
# RATE MODEL
actors[!(actors$label %in% N_sample$id),]$present <- FALSE # Let's send those non-deposed to non-present for the rate function

formula1 <- denunciationDependent ~ 1 +
  ego(actors$woman) + # tendency to denounce for women 
  indeg(denunciationNetwork,type='ego',transformFun=sqrt) +
  ego(actors$summoned) + # tendency to denounce for those who were summoned by the inquisitor
  ego(actors$redeposed) + # tendency to denounce for those who have already been deposed
  ego(actors$tortured)

mod01Rate <- estimate(x=formula1,model="DyNAM",subModel="rate",
                      estimationInit=list(engine="default_c",returnIntervalLogL=TRUE))
# Summary
summary(mod01Rate)
(rate1 <- dust(tidy(mod01Rate)) %>% sprinkle(col = 2:4, round = 2) %>% sprinkle(col = 5, fn = quote(pvalString(value))))
rate1OR <- data.table(OR = exp(mod01Rate$parameters),
                      low95 = exp(mod01Rate$parameters - mod01Rate$standardErrors*qnorm(.975)),
                      high95 = exp(mod01Rate$parameters + mod01Rate$standardErrors*qnorm(.975)))
round(rate1OR,2)
glance(mod01Rate)
examineOutliers(mod01Rate)
chpoint1rate <- examineChangepoints(mod01Rate)

# CHOICE MODEL
# We must constraint to only those ever reported as heretics (N = 239 instead of N = 267)
actors$present <- actors[,label %in% unique(denunciations$target)]

formula2 <- denunciationDependent ~ 
  alter(actors$woman) + # tendency to denounce a woman 
  same(actors$woman) + # tendency to denounce individuals of the same gender
  alter(actors$dead) + # tendency to denounce deceased individuals
  recip(denunciationNetwork) + # tendency to denounce previous denouncers 
  commonSender(denunciationNetwork) + # tendency to denounce somebody who denounced a denouncer of ours
  indeg(denunciationNetwork,type='alter',transformFun=sqrt) 

mod01Choice <- estimate(x=formula2,model="DyNAM",subModel="choice",
                        estimationInit=list(engine="default",returnIntervalLogL=TRUE))
# Summary
summary(mod01Choice)
(choice1 <- dust(tidy(mod01Choice)) %>% sprinkle(col = 2:4, round = 2) %>% sprinkle(col = 5, fn = quote(pvalString(value))))
choice1OR <- data.table(OR = exp(mod01Choice$parameters),
                        low95 = exp(mod01Choice$parameters - mod01Choice$standardErrors*qnorm(.975)),
                        high95 = exp(mod01Choice$parameters + mod01Choice$standardErrors*qnorm(.975)))
round(choice1OR,2)
glance(mod01Choice)
examineOutliers(mod01Choice)
chpoint1choice <- examineChangepoints(mod01Choice)

##########################

# Extra model
formulaExt <- denunciationDependent ~ 
  alter(actors$woman) + # tendency to denounce a woman 
  same(actors$woman) + # tendency to denounce individuals of the same gender
  alter(actors$dead) + # tendency to denounce deceased individuals
  recip(denunciationNetwork) + # tendency to denounce previous denouncers 
  commonSender(denunciationNetwork) + # tendency to denounce somebody who denounced a denouncer of ours
  indeg(denunciationNetwork,type='alter',transformFun=sqrt) +
  #alter(actors$congEigen) +
  alter(actors$summoned) +
  alter(actors$absconded)

modExtChoice <- estimate(x=formulaExt,model="DyNAM",subModel="choice",
                         estimationInit=list(engine="default",returnIntervalLogL=TRUE))
# Summary
summary(modExtChoice)
(choiceExt <- dust(tidy(modExtChoice)) %>% sprinkle(col = 2:4, round = 2) %>% sprinkle(col = 5, fn = quote(pvalString(value))))
choiceExtOR <- data.table(OR = exp(modExtChoice$parameters),
                          low95 = exp(modExtChoice$parameters - modExtChoice$standardErrors*qnorm(.975)),
                          high95 = exp(modExtChoice$parameters + modExtChoice$standardErrors*qnorm(.975)))
round(choiceExtOR,2)
glance(modExtChoice)
examineOutliers(modExtChoice)
chpointExtchoice <- examineChangepoints(modExtChoice)

########################################################################################################################

# 2) DENUNCIATIONS DIRECTED AT FAMILY MEMBERS ONLY
denunciationsKinship <- denunciations[!is.na(denunciations$role.x),
                                      c('time','source','target')]
names(denunciationsKinship) <- c('time','sender','receiver')
denunciationsKinship <- denunciationsKinship[order(denunciationsKinship$time),] # order by time
denunciationsKinship$replace <- 1 # the value that will replace 0 in denunciationNetwork

denunciationNetwork <- linkEvents(x=denunciationNetwork,changeEvent=denunciationsKinship,nodes=actors)
denunciationDependent <- defineDependentEvents(events=denunciationsKinship,nodes=actors,defaultNetwork=denunciationNetwork)
denunciationDependent

# We must constrain the individuals choosable to only family members 
#(see: https://stocnet.github.io/goldfish/articles/dynami-example.html)
# Event sender
opportunities <- list()
eventsender <- denunciationDependent$sender
# Choosable subjects per event
for(i in seq_along(eventsender)){
  x <- eventsender[[i]]
  opportunities[[i]] <- kinshipNetwork[x,]
  opportunities[[i]] <- as.vector(which(opportunities[[i]] == 1))
}
opportunities

##########################

# Denunciations accumulated but from non family members
denunciationsBeyond <- denunciations[is.na(denunciations$role.x),c('time','source','target')]
names(denunciationsBeyond) <- c('time','sender','receiver')

node <- unique(denunciationsBeyond$receiver)
time <- unique(denunciationsBeyond$time)

denunciationsAccumulated <- data.frame(time=rep(time,times=length(node)),
                                       node=rep(node,each=length(time)),
                                       replace = NA) 

for(i in 1:nrow(denunciationsAccumulated)){
  denunciationsAccumulated$replace[i] <- nrow(
    denunciationsBeyond[denunciationsBeyond$receiver == denunciationsAccumulated$node[i] & 
                          denunciationsBeyond$time < denunciationsAccumulated$time[i],] # can be changed to "<=" instead of "<"
  )
}

# Remove zeroes and duplicated values
denunciationsAccumulated <- denunciationsAccumulated[denunciationsAccumulated$replace != 0,]
denunciationsAccumulated <- denunciationsAccumulated[!duplicated(denunciationsAccumulated[,c('node','replace')]),]
denunciationsAccumulated <- denunciationsAccumulated[order(denunciationsAccumulated$time),]
denunciationsAccumulated$replace <- sqrt(denunciationsAccumulated$replace)
# Link denounces received to the nodes' attributes
actors <- linkEvents(x=actors,changeEvents=denunciationsAccumulated,attribute='denouncesRec')

# ESTIMATION PART
# RATE MODEL
actors[!(actors$label %in% N_sample$id),]$present <- FALSE # Let's send those non-deposed to non-present for the rate function

formula3 <- denunciationDependent ~ 1 +
  ego(actors$woman) + # tendency to denounce for women 
  indeg(denunciationNetwork,type='ego',transformFun=sqrt) +
  #ego(actors$denouncesRec) +
  ego(actors$summoned) + # tendency to denounce for those who were summoned by the inquisitor
  ego(actors$redeposed) + # tendency to denounce for those who have already been deposed
  ego(actors$tortured)

mod02Rate <- estimate(x=formula3,model="DyNAM",subModel="rate",
                      estimationInit=list(opportunitiesList = opportunities, # constrain possible receivers
                                          engine="default",returnIntervalLogL=TRUE))
# Summary
summary(mod02Rate)
(rate2 <- dust(tidy(mod02Rate)) %>% sprinkle(col = 2:4, round = 2) %>% sprinkle(col = 5, fn = quote(pvalString(value))))
rate2OR <- data.table(OR = exp(mod02Rate$parameters),
                      low95 = exp(mod02Rate$parameters - mod02Rate$standardErrors*qnorm(.975)),
                      high95 = exp(mod02Rate$parameters + mod02Rate$standardErrors*qnorm(.975)))
round(rate2OR,2)
glance(mod02Rate)
examineOutliers(mod02Rate)
chpoint2rate <- examineChangepoints(mod02Rate)

# CHOICE MODEL
actors$present <- TRUE # All nodes present for the estimation of target's effects

formula4 <- denunciationDependent ~ 
  alter(actors$woman) + # tendency to denounce a woman 
  same(actors$woman) + # tendency to denounce individuals of the same gender
  alter(actors$dead) + # tendency to denounce deceased individuals
  recip(denunciationNetwork) + # tendency to denounce previous denouncers 
  commonSender(denunciationNetwork) + # tendency to denounce somebody who denounced a denouncer of ours
  indeg(denunciationNetwork,type='alter',transformFun=sqrt) 

mod02Choice <- estimate(x=formula4,model="DyNAM",subModel="choice",
                        estimationInit=list(opportunitiesList = opportunities, # constrain possible receivers
                                            engine="default",returnIntervalLogL=TRUE))
# Summary
summary(mod02Choice)
(choice2 <- dust(tidy(mod02Choice)) %>% sprinkle(col = 2:4, round = 2) %>% sprinkle(col = 5, fn = quote(pvalString(value))))
glance(mod02Choice)
examineOutliers(mod02Choice)
chpoint2choice <- examineChangepoints(mod02Choice)

########################################################################################################################

# 3) DENUNCIATIONS DIRECTED AT CONGREGATION FELLOWS
denunciationsCong <- denunciations[!is.na(denunciations$congregation.x) & is.na(denunciations$role.x),
                                   c('time','source','target')]
names(denunciationsCong) <- c('time','sender','receiver')
denunciationsCong <- denunciationsCong[order(denunciationsCong$time),] # order by time
denunciationsCong$replace <- 1 # the value that will replace 0 in denunciationNetwork

denunciationNetwork <- linkEvents(x=denunciationNetwork,changeEvent=denunciationsCong,nodes=actors)
denunciationDependent <- defineDependentEvents(events=denunciationsCong,nodes=actors,defaultNetwork=denunciationNetwork)
denunciationDependent

# ESTIMATION PART
# RATE MODEL
actors[!(actors$label %in% N_sample$id),]$present <- FALSE # Let's send those non-deposed to non-present for the rate function

formula5 <- denunciationDependent ~ 1 +
  ego(actors$woman) + # tendency to denounce for women 
  indeg(denunciationNetwork,type='ego',transformFun=sqrt) +
  ego(actors$summoned) + # tendency to denounce for those who were summoned by the inquisitor
  ego(actors$redeposed) + # tendency to denounce for those who have already been deposed
  ego(actors$tortured)

mod03Rate <- estimate(x=formula5,model="DyNAM",subModel="rate", # the same formula as with kinship ties
                      estimationInit=list(engine="default",returnIntervalLogL=TRUE))
# Summary
summary(mod03Rate)
(rate3 <- dust(tidy(mod03Rate)) %>% sprinkle(col = 2:4, round = 2) %>% sprinkle(col = 5, fn = quote(pvalString(value))))
rate3OR <- data.table(OR = exp(mod03Rate$parameters),
                      low95 = exp(mod03Rate$parameters - mod03Rate$standardErrors*qnorm(.975)),
                      high95 = exp(mod03Rate$parameters + mod03Rate$standardErrors*qnorm(.975)))
round(rate3OR,2)
glance(mod03Rate)
examineOutliers(mod03Rate)
chpoint3rate <- examineChangepoints(mod03Rate)

# CHOICE MODEL
# We must constraint to only those ever reported as heretics (N = 239 instead of N = 267)
actors$present <- actors[,label %in% unique(denunciations$target)]

formula6 <- denunciationDependent ~ 
  alter(actors$woman) + # tendency to denounce a woman 
  same(actors$woman) + # tendency to denounce individuals of the same gender
  alter(actors$dead) + # tendency to denounce deceased individuals
  recip(denunciationNetwork) + # tendency to denounce previous denouncers 
  commonSender(denunciationNetwork) + # tendency to denounce somebody who denounced a denouncer of ours
  indeg(denunciationNetwork,type='alter',transformFun=sqrt)

mod03Choice <- estimate(x=formula6,model="DyNAM",subModel="choice",
                        estimationInit=list(engine="default",returnIntervalLogL=TRUE))
# Summary
summary(mod03Choice)
(choice3 <- dust(tidy(mod03Choice)) %>% sprinkle(col = 2:4, round = 2) %>% sprinkle(col = 5, fn = quote(pvalString(value))))
glance(mod03Choice)
examineOutliers(mod03Choice)
chpoint3choice <- examineChangepoints(mod03Choice)

########################################################################################################################

# DYNAM VISUALIZATION

prmtrs <- c('Intercept','Woman (ego)','In-degree activity (sqrt) (ego)','Summoned (ego)','Previously deposed (ego)','Tortured (ego)',
            'Woman (alter)','Same sex','Deceased (alter)',
            'Reciprocity','Common denouncer','In-degree popularity (sqrt) (alter)')

dynamplot <- data.table(parameter = prmtrs,
                        coeff = c(mod01Rate$parameters,mod01Choice$parameters,
                                  mod03Rate$parameters,mod03Choice$parameters, # to congregation fellows
                                  mod02Rate$parameters,mod02Choice$parameters),
                        se = c(mod01Rate$standardErrors,mod01Choice$standardErrors,
                               mod03Rate$standardErrors,mod03Choice$standardErrors,
                               mod02Rate$standardErrors,mod02Choice$standardErrors),
                        funct = c(rep('Rate function',6),rep('Choice function',6)),
                        model = rep(c('Anyone','Congregation fellows','Kin members'),each=12))

# For order of appearance in the plot
dynamplot[,parameter := factor(parameter,levels=rev(prmtrs[c(1:12)]))]
dynamplot[,funct := factor(funct,levels=c('Rate function','Choice function'))]

# add significance level for color
dynamplot[,sig := ifelse(coeff - se*qnorm(.975) > 0,'Positive effect',
                         ifelse(coeff + se*qnorm(.975) < 0,'Negative effect','No effect'))]

# Visualization
tiff(filename="Fig4.tiff",
     width=22, height=12,units="cm", 
     compression="lzw",
     bg="white",
     res=1000
)

options(scipen = 999) # no scientific notation
ggplot(data=dynamplot[parameter != 'Intercept',],
       aes(x=exp(coeff),y=parameter,color=sig,fill=sig),shape=23) +
  geom_vline(xintercept = 1,color='black',linetype='dashed') +
  geom_pointrange(aes(xmin=exp(coeff - se*qnorm(.975)),xmax=exp(coeff + se*qnorm(.975))),shape=21) +
  facet_grid(funct~model,scales='free') +
  scale_color_manual(values=c('No effect'='grey60','Positive effect'='navyblue','Negative effect'='red')) +
  scale_fill_manual(values=c('No effect'='grey80','Positive effect'='royalblue2','Negative effect'='indianred2')) +
  scale_x_log10(breaks = c(0.1,1,10,100,1000),labels=c(0.1,1,10,100,1000)) +
  theme_bw() +
  labs(x='Odds ratio (log scale)',y='',color='',fill='',shape='') +
  theme(legend.position = 'top',legend.justification = 'center',
        strip.background = element_rect(fill='black'),strip.text=element_text(color='white'))
dev.off()

####################

# Change points
tiff(filename="FigB2.tiff",
     width=25, height=20,units="cm", 
     compression="lzw",
     bg="white",
     res=1000
)
ggarrange(chpoint1rate,chpoint1choice + labs(y=''),
          chpoint3rate + labs(x='Rate function'),chpoint3choice + labs(x='Choice function'),
          labels=c('A','B','C','D'),
          nrow=2,ncol=2)
dev.off()

########################################################################################################################

# Print results (DyNAMs)
write.table(rate1,'DyNAM rate (all denunciations).csv',sep=',',row.names=FALSE)
write.table(rate2,'DyNAM rate (denunciations to family).csv',sep=',',row.names=FALSE)
write.table(rate3,'DyNAM rate (denunciations to congregation fellows).csv',sep=',',row.names=FALSE)
write.table(choice1,'DyNAM choice (all denunciations).csv',sep=',',row.names=FALSE)
write.table(choice2,'DyNAM choice (denunciations to family).csv',sep=',',row.names=FALSE)
write.table(choice3,'DyNAM choice (denunciations to congregation fellows).csv',sep=',',row.names=FALSE)
write.table(choiceExt,'DyNAM choice (Extra).csv',sep=',',row.names=FALSE)

########################################################################################################################
