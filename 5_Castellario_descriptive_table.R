########################################################################################################################
## INQUISITION IN GIAVENO (1335)
## (5) Descriptive table
## R script written by Jose Luis Estevez (Vaestoliitto)
## Date: Oct 6th, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(data.table);library(igraph);library(ggplot2);library(MKinfer)

# DATA LOADING 
rm(list=ls())
load('data/data5.RData')

########################################################################################################################

# DESCRIPTIVE TABLE 

# ALL THE SAMPLE (N=267)
(desc1 <- incriminated_nodes[,c('id','sex','occupation_type','origin_or_residence','dead','deposed')])
desc1$sex <- (desc1$sex == 'f')*1
desc1$occupation_type <- (!is.na(desc1$occupation_type))*1
desc1$origin_or_residence <- (!is.na(desc1$origin_or_residence))*1
desc1$redeposed <-(desc1$id %in% redepositionDates$node)*1
desc1$summoned <-(desc1$id %in% summonDates$node)*1
desc1$tortured <-(desc1$id %in% tortureDates$node)*1

# Let's add the number of denunciations issued and received
graph <- graph_from_edgelist(as.matrix.data.frame(denunciations[,c("source","target")]),directed = TRUE)
graph <- simplify(graph)
# Add nodes
incriminated_nodes$id[!(incriminated_nodes$id %in% V(graph)$name)] # nodes that are not in the ties data file
graph <- igraph::add_vertices(graph,
                              length(incriminated_nodes$id[!(incriminated_nodes$id %in% V(graph)$name)]),
                              name= incriminated_nodes$id[!(incriminated_nodes$id %in% V(graph)$name)])
# Let's paired the network and descriptive dataset
desc1 <- desc1[match(V(graph)$name,desc1$id),]
desc1$id == V(graph)$name

desc1$denSent <- degree(graph,mode='out') # denunciations issued
desc1$denRec <- degree(graph,mode='in') # denunciations received

# SUMMARY TABLE
tab1 <- as.data.frame(matrix(NA,10,4,
                             dimnames=list(c('Woman','Occupation known','Residence known','Deceased','Deposed',
                                             'Redeposed','Summoned','Tortured','Denunciations issued','Denunciations received'),
                                           c('mean','SD','min','max'))))

tab1$mean <- apply(desc1[,-1],2,mean)
tab1$SD <- apply(desc1[,-1],2,sd)
tab1$min <- apply(desc1[,-1],2,min)
tab1$max <- apply(desc1[,-1],2,max)
round(tab1,2)

########################################################################################################################

# ONLY DEPONENTS (N=110)
desc2 <- desc1[desc1$deposed == 1,]
tab2 <- tab1*NA

tab2$mean <- apply(desc2[,-1],2,mean)
tab2$SD <- apply(desc2[,-1],2,sd)
tab2$min <- apply(desc2[,-1],2,min)
tab2$max <- apply(desc2[,-1],2,max)

tab2 <- tab2[!(rownames(tab2) %in% c('Deceased','Deposed')),] # Deceased and deposed have no longer variability
round(tab2,2)

########################################################################################################################

# DENUNCIATIONS (N=829)
desc3 <- denunciations[,c('source','target','time','congregation.x','role.x','deceased_target')]

desc3$congregation.x <- (!is.na(desc3$congregation.x))*1 # to congregation fellows
desc3$role.x <- (!is.na(desc3$role.x))*1 # to kinship members
# Add gender
women <- incriminated_nodes$id[incriminated_nodes$sex == 'f']
desc3$womanSend <- (desc3$source %in% women)*1
desc3$womanRec <- (desc3$target %in% women)*1
desc3$samegender <- (desc3$womanSend == desc3$womanRec)*1
# Add whether already accused at deposition, summoned, and in redeposition
desc3 <- merge(desc3,N_sample[,c('id','date','accused_at_deposition','summoned','redeposition')],
               by.x=c('source','time'),by.y=c('id','date'),all.x=TRUE)

desc3 <- desc3[order(desc3$time),]
desc3 <- desc3[!duplicated(desc3[,c('source','target')]),]

# Add whether the individuals was tortured
desc3$tortured <- 0
for(i in 1:nrow(tortureDates)){
  s <- tortureDates$node[i]
  t <- tortureDates$time[i]
  desc3[source == s & time > t]$tortured <- 1
}

# Add whether the individual had absconded the trial
desc3$absconded <- ifelse(desc3$target %in% absconded,1,0)

# SUMMARY TABLE
tab3 <- as.data.frame(matrix(NA,11,5,
                             dimnames=list(c('Congregation fellow','Family member','Deceased target','Woman (denouncer)',
                                             'Woman (denounced)','Same sex','Denounced at deposition (denouncer)',
                                             'Summoned (denouncer)','Redeposed (denouncer)','Tortured (denouncer)','Absconded (denounced)'),
                                           c('count','mean','SD','min','max'))))

tab3$count <- apply(desc3[,4:14],2,sum)
tab3$mean <- apply(desc3[,4:14],2,mean)
tab3$SD <- apply(desc3[,4:14],2,sd)
tab3$min <- apply(desc3[,4:14],2,min)
tab3$max <- apply(desc3[,4:14],2,max)
round(tab3,2)

##############

# Analysis of the denunciations based on the centrality in the informal networks
congCent <- graph_from_adjacency_matrix(congGraph,mode='undirected')

centralities <- data.table(id = names(degree(congCent)),
                           Congdegree = degree(congCent),
                           congEigen = eigen_centrality(congCent)$vector)

# Descriptives of eigenvector centrality in the heretic network
desc1 <- merge(desc1,centralities,by='id')
mean(desc1$congEigen);sd(desc1$congEigen);min(desc1$congEigen);max(desc1$congEigen)

desc2 <- merge(desc2,centralities,by='id')
mean(desc2$congEigen);sd(desc2$congEigen);min(desc2$congEigen);max(desc2$congEigen)

# Standardized value
centralities$congEigen <- scale(centralities$congEigen,center=TRUE,scale=TRUE)

desc3 <- merge(desc3,centralities,by.x='source',by.y='id')
desc3 <- merge(desc3,centralities,by.x='target',by.y='id')

# Degree centrality
mean(desc3$Congdegree.x);sd(desc3$Congdegree.x);min(desc3$Congdegree.x);max(desc3$Congdegree.x)
mean(desc3$Congdegree.y);sd(desc3$Congdegree.y);min(desc3$Congdegree.y);max(desc3$Congdegree.y)
# Eigenvector centrality
mean(desc3$congEigen.x);sd(desc3$congEigen.x);min(desc3$congEigen.x);max(desc3$congEigen.x)
mean(desc3$congEigen.y);sd(desc3$congEigen.y);min(desc3$congEigen.y);max(desc3$congEigen.y)

########################################################################################################################

# THE GENERAL SILENCE PERIOD (1ST TO 5TH FEB)

# Depositions between 1st and 7th Feb
N_sample2 <- N_sample[N_sample$date > "1335-02-01",]
N_sample2 <- N_sample2[N_sample2$date < "1335-02-08",]
# Denunciations issued between 1st and 7th Feb
desc3 <- desc3[desc3$time > "1335-02-01",]
desc3 <- desc3[desc3$time < "1335-02-08",]

# Number of depositions and deponents
nrow(N_sample2) 
length(unique(N_sample2$id)) 
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function 'not in'
length(unique(N_sample2[N_sample2$id %!in% c('P0053','P0196','P0041'),]$id)) # Excluding torture

# Deponent who gave no names
sum(N_sample2$accused == 0) 
# Deponents who gave one or two names
sum(N_sample2$accused %in% c(1,2)) 

# Mean denunciations issued
mean(N_sample2$accused)
mean(N_sample2[N_sample2$id %!in% c('P0053','P0196','P0041'),]$accused) # Without Gauterii, Vet, and Rosseto

# Denounces issued by 
nrow(desc3) # all denunciations in this period
nrow(desc3[desc3$source %in% c('P0053','P0196','P0041'),]) # Denunciations issued by main characters

# How many of these will be redeposed later
silent_characters <- unique(N_sample2$id)
nrow(redepositionDates[redepositionDates$node %in% silent_characters & redepositionDates$time > "1335-02-08",])
# Name of those redeposed later
redp <- redepositionDates[redepositionDates$node %in% silent_characters & redepositionDates$time > "1335-02-08",]$node

# Denunciations in redeposition for these 13
N_sample[N_sample$id %in% redp & N_sample$date > "1335-02-09",]$accused
mean(N_sample[N_sample$id %in% redp & N_sample$date > "1335-02-09",]$accused)
range(N_sample[N_sample$id %in% redp & N_sample$date > "1335-02-09",]$accused)

N_sample[N_sample$id %in% redp & N_sample$date > "1335-02-09",]$family_accused
sum(N_sample[N_sample$id %in% redp & N_sample$date > "1335-02-09",]$family_accused != 0)
mean(N_sample[N_sample$id %in% redp & N_sample$date > "1335-02-09",]$family_accused)
range(N_sample[N_sample$id %in% redp & N_sample$date > "1335-02-09",]$family_accused)

########################################################################################################################

# Print results
write.table(round(rbind(tab1,tab2,tab3[,-1]),2),
            'Desc (all sample).csv',sep=',',row.names=TRUE)

########################################################################################################################

# FOCUS ON THOSE DEPOSED TWICE

# The 20 who were re-deposed
ids <- N_sample[N_sample$redeposition == 1,]$id # ids
redep <- N_sample[N_sample$id %in% ids,
                  c('id','redeposition','accused','family_accused',"congregation_fellows_accused")]

# Comparisons
# Number of accusations (all)
mean(redep[redeposition == 1,]$accused)
mean(redep[redeposition == 0,]$accused)
set.seed(0708)
boot.t.test(redep[redep$redeposition == 1,]$accused,
            redep[redep$redeposition == 0,]$accused,
            paired=TRUE,R=1000)

# Number of congregation fellows accused
set.seed(0708)
sum(redep[redeposition == 1,]$congregation_fellows_accused >= 1) / 20
boot.t.test(redep[redep$redeposition == 1,]$congregation_fellows_accused,
            redep[redep$redeposition == 0,]$congregation_fellows_accused,
            paired=TRUE,R=1000)

# Number of family members accused
set.seed(0708)
boot.t.test(redep[redep$redeposition == 1,]$family_accused,
            redep[redep$redeposition == 0,]$family_accused,
            paired=TRUE,R=1000)

# Visualization
x <- redep[,c('id','redeposition','accused')]
y <- redep[,c('id','redeposition','congregation_fellows_accused')]
z <- redep[,c('id','redeposition','family_accused')]
names(z) <- names(y) <- c('id','redeposition','accused')

x$type <- 'Anyone'
y$type <- 'Congregation fellows'
z$type <- 'Kin members'

redep2 <- rbind(x,y,z)
# For better visualisation
redep2$redeposition2 <- redep2$redeposition
# Let's jitter the values a little bit
set.seed(123)
redep2$redeposition2 <- jitter(redep2$redeposition2,factor=.3)

# Let's put Gauterii and Vet in the middle
redep2[redep2$id %in% c('P0053','P0196'),]$redeposition2 <- redep2[redep2$id %in% c('P0053','P0196'),]$redeposition

# To add to the legend
redep2[,keypeople := ifelse(id == 'P0053','Iohannes Guaterii',
                            ifelse(id == 'P0196','Stephanus Vet','Other'))]
redep2[,keypeople := factor(keypeople,levels=c('Iohannes Guaterii','Stephanus Vet','Other'))]

fig <- ggplot(data=redep2) +
  geom_violin(aes(x=redeposition,y=accused,group=redeposition),fill='gray',alpha=.2) +
  geom_line(aes(x=redeposition2,y=accused,group=id,color=keypeople),alpha=2/3) +
  geom_point(aes(x=redeposition2,y=accused,color=keypeople,shape=keypeople),size=3) +
  scale_x_continuous(breaks = 0:1,labels=c('First deposition','Second deposition')) +
  facet_wrap(~type,scales='free') +
  labs(x='',y='Number of denunciations',color='',shape='') +
  theme_bw() + 
  scale_color_manual(values=c('Iohannes Guaterii'='red','Stephanus Vet'='red','Other'='royalblue')) +
  scale_shape_manual(values=c('Iohannes Guaterii'=15,'Stephanus Vet'=17,'Other'=10)) +
  theme(legend.position = 'top',strip.background = element_rect(fill='black'),strip.text=element_text(color='white'))

ggsave("FigB1.png", fig, width = 10, height = 6)

########################################################################################################################

# UNCOOPERATIVE DEPONENTS: Let's examine the profile of those who remained quiet during their depositions
profiles <- desc2[,.(id,sex,summoned,redeposed,denSent,denRec)]
profiles[,quiet := ifelse(denSent == 0,TRUE,FALSE)]

# Let's redefine the variables
profiles[,sex := as.factor(ifelse(sex == 0,'m','f'))]
profiles[,summoned := as.factor(ifelse(summoned == 0,'no','yes'))]
profiles[,redeposed := as.factor(ifelse(redeposed == 0,'no','yes'))]
summary(profiles) # 18 individuals did not cooperate

# Some descriptive numbers
profiles[,table(sex,quiet)]
profiles[,prop.table(table(sex,quiet),margin=1)]
prop.test(profiles[,table(sex,quiet)])

profiles[,table(summoned,quiet)]
profiles[,prop.table(table(summoned,quiet),margin=1)]
profiles[,table(redeposed,quiet)]
profiles[,prop.table(table(redeposed,quiet),margin=1)]

# Denunciations received (at the end of the trial)
profiles[,table(denRec,quiet)]
profiles[,mean(denRec),by=quiet] # mean

t.test(profiles[quiet == TRUE,denRec],
       profiles[quiet == FALSE,denRec])

# Let's add the date of the deposition to figure out their in-degree at deposition
# But, first, let's divide those with two depositions
profiles1 <- profiles[redeposed == 'no']
profiles2 <- profiles[redeposed == 'yes']

profiles1 <- merge(profiles1,depositionDates[,.(node,time)],by.x='id',by.y='node')
profiles2 <- merge(profiles2,redepositionDates[,.(node,time)],by.x='id',by.y='node')

# And merge back into one dataset
profiles <- rbind(profiles1,profiles2)
profiles[,time := time - 60] # Remove the extra minutes used in DyNAM
profiles

# It finally remained to find the indegree at the date of the deposition
profiles[,denRecAtDep := 0]

for(i in 1:nrow(profiles)){
  profiles$denRecAtDep[i] <- nrow(denunciations[target == profiles$id[i] & 
                                                   time <= profiles$time[i]])
}

profiles[,table(denRecAtDep,quiet)]
profiles[,mean(denRecAtDep),by=quiet] # mean

t.test(profiles[quiet == TRUE,denRecAtDep],
       profiles[quiet == FALSE,denRecAtDep])

# Let us exclude those who deposed before the Grace Period
profiles2 <- profiles[time > "1335-01-28"]

profiles2[,table(denRecAtDep,quiet)]
profiles2[,mean(denRecAtDep),by=quiet] # mean

t.test(profiles2[quiet == TRUE,denRecAtDep],
       profiles2[quiet == FALSE,denRecAtDep])

# Now, let's check the centrality of these individuals in the congregation network
profiles <- merge(profiles,centralities,by='id',all.x = TRUE)

# Degree centrality
profiles[,table(Congdegree,quiet)]
profiles[,mean(Congdegree),by=quiet] # mean

t.test(profiles[quiet == TRUE,Congdegree],
       profiles[quiet == FALSE,Congdegree])

# Let's square the values
profiles$CongdegreeSqrt <- sqrt(profiles$Congdegree)

t.test(profiles[quiet == TRUE,CongdegreeSqrt],
       profiles[quiet == FALSE,CongdegreeSqrt])

# Eigenvector centrality
# Let's standardized eigenvector centrality
profiles$congEigen <- scale(profiles$congEigen,center=TRUE,scale=TRUE)
profiles[,table(congEigen,quiet)]
profiles[,mean(congEigen),by=quiet] # mean

t.test(profiles[quiet == TRUE,congEigen],
       profiles[quiet == FALSE,congEigen])

########################################################################################################################