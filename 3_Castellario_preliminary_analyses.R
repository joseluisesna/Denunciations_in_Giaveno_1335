########################################################################################################################
## INQUISITION IN GIAVENO (1335)
## (3) Preliminary analyses
## R script written by Jose Luis Estevez (Vaestoliitto)
## Date: Sep 30th, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(igraph);library(data.table);library(psych);library(ggplot2)

# DATA LOADING 
rm(list=ls())
load('data/data3.RData')

########################################################################################################################

# OVERLAP BETWEEN DENUNCIATIONS AND FAMILY TIES: How many people incriminated a family member

# Let's turn the denunciations into an igraph object
inc_graph <- graph_from_edgelist(as.matrix.data.frame(denunciations[,c('source','target')]),directed = TRUE)
# Add deponents who did not accused or were accused by somebody else
inc_graph <- igraph::add_vertices(inc_graph,
                                  length(incriminated_nodes$id[!(incriminated_nodes$id %in% V(inc_graph)$name)]),
                                  name= incriminated_nodes$id[!(incriminated_nodes$id %in% V(inc_graph)$name)])

# Add date of deposition
E(inc_graph)$date <- denunciations$time

# Let's keep only the first time the tie was reported for visualisation purposes
inc_graph <- simplify(inc_graph,remove.multiple = TRUE,edge.attr.comb='first')

# Only family ties between those in the incriminated sample (deponents and/or accused)
kinships2 <- kinships[kinships$V1 %in% incriminated_nodes$id & kinships$V2 %in% incriminated_nodes$id,]
family_graph <- graph_from_edgelist(as.matrix.data.frame(kinships2[,c('V1','V2')]),directed = FALSE)
# Add the type of family tie
E(family_graph)$type <- kinships2$role
# Add extra nodes
family_graph <- igraph::add_vertices(family_graph,
                                     length(incriminated_nodes$id[!(incriminated_nodes$id %in% V(family_graph)$name)]),
                                     name= incriminated_nodes$id[!(incriminated_nodes$id %in% V(family_graph)$name)])

# Also, let's turn the congregation matrix into an igraph object
congrations_graph <- graph_from_adjacency_matrix(congGraph,mode='undirected')

# Now, let's retrieve the matrices from the igraph objects
inc_mtx <- as.matrix(as_adjacency_matrix(inc_graph))
inc_mtx <- inc_mtx[order(rownames(inc_mtx)),order(colnames(inc_mtx))]
family_mtx <- as.matrix(as_adjacency_matrix(family_graph))
family_mtx <- family_mtx[order(rownames(family_mtx)),order(colnames(family_mtx))]
diag(inc_mtx) <- diag(family_mtx) <- NA # remove the diagonal

# Let's remove the indegree of all those who were not deposed
non_deposed <- incriminated_nodes[incriminated_nodes$deposed == 0,]$id
inc_mtx[rownames(inc_mtx) %in% non_deposed,] <- NA

# SUMMARY TABLE
overlap <- data.frame(dim=c('denunciations','Family ties','Congregation fellows',
                            'Overlap (incrimination-family)','Hamming distance (incrimination-family)','Jaccard index (incrimination-family)',
                            'Overlap (incrimination-congregation)','Hamming distance (incrimination-congregation)','Jaccard index (incrimination-congregation)',
                            'Overlap (family-congregation)','Hamming distance (family-congregation)','Jaccard index (family-congregation)',
                            'Individuals deposed','Individuals who incriminated somebody','Individuals who incriminated somebody alive',
                            'Individuals incriminating a family member','Individuals incriminating a family member alive',
                            'Individuals incriminating a congregation fellow','Individuals incriminating a congregation fellow alive'),
                      val=NA)

overlap$val[1] <- ecount(inc_graph)
overlap$val[2] <- ecount(family_graph)
overlap$val[3] <- ecount(congrations_graph)
overlap$val[4] <- sum((inc_mtx + family_mtx) == 2,na.rm = TRUE)
overlap$val[5] <- sum(inc_mtx != family_mtx,na.rm=TRUE)

# Function to calculate the Jaccard index
Jaccard <- function(matrix1,matrix2){
  shared_ties <- matrix1*matrix2
  diff_ties <- 1*((matrix1+matrix2)==1)
  denominator <- sum(shared_ties,na.rm=TRUE)+sum(diff_ties,na.rm=TRUE)
  outcome <- ifelse(denominator==0,0,sum(shared_ties,na.rm=TRUE)/denominator)
  return(outcome)
}

overlap$val[6] <- Jaccard(inc_mtx,family_mtx)

# denunciations and congregation
overlap$val[7] <- sum((inc_mtx + congGraph) == 2,na.rm = TRUE)
overlap$val[8] <- sum(inc_mtx != congGraph,na.rm=TRUE)
overlap$val[9] <- Jaccard(inc_mtx,congGraph)
# Family and congregation
overlap$val[10] <- sum((family_mtx + congGraph) == 2,na.rm = TRUE)/2 # these two are indirected networks
overlap$val[11] <- sum(family_mtx != congGraph,na.rm=TRUE)
overlap$val[12] <- Jaccard(family_mtx,congGraph)
# Individual-level dimensions
overlap$val[13] <- sum(incriminated_nodes$deposed == 1)
overlap$val[14] <- sum(incriminated_nodes$accuser == 1)

# Let's remove those deceased and re-calculate
deceased <- incriminated_nodes[incriminated_nodes$dead == 1,]$id
inc_mtx_nodead <- inc_mtx[!(rownames(inc_mtx) %in% deceased),!(colnames(inc_mtx) %in% deceased)] # inc_mtx (no dead)
overlap$val[15] <- sum(rowSums(inc_mtx_nodead,na.rm=TRUE) > 0)

# Let's make a matrix with denunciations directed to family members only 
both_mtx <- inc_mtx + family_mtx
both_mtx <- 1*(both_mtx == 2)
overlap$val[16] <- sum(rowSums(both_mtx,na.rm=TRUE) > 0)

# Family members alive at the time
both_mtx_nodead <- both_mtx[!(rownames(both_mtx) %in% deceased),!(colnames(both_mtx) %in% deceased)] # inc_mtx (no dead)
overlap$val[17] <- sum(rowSums(both_mtx_nodead,na.rm=TRUE) > 0)

# Let's make another matrix with denunciations directed to congregation fellows only 
both_mtx <- inc_mtx + congGraph
both_mtx <- 1*(both_mtx == 2)
overlap$val[18] <- sum(rowSums(both_mtx,na.rm=TRUE) > 0)

# Family members alive at the time
both_mtx_nodead <- both_mtx[!(rownames(both_mtx) %in% deceased),!(colnames(both_mtx) %in% deceased)] # inc_mtx (no dead)
overlap$val[19] <- sum(rowSums(both_mtx_nodead,na.rm=TRUE) > 0)

# SUMMARY TABLE
format(overlap,scientific=FALSE)

########################################################################################################################

# ASSOCIATIONS AT THE LEVEL OF THE DEPOSITION (DEPONENT-TIME)

# Because some people were deposed twice, let's obtain deponent-date observations
N_sample <- castellario$edges[,c('source','time','summoned')]
N_sample <- N_sample[!is.na(N_sample$time),]
N_sample <- as.data.table(N_sample[!duplicated(N_sample),])

# Is not first deposition of this person?
N_sample <- N_sample[order(N_sample$time),]
N_sample$redeposition <- 1*duplicated(N_sample$source)

# Add some attributes of the individual who deposed: name, sex, origin, etc.
names(N_sample) <- c('id','date','summoned','redeposition')
N_sample <- merge(N_sample,incriminated_nodes[,c('id','label','sex','origin_or_residence','occupation_type')],
                  by='id',all.x=TRUE)

# ATTRIBUTES BASED ON DEPOSITION
# 1) NUMBER OF PEOPLE ACCUSED 
N_sample <- merge(N_sample,denunciations[,length(target),by=.(source,time)],
                  by.x=c('id','date'),by.y=c('source','time'),all.x = TRUE)
setnames(N_sample,'V1','accused')
N_sample$accused[is.na(N_sample$accused)] <- 0 # NA's to zeroes (nobody incriminated)

# 2) ACCUSED WHO WERE ALSO ALIVE
denunciations$deceased_target <- 1*(denunciations$target %in% deceased)
N_sample <- merge(N_sample,denunciations[deceased_target == 0,
                                          length(target),
                                          by=.(source,time)],
                  by.x=c('id','date'),by.y=c('source','time'),all.x = TRUE)
setnames(N_sample,'V1','accused_alive')
N_sample$accused_alive[is.na(N_sample$accused_alive)] <- 0 # NA's to zeroes (nobody incriminated)

# 3) ACCUSED WHO ARE FAMILY MEMBERS
denunciations <- merge(denunciations,kinships,by.x=c('source','target'),by.y=c('V1','V2'),all.x = TRUE)
denunciations <- merge(denunciations,kinships,by.x=c('source','target'),by.y=c('V2','V1'),all.x = TRUE)

for(i in 1:nrow(denunciations)){
  if(is.na(denunciations$role.x[i])){
    denunciations$role.x[i] <- denunciations$role.y[i]
  }
}

N_sample <- merge(N_sample,denunciations[!is.na(role.x),
                                          length(target),
                                          by=.(source,time)],
                  by.x=c('id','date'),by.y=c('source','time'),all.x = TRUE)
setnames(N_sample,'V1','family_accused')
N_sample$family_accused[is.na(N_sample$family_accused)] <- 0 # NA's to zeroes (nobody incriminated)

N_sample$any_family_accused <- 1*(N_sample$family_accused > 0) # Dichotomisation

# 4) ACCUSED WHO ARE FAMILY MEMBERS AND ARE ALSO ALIVE
N_sample <- merge(N_sample,denunciations[!is.na(role.x) & deceased_target == 0,
                                          length(target),
                                          by=.(source,time)],
                  by.x=c('id','date'),by.y=c('source','time'),all.x = TRUE)
setnames(N_sample,'V1','family_accused_alive')
N_sample$family_accused_alive[is.na(N_sample$family_accused_alive)] <- 0 # NA's to zeroes (nobody incriminated)

N_sample$any_family_alive_accused <- 1*(N_sample$family_accused_alive > 0) # Dichotomisation

# 5) ACCUSED WHO ARE CORE-FAMILY MEMBERS (PARENT-CHILD, SIBLINGS, SPOUSES) AND ARE ALSO ALIVE
N_sample <- merge(N_sample,denunciations[!is.na(role.x) & 
                                            role.x %in% c('siblings','parentchild','marriage','lover',
                                                          'inf_siblings','inf_siblings2',
                                                          'inf_parentchild','inf_parentchild2') & 
                                            deceased_target == 0,
                                          length(target),
                                          by=.(source,time)],
                  by.x=c('id','date'),by.y=c('source','time'),all.x = TRUE)
setnames(N_sample,'V1','core_family_accused_alive')
N_sample$core_family_accused_alive[is.na(N_sample$core_family_accused_alive)] <- 0 # NA's to zeroes (nobody incriminated)

N_sample$any_core_family_alive_accused <- 1*(N_sample$core_family_accused_alive > 0) # Dichotomisation

# 6) ACCUSED WHO ARE CONGREGATION FELLOWS
congTies <- as.data.frame(as_edgelist(congrations_graph)) # edge list with congregation ties among people
congTies$congregation <- 1

denunciations <- merge(denunciations,congTies,by.x=c('source','target'),by.y=c('V1','V2'),all.x = TRUE)
denunciations <- merge(denunciations,congTies,by.x=c('source','target'),by.y=c('V2','V1'),all.x = TRUE)

for(i in 1:nrow(denunciations)){
  if(is.na(denunciations$congregation.x[i])){
    denunciations$congregation.x[i] <- denunciations$congregation.y[i]
  }
}

N_sample <- merge(N_sample,denunciations[!is.na(congregation.x),
                                          length(target),
                                          by=.(source,time)],
                  by.x=c('id','date'),by.y=c('source','time'),all.x = TRUE)
setnames(N_sample,'V1','congregation_fellows_accused')
N_sample$congregation_fellows_accused[is.na(N_sample$congregation_fellows_accused)] <- 0 # NA's to zeroes (nobody incriminated)

N_sample$any_congregation_fellow_accused <- 1*(N_sample$congregation_fellows_accused > 0) # Dichotomisation

# 7) ACCUSED ALREADY BY SOMEBODY WHEN DEPOSED?
N_sample <- merge(N_sample,denunciations[,min(time),by=target],
                  by.x='id',by.y='target',all.x=TRUE)
setnames(N_sample,'V1','first_time_reported')

N_sample$accused_at_deposition <- 1*(N_sample$first_time_reported < N_sample$date)
N_sample$accused_at_deposition[is.na(N_sample$accused_at_deposition)] <- 0

########################################################################################################################

# SUMMARY TABLES

# Differences between depositions when the deponents was already incriminated by somebody
N_sample[,.(male=sum(sex == 'm'),women=sum(sex == 'f'),
            accusations=mean(accused),
            family.accused.prop=sum(any_family_accused == 1)/length(any_family_accused),
            cong.fellow.accused.prop=sum(any_congregation_fellow_accused == 1)/length(any_congregation_fellow_accused)),
         by=accused_at_deposition]

# Some tests
t.test(N_sample$accused[N_sample$accused_at_deposition == 1],
       N_sample$accused[N_sample$accused_at_deposition == 0])

prop.test(table(N_sample$accused_at_deposition,N_sample$any_congregation_fellow_accused)) # Congregation fellow
prop.test(table(N_sample$accused_at_deposition,N_sample$any_family_accused)) # Family

# Differences between depositions when summoned or not
N_sample[,.(male=sum(sex == 'm'),women=sum(sex == 'f'),
            accusations=mean(accused),
            family.accused.prop=sum(any_family_accused == 1)/length(any_family_accused),
            cong.fellow.accused.prop=sum(any_congregation_fellow_accused == 1)/length(any_congregation_fellow_accused)),
         by=summoned]

t.test(N_sample$accused[N_sample$summoned == 1],
       N_sample$accused[N_sample$summoned == 0])

# Differences between depositions when it is the first time the deponent deposed vs. it is not
N_sample[,.(male=sum(sex == 'm'),women=sum(sex == 'f'),
            accusations=mean(accused),
            family.accused.prop=sum(any_family_accused == 1)/length(any_family_accused),
            cong.fellow.accused.prop=sum(any_congregation_fellow_accused == 1)/length(any_congregation_fellow_accused)),
         by=redeposition]

t.test(N_sample$accused[N_sample$redeposition == 1],
       N_sample$accused[N_sample$redeposition == 0])

prop.test(table(N_sample$redeposition,N_sample$any_congregation_fellow_accused)) # Congregation fellow
prop.test(table(N_sample$redeposition,N_sample$any_family_accused)) # Family

########################################################################################################################

# CORRELATION PLOTS

# Correlation among predictors and the response variable
cor_plot <- N_sample[,.(accused,family_accused,congregation_fellows_accused,sex,
                        summoned,redeposition,accused_at_deposition,date)]
cor_plot$sex <- ifelse(cor_plot$sex == 'f',1,0)

# change the names for the plot
names(cor_plot) <- c('People accused\nper deposition','Family members\naccused','Congregation fellows\naccused','Woman\n(deponent)',
                     'Summoned','Second\ndeposition','Deponent accused\nprior to deposition','Deposition\ndate')
pairs.panels(cor_plot,
             method = "spearman",stars = TRUE,
             lm=TRUE,ci=TRUE,ellipses=FALSE,
             pch = 21,jiggle=TRUE,factor=.15,hist.col = 'skyblue',scale=FALSE)

########################################################################################################################

# TEMPORAL PLOT (Denunciations by time and type of denounced person: family, congregation, neither)

# Let's take all the denunciations in our data, and the relationship between denouncer and denounced
temp_plot <- denunciations[,c('time','role.x','congregation.x')]
names(temp_plot) <- c('time','family','congregation')
# Dichotomise variables
temp_plot$family <- (!is.na(temp_plot$family))*1
temp_plot$congregation <- (!is.na(temp_plot$congregation))*1

# Let's turn this into a factor variable: congregation, family, or neither
temp_plot[temp_plot$family == 1,]$congregation <- 2
temp_plot$type <- factor(temp_plot$congregation,levels=c(0,1,2),labels=c('Neither','Congregation fellow','Family member'))
temp_plot <- temp_plot[,c('time','type')]
temp_plot$x <- 1

# Let's obtain the number of different denunciations per day
temp_plot <- temp_plot[,.(x=length(x)),by=.(time,type),all=TRUE]

# Now, let's put this into a dataframe with all dates between the first and last day of the inquisition
dates <- seq(min(castellario$edges$time,na.rm = TRUE) - 86400, # one day before the first deposition
             max(castellario$edges$time + 3*24*60*60,na.rm = TRUE),by="days")
temp_plot2 <- data.frame(time=rep(dates,each=3),
                         type=rep(unique(temp_plot$type),times=length(dates)))
temp_plot <- merge(temp_plot2,temp_plot,by=c('time','type'),all.x = TRUE) # and merge the data
temp_plot[is.na(temp_plot$x),]$x <- 0 # and NAs to zeroes

# Finally, let's get the cummulative sums
temp_plot[temp_plot$type == 'Family member',]$x <- cumsum(temp_plot[temp_plot$type == 'Family member',]$x)
temp_plot[temp_plot$type == 'Congregation fellow',]$x <- cumsum(temp_plot[temp_plot$type == 'Congregation fellow',]$x)
temp_plot[temp_plot$type == 'Neither',]$x <- cumsum(temp_plot[temp_plot$type == 'Neither',]$x)

# VISUALISATION
# Add line with denunciations per day
den <- N_sample[,sum(accused),by=date] 
denun <- data.frame(date=seq(min(den$date),max(den$date),by="days"))
denun <- merge(denun,den,by='date',all.x=TRUE)
denun$V2 <- denun$V1
denun$V1[is.na(denun$V1)] <- 0

levels(temp_plot$type) <- c('Neither','Congregation fellow','Kin') # Change label to "kin" instead of family

tiff(filename="Fig1.tiff",
     width=30, height=18,units="cm", 
     compression="lzw",
     bg="white",
     res=1000
)

ggplot() +
  geom_area(data=temp_plot,aes(x=time, y=x, fill=type),colour="black", size=.5, alpha= .75) +
  geom_line(data=denun,aes(x=date,y=V1),colour='navyblue') +
  geom_vline(xintercept=denun$date[10],linetype='dotted',colour='seagreen') +
  geom_vline(xintercept=denun$date[14],linetype='dotted',colour='darkorange2') +
  geom_vline(xintercept=denun$date[16],linetype='dotted',colour='indianred3') +
  geom_vline(xintercept=denun$date[18],linetype='dotted',colour='darkorange2') +
  geom_vline(xintercept=denun$date[20],linetype='dotted',colour='darkorange2') +
  geom_vline(xintercept=denun$date[21],linetype='dotted',colour='indianred') +
  geom_vline(xintercept=denun$date[31],linetype='dotted',colour='darkorange2') +
  geom_point(data=denun,aes(x=date,y=V2),size=3,colour='navyblue',alpha=.75) +
  geom_vline(xintercept=denun$date[11],linetype='solid',colour='seagreen',linewidth=19,alpha=.33) +
  geom_vline(xintercept=(denun$date[17]+12*60*60),linetype='solid',colour='indianred3',linewidth=28,alpha=.33) +
  annotate("text",x=(denun$date[10]-43200),y=700,label="Grace period proclaimed",color='seagreen',srt=90) +
  annotate("text",x=(denun$date[14]-43200),y=700,label="First summonses",color='darkorange2',srt=90) +
  annotate("text",x=(denun$date[16]-43200),y=650,label="Guaterii appears before Castellario",color='indianred3',srt=90) +
  annotate("text",x=(denun$date[18]-43200),y=700,label="Second summonses",color='darkorange2',srt=90) +
  annotate("text",x=(denun$date[20]-43200),y=700,label="Third summonses",color='darkorange2',srt=90) +
  annotate("text",x=(denun$date[21]-43200),y=650,label="Goytrati reappears before Castellario",color='indianred3',srt=90) +
  annotate("text",x=(denun$date[31]-43200),y=700,label="Last summonses",color='darkorange2',srt=90) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(temp_plot$type))) +
  xlab('Timeline')+ylab('Denunciations')+labs(fill='') +
  scale_x_continuous(breaks = temp_plot$time) +
  theme_bw() + 
  theme(legend.position="top", legend.justification="center") +
  theme(axis.text.x = element_text(angle=90, vjust =0.5, hjust=1))
dev.off()

########################################################################################################################

# DIFFERENCES OBSERVED IN THOSE DEPOSED TWICE

redep <- N_sample[N_sample$redeposition == 1,]$label
first.dep <- N_sample[N_sample$redeposition == 0 & N_sample$label %in% redep,c('label','accused')]
second.dep <- N_sample[N_sample$redeposition == 1 & N_sample$label %in% redep,c('label','accused')]
merge(first.dep,second.dep,by='label')

# DIFFERENCES OBSERVED BY STAGE OF THE PROCESS
periods <- N_sample[order(N_sample$date),]

# Before Grace period
periodsum <- data.frame(matrix(0,5,4))
colnames(periodsum) <- c('before','during','after','later')
rownames(periodsum) <- c('N','mean','sd','min','max')

'%!in%' <- function(x,y)!('%in%'(x,y)) # Function 'not in'
periods <- periods[periods$id %!in% c('P0053','P0196','P0041'),] # Let's remove Gauterii, Vet, and Rosseto

periodsum$before <- c(length(periods$accused[periods$date < "1335-01-29 UTC"]),
                      mean(periods$accused[periods$date < "1335-01-29 UTC"]),
                      sd(periods$accused[periods$date < "1335-01-29 UTC"]),
                      range(periods$accused[periods$date < "1335-01-29 UTC"]))
periodsum$during <- c(length(periods$accused[as.character(periods$date) %in% c("1335-01-29","1335-01-30","1335-01-31")]),
                      mean(periods$accused[as.character(periods$date) %in% c("1335-01-29","1335-01-30","1335-01-31")]),
                      sd(periods$accused[as.character(periods$date) %in% c("1335-01-29","1335-01-30","1335-01-31")]),
                      range(periods$accused[as.character(periods$date) %in% c("1335-01-29","1335-01-30","1335-01-31")]))
periodsum$after <- c(length(periods$accused[as.character(periods$date) %in% c("1335-02-01","1335-02-02","1335-02-03","1335-02-04","1335-02-05","1335-02-06","1335-02-07")]),
                     mean(periods$accused[as.character(periods$date) %in% c("1335-02-01","1335-02-02","1335-02-03","1335-02-04","1335-02-05","1335-02-06","1335-02-07")]),
                     sd(periods$accused[as.character(periods$date) %in% c("1335-02-01","1335-02-02","1335-02-03","1335-02-04","1335-02-05","1335-02-06","1335-02-07")]),
                     range(periods$accused[as.character(periods$date) %in% c("1335-02-01","1335-02-02","1335-02-03","1335-02-04","1335-02-05","1335-02-06","1335-02-07")]))
periodsum$later <- c(length(periods$accused[periods$date > "1335-02-07 UTC"]),
                      mean(periods$accused[periods$date > "1335-02-07 UTC"]),
                      sd(periods$accused[periods$date > "1335-02-07 UTC"]),
                      range(periods$accused[periods$date > "1335-02-07 UTC"]))
round(periodsum,2)

# T-tests (After grace period vs. later)
t.test(periods$accused[as.character(periods$date) %in% c("1335-02-01","1335-02-02","1335-02-03","1335-02-04","1335-02-05","1335-02-06","1335-02-07")],
       periods$accused[periods$date > "1335-02-08 UTC"],
       alternative='two.sided')

########################################################################################################################

# REGRESSION MODELS

# Quantitative predictors are standardized
N_sample$z.date <- as.numeric(N_sample$date)
N_sample$z.date <- scale(N_sample$date,center=TRUE,scale=TRUE)
N_sample$woman <- 1*(N_sample$sex == 'f') # woman instead of sex

# 1) NUMBER OF PEOPLE ACCUSED (OVERDISPERSED POISSON REGRESSION)
model1 <- glm(accused ~ woman + summoned + redeposition + accused_at_deposition + poly(z.date,2),
              family='poisson'(link='log'),data=N_sample)

# Check for overdispersion in the data
yhat <- predict(model1,type='response') # predicted values
z <- (N_sample$accused - yhat)/sqrt(yhat) # residuals
n <- length(z) # Sample size
k <- length(coef(model1)) # number of linear predictors
cat('overdispersion ratio is ',sum(z^2)/(n-k))
cat('p-value of overdispersion test is ',pchisq(sum(z^2),n-k)) # evidence of overdisperson, quasipoisson model instead

model1 <- glm(accused ~ woman + summoned + redeposition + accused_at_deposition + poly(z.date,2),
              family='quasipoisson'(link='log'),data=N_sample)
summary(model1)

# Modelling the outcome in SEVERAL STEPS: LOGISTIC FIRST, POISSON LATER
# 1.1) Whether a deponent accused at least one person or not
N_sample$somebody_accused <- ifelse(N_sample$accused > 0,1,0)
model1.1 <- glm(somebody_accused ~ woman + summoned + redeposition + accused_at_deposition + poly(z.date,2),
              family='binomial'(link='logit'),data=N_sample)
summary(model1.1)

# Error rate
(error.rate <- mean((model1.1[['fitted.values']] > .5 & N_sample$somebody_accused == 0) | 
                      (model1.1[['fitted.values']] < .5 & N_sample$somebody_accused == 1)))

# 1.2) If the deponent accused at least one person, contributions to  how many
model1.2 <- glm(accused ~ woman + summoned + redeposition + accused_at_deposition + poly(z.date,2),
              family='quasipoisson'(link='log'),data=N_sample[N_sample$accused > 0,])
summary(model1.2)

# 2) NUMBER OF FAMILY MEMBERS ACCUSED (IN SEVERAL STEPS TOO)
# 2.1) Whether a deponent accused a family member or not
model2.1 <- glm(any_family_accused ~ woman + summoned + redeposition + accused_at_deposition + poly(z.date,2), 
                family='binomial'(link='logit'),offset=log(accused+1),data=N_sample)
summary(model2.1)

# 2.2) If the deponent accused at least one family member, contributions to how many
model2.2 <- glm(family_accused ~ woman + summoned + redeposition + accused_at_deposition + poly(z.date,2), 
                family='poisson'(link='log'),offset=log(accused+1),data=N_sample[N_sample$any_family_accused == 1,])
summary(model2.2)

# 3) NUMBER OF CONGREGATION FELLOWS ACCUSED (IN SEVERAL STEPS TOO)
# 3.1) Whether a deponent accused a family member or not
model3.1 <- glm(any_congregation_fellow_accused ~ woman + summoned + redeposition + accused_at_deposition + poly(z.date,2), 
                family='binomial'(link='logit'),offset=log(accused+1),data=N_sample)
summary(model3.1)

# 3.2) If the deponent accused at least one family member, contributions to how many
model3.2 <- glm(congregation_fellows_accused ~ woman + summoned + redeposition + accused_at_deposition + poly(z.date,2), 
                family='poisson'(link='log'),offset=log(accused+1),data=N_sample[N_sample$any_congregation_fellow_accused == 1,])
summary(model3.2)

########################################################################################################################

# Remove unnecessary objects
rm(list=setdiff(ls(), c("castellario","incriminated_nodes","denunciations","congregations","groups","nodes",
                        "kinships","congGraph","N_sample")))

# Save image
save.image('data/data4.RData')

########################################################################################################################