########################################################################################################################
## INQUISITION IN GIAVENO (1335)
## (1) Data exploration
## R script written by Jose Luis Estevez (Vaestoliitto)
## Date: Sep 25th, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(data.table);library(ggplot2);library(igraph);library(sna);library(network);library(netseg)

# DATA LOADING 
rm(list=ls())
load('data/data.RData')

########################################################################################################################

# COLLECTIVE ENTITIES
# Let's start by unpacking groups and congregations
congregations <- castellario$congregations
groups <- castellario$groups

# CONGREGATIONS
congregations <- congregations[,c('id','participants','ministers')]
# Let's bring all people involved together, whether participants or ministers
congregations$participants <- paste(congregations$participants,congregations$ministers,sep='#')
congregations <- congregations[,c('id','participants')]

# Let's turn this into a dataset
participants <-  strsplit(congregations$participants,split='#')
for(i in seq_along(participants)){
  participants[[i]] <- data.frame(members = as.vector(participants[[i]]))
  participants[[i]]$congregation <- congregations$id[i]
}
congregations <- do.call(rbind,participants)
# Let's turn all letters uppercase in case of typos
congregations$members <- toupper(congregations$members)
congregations$congregation <- toupper(congregations$congregation)

# GROUPS
# Let's keep only the group ID and the group members
groups <- groups[,c('id','members')]
groups <- groups[!is.na(groups$members),] # Let's remove groups with non-identifiable members

# Now, let's turn this into a dataset
groupmembers <- strsplit(groups$members,split='#')
for(i in seq_along(groupmembers)){
  groupmembers[[i]] <- data.frame(members = as.vector(groupmembers[[i]]))
  groupmembers[[i]]$group <- groups$id[i]
}
groups <- do.call(rbind,groupmembers)

# remove unnecessary objects
rm(groupmembers);rm(participants)

########################################################################################################################

# SAMPLE SELECTION
# Let's have a look at individuals and denunciations/other ties in our data
nodes <- castellario$nodes # all people in the trial 
ties <- castellario$edges # denunciations 

# SAVE WHO DEPOSED: Let's save the IDs of all those who were deposed
deposed <- castellario$keyevents[castellario$keyevents$type == 'deposition',]$id
deposed <- unique(deposed) # 110 individuals

# NAMED PEOPLE AND GROUP denunciations
any(!(ties$source %in% nodes$id)) # Any deponent that is not in the node list? No
any(!(ties$target %in% nodes$id)) # Any target that is not in the node list? Yes
which(!(ties$target %in% nodes$id)) # who?
(groups_accused <- ties$target[which(!(ties$target %in% nodes$id))]) # All these are groups (households)
groups_accused <- ties[ties$target %in% groups_accused,] # I will keep denunciations to group aside for now

# Let's unpack the groups
groups_accused <- merge(groups_accused,groups,by.x='target',by.y='group')
groups_accused <- groups_accused[,-1] # remove group labels
names(groups_accused)[4] <- 'target' # rename group members as target
ties <- rbind(ties,groups_accused) # let's merge individual and group accusations
ties <- ties[which(ties$target %in% nodes$id),] 

# NON-DUPLICATED TIES: Finally let's see whether some denunciations may be duplicated
any(duplicated(ties[,c('source','target','time')])) # Yes
which(duplicated(ties[,c('source','target','time')])) 
ties <- ties[!duplicated(ties[,c('source','target','time')]),] 

# REMOVE SELF-denunciations (loops)
ties <- ties[ties$source != ties$target,] # Ties reduced to 982

# REMOVE WALDENSIAN MASTERS: These are not actual accusations (accused are those being seen with them)
waldensian_masters <- castellario$nodes[castellario$nodes$waldensian_master == 1,]$id
ties <- ties[!(ties$target %in% waldensian_masters),] # For a total of 829 denunciations

# SAVE WHO ACCUSED AT LEAST SOMEBODY ELSE, AND WHO IS ACCUSED BY AT LEAST SOMEBODY ELSE
accusers <- unique(ties$source) # 92
accused <- unique(ties$target) # 239
# And those who were both accused and deposed
accused_deposed <- intersect(deposed,accused) # 82

# Summary table
data.frame(Node_type = c('Deposed','Accusers','Accused','Accused & deposed'),
           N = c(length(deposed),length(accusers),length(accused),length(accused_deposed)))

######################

# IMPUTTING MISSING DATES

# There are two denunciations from P0051 whose date we don't know
ties[is.na(ties$time),]
# It seems that this person deposed twice, so those ties could have been reported either the 3rd or the 9th of Feb
castellario$keyevents[castellario$keyevents$id == 'P0051' & castellario$keyevents$type == 'deposition',]
# It seems likely that these denunciations happen in 2nd deposition
t <- castellario$keyevents[castellario$keyevents$id == 'P0051' & castellario$keyevents$type == 'deposition',]$time[2]
ties[is.na(ties$time),]$time <- t

########################################################################################################################

# NODES ATTRIBUTES

# EXTRA DIMENSIONS: Let's add to these nodes whether they were deposed or not, accusers or not, and accused or not
nodes$deposed <- nodes$id %in% deposed * 1
nodes$accuser <- nodes$id %in% accusers * 1
nodes$accused <- nodes$id %in% accused * 1

# REMOVE NON-DEPOSED AND NON-ACCUSED: Let's remove not involved as either deponents or accused
nodes <- nodes[(nodes$deposed + nodes$accused) != 0,] # 267 nodes

# Turn into a data.table object
nodes <- as.data.table(nodes)

# Summary table: female prop, known occupation and known residence
nodes[,.(prop_female=sum(sex == 'f')/sum(!is.na(sex)),
         prop_known_job=sum(!is.na(occupation_type))/length(occupation_type),
         prop_known_residence=sum(!is.na(origin_or_residence))/length(origin_or_residence),
         prop_dead=sum(dead)/length(dead),
         prop_deposed=sum(deposed == 1)/length(deposed))]
# By whether the individuals was deposed or not
nodes[,.(prop_female=sum(sex == 'f')/sum(!is.na(sex)),
         prop_known_job=sum(!is.na(occupation_type))/length(occupation_type),
         prop_known_residence=sum(!is.na(origin_or_residence))/length(origin_or_residence)),
      keyby=.(deposed)]

########################################################################################################################

# NETWORK OF DENUNCIATIONS

# IGRAPH OBJECT: Turn the ties into an igraph object
graph <- graph_from_edgelist(as.matrix.data.frame(ties[,c("source","target")]),directed = TRUE)
# Plus isolates
nodes$id[!(nodes$id %in% V(graph)$name)]
graph <- igraph::add_vertices(graph,
                      length(nodes$id[!(nodes$id %in% V(graph)$name)]),
                      name= nodes$id[!(nodes$id %in% V(graph)$name)])

# ADD ATTRIBUTES OF THE NODES TO THE GRAPH: SEX, OCCUPATION TYPE, RESIDENCE
nodes <- nodes[match(V(graph)$name,nodes$id),] # Match IDs in the same order

V(graph)$label <- nodes$label
V(graph)$sex <- nodes$sex
V(graph)$deceased <- ifelse(nodes$dead == 0,'no','yes')

# ADD ATTRIBUTES OF THE EDGES TO THE GRAPH: DATE OF THE DEPOSITION
E(graph)$date <- ties$time

# Since some deponents accused the same target in more than one deposition, some ties can be repeated
is_simple(graph) # only 6 cases, from Gauterii's redeposition
# Let's keep only the first time the tie was reported for visualisation purposes
graph <- simplify(graph,remove.multiple = TRUE,edge.attr.comb='first')

# VISUALISATION OF THE GRAPH
set.seed(0708)
common_layout <- layout_with_fr(graph)

plot(graph,
     vertex.label=NA,vertex.size=2,
     vertex.color=ifelse(V(graph)$deceased == 'yes',grey(.5,.2),
                         ifelse(V(graph)$sex == 'f','tomato','dodgerblue')),
     vertex.frame.color=ifelse(V(graph)$deceased == 'yes',grey(0,0.2),'black'),
     edge.arrow.size=.2,edge.color='darkgrey',edge.lty=1,
     layout=common_layout, # Fruchterman-Reingold layout
     main='Graph visualization of the denunciations')
legend("bottomleft",legend = c('Woman','Man','Deceased'), pch=21,
       col=c("black","black",grey(0,0.2)), pt.bg=c("tomato","dodgerblue",grey(0.5,0.2)),
       pt.cex=1, cex=1, bty="o", ncol=1)

########################################################################################################################

# NETWORK DESCRIPTIVE STATS (I will use sna and network rather than the igraph package)
mtx <- as.matrix(as_adjacency_matrix(graph))

# First, let's send to NAs the rows of those who were not deposed
mtx[!(rownames(mtx) %in% deposed),] <- NA
diag(mtx) <- NA # Also the diagonal

# As network object
ntw <- network(mtx,
               vertex.attr = list(nodes$label,nodes$sex,nodes$occupation_type,nodes$origin_or_residence),
               vertex.attrnames = list('label','sex','occupation','residence')) 

# DESCRIPTIVE
data.frame(stat = c('nodes','ties','missing_tie_frac','density','recip','trans',
                    'ave_degree','sd_out','sd_ind','degree centralization',
                    'components','isolates','EI (sex)','Assort coeff'),
           val = c(vcount(graph), # nodes
                   ecount(graph),  #ties
                   network::network.naedgecount(ntw) / (vcount(graph)*(vcount(graph)-1)), # missing tie fraction
                   gden(ntw),
                   grecip(ntw,measure='edgewise'),
                   transitivity(graph),
                   mean(degree(ntw,cmode='outdegree')),
                   sd(degree(ntw,cmode='outdegree')),
                   sd(degree(ntw,cmode='indegree')),
                   centr_degree(graph,mode='total')$centralization,
                   length(igraph::components(graph)$csize),
                   sum(igraph::components(graph)$csize == 1),
                   ei(graph,'sex'), # EI index
                   assort(graph,'sex') # Newman's assortative coefficient
           ))

# More on homophily by gender
mixingm(graph,"sex",full=TRUE)
orwg(graph,"sex") # Odds ratio
gamix(graph,"sex")
# By gender
coleman(graph,"sex")
smi(graph,"sex") # Segregation matrix index

########################################################################################################################

# TEMPORAL PART

# Let's see how many new depositions and new targets we get over time
ties <- as.data.table(ties)
new_accused <- ties[!duplicated(ties$target),] # only new targets

# denunciations 
denunciations <- ties[,length(target),keyby=time] # denunciations per day
new_accused <- new_accused[,length(target),keyby=time] # new targets per day

# Number of deponents per date
deponent_date <- castellario$edges[!duplicated(castellario$edges[,c('source','time')]),]
deponent_date <- deponent_date[!is.na(deponent_date$time),] # remove missing dates
deponent_date <- as.data.table(deponent_date)
deponent_date <- deponent_date[,length(source),keyby=time] 

# Let's find the first and last day a deponent went to the Inquisitor (irrespective of where any accused)
temp_data <- data.frame(time = seq(min(castellario$edges$time,na.rm = TRUE),
                                   max(castellario$edges$time,na.rm = TRUE),by="days"))

# Now, let's merge everything together
temp_data <- merge(temp_data,denunciations,by='time',all.x=TRUE)
temp_data <- merge(temp_data,new_accused,by='time',all.x=TRUE)
temp_data <- merge(temp_data,deponent_date,by='time',all.x=TRUE)

temp_data[is.na(temp_data)] <- 0 # Zeroes when no deposition
names(temp_data) <- c('time','denunciations','new_targets','number_deponents')
temp_data$new_targets <- cumsum(temp_data$new_targets) # cumsum of new targets

# denunciations per deponent
temp_data$denunciations_per_deponent <- temp_data$denunciations / temp_data$number_deponents
temp_data$denunciations_per_deponent[is.nan(temp_data$denunciations_per_deponent)] <- 0

# Change data from from wide to long
temp_data <- tidyr::gather(temp_data,key="category",value='number',-time)
temp_data$category <- factor(temp_data$category,levels=c('number_deponents','denunciations','denunciations_per_deponent','new_targets'),
                             labels=c('Deponents','denunciations','denunciations per deponent','Accumulated targets'))
temp_data[temp_data$number == 0,]$number <- NA # Zeroes to NAs

# To see if over-dispersion in the number of denunciations per day, or in the number of people deposed
inc_var <- var(temp_data[temp_data$category == 'denunciations',]$number,na.rm = TRUE)
inc_mean <- mean(temp_data[temp_data$category == 'denunciations',]$number,na.rm = TRUE)

ind_var <- var(temp_data[temp_data$category == 'Deponents',]$number,na.rm = TRUE)
ind_mean <- mean(temp_data[temp_data$category == 'Deponents',]$number,na.rm = TRUE)

inc_dep_var <- var(temp_data[temp_data$category == 'denunciations per deponent',]$number,na.rm = TRUE)
inc_dep_mean <- mean(temp_data[temp_data$category == 'denunciations per deponent',]$number,na.rm = TRUE)

ggplot(data=temp_data)+
  geom_line(aes(x=time,y=number,color=category)) +
  geom_point(aes(x=time,y=number,color=category),size=3.5,alpha=.65) +
  scale_colour_manual(values = c('royalblue','darkgoldenrod','firebrick3','cyan4'))+
  theme_classic() +
  scale_y_continuous(trans='log10',breaks = c(1:5,10,25,50,100,200)) +
  scale_x_continuous(breaks = temp_data$time[!is.na(temp_data$time)]) +
  theme(axis.text.x = element_text(angle=90, vjust =0.5, hjust=1)) +
  xlab('Time') + ylab(expression(paste('Count (', log[10], ' scale)'))) +
  theme(legend.position="top", legend.justification="center",legend.title=element_blank()) +
  annotate("label",x= quantile(as.numeric(temp_data$time),0.92),y=20,color='darkgoldenrod',size=3.5,
           label=paste("var/mean = ",round(inc_var/inc_mean,0))) +
  annotate("label",x= quantile(as.numeric(temp_data$time),0.92),y=2,color='royalblue',size=3.5,
           label=paste("var/mean = ",round(ind_var/ind_mean,0))) +
  annotate("label",x= quantile(as.numeric(temp_data$time),0.92),y=5,color='firebrick3',size=3.5,
           label=paste("var/mean = ",round(inc_dep_var/inc_dep_mean,0))) 

########################################################################################################################

# NETWORK EVOLUTION (VISUALIZATION)
# Let's visualize the network graph over time
# Dates to divide the graph
key_dates <- unique(ties$time)
key_dates <- key_dates[!is.na(key_dates)]
key_dates <- key_dates[order(key_dates)] # order chronologically

graph <- delete_edges(graph,which(is.na(E(graph)$date))) # let's remove the ties we don't know when they were reported

snapshot_ntw <- list()
for(i in seq_along(key_dates)){
  snapshot_ntw[[i]] <- delete_edges(graph,which(E(graph)$date > as.numeric(key_dates[[i]])))
}
names(snapshot_ntw) <- as.character(key_dates) # add names with the date

# VISUALISATION
tiff(filename="Fig3.tiff",
     width=25, height=25,units="cm", 
     compression="lzw",
     bg="white",
     res=1000
)
par(mfrow=c(2,2))
# Denunciations before Grace Period
plot(snapshot_ntw$`1335-01-28`,
     vertex.label=NA,vertex.size=3,
     edge.width=.5,edge.arrow.size=.15,edge.lty=1,
     vertex.color=ifelse(igraph::degree(snapshot_ntw$`1335-01-28`,mode='total') == 0,grey(0.5,0.0),
                         ifelse(V(snapshot_ntw$`1335-01-28`)$deceased == 'yes',grey(0.5,0.2),
                                ifelse(V(snapshot_ntw$`1335-01-28`)$sex == 'f','tomato','dodgerblue'))),
     vertex.frame.color=ifelse(igraph::degree(snapshot_ntw$`1335-01-28`,mode='total') == 0,grey(0,0.0),
                               ifelse(V(snapshot_ntw$`1335-01-28`)$deceased == 'yes',grey(0,0.2),'black')),
     edge.color='black',
     layout=common_layout,
     main='Denunciations before the Grace Period\n(January 20-January 28, 1335)')
# Denunciations during Grace Period
plot(snapshot_ntw$`1335-01-31`,
     vertex.label=NA,vertex.size=3,
     edge.width=.5,edge.arrow.size=.15,edge.lty=1,
     vertex.color=ifelse(igraph::degree(snapshot_ntw$`1335-01-31`,mode='total') == 0,grey(0.5,0.0),
                         ifelse(V(snapshot_ntw$`1335-01-31`)$deceased == 'yes',grey(0.5,0.2),
                                ifelse(V(snapshot_ntw$`1335-01-31`)$sex == 'f','tomato','dodgerblue'))),
     vertex.frame.color=ifelse(igraph::degree(snapshot_ntw$`1335-01-31`,mode='total') == 0,grey(0,0.0),
                               ifelse(V(snapshot_ntw$`1335-01-31`)$deceased == 'yes',grey(0,0.2),'black')),
     edge.color= ifelse(E(snapshot_ntw$`1335-01-31`)$date < key_dates[6],grey(0.5,.35),'black'),
     layout=common_layout,
     main='Denunciations during the Grace Period\n(January 29-January 31, 1335)')
# Denunciations first week after Grace Period
plot(snapshot_ntw$`1335-02-07`,
     vertex.label=NA,vertex.size=3,
     edge.width=.5,edge.arrow.size=.15,edge.lty=1,
     vertex.color=ifelse(igraph::degree(snapshot_ntw$`1335-02-07`,mode='total') == 0,grey(0.5,0.0),
                         ifelse(V(snapshot_ntw$`1335-02-07`)$deceased == 'yes',grey(0.5,0.2),
                                ifelse(V(snapshot_ntw$`1335-02-07`)$sex == 'f','tomato','dodgerblue'))),
     vertex.frame.color=ifelse(igraph::degree(snapshot_ntw$`1335-02-07`,mode='total') == 0,grey(0,0.0),
                               ifelse(V(snapshot_ntw$`1335-02-07`)$deceased == 'yes',grey(0,0.2),'black')),
     edge.color= ifelse(E(snapshot_ntw$`1335-02-07`)$date < key_dates[9],grey(0.5,.35),'black'),
     layout=common_layout,
     main='Denunciations the week after the Grace Period\n(February 1-February 7, 1335)')
# Legend to the plot
legend("bottomleft",legend = c('Woman','Man','Deceased'), pch=21,
       col=c("black","black",grey(0,0.2)), pt.bg=c("tomato","dodgerblue",grey(0.5,0.2)),
       pt.cex=.8, cex=.8, bty="o", ncol=1)
# All denunciations by the end of the trial
plot(snapshot_ntw$`1335-02-21`,
     vertex.label=NA,vertex.size=3,
     edge.width=.5,edge.arrow.size=.15,edge.lty=1,
     vertex.color=ifelse(igraph::degree(snapshot_ntw$`1335-02-21`,mode='total') == 0,grey(0.5,0.0),
                         ifelse(V(snapshot_ntw$`1335-02-21`)$deceased == 'yes',grey(0.5,0.2),
                                ifelse(V(snapshot_ntw$`1335-02-21`)$sex == 'f','tomato','dodgerblue'))),
     vertex.frame.color=ifelse(igraph::degree(snapshot_ntw$`1335-02-21`,mode='total') == 0,grey(0,0.0),
                               ifelse(V(snapshot_ntw$`1335-02-21`)$deceased == 'yes',grey(0,0.2),'black')),
     edge.color= ifelse(E(snapshot_ntw$`1335-02-21`)$date < key_dates[14],grey(0.5,.35),'black'),
     edge.color=grey(0.25,1),
     layout=common_layout,
     main="Denunciations after the application of torture\n(February 8-February 23, 1335)")
dev.off()

########################################################################################################################

# Remove unnecessary objects
denunciations <- ties
incriminated_nodes <- nodes
rm(list=setdiff(ls(), c("castellario","incriminated_nodes","denunciations","congregations","groups")))

# Save image
save.image('data/data2.RData')

########################################################################################################################