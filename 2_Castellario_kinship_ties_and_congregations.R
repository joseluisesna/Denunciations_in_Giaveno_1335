########################################################################################################################
## INQUISITION IN GIAVENO (1335)
## (2) Kinship & congregation ties
## R script written by Jose Luis Estevez (Vaestoliitto)
## Date: Sep 27th, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(igraph)

# DATA LOADING 
rm(list=ls())
load('data/data2.RData')

########################################################################################################################

# INFERRAL OF KINSHIP TIES FROM THE SAMPLE OF THOSE RECORDED 

# Castellario nodes and social ties
nodes <- castellario$nodes
soc <- castellario$kinship

# Let's select only close kinship ties: marriages, siblings, and parent-child ties
kin_kinds <- unique(soc$type)
kin_kinds <- kin_kinds[kin_kinds %in% c('son','wife','daughter','sister','brother','mother')]
soc <- soc[soc$type %in% kin_kinds,]

graphs <- list()
graphs[['marriage']] <- graph_from_edgelist(as.matrix.data.frame(soc[soc$type %in% 'wife',c('id1','id2')]),
                                            directed=FALSE)
graphs[['siblings']] <- graph_from_edgelist(as.matrix.data.frame(soc[soc$type %in% c('sister','brother'),c('id1','id2')]),
                                            directed=FALSE)
graphs[['parentchild']] <- graph_from_edgelist(as.matrix.data.frame(soc[soc$type %in% c('son','daughter'),c('id1','id2')]),
                                               directed=TRUE)
graphs[['parentchild2']] <- graph_from_edgelist(as.matrix.data.frame(soc[soc$type %in% c('mother'),c('id1','id2')]),
                                                directed=TRUE)

# Add vertices
for(i in seq_along(graphs)){
  graphs[[i]] <- igraph::add_vertices(graphs[[i]],
                              length(nodes$id[!(nodes$id %in% V(graphs[[i]])$name)]),
                              name= nodes$id[!(nodes$id %in% V(graphs[[i]])$name)])
}
graphs$siblings <- simplify(graphs$siblings,remove.multiple = TRUE) # remove duplicates

# Turn igraph objects into matrices and order row-columns in the exact same order
for(i in seq_along(graphs)){
  graphs[[i]] <- as.matrix(as_adjacency_matrix(graphs[[i]]))
  graphs[[i]] <- graphs[[i]][order(rownames(graphs[[i]])),order(colnames(graphs[[i]]))]
}

# Merge parentchild1 and parentchild2
graphs$parentchild <- graphs$parentchild + graphs$parentchild2
graphs$parentchild2 <- NULL

########################################################################################################################

# Now that we have marriages, siblings, and parent-child ties, we can star inferring more kinship ties
graphs$inf_siblings <- graphs$siblings %*% graphs$siblings
graphs$inf_siblings2 <- t(graphs$parentchild) %*% graphs$parentchild
graphs$inf_lovers <- graphs$parentchild %*% t(graphs$parentchild)
graphs$inf_grandparentchild <- graphs$parentchild %*% graphs$parentchild
graphs$inf_unclenephew <- graphs$siblings %*% graphs$parentchild
graphs$inf_parentchild <- graphs$parentchild %*% graphs$siblings
graphs$inf_siblingsinlaw <- graphs$siblings %*% graphs$marriage
graphs$inf_parentchild2 <- graphs$marriage %*% graphs$parentchild
graphs$inf_parentchildinlaw <- graphs$parentchild %*% graphs$marriage

# Transform back into edge lists
# First, the undirected matrices
for(i in c('marriage','siblings','inf_siblings','inf_siblings2','inf_lovers','inf_siblingsinlaw')){
  graphs[[i]] <- graph_from_adjacency_matrix(graphs[[i]],mode='max',diag=FALSE)
  graphs[[i]] <- as.data.frame(as_edgelist(graphs[[i]]))
  graphs[[i]]$role <- i
}
# Second, the directed matrices
for(i in c('parentchild','inf_grandparentchild','inf_unclenephew','inf_parentchild','inf_parentchild2','inf_parentchildinlaw')){
  graphs[[i]] <- graph_from_adjacency_matrix(graphs[[i]],mode='directed',diag=FALSE)
  graphs[[i]] <- as.data.frame(as_edgelist(graphs[[i]]))
  graphs[[i]]$role <- i
}

kinships <- do.call(rbind,graphs) # merge altogether
kinships <- kinships[!duplicated(kinships[,c('V1','V2')]),] # remove duplicated ties from those inferred

########################################################################################################################

# Now that we have ties inferred, let's bring it other recorded ties (social, unspecified kin, etc.)
soc <- castellario$kinship
# ties other than those used for inferring
soc <- soc[!(soc$type %in% kin_kinds),c('id1','id2','type')] 
# Let's remove the groups
soc <- soc[startsWith(soc$id2,'P'),]
# Let's remove non-kin
soc <- soc[soc$type %in% c('lover','daughter-in-law','granddaughter','cognate','kinperson'),]

# Merge all ties together
names(soc) <- c('V1','V2','role') 
all_ties <- rbind(kinships,soc)

########################################################################################################################

# VISUALISATION
kinship_graph <- graph_from_edgelist(as.matrix.data.frame(all_ties[,c('V1','V2')]),directed = FALSE)
E(kinship_graph)$kind <- all_ties$role # add type of tie

is_simple(kinship_graph) # any redundant tie?
# There are two redundant ties between P0184-P0185 ('cognate) and P0224-P0226 (kin-person)
kinship_graph <- simplify(kinship_graph,remove.multiple = TRUE,edge.attr.comb='first')
all_ties <- all_ties[!(all_ties$V1 == 'P0185' & all_ties$V2 == 'P0184') & 
                       !(all_ties$V1 == 'P0226' & all_ties$V2 == 'P0224'),]
# Add vertices not-connected to anybody via kinship
kinship_graph <- igraph::add_vertices(kinship_graph,
                              length(nodes$id[!(nodes$id %in% V(kinship_graph)$name)]),
                              name=nodes$id[!(nodes$id %in% V(kinship_graph)$name)])

# Add whether somebody is dead, and sex
V(kinship_graph)$deceased <- ifelse(nodes[match(V(kinship_graph)$name,nodes$id),]$dead == 1,'yes','no')
V(kinship_graph)$sex <- nodes[match(V(kinship_graph)$name,nodes$id),]$sex

# Let's show only kinship ties between those involved in the process: deponents and/accused
kinship_graph <- igraph::delete_vertices(kinship_graph,!(V(kinship_graph)$name %in% incriminated_nodes$id))

plot(kinship_graph,
     edge.color=ifelse(E(kinship_graph)$kind %in% c('marriage','lover','inf_lovers'),'sienna2', # spouses in orange
                       ifelse(E(kinship_graph)$kind %in% c('parentchild','inf_parentchild','inf_parentchild2'),'firebrick3', # parent-child in red
                              ifelse(E(kinship_graph)$kind %in% c('siblings','inf_siblings','inf_siblings2'),'springgreen4', # siblings in green
                                     ifelse(E(kinship_graph)$kind %in% c('inf_siblingsinlaw','inf_parentchildinlaw','daugther-in-law'),'purple',grey(.6))))), 
     edge.width=4,
     edge.lty=ifelse(startsWith(E(kinship_graph)$kind,'inf'),3,1),
     vertex.size=2,
     vertex.label=NA,
     layout=layout_nicely(kinship_graph),
     vertex.color=ifelse(V(kinship_graph)$deceased == 'yes',grey(0.5,0.2),
                         ifelse(V(kinship_graph)$sex == 'f','tomato','dodgerblue')),
     vertex.frame.color=ifelse(V(kinship_graph)$deceased == 'yes',grey(0,0.2),'black'),
     main='Kinship ties recorded and inferred'
     )
legend("bottomleft", legend = c('Male','Female','Deceased'), pch=21,
       col=c("black","black",grey(0,0.2)), pt.bg=c("dodgerblue","tomato",grey(0.5,0.2)),
       pt.cex=2, cex=0.7, bty="n", ncol=1,x.intersp = .5,y.intersp = .5,inset=c(-0.01,0))
legend("bottomright", legend = c('Siblings','Parent-child','Spouses/lovers','In-law','Other',
                                 'Siblings (inferred)','Parent-child (inferred)','Spouses/lovers (inferred)',
                                 'In-law (inferred)','Other (inferred)'),
       col=c('springgreen4','firebrick3','sienna2','purple',grey(.6),
             'springgreen4','firebrick3','sienna2','purple',grey(.6)),
       lty=c(1,1,1,1,1,3,3,3,3,3), 
       lwd=4,cex=0.7, bty="n",ncol=1,x.intersp = .5,y.intersp = .5,inset=c(-0.5,0))

########################################################################################################################

# CONGREGATION NETWORK

# We need to unpack the groups that are listed as members of a congregation
# So let's start by separating individual members from group members
congMembers <- congregations[startsWith(congregations$members,'P'),]
congGroups <- congregations[!startsWith(congregations$members,'P'),]

# Let's remove the square brackets
congGroups$members <- gsub('[','',congGroups$members,fixed = TRUE)
congGroups$members <- gsub(']','',congGroups$members,fixed = TRUE)

congGroups <- merge(congGroups,groups,by.x='members',by.y='group')
congGroups <- congGroups[,-1] # let's remove group labels
names(congGroups)[2] <- 'members' # and rename individuals as members

# Let's merge all the data back again
congregations <- rbind(congMembers,congGroups)
congregations <- congregations[!duplicated(congregations),] # remove duplicated

########################################################################################################################

# Let's turn this into a TWO-MODE NETWORK (person-congregation)
nodesSet1 <- unique(congregations$members) # individuals
nodesSet2 <- unique(congregations$congregation) # congregations
edgeList <- congregations # edges

congGraph <- graph.empty()
congGraph <- igraph::add_vertices(congGraph,nv=length(nodesSet1),attr=list(name=nodesSet1,
                                                                   type=rep('person',length(nodesSet1))))
congGraph <- igraph::add_vertices(congGraph,nv=length(nodesSet2),attr=list(name=nodesSet2,
                                                                   type=rep('congregation',length(nodesSet2))))
edgeListVec <- as.vector(t(as.matrix(data.frame(S1=edgeList$members,S2=edgeList$congregation))))
congGraph <- igraph::add_edges(congGraph,edgeListVec)
congGraph <-as.undirected(congGraph) # make ties undirected
igraph::is_bipartite(congGraph)

########################################################################################################################

# VISUALISATION

# Let's show only ties between those involved in the process: deponents and/accused
congGraph <- igraph::add_vertices(congGraph, # add those who are not already
                                  length(incriminated_nodes$id[!(incriminated_nodes$id %in% V(congGraph)$name)]),
                                  name=incriminated_nodes$id[!(incriminated_nodes$id %in% V(congGraph)$name)],
                                  type='person')
congGraph <- igraph::delete_vertices(congGraph, # remove those beyond the sample
                                     startsWith(V(congGraph)$name,'P') & # remove only persons (not congregations)
                                       !(V(congGraph)$name %in% incriminated_nodes$id))

# Further customization (nodes attributes)
deponents <- incriminated_nodes[incriminated_nodes$deposed == 1,]$id # deponent vs. accused
deceased <- incriminated_nodes[incriminated_nodes$dead == 1,]$id # deceased
V(congGraph)$deponent <- ifelse(V(congGraph)$name %in% deponents,1,0)
V(congGraph)$deceased <- ifelse(V(congGraph)$name %in% deceased,1,0)

plot(congGraph,
     edge.color='darkgrey', 
     vertex.size=ifelse(V(congGraph)$type == 'person',2,2.5),
     vertex.shape=ifelse(V(congGraph)$type == 'person','circle','square'),
     vertex.label=NA,
     layout=layout_with_kk(congGraph),
     vertex.color=ifelse(V(congGraph)$type == 'congregation','cornsilk',
                         ifelse(V(congGraph)$deceased == 1,grey(.5,.2),
                                ifelse(V(congGraph)$deponent == 1,'tomato','dodgerblue'))),
     vertex.frame.color=ifelse(V(congGraph)$deceased == 1,grey(0,0.2),'black'),
     main='Bipartite network (person-congregation)'
)
legend("bottomleft", legend = c('Congregation','Deponent','Accused','Deceased'), 
       pch=c(22,21,21,21),col=c("black","black","black",grey(0,0.2)),
       pt.bg=c('cornsilk','tomato','dodgerblue',grey(.5,.2)),
       pt.cex=2, cex=0.75, bty="n",ncol=1,y.intersp = 0.5)

########################################################################################################################

# PROJECTION TO A ONE-MODE NETWORK
V(congGraph)$type <- bipartite_mapping(congGraph)$type
congGraph <- bipartite_projection(congGraph) 

congGraph <- as.matrix(as_adjacency_matrix(congGraph$proj1)) # the person-person matrix
congGraph <- congGraph[order(rownames(congGraph)),order(colnames(congGraph))] # order rows and columns

########################################################################################################################

# Remove unnecessary objects
kinships <- all_ties
rm(list=setdiff(ls(), c("castellario","incriminated_nodes","denunciations","congregations","groups",
                        "kinships","congGraph")))

# Save image
save.image('data/data3.RData')

########################################################################################################################