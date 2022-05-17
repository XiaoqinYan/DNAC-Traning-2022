###Loading Packages
library(tidyverse)
library(statnet)

###Help
#For more info on the statnet package
help(package = statnet)


######################
#   IMPORTING DATA   #
######################

###Import saved, clean network data made in previous lab.
 #Note: you want the one that says 'Network' and '.Rda',
  #not any of the other AHS pieces from prior labs.
load("C:/Users/mc418/Dropbox/Duke/SNH2019/AHS_Network.Rda")

#check network characteristics
AHS_Network
 #Do you see a description of a network? If not, go back up
  #and repeat to find the correct file...

#We see 440 vertices (aka nodes), directed, no loops,
 #not bipartite, has vertex attributes, looks good!

############################
#   FUNDAMENTAL CONCEPTS   #
############################

#Remember, in this dataset:
 #Vertex/Node = student
 #Edge = friendship nomination
 #Students are grades 7-12, nested within schools
 #Friendship nominations capped at 5 male and 5 female friends

###Basic Measures

#Network Size
 #We also see this when we print the description of the network
network.size(AHS_Network)

#Number of Edges
 #(also in the description of the network)
network.edgecount(AHS_Network) 

#Number of Dyads (Node Pairs)
network.dyadcount(AHS_Network) 


######################################
#   Node-level Centrality measures   #
######################################

###Degree Centrality

#Total Degree Centrality: 
 #The Total Number of Adjacent Nodes
 #Here, total sent and received friendship nominations
 #total degree is default of 'degree' function
TotalDegree <- degree(AHS_Network)

#Because we have directed data, we can distinguish
 #between in-degree and out-degree centrality:

#In-Degree Centrality
 #The number of nodes adjacent to node i (W&F, p. 126) i <--
 #Here, the number of friendship nominations ego receives
InDegree <- degree(AHS_Network, cmode="indegree")

#Out-Degree Centrality
 #The number of nodes adjacent from node i (W&F, p. 126) i -->
 #Here, the number of friendship nominations ego sends
OutDegree <- degree(AHS_Network, cmode="outdegree")

#After calculating centrality measures for each node,
 #can use them with standard R methods
 #(like any other variable or R object):

#Plot indegree by outdegree:
 #This can be useful for finding outliers
  #or to double-check for errors in making the network
#A simple plot
plot(InDegree,
     OutDegree)
#See many observations stacked on top of each other
 #(expected, because only whole values for degree)

#A better plot
 #This adds a line for the diagonal and id labels
plot(InDegree,
     OutDegree,
     type="n",
     xlab="Received Friendships",
     ylab= "Sent Friendships")
abline(0, 1, lty=3)   
text(jitter(InDegree), 
     jitter(OutDegree), 
     network.vertex.names(AHS_Network), 
     cex=0.75, 
     col=2)

#See many nodes near the diagonal (most students
 #receive and send about the same number of friendship
 #nominations), but it skews toward some nodes who
 #receive more nominations than they send:
 #as expected given the nomination spaces are capped at 10!


     
###Plotting sociogram with Degree Centrality for each node

#Make degree measure we calculated network vertex attributes
set.vertex.attribute(AHS_Network, "TotalDegree", TotalDegree)
set.vertex.attribute(AHS_Network, "InDegree", InDegree)
set.vertex.attribute(AHS_Network, "OutDegree", OutDegree)

#Plot node Indegree
 #A simple graph for now
 #We set a seed so the graph will be oriented the same way
  #if we run it again (or want to visually compare measures)
set.seed(12345)
gplot(AHS_Network,           #network to use
      vertex.cex=(InDegree), #size nodes by InDegree
      vertex.sides=50,       #make node shapes circles
      displaylabels=FALSE)   #no labels (visual clutter)

#Oops! Some high in-degree nodes are taking over,
 #let's rescale so that doesn't happen

#Scaling node-size by degree:
set.seed(12345)
gplot(AHS_Network, 
      vertex.cex=(InDegree)*.2, 
      vertex.sides=50,
      displaylabels=FALSE)

#See some variation in in-degree and some
 #high indegree clusters in the dense parts of
 #the network, as expected

#Plotting Outdegree
set.seed(12345)
gplot(AHS_Network, 
      vertex.cex=(OutDegree)*.3, 
      vertex.sides=50,
      vertex.col = "coral", 
      displaylabels=FALSE)

#Plotting Totaldegree
set.seed(12345)
gplot(AHS_Network, 
      vertex.cex=(TotalDegree)*.15, 
      vertex.sides=50,
      vertex.col = "steelblue1", 
      displaylabels=FALSE)



###Closeness Centrality
 #Closeness is the inverse average distance between 
   #actor i and all other actors (W&F p. 185)
 #Ranges from 0-1 but does not work if there are disconnected
  #components because the distance is technically infinite
 #Acton & Jasny's tutorial makes an alternative closeness function

Closeness <- closeness(AHS_Network, 
                       gmode="digraph", 
                       cmode="directed")
Closeness
hist(Closeness , xlab="Closness", prob=TRUE)
#Doesn't work due to disconnected components

#Measure closeness on only the largest strong component
 #(more about components later on, this is just to make
  #closeness work for now by only looking at connected nodes)
cl <- component.largest(AHS_Network, connected="strong")

Closeness2 <- closeness(AHS_Network[cl,cl], #index component only
                        gmode="digraph", 
                        cmode="directed")
Closeness2
hist(Closeness2 , xlab="Closeness", prob=TRUE) 
#See distribution of closeness, some nodes are close
 #to many others in the network, fairly normal within
 #range of closeness

#Could visualize this on the graph like degree,
 #but for the sake of time, we won't right now.

#Note, can also calculate undirected closeness 
#by specifying (gmode="graph", cmode="undirected)



###Bonacich Centrality
 #Indicates connections to connected alters
 #Change Beta parameter (called 'exponent' by statnet) to
  #change the weight given to alters' centrality:
 #0 emphasizes local structure (equivalent to degree),
  #up to 1 which emphasizes global reach.
 #Negative Beta makes being tied to nodes who are
  #less (rather than more) central generate a 
  #higher Bonacich centrality score.
 #Like closeness, it will not calculate across
  #several disconnected components.

#Calculating Bonacich on largest strong component only
Bonacich1 <- bonpow(AHS_Network[cl,cl], 
                    gmode="digraph", 
                    exponent=1)
Bonacich1
hist(Bonacich1, xlab="Bonacich, Beta=1", prob=TRUE )
#see few very high and very low on Bonacich, most
 #nodes around 0 when Beta=1

#Calculating with a lower beta parameter
Bonacich2 <- bonpow(AHS_Network[cl,cl], 
                    gmode="digraph", 
                    exponent=.25)
Bonacich2
hist(Bonacich2, xlab="Bonacich, Beta=.25", prob=TRUE )
#See most nodes have a lower value if a lower Beta:
 #prioritizing centralized, local structure

#Calculating with a negative Beta
Bonacich3 <- bonpow(AHS_Network[cl,cl], 
                    gmode="digraph", 
                    exponent=-.5)
Bonacich3
hist(Bonacich3, xlab="Bonacich, Beta=-.5", prob=TRUE )

#Plotting  Bonacich centrality
set.seed(12345)
gplot(AHS_Network[cl, cl], 
      vertex.cex=(Bonacich2), 
      vertex.sides=50,
      vertex.col = "gold", 
      displaylabels=FALSE)

#Plot Bonacich that uses negative Beta
set.seed(12345)
gplot(AHS_Network[cl, cl], 
      vertex.cex=(Bonacich3), 
      vertex.sides=50,
      vertex.col = "chartreuse4", 
      displaylabels=FALSE)

#Let's look at these Bonacich networks side by side:

#set up 2-panel display
par(mfrow=c(1,2))

#rerun both plots
set.seed(12345)
gplot(AHS_Network[cl, cl], 
      vertex.cex=(Bonacich2), 
      vertex.sides=50,
      vertex.col = "gold", 
      displaylabels=FALSE)

set.seed(12345)
gplot(AHS_Network[cl, cl], 
      vertex.cex=(Bonacich3), 
      vertex.sides=50,
      vertex.col = "chartreuse4", 
      displaylabels=FALSE)

#See 'gold' plot on left where B=.25, nodes
 #with higher values are in dense, central structures
 #embedded in network;
 #For the 'green' plot on right where B=-.5, higher
 #Bonacich nodes are connected to other less central,
 #fairly disconnected nodes (easy to imagine how each
 #might be useful if thinking about a disease or 
 #information spreading in a network, or actors with
 #different types of power 'local' to parts of the net).

#return plot display to one cell
par(mfrow=c(1,1))



###Information Centrality
 #Conceptually, info centrality is like a hybrid of closeness
 #and Bonacich centralities. High info centrality
 #indicates a large number of short paths to many others.
?infocent  #For more information

Info <- infocent(AHS_Network, rescale=TRUE)
Info
hist(Info , xlab="Information Centrality", prob=TRUE) 

#See relatively little variation in information centrality here.
#If examine only connected component, distribution is 
 #similar to other centrality measures.



###Betweenness Centrality
 #Betweenness Centrality (for node i) is the ratio of the 
   #sum of all shortest paths linking j and k that 
   #includes node i (W&F, p. 191).
 #High Betweenness indicates an actor who is on many 
   #paths between other actors, similar to bridging

Betweenness <- betweenness(AHS_Network, gmode="digraph")  
Betweenness
hist(Betweenness , xlab="Betweenness Centrality", prob=TRUE) 

#See some values of betweenness are very big, 
 #so rescale as square-root
BetweenSqrt <- sqrt(Betweenness)

#Plot Betweenness
set.seed(12345)
gplot(AHS_Network, 
      vertex.cex=BetweenSqrt/20, 
      vertex.sides=50,
      vertex.col = "cyan", 
      displaylabels=FALSE)

#See many nodes in 'bridging' positions connecting
 #others, some with very high values, not always 
 #in densest parts of the network

#Take a closer look at Indegree and Betweenness
 #in the largest strong component (doesn't need
 #to be restricted to largest component, just 
 #doing that here to be able to look closely)
set.seed(12345)
gplot(AHS_Network[cl,cl], 
      vertex.cex=(InDegree)*.2, 
      vertex.sides=50,
      vertex.col=heat.colors(max(BetweenSqrt)+1),
      displaylabels=FALSE)

#Here, node color represents betweenness and node
 #size shows indegree
 #See directed nature, many 'bridges' at work,
 #but many high indegree/low betweenness nodes, and
 #vice versa

#What does that fancy color scale mean?
 #checking direction of 'heat.colors':
#barplot(rep(1,12), col =  heat.colors(12))



###Comparing centrality Measures
 #Remember, centrality is just one more attribute for each node,
  #so you can do any basic R techniques you would normally use:

#Visualize Indegree vs. Betweenness
par(cex=2,las=1)
plot(InDegree,BetweenSqrt)

#Compare correlations
cor(cbind(InDegree, OutDegree, BetweenSqrt, Info))



###A note on calculating stuctural holes, brokerage, and similar measures###
 
#Burt's (1992) measures of structural holes are supported by iGraph
 #Descriptions and code to run these measures are here: http://igraph.org/r/doc/constraint.html

#Brokerage: The brokerage measure in the sna package is a group oriented measure. 
#It is not the Burt style of brokerage.
 #Instead, this type of brokerage, the Gould-Fernandez measure, focuses on 
  #groups based on attributes and different roles,
  #such as gatekeeping, liaisons, or representatives.


###############################################
#   Centralization & Whole Network Measures   #
###############################################

###Density
 #Ratio of Observed Ties/All Possible Ties
gden(AHS_Network, mode = 'digraph')

#Density is generally low for adolescent social networks

#Degree Distribution
 #Using Indegree and Outdegree calculated above

#Visualizing degree distribution

#set plot display to multiple cells
par(mar = rep(2, 4))
par(mfrow=c(2,2)) 

hist(InDegree, xlab="Indegree", 
     main="In-Degree Distribution", prob=FALSE)
hist(OutDegree, xlab="Outdegree", 
     main="Out-Degree Distribution", prob=FALSE)
hist(InDegree+OutDegree, xlab="Total Degree", 
     main="Total Degree Distribution", prob=FALSE)

#Return plot display to 1 cell
par(mfrow=c(1,1)) 



###Clustering Coefficient (Transitivity)
 #A triad is transitive when i --> k if i --> j and j --> k (W&F, p. 243)
gtrans(AHS_Network)

#'Weak census' returns the count of transitive triads.
gtrans(AHS_Network, mode='digraph', measure='weakcensus')

#see 2428 transitive triads in the network


###Centralization of other Centrality Measures
 #Testing how concentrated a given centrality measure
  #is among a few nodes

#Indegree Centralization
 #In this network: is popularity highly concentrated
  #or more evenly distributed in the network?
centralization(AHS_Network, degree, cmode="indegree")

#Betweenness Centralization
centralization(AHS_Network, betweenness)

#This can be useful if comparing across networks,
 #but what does a betweenness centralization of .016
 #or indegree centralization of .03 for a network mean?



###CUG (Conditional Uniform Graph) Test
 #Compare clustering observed in graph 
   #to that expected by chance (W&F, pp.543-545)
 #Simulating networks of same size (or whichever parameter you choose)
  #then measuring a chosen feature of each network, and comparing
  #values between the observed 'real' network and simulated networks
 #Can compare networks of same size, density, dyad census, etc.

#(Sidebar: not getting too in-depth into these methods since they 
  #stray from the 'descriptives' purview of this lab. You'll hear
  #more in this vein on Thursday.)

#NOTE: These can take a little while to run: If you have a
 #slow machine, you might want to just follow along and watch

#Test Indegree Centralization against change networks of same size
Cug_In <-cug.test(AHS_Network,
                  centralization,
                  cmode="size",
                  FUN.arg=list(FUN=degree, cmode="indegree"))
#Print It
Cug_In
#Plot It
plot(Cug_In)
#The AHS network has much less indegree centralization than
 #we would expect by chance of a network of the same size

#Test transitivity against chance networks of same density
Cug_Trans <- cug.test(AHS_Network,
                      gtrans,
                      cmode="edges")
Cug_Trans
plot(Cug_Trans)
#See transitivity is higher than what is expected by 
#chance for a given network of same density

#Note: This can also be a useful first step before moving
 #on to more complicated predictive or simulated models.



###########################
#   MESO-LEVEL MEASURES   #
###########################

#Dyads
#Null: Not connected
#Asymmetric: Pair of nodes with a directed edge in one direction or the other
#Mutual/Symmetric: Pair of nodes with directed edges to each other 

#Number of Symmetric Dyads
mutuality(AHS_Network)

#Edgewise Ratio
 #Ratio of Reciprocated Edges to All Edges
grecip(AHS_Network, measure="edgewise")

#So we have 455 reciprocated ties and about
 #43% of all ties are reciprocal



###Calculate Triad Census
triad.census(AHS_Network)

#As is typical in social networks, empty ('vacuous')
 #triads are common, transitive triads more common
 #(in line with CUG transitivity comparison) and
 #some patterns, like cycles (030C), are rare.

  #Reminder of what these types mean:
  #Triad types (per Davis & Leinhardt):
  #003  A, B, C, empty triad.
  #012  A->B, C 
  #102  A<->B, C  
  #021D A<-B->C 
  #021U A->B<-C 
  #021C A->B->C
  #111D A<->B<-C
  #111U A<->B->C
  #030T A->B<-C, A->C
  #030C A->B->C, C->A.
  #201  A<->B<->C.
  #120D A<-B->C, A<->C.
  #120U A->B<-C, A<->C.
  #120C A->B->C, A<->C.
  #210  A->B<->C, A<->C.
  #300  A<->B<->C, A<->C, completely connected.


###Components
 #Components are maximally connected subgraphs (W&F p. 109). 
 #Strong components: connected through directed paths (i --> j --> k)
 #Weak components: connected through semi-paths (i <-- j --> k)

#Calculate strong component
 #(Note: we did this before, to use when we were
  #calculating Bonacich and closeness centrality)
components(AHS_Network, connected="strong")

#Calculate weak component
components(AHS_Network, connected="weak")

#Examining the strong component
AHS_Comp <- component.dist(AHS_Network, connected="strong")

#See the component each node belongs to
AHS_Comp$membership 

#See the size of each component
AHS_Comp$csize     
hist(AHS_Comp$csize, xlab="Component Sizes", prob=TRUE) 



###Bi-Components
 #Bi-Components are subgraphs that require at least the removal 
   #of two nodes or two edges to disconnect the set. 
 #In large highly connected networks, bi-components are often
   #more informative than components
AHS_Bicomp <- bicomponent.dist(AHS_Network) 

#See size of bicomponents
AHS_Bicomp$csize
hist(AHS_Bicomp$csize, xlab="Bi-Component Sizes", prob=TRUE) 



###Cut-Points
 #Cut-points are nodes (or sometimes edges) whose removal
  #break components into subgraphs

#Calculate cut-points
cutpoints(AHS_Network, connected="strong")

#Graph the cut-point nodes
set.seed(12345)
gplot(AHS_Network,
      vertex.col=5+cutpoints(AHS_Network,
                             mode="graph",
                             return.indicator=T))

#Note: Used '5' to get these colors, can use a different
 #number, or set colors specifically with names, rgb #, etc.



###K-Cores
 #A k-core is a subgraph in which each node is connect to
   #at least k other nodes in the subgrap (W&F, p. 266). 

#Calculate k-cores
kcores(AHS_Network) 

#Visualize k-cores
AHS_kc<-kcores(AHS_Network, cmode="indegree")
set.seed(12345)
gplot(AHS_Network,
      vertex.col=rainbow(max(AHS_kc)+1)
      [AHS_kc+1])

#Show only members of 3- and 4-core
gplot(AHS_Network[AHS_kc>2,AHS_kc>2],
      vertex.col=rainbow(max(AHS_kc)+1)[AHS_kc[AHS_kc>2]+1])

#see 4-core nested within 3-core

#Show just the 4-core
gplot(AHS_Network[AHS_kc>3,AHS_kc>3],
      vertex.col=rainbow(max(AHS_kc)+1)[AHS_kc[AHS_kc>3]+1],
      displaylabels = TRUE)



###Cliques
 #A clique is a subset of nodes that are all 
  #connected to each other (Luce and Perry 1949)
 
#Find cliques in the network
 #This code tells R to count up all the cliques and 
  #save it into an object. We tell R it's directed
  #data, suppress the long output listing each
  #node's clique and listing out each clique (these are 
  #the two 'False' statements), and sum up co-membership
  #for each vertex.
AHS_Cliques <- clique.census(AHS_Network, mode = "digraph",
                             tabulate.by.vertex = FALSE,
                             enumerate = FALSE,
                             clique.comembership="sum")

#Count cliques of each membership size
AHS_Cliques$clique.count

#see many small 'cliques'; only 12 cliques with 4 nodes
 #and 1 clique with 5 nodes



############################
#  Structural Equivalence  #
############################

#Distance Measures
  #There are several different Similarity/Distance measures 
  #addressing equivalence concepts to explore further:
     #Correlation
     #Euclidean Distance
     #Hamming Distance
     #Gamma Correlation

#Calculate Structural Equivalence of Vertices using
 #Hamming Distance. This makes a matrix of 
 #similarity/difference scores for each pair of nodes
 #that can be used in further analysis.
Hamdist <- sedist(AHS_Network, 
                  mode="digraph", 
                  method="hamming")

#Cluster based on structural equivalence:
AHS_Clustering <- equiv.clust(AHS_Network, 
                              mode="digraph",
                              plabels=network.vertex.names(AHS_Network))
AHS_Clustering                        

#Plot as a dendrogram
plot(AHS_Clustering)                  

#Make a Block Model based on the Structural Equivalence Clustering
AHS_Block <- blockmodel(AHS_Network, AHS_Clustering, h=30)
AHS_Block

#Extract the block image for Visualization and allow self-loops:
bimage <- AHS_Block$block.model
bimage
bimage[is.nan(bimage)] <- 1

#Visualizing the block image (with self-reflexive ties)
gplot(bimage, 
      diag=TRUE, 
      edge.lwd=bimage*5, 
      vertex.cex=sqrt(table(AHS_Block$block.membership))/2,
      gmode="graph", 
      vertex.sides=50, 
      vertex.col=gray(1-diag(bimage)))


#############################################
#  Two-Mode/Bipartite Network Descriptives  #
#############################################

#Using classic dataset, Davis' Southern Women:
 #observed women attending meetings, parties, etc.
 #so have 18 women at 14 events

#I find igraph much easier for bipartite data than
 #statnet (more pre-made measures, less matrix algebra)
 #so I'm going to detach statnet and load igraph, 
 #because they tend to conflict if both loaded
 #you can also 'mask' one if you don't want to detach

#Detach statnet
detach("package:statnet", unload=TRUE)

#Load igraph
library(igraph)

###Import and Explore 2-Mode Dataset:

#If you haven't yet in this R session,
 #set your working directory:
#setwd("C:/Users/mc418/Dropbox/Duke/SNH2019")

#Read in data
 #Remember, this only works if you have the 'davis.txt'
  #file in your set working directory
davis<-read.csv("davis.txt",header=T,row.names=1)

 #Or choose it from the pop-up window:
   # davis=read.csv(file.choose(),header=T, row.names=1)

#Look at the data
davis


###Make it a bipartite network object
davisnet <- graph.incidence(davis)
davisnet
 #igraph tells us it's UN-B 32 89
  #meaning: Undirected, Named, Bipartite network
  #with 32 nodes and 89 edges
  #(note that 32 nodes includes women and events)
  #igraph finds 2 attributes of the graph: type and name

#Type shows the level of the 2-mode network for each node
V(davisnet)$type

#Name shows each node's name
V(davisnet)$name

#Quick and simple plot
plot(davisnet,
     vertex.color=V(davisnet)$type)
#not pretty, but looks like network data are clean

#To make a better plot:
#Set color and shape of vertices based on type (event or woman)
 #We can use the 'ifelse' function with the network because it already
  #has true/false for the type as a network attribute
V(davisnet)$color <- ifelse(V(davisnet)$type, "gold", "lightblue1")
V(davisnet)$shape <- ifelse(V(davisnet)$type, "square", "circle")

#Re-plot after setting shape and color
 #adjust labels, edge color, and layout here, too
plot(davisnet,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     edge.color="gray70", 
     layout=layout.fruchterman.reingold)

#Bipartite plot
 #Separates types of nodes, but often hard to read
plot(davisnet,
     vertex.size=12, 
     vertex.label.cex=0.6,
     layout=layout.bipartite)



###Making one-mode projections from bipartite data
 #This will make 2 projections:
 #1 network of women where nodes are women and
  #the edges represent co-attendance at an event
 #1 network of events where nodes are events and
  #the edges represent the number of women who attended both
davisproj <- bipartite.projection(davisnet)

#Look at projections
davisproj

#See we have 2 separate networks of women and events
#Both networks are now weighted by the information
 #from the bipartite data (i.e. edges are weighted by
 #the number of events or women shared between nodes)

#Quick Plot of Events Projection
 #We tell igraph we want the 2nd projection,
  #(events, not women), and to make the edges wider
  #based on the weight attribute in that projection.
  #We set the labels to match the names from the 2nd
  #projection and set colors, shapes, and titles to taste.
plot(davisproj$proj2,
     edge.width=E(davisproj$proj2)$weight,
     vertex.label=V(davisproj$proj2)$name,
     vertex.color="gold",
     vertex.shape = "square",
     layout=layout.fruchterman.reingold,
     main="Jointly Attended Events")


#This graph shows events connected by women who 
 #attended both (weight of edge = # of co-attenders)
#For example, see that many more women attended both
 #E8 and E9 than E7 and E2

#If your research question is about the events,
 #you can use any of the descriptives used above
 #(or your other network skills) on this projection,
 #provided you are conscientious about the inherent 
 #properties of projection (as discussed in lecture).


###Plot the women's co-attendance network

#First, make standardized positions for vertices:
 #This is an alternative to the 'set.seed' method
 #for keeping your graphs oriented in the same way
 #that might be useful if you want a specific type
 #of layout for your graph in igraph. This can also
 #be used to index certain nodes and set their location,
 #say if you need to highlight certain actors.
set.seed(12345)
coords <- layout_with_fr(davisproj$proj1)

#Plot
plot(davisproj$proj1,
     edge.width=E(davisproj$proj1)$weight,
     vertex.label=V(davisproj$proj1)$name,
     vertex.label.cex = .5,
     vertex.color="lightblue1",
     vertex.size = 20,  
     layout=coords,
     main="Co-Attendance of Women")

#See quickly from edge widths, some women attended 
 #many of the same events (Ex: Theresa and Evelyn)
 #and some did not often attend the same events
 #(Ex: Flora and Helen).
#We can also start to see something like a core-
 #periphery structure of a central core of women
 #attending the same events, with others at the fringe.

#If your research question is about who co-attends the
 #same events (or co-authors, is co-present, or any
 #other connection between actors in the bipartite net),
 #you can further describe and analyze this projection,
 #keeping in mind that it is a project (which changes
 #the interpretation of edges and some of the 
 #expectations for properties and measures).



###Centrality measures in one-mode projection
 #Remember, it is an undirected network,
  #because co-attendance is symmetric.

#Calculate Degree
deg <- degree(davisproj$proj1)
deg

#Calculate Betweenness
bet <- betweenness(davisproj$proj1)

#Caclculate Eigenvector Centrality
 #Tell igraph you only want the vector score
  #or else it will save a lot more info that
  #we don't need and can make the score harder
  #to handle because it isn't one score per node.
eigc <- eigen_centrality(davisproj$proj1)$vector

#Table of centralities
coatt_cent <- data.frame(deg, bet, eigc)
coatt_cent
#See the higher, 'inflated' centrality compared to the AHS
 #network above, because it is the 1-mode projection of
 #a bipartite network.



###Plot Centrality of Nodes
 #As earlier in this lab, can plot network with
 #nodes' centrality information included:

#Plot Eigenvector centrality as vertex size
 #(scaling eigenvector x20 to make it visible
 # because it is a small value)
plot(davisproj$proj1,
     edge.width=E(davisproj$proj1)$weight,
     vertex.label=V(davisproj$proj1)$name,
     vertex.label.cex = .5,
     vertex.color="lightblue1",
     layout=coords,
     vertex.size=eigc*20,
     main="Women - Eigenvector Centrality")

#See variaton in Eigenvector centrality (in line with 
 #our earlier observation of core/periphery).
 #For example, Theresa is connected to other highly 
 #connected nodes (high EC.) while Flora is not (low EC.).
 #Katherine and Dorothy have the same degree, but Katherine
 #has higher EC because she is connected to others with 
 #higher degree (same for Brenda and Frances).

#Can thoughtfully do whatever network descriptives you do
 #with one-mode data with either one-mode projection,
 #though some network scientists recommend specific R packages
 #that account for inflation inherent in projection.
 #These packages often have their own data structures and
 #conventions (can take a long time to run):
 #
 #Some popular general options include:
 #Tore Opsahl's 'tnet': https://toreopsahl.com/tnet/software/installing/
 #Dormann et al.'s 'bipartite': https://cran.r-project.org/web/packages/bipartite/bipartite.pdf
 #
 #Other options are more specific (Ex: ergm has specific commands for
 #2-mode ERGM's,there are other specialized 2-mode community 
 #detection packages, etc.).

