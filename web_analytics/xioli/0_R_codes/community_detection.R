
#Load the package igraph (Network analysis and visualization)
library(igraph)

#graph.famous {igraph}  R Documentation
#Creating named graphs
#Description
#There are some famous, named graphs, sometimes counterexamples to some conjecture or unique graphs with given features. These can be created with this function

#Zachary
#Social network of friendships between 34 members of a karate club 
#at a US university in the 1970s. 
#See W. W. Zachary, An information flow model for conflict and fission in small groups, 
#Journal of Anthropological Research 33, 452-473 (1977).
g<- graph.famous("Zachary")
plot(g)

g<- graph.famous("Chvatal")
plot(g)

g<- graph.famous("Coxeter")
plot(g)

g<- graph.famous("Diamond")
plot(g)

g<- graph.famous("Krackhardt\_Kite")
plot(g)

g<- graph.famous("Meredith")
plot(g)


###########################################
#community Detection
##########################################

##########################################
# Clique detection
##########################################
# WE EXPLAIN TWO EXAMPLES

#################
#    EXAMPLE 1  #
#################
# this usually contains cliques of size six
g <- erdos.renyi.game(100, 0.3)
plot(g)
# The functions find cliques, ie. complete subgraphs in a graph
# clique.number calculates the size of the largest clique(s).
clique.number(g)

#cliques find all complete subgraphs in the input graph, 
#obeying the size limitations given in the min and max arguments.
cliques(g, min=6)

# How about we want to find smaller cliques
cliques(g, min=4)

#maximal.cliques finds all maximal cliques in the input graph.
#A clique in maximal if it cannot be extended to a larger clique. 
#The largest cliques (maximum cliques) are always maximal, but a maximal clique is not neccessarily the largest.
maximal.cliques(g)

# Maximum cliques: largest.cliques finds all largest cliques in the input 
# graph. A clique is largest if there is no other clique including more vertices.
largest.cliques(g)


#################
#    EXAMPLE 2  #
#################
# To have a bit less maximal cliques, about 100-200 usually
g <- erdos.renyi.game(100, 0.1)
plot(g)

#maximal.cliques finds all maximal cliques in the input graph.
#A clique in maximal if it cannot be extended to a larger clique. 
#The largest cliques (maximum cliques) are always maximal, but a maximal clique is not neccessarily the largest.
maximal.cliques(g)


# Maximum cliques: largest.cliques finds all largest cliques in the input 
# graph. A clique is largest if there is no other clique including more vertices.
largest.cliques(g)


####################################################
# walktrap method
#####################################################
karate <- graph.famous("Zachary")
#walktrap.community
#This function is the implementation of theWalktrap community finding algorithm, 
#see Pascal Pons,
#Matthieu Latapy: Computing communities in large networks using random walks, 
#http://arxiv.org/abs/physics/0512106

wc <- walktrap.community(karate)

#modularity calculates the modularity of a graph with respect to the given membership vector.
#Clauset, A.; Newman, M. E. J. & Moore, C. Finding community structure in very large networks,
#Phyisical Review E 2004, 70, 066111
modularity(wc)

membership(wc)
plot(wc, karate)



############################################################
# edge.betweenness.community
# Community structure detection based on edge betweenness
###########################################################
# Many networks consist of modules which are densely connected themselves 
#but sparsely connected to other modules.
#M Newman and M Girvan: Finding and evaluating community structure in networks, Physical
#Review E 69, 026113 (2004)
g <- barabasi.game(100,m=2) 
plot(g)
eb <- edge.betweenness.community(g)
eb


g <- graph.full(10) %du% graph.full(10)
plot(g)
g <- add.edges(g, c(1,11))
plot(g)
eb <- edge.betweenness.community(g)
eb


g <- add.edges(g, c(2,19))
plot(g)
eb <- edge.betweenness.community(g)
eb


g <- add.edges(g, c(5,20))
g <- add.edges(g, c(8,16))
plot(g)
eb <- edge.betweenness.community(g)
eb


############################################################
# fastgreedy.community 
# Community structure via greedy optimization of modularity
###########################################################
g <- graph.full(5) %du% graph.full(5) %du% graph.full(5)
plot(g)
g <- add.edges(g, c(1,6, 1,11, 6, 11))
plot(g)
fc <- fastgreedy.community(g)
membership(fc)
sizes(fc)

karate <- graph.famous("Zachary")
plot(karate)
fc <- fastgreedy.community(karate)
fc
dendPlot(fc)

##########################################################
# leading.eigenvector.community
# Community structure detecting based on the leading eigenvector of the
# community matrix
##########################################################
g <- graph.full(5) %du% graph.full(5) %du% graph.full(5)
plot(g)
lec <- leading.eigenvector.community(g)
lec
leading.eigenvector.community(g, start=membership(lec))


g <- add.edges(g, c(1,6))
plot(g)
lec <- leading.eigenvector.community(g)
lec
leading.eigenvector.community(g, start=membership(lec))


g <- add.edges(g, c(1,11))
plot(g)
lec <- leading.eigenvector.community(g)
lec
leading.eigenvector.community(g, start=membership(lec))


g <- add.edges(g, c(6,11))
plot(g)
lec <- leading.eigenvector.community(g)
lec
leading.eigenvector.community(g, start=membership(lec))


############################################################
# spinglass.community 
# Finding communities in graphs based on statistical meachanics
# This function tries to find communities in graphs via a 
# spin-glass model and simulated annealing.
############################################################
# J. Reichardt and S. Bornholdt: Statistical Mechanics of Community Detection, Phys. Rev. E, 74,
# 016110 (2006), http://arxiv.org/abs/cond-mat/0603718
# M. E. J. Newman and M. Girvan: Finding and evaluating community structure in networks, Phys.
# Rev. E 69, 026113 (2004)
# V.A. Traag and Jeroen Bruggeman: Community detection in networks with positive and negative
# links, http://arxiv.org/abs/0811.2329 (2008).
g <- erdos.renyi.game(10, 5/10) %du% erdos.renyi.game(9, 5/9)
plot(g)
g <- add.edges(g, c(1, 12))
plot(g)
#subgraph creates a subgraph of a graph, 
#containing only the specified vertices and all the edges among them.
g <- induced.subgraph(g, subcomponent(g, 1))
plot(g)
spinglass.community(g, spins=2)
spinglass.community(g, vertex=1)



#infomap Method
#########################################
## Zachary's karate club
g <- graph.famous("Zachary")
plot(g)
imc <- infomap.community(g)
membership(imc)
communities(imc)


#########################################
# http://www.sixhat.net/finding-communities-in-networks-with-r-and-igraph.html
########################################

# let's generate two networks and merge them into one graph.
g2 <- barabasi.game(50, p=2, directed=F)
plot(g2)
g1 <- watts.strogatz.game(1, size=100, nei=5, p=0.05)
plot(g1)
g <- graph.union(g1,g2)
plot(g)

# let's remove multi-edges and loops
g <- simplify(g)

# let's see if we have communities here using the 
# Grivan-Newman algorithm
# 1st we calculate the edge betweenness, merges, etc...
ebc <- edge.betweenness.community(g, directed=F)
ebc

# Now we have the merges/splits and we need to calculate the modularity
# for each merge for this we'll use a function that for each edge
# removed will create a second graph, check for its membership and use
# that membership to calculate the modularity
mods <- sapply(0:ecount(g), function(i){
  g2 <- delete.edges(g, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  # March 13, 2014 - compute modularity on the original graph g 
  # (Thank you to Augustin Luna for detecting this typo) and not on the induced one g2. 
  modularity(g,cl)
})

# we can now plot all modularities
plot(mods, pch=20)

# Now, let's color the nodes according to their membership
g2<-delete.edges(g, ebc$removed.edges[seq(length=which.max(mods)-1)])
V(g)$color=clusters(g2)$membership

# Let's choose a layout for the graph
g$layout <- layout.fruchterman.reingold

# plot it
plot(g, vertex.label=NA)

# if we wanted to use the fastgreedy.community agorithm we would do
fc <- fastgreedy.community(g)
com<-community.to.membership(g, fc$merges, steps= which.max(fc$modularity)-1)
V(g)$color <- com$membership+1
g$layout <- layout.fruchterman.reingold
plot(g, vertex.label=NA)


###############################################################
# The following URL could be useful for community detection 
###############################################################
#http://igraph.wikidot.com/community-detection-in-r
#########################################
#https://sites.google.com/site/andrewjedelman/statistical-tools/network-analysis/community-detection
###########################################

#########################################
#http://smallstats.blogspot.sg/2012/12/community-detection-algorithm-with.html

