#Load the package SNA (Social Network Analysis Library)
#Load the package igraph (Network analysis and visualization)
library(sna)
library(igraph)


#########################################################################

# define function to plot a graph
plotG <- function(g) {
  plot(g, 
       # force-directed layout
       layout=layout.fruchterman.reingold,
       #vertex.label=NA,
       vertex.label.font=2, vertex.size=5, 
       vertex.color="blue",
       vertex.frame.color=FALSE, 
       edge.color="black")
}

# Make up data
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David", "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        weight=c(4,5,5,2,1,1))
# Alternatively, you could read in the data from a similar CSV file as follows:
SampleGraph <- read.csv("A_Sample_CSV_Graph.csv")

# Load (DIRECTED) graph from data frame 
g <- graph.data.frame(relations, directed=TRUE)
g1 <- graph.data.frame(SampleGraph, directed=TRUE)

# Plot graph

plot(g, edge.width=E(g)$weight)
plotG(g)

plot(g1, edge.width=E(g)$weight)
plotG(g1)


g <- graph.ring(10)
plotG(g)
degree(g)
degree(g)[1]


#Generate random graphs according to the Erdos-Renyi model
#This model is very simple, every possible edge is created with the same constant probability.
g1 <- erdos.renyi.game(10, 1/10)
#n The number of vertices in the graph.
#p.or.m Either the probability for drawing an edge between two arbitrary vertices (G(n,p)graph),
#or the number of edges in the graph (for G(n,m) graphs).
#type=c("gnp", "gnm"),
#directed = FALSE, loops = FALSE,
#directed Logical, whether the graph will be directed, defaults to FALSE.
#loops Logical, whether to add loop edges, defaults to FALSE.
plotG(g1)
degree(g1)
degree(g1)[30]


g2 <- erdos.renyi.game(50, 1/10, type=c("gnp"),directed = FALSE,loops = FALSE)
plotG(g2)
degree(g2)
degree.distribution(g2)


g3 <- erdos.renyi.game(50, 50, type=c("gnm"))
plotG(g3)

g4 <- barabasi.game(10)
plotG(g4)
degree.distribution(g4)


#Prameters required for the graph
#N(number of vertices in the graph)
#plink (probability of a link between any 2 vertices)
N=10
plink = 0.2
#sna::rgraph() -- Generate Bernoulli Random Graphs
#2nd argument (1) for one graph is to generated
#4th argument ("graph") for the graph to be undirected
#5th argument(FALSE) for the possibility of loops
graph=rgraph(N,1, plink, "graph", FALSE)
#generated graph in a matrix format
#graph


#degree for each vertex, For undirected graphs, total degree = in-degree + out-degree
degree(graph)

#Total degree
sum(degree(graph))

#gden is used to find the density of graph
gden(graph, mode="graph")

#when connectedness is 1, then the graph given is fully connected
#a connectedness of 1 also gives is.connected value to be TRUE
#a connceredness of o indicate completely isolated vertices
#is.connected is used for strict connectedness
is.connected(graph)
connectedness(graph)

#Similarly is.isolate() is used to check if a given node is isolated in the graph given.
is.isolate(graph,1)
is.isolate(graph,10)

g1 <- graph.adjacency(graph)
betweenness(g1)

#shortest paths (geodesics) 
geo=geodist(graph)
geo$dist

#ego.extract takes one or more input graphs and returns a list containing the egocentric 
#networks centered on vertices named in ego, using adjacency rule neighborhood to define inclusion
ego.extract(graph, 6)

#closeness centrality measures how many steps are required to access every other vertex from a given vertex\
closeness(g1)



##########################################################################################################################
# PAGE RANK
##########################################################################################################################
#How does Google rank web pages in order to provide meaningful search results?
#The 'igraph' package contains the function 'page.rank' that is capable of taking a graph object as an input and computing the PageRank of the vertices in the graph object.
#The above function call creates a directed random graph with 20 vertices.
#This is stored on the graph object 'g' with an edge between two vertices occurring with probability of 5/20.
g<-random.graph.game(20, 5/20, directed=TRUE)
plot(g)
page.rank(g)$vector


#The  'graph.star' function creates a star graph 'g2'. 
#In this every single vertex is connected to  only the center vertex. 
#This is used to depict the vertex that has the highest PageRank in our simulation.

g2<-graph.star(10)
page.rank(g2)$vector
plot(g)
plot(g2)


g2<-graph.star(10, mode = c("undirected"))
plot(g2)



# Personalized PageRank
g3 <- graph.ring(10)
page.rank(g3)$vector
reset <- seq(vcount(g3))
page.rank(g3, personalized=reset)$vector


##########################################################################################################################
#Kleinberg's hub and authority scores.
##########################################################################################################################

g <- graph.star(10)
plotG(g)
hub.score(g)$vector
authority.score(g)$vector
## A ring
g2 <- graph.ring(10)
plotG(g2)
hub.score(g2)$vector
authority.score(g2)$vector

