library(igraph)
library(ggplot2)

# read graph
graphPolBlogs <- read.graph("polblogs/polblogs.gml", "gml")
graphNeural <- read.graph("celegansneural/celegansneural.gml", "gml")
graphInternet <- read.graph("as-22july06/as-22july06.gml", "gml")
graphDolphins <- read.graph("dolphins/dolphins.gml", "gml")

# Problem 1
# degree
V(graphPolBlogs)$label[degree(graphPolBlogs, mode="in")==max(degree(graphPolBlogs, mode="in"))]
V(graphNeural)$label[degree(graphNeural, mode="in")==max(degree(graphNeural, mode="in"))]
V(graphInternet)$label[degree(graphInternet, mode="in")==max(degree(graphInternet, mode="in"))]
V(graphDolphins)$label[degree(graphDolphins, mode="in")==max(degree(graphDolphins, mode="in"))]

# eccentricity
V(graphPolBlogs)$label[eccentricity(graphPolBlogs, vids = V(graphPolBlogs), mode = c("all"))
                       ==min(eccentricity(graphPolBlogs, vids = V(graphPolBlogs), mode = c("all")))]
V(graphNeural)$label[eccentricity(graphNeural, vids = V(graphNeural), mode = c("all"))
                     ==min(eccentricity(graphNeural, vids = V(graphNeural), mode = c("all")))]
V(graphInternet)$label[eccentricity(graphInternet, vids = V(graphInternet), mode = c("all"))
                       ==min(eccentricity(graphInternet, vids = V(graphInternet), mode = c("all")))]
V(graphDolphins)$label[eccentricity(graphDolphins, vids = V(graphDolphins), mode = c("all"))
                       ==min(eccentricity(graphDolphins, vids = V(graphDolphins), mode = c("all")))]

# closeness
V(graphPolBlogs)$label[closeness(graphPolBlogs, vids = V(graphPolBlogs), mode = c("all"))
                       ==min(closeness(graphPolBlogs, vids = V(graphPolBlogs), mode = c("all")))]
V(graphNeural)$label[closeness(graphNeural, vids = V(graphNeural), mode = c("all"))
                     ==min(closeness(graphNeural, vids = V(graphNeural), mode = c("all")))]
V(graphInternet)$label[closeness(graphInternet, vids = V(graphInternet), mode = c("all"))
                       ==min(closeness(graphInternet, vids = V(graphInternet), mode = c("all")))]
V(graphDolphins)$label[closeness(graphDolphins, vids = V(graphDolphins), mode = c("all"))
                       ==min(closeness(graphDolphins, vids = V(graphDolphins), mode = c("all")))]

#betweeness
V(graphPolBlogs)$label[betweenness(graphPolBlogs, v = V(graphPolBlogs))
                       ==min(betweenness(graphPolBlogs, v = V(graphPolBlogs)))]
V(graphNeural)$label[betweenness(graphNeural, v = V(graphNeural))
                     ==min(betweenness(graphNeural, v = V(graphNeural)))]
V(graphInternet)$label[betweenness(graphInternet, v = V(graphInternet))
                       ==min(betweenness(graphInternet, v = V(graphInternet)))]
V(graphDolphins)$label[betweenness(graphDolphins, v = V(graphDolphins))
                       ==min(betweenness(graphDolphins, v = V(graphDolphins)))]

# katz index
V(graphPolBlogs)$label[eigen_centrality(graphPolBlogs)$vector
                       ==max(eigen_centrality(graphPolBlogs)$vector)]
V(graphNeural)$label[eigen_centrality(graphNeural)$vector
                     ==max(eigen_centrality(graphNeural)$vector)]
V(graphInternet)$label[eigen_centrality(graphInternet)$vector
                       ==max(eigen_centrality(graphInternet)$vector)]
V(graphDolphins)$label[eigen_centrality(graphDolphins)$vector
                       ==max(eigen_centrality(graphDolphins)$vector)]

# PageRank
V(graphPolBlogs)$label[page_rank(graphPolBlogs, vids = V(graphPolBlogs))$vector
                       ==max(page_rank(graphPolBlogs, vids = V(graphPolBlogs))$vector)]
V(graphNeural)$label[page_rank(graphNeural, vids = V(graphNeural))$vector
                     ==max(page_rank(graphNeural, vids = V(graphNeural))$vector)]
V(graphInternet)$label[page_rank(graphInternet, vids = V(graphInternet))$vector
                       ==max(page_rank(graphInternet, vids = V(graphInternet))$vector)]
V(graphDolphins)$label[page_rank(graphDolphins, vids = V(graphDolphins))$vector
                       ==max(page_rank(graphDolphins, vids = V(graphDolphins))$vector)]

# Kleinberg's Authority
V(graphPolBlogs)$label[authority_score(graphPolBlogs)$vector
                       ==max(authority_score(graphPolBlogs)$vector)]
V(graphNeural)$label[authority_score(graphNeural)$vector
                     ==max(authority_score(graphNeural)$vector)]
V(graphInternet)$label[authority_score(graphInternet)$vector
                       ==max(authority_score(graphInternet)$vector)]
V(graphDolphins)$label[authority_score(graphDolphins)$vector
                       ==max(authority_score(graphDolphins)$vector)]

# Kleinberg's Hub
V(graphPolBlogs)$label[hub_score(graphPolBlogs)$vector
                       ==max(hub_score(graphPolBlogs)$vector)]
V(graphNeural)$label[hub_score(graphNeural)$vector
                     ==max(hub_score(graphNeural)$vector)]
V(graphInternet)$label[hub_score(graphInternet)$vector
                       ==max(hub_score(graphInternet)$vector)]
V(graphDolphins)$label[hub_score(graphDolphins)$vector
                       ==max(hub_score(graphDolphins)$vector)]

# Problem 2
# generating 2 graphs with 20 nodes
erg1 <- erdos.renyi.game(20, .1)
E(erg1)
bag1 <- barabasi.game(20, power = 1)
E(bag1)

# generating 2 graphs with 40 nodes
erg2 <- erdos.renyi.game(40, .05)
E(erg2)
bag2 <- barabasi.game(40, power = 1)
E(bag2)

# number of components
components(erg1)$no
components(bag1)$no
components(erg2)$membership
components(bag2)$no

# get the largest connected component for the graphs with more than one component
derg1 <- decompose.graph(erg1)
gccerg1 <- derg1[[1]]
derg2 <- decompose.graph(erg2)
gccerg2 <- derg2[[2]]

# n, number of nodes
V(gccerg1)
V(bag1)
V(gccerg2)
V(bag2)

# m, number of edges
E(gccerg1)
E(bag1)
E(gccerg2)
E(bag2)

# dmin, min degree
min(degree(gccerg1))
min(degree(bag1))
min(degree(gccerg2))
min(degree(bag2))

# dmax, max degree
max(degree(gccerg1))
max(degree(bag1))
max(degree(gccerg2))
max(degree(bag2))

# l, average path length
average.path.length(gccerg1)
average.path.length(bag1)
average.path.length(gccerg2)
average.path.length(bag2)

# D, diameter
diameter(gccerg1)
diameter(bag1)
diameter(gccerg2)
diameter(bag2)

# ccg, global clustering coefficient
transitivity(gccerg1, type = "undirected")
transitivity(bag1, type = "undirected")
transitivity(gccerg2, type = "undirected")
transitivity(bag2, type = "undirected")

# Laplacian
lgccerg1 <- laplacian_matrix(gccerg1)
lbag1 <- laplacian_matrix(bag1)

lgccerg2 <- laplacian_matrix(gccerg2)
lbag2 <- laplacian_matrix(bag2)

# lambda2, algebraic connectivity
ac_gccerg1 <- sort(eigen(lgccerg1)$value)
ac_gccerg1[2]
ac_bag1 <- sort(eigen(lbag1)$value)
ac_bag1[2]
ac_gccerg2 <- sort(eigen(lgccerg2)$value)
ac_gccerg2[2]
ac_bag2 <- sort(eigen(lbag2)$value)
ac_bag2[2]

# lambdan largest eigenvalue
max(eigen(lgccerg1)$value)
max(eigen(lbag1)$value)
max(eigen(lgccerg2)$value)
max(eigen(lbag2)$value)

eigen(lbag1)$vector[2,]

# plot
plot(V(bag1), eigen(lbag1)$vector[4,], col='red')
plot(V(bag1), eigen(lbag1)$vecor[5,], col='blue')







