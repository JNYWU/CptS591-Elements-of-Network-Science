library(igraph)

graphPolBlogs <- read.graph("polblogs/polblogs.gml", "gml")
graphNeural <- read.graph("celegansneural/celegansneural.gml", "gml")
graphInternet <- read.graph("as-22july06/as-22july06.gml", "gml")

erg1 <- erdos.renyi.game(2000, .01)
erg2 <- erdos.renyi.game(2000, .005)
erg3 <- erdos.renyi.game(2000, .0025)

is.directed(graphPolBlogs)
is.directed(graphNeural)
is.directed(graphInternet)
is.directed(erg1)
is.directed(erg2)
is.directed(erg3)

V(graphPolBlogs)
V(graphNeural)
V(graphInternet)
V(erg1)
V(erg2)
V(erg3)

gsize(graphPolBlogs)
gsize(graphNeural)
gsize(graphInternet)
gsize(erg1)
gsize(erg2)
gsize(erg3)

components(graphPolBlogs, 'strong')$no
components(graphPolBlogs, 'weak')$no
components(graphNeural, 'strong')$no
components(graphNeural, 'weak')$no

components(graphInternet)$no
components(erg1)$no
components(erg2)$no
components(erg3)$no

max(degree(graphPolBlogs))
max(degree(graphNeural))
max(degree(graphInternet))
max(degree(erg1))
max(degree(erg2))
max(degree(erg3))

average.path.length(graphPolBlogs)
average.path.length(graphNeural)
average.path.length(graphInternet)
average.path.length(erg1)
average.path.length(erg2)
average.path.length(erg3)

diameter(graphPolBlogs)
diameter(graphNeural)
diameter(graphInternet)
diameter(erg1)
diameter(erg2)
diameter(erg3)

transitivity(graphPolBlogs, type = "localaverageundirected")
transitivity(graphNeural, type = "localaverageundirected")
transitivity(graphInternet, type = "localaverageundirected")
transitivity(erg1, type = "localaverageundirected")
transitivity(erg2, type = "localaverageundirected")
transitivity(erg3, type = "localaverageundirected")

transitivity(graphPolBlogs, type = "undirected")
transitivity(graphNeural, type = "undirected")
transitivity(graphInternet, type = "undirected")
transitivity(erg1, type = "undirected")
transitivity(erg2, type = "undirected")
transitivity(erg3, type = "undirected")

plot(degree.distribution(graphPolBlogs))
plot(degree.distribution(graphNeural))
plot(degree.distribution(graphInternet))
plot(degree.distribution(erg1))
plot(degree.distribution(erg2))
plot(degree.distribution(erg3))

plPolBlogs <- path.length.hist(graphPolBlogs)$res/sum(path.length.hist(graphPolBlogs)$res)
plNeural <- path.length.hist(graphNeural)$res/sum(path.length.hist(graphNeural)$res)
plInternet <- path.length.hist(graphInternet)$res/sum(path.length.hist(graphInternet)$res)
plerg1 <- path.length.hist(erg1)$res/sum(path.length.hist(erg1)$res)
plerg2 <- path.length.hist(erg2)$res/sum(path.length.hist(erg2)$res)
plerg3 <- path.length.hist(erg3)$res/sum(path.length.hist(erg3)$res)

plot(plPolBlogs)
plot(plNeural)
plot(plInternet)
plot(plerg1)
plot(plerg2)
plot(plerg3)

graphNoun <- read.graph("adjnoun/adjnoun.gml", "gml")
is.directed(graphNoun)
V(graphNoun)
gsize(graphNoun)
components(graphNoun)$no
max(degree(graphNoun))
average.path.length(graphNoun)
diameter(graphNoun)
transitivity(graphNoun, type = "localaverageundirected")
transitivity(graphNoun, type = "undirected")
plot(degree.distribution(graphNoun))

plNoun <- path.length.hist(graphNoun)$res/sum(path.length.hist(graphNoun)$res)
plot(plNoun)






