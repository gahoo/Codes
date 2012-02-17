#http://www.r-bloggers.com/grey%e2%80%99s-anatomy-network-of-sexual-relations/
library(igraph)
ga.data <- read.csv('http://www.babelgraph.org/data/ga_edgelist.csv', header=TRUE)
g <- graph.data.frame(ga.data, directed=FALSE)
summary(g)
g$layout <- layout.fruchterman.reingold(g)
plot(g)

V(g)$label <- NA # remove labels for now
V(g)$size <- degree(g) * 2 # multiply by 2 for scale
plot(g)

clo <- closeness(g)
# rescale values to match the elements of a color vector
clo.score <- round( (clo - min(clo)) * length(clo) / max(clo) ) + 1
# create color vector, use rev to make red "hot"
clo.colors <- rev(heat.colors(max(clo.score)))
V(g)$color <- clo.colors[ clo.score ]
plot(g)

btw <- betweenness(g)
btw.score <- round(btw) + 1
btw.colors <- rev(heat.colors(max(btw.score)))
V(g)$color <- btw.colors[ btw.score ]
plot(g)

gnc <- edge.betweenness.community(g, directed=FALSE)
m <- vector()
for (s in 0:nrow(gnc$merges) ) {
        memb <- community.to.membership(g,gnc$merge,steps=s)$membership
  m <- c(m,modularity (g, memb, weights=NULL))
}
ideal_steps <- which(m==max(m)) - 1
plot(0:(length(m)-1),m, col="blue",xlab="Steps",ylab="Modularity")
gn.groups <- community.to.membership(g,gnc$merge, steps=ideal_steps)$membership
V(g)$color <- gn.groups
V(g)$size <- 15 # reset to default size
plot(g)

V(g)$color <- 'grey'
V(g)$label <- V(g)$name
V(g)$label.cex <- 0.7 # rescale the text size of the label
plot(g)