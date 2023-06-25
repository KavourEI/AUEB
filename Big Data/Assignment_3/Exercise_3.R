##### Assignement 3 #####

#### Exercise 3 ####

library(igraph)
library(network)
library(intergraph)
library(ergm)
library(latentnet)

m1 <- graph( ~'Themis'-'ChristosR'-'Themis'-'Thodoris'-'Themis'-'Thrasivoulos'-'Themis'-'Zisis'-'Themis'-'NikosD'-'Themis'-'DimitrisK'-'Themis'-'Athanasia'-'Themis'-'Nikos'-'Themis'-'Theodosia'-'Themis'-'Elina'-'Themis'-'NikosDaoul'-'Themis'-'Tasos'-'ChristosR'-'Thodoris'-'ChristosR'-'Thrasivoulos'-'ChristosR'-'Zisis'-'ChristosR'-'NikosD'-'ChristosR'-'DimitrisK'-'ChristosR'-'Theodosia'-'ChristosR'-'Elina'-'ChristosR'-'NikosDaoul'-'ChristosR'-'Tasos'-'Thodoris'-'Thrasivoulos'-'Thodoris'-'Zisis'-'Thodoris'-'NikosD'-'Thodoris'-'DimitrisK'-'Thodoris'-'Nikos'-'Thodoris'-'Theodosia'-'Thodoris'-'Elina'-'Thodoris'-'NikosDaoul'-'Thodoris'-'Tasos'-'Thrasivoulos'-'Zisis'-'Thrasivoulos'-'DimitrisK'-'Thrasivoulos'-'Nikos'-'Thrasivoulos'-'Theodosia'-'Thrasivoulos'-'Elina'-'Thrasivoulos'-'Tasos'-'Zisis'-'Tasos'-'Zisis'-'Athanasia'-'Zisis'-'Theodosia'-'Zisis'-'NikosDaoul'-'NikosD'-'Theodosia'-'NikosD'-'Elina'-'NikosD'-'Tasos'-'NikosD'-'Tasos'-'DimitrisK'-'Nikos'-'DimitrisK'-'Theodosia'-'DimitrisK'-'Elina'-'DimitrisK'-'Tasos'-'Athanasia'-'Theodosia'-'Nikos'-'Theodosia'-'Nikos'-'Elina'-'Nikos'-'Tasos'-'Theodosia'-'Elina'-'Theodosia'-'NikosDaoul'-'NikosDaoul'-'Tasos',
             'Themis'-'Nektaria'-'Themis'-'Nikos'-'Themis'-'Evangelia'-'Themis'-'Thanasis'-'Themis'-'Kaliopi'-'Themis'-'Machi'-'Themis'-'Theodosia'-'Nektaria'-'Nikos'-'Nektaria'-'Evangelia'-'Nektaria'-'Thanasis'-'Nektaria'-'Kaliopi'-'Nektaria'-'Machi'-'Nektaria'-'Theodosia'-'Nikos'-'Evangelia'-'Nikos'-'Thanasis'-'Nikos'-'Kaliopi'-'Nikos'-'Machi'-'Nikos'-'Theodosia'-'Evangelia'-'Thanasis'-'Evangelia'-'Kaliopi'-'Evangelia'-'Machi'-'Evangelia'-'Theodosia'-'Thanasis'-'Machi'-'Thanasis'-'Theodosia'-'Kaliopi'-'Machi'-'Kaliopi'-'Theodosia'-'Machi'-'Theodosia',
             'Themis'-'Vivian'-'Themis'-'Anna'-'Themis'-'Kostas'-'Themis'-'AkisM'-'Themis'-'AkisK'-'Themis'-'Myrto'-'Themis'-'Theodosia'-'Themis'-'Evangelia'-'Vivian'-'Anna'-'Vivian'-'AkisM'-'Vivian'-'AkisK'-'Vivian'-'Theodosia'-'Anna'-'Kostas'-'Anna'-'AkisK'-'Anna'-'Myrto'-'Anna'-'Theodosia'-'Anna'-'Evangelia'-'Kostas'-'Myrto'-'Kostas'-'Theodosia'-'Kostas'-'Evangelia'-'AkisM'-'AkisK'-'AkisM'-'Theodosia'-'AkisK'-'Myrto'-'AkisK'-'Theodosia'-'AkisK'-'Evangelia'-'Myrto'-'Theodosia'-'Myrto'-'Evangelia'-'Theodosia'-'Evangelia', 
             'Themis'-'Theodosia'-'Themis'-'Stavros'-'Themis'-'Tina'-'Themis'-'Erina'-'Themis'-'Despoina'-'Themis'-'Artemis'-'Themis'-'AnnaMaria'-'Themis'-'Thanos'-'Theodosia'-'Stavros'-'Theodosia'-'Tina'-'Theodosia'-'Erina'-'Theodosia'-'Despoina'-'Theodosia'-'Artemis'-'Theodosia'-'AnnaMaria'-'Theodosia'-'Thanos'-'Stavros'-'Tina'-'Stavros'-'Erina'-'Tina'-'Erina'-'Tina'-'Artemis'-'Tina'-'Thanos'-'Despoina'-'Artemis'-'Despoina'-'AnnaMaria'-'Despoina'-'Thanos'-'Artemis'-'AnnaMaria'-'Artemis'-'Thanos'-'AnnaMaria'-'Thanos'-'Artemis'-'AkisK'-'Artemis'-'Thodoris'-'Despoina'-'Anna'-'Despoina'-'Myrto'-'Despoina'-'Evangelia'-'Despoina'-'Kostas')
plot(m1,edge.arrow.size=2, main = "Social Networks of Themis Kavour")    # Plot the network 

m1[]    # Show the adj.matrix

E(m1)    # Show the edges of the object 

V(m1)    #Show the vertices of the object

V(m1)$name
V(m1)$gender <- c(rep("male",7),"female","male",rep("female",2),rep("male",2),rep("female",2),"male",rep("female",4),rep("male",3),"female","male",rep("female",3),"male","female","male")    #Genders 

# You may erase this part!!
V(m1)$Introduction <-  c(rep("Alexandroupolis",9),"Thessaloniki",rep("Alexandroupolis",3),'Kozani',rep('Thessaloniki',3), "Kozani",rep('Thessaloniki',6),rep("Athens",7))   # Where introduction took place

vertex_attr(m1)

plot(m1, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5, vertex.color=c( "pink", "skyblue")[1+(V(m1)$gender=="male")] )
legend(x=1.5, y=1, c('Female','Male'), pch=21, col="#777777", pt.bg=c( "pink", "skyblue"), pt.cex=2, cex=.8, bty="n", ncol=1, title = 'Gender')

vcount(m1)    # Number of nodes
ecount(m1)    # Number of edges
edge_density(m1)
reciprocity(m1)    # Expected as it is a non directed social network graph

transitivity(m1, type = 'global')    # ratio of triangles to connected triples

diameter(m1)    # The longest possible route one could take

s_degree <- degree(m1, mode = 'all')
plot(m1, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5, vertex.color=c( "pink", "skyblue")[1+(V(m1)$gender=="male")], vertex.size = s_degree)
legend(x=1.5, y=1, c('Female','Male'), pch=21, col="#777777", pt.bg=c( "pink", "skyblue"), pt.cex=2, cex=.8, bty="n", ncol=1, title = 'Gender')

# Community Detection
m <- make_graph(V(m1)$name)
fc <- cluster_fast_greedy(as.undirected(m))
plot(fc,m1)

d = get.diameter(m1)
E(m1)$color = "grey"
E(m1)$width = 2
E(m1, path=d)$color = "red"
E(m1, path=d)$width = 3
V(m1)$color  = "SkyBlue2"
V(m1)[d]$color = "red"
coords = layout.fruchterman.reingold(m1)
plot(m1, layout=coords, vertex.size=3)

hist(s_degree, breaks=1:vcount(m1)-1, main="Node degree of Social Network",xlab = 'Social Connections Degree', labels = TRUE)
lines(density(s_degree))
polygon(density(s_degree),col=rgb(1,0,1,.2))


net.m1 <- asNetwork(m1)
model1 <- ergm(net.m1 ~ edges)
model1
summary(model1)

control.ergm(seed = 3622114)
m1gof <- gof(model1, GOF = ~ distance + espartners + triadcensus,
             verbose = TRUE, interval = 5e+4)
par(mfrow = c(2,2))
plot(m1gof, cex.lab=1.6, cex.axis=1.6, plotlogodds = TRUE)

model.fit <- ergmm(net.m1 ~ euclidean(d = 2, G = 2), verbose = TRUE)
summary(model.fit)
attr(model.fit$sample, "Q")
par(mfrow = c(1,2))
plot(model.fit)
plot(model.fit, pie = TRUE, vertex.cex = 2.5)

model.fit.2 <- ergmm(net.m1 ~ euclidean(d = 2, G = 3), verbose = TRUE)
summary(model.fit.2)
attr(model.fit.2$sample, "Q")
par(mfrow = c(1,2))
plot(model.fit.2)
plot(model.fit.2, pie = TRUE, vertex.cex = 2.5)

model.fit.3 <- ergmm(net.m1 ~ euclidean(d = 2, G = 4), verbose = TRUE)
summary(model.fit.3)
attr(model.fit.3$sample, "Q")
par(mfrow = c(1,2))
plot(model.fit.3)
plot(model.fit.3, pie = TRUE, vertex.cex = 2.5)

model.fit.4 <- ergmm(net.m1 ~ euclidean(d = 2, G = 5), verbose = TRUE)
summary(model.fit.4)
attr(model.fit.4$sample, "Q")
par(mfrow = c(1,2))
plot(model.fit.4)
plot(model.fit.4, pie = TRUE, vertex.cex = 2.5)
