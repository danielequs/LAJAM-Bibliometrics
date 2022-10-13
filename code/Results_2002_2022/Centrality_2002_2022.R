################################################################################
################################################################################

#Packages
library(ggplot2)
library(igraph)
library(readr)
library(tidyr)

################################################################################
################################################################################

#Import data
Adj_2002_2022<-read_csv("data/Data_2002_2022/CoAuthorship_Network/CoAuthors_2002_2022.csv",locale=locale(encoding="latin1"))
rownames(Adj_2002_2022)<-colnames(Adj_2002_2022)

################################################################################
################################################################################

#Create graph
gr_2002_2022<-graph_from_adjacency_matrix(as.matrix(Adj_2002_2022), mode = "undirected")

#Calculate centrality
deg_2002_2022<-degree(gr_2002_2022)
bet_2002_2022<-betweenness(gr_2002_2022, directed = FALSE)
clo_2002_2022<-closeness(gr_2002_2022, mode= "all")
eig_2002_2022<-eigen_centrality(gr_2002_2022)$vector
cor_2002_2022<-coreness(gr_2002_2022)

#Data frame - summary
centrality_2002_2022<-data.frame(Degree=deg_2002_2022, Betweenness=bet_2002_2022, 
      Closeness=clo_2002_2022, Eigenvector=eig_2002_2022, Coreness=cor_2002_2022)

#Export centrality
write.csv(centrality_2002_2022, file="data/Data_2002_2022/CoAuthorship_Network/AuthorsCentrality_2002_2022.csv")

################################################################################
################################################################################

#Depurate data
centrality_data<- centrality_2002_2022[order(centrality_2002_2022$Degree, decreasing = TRUE),]
centrality_dep<-centrality_data[1:20,]
centrality_dep<-data.frame(Author=rownames(centrality_dep), centrality_dep)

#Create data
centrality_plot<- centrality_dep %>%
  gather(key = "Centrality", value = "Value", -c(Author))

#Create factors
centrality_plot$Centrality<-as.factor(centrality_plot$Centrality)
centrality_plot$Centrality<-factor(centrality_plot$Centrality,
      levels= c("Degree","Betweenness","Closeness","Coreness","Eigenvector"))
centrality_plot$Author<-as.factor(centrality_plot$Author)
centrality_plot$Author<-factor(centrality_plot$Author, levels=rev(centrality_dep$Author))  

#Plot
cent_plot<-ggplot(centrality_plot, aes(x=Author, y=Value))+
  geom_point()+theme_bw()+
  facet_wrap(~Centrality, nrow=1,scales = "free_x")+
  coord_flip()+
  ylab(NULL)+
  theme(axis.text.x=element_blank(), axis.ticks.x =element_blank(), text = element_text(size=14))+
  xlab("Author")
cent_plot 

#Export
png(filename = "graphics/Data_2002_2022/CoAuthorship_Network/Centrality_Authors.png", width = 2700, height = 1950,
    units = "px",  type = "cairo", res = 300)
cent_plot
dev.off()
  
################################################################################
################################################################################
