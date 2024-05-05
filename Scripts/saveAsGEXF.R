library(rgexf) 
library(igraph)

### creat an igraph object 
## 1 node
mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators_new.xlsx', sheet = 1)
network <- mtc.network(data.ab = mydata)

nodes <- network$treatments
colnames(nodes) <- c("id", "lable")

dat <- network[['data.ab']]
yaowu <- network[['treatments']]$description

number <- data.frame(matrix(ncol = 2, nrow = 0))
for (x in 1:length(yaowu)){
  number[x,][1] <- yaowu[x]
  number[x,][2] <- sum(dat[dat$treatment == yaowu[x],]$sampleSize)
}

colnames(number) <- c("id", "weight")
nodes <- merge(nodes,number, by="id") #点完成

## 2 edge

edges <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(edges) <- c("from", "to", "weight")

# 边未完成：如何计算每个结局的研究数量作为边的权重

edges <- read.csv(file = "data/edges.csv")



net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)

# -------------------------------------------------------------------------
# cannot save，不知原因
cg1 <- erdos.renyi.game(5, 0.4)
# Converts the given igraph object to GEXF format and saves it at the given filepath location
#     g: input igraph object to be converted to gexf format
#     filepath: file location where the output gexf file should be saved
#
saveAsGEXF = function(g, filepath="converted_graph.gexf")
{
  require(igraph)
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  if(is.null(V(g)$label))
    V(g)$label <- as.character(V(g))
  
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(E(g)$weight))
    E(g)$weight <- rep.int(1, ecount(g))
  
  nodes <- data.frame(cbind(V(g), V(g)$label))
  edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(list.vertex.attributes(g), "label") 
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))))
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  eAttrNames <- setdiff(list.edge.attributes(g), "weight") 
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))))
  
  # combine all graph attributes into a meta-data
  graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&",get.graph.attribute(g, attr)))
  
  # generate the gexf object
  output <- write.gexf(nodes, edges, 
                       edgesWeight=E(g)$weight,
                       edgesAtt = edgesAtt,
                       nodesAtt = nodesAtt,
                       meta=c(list(creator="Gopalakrishna Palem", description="igraph -> gexf converted file", keywords="igraph, gexf, R, rgexf"), graphAtt))
  
  print(output, filepath, replace=T)
}

saveAsGEXF(net) 
