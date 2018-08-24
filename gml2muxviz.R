library(igraph)

#takes path to gml with types of nodes, 
#and writes extended edge list, layer info and layout

gml2muxviz <- function(gml, 
                       weighted = TRUE, 
                       type = "type", 
                       layerLabel = "Layer_", 
                       outpath = "", 
                       base.name = "graph"){
  g = igraph::read.graph(gml, "gml")
  
  #set node attribute layer based on node attribute "type"
  g = set.vertex.attribute(g, name = "layer", value = as.numeric(as.factor(vertex_attr(g)[[type]]))) 
  
  #set atttribute "source layer"
  g = set_edge_attr(graph = g, 
                    name = "SourceLayer", 
                    index = E(g), 
                    value = get.vertex.attribute(graph = g, 
                                                 name = "layer", 
                                                 index = head_of(graph = g, 
                                                                 es = E(g)
                                                 )
                    )
  )
  #set attribute target layer
  g = set_edge_attr(graph = g, 
                    name = "TargetLayer", 
                    index = E(g), 
                    value = get.vertex.attribute(graph = g, 
                                                 name = "layer", 
                                                 index = tail_of(graph = g, 
                                                                 es = E(g)
                                                 )
                    )
  )
  
  #make edge data frame for extended edge list
  dat.frm = get.data.frame(g)
  
  if(weighted==TRUE){
  dat.frm = dat.frm[, c("from", "SourceLayer", "to", "TargetLayer", "weight")]
  }else{
    dat.frm = dat.frm[, c("from", "SourceLayer", "to", "TargetLayer")]
  }
  
  #layout data frame
  layoutDF = data.frame(nodeID = 1:length(V(g)), 
                        nodeLabel   = V(g)$name
  )
  
  #layer data frame 
  layerDF = data.frame(layerID = unique(V(g)$layer), 
                       layerLabel = paste0(layerLabel, 
                                           unique(V(g)$layer)
                                           )
  )
  
  #write out 
  
  #extended Edge
  fileOut = paste0(outpath, base.name, ".edge")
  write.table(x = dat.frm, file = fileOut, 
              row.names = FALSE, 
              col.names = FALSE, 
              quote = FALSE, 
              sep = " "
  )
  
  ##layout file
  fileOut = paste0(outpath, base.name, ".layout")
  write.table(x = layoutDF, file = fileOut, 
              row.names = FALSE, 
              col.names = TRUE, 
              quote = FALSE, 
              sep = " "
  )
  
  ## layer info
  fileOut = paste0(outpath, base.name, ".layer")
  write.table(x = layerDF, file = fileOut, 
              row.names = FALSE, 
              col.names = TRUE, 
              quote = FALSE, 
              sep = " "
  )
  
  
}
