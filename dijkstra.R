dijkstra <- function(graph, init_node){
  
  stopifnot(is.data.frame(graph), is.numeric(init_node))# Check if graph is a data frame and if init_node is numeric
  stopifnot(length(graph) == 3, length(init_node) == 1) #check if graph have the three columns and if the length of ini_node is only 1
  stopifnot(is.element(init_node,graph[[1]])) #Check that the initial node is part from the nodes in the data frame graph
  stopifnot(colnames(graph) == c("v1","v2", "w")) #check that the data frame has the three columns with the proper names
  # End of the format check
  
  }