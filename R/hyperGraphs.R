list2Matrix <- function(namedList, type="interaction"){

  ##currently, this will output the incidence matrix so multiplicity is killed
  
  numHE <- length(namedList)
  sizeHE <- listLen(lapply(namedList, unique))

  if(length(numHE)==1 && numHE == 1 && sizeHE == 1){
    m = matrix(1,1,1)
    colnames(m) = names(namedList)
    rownames(m) = unique(unlist(namedList))
    return(m)}
  else{

	if(type=="interaction"){
          id <- names(namedList)	
          he <- mapply(function(x,y) Hyperedge(x,y), namedList, id)
          bpHG <- new("Hypergraph", nodes = union(id,unique(unlist(namedList))), he)
          bpMat <- inciMat(bpHG)
          return(bpMat)
	}	
        
	if(type=="complex"){
          id <- names(namedList)	
          he <- mapply(function(x,y) Hyperedge(x,y), namedList, id)
          bpHG <- new("Hypergraph", nodes = unique(unlist(namedList)), he)
          bpMat <- inciMat(bpHG)
          return(bpMat)
          
	}
        
        
      }
}


genBPGraph <- function(bpMat, directed=TRUE, bp=TRUE){

  bpMat1 <- bpMat
  b <- rownames(bpMat)
  p <- colnames(bpMat)

  if(!bp){
    if(sum(b != p) != 0){
      stop("The rownames and the colnames must be identical.")
    }
  }

  else{
    baits <- union(rownames(bpMat), colnames(bpMat))
    preys <- baits

    bpMat1 <- matrix(0, length(baits), length(preys))
    dimnames(bpMat1) <- list(baits, preys)
    bpMat1[b,p] <- bpMat
    if(!directed) {
      bpMat1 <- bpMat1 + t(bpMat1)
      mode(bpMat1) <- "logical"
      mode(bpMat1) <- "numeric"
    }
  }

  if(directed){
    bpGraph <- as(bpMat1, "graphNEL")
  }

  else{
    bpGraph <- as(bpMat1, "graphNEL")
    bpGraph <- ugraph(removeSelfLoops(bpGraph))
  }
  
  bpGraph
  
}
