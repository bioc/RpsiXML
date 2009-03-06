
graphSpeciesConverter = function(graph,
                                 srcSpecies,
                                 destSpecies,
                                 srcIDType="UNIPROT",
                                 destIDType="UNIPROT",
                                 keepMultGeneMatches=FALSE,
                                 keepMultProtMatches=FALSE){
    ##argument checking.
    if(!is(graph, "graphNEL")){stop("parameter graph needs to be a graphNEL.")}
    
    ##Format as a matrix for easy accessibility
    mat = as(graph, "matrix")
    if( dim(mat)[1]==dim(mat)[2] ){
        ##ReMap the col IDs        
        mcnames = inpIDMapper(colnames(mat),
          srcSpecies,
          destSpecies,
          srcIDType,
          destIDType,
          keepMultGeneMatches,
          keepMultProtMatches,
          keepMultDestIDMatches = FALSE)#you always need to limit what is returned if things are not an EG
        if( length(mcnames)==0 ){stop("There are no matches in the destination species (matching on the colname).")}
        ##Do some indexing to adjust for the fact that not everything will map over
        cnames = colnames(mat)
        names(cnames) = colnames(mat)
        cindex = cnames[names(mcnames)]

        ##select out the data from the matrix that we want.
        mat = mat[cindex,cindex]

        ##then finally reassign to the "dest species" names.
        colnames(mat) = rownames(mat) = mcnames[cindex]
        
    }else{stop("graph matrix is not symmetrical.")}
    
    ##Reformat as a graphNEL
    as(mat,"graphNEL")
}



#inciMat() ##gives the matrix of the hypergraph
#mention that if they pass an incidence matrix

hyperGraphSpeciesConverter = function(graph,
                                      srcSpecies,
                                      destSpecies,
                                      srcIDType="UNIPROT",
                                      destIDType="UNIPROT",
                                      keepMultGeneMatches=FALSE,
                                      keepMultProtMatches=FALSE){
    ##argument checking:
    if( is(graph,"Hypergraph")==FALSE && is(graph,"matrix")==FALSE ){
        stop("parameter graph needs to be a Hypergraph or an incidence matrix.")}

    ##graph could be a matrix or a Hypergraph.  If its a Hypergraph, then we want to convert it for now
    if(is(graph, "Hypergraph")==TRUE){
        mat = inciMat(graph)
    }else{
        mat = graph
    }

    ##Its ALWAYS going to be the rows that are the nodes when you call inciMat()    
    ##ReMap the row IDs        
    mrnames = inpIDMapper(rownames(mat),
      srcSpecies,
      destSpecies,
      srcIDType,
      destIDType,
      keepMultGeneMatches,
      keepMultProtMatches,
      keepMultDestIDMatches = FALSE)#you always need to limit what is returned if things are not an EG
    if( length(mrnames)==0 ){stop("There are no matches in the destination species (matching on the rownames).")}
    ##Do some indexing to adjust for the fact that not everything will map over
    rnames = rownames(mat)
    names(rnames) = rownames(mat)
    rindex = rnames[names(mrnames)]
    
    ##select out the data from the matrix that we want.
    mat = mat[rindex,]
    
    ##then finally reassign to the "dest species" names.
    rownames(mat) = mrnames[rindex]

    ##return the correct format
    if(is(graph, "Hypergraph")){
        ##Reformat as a Hypergraph and return
        return(revInciMat(mat)) 
    }else{
        ##return incidence matrix
        return(mat)
    }
    
}

