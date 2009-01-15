
graphSpeciesConverter = function(graphNEL,
                                 srcSpecies,
                                 destSpecies,
                                 srcIDType="UNIPROT",
                                 destIDType="UNIPROT",
                                 keepMultGeneMatches=FALSE,
                                 keepMultProtMatches=FALSE){

    ##Format as a matrix for easy accessibility
    mat = as(graphNEL, "matrix")
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
