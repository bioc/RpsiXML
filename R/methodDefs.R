setMethod("show", "psimi25Interactor", function(object) {
  cat("interactor (", object@uniprotId, "):\n",
      "---------------------------------\n",
      "[ source database ]: ", object@sourceDb, "\n",
      "[ source ID ]: ", object@sourceId, "\n",
      "[ short label ]: ", object@shortLabel, "\n",
      "[ UniProt ID ]: ", object@uniprotId, "\n",
      "[ organism ]: ", object@organismName, "\n",
      "[ NCBI Taxonomy ID ]: ", object@taxId, "\n",
      "[ xref ]: ",nrow(xref(object)),"xrefs found, Use 'xref(x)' to see more annotation\n")
})

setMethod("show", "psimi25Interaction", function(object) {
  cat("interaction (", object@sourceId, "):\n",
      "---------------------------------\n",
      "[ source database ]: ", object@sourceDb, "\n",
      "[ source experiment ID ]: ", object@expSourceId, "\n",
      "[ interaction type ]: ", object@interactionType, "\n",
      "[ experiment ]: pubmed ", object@expPubMed, "\n",
      "[ participant ]: ", object@participant, "\n",
      "[ bait ]: ", object@bait, "\n",
      "[ bait UniProt ]: ", object@baitUniProt, "\n",
      "[ prey ]: ", object@prey, "\n",
      "[ prey UniProt ]: ", object@preyUniProt, "\n"
      )
}
          )

setMethod("show", "psimi25InteractionEntry", function(object) {
  cat("==================================\n",
      "interaction entry (", object@releaseDate, "):\n",
      "==================================\n",
      "[ organism ]: ", object@organismName, "\n",
      "[ taxonomy ID ]: ", object@taxId, "\n",
      "[ interactors ]: there are ", length(interactors(object)), 
      " interactors in total, here are the first few ones:\n")
  print(utils::head(interactorInfo(object)))
  cat("...\n", "[ interactions ]: there are ", 
      length(object@interactions), " interactions in total, here are the first few ones:\n")
  print(utils::head(object@interactions))
  cat("...\n")
  cat("Please use 'interactors' to view all interactors, and 'interactions' to view all interactions.\n")
})

setMethod("show","psimi25Complex", function(object) {
  cat("complex (", object@sourceId, ")\n",
      "---------------------------------\n",
      "[ source database ]: ", object@sourceDb, "\n",
      "[ source ID ]: ", object@sourceId, "\n",
      "[ full name ]: ", object@fullName, "\n",
      "[ organism ]: ", object@organismName, "\n",
      "[ taxonomy ID ]: ", object@taxId, "\n",
      "[ attributes ]: \n")
  attrs = object@attributes
  for (i in seq(along=attrs))
    cat(strbreak(paste(names(attrs)[i], attrs[i], sep=": ")), "\n")
  cat(" [ members ]: \n")
  print(object@members)
})

setMethod("show", "psimi25ComplexEntry", function(object) {
  cat("==================================\n",
      "complex entries (", object@releaseDate, "):\n",
      "==================================\n",
      "[ interactors ]: there are", length(object@interactors), 
      "interactors in total, here are the first few ones:\n")
  print(utils::head(interactorInfo(object)))
  cat("...\n", "[ complexes ]: there are", length(object@complexes), 
      "complexes in total, here are the first few ones:\n")
  print(utils::head(object@complexes))
  cat("...\n")
  cat("Please use 'interactors' to view all interactors, and use 'complexes' to view all complexes.\n")
})

          
setMethod("show", "psimi25Graph", function(object){
  print(as(object, "graph"))
  print(class(object))
})

setMethod("show", "psimi25Hypergraph", function(object){
  print(class(object))
})

setMethod("abstract", signature(object="psimi25Graph"),
          function(object) object@abstract)

setMethod("sourceDb", signature(x="psimi25Interactor"),
          function(x) x@sourceDb)
setMethod("sourceDb", signature(x="psimi25Complex"),
          function(x) x@sourceDb)


setMethod("sourceId", signature(x="psimi25Complex"),
          function(x) x@sourceId)

setMethod("sourceDb", signature(x="psimi25Interaction"),
          function(x) x@sourceDb)
setMethod("sourceDb", signature(x="psimi25Source"),
          function(x) x@sourceDb)

setMethod("sourceId", signature(x="psimi25Interactor"),
          function(x) x@sourceId)
setMethod("sourceId", signature(x="psimi25Complex"),
          function(x) x@sourceId)
setMethod("sourceId", signature(x="psimi25Interaction"),
          function(x) x@sourceId)

setMethod("uniprot", signature("psimi25Source"),
          function(x) x@uniprotSymbol)
setMethod("uniprot", signature("psimi25Interactor"),
          function(x) x@uniprotId)

setMethod("translateID", signature(r="psimi25Graph"),
          function(r, to){
            its <- interactors(r)
            trIds <- translateID(its, to=to)
            ##tc asks - is it safer to put nodes(r) = trIds[nodes(r)]
            ##since the names of trIds is the uniprot Ids?
            ##  -- is it necessary? trIds are the vector of same length returned by translateID. It should match
            ##     nodes(r) well. Am I right? And try names(trIds) here, you will find a suffix ".id" is added
            nodes(r) <- trIds
            return(r)
          })

setMethod("translateID", signature(r="psimi25Hypergraph"),
          function(r, to, uniprotId){
            its <- interactors(r)
            if(length(uniprotId) == 1 && is.na(uniprotId)) {
              nits <- is.na(names(its))
              return(translateID(its[nits], to))
            } else if (!uniprotId %in% names(its)) {
              warning("'uniprotId':", uniprotId, " not found in the hypergraph!")
              return(NULL)
            } else {
              translateID(its[[uniprotId]], to)
            }
          })

setMethod("translateID", signature(r="list"),
          function(r, to) {
            if (!all(sapply(r, is, "psimi25Interactor"))) {
              stop("'x' must be a list of psimi25Interactor objects!")
            }
            ## all available dbs
            ALLDB <- toupper(unique(unlist(sapply(r, availableXrefs))))
            TO <- toupper(to)
            if (!any(ALLDB %in% TO)) {
              stop(sprintf("%s not available! available xrefs are: %s\n",
                           TO,
                           paste(ALLDB, collapse=",")
                           ))
            }
            sapply(r, translateID, to=to)
          })

setMethod("translateID",
          signature(r="psimi25Interactor"),
          function(r, to) {
            if(missing(to)) {
              to <- "sourceId"
            }
            TO <- toupper(to)
            xrefs <- xref(r)
            xrefDB <- toupper(xrefs[,"db"])
            xrefid <- xrefs[,"id"]

            isTo <- grep(TO,xrefDB)
            newID <- unlist(xrefs[isTo,"id"])
            
            return(null2na(newID))
          })

setMethod("initialize",
          signature=signature(
            .Object="psimi25Hypergraph"),
          function(.Object, interactors, ...) {
            .Object@interactors <- interactors
            callNextMethod(.Object, ...)
          })

setMethod("confidenceValue", "psimi25Interaction", function(x) {
   return(x@confidenceValue)
})

### interactors
setMethod("xref", "psimi25Interactor", function(x) {
  return(get("xref", x@xref))
})
setMethod("availableXrefs", signature(x="psimi25Interactor"),
          function(x) {
            xrefs <- xref(x)
            unique(unlist(xrefs[,"db"]))
          })
setMethod("availableXrefs", signature(x="list"),
          function(x, intersect=FALSE) {
            isClass <- all(sapply(x, inherits, "psimi25Interactor"))
            if(!isClass) {
              stop("'x' must be a list of 'psimi25Interactor' objects!")
            }
            xrefs <- lapply(x, availableXrefs)
            uXrefs <- unique(unlist(xrefs))
            if(!intersect) {
              return(uXrefs)
            } else {
              mat <- sapply(xrefs,function(x) uXrefs %in% x)
              uXrefEverywhere <- apply(mat, 1, all)
              return(uXrefs[uXrefEverywhere])
            }
          })
setMethod("availableXrefs",
          signature(x="psimi25InteractionEntry"),
          function(x,...) {
            ints <- interactors(x)
            return(availableXrefs(ints, ...))
          })

setMethod("interactors", signature(x="psimi25InteractionEntry"),
          function(x) x@interactors)

setReplaceMethod("interactors", signature(x="psimi25InteractionEntry", value="NULL"),
          function(x,value) {
            x@interactors <- list()
            return(x)
          })
setReplaceMethod("interactors", signature(x="psimi25InteractionEntry", value="list"),
          function(x,value) {
            vcInt <- sapply(value, inherits, "psimi25Interactor")
            if(!all(vcInt))
              stop("'value' must be a list of psimi25Interactors")
            x@interactors <- value
            return(x)
            })           

setMethod("interactors", signature(x="psimi25ComplexEntry"),
          function(x) x@interactors)
setMethod("interactors", signature(x="psimi25Graph"),
          function(x) x@interactors)
setMethod("interactors", signature(x="psimi25Hypergraph"),
          function(x) x@interactors)

setMethod("interactorInfo", signature(x="psimi25InteractionEntry"),
          function(x) {
            interactors <- interactors(x)
            interactorInfo(interactors)
            })
setMethod("interactorInfo", signature(x="psimi25Graph"),
          function(x) {
            interactors <- interactors(x)
            interactorInfo(interactors)
            })
setMethod("interactorInfo", signature(x="psimi25Hypergraph"),
          function(x) {
            interactors <- interactors(x)
            interactorInfo(interactors)
            })
setMethod("interactorInfo", signature(x="psimi25ComplexEntry"),
          function(x) {
            interactors <- interactors(x)
            interactorInfo(interactors)
            })

setMethod("interactorInfo", signature(x="list"),
          function(x) {
            stopifnot(all(sapply(x, is, "psimi25Interactor")))
            interactorFeatures <- c("sourceDb","sourceId","shortLabel",
                                    "uniprotId",  "organismName", "taxId")
            interactorInfo <- matrix(character(0), length(x), 
                                     length(interactorFeatures))
            colnames(interactorInfo) <- interactorFeatures
            if(length(x) > 0) {
              for (p in seq(x)) {
                for (q in interactorFeatures) {
                  interactorInfo[p,q] <- slot(x[[p]], q)
                }
              }
            }
            rownames(interactorInfo) <- interactorInfo[,"uniprotId"]
            interactorInfo
          })

setMethod("interactions", signature(x="psimi25InteractionEntry"),
          function(x) x@interactions)
setReplaceMethod("interactions", signature(x="psimi25InteractionEntry", value="list"),
          function(x, value) {
            vlInteraction <- sapply(value, inherits, "psimi25Interaction")
            if(!all(vlInteraction))
              stop("'value' must be a list of psimi25Interaction")
            x@interactions <- value
            return(x)
            })

setMethod("complexes", signature(x="psimi25ComplexEntry"),
          function(x) x@complexes)
setMethod("taxId", signature(x="psimi25Complex"),
          function(x) x@taxId)


setMethod("complexMembers", "psimi25Complex", function(x) {
  return(x@members)
})
setMethod("complexName", "psimi25Complex", function(x) {
  return(x@fullName)
})
setMethod("complexAttributes", "psimi25Complex", function(x) {
  return(x@attributes)
})


## interactor accessors
setMethod("accession", "psimi25Interactor", function(x) {
  sourcedb <- sourceDb(x)
  accession <- translateID(x, sourceDb(x))
  return(accession)
})

## interaction accessors
setMethod("bait", "psimi25Interaction", function(x) {
  return(x@baitUniProt)
})
setMethod("baitAccession", "psimi25Interaction", function(x) {
  return(x@bait)
})
setMethod("prey", "psimi25Interaction", function(x)  {
  return(x@preyUniProt)
})
setMethod("preyAccession", "psimi25Interaction", function(x) {
  return(x@prey)
})
setMethod("participant", "psimi25Interaction", function(x) {
  return(x@participant)
})

setMethod("inhibitor", "psimi25Interaction", function(x) {
  return(x@inhibitor)
})
setMethod("neutralComponent", "psimi25Interaction", function(x) {
  return(x@neutralComponent)
})
setMethod("pubmedID", "psimi25Interaction", function(x) {
  return(x@expPubMed)
})



setMethod("organismName", signature(x="psimi25InteractionEntry"),
          function(x) x@organismName)
setReplaceMethod("organismName", signature(x="psimi25InteractionEntry", value="character"),
                 function(x, value) {
                   x@organismName <- value
                   return(x)
                 })
setMethod("taxId", signature(x="psimi25InteractionEntry"),
          function(x) x@taxId)
setReplaceMethod("taxId", signature(x="psimi25InteractionEntry", value="character"),
                 function(x, value) {
                   x@taxId <- value
                   return(x)
                   
                 })
setMethod("releaseDate", signature(x="psimi25InteractionEntry"),
          function(x) x@releaseDate)
setReplaceMethod("releaseDate", signature(x="psimi25InteractionEntry", value="character"),
                 function(x, value) {
                   x@releaseDate <- value
                   return(x)
                 })

setMethod("interactionType", signature(x="psimi25Interaction"),
          function(x) x@interactionType)


setMethod("parseExperiment", signature(x="psimi25Source"),
          function(x,...) x@parseExperiment(...,x)
          )
setMethod("parseInteractor", signature(x="psimi25Source"),
          function(x,...) x@parseInteractor(..., x)
          )
setMethod("parseComplex", signature(x="psimi25Source"),
          function(x,...) x@parseComplex(...,x)
          )

setMethod("numInteractors", signature(x="psimi25InteractionEntry"), function(x) {
  xit <- interactors(x)
  return(length(xit))
})

setMethod("numInteractions", signature(x="psimi25InteractionEntry"), function(x) {
  xit <- interactions(x)
  return(length(xit))
})
