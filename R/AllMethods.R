##----------------------------------------------------------------------------##
##
## AllMethods: S4-class methods for RpsiXML package
##
## Author: Jitao David Zhang <j.zhang@dkfz.de>
##----------------------------------------------------------------------------##

##------------------------------------------------------------##
## auxilliary functions
##------------------------------------------------------------##
printManyItems <- function(title, x) {
  cat(title, ":", paste(x, collapse=", "), "\n")
}

setMethod("name", "ANY", function(object) {
  object@name
})
setMethod("id", "ANY", function(object) {
  object@id
})
setReplaceMethod("id", c("ANY", "numeric"), function(object, value) {
  value <- as.integer(value)
  object@id <- value
  return(value)
})

setReplaceMethod("name", c("ANY","character"), function(object,value) {
  object@name <- value
  return(object)
})

##------------------------------------------------------------##
## typedList
##------------------------------------------------------------##
setMethod("typedList", "ANY" , function(..., type) {
  ## compatible with both parameters and a list
  x <- list(...)
  if(length(x) == 1)
    x <- x[[1]]
  
  if(missing(type))
    type <- class(x[[1]])
  obj <- new("typedList", .Data=x, type=type)
  return(obj)
})

setMethod("show", "typedList", function(object) {
  cat("Typed List of", object@type, "\n")
  x <- object@.Data ## using temporary object, otherwise the list name is missing
  names(x) <- names(object)
  show(x)
})

##------------------------------------------------------------##
## psimi25NamesType
##------------------------------------------------------------##
setMethod("psimi25NamesType",
          c("character","character","character"),
          function(shortLabel, fullName, alias) {
            obj <- new("psimi25NamesType",
                       shortLabel=shortLabel,
                       fullName=fullName,
                       alias=alias)
            return(obj)
          })
setMethod("show", "psimi25NamesType", function(object) {
  printManyItems("Short Label", object@shortLabel)
  printManyItems("Full name", object@fullName)
  printManyItems("Alias", object@alias)
})

##------------------------------------------------------------##
## psimi25Attribute
##------------------------------------------------------------##
setMethod("psimi25Attribute",
          c("character","ANY","ANY"),
          function(iValue, name, nameAc) {
            if(missing(name))
              name <- as.character(NA)
            if(missing(nameAc))
              nameAc <- as.character(NA)
            obj <- new("psimi25Attribute",
                       .Data=iValue,
                       name=name,
                       nameAc=nameAc)
            return(obj)
          })
setMethod("show", "psimi25Attribute", function(object) {
  cat(object@name, "[", object@nameAc, "]", ":", object@.Data, "\n")
})
setMethod("name", "psimi25Attribute", function(object) {
  return(object@name)
})
setReplaceMethod("name", c("psimi25Attribute", "character"), function(object,value) {
  object@name <- value
  return(object)
})
setMethod("nameAc", "psimi25Attribute", function(object) {
  return(object@nameAc)
})
setReplaceMethod("nameAc", "psimi25Attribute", function(object,value) {
  object@nameAc <- value
  return(object)
})
setMethod("iValue", "psimi25Attribute", function(object) {
  return(object@.Data)
})
setReplaceMethod("iValue", "psimi25Attribute", function(object,value) {
  object@.Data <- value
  return(object)
})

setMethod("psimi25AttributeListType",
          "typedList",
          function(typedList) {
            obj <- new("psimi25AttributeListType",
                       .Data=typedList)
            return(obj)
          })
          

##------------------------------------------------------------##
## psimi25DbReferenceType
##------------------------------------------------------------##
setMethod("psimi25DbReferenceType",
          c("typedList", "ANY", "ANY","ANY","ANY","ANY","ANY","ANY"),
          function(typedList, db, dbAc, id, secondary, version, refType, refTypeAc) {
            if(missing(db)) db <- as.character(NA)
            if(missing(dbAc)) dbAc <- as.character(NA)
            if(missing(id)) id <- as.character(NA)
            if(missing(secondary)) secondary <- as.character(NA)
            if(missing(version)) version <- as.character(NA)
            if(missing(refType)) refType <- as.character(NA)
            if(missing(refTypeAc)) refTypeAc <- as.character(NA)

            obj <- new("psimi25DbRereferenceType",
                       .Data=typedList,
                       db=db, dbAc=dbAc, id=id, secondary=secondary,
                       version=version, refType=refType, refTypeAc=refTypeAc)
          })
          
setMethod("show", "psimi25DbReferenceType",
          function(object) {
            show(object@.Data)
            info <- data.frame(Attribute=c("db","dbAc","id","secondary",
                                 "version","refType","refTypeAc"),
                               Value=c(object@db, object@dbAc, object@id, object@secondary,
                                 object@version, object@refType, object@refTypeAc))
            show(info)
          })

##------------------------------------------------------------##
## psimi25XrefType
##------------------------------------------------------------##
setMethod("psimi25XrefType",
          c("psimi25DbReferenceType","psimi25DbReferenceTypeList"),
          function(primaryRef,secondaryRef) {
            obj <- new("psimi25XrefType",
                       primaryRef=primaryRef,
                       secondaryRef=secondaryRef)
            return(obj)
          })

##------------------------------------------------------------##
## psimi25Availability
##------------------------------------------------------------##
setMethod("psimi25AvailabilityType",
          c("character","ANY"),
          function(iValue, id) {
            suppressWarnings(id <- as.integer(id)) ## reason: some repositories do not adhere to the specification of id
            obj <- new("psimi25AvailabilityType",
                       .Data=iValue, id=id)
            return(obj)
          })
setMethod("show",
          "psimi25AvailabilityType",
          function(object) {
            printManyItems(object@id, object@.Data)
          })
setMethod("iValue", "psimi25AvailabilityType",
          function(object) {
            return(object@.Data)
          })
setReplaceMethod("iValue", "psimi25AvailabilityType",
                 function(object,value) {
                   object@.Data <- value
                   return(object)
                 })

##------------------------------------------------------------##
## psimi25CvType and psimi25OpenCvType
##------------------------------------------------------------##
setMethod("name", "psimi25CommonNameRef", function(object) {
  object@name
})

setReplaceMethod("name", c("psimi25CommonNameRef","psimi25NamesType"), function(object,value) {
  object@name <- value
  return(object)
})

setMethod("xref", "psimi25CommonNameRef", function(object) {
  object@xref
})

setReplaceMethod("xref", c("psimi25CommonNameRef","psimi25XrefType"), function(object,value) {
  object@xref <- value
  return(object)
})

setMethod("attributeList", "psimi25CommonNameRefAttr", function(object) {
  object@attributeList
})


setReplaceMethod("attributeList", c("psimi25CommonNameRefAttr","psimi25AttributeListType"), function(object,value) {
  object@attributeList <- value
  return(object)
})


setMethod("psimi25CvType",
          c("psimi25NamesType","psimi25XrefType"),
          function(name, xref) {
            obj <- new("psimi25CvType",
                       name=name,
                       xref=xref)
            return(obj)
          })
setMethod("psimi25OpenCvType",
          c("psimi25NamesType", "psimi25XrefType","psimi25AttributeListType"),
          function(name, xref, attributeList) {
            obj <- new("psimi25OpenCvType",
                       name=name,
                       xref=xref,
                       attributeList=attributeList)
            return(obj)
          })

##------------------------------------------------------------##
## biosourceType
##------------------------------------------------------------##
setMethod("psimi25BioSourceType",
          c("psimi25NamesType","psimi25OpenCvType","psimi25OpenCvType", "psimi25OpenCvType", "numeric"),
          function(name, cellType, compartment, tissue, ncbiTaxId) {
            ncbiTaxId <- as.integer(ncbiTaxId) ## in case of numeric
            obj <- new("psimi25BioSourceType",
                       name=name,
                       cellType=cellType,
                       compartment=compartment,
                       tissue=tissue,
                       ncbiTaxId=ncbiTaxId)
            return(obj)
          })
setMethod("cellType", "psimi25BioSourceType", function(object) object@cellType)
setMethod("compartment", "psimi25BioSourceType", function(object) object@compartment)
setMethod("tissue", "psimi25BioSourceType", function(object) object@tissue)
setMethod("ncbiTaxId", "psimi25BioSourceType", function(object) object@ncbiTaxId)

setReplaceMethod("cellType", c("psimi25BioSourceType","psimi25OpenCvType"),
                 function(object,value) { object@cellType <- value; return(object) })
setReplaceMethod("compartment", c("psimi25BioSourceType","psimi25OpenCvType"),
                 function(object,value) { object@compartment <- value; return(object) })
setReplaceMethod("tissue", c("psimi25BioSourceType","psimi25OpenCvType"),
                 function(object,value) { object@tissue <- value; return(object) })
setReplaceMethod("ncbiTaxId", c("psimi25BioSourceType","numeric"),
                 function(object,value) {
                   value <- as.integer(value)
                   object@ncbiTaxId <- value;
                   return(object)
                 })

##------------------------------------------------------------##
## psimi25ConfidenceType
##------------------------------------------------------------##
setMethod("psimi25ConfidenceType",
          c("psimi25OpenCvType","character"),
          function(unit, value) {
            obj <- new("psimi25ConfidenceType",
                       unit=unit,
                       value=value)
            return(obj)
          })
setMethod("unit", "psimi25ConfidenceType", function(object) object@unit)
setMethod("value", "psimi25ConfidenceType", function(object) object@value)

setReplaceMethod("unit", c("psimi25ConfidenceType","psimi25OpenCvType"),
                 function(object,value) { object@unit <- value; return(object) })
setReplaceMethod("value", c("psimi25ConfidenceType","character"),
                 function(object,value) { object@value <- value; return(object) })

##------------------------------------------------------------##
## psimi25InteractorElementType
##------------------------------------------------------------##
setMethod("psimi25InteractorElementType",
          c("psimi25NamesType","psimi25XrefType","psimi25AttributeListType",
            "psimi25CvType","psimi25BioSourceType","character", "numeric"),
          function(name, xref, attributeList,
                   interactorType, organism, sequence, id) {
            obj <- new("psimi25InteractorElementType",
                       name=name,
                       xref=xref,
                       attributeList=attributeList,
                       interactorType=interactorType,
                       organism=organism,
                       sequence=sequence,
                       id=id)
            return(obj)
          })

setMethod("interactorType", "psimi25InteractorElementType", function(object) object@interactorType)
setMethod("organism", "psimi25InteractorElementType", function(object) object@organism)
setMethod("Sequence", "psimi25InteractorElementType", function(object) object@sequence)

setReplaceMethod("interactorType", c("psimi25InteractorElementType","psimi25CvType"),
                 function(object,value) { object@interactorType <- value; return(object) })
setReplaceMethod("organism", c("psimi25InteractorElementType","psimi25BioSourceType"),
                 function(object,value) { object@organism <- value; return(object) })
setReplaceMethod("Sequence", c("psimi25InteractorElementType","character"),
                 function(object,value) { object@sequence <- value; return(object) })

##------------------------------------------------------------##
## psimi25ParticipantType
##------------------------------------------------------------##
setMethod("interactorRef", "psimi25ParticipantType", function(object) object@interactorRef)
setMethod("interactor", "psimi25ParticipantType", function(object) object@interactor)
setMethod("interactionRef", "psimi25ParticipantType", function(object) object@interactionRef)
setMethod("participantIdentificationMethodList", "psimi25ParticipantType", function(object) object@participantIdentificationMethodList)
setMethod("biologicalRole", "psimi25ParticipantType", function(object) object@biologicalRole)
setMethod("experimentalRoleList", "psimi25ParticipantType", function(object) object@experimentalRoleList)
setMethod("experimentalPreparationList", "psimi25ParticipantType", function(object) object@experimentalPreparationList)
setMethod("experimentalInteractorList", "psimi25ParticipantType", function(object) object@experimentalInteractorList)
setMethod("featureList", "psimi25ParticipantType", function(object) object@featureList)
setMethod("hostOrganismList", "psimi25ParticipantType", function(object) object@hostOrganismList)
setMethod("confidenceList", "psimi25ParticipantType", function(object) object@confidenceList)
setMethod("parameterList", "psimi25ParticipantType", function(object) object@parameterList)

setReplaceMethod("interactorRef", c("psimi25ParticipantType","numeric"),
                 function(object,value) {
                   value <- as.integer(value)
                   object@interactorRef <- value; return(object)
                 })
setReplaceMethod("interactor", c("psimi25ParticipantType","psimi25InteractorElementType"),
                 function(object,value) {
                   object@interactor <- value; return(object)
                 })
setReplaceMethod("interactionRef", c("psimi25ParticipantType","integer"),
                 function(object,value) {
                   value <- as.integer(value)
                   object@interactionRef <- value; return(object)
                 })
setReplaceMethod("participantIdentificationMethodList", c("psimi25ParticipantType","psimi25CvExperimentRefsList"),
                 function(object,value) {
                   object@participantIdentificationMethodList <- value; return(object)
                 })
setReplaceMethod("biologicalRole", c("psimi25ParticipantType","psimi25CvType"),
                 function(object,value) {
                   object@biologicalRole <- value; return(object)
                 })
setReplaceMethod("experimentalRoleList", c("psimi25ParticipantType","psimi25CvExperimentRefsList"),
                 function(object,value) {
                   object@experimentalRoleList <- value; return(object)
                 })
setReplaceMethod("experimentalPreparationList", c("psimi25ParticipantType","psimi25CvExperimentRefsList"),
                 function(object,value) {
                   object@experimentalPreparationList <- value; return(object)
                 })
setReplaceMethod("experimentalInteractorList", c("psimi25ParticipantType","psimi25ExperimentInteractor"),
                 function(object,value) {
                   object@experimentalInteractorList <- value; return(object)
                 })
setReplaceMethod("featureList", c("psimi25ParticipantType","psimi25FeatureElementTypeList"),
                 function(object,value) {
                   object@featureList <- value; return(object)
                 })
setReplaceMethod("hostOrganismList", c("psimi25ParticipantType","psimi25HostOrganismList"),
                 function(object,value) {
                   object@hostOrganismList <- value; return(object)
                 })
setReplaceMethod("confidenceList", c("psimi25ParticipantType","psimi25ConfidenceListType"),
                 function(object,value) {
                   object@confidenceList <- value; return(object)
                 })
setReplaceMethod("parameterList", c("psimi25ParticipantType","psimi25ParameterTypeList"),
                 function(object,value) { object@parameterList <- value; return(object) })

##------------------------------------------------------------##
## psimi25InteractionElementType
##------------------------------------------------------------##
setMethod("participantList", "psimi25InteractionElementType", function(object) object@participantList)
setMethod("inferredInteractionList", "psimi25InteractionElementType", function(object) object@inferredInteractionList)
setMethod("interactionType", "psimi25InteractionElementType", function(object) object@interactionType)
setMethod("modelled", "psimi25InteractionElementType", function(object) object@modelled)
setMethod("experimentList", "psimi25InteractionElementType", function(object) object@experimentList)
setMethod("intraMolecular", "psimi25InteractionElementType", function(object) object@intraMolecular)
setMethod("negative", "psimi25InteractionElementType", function(object) object@negative)
setMethod("confidenceList", "psimi25InteractionElementType", function(object) object@confidenceList)
setMethod("parameterList", "psimi25InteractionElementType", function(object) object@parameterList)
setMethod("imexId", "psimi25InteractionElementType", function(object) object@imexId)

setReplaceMethod("participantList", c("psimi25InteractionElementType","psimi25ParticipantTypeList"),
                 function(object,value) { object@participantList <- value; return(object) })
setReplaceMethod("inferredInteractionList", c("psimi25InteractionElementType","psimi25InferredInteraction"),
                 function(object,value) { object@inferredInteractionList <- value; return(object) })
setReplaceMethod("interactionType", c("psimi25InteractionElementType","psimi25CvType"),
                 function(object,value) { object@interactionType <- value; return(object) })
setReplaceMethod("modelled", c("psimi25InteractionElementType","logical"),
                 function(object,value) { object@modelled <- value; return(object) })
setReplaceMethod("intraMolecular", c("psimi25InteractionElementType","logical"),
                 function(object,value) { object@intraMolecular <- value; return(object) })
setReplaceMethod("negative", c("psimi25InteractionElementType","logical"),
                 function(object,value) { object@negative <- value; return(object) })
setReplaceMethod("confidenceList", c("psimi25InteractionElementType","psimi25ConfidenceListType"),
                 function(object,value) { object@confidenceList <- value; return(object) })
setReplaceMethod("parameterList", c("psimi25InteractionElementType","psimi25ParameterTypeList"),
                 function(object,value) { object@parameterList <- value; return(object) })
setReplaceMethod("imexId", c("psimi25InteractionElementType","character"),
                 function(object,value) { object@imexId <- value; return(object) })

##------------------------------------------------------------##
## entry
##------------------------------------------------------------##
setMethod("level", "psimi25Entry", function(object) object@level)
setMethod("version", "psimi25Entry", function(object) object@version)
setMethod("minorVersion", "psimi25Entry", function(object) object@minorVersion)

setReplaceMethod("level", c("psimi25Entry", "numeric"), function(object, value) {
  value <- as.integer(value)
  object@level <- value
  return(object)
})
setReplaceMethod("version", c("psimi25Entry", "numeric"), function(object, value) {
  value <- as.integer(value)
  object@version <- value
  return(object)
})
setReplaceMethod("minorVersion", c("psimi25Entry", "numeric"), function(object, value) {
  value <- as.integer(value)
  object@minorVersion <- value
  return(object)
})

##------------------------------------------------------------##
## show methods
##------------------------------------------------------------##


setMethod("show", "psimi25Interactor", function(object) {
  cat("interactor (", object@uniprotId, "):\n",
      "---------------------------------\n",
      "[ source database ]: ", object@sourceDb, "\n",
      "[ source ID ]: ", object@sourceId, "\n",
      "[ short label ]: ", object@shortLabel, "\n",
      "[ UniProt ID ]: ", object@uniprotId, "\n",
      "[ organism ]: ", paste(object@organismName, collapse=", "), "\n",
      "[ NCBI Taxonomy ID ]: ", object@taxId, "\n",
      "[ xref ]: ",nrow(xref(object)),"xrefs found, Use 'xref(x)' to see more annotation\n")
})

setMethod("show", "psimi25Interaction", function(object) {
  cat("interaction (", object@sourceId, "):\n",
      "---------------------------------\n",
      "[ source database ]:", object@sourceDb, "\n",
      "[ source experiment ID ]:", object@expSourceId, "\n",
      "[ interaction type ]:", object@interactionType, "\n",
      "[ experiment ]: pubmed", object@expPubMed, "\n",
      "[ participant ]:", object@participant, "\n",
      "[ bait ]:", object@bait, "\n",
      "[ bait UniProt ]:", object@baitUniProt, "\n",
      "[ prey ]:", object@prey, "\n",
      "[ prey UniProt ]:", object@preyUniProt, "\n",
      "[ confidence value ]:", confidenceValue(object), "\n"
      )
}
          )

setMethod("show", "psimi25InteractionEntry", function(object) {
  cat("==================================\n",
      "interaction entry (", object@releaseDate, "):\n",
      "==================================\n",
      "[ organism ]: ", paste(object@organismName, collapse=", "), "\n",
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
      "[ organism ]: ", paste(object@organismName, collapse=", "), "\n",
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
            trNodes <- names(trIds)[match(nodes(r), names(trIds))]
            trNodes[is.na(trNodes)] <- "NA"
            nodes(r) <- trNodes
            return(r)
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



setMethod("confidenceValue", "psimi25Interaction", function(x) {
   return(x@confidenceValue)
})

### interactors
setMethod("xref", "psimi25Interactor", function(object) {
  return(get("xref", object@xref))
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
setMethod("organismName", "psimi25Complex", function(x) {
  x@organismName
})

setMethod("organismName", "psimi25Interactor", function(x) {
  x@organismName
})

setMethod("taxId", signature(x="psimi25InteractionEntry"),
          function(x) x@taxId)
setReplaceMethod("taxId", signature(x="psimi25InteractionEntry", value="character"),
                 function(x, value) {
                   x@taxId <- value
                   return(x)
                   
                 })
setMethod("taxId", "psimi25Interactor", function(x) {
  x@taxId
})

setMethod("releaseDate", signature(x="psimi25InteractionEntry"),
          function(x) x@releaseDate)
setReplaceMethod("releaseDate", signature(x="psimi25InteractionEntry", value="character"),
                 function(x, value) {
                   x@releaseDate <- value
                   return(x)
                 })

setMethod("interactionType", signature(object="psimi25Interaction"),
          function(x) object@interactionType)


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

##--------------------##
## hypergraph methods
##--------------------##
setMethod("initialize",
          signature=signature(
            .Object="psimi25Hypergraph"),
          function(.Object, interactors, ...) {
            .Object@interactors <- interactors
            callNextMethod(.Object, ...)
          })

setMethod("hyperedgeNodes", "Hypergraph", function(x) {
  hyperEdges <- hyperedges(x)
  res <- lapply(hyperEdges, nodes)
  names(res) <- hyperedgeLabels(x)
  return(res)
})

setMethod("edgeLabel", "psimi25Hypergraph",function(x) {
  hyperedgeLabels(x)
})

setMethod("complexes", "psimi25Hypergraph", function(x) {
  hyperedgeNodes(x)
})

setMethod("interactors", "psimi25Hypergraph", function(x) {
  x@interactors
})

setMethod("numInteractors", "psimi25Hypergraph", function(x) {
  xit <- interactors(x)
  return(length(xit))
})

setMethod("numEdges", "psimi25Hypergraph", function(object) {
  return(length(hyperedges(object)))
})

setMethod("show", "psimi25Hypergraph", function(object){
  cat(paste("========================\n",
      "psimi25Hypergraph\n",
      "========================\n",
      "[ complexes ]: there are ", numEdges(object), " complexes (hyperedges) with ", numNodes(object) ," proteins (nodes) in the hypergraph, here are the first new ones:\n",sep=""))
  print(utils::head(complexes(object),3))
  cat("...\n")
  cat("Please use 'edgeLabel' to list only complexes names, or use 'complexes' to view all the complexes in list form\n")
})

setMethod("interactorInfo", signature(x="psimi25Hypergraph"),
          function(x) {
            interactors <- interactors(x)
            interactorInfo(interactors)
          })
setMethod("translateID", signature(r="psimi25Hypergraph"),
          function(r, to, uniprotId){
            its <- interactors(r)
            if(missing(uniprotId)) {
              return(translateID(its, to))
            }
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

setMethod("revInciMat", signature(x="matrix"), function(x) {
  hyperNodes <- rownames(x)
  hyperEdgeNames <- colnames(x)
  hEdges <- list()
  for(i in seq(along=hyperEdgeNames)) {
    hEdges[[i]] <- hyperNodes[x[,i] == 1]
  }
  names(hEdges) <- hyperEdgeNames
  hyperEdges <- lapply(hEdges, "Hyperedge")

  hg <- new("Hypergraph", nodes=hyperNodes, hyperedges=hyperEdges)
  return(hg)
})
