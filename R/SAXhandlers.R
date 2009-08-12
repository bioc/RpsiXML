##----------------------------------------------------------------------------##
##
## Handler/Callbacks for SAX (Simple API for XML) parse
##
## Jitao David Zhang <j.zhang@dkfz.de>, http://www.NextBioMotif.com
## Copyrighted 2009, released, see '../LICENSE' for the license
##
##
## Description: Handlers to parse PSI-MI XML 2.5 files with SAX
##
##----------------------------------------------------------------------------##


##----------------------------------------##
## auxilliary files
##----------------------------------------##
null2na <- function (x) 
{
  if (is.null(x) || length(x) == 0) 
    x <- as.character(NA)
  else x <- unique(unlist(x))
  x
}

xmlValueNullsafe <- function(x) {
  if (is.null(x) || length(x) == 0)
    return(as.character(NA))

  v <- xmlValue(x)
  if (is.null(v) || length(v) == 0)
    v <- as.character(NA)
  else
    v <- unique(unlist(v))
  return(v)
}

getNamedElement <- function (vector, name) {
    if (name %in% names(vector)) 
        return(vector[[name]])
    else return(as.character(NA))
}

xmlApplyTyped <- function(X, FUN, ...) {
  res <- xmlApply(X, FUN, ...)
  res <- as(res, "typedList")
  return(res)
}

##----------------------------------------##
## handlers
##----------------------------------------##
psimi25NamesTypeHandler <- function(node, attrs) {
  if(is.null(node))
    return(GC_mockNamesType)
  child <- xmlChildren(node)
  shortLabel <- xmlValueNullsafe(child$shortLabel)
  fullName <- xmlValueNullsafe(child$fullName)
  ## missing alias, TODO
  
  names <- psimi25NamesType(shortLabel = shortLabel,
                            fullName = fullName)
  return(names)
}

psimi25DbReferenceTypeHandler <- function(node) {
  att <- xmlAttrs(node)
  ## missing attribute list TODO
  dbr <- psimi25DbReferenceType(db=getNamedElement(att, "db"),
                                id=getNamedElement(att, "id"),
                                secondary=getNamedElement(att, "secondary"),
                                dbAc=getNamedElement(att, "dbAc"),
                                refType=getNamedElement(att, "refType"),
                                refTypeAc=getNamedElement(att, "refTypeAc"))
                                
  return(dbr)
}

psimi25AttributeHandler <- function(node) {
  att <- xmlAttrs(node)
  attribute <- psimi25Attribute(iValue=xmlValueNullsafe(node),
                                name = getNamedElement(att, "name"),
                                nameAc = getNamedElement(att, "nameAc"))
  return(attribute)
}
psimi25AttributeListTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockAttributeListType)
  alt <- psimi25AttributeListType(xmlApplyTyped(node, psimi25AttributeHandler))
  return(alt)
}

psimi25AvailabilityTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockAvailabilityType)
  att <- xmlAttrs(node)
  ava <- psimi25AvailabilityType(iValue = xmlValueNullsafe(node),
                             id = getNamedElement(att, "id"))
  return(ava)
}

psimi25AvailabilityTypeListHandler <- function(node) {
  ava <- xmlApplyTyped(node, psimi25AvailabilityTypeHandler)
  return(ava)
}

psimi25BibrefTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockBibrefType)
  
  child <- xmlChildren(node)
  xrefs <- psimi25XrefTypeHandler(child$xref)
  attl <- psimi25AttributeListTypeHandler(child$attributeList)
  bib <- psimi25BibrefType(xref=xrefs,
                           attributeList=attl)
  return(bib)
}


psimi25CvTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockCvType)
  child <- xmlChildren(node)
  names <- psimi25NamesTypeHandler(child$names)
  xref <- psimi25XrefTypeHandler(child$xref)
  cvt <- psimi25CvType(name=names,
                       xref=xref)
  return(cvt)
}

psimi25OpenCvTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockOpenCvType)
  child <- xmlChildren(node)
  names <- psimi25NamesTypeHandler(child$names)
  xref <- psimi25XrefTypeHandler(child$xref)
  att <- psimi25AttributeListTypeHandler(child$attributeList)
  oct <- psimi25OpenCvType(name=name,
                           xref=xref,
                           attributeList=att)
  return(oct)
}

psimi25ExperimentTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockExperimentType)
  
  child <- xmlChildren(node)
  att <- xmlAttrs(node)
  
  names <- psimi25NamesTypeHandler(child$names)
  id <- as.integer(getNamedElement(att, "id"))
  attList <- psimi25AttributeListTypeHandler(child$attributeList)
  bibref <- psimi25BibrefTypeHandler(child$bibref)
  xref <- psimi25XrefTypeHandler(child$xref)
  idm <- psimi25CvTypeHandler(child$interactionDetectionMethod)
  et <- psimi25ExperimentType(name=names,
                              attributeList=attList,
                              id=id,
                              bibref=bibref,
                              interactionDetectionMethod=idm,
                              xref=xref)
  return(et)
}

psimi25DbReferenceTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockDbReferenceType)
  attrs <- xmlAttrs(node)
  ## Attribute list TODO
  dbr <- psimi25DbReferenceType(db=attrs["db"],
                         dbAc=attrs["dbAc"],
                         id=attrs["id"],
                         secondary=attrs["secondary"],
                         version=attrs["version"],
                         refType=attrs["refType"],
                         refTypeAc=attrs["refTypeAc"])
  return(dbr)
  
}
psimi25XrefTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockXrefType)
 child <- xmlChildren(node)
 pr <- psimi25DbReferenceTypeHandler(child$primaryRef)
 isSecondaryRef <- names(child) == "seoncdaryRef"
 if(sum(isSecondaryRef)==0) {
   sr <- new("psimi25DbReferenceTypeList")
 } else {
   sr <- xmlApplyTyped(child[isSecondaryRef], psimi25DbReferenceTypeHandler)
 }
 
 xr <- psimi25XrefType(primaryRef=pr,
                       secondaryRef=sr)
 return(xr)
}

psimi25SourceHandler <- function(node) {
  child <- xmlChildren(node)
  attr <- xmlAttrs(node)
  
  name <- psimi25NamesTypeHandler(child$names)
  xref <- psimi25XrefTypeHandler(child$xref)
  bibref <- psimi25BibrefTypeHandler(child$bibref)
  attL <- psimi25AttributeListTypeHandler(child$attributeLite)

  release <- getNamedElement(attr, "release")
  releaseDate <- getNamedElement(attr, "releaseDate")

  sour <- psimi25Source(name=name, xref=xref, bibref=bibref, attributeList=attL,
                        release=release, releaseDate=releaseDate)
  return(sour)
}

psimi25ExperimentListHandler <- function(node) {
  res <- xmlApply(node, psimi25ExperimentTypeHandler)
  re <- as(res, "psimi25ExperimentTypeList")
  return(res)
}

psimi25BioSourceTypeHandler <- function(node) {
  att <- xmlAttrs(node)
  child <- xmlChildren(node)

  name <- psimi25NamesTypeHandler(child$name)
  cellType <- psimi25OpenCvTypeHandler(child$cellType)
  compartment <- psimi25OpenCvTypeHandler(child$compartment)
  tissue <- psimi25OpenCvTypeHandler(child$tissue)
  ncbiTaxId <- suppressWarnings(as.integer(getNamedElement(att, "ncbiTaxId")))
  
  bst <- psimi25BioSourceType(name=name,
                              cellType=cellType,
                              compartment = compartment,
                              tissue=tissue,
                              ncbiTaxId=ncbiTaxId)
  return(bst)
}

psimi25InteractorElementTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockInteractorElementType)
  
  att <- xmlAttrs(node)
  child <- xmlChildren(node)

  id <- suppressWarnings(as.integer(getNamedElement(att, "id")))
  name <- psimi25NamesTypeHandler(child$name)
  xref <- psimi25XrefTypeHandler(child$xref)
  interactorTypes <- psimi25CvTypeHandler(child$interactorType)
  organism <- psimi25BioSourceTypeHandler(child$organism) ## NOT COMPLETE: ncbiTaxId DAZU! TODO. NOT SURE, SINCE TAXID IS CONTAINED
  
  sequence <- child$sequence
  if(!is.null(sequence)) {
    sequence <- xmlValue(sequence)
  } else {
    sequence <- as.character(NA)
  }
  att <- psimi25AttributeListTypeHandler(child$attributeList)

  iet <- psimi25InteractorElementType(name=name, xref=xref,
                                      attributeList=att,
                                      interactorType=interactorTypes,
                                      organism=organism,
                                      sequence=sequence,
                                      id=id)
  return(iet)

}

psimi25InteractorElementTypeListHandler <- function(node) {
  if(is.null(node))
    return(GC_mockInteractorElementTypeList)
  res <- xmlApply(node, psimi25InteractorElementTypeHandler)
  re <- as(res, "psimi25InteractorElementTypeList")
  return(res)
}

psimi25ExperimentListHandler <- function(node) {
  if(is.null(node))
    return(GC_mockExperimentList)
  
  child <- xmlChildren(node)

  eRef <- suppressWarnings(as.integer(xmlValueNullsafe(child$experimentRef)))
  eDes <- psimi25ExperimentTypeHandler(child$experimentDescription)
  ela <- psimi25ExperimentList(experimentRef=eRef,
                               experimentDescription=eDes)
  return(ela)
}

psimi25InferredInteractionParticpantAtomHandler <- function(node) {
  child <- xmlChildren(node)

  participantRef <- quiteAsInteger(xmlValueNullsafe(node$participantRef))
  participantFeatureRef <- quiteAsInteger(xmlValueNullsafe(node$participantFeatureRef))

  obj <- new("psimi25InferredInteractionAtomParticipantAtom",
             participantRef=participantRef,
             participantFeatureRef=participantFeatureRef)
  return(obj)
}

psimi25ExperimentRefListTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockExperimentRefListType)
  
  child <- xmlChildren(node)
  val <- quiteAsInteger(xmlApply(child, xmlValueNullsafe))
  obj <- psimi25ExperimentRefListType(object=obj)
  return(obj)
}

psimi25InferredInteractionHandler <- function(node) {
  child <- xmlChildren(node)

  isParticipant <- names(child) == "participant"
  part <- tlapply(child[isParticipant], psimi25InferredInteractionParticpantAtomHandler)
  expRefList <- psimi25ExperimentRefListTypeHandler(child$experimentRefList)

  obj <- new("psimi25InferredInteraction",
             participant=part,
             experimentRefList=expRefList)
  return(obj)
  
}
psimi25CvExperimentRefsHandler <- function(node) {
  if(is.null(node))
    return(GC_mockCvExperimentRefs)
  cv <- psimi25CvTypeHandler(node)
  expRefList <- psimi25ExperimentRefListTypeHandler(node[["experimentRefList"]])
  obj <- psimi25CvExperimentRefs(cv = cv,
                                 experimentRefList = expRefList)
  return(obj)
}

psimi25CvExperimentRefsListHandler <- function(node) {
  if(is.null(node))
    return(GC_mockCvExperimentRefsList)

  res <- xmlApplyTyped(node, psimi25CvExperimentRefsHandler)
  res <- psimi25CvExperimentRefsList(res)
  return(res)
}

psimi25CvExperimentInteractorHandler <- function(node) {
}

psimi25FeatureElementTypeHandler <- function(node) {
  if(is.null(node))
    return(GC_mockFeatureElementType)
}

psimi25FeatureElementTypeListHandler <- function(node) {
  if(is.null(node))
    return(GC_mockFeatureElementTypeList)
  res <- xmlApplyTyped(node, psimi25FeatureElementTypeHandler)
  return(res)
}

psimi25InferredInteractionListHandler <- function(node) {
  
}

psimi25ConfidenceTypeHandler <- function(node) {

}

psimi25ConfidenceListTypeHandler <- function(node) {

}

psimi25ParameterTypeHandler <- function(node) {

}

psimi25ParameterTypeListHandler <- function(node) {

}

psimi25HostOrganismHandler <- function(node) {
  if(is.null(node))
    return(GC_mockHostOrganism)

  children <- xmlChildren(node)
  bst <- psimi25BioSourceTypeHandler(node)
  erf <- psimi25ExperimentRefListTypeHandler(children$experimentRefList)
  obj <- psimi25HostOrganism(bioSourceType=bst,
                             experimentRefList=erf)
  return(obj)
}

psimi25HostOrganismListHandler <- function(node) {
  if(is.null(node))
    return(GC_mockHostOrganismList)
  res <- xmlApplyTyped(node, psimi25HostOrganismHandler)
  return(res)
}

psimi25ParticipantTypeHandler <- function(node) {
  attr <- xmlAttrs(node)
  child <- xmlChildren(node)

  id <- quiteAsInteger(getNamedElement(attr,"id"))
  name <- psimi25NamesTypeHandler(child$names)
  xref <- psimi25XrefTypeHandler(child$xref)
  interactorRef <- quiteAsInteger(xmlValueNullsafe(child$interactorRef))
  ## ATTENTION: not child$interactor, otherwise it uses partial matching!
  intera <- psimi25InteractorElementTypeHandler(child[["interactor"]])
  interactionRef <- quiteAsInteger(xmlValueNullsafe(child$interactionRef))
  pim <- psimi25CvExperimentRefsListHandler(child$participantIdentificationMethodList)  ## NOT FINISHED
  br <- psimi25CvTypeHandler(child$biologicalRole)
  erl <- psimi25CvExperimentRefsListHandler(child$experimentalRoleList)
  epl <- psimi25CvExperimentRefsListHandler(child$experimentalPreparationList)
  eil <- psimi25CvExperimentInteractorHandler(child$experimentalInteractorList)
  fl <- psimi25FeatureElementTypeListHandler(child$featureList)
  hol <- psimi25HostOrganismListHandler(child$hostOrganismList)
  cl <- psimi25ConfidenceListTypeHandler(child$confidenceList)
  pl <- psimi25ParameterTypeListHandler(child$parameterList)

  pt <- new("psimi25ParticipantType",
            interactorRef=interactorRef,
            interactor=intera,
            interactionRef=interactionRef,
            participantIdentificationMethodList=pim,
            biologicalRole=br,
            experimentalRoleList=erl,
            experimentalPreparationList=epl,
            experimentalInteractorList=eil,
            featureList=fl,
            hostOrganismList=hol,
            confidenceList=cl,
            parameterList=pl,
            id=id)

  return(pt)
}

psimi25ParticipantTypeListHandler <- function(node) {
  res <- xmlApplyTyped(node, psimi25ParticipantTypeHandler)
  return(res)
}



getLogicalXmlValue <- function(node, default=FALSE) {
  if(is.null(node))
    return(default)
  res <- as.logical(xmlValue(res))
  return(res)
}
psimi25InteractionElementTypeHandler <- function(node) {
  att <- xmlAttrs(node)
  child <- xmlChildren(node)

  id <- suppressWarnings(as.integer(getNamedElement(att, "id")))
  imexId <- getNamedElement(att, "id")
  name <- psimi25NamesTypeHandler(child$names)
  xref <- psimi25XrefTypeHandler(child$xref)
  availRef <- child$availabilityRef
  if(is.null(availRef)) {
    availRef <- as.integer(NA)
  } else {
    availRef <- suppressWarnings(as.integer(availRef))
  }
  availability <- psimi25AvailabilityTypeHandler(child$availability)
  experimentList <- psimi25ExperimentListHandler(child$experimentList)
  participantList <- psimi25ParticipantTypeListHandler(child$participantList)
  inferredInteractionList <- psimi25InferredInteractionListHandler(child$inferredInteractionList)

  isInteractionType <- names(child) == "interactionType"
  if(any(isInteractionType))
    interactionType <- tlapply(child[isInteractionType], psimi25CvTypeHandler)

  modelled <- getLogicalXmlValue(child$modelled)
  intraMolecular <- getLogicalXmlValue(child$intraMolecular)
  negative <- getLogicalXmlValue(child$negative)

  confidenceList <- psimi25ConfidenceListTypeHandler(child$confidenceList)
  parameterList <- psimi25ParameterTypeListHandler(child$parameterList)
  att <- psimi25AttributeListTypeHandler(child$attributeList)

  obj <- new("psimi25InteractionElementType",
             name=name, xref=xref,
             attributeList=att,
             availabilityRef=availRef, availability=availability,
             experimentList=experimentList, participantList=participantList,
             inferredInteractionList=inferredInteractionList,
             interactionType=interactionType, modelled=modelled,
             intraMolecular=intraMolecular, negative=negative,
             confidenceList=confidenceList, parameterList=parameterList,
             imexId=imexId, id=id)
  return(obj)
  browser()
}
##----------------------------------------##
## source for SAX
##----------------------------------------##
## source
##   names
##     shortLabel
##     fullName
##     alias
##   bibref
##     xref
##       primaryRef
##       secondaryRef
##   xref
##       primaryRef
##       secondaryRef
##   attributeList
##       attribute

##psimi25SourceHandler <- function() {
##  sourceList <- list()
##  
##  curEL <- 1
##  curPIR <- NULL
##  ## inN: in names
##  ## inB: in bibref
##  ## inX: in xref
##  ## inA: in attributeList
##  inN <- inB <- inX <- inA <- FALSE
##  ## inSL: in shortLabel
##  ## inFN: in full name
##  ## inAL: in alias
##  ## inPR: in primaryRef
##  ## inSR: in secondaryRef
##  ## inAT : in Attribute
##  inSL <- inFN <- inAL <- inPR <- inSR <- inAT <- FALSE
##
##  startElement = function(x, atts, ...) {
##    if (x == "names") 
##      {
##        sourceList[[ curEL ]] <<- list()
##        inN <<- TRUE
##      }
##    else if (x == "bibref") inB <<- TRUE
##    else if (x == "xref") inX <<- TRUE
##    else if (x == "attributeList") inA <<- TRUE
##    else if (x == "shortLabel") inSL <<- TRUE
##    else if (x == "fullName") inFN <<- TRUE
##    else if (x == "primaryRef") inPR <<- TRUE
##    else if (x == "secondaryRef") inSR <<- TRUE
##    else if (x == "attribute") inAT <<- TRUE
##  }
##  endElement = function(x, ...) {
##    if (x == "names") 
##      {
##        inN <<- FALSE
##        curEL <<- curEL + 1
##      }
##    else if (x == "bibref") inB <<- FALSE
##    else if (x == "xref") inX <<- FALSE
##    else if (x == "attributeList") inA <<- FALSE
##    else if (x == "shortLabel") inSL <<- FALSE
##    else if (x == "fullName") inFN <<- FALSE
##    else if (x == "primaryRef") inPR <<- FALSE
##    else if (x == "secondaryRef") inSR <<- FALSE
##    else if (x == "attribute") inAT <<- FALSE
##  }
##  
##  text = function(x, atts, ...) {
##  }  
##  dump = function() {
##  }
##  list(startElement= startElement, 
##       text=text, 
##       endElement=endElement,
##       dump=dump )
##}
##
##

##----------------------------------------##
## Handlers inherited from Rintact
##----------------------------------------##
# iListHandler for interactionList in psi25xml data
# adapted from Rintact
# interactionList
#  interaction
#   experimentList
#    experimentRef
#  participantList
#   proteinParticipant
#    proteinInteractorRef
#    role
#   interactionType
#    names
#     shortLabel
#    xref
#     primaryRef

iListHandler <- function() {
  iList <- list()
  pairvec <- ""
  curEL <- 1
  curPIR <- NULL
  inI <- inEL <- inER <- inPL <- inPP <- inPIR <- FALSE
  inRole <- inIT <- inNA <- inSL <- inXR <- inPR <- FALSE

  startElement = function(x, atts, ...) {
    if (x == "interaction") 
      {
        iList[[ curEL ]] <<- list()
        inI <<- TRUE
      }
    else if (x == "experimentList") inEL <<- TRUE
    else if (x == "experimentRef") inER <<- TRUE
    else if (x == "participantList") inPL <<- TRUE
    else if (x == "proteinInteractorRef") 
      {
        curPIR <<- atts
        if (nchar(pairvec[curEL])==0)
          pairvec[curEL] <<- paste(pairvec[curEL], curPIR, sep="")
        else  pairvec[curEL] <<- paste(pairvec[curEL], curPIR, sep=":")
        inPIR <<- TRUE
      }
    else if (x == "role") inRole <<- TRUE
    else if (x == "interactionType") inIT <<- TRUE
    else if (x == "names") inNA <<- TRUE
    else if (x == "shortLabel") inSL <<- TRUE
    else if (x == "xref") inXR <<- TRUE
    else if (x == "primaryRef") inPR <<- TRUE
  }
  endElement = function(x, ...) {
    if (x == "interaction") 
      {
        inI <<- FALSE
        curEL <<- curEL + 1
        pairvec <<- c(pairvec,"")
      }
    else if (x == "experimentList") inEL <<- FALSE
    else if (x == "experimentRef") inER <<- FALSE
    else if (x == "participantList") inPL <<- FALSE
    else if (x == "proteinInteractorRef") inPIR <<- FALSE
    else if (x == "role") inRole <<- FALSE
    else if (x == "interactionType") inIT <<- FALSE
    else if (x == "names") inNA <<- FALSE
    else if (x == "shortLabel") inSL <<- FALSE
    else if (x == "xref") inXR <<- FALSE
    else if (x == "primaryRef") inPR <<- FALSE
  }
  text = function(x, atts, ...) {
    if (inRole)
      iList[[curEL]][[curPIR]] <<- x
  }  
  dump = function() {
    names(iList) <<- pairvec[1:length(iList)]
    return(iList)
  }
  list(startElement= startElement, 
       text=text, 
       endElement=endElement,
       dump=dump )
}

#experimentList for experimentList of psi25xml data
#  experimentDescription[id]
#    shortLabel
#    fullName
#    hostOrganism
#      shortLabel
#    interactionDetection
#      shortLabel
#      primaryRef
#    participantDetection
#      shortLabel
#      primaryRef

eListHandler <- function() {
  eList <- list()
  inED <- inOrg <- inID <- inPD <- inNA <- inSL <- FALSE
  inPR <- inSR <- FALSE
  curEL <- 1
  
  startElement = function(x, atts, ...) {
    if (x == "experimentDescription")
      {
        eList[[ curEL <<- atts["id"] ]] <<- list()
        inED <<- TRUE
      }
    else if (x == "names") inNA <<- TRUE
    else if (x == "shortLabel") inSL <<- TRUE
    else if (x == "hostOrganism") inOrg <<- TRUE
    else if (x == "interactionDetection") 
      {
        eList[[ curEL ]][["interactionDetection"]] <<- list()
        inID <<- TRUE
      }
    else if (x == "participantDetection") 
      {
        eList[[ curEL ]][["participantDetection"]] <<- list()
        inPD <<- TRUE
      }
    else if (x == "primaryRef") 
      {
        inPR <<- TRUE
        if (inID) 
          eList[[ curEL ]][["interactionDetection"]][["primRef"]] <<- atts
        else if (inPD) 
          eList[[ curEL ]][["participantDetection"]][["primRef"]] <<- atts
      }
  }
  endElement = function(x, ...) {
    if (x == "names") inNA <<- FALSE
    else if (x == "shortLabel") inSL <<- FALSE
    else if (x == "hostOrganism") inOrg <<- FALSE
    else if (x == "interactionDetection") inID <<- FALSE
    else if (x == "participantDetection") inPD <<- FALSE
    else if (x == "primaryRef") inPR <<- FALSE
    else if (x == "experimentDescription") inED <<- FALSE
  }
  text = function(x, atts, ...) {
    if (inOrg & inSL) 
      eList[[ curEL ]][["orgShort"]] <<- x
    else if (inID & inSL & inPR) 
      eList[[ curEL ]][["interactionDetection"]][["primaryRef"]] <<- x
    else if (inID & inSL & !inPR) 
      eList[[ curEL ]][["interactionDetection"]][["shortLabel"]] <<- x
    else if (inPD & inSL & !inPR) 
      eList[[ curEL ]][["participantDetection"]][["shortLabel"]] <<- x
    else if (inED & inNA & inSL & !inOrg & !inID & !inPD)
      eList[[ curEL ]][["expShortLabel"]] <<- x
  }
  
  dump = function() return(eList)
  list( startElement= startElement, 
       text=text, 
       endElement=endElement,
       dump=dump )
}
