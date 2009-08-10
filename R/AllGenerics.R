##----------------------------------------------------------------------------##
## Generic functions for the RpsiXML package
##
## Author: Jitao David Zhang <j.zhang@dkfz.de>
##----------------------------------------------------------------------------##

##----------------------------------------##
## old interfaces
##----------------------------------------##
## interactors
setGeneric("interactors", function(x) standardGeneric("interactors"))
setGeneric("interactors<-", function(x, value) standardGeneric("interactors<-"))

setGeneric("availableXrefs", function(x,...) standardGeneric("availableXrefs"))

setGeneric("translateID",
           function(r, ...) standardGeneric("translateID"))
setGeneric("sourceDb",function(x) standardGeneric("sourceDb"))
setGeneric("sourceId",function(x) standardGeneric("sourceId"))

setGeneric("interactions", function(x) standardGeneric("interactions"))
setGeneric("interactions<-", function(x, value) standardGeneric("interactions<-"))
setGeneric("complexes", function(x) standardGeneric("complexes"))
setGeneric("members", function(x) standardGeneric("members"))
setGeneric("fullName", function(x) standardGeneric("fullName"))

setGeneric("interactionType", function(x) standardGeneric("interactionType"))
setGeneric("organismName", function(x) standardGeneric("organismName"))
setGeneric("organismName<-", function(x, value) standardGeneric("organismName<-"))

setGeneric("taxId", function(x) standardGeneric("taxId"))
setGeneric("taxId<-", function(x, value) standardGeneric("taxId<-"))

setGeneric("releaseDate", function(x) standardGeneric("releaseDate"))
setGeneric("releaseDate<-", function(x, value) standardGeneric("releaseDate<-"))

setGeneric("interactionType", function(x) standardGeneric("interactionType"))
setGeneric("interactorInfo", function(x) standardGeneric("interactorInfo"))

setGeneric("parseExperiment", function(x,...) standardGeneric("parseExperiment"))
setGeneric("parseInteractor", function(x,...) standardGeneric("parseInteractor"))
setGeneric("parseComplex", function(x,...) standardGeneric("parseComplex"))
setGeneric("uniprot", function(x) standardGeneric("uniprot"))

## count methods
setGeneric("numInteractors", function(x) standardGeneric("numInteractors"))
setGeneric("numInteractions", function(x) standardGeneric("numInteractions"))

## edge/node methods
setGeneric("edgeLabel", function(x,...) standardGeneric("edgeLabel"))
setGeneric("hyperedgeNodes", function(x,...) standardGeneric("hyperedgeNodes"))

## accessors
setGeneric("accession", function(x) standardGeneric("accession"))
setGeneric("bait", function(x,...) standardGeneric("bait"))
setGeneric("baitAccession", function(x,...) standardGeneric("baitAccession"))
setGeneric("prey", function(x,...) standardGeneric("prey"))
setGeneric("preyAccession", function(x,...) standardGeneric("preyAccession"))
setGeneric("participant", function(x,...) standardGeneric("participant"))
setGeneric("inhibitor", function(x,...) standardGeneric("inhibitor"))
setGeneric("neutralComponent", function(x,...) standardGeneric("neutralComponent"))
setGeneric("pubmedID", function(x,...) standardGeneric("pubmedID"))
setGeneric("confidenceValue", function(x,...) standardGeneric("confidenceValue"))
setGeneric("xref", function(x,...) standardGeneric("xref"))

setGeneric("complexMembers", function(x,...) standardGeneric("complexMembers"))
setGeneric("complexName", function(x,...) standardGeneric("complexName"))
setGeneric("complexAttributes", function(x,...) standardGeneric("complexAttributes"))

setGeneric("revInciMat", function(x,...) standardGeneric("revInciMat"))

##----------------------------------------##
## constructor methods
##----------------------------------------##


setGeneric("typedList", function(..., type) standardGeneric("typedList"))
setGeneric("psimi25NamesType", function(shortLabel, fullName, alias) standardGeneric("psimi25NamesType"))
setGeneric("psimi25Attribute", function(iValue, name, nameAc) standardGeneric("psimi25Attribute"))
setGeneric("psimi25AttributeListType", function(typedList) standardGeneric("psimi25AttributeListType"))
setGeneric("psimi25DbReferenceType",
           function(typedList, db, dbAc, id, secondary, version, refType, refTypeAc)
           standardGeneric("psimi25DbReferenceType"))
setGeneric("psimi25XrefType", function(primaryRef, secondaryRef) standardGeneric("psimi25XrefType"))
setGeneric("psimi25AvailabilityType", function(iValue, id) standardGeneric("psimi25AvailabilityType"))

##----------------------------------------##
## accessors (internal)
##----------------------------------------##
setGeneric("name", function(object) standardGeneric("name"))
setGeneric("name<-", function(object, value) standardGeneric("name<-"))
setGeneric("nameAc", function(object) standardGeneric("nameAc"))
setGeneric("nameAc<-", function(object, value) standardGeneric("nameAc<-"))
setGeneric("iValue", function(object) standardGeneric("iValue")) ## inherited value (.Data)
setGeneric("iValue<-", function(object, value) standardGeneric("iValue<-")) ## inherited value (.Data)
setGeneric("id", function(object) standardGeneric("id"))
setGeneric("id<-", function(object, value) standardGeneric("id<-"))
