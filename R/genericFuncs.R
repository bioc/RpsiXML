## interactors
setGeneric("interactors", function(x) standardGeneric("interactors"))
setGeneric("availableXrefs", function(x,...) standardGeneric("availableXrefs"))

setGeneric("translateID",
           function(r, ...) standardGeneric("translateID"))
setGeneric("sourceDb",function(x) standardGeneric("sourceDb"))
setGeneric("sourceId",function(x) standardGeneric("sourceId"))

setGeneric("interactions", function(x) standardGeneric("interactions"))
setGeneric("complexes", function(x) standardGeneric("complexes"))
setGeneric("members", function(x) standardGeneric("members"))
setGeneric("fullName", function(x) standardGeneric("fullName"))

setGeneric("confidenceValue", function(x) standardGeneric("confidenceValue"))
setGeneric("interactionType", function(x) standardGeneric("interactionType"))
setGeneric("organismName", function(x) standardGeneric("organismName"))
setGeneric("taxId", function(x) standardGeneric("taxId"))
setGeneric("releaseDate", function(x) standardGeneric("releaseDate"))
setGeneric("interactionType", function(x) standardGeneric("interactionType"))
setGeneric("interactorInfo", function(x) standardGeneric("interactorInfo"))

setGeneric("parseExperiment", function(x,...)
           standardGeneric("parseExperiment"))
setGeneric("parseInteractor", function(x,...)
           standardGeneric("parseInteractor"))
setGeneric("parseComplex", function(x,...)
           standardGeneric("parseComplex"))
setGeneric("uniprot", function(x) standardGeneric("uniprot"))
