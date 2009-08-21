##----------------------------------------------------------------------------##
## Class definitions of RpsiXML package
##
## Author: Jitao David Zhang
## Description: Data structures of RpsiXML package
##              See documentation and help pages for details
##              The PSI-MI 2.5 schema: http://psidev.sourceforge.net/mi/rel25/doc/
##----------------------------------------------------------------------------##

##----------------------------------------##
## low-level data structure
##----------------------------------------##
##validityTypedList <- function(object) {
##  if(length(object)==0) {
##    ##    warning("Empty object! It is not possible to determine type sanity\n")
##    return(TRUE)
##  }
##  
##  classes <- lapply(object, class)
##  uClass <- unique(classes)
##
##  if(length(uClass)==1) {
##    val <- TRUE
##    expType <- object@type
##    if(length(expType)>1 || !is.na(expType)) {
##      if(!all(uClass[[1]] == expType)) {
##        val <- paste(expType, " Type(s) expected, but got ", uClass[[1]], " type (s)\n", sep="")
##      }
##    }
##  } else {
##    val <- "Hetero. types detected\n"
##  }
##  return(val)
##}
##
##setClass("typedList",
##         representation(type="character"),
##         prototype=prototype(type=as.character(NA)),
##         contains="list",
##         validity=validityTypedList)
##


##------------------------------------------------------------##
## PSI-MI 25 Elementary parent classes
## 24 complex types (R name, wcw. with convenient wrapper) in dependence-order
## 1.  namesType (psimi25NamesType)
## 2.  attributeListType (psimi25AttributeListType)
## 3.  availabilityType (psimi25AvailabilityType)
## 4.  dbReferenceType (psimi25DbRereferenceType)
## 5.  xrefType (psimi25XrefType)
## 6.  bibrefType (psimi25BibrefType)
## 7.  cvType (psimi25CvType, wcw. psimi25CvTypeList)
## 8.  bioSourceType (psimi25BioSourcType, wcw. psimi25BioSourceTypeList)
## 9.  openCvType (psimi25OpenCvType, wcw. psimi25OpenCvTypelist)
## 10.  experimentRefListType
## 11. confidenceType (psimi25ConfidenceType)
## 12. confidenceListType (psimi25ConfidenceListType)
## 13. interactorElementType (psimi25InteractorElementType)
## 14. experimentType (psimi25ExperimentType)
## 15. fullNameType (psimi25FullNameType)
## 16. labelType (psimi25LabelType)
## 17. parameterType (psimi25ParameterType)
## 18. intervalType (psimi25IntervalType)
## 19. positionType (psimi25PositionType)
## 20. baseLocationType (psimi25BaseLocationType) --> needs testing
## 21. featureElementType (psimi25FeatureElementType)
## 22. participantType (psimi25PariticipantType)
## 23. interactionElementType (psimi25InteractionElementType)
##------------------------------------------------------------##

##--------------------##
## namesType
##--------------------##
setClass("psimi25NamesAlias",
         representation(typeAc="character",
                        type="character"),
         contains="character",
         prototype=prototype(as.character(NA),
                    type=as.character(NA),
                    typeAc=as.character(NA)))

setClass("psimi25NamesAliasList",
          contains ="list",
          prototype=list())

setClass("psimi25NamesType",
         representation(shortLabel="character",
                        fullName="character",
                        alias="psimi25NamesAliasList"),
         prototype=prototype(shortLabel=as.character(NA),
           fullName=as.character(NA)
         ))
GC_mockNamesType <- new("psimi25NamesType")

##--------------------##
## attributeListType
##--------------------##
setClass("psimi25Attribute",
         representation(name="character", ## required
                        nameAc="character"),
         prototype=prototype(name=as.character(NA), nameAc=as.character(NA)),
         contains="character")

setClass("psimi25AttributeListType",
         prototype=list(),
         contains="list")
GC_mockAttributeListType <- new("psimi25AttributeListType")

##--------------------##
## availabilityType
##--------------------##
setClass("psimi25AvailabilityType",
         representation(id="integer"), 
         contains="character")

setClass("psimi25AvailabilityTypeList",
         prototype=list(),
         contains="list")

GC_mockAvailabilityType <- new("psimi25AvailabilityType")
GC_mockAvailabilityTypeList <- new("psimi25AvailabilityTypeList")

##--------------------##
## dbReferenceType
##--------------------##
setClass("psimi25DbReferenceType",
         representation(db="character", ## required
                        dbAc="character",
                        id="character", ## required
                        secondary="character",
                        version="character",
                        refType="character",
                        refTypeAc="character"),
         prototype=prototype(list(),
           db=as.character(NA),dbAc=as.character(NA),id=as.character(NA),
           secondary=as.character(NA), version=as.character(NA), refType=as.character(NA),
           refTypeAc=as.character(NA)),
         contains="list")

GC_mockDbReferenceType <- new("psimi25DbReferenceType")

setClass("psimi25DbReferenceTypeList",
         prototype=list(),
         contains="list")

##--------------------##
## xrefType
##--------------------##
setClass("psimi25XrefType",
         representation(primaryRef="psimi25DbReferenceType",
                        secondaryRef="psimi25DbReferenceTypeList"),
         validity=function(object) {
##           length(object@primaryRef)==1
         })

##--------------------##
## bibrefType
##--------------------##
GC_mockXrefType <- new("psimi25XrefType")
setClass("psimi25BibrefType",
         representation(xref="psimi25XrefType",
                        attributeList="psimi25AttributeListType"), ## ONLY one of the xref/attributelist is allowed
         validity=function(object) {
           attLength <- length(object@attributeList)
           newXref <- !identical(GC_mockXrefType, length(object@xref@primaryRef) == 1)
           if (attLength && newXref)
             return("Choice between xref and attributeList in psimi25XrefType!\n")
           return(TRUE)
         }## only ONE of xref/attributeList
         )
GC_mockBibrefType <- new("psimi25BibrefType")

##--------------------##
## cv and opencv
##--------------------##
#### common combinations
setClass("psimi25CommonNameRef",
         representation(name="psimi25NamesType",
                        xref="psimi25XrefType"),
         prototype=prototype(name=new("psimi25NamesType"),
           xref=new("psimi25XrefType"))
         )

setClass("psimi25CommonNameRefAttr",
         representation(attributeList="psimi25AttributeListType"),
         contains="psimi25CommonNameRef"
         )
setClass("psimi25CvType",
         contains="psimi25CommonNameRef")

setClass("psimi25CvTypeList",
         prototype=list(),
         contains="list")

setClass("psimi25OpenCvType",
         contains="psimi25CommonNameRefAttr")

setClass("psimi25OpenCvTypeList",
         prototype=list(),
         contains="list")

GC_mockCvType <- new("psimi25CvType")
GC_mockOpenCvType <- new("psimi25OpenCvType")
GC_mockCvTypeList <- new("psimi25CvTypeList")
##--------------------##
## bioSourceType
##--------------------##
setClass("psimi25BioSourceType",
         representation(name="psimi25NamesType",
                        cellType="psimi25OpenCvType",
                        compartment="psimi25OpenCvType",
                        tissue="psimi25OpenCvType",
                        ncbiTaxId="integer")
         )
GC_mockBioSourceType <- new("psimi25BioSourceType")
setClass("psimi25BioSourceTypeList",
         prototype=list(),
         contains="list")


##--------------------##
## experimentRefListType
##--------------------##
setClass("psimi25ExperimentRef",
         contains="integer")
setClass("psimi25ExperimentRefListType",
         prototype=list(),
         contains="list")
GC_mockExperimentRefListType <- new("psimi25ExperimentRefListType")

##----------------------------------------##
## confidenceType/confidenceListType
##----------------------------------------##
setClass("psimi25ConfidenceType",
         representation(unit="psimi25OpenCvType",
                        value="character"),
         validity=function(object) {
           length(object@value) == 1
         }
         )
GC_mockConfidenceType <- new("psimi25ConfidenceType")

setClass("psimi25Confidence",
         representation(experimentRefList="psimi25ExperimentRefListType"),
         contains="psimi25ConfidenceType")
GC_mockConfidence <- new("psimi25Confidence")

setClass("psimi25ConfidenceListType",
         prototype=list(),
         contains="list")
GC_mockConfidenceListType <- new("psimi25ConfidenceListType")

##--------------------##
## interactorElementType
##--------------------##
## Attention: since 'names' is keyword in 'representation' function, the slot is renamed as 'name'
setClass("psimi25InteractorElementType",
         representation(interactorType="psimi25CvType",
                        organism="psimi25BioSourceType",
                        sequence="character", ## will be replaced by BioString! TODO
                        id="integer"),
         contains="psimi25CommonNameRefAttr")
GC_mockInteractorElementType <- new("psimi25InteractorElementType")

setClass("psimi25InteractorElementTypeList",
         prototype=list(),
         contains="list")
GC_mockInteractorElementTypeList <- new("psimi25InteractorElementTypeList")

##--------------------##
## experimentType
##--------------------##
## Attention: since 'names' is keyword in 'representation' function, the slot is renamed as 'name'
setClass("psimi25ExperimentType",
         representation(bibref="psimi25BibrefType",
                        hostOrganismList="psimi25BioSourceTypeList",
                        interactionDetectionMethod="psimi25CvType",
                        participantIdentificationMethod="psimi25CvType",
                        featureDetectionMethod="psimi25CvType",
                        confidenceList="psimi25ConfidenceListType",
                        id="integer"),
         contains="psimi25CommonNameRefAttr"
         )
GC_mockExperimentType <- new("psimi25ExperimentType")

setClass("psimi25ExperimentTypeList",
         prototype=list(),
         contains="list")

##--------------------##
## other simple types
##--------------------##
setClass("psimi25FullNameType", contains="character")
setClass("psimi25LabelType", contains="character")
setClass("psimi25PositionType", representation(position="integer"))
setClass("psimi25ParameterType",
         representation(term="character", ## required
                        termAc="character",
                        unit="character",
                        unitAc="character",
                        base="integer",
                        exponent="integer",
                        factor="numeric"),
         contains="numeric")
GC_mockParameterType <- new("psimi25ParameterType")
setClass("psimi25ParameterTypeList",
         contains="list",
         prototype=list())
         
GC_mockParameterTypeList <- new("psimi25ParameterTypeList")
setClass("psimi25IntervalType",
         representation(begin="integer", ## required
                        end="integer"), ## required
         )
##--------------------##
## baseLocationType
##--------------------##
setClass("psimi25BaseLocationTypeStartAtom",
         representation(startStatus="psimi25CvType",
                        begin="integer",
                        beginInterval="psimi25IntervalType")) ## only one of begin/beginInterval is allowed
setClass("psimi25BaseLocationTypeStart",
         contains="list",
         prototype=list())
         
setClass("psimi25BaseLocationTypeEndAtom",
         representation(endStatus="psimi25CvType",
                        end="integer",
                        endInterval="psimi25IntervalType")) ## only one of begin/beginInterval is allowed
setClass("psimi25BaseLocationTypeEnd",
         contains="list",
         prototype=list())


setClass("psimi25BaseLocationType",
         representation(start="psimi25BaseLocationTypeStart",
                        end="psimi25BaseLocationTypeEnd",
                        isLink="logical"),
         prototype=prototype(isLink=FALSE))

##--------------------##                        
## featureElementType
##--------------------##
## Attention: since 'names' is keyword in 'representation' function, the slot is renamed as 'name'
setClass("psimi25FeatureRangeList",
          contains="list",
         prototype=list())

setClass("psimi25FeatureElementType",
         representation(name="psimi25NamesType",
                        xref="psimi25XrefType",
                        featureType="psimi25CvType",
                        featureDetectionMethod="psimi25CvType",
                        experimentRefList="psimi25ExperimentRefListType",
                        featureRangeList="psimi25FeatureRangeList",
                        attributesList="psimi25AttributeListType"))

GC_mockFeatureElementType <- new("psimi25FeatureElementType")

setClass("psimi25FeatureElementTypeList",
         prototype=list(),
         contains="list")
GC_mockFeatureElementTypeList <- new("psimi25FeatureElementTypeList")

##--------------------##
## participantType
##--------------------##
## Attention: since 'names' is keyword in 'representation' function, the slot is renamed as 'name'
setClass("psimi25CvExperimentRefs",
         representation(experimentRefList="psimi25ExperimentRefListType"),
         contains="psimi25CvType")
GC_mockCvExperimentRefs <- new("psimi25CvExperimentRefs")

setClass("psimi25CvExperimentRefsList",
         prototype=list(),
         contains="list")
GC_mockCvExperimentRefsList <- new("psimi25CvExperimentRefsList")

setClass("psimi25ExperimentInteractor",
         representation(interactorRef="integer",
                        interactor="psimi25InteractorElementTypeList",## only one of interactorRef or interactor
                        experimentRefList="psimi25ExperimentRefListType"))
GC_mockExperimentInteractor <- new("psimi25ExperimentInteractor")

setClass("psimi25ExperimentInteractorList",
         contains="list",
         prototype=list())
GC_mockExperimentInteractorList <- new("psimi25ExperimentInteractorList")

setClass("psimi25HostOrganism",
         representation(experimentRefList="psimi25ExperimentRefListType"),
         contains="psimi25BioSourceType")
GC_mockHostOrganism <- new("psimi25HostOrganism")

setClass("psimi25HostOrganismList",
         contains="list",
         prototype=list())
GC_mockHostOrganismList <- new("psimi25HostOrganismList")

setClass("psimi25ParticipantType",
         representation(interactorRef="integer",
                        interactor="psimi25InteractorElementType",
                        interactionRef="integer", ## ONLY one of the interactorRef/interactor/interactionRef,
                        participantIdentificationMethodList="psimi25CvExperimentRefsList",
                        biologicalRole="psimi25CvType",
                        experimentalRoleList="psimi25CvExperimentRefsList",
                        experimentalPreparationList="psimi25CvExperimentRefsList",
                        experimentalInteractorList="psimi25ExperimentInteractorList",
                        featureList="psimi25FeatureElementTypeList",
                        hostOrganismList="psimi25HostOrganismList",
                        confidenceList="psimi25ConfidenceListType",
                        parameterList="psimi25ParameterTypeList",
                        id="integer"),
         contains="psimi25CommonNameRefAttr")
setClass("psimi25ParticipantTypeList",
          prototype=list(),
         contains="list")

##--------------------##
## interactionElementType
##--------------------##
## Attention: since 'names' is keyword in 'representation' function, the slot is renamed as 'name'
setClass("psimi25ExperimentList",
         representation(experimentRef="integer",
                        experimentDescription="psimi25ExperimentType" ## ONLY one of the experimentRef/experimentDescription is needed
                        ))
GC_mockExperimentList <- new("psimi25ExperimentList")
## TODO: not to be confused with the element of entry

setClass("psimi25InferredInteractionParticipantAtom",
         representation(participantRef="integer",
                        participantFeatureRef="integer") ## ONLY one of the participantRef/participantFeatureRef is needed
         )
setClass("psimi25InferredInteractionParticipant",
         contains="list",
         prototype=list(),
         validity=function(object) {
           length(object)>=2
         })

setClass("psimi25InferredInteraction",
         representation(participant="psimi25InferredInteractionParticipant",
                        experimentRefList="psimi25ExperimentRefListType")
         )
GC_mockInferredInteraction <- new("psimi25InferredInteraction")

setClass("psimi25InferredInteractionList",
         contains="list",
         prototype=list())
GC_mockInferredInteractionList <- new("psimi25InferredInteractionList")

setClass("psimi25InteractionElementType",
         representation(availabilityRef="integer",
                        availability="psimi25AvailabilityType", ## ONLY one of the availabilityRef/availability is needed
                        experimentList="psimi25ExperimentList",
                        participantList="psimi25ParticipantTypeList",
                        inferredInteractionList="psimi25InferredInteractionList",
                        interactionType="psimi25CvTypeList",
                        modelled="logical",
                        intraMolecular="logical",
                        negative="logical",
                        confidenceList="psimi25ConfidenceListType",
                        parameterList="psimi25ParameterTypeList",
                        imexId="character",
                        id="integer"),
         prototype=prototype(negative=FALSE, intraMolecular=FALSE),
         contains="psimi25CommonNameRefAttr"
         )
GC_mockInteractionElementType <- new("psimi25InteractionElementType")
setClass("psimi25InteractionElementTypeList",
         prototype=list(),
         contains="list")

##----------------------------------------------------------------------------##
## PSI-MI XML Elements
##----------------------------------------------------------------------------##
setClass("psimi25Source", ## ATTENTION: old API is affected!
         representation(bibref="psimi25BibrefType",
                        release="character",
                        releaseDate="character"),
         contains="psimi25CommonNameRefAttr"
         )

setClass("psimi25Entry",
         representation(source="psimi25Source",
                        availabilityList="psimi25AvailabilityTypeList",
                        experimentList="psimi25ExperimentTypeList",
                        interactorList="psimi25InteractorElementTypeList",
                        interactionList="psimi25InteractionElementTypeList",
                        attributeList="psimi25AttributeListType")
         )

setClass("psimi25EntrySet",
         representation(level="integer",
                        version="integer",
                        minorVersion="integer"),
         contains="list")


##----------------------------------------##
## Old PSI-MI 25 Interfaces
##----------------------------------------##
## PSI-MI 25 Interaction Entry, roughly corresponding to to the 'entry' definition in MIF25.xsd
setClass("psimi25InteractionEntry",
          representation(organismName = "character",
                        taxId = "character",
                        releaseDate = "character",
                        interactors = "list",
                        interactions = "list" # Liste aus mehrere Interactions
                        )
         )

## psimi25 interaction, roughly matching the 'interactionElementType'
setClass("psimi25Interaction",
         representation(sourceDb = "character",
                        sourceId = "character",
                        interactionType = "character",
                        expPubMed = "character",
                        expSourceId = "character",
                        confidenceValue = "character",
                        participant = "character",
                        bait = "character",
                        baitUniProt = "character",
                        prey = "character",
                        preyUniProt = "character",
                        inhibitor = "character",
                        neutralComponent = "character"
                        )
         )

## psimi25 interactor, roughly matching the 'interactorElementType'
setClass("psimi25Interactor",
         representation(sourceDb = "character",
                        sourceId = "character",
                        shortLabel = "character",
                        uniprotId = "character",
                        organismName = "character",
                        taxId = "character",
                        xref = "environment"
                        )
         )


## psimi25 complex
setClass("psimi25Complex",
         representation(sourceDb = "character",
                        sourceId = "character",
                        shortLabel = "character",
                        fullName = "character",
                        organismName = "character",
                        taxId = "character",
                        members = "data.frame",
                        attributes = "character"
                        )
         )

## psimi25 complex Entry
setClass("psimi25ComplexEntry",
         representation(releaseDate = "character",
                        interactors = "list",
                        complexes = "list"
                        )
         )

## psimi25 graph
setClass("psimi25Graph",
         representation(interactors = "list",
                        abstract = "pubMedAbst"),
         contains = "graphNEL"
         )

## psimi25 hypergragh
#setMethod("initialize", "Hypergraph", function(.Object, nodes="", hyperedges=list()) {
#  .Object@nodes <- nodes
#  .Object@hyperedges <- hyperedges
#    .Object
#})
#

setClass("psimi25Hypergraph",
         representation(interactors = "list"),
         contains = "Hypergraph",
         )

## psimi25 experiment
setClass("psimi25Experiment",
         representation(sourceDb = "character",
                        expSourceId = "character",
                        interactionType = "character",
                        expPubMed = "character"
                        )
         )

setClass("psimi25Source",
         representation(label="character",
                        sourceDb = "character",
                        uniprotSymbol = "character",
                        parseExperiment = "function",
                        parseInteractor = "function",
                        parseComplex = "function"
                         ),
         prototype = list(
           label = "Label for the database",
           sourceDb = "source database identifier in the psi-mi 25 file",
           uniprotPath = "NA",
           parseExperiment = function(root, namespaces, psimi25source) {
             .parsePsimi25Experiment(root, namespaces, sourceDb(psimi25source))
           },
           parseInteractor = function(root, namespaces, psimi25source) {
             .parsePsimi25Interactor(root, namespaces,
                                    sourceDb(psimi25source),
                                    uniprot(psimi25source))
           },
           parseComplex = function(root, namespaces, psimi25source) {
             .parsePsimi25Complex(root, namespaces, sourceDb(psimi25source))
           }
           )
         )
