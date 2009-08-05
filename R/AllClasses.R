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
validityTypedList <- function(object) {
  if(length(object)==0) {
    ##    warning("Empty object! It is not possible to determine type sanity\n")
    return(TRUE)
  }
  
  classes <- lapply(object, class)
  uClass <- unique(classes)

  if(length(uClass)==1) {
    val <- TRUE
    expType <- object@type
    if(!is.na(expType)) {
      if(!all(uClass[[1]] == expType)) {
        val <- paste(expType, " Type(s) expected, but got ", uClass[[1]], "type (s)\n", sep="")
      }
    }
  } else {
    val <- "Hetero. types detected\n"
  }
  return(val)
}

setClass("typedList",
         representation(type="character"),
         prototype=prototype(type=as.character(NA)),
         contains="list",
         validity=validityTypedList)

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
setClass("psimi25NamesTypeAtom",
         representation(shortLabel="character",
                        fullName="character",
                        alias="character"),
         validity=function(object) {
           length(object@shortLabel)==1 & length(object@fullName)==1
         })
setClass("psimi25NamesType",
          prototype=prototype(new("typedList", type="psimi25NamesTypeAtom")),
         contains="typedList")

##--------------------##
## attributeListType
##--------------------##
setClass("psimi25Attribute",
         representation(name="character", ## required
                        nameAc="character"),
         contains="character")

setClass("psimi25AttributeListType",
         prototype=prototype(new("typedList", type="psimi25Attribute")),
         contains="typedList")

##--------------------##
## availabilityType
##--------------------##
setClass("psimi25AvailabilityType",
         representation(id="integer"),
         contains="character")


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
         prototype=prototype(new("typedList", type="psimi25AttributeListType")),
         contains="typedList")
setClass("psimi25DbReferenceTypeList",
         prototype=prototype(new("typedList", type="psimi25DbReferenceTypeList")),
         contains="typedList")
##--------------------##
## xrefType
##--------------------##
setClass("psimi25XrefTypeAtom",
         representation(primaryRef="psimi25DbReferenceType",
                        secondaryRef="psimi25DbReferenceTypeList"))

setClass("psimi25XrefType",
         prototype=prototype(new("typedList", type="psimi25XrefType")),
         contains="typedList")


##--------------------##
## bibrefType
##--------------------##
setClass("psimi25BibrefType",
         representation(xref="psimi25XrefType",
                        attributeList="psimi25AttributeListType") ## only ONE of xref/attributeList
         )


##--------------------##
## cv and opencv
##--------------------##
#### common combinations
setClass("psimi25CommonNameRef",
         representation(name="psimi25NamesType",
                        xref="psimi25XrefType"))

setClass("psimi25CommonNameRefAttr",
         representation(attributeList="psimi25AttributeListType"),
         contains="psimi25CommonNameRef"
         )
setClass("psimi25CvType",
         prototype=prototype(new("typedList", type="psimi25CommonNameRef")),
         contains="typedList")

setClass("psimi25CvTypeList",
         prototype=prototype(new("typedList",type="psimi25CvType")),
         contains="typedList")

setClass("psimi25OpenCvType",
         prototype=prototype(new("typedList", type="psimi25CommonNameRefAttr")),
         contains="typedList")

setClass("psimi25OpenCvTypeList",
         prototype=prototype(new("typedList",type="psimi25OpenCvType")),
         contains="typedList")

##--------------------##
## bioSourceType
##--------------------##
setClass("psimi25BioSourceType",
         representation(name="psimi25NamesType",
                        cellType="psimi25OpenCvType",
                        compartment="psimi25OpenCvType",
                        tissue="psimi25OpenCvType"))
setClass("psimi25BioSourceTypeList",
         prototype=prototype(new("typedList",type="psimi25BioSourceType")),
         contains="typedList")

##--------------------##
## experimentRefListType
##--------------------##
setClass("psimi25ExperimentRef",
         contains="integer")
setClass("psimi25ExperimentRefListType",
         prototype=prototype(new("typedList", type="psimi25ExperimentRef")),
         contains="typedList")
setClass("psimi25ExperimentRefListTypeList",
         prototype=prototype(new("typedList", type="psimi25ExperimentRefListType")),
         contains="typedList")
##----------------------------------------##
## confidenceType/confidenceListType
##----------------------------------------##
setClass("psimi25ConfidenceTypeAtom",
         representation(unit="psimi25OpenCvType",
                        value="character"),
         )

setClass("psimi25ConfidenceType",
         prototype=prototype(new("typedList",type="psimi25ConfidenceTypeAtom")),
         contains="typedList"
         )
setClass("psimi25Confidence",
         representation(experimentRefList="psimi25ExperimentRefListTypeList"),
         contains="psimi25ConfidenceType")

setClass("psimi25ConfidenceListType",
         prototype=prototype(new("typedList", type="psimi25Confidence")),
         contains="typedList")


##--------------------##
## interactorElementType
##--------------------##
## Attention: since 'names' is keyword in 'representation' function, the slot is renamed as 'name'
setClass("psimi25InteractorElementType",
         representation(name="psimi25NamesType",
                        xref="psimi25XrefType",
                        interactorType="psimi25CvType",
                        organism="psimi25BioSourceType",
                        sequence="character", ## will be replaced by BioString! TODO
                        attributeList="psimi25AttributeListType"))
setClass("psimi25InteractorElementTypeList",
         prototype=prototype(new("typedList", type="psimi25InteractorElementType")),
         contains="typedList")

##--------------------##
## experimentType
##--------------------##
## Attention: since 'names' is keyword in 'representation' function, the slot is renamed as 'name'
setClass("psimi25ExperimentType",
         representation(name="psimi25NamesType",
                        bibref="psimi25BibrefType",
                        xref="psimi25XrefType",
                        hostOrganismList="psimi25BioSourceTypeList",
                        interactionDetectionMethod="psimi25CvType",
                        participantIdentificationMethod="psimi25CvType",
                        featureDetectionMethod="psimi25CvType",
                        confidenceList="psimi25ConfidenceListType",
                        attributeList="psimi25AttributeListType")
         )

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
setClass("psimi25ParameterTypeList",
         contains="typedList",
         prototype=prototype(new("typedList", type="psimi25ParameterType")))

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
         contains="typedList",
         prototype=prototype(new("typedList", type="psimi25BaseLocationTypeStartAtom")))
setClass("psimi25BaseLocationTypeEndAtom",
         representation(endStatus="psimi25CvType",
                        end="integer",
                        endInterval="psimi25IntervalType")) ## only one of begin/beginInterval is allowed
setClass("psimi25BaseLocationTypeEnd",
         contains="typedList",
         prototype=prototype(new("typedList", type="psimi25BaseLocationTypeEndAtom")))


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
          contains="typedList",
         prototype=prototype(new("typedList", type="psimi25BaseLocationType")))

setClass("psimi25FeatureElementType",
         representation(name="psimi25NamesType",
                        xref="psimi25XrefType",
                        featureType="psimi25CvType",
                        featureDetectionMethod="psimi25CvType",
                        experimentRefList="psimi25ExperimentRefListType",
                        featureRangeList="psimi25FeatureRangeList",
                        attributesList="psimi25AttributeListType"))
setClass("psimi25FeatureElementTypeList",
         prototype=prototype(new("typedList", type="psimi25FeatureElementType")),
         contains="typedList")

##--------------------##
## participantType
##--------------------##
## Attention: since 'names' is keyword in 'representation' function, the slot is renamed as 'name'
setClass("psimi25CvExperimentRefsAtom",
         representation(experimentRefList="psimi25ExperimentRefListType"),
         contains="psimi25CvType")
setClass("psimi25CvExperimentRefsList",
         contains="psimi25CvExperimentRefsAtom")
setClass("psimi25ExperimentInteractorAtom",
         representation(interactorRef="integer",
                        interactor="psimi25InteractorElementTypeList", ## only one of interactorRef or interactor
                        experimentRefList="psimi25ExperimentRefListType"))
setClass("psimi25ExperimentInteractor",
         contains="typedList",
         prototype=prototype(new("typedList", type="psimi25ExperimentInteractorAtom")))

setClass("psimi25ParticipantType",
         representation(name="psimi25NamesType",
                        xref="psimi25XrefType",
                        interactorRef="integer",
                        interactor="psimi25InteractorElementType",
                        interactionRef="integer", ## ONLY one of the interactorRef/interactor/interactionRef,
                        participantIdentificationMethodList="psimi25CvExperimentRefsList",
                        biologicalRole="psimi25CvType",
                        experimentalRoleList="psimi25CvExperimentRefsList",
                        experimentalPreparationList="psimi25CvExperimentRefsList",
                        experimentInteractorList="psimi25ExperimentInteractor",
                        featureList="psimi25FeatureElementTypeList",
                        confidenceList="psimi25ConfidenceListType",
                        parameterList="psimi25ParameterTypeList",
                        attributeList="psimi25AttributeListType"))
setClass("psimi25ParticipantTypeList",
          prototype=prototype(new("typedList", type="psimi25ParticipantType")),
         contains="typedList")

##--------------------##
## interactionElementType
##--------------------##
## Attention: since 'names' is keyword in 'representation' function, the slot is renamed as 'name'
setClass("psimi25ExperimentListAtom",
         representation(experimentRef="integer",
                        experimentDescription="psimi25ExperimentType" ## ONLY one of the experimentRef/experimentDescription is needed
                        ))
setClass("psimi25ExperimentList",
         prototype=prototype(new("typedList", type="psimi25ExperimentAtom")),
         contains="typedList") ## TODO: here we also have to check each atom follows the same way

setClass("psimi25InferredInteractionAtomParticipantAtom",
         representation(participantRef="integer",
                        participantFeatureRef="integer") ## ONLY one of the participantRef/participantFeatureRef is needed
         )
setClass("psimi25InferredInteractionAtomParticipant",
         contains="typedList",
         prototype=prototype(new("typedList", type="psimi25InferredInteractionAtomParticipantAtom")))
setClass("psimi25InferredInteractionAtom",
         representation(participant="psimi25InferredInteractionAtomParticipant",
                        experimentRefList="psimi25ExperimentRefListType")
         )
setClass("psimi25InferredInteraction",
         contains="typedList",
         prototype=prototype(new("typedList", type="psimi25InferredInteractionAtom")))
         
setClass("psimi25InteractionElementType",
         representation(name="psimi25NamesType",
                        xref="psimi25XrefType",
                        availabilityRef="integer",
                        availability="psimi25AvailabilityType", ## ONLY one of the availabilityRef/availability is needed
                        experimentList="psimi25ExperimentList",
                        participantList="psimi25ParticipantTypeList",
                        inferredInteractionList="psimi25InferredInteraction",
                        interactionType="psimi25CvType",
                        modelled="logical",
                        intraMolecular="logical",
                        negative="logical",
                        confidenceList="psimi25ConfidenceListType",
                        parameterList="psimi25ParameterTypeList",
                        attributeList="psimi25AttributeListType"),
         prototype=prototype(negative=FALSE, intraMolecular=FALSE)
         )

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
