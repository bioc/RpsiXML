## psimi25 interaction entry
setClass("psimi25InteractionEntry",
         representation(organismName = "character",
                        taxId = "character",
                        releaseDate = "character",
                        interactors = "list",
                        interactions = "list" # Liste aus mehrere Interactions
                        )
         )


## psimi25 interaction
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

## psimi25 interactor
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
         representation(interactors = "list", abstract = "pubMedAbst"),
         contains = "graphNEL"
         )

## psimi25 hypergragh
## Comment by David on 01.04.09: suddenly I have the problem of initializing Hypergraph object
## so I have re-written the initialize method for Hypergraph here. Hopefully the issue could be solved soon
setMethod("initialize", "Hypergraph", function(.Object, nodes="", hyperedges=list()) {
  .Object@nodes <- nodes
  .Object@hyperedges <- hyperedges
    .Object
})

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
