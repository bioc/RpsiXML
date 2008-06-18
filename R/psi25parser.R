## misc functions
null2na <- function (x) 
{
  if (is.null(x) || length(x) == 0) 
    x <- as.character(NA)
  else x <- unique(unlist(x))
  x
}

genBPGraph <- function(bpMat, directed=TRUE, bp=TRUE){

  bpMat1 <- bpMat
  b <- rownames(bpMat)
  p <- colnames(bpMat)
 
  if(!bp){
    if(sum(b != p) != 0){
      stop("The rownames and the colnames must be identical.")
    }
  } else{
    baits <- union(rownames(bpMat), colnames(bpMat))
    preys <- baits

    bpMat1 <- matrix(0, length(baits), length(preys))
    dimnames(bpMat1) <- list(baits, preys)
   
    bpMat1[b,p] <- bpMat
    if(!directed) {
      bpMat1 <- bpMat1 + t(bpMat1)
      mode(bpMat1) <- "logical"
      mode(bpMat1) <- "numeric"
    }
  }

  if(directed){
    bpGraph <- as(bpMat1, "graphNEL")
  }

  else{
    bpGraph <- as(bpMat1, "graphNEL")
    bpGraph <- ugraph(removeSelfLoops(bpGraph))
  }
  
  bpGraph
  
}

####################################################
## PSI-MI 2.5 XML entry parsers
## Low-level parsers, only accessible for developers
####################################################

## experiment parser
.parsePsimi25Experiment <- function(root, namespaces, sourceDb) {
  subDoc <- xmlDoc(root)
  
  interactionType <- unlist(xpathApply(doc = subDoc,
                                       path = "/ns:experimentDescription/ns:interactionDetectionMethod/ns:names/ns:shortLabel",
                                       fun = xmlValue,
                                       namespaces = namespaces)
                            )
  expPubMed <- unlist(xpathApply(doc = subDoc,
                                 path = "/ns:experimentDescription/ns:bibref/ns:xref/ns:primaryRef[@db='pubmed']", 
                                 fun = xmlGetAttr,
                                 name = "id",
                                 namespaces = namespaces)
                      )
  expSourceId <- unlist(xpathApply(doc = subDoc,
                                   path = paste("/ns:experimentDescription/ns:xref/ns:primaryRef[@db='",sourceDb,
                                     "']|/ns:experimentList/ns:experimentDescription/ns:xref/ns:secondaryRef[@db='",sourceDb,
                                     "']|/ns:experimentDescription",sep=""), 
                                   fun = xmlGetAttr,
                                   name = "id", namespaces = namespaces)
                        )
  free(subDoc)
  experiment <- new("psimi25Experiment",
                    sourceDb = null2na(sourceDb),
                    interactionType = null2na(interactionType),
                    expSourceId = null2na(expSourceId),
                    expPubMed = null2na(expPubMed)
                    )
  return(experiment)
}


## interactor parse
.parsePsimi25Interactor <- function(root, namespaces, sourceDb, uniprotsymbol) {
  subDoc <- xmlDoc(root)
  sourceIds <- unlist(xpathApply(doc = subDoc, path = "/ns:interactor", 
                                 fun = xmlGetAttr, name = "id", namespaces = namespaces))
  shortLabels <- unlist(xpathApply(doc = subDoc,
                                   path="/ns:interactor/ns:names/ns:shortLabel",
                                   fun = xmlValue,
                                   namespaces = namespaces))
  if(length(shortLabels) == 0 && is.null(shortLabels)) {
    shortLabels <- unlist(xpathApply(doc = subDoc,
                                     path="/ns:interactor/ns:names/ns:fullName",
                                     fun = xmlValue,
                                     namespaces = namespaces))
  }
  shortLabels <- null2na(shortLabels)
  
  organismNames <- null2na(unlist(xpathApply(doc = subDoc, path = "/ns:interactor/ns:organism/ns:names/ns:fullName", 
                                             fun = xmlValue, namespaces = namespaces)))
  taxIds <- unlist(null2na(xpathApply(doc = subDoc, path = "/ns:interactor/ns:organism", 
                              fun = xmlGetAttr, name = "ncbiTaxId", namespaces = namespaces)))
  
  uniprot <- unlist(null2na(xpathApply(doc = subDoc,
                                       path = sprintf("/ns:interactor/ns:xref/ns:primaryRef[@db='%s']|/ns:interactor/ns:xref/ns:secondaryRef[@db='%s']",uniprotsymbol, uniprotsymbol),
                                       fun = xmlGetAttr,
                                       name = "id",
                                       namespaces=namespaces)))[1]
  xrefDb <- xpathApply(doc = subDoc,
                      path = "/ns:interactor/ns:xref/ns:primaryRef|/ns:interactor/ns:xref/ns:secondaryRef",
                      fun = xmlGetAttr,
                      name = "db",
                      namespaces = namespaces)
  xrefId <- xpathApply(doc = subDoc,
                      path = "/ns:interactor/ns:xref/ns:primaryRef|/ns:interactor/ns:xref/ns:secondaryRef",
                      fun = xmlGetAttr,
                      name = "id",
                      namespaces = namespaces)

  sourceIdMat <- cbind("sourceId", sourceIds)
  tempMat <- matrix(c(xrefDb, xrefId), ncol=2, byrow=FALSE)
  xrefMat <- rbind(sourceIdMat, tempMat)
  colnames(xrefMat) <- c("db","id")
  xrefEnv <- new.env()
  xref <- assign("xref",xrefMat, envir=xrefEnv) 
                     
  free(subDoc)
  interactor <- new("psimi25Interactor",
                    sourceDb = sourceDb,
                    sourceId = sourceIds,
                    shortLabel = shortLabels,
                    uniprotId = uniprot,
                    organismName = organismNames,
                    taxId = taxIds,
                    xref = xrefEnv
                    )
  return(interactor)
}


## complex parser
.parsePsimi25Complex <- function(root, namespaces, sourceDb) {
	subDoc <- xmlDoc(root)
	# source ID
	sourceId <- as.character(xpathApply(doc=subDoc,
				path=paste("/ns:interaction/ns:xref/ns:primaryRef[@db='",sourceDb,"']",sep=""),
				fun=xmlGetAttr,
				name="id",
				namespaces=namespaces))
	# short label
	shortLabel <- as.character(xpathApply(doc=subDoc,
				path="/ns:interaction/ns:names/ns:shortLabel",
				fun=xmlValue,
				namespaces=namespaces))
	# full name
	fullName <-as.character(xpathApply(doc=subDoc,
                                path="/ns:interaction/ns:names/ns:fullName",
                                fun=xmlValue,
                                namespaces=namespaces))
 
	# interactors
	interactorIds <-as.character(xpathApply(doc=subDoc,
                                path="/ns:interaction/ns:participantList/ns:participant/ns:interactorRef",
                                fun=xmlValue,
                                namespaces=namespaces))

	# attributes
	attributeNodes <- getNodeSet(subDoc, 
				"/ns:interaction/ns:attributeList/ns:attribute[@name]",
				namespaces=namespaces)
	if (length(attributeNodes)>0) {
		attributeNames <- sapply(attributeNodes, xmlGetAttr, name="name")
		attributes <- sapply(attributeNodes, xmlValue)
		names(attributes) <- attributeNames
	} else {
		attributes <- character(0)
	}
	free(subDoc)
	
	list(sourceDb = sourceDb,
             sourceId = sourceId,
             shortLabel= shortLabel,
             fullName=fullName,
             interactorIds=interactorIds,
             attributes=attributes)
}

##############################################
## High-level parsers taking files as input
##############################################

## File parser: parsing file into interaction entries
parsePsimi25Interaction <- function (psimi25file, psimi25source) {
  psimi25Doc <- xmlTreeParse(psimi25file, useInternalNodes = TRUE)
                           
  psimi25NS <- getDefaultNamespace(psimi25Doc)
  namespaces <- c(ns = psimi25NS)
  entry <- getNodeSet(psimi25Doc, "/ns:entrySet/ns:entry", namespaces)
  entryCount <- length(entry)

  getValueWithPath <- function(doc, path, ns) {
    x <- unlist(xpathApply(doc=doc,path=path,fun=xmlValue, namespaces=ns))
    return(null2na(x))
  }

        
  getMapping <- function(x, matrix, nameCol, valueCol) {
    names <- matrix[,nameCol]
    ind <- match(x, names)
    if (length(ind) == 1 && is.na(ind)) {
      value <- as.character(NA)
    } else {
      value <- unlist(matrix[ind, valueCol])
    }
    return(value)
  }
  
  ## get interaction details from interaction node
  getInteraction <- function(theNodes, index,  sourcedb, expEnv, interactorInfo, namespaces) {
    interactions <- vector("list",length=length(theNodes))
    for (i in seq(along=theNodes)) {
      theNode <- theNodes[[i]]
      subDoc <- xmlDoc(theNode)
      psimi25Ids <- xpathApply(doc = subDoc,
                               path = paste("/ns:interaction/ns:xref/ns:primaryRef[@db='",
                                sourcedb,"']",sep=""), 
                              fun = xmlGetAttr, name = "id", namespaces = namespaces)
      psimi25Id <- null2na(unlist(psimi25Ids)[[1]])
      
      expRef <- unlist(xpathApply(doc = subDoc,
                                  path = sprintf("/ns:interaction/ns:experimentList/ns:experimentRef"), 
                                  fun = xmlValue, namespaces = namespaces))

      if ((!is.null(expRef)) && exists(expRef, envir = expEnv)) {
        expData <- get(expRef, envir = expEnv)
        interactionType <- expData@interactionType
        expPsimi25 <- expData@expSourceId
        expPubMed <- expData@expPubMed
      }
      else {
        interactionType <- as.character(NA)
        expPsimi25 <- as.character(NA)
        expPubMed <- as.character(NA)
      }

      ### misc attributes
      ## confidence value
      rolePath <-  "/ns:interaction/ns:confidenceList/ns:confidence/ns:value"
      confidenceValue <- getValueWithPath(doc=subDoc,path = rolePath, ns = namespaces)

      ## participant
      rolePath <- "/ns:interaction/ns:participantList/ns:participant/ns:interactorRef"
      participantRefs <- getValueWithPath(doc=subDoc, path=rolePath, ns = namespaces)
      
      ## bait
      rolePath <- "/ns:interaction/ns:participantList/ns:participant[ns:experimentalRoleList/ns:experimentalRole/ns:names/ns:fullName='bait']/ns:interactorRef"
      baitRefs <- getValueWithPath(doc=subDoc, path = rolePath, ns = namespaces)

      ##prey
      rolePath <- "/ns:interaction/ns:participantList/ns:participant[ns:experimentalRoleList/ns:experimentalRole/ns:names/ns:fullName='prey']/ns:interactorRef"
      preyRefs <- getValueWithPath(doc=subDoc, path = rolePath, ns = namespaces)

      ## inhibitor
      rolePath <- "/ns:interaction/ns:participantList/ns:participant[ns:experimentalRoleList/ns:experimentalRole/ns:names/ns:fullName='inhibitor']/ns:interactorRef"
      inhibitorRefs <- getValueWithPath(doc=subDoc, path=rolePath, ns = namespaces)

      ## neutral component
      rolePath <- "/ns:interaction/ns:participantList/ns:participant[ns:experimentalRoleList/ns:experimentalRole/ns:names/ns:fullName='neutral component']/ns:interactorRef"
      neutralComponentRefs <- getValueWithPath(doc=subDoc, path = rolePath, ns = namespaces)

      free(subDoc)


      srcLabel <- "sourceId"; uniLabel <- "uniprotId"
      participantUniprot <- getMapping(participantRefs, interactorInfo, srcLabel, uniLabel)
      preyUniprot <- getMapping(preyRefs, interactorInfo, srcLabel, uniLabel)
      baitUniprot <- getMapping(baitRefs, interactorInfo, srcLabel, uniLabel)
      inhibitorUniprot <- getMapping(inhibitorRefs,  interactorInfo, srcLabel, uniLabel)
      neutralComponentUniprot <- getMapping(neutralComponentRefs,  interactorInfo, srcLabel, uniLabel)

      
      interactions[[i]] <- new("psimi25Interaction",
                               sourceDb = sourcedb,
                               sourceId = as.character(psimi25Id), ## FIXME: can we do it nullable?
                               interactionType = interactionType, 
                               expPubMed = expPubMed,
                               expSourceId = expPsimi25, 
                               confidenceValue = confidenceValue,
                               participant = participantUniprot,
                               bait = baitRefs,
                               baitUniProt = baitUniprot, 
                               prey = preyRefs,
                               preyUniProt = preyUniprot,
                               inhibitor = inhibitorUniprot, 
                               neutralComponent = neutralComponentUniprot
                               )
    }
    interactions
  }
  
  entryList <- lapply(seq(entryCount), function(i) {
    basePath <- paste("/ns:entrySet/ns:entry[", i, "]", sep = "", 
                      collapse = "")
    thisEntry <- new("psimi25InteractionEntry")

    ## experiment
    experimentPath <- paste(basePath, "/ns:experimentList/ns:experimentDescription", 
                            sep = "", collapse = "")
    experimentNodes <- getNodeSet(psimi25Doc, experimentPath, 
                                  namespaces)
    experimentEnv <- new.env(parent = emptyenv(), hash = TRUE)
    lapply(experimentNodes, function(thisExperiment) {
      experimentId <- xmlGetAttr(thisExperiment, name = "id")
      experimentData <- parseExperiment(psimi25source, thisExperiment,namespaces)
      assign(experimentId, experimentData, envir = experimentEnv)
      ""
    })
    releaseDatePath <- paste(basePath, "/ns:source", sep = "", 
                             collapse = "")
    releaseDate <- xpathApply(doc = psimi25Doc, path = releaseDatePath, 
                              fun = xmlGetAttr, name = "releaseDate", namespaces = namespaces)
    thisEntry@releaseDate <- null2na(unlist(releaseDate))

    ## interactor
    interactorPath <- paste(basePath, "/ns:interactorList/ns:interactor", 
                            sep = "", collapse = "")
    interactorNodes <- getNodeSet(psimi25Doc, interactorPath, 
                                  namespaces)
    interactorIds <- sapply(interactorNodes, xmlGetAttr, 
                            name = "id")
    interactorCount <- length(interactorNodes)
    interactors <- vector("list",length=interactorCount)
    if (interactorCount > 0) {
      for (p in seq(interactorCount)) {
        theRes <- parseInteractor(psimi25source, interactorNodes[[p]], namespaces)
        interactors[[p]] <- theRes
      }
    }
    interactorInfMat <- interactorInfo(interactors)
    names(interactors) <- interactorInfMat[,"uniprotId"]
    
    organismName <- unique(unlist(interactorInfMat[, "organismName"]))
    thisEntry@organismName <- organismName
    taxId <- unique(unlist(interactorInfMat[, "taxId"]))
    thisEntry@taxId <- taxId
    interactionPath <- paste(basePath, "/ns:interactionList/ns:interaction", 
                             sep = "", collapse = "")
    interactionNodes <- getNodeSet(psimi25Doc, interactionPath, 
                                   namespaces = namespaces)
    sourcedb <- sourceDb(psimi25source)

    interactions <- getInteraction(interactionNodes,
                                   sourcedb = sourcedb, expEnv = experimentEnv,
                                   interactorInfo = interactorInfMat,
                                   namespaces=namespaces)
    
    thisEntry@interactions <- interactions
    rownames(interactorInfMat) <- interactorInfMat[, "uniprotId"]
    thisEntry@interactors <- interactors
    thisEntry
  })
  
  free(psimi25Doc)
  entryList <- entryList[[1]]
  entryList
}


## File parser: parsing file into complex
parsePsimi25Complex <- function(psi25File, psimi25source) {
  psiDoc <- xmlTreeParse(psi25File, useInternalNodes=TRUE)
  psiNS <- xmlNamespaceDefinitions(psiDoc)
  namespaces <- c(ns=psiNS[[1]]$uri)

  releaseDate <- null2na(xpathApply(doc=psiDoc,
                            path="//ns:entry/ns:source", 
                            fun=xmlGetAttr,
                            name="releaseDate", 
                            namespaces=namespaces)[[1]])
  ##############
  # interactor #
  ##############
  interactorNodes <- getNodeSet(psiDoc,
                                "//ns:interactorList/ns:interactor", namespaces)
  interactorIds <- sapply(interactorNodes, xmlGetAttr, name="id")
  interactorCount <- length(interactorNodes)
  interactors <- vector("list", length=interactorCount)
  for (i in seq(interactorCount)) {
    theRes <- parseInteractor(psimi25source, interactorNodes[[i]], namespaces)
    interactors[[i]] <- theRes
  }
  interactorInfo <- interactorInfo(interactors)
  names(interactors) <- interactorInfo[,"uniprotId"]
  ##############
  # complex    #
  ##############
  complexNodes <- getNodeSet(psiDoc, "//ns:interactionList/ns:interaction", namespaces)
  complexList <- lapply(complexNodes, function(thisNode) {
    thisComplex <- parseComplex(psimi25source, thisNode, namespaces)
    participantRef <- split(thisComplex$interactorIds, 
                            as.factor(thisComplex$interactorIds))
    multiplicity <- sapply(participantRef, length)
    if ( !all(names(participantRef) %in% interactorIds) ) {
      msg <- paste("parse complex ", thisComplex$shortLabel, 
                   "can't resolve all interactor references.")
      stop(msg)
    }	
    participantIndex <- interactorIds %in% names(participantRef)
    participants <- interactorInfo[participantIndex, "sourceId"];
    participantsUniProt <- interactorInfo[participantIndex, "uniprotId"];
    names(participants) <- as.character(participants)
    participants <- data.frame(sourceId=participants, uniprotId = participantsUniProt, multiplicity=multiplicity)
    thisOrganism <- unique(as.character(interactorInfo[participantIndex,"organismName"]))
    thisTaxid <- unique(as.character(interactorInfo[participantIndex, "taxId"]))
    new("psimi25Complex",
        sourceDb = thisComplex$sourceDb,
        sourceId = thisComplex$sourceId,
        shortLabel=thisComplex$shortLabel,
        fullName=thisComplex$fullName,
        organismName=thisOrganism,
        taxId=thisTaxid,
        members=participants,
        attributes=thisComplex$attributes
        )
  })
  free(psiDoc)
  new("psimi25ComplexEntry",
      interactors=interactors,
      complexes=complexList,
      releaseDate=releaseDate)
}

## File parser: parsing file into graph
psimi25XML2Graph <- function(psimi25files,psimi25source,
                             type="interaction",
                             directed=TRUE) {

  options(error=recover)
  
  stopifnot(type %in% c("interaction","complex"))

  if(type == "interaction"){

    result <- lapply(psimi25files, parsePsimi25Interaction, psimi25source)
    
    baitList <- lapply(result, function(x){
      baits <- sapply(x@interactions, function(y) y@baitUniProt)
    })
    
    preyList <- lapply(result, function(x){
      prey <- sapply(x@interactions, function(y) y@preyUniProt)
    })

    index <- sapply(baitList, function(x) class(x)) == sapply(preyList, function(x) class(x))

    for(i in 1:length(index)){

        if(!index[i]){
            newBait <- vector(length=length(unlist(preyList[[i]])))
            k <- 1
            for(j in 1:length(preyList[[i]])){
                newBait[k:(k+length(preyList[[i]][[j]])-1)] <- rep(baitList[[i]][j], length(preyList[[i]][[j]]))
                k <- k + length(preyList[[i]][[j]])
            }
            baitList[[i]] <- newBait
        }

    }

    
    b <- unlist(baitList); b[is.na(b)] <- "NA";
    p <- unlist(preyList); p[is.na(p)] <- "NA";
    
    bpList <- split(p,b)
    bpMat <- list2Matrix(bpList)
    
    bpG <- genBPGraph(bpMat, directed = directed)
    
    bpInteractors <- list()
    for(i in seq(result)) {
      ints <- interactors(result[[i]])
      newInts <- which(!names(ints) %in% bpInteractors)
      bpInteractors <- append(bpInteractors, ints[newInts])
    }
    bpGraph <- as(bpG, "psimi25Graph")
    bpGraph@interactors <- bpInteractors
    
  }
  
  if (type == "complex"){
    result <- lapply(psimi25files, parsePsimi25Complex, psimi25source)

    listOfListOfComps <- lapply(result, function(x){
      lapply(x@complexes, function(y){
        p <- as.vector(rep(y@members[,2], y@members[,3]))
        
        attr(p, "complexName") <- y@fullName 
        p
        })
      })
    he <- do.call(c, listOfListOfComps)
    nodes <- unique(unlist(he))
    hEdges <- lapply(he, function(x) {e <- Hyperedge(x, attr(x,"complexName"))})

    bpInteractors <- list()
    for(i in seq(result)) {
      ints <- interactors(result[[i]])
      newInts <- which(!names(ints) %in% bpInteractors)
      bpInteractors <- append(bpInteractors, ints[newInts])
    }
    
    bpGraph <- new("psimi25Hypergraph",
                   interactors=bpInteractors,
                   nodes=nodes,
                   hyperedges = hEdges)
  }

  return(bpGraph)
}


separateXMLDataByExpt <- function(xmlFiles, psimi25source, type = "direct", directed=TRUE, abstract=FALSE){
  

  if(!(type %in% c("direct","indirect", "eg"))){
    stop("The argument type needs to be either direct or indirect")
  }

  if(type == "direct"){
    interactionTypeWanted = c("two hybrid","two hybrid array", 
                               "2h fragment pooling", "2 hybrid")}
  if(type == "indirect"){
    interactionTypeWanted = c("coip","anti tag coip","pull down","tap",
        "anti bait coip","affinity chrom","chromatography","ion exchange chrom"
        ,"affinity techniques")}
  if(type == "eg"){
    interactionTypeWanted = c("spr")}
  
  #create a list of psimi25interaction objects corresponding to the list of xml files
  ie <- lapply(xmlFiles, parsePsimi25Interaction, psimi25source)

  
  #create an aggregate interactor list from all psimi25interactio objects
  interactorList <- lapply(ie, function(x) x@interactors)
  combinedList <- do.call(c, interactorList)
  protId <- unique(names(combinedList))
  uniqueCombList <- combinedList[protId]

  psiM <- lapply(ie, function(x){getDesired(x,interactionTypeWanted)})

  #now we have a kx3 matrix that has (bait,prey,pmid) info for all xml files
  psi <- do.call(rbind, psiM)

  if(is.null(psi)) {stop("There are no interactions of the type that you requested
                         within these XML files.")}

  #split the bait/prey info by pmid
  psiBPPairs <- split(data.frame(psi[,c("Bait","Prey")],
    stringsAsFactors=FALSE),psi[,"PMID"])

  #for each pmid, split the prey by the baits (we only want non-empty stuff)
  psiBPList <- lapply(psiBPPairs, function(x){split(x[,2], x[,1])})
  psiBPList <- lapply(psiBPList, function(x) {y = x[listLen(x)>0]; return(y)})

  #from each bpList for each pmid, we create adjMat and graphNEL
  psiBPMatrix <- lapply(psiBPList, list2Matrix)
  psiBPMatrix <- lapply(psiBPMatrix, t)
  psiBPGraphs <- lapply(psiBPMatrix, function(x) {genBPGraph(x, directed=directed)})

  
  
  #get abstract information for each dataset if desired
  if(abstract){
    pmids <- names(psiBPGraphs)
    abst <- getAbstractByPMID(pmids)

  #lastly, we create a psimi25Graph from each of the graphNELs
    psiGraphs <- mapply(function(x,w) {y <- as(x, "psimi25Graph");
                                       y@interactors <- uniqueCombList[nodes(x)];
                                       y@abstract = w; return(y)},
                        psiBPGraphs, abst)
  }

  else{

    psiGraphs <- lapply(psiBPGraphs, function(x,w) {#browser()
      y <- as(x, "psimi25Graph"); y@interactors <- uniqueCombList[nodes(x)];
      return(y)})

  }
  #return the graph objects
  return(psiGraphs)

  ###this works for intact, need to test on all other databases!!!
}

getDesired <- function(interactionEntry, intType){

  #options(error=recover)
  x <- interactions(interactionEntry)
  
  dataL <- lapply(x, function(y){if(any(y@interactionType %in% intType)) {
    z <- c(bait(y), prey(y), pubmedID(y), interactionType(y));
    if(length(z)==4){if(!(is.na(z[1])) && !(is.na(z[2]))) {return(z)}
                   else return(NULL)}
    if(length(z)>4){bpm = matrix(nrow=length(prey(y)), ncol=4);
                    bpm[,1]=rep(bait(y), length(prey(y)));
                    bpm[,2]=prey(y)
                    bpm[,3] = rep(pubmedID(y), length(prey(y)));
                    bpm[,4] = rep(interactionType(y), length(prey(y)));
                    return(bpm)}}})

  dataL <- dataL[!(sapply(dataL, is.null))]
  if(length(dataL)>0){
    dataM <- do.call(rbind, dataL)
    colnames(dataM) <- c("Bait","Prey","PMID", "Interaction Type")
  
    return(dataM)}
  else {return(NULL)}
  
}
  
