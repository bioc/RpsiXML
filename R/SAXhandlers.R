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

psixmlHandlerPrimaryRef <- function(name, attrs,...) {
  testPR[[length(testPR)+1]] <<- new("psimi25DbReferenceType",
                                     db=attrs["db"],
                                     dbAc=attrs["dbAc"],
                                     id=attrs["id"],
                                     secondary=attrs["secondary"],
                                     version=attrs["version"],
                                     refType=attrs["refType"],
                                     refTypeAc=attrs["refTypeAc"])
}

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

