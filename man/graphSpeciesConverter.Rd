\name{graphSpeciesConverter}

\alias{graphSpeciesConverter}
\alias{hyperGraphSpeciesConverter}

\title{maps one type of grap onto another}

\description{
  The graphSpeciesConverter function takes a graphNEL object, along with
  some information about what species that graphNEL is from and what
  species you wish to convert it into, and then uses the mappings in the
  inparanoid packages to convert that graph into an equivalent graph
  from the other species.  The hyperGraphSpeciesConverter performs the
  same service for either an incidence matrix or a Hypergraph. 
}

\usage{
  graphSpeciesConverter(graph, srcSpecies, destSpecies,
    srcIDType="UNIPROT", destIDType="UNIPROT", keepMultGeneMatches=FALSE,
    keepMultProtMatches=FALSE)
}

\arguments{
  \item{graph}{If calling graphSpeciesConverter. then this is a graphNEL
  object.  Otherwise, it will be an incidence matrix or a hyperGraph}
  \item{srcSpecies}{The original source species in in paranoid
  format. In other words, the 3 letters of the genus followed by 2
  letters of the species in all caps.  Ie. 'HOMSA' is for Homo sapiens etc.}
  \item{destSpecies}{the destination species in inparanoid format}
  \item{srcIDType}{The source ID type written exactly as it would be
  used in a mapping name for an eg package.  So for example, 'UNIPROT'
  is how the uniprot mappings are always written, so we keep that
  convention here.}
  \item{destIDType}{the destination ID, written the same way as you
  would write the srcIDType.}
  \item{keepMultGeneMatches}{Do you want to try and keep the 1st ID in
  those ambiguous cases where more than one protein is suggested?  (You
  probably want to filter them out - hence the default is FALSE)}
  \item{keepMultProtMatches}{Do you want to try and keep the 1st ID in
  those ambiguous cases where more than one protein is suggested?
  (default = FALSE)}
}

\value{
  a graphNEL containing as many nodes as it was possible to find matches
  for.
}

\author{Marc Carlson}

\examples{

  ##Set up a graphNEL object to convert
  library(RpsiXML)  
  directory <- system.file("/extdata/psi25files",
                           package="RpsiXML")
  mintXML <- file.path(directory, 
                       "mint_200711_test.xml")
  mintGraph <- separateXMLDataByExpt(xmlFiles=mintXML, 
                                     psimi25source = MINT.PSIMI25, 
                                     type = "indirect", 
                                     directed=TRUE, 
                                     abstract=FALSE)

  if(require("hom.Mm.inp.db")) {
    ##convert it
    newGraph = graphSpeciesConverter(mintGraph[[1]], "MUSMU", "HOMSA")
  }

  ##Get a hypergraph
  xmlDir <- system.file("/extdata/psi25files",package="RpsiXML")
  intactComplexxml <- file.path(xmlDir,"intact_complexSample.xml")
  hyperGraph <- buildPCHypergraph(intactComplexxml, INTACT.PSIMI25)

  if(require("hom.Hs.inp.db")) {
  ##Convert it
  newHyper = hyperGraphSpeciesConverter(hyperGraph, "HOMSA", "MUSMU")
  }

  ##Get a incidence matrix from ScISI
  if(require(ScISI)) {
    data(ScISIC)

    if(require("org.Sc.eg.db")) {
    ##Convert it
    newScISIC = hyperGraphSpeciesConverter(ScISIC, "SACCE", "MUSMU",
    srcIDType="ORF", destIDType = "EG")
  }
  }
}

\keyword{manip}
