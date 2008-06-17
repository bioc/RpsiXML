\name{psimi25Source-class}
\docType{class}
\alias{psimi25Source-class}
\alias{parseComplex,psimi25Source-method}
\alias{parseExperiment,psimi25Source-method}
\alias{parseInteractor,psimi25Source-method}
\alias{uniprot,psimi25Source-method}

\title{Class "psimi25Source"}
\description{ A class representing data source compatible with PSI-MI
  2.5 XML data exchange format}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("psimi25Source", ...)}
}
\section{Slots}{
	 \describe{
    \item{\code{label}:}{Object of class \code{"character"}, a
      meaningful unique label of the database, does not necessarily have to be
      the identifier used in the PSI-MI files. The value is advised to
      be overwritten by the objects}
    \item{\code{sourceDb}:}{Object of class \code{"character"}, the
      character string used in the PSI-MI files to identify the source
      database. The value must be overwritten by the object, depending
      on the database identifier used in the PSI-MI file.}
    \item{\code{uniprotSymbol}:}{Object of class \code{"character"}, the
    identifier of UniProf reference in "primaryRef" or "secondaryRef"
    attributes of the interactors, telling the parser where to find
    UniProt symbol of the interactors. The value must be overwritten by
    the objects.}
    \item{\code{parseExperiment}:}{Object of class \code{"function"},
      the function to parse experiment from the PSI-MI 2.5 file. Except
      for special need this function should not be overwritten by the
      objects, since it parses the experiment node strictly according
      the PSI-MI standard }
    \item{\code{parseInteractor}:}{Object of class \code{"function"},
      the function to parse interactor from the PSI-MI 2.5 file. Except
      for special need this function should not be overwritten by the
      objects, since it parses the interactor node strictly according
      the PSI-MI standard  }
    \item{\code{parseComplex}:}{Object of class \code{"function"},
      the function to parse complex from the PSI-MI 2.5 file. Except
      for special need this function should not be overwritten by the
      objects, since it parses the complex node strictly according
      the PSI-MI standard }
  }
}
\section{Methods}{
  \describe{
    \item{parseComplex}{\code{signature(x = "psimi25Source")}: parse
      complex node of PSI-MI 2.5 file }
    \item{parseExperiment}{\code{signature(x = "psimi25Source")}: parse
      experiment node of PSI-MI 2.5 file}
    \item{parseInteractor}{\code{signature(x = "psimi25Source")}: parse
      interactor node of PSI-MI 2.5 file}
    \item{sourceDb}{\code{signature(x = "psimi25Source")}: returns the
      source database}
    \item{uniprot}{\code{signature(x = "psimi25Source")}: returns the
      uniprot identifier of the PSI-MI 2.5 XML file }
  } 
}
       
\author{ Jitao David Zhang <j.zhang@dkfz.de> }
\note{
  In case of a new data repository, you can first try creating a
  new object of the class with \code{label}, \code{sourceDb} and
  \code{uniprotSymbol} adjusted as needed. Try parsing a function with
  this new psimi25Source object. In case of warnings or errors, it may
  mean that the file provided is not strictly compatible with PSI-MI 2.5
  data exchange format standard, please then contact the data provider.

  By 02.06.08, six sources have been registered in the release version
  of \code{RpsiXML}:  BIOGRID.PSIMI25, DIP.PSIMI25, HPRD.PSIMI25, INTACT.PSIMI25,,
  MINT.PSIMI25,MIPS.PSIMI25. Along the development we will implement
  more data sources under PSI-MI 25 standard.
}

\seealso{
  \code{\link{parsePsimi25Interaction}},
  \code{\link{parsePsimi25Complex}},
  \code{\link{psimi25XML2Graph}}
}
\examples{
showClass("psimi25Source")
}
\keyword{classes}
