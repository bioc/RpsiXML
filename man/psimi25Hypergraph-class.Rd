\name{psimi25Hypergraph-class}
\docType{class}
\alias{psimi25Hypergraph-class}
\alias{initialize,psimi25Hypergraph-method}
\alias{show,psimi25Hypergraph-method}


\title{Class "psimi25Hypergraph"}
\description{ Class to present PSI-MI 2.5 XML data as hypergraph.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{psimi25XML2Graph}
}
\section{Slots}{
  \describe{
    \item{\code{interactors}:}{Object of class \code{"matrix"}, Object of class \code{"matrix"},
      interactor information in a matrix, Each row represents one
      interactor. Source IDs are used as row names. Each column represents one annotation. 
      Annotations include: UniProt ID, short label, organism name, and NCBI taxonomy ID. Only those interactors which are 
      the nodes of the psimi25Graph is given.}
  }
}
\section{Extends}{
Class \code{Hypergraph}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "psimi25Hypergraph")} }
    \item{show}{\code{signature(object = "psimi25Hypergraph")}: show method }
    \item{translateSourceID}{\code{signature(r = "psimi25Hypergraph")}:
      translate source ID into other IDs }
	 }
}
\author{ Tony Chiang <tchiang@ebi.ac.uk> , Jitao David Zhang <j.zhang@dkfz.de> }
\seealso{
  \code{\link{psimi25XML2Graph}},\code{S4class\link{psimi25Graph-class}}
}
\examples{
showClass("psimi25Hypergraph")
}
\keyword{classes}
