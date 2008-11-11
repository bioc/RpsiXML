\name{psimi25Complex-class}
\docType{class}
\alias{psimi25Complex-class}
\alias{show,psimi25Complex-method}
\alias{complexAttributes,psimi25Complex-method}
\alias{complexMembers,psimi25Complex-method}
\alias{complexName,psimi25Complex-method}

\title{Class "psimi25Complex"}
\description{ A class representing complex data}
\section{Objects from the Class}{
  Objects can be created by calls of \code{parsePsimi25Complex}
}
\section{Slots}{
	 \describe{
    \item{\code{sourceDb}:}{Object of class \code{"character"}, short
      label of the source database}
    \item{\code{sourceId}:}{Object of class \code{"character"}, complex
      ID of the source database }
    \item{\code{shortLabel}:}{Object of class \code{"character"}, short
      label of the complex}
    \item{\code{fullName}:}{Object of class \code{"character"}, full
      name of the complex}
    \item{\code{organismName}:}{Object of class \code{"character"},
      orgnism name}
    \item{\code{taxId}:}{Object of class \code{"character"}, taxonomy ID}
    \item{\code{members}:}{Object of class \code{"data.frame"}, members
      (UniProt ID if available)}
    \item{\code{attributes}:}{Object of class \code{"character"},
      attributes of the complex }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "psimi25Complex")}: shows
      information of the complex }
    \item{sourceDb}{\code{signature(x = "psimi25Complex")}: returns source database }
    \item{sourceId}{\code{signature(x = "psimi25Complex")}: returns
      source ID }
    \item{complexAttributes}{\code{signature(x = "psimi25Complex")}: returns
      attributes of the complex}
    \item{complexMembers}{\code{signature(x = "psimi25Complex")}: returns
      members of the complex}
    \item{complexName}{\code{signature(x = "psimi25Complex")}: returns
      complex name }
	 }
}
\author{ Jitao David Zhang <j.zhang@dkfz.de>, Tony Chiang <tchiang@ebi.ac.uk> }
\seealso{\code{\link{parsePsimi25Complex}}}
\examples{
showClass("psimi25Complex")
}
\keyword{classes}
