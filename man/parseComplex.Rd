\name{parseComplex}
\alias{parseComplex}
\alias{parseInteractor}
\alias{parseExperiment}
\title{ Parse complex, interactor and experiment from PSI-MI 2.5 XML node}
\description{
  Wrappers of internal functions (".parsePsimi25Complex",
  ".parsePsimi25Interactor", ".parsePsimi25Experiment") to parse nodes
  from PSI-MI 2.5 XML node. By default these parsers is used in objects
  of \code{\link{psimi25Source-class}} without any modification. Though
  they can be overwritten in these objects in special cases, it is not
  advisory to do so due to the compatibility.
}
\usage{
parseComplex(x, ...)
parseInteractor(x, ...)
parseExperiment(x, ...)
}
\arguments{
  \item{x}{ An object of the \code{\link{psimi25Source-class}} }
  \item{\dots}{ Parameters passed to internal functions}
}
\value{
  Object of \code{\link{psimi25Complex-class},
    \code{\link{psimi25Interactor-class}} or
    \code{\link{psimi25Experiment-class}}
  }
}
\author{ Jitao David Zhang <j.zhang@dkfz.de>, Tony Chiang <tchiang@ebi.ac.uk> }
\note{
  An newly initialized instance of \code{\link{psimi25Source-class}} contains the default
  form of these functions. Though they are allowd to be overwritten, it is not
  advisory to do so since this means the file to parse is not
  fully compatible with PSI-MI 2.5 standard. It is more sensible to
  contact the data provider before modifying or overwritting these functions.
}
\keyword{ models }
