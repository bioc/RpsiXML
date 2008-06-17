\name{taxId}
\alias{taxId}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ The taxonomy ID of the given parameter}
\description{
  Returns the taxonomy ID of the object
}
\usage{
taxId(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An object of \code{\link{psimi25InteractionEntry-class}} or \code{\link{psimi25Complex-class}}}
}
\value{
  A character string of taxIDs
}
\references{ PSI-MI 2.5 XML data exchange format, \url{http://www.psidev.info/index.php?q=node/60} }
\author{ Jitao David Zhang <j.zhang@dkfz.de>, Tony Chiang <tchiang@ebi.ac.uk> }
\seealso{ \code{S4CLASS\link{psimi25InteractionEntry-class}} }
\examples{
xmlDir <- system.file("/extdata/psi25files",package="RpsiXML")

hprdxml <- file.path(xmlDir, "hprd_200709_test.xml")
hprdSet <- parsePsimi25Interaction(hprdxml, HPRD.PSIMI25)

taxId(hprdSet)
}
\keyword{ models }
