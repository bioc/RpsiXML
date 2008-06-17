\name{interactors}
\alias{interactors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interactors of the given object }
\description{
  Return the interactors of the given object
}
\usage{
interactors(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ either an object of \code{\link{psimi25InteractionEntry-class}}
    or \code{\link{psimi25ComplexEntry-class}} }
}
\value{
  A matrix of interactor information
}
\author{ Tony Chiang <tchiang@ebi.ac.uk>, Jitao David Zhang <j.zhang@dkfz.de>}
\seealso{ \code{\link{psimi25InteractionEntry-class}},
  \code{\link{psimi25ComplexEntry-class}}
}
\examples{
xmlDir <- system.file("/extdata/psi25files",package="RpsiXML")

hprdxml <- file.path(xmlDir, "hprd_200709_test.xml")
hprdSet <- parsePsimi25Interaction(hprdxml, HPRD.PSIMI25)

interactors(hprdSet)

## Not run:
intactComplexxml <- file.path(xmlDir,"intact_complexSample.xml")
intactComplexSet <- parsePsimi25Complex(intactComplexxml,
INTACT.PSIMI25)

interactors(intactComplexSet)
## End(Not run)
}
\keyword{ models }

