\name{complexName}
\alias{complexName}
\alias{complexMembers}
\alias{complexAttributes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Accessor functions for complex }
\description{
  These functions are used to extract useful information of complex in
  the form of \code{\link{psimi25Complex-class}} object.
}
\usage{
complexName(x)
complexMembers(x)
complexAttributes(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An object of \code{\link{psimi25Complex-class}} }
}
\details{
  See examples
}
\value{
  \item{complexName}{Returns the name of the complex in characters}
  \item{complexMembers}{A data frame of protein members building the
    complex and their information}
  \item{complexAttributes}{A named vector of character strings, the
    names are the attribute names while the values are attributes
    documented in the PSI-MI 2.5 file}
}
\author{ Jitao David Zhang <j.zhang@dkfz.de>, Tony Chiang <tchiang@ebi.ac.uk> }
\seealso{\code{\link{psimi25Complex-class}}}
\examples{
xmlDir <- system.file("/extdata/psi25files",package="RpsiXML")

intactComplexxml <- file.path(xmlDir,"intact_complexSample.xml")
intactComplexSet <- parsePsimi25Complex(intactComplexxml, INTACT.PSIMI25)
complexSample <- complexes(intactComplexSet)[[2]]

complexName(complexSample)
complexAttributes(complexSample)
complexMembers(complexSample)
}
\keyword{ models }
