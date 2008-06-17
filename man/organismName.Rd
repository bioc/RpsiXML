\name{organismName}
\alias{organismName}
\title{ Organism name of the psimi25InteractionEntry }
\description{
  Returns the organism name(s) of the \code{psimi25InteractionEntry}
}
\usage{
organismName(x)
}
\arguments{
  \item{x}{ An object of \code{\link{psimi25InteractionEntry-class}} }
}
\value{
  A vector of character strings representing the organism names of the proteins
}
\author{ Jitao David Zhang <j.zhang@dkfz.de>, Tony Chiang <tchiang@ebi.ac.uk> }
\seealso{ \code{\link{psimi25InteractionEntry-class}} }
\examples{
xmlDir <- system.file("/extdata/psi25files",package="RpsiXML")
hprdxml <- file.path(xmlDir, "hprd_200709_test.xml")
hprdSet <- parsePsimi25Interaction(hprdxml, HPRD.PSIMI25)
organismName(hprdSet)
}
\keyword{ models }
