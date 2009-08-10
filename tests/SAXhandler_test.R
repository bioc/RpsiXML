library(RpsiXML)
library(XML)
filename <- system.file("extdata/psi25files/biogrid_200804_test.xml",
                        package="RpsiXML")

##xmlInternalTreeParse(filename,
##             handlers=list(names=psimi25NamesTypeHandler,
##               primaryRef=psimi25DbReferenceTypeHandler,
##               attributeList=psimi25AttributeListTypeHandler,
##               availabilityList=psimi25AvailabilityTypeListHandler,
##               experimentDescription=psimi25ExperimentTypeHandler,
##               interactionDetectionMethod=psimi25CvTypeHandler,
##               xref=psimi25XrefTypeHandler))
