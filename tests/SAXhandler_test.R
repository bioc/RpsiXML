
#filename <- system.file("extdata/psi25files/biogrid_200804_test.xml",
#                        package="RpsiXML")

#testPR <- list()
#test <- xmlEventParse(filename,
#              list(startElement=function(name, attrs, ...){
#                cat(name,"\n")
#              }, primaryRef=psixmlHandlerPrimaryRef),
#              useTagName=TRUE, addContext = TRUE)
