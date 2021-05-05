FROM rocker/r-devel
RUN RD -e "devtools::install_github('Accio/RpsiXML')"
