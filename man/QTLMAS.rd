\name{QTLMAS}
\docType{data}
\alias{QTLMAS}
\title{Simulated Data Set for the QTLMAS 2009 Workshop}
\description{
The data was simulated for the QTLMAS 2009 workshop in Wageningen, The Netherlands. The data was made available at http://www.qtlmas2009.wur.nl/UK/Dataset/ and consists of markers, trait values and pedigree information. The original data set consisted of several traits and markers from several chromosomes, whereas the current data set included in this package consists of one trait ("P265"), pedigree information and data from 90 markers on chromosome number 1. There are 2025 individuals in the pedigree where 1000 individuals have trait values.
}
\format{A matrix containing 1000 rows and 2116 columns. The first column contains the trait values. 
Columns 2 to 2026 contains matrix Z, i.e. the pedigree information (as the Colesky factorization of the additive relationship matrix).
Columns 2027 to 2116 contains matrix Z.marker, i.e. the marker information for the 90 markers on chromosome 1.}
\source{QTLMAS 2009 Workshop http://www.qtlmas2009.wur.nl/UK/Dataset/}
\references{Coster, A., Bastiaansen J., Calus M., Maliepaard C. and Bink M. 2009. QTLMAS 2009: Simulated dataset. (submitted)

}
\keyword{datasets}