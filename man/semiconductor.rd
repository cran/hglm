\name{semiconductor}
\docType{data}
\alias{semiconductor}
\title{Semiconductor data set from GenStat.}
\description{
 The semiconductor data set is obtained from a 2\eqn{\mbox{\textasciicircum}}{^}(6-2) factorial design conducted in a semiconductor plant. The design variables, Lamination (3 factors; Temperature, Time and Pressure) and Firing  (3 factors; Temperature, Cycle Time and Dew Point), are each taken at two levels. The goal of the original data analysis was to model the curvature or camber (taken in 1e-4 in./in.) as a function of the desing variables. The data set is taken from GenStat 11.1. It is also used in Lee et al. (2006) where Mayers et al. (2002) is reffered to as the the original source of the data.   
}
\format{ This data set contains 64 rows and the following columns
\describe{
\item{Device}{Subtrate device}
\item{x1}{Lamination Temperature; two levels +1 and -1.}
\item{x2}{Lamination Time; two levels: +1 and -1.}
\item{x3}{Lamination Presure; two levels: +1 and -1.}
\item{x4}{Firing Temperature; two levels: +1 and -1.}
\item{x5}{Firing Cycle Time; two levels: +1 and -1.}
\item{x6}{Firing Dew Point: two levels: +1 and -1.}
\item{y}{Camber measure; in 1e-4 in./in.}
 }
}
\source{GenStat(R) Release 11.1. VSN International Limited.}
\references{Lee, Y. and Nelder J. A., and Pawitan, Y. 2006. \emph{Generalized Linear Models with Random Effectes}, Chapman and Hall/CRC. \cr
Mayers, P. H., Montgomery, D. C. and Vining G. G. 2002. \emph{Generalized Linear Models with Application in Engineering and Science}, John Wiley and Sons.
}
\keyword{datasets}