\name{PraatR}

\alias{PraatR}

\docType{package}

\title{An architecture for controlling the phonetics software 'Praat'}

\description{
PraatR is an architecture for controlling the phonetics software Praat (\url{http://www.fon.hum.uva.nl/praat/}) with R code. The package's core function, \code{\link{praat}()}, sends a shell command to the operating system that invokes the command-line form of Praat with an associated Praat script that imports a file, applies a Praat command to it, and then either brings the output directly into R or exports the output as a textfile.}

\details{
\tabular{ll}{
Package: \tab PraatR\cr
Type: \tab Package\cr
Version: \tab 2.1\cr
Date: \tab 2014-11-05\cr
License: \tab GNU General Public License (Version 3 or later)\cr
}
For information on how to use \code{\link{praat}()}, see \url{ http://www.aaronalbin.com/praatr/ }. For information about the various commands that can be executed, see \url{ http://www.aaronalbin.com/praatr/commands.html }.
}

\author{
Aaron Albin (\url{http://www.aaronalbin.com/})
}

\references{
Albin, A. (2014). PraatR: An architecture for controlling the phonetics software "Praat" with the R programming language. \emph{Journal of the Acoustical Society of America}, \emph{135}(4), 2198. \url{http://scitation.aip.org/content/asa/journal/jasa/135/4/10.1121/1.4877175}
}

\keyword{ package }

\seealso{
\code{\link{praat}}
}
