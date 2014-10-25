\name{praat}

\alias{praat}

\title{
Execute a Praat command
}

\description{
Sends a shell command to the operating system that invokes the command-line form of Praat with an associated Praat script that imports a file, applies a Praat command to it, and then either brings the output directly into R or exports the output as a textfile.
}

\usage{
praat( command, arguments, input, output, overwrite, filetype, simplify )
}

\arguments{
  \item{command}{
A character string indicating what command should be executed.
At present, 972 different commands can be applied to 100 of Praat's different object classes, for a total of 2163 unique combinations of command + object.
For the full list, see \url{ http://www.aaronalbin.com/praatr/commands.html }.
}

  \item{arguments}{
Only required for certain commands.
A list() of the arguments to the Praat command.
These must be in the exact same order as used in Praat.
The components of the list can optionally be named, but these names are ignored by PraatR.
}

  \item{input}{
The path to the file that the specified Praat command takes as input.
This should be the full path on your hard drive (not merely the file name) and must not contain any spaces.
}

  \item{output}{
Required for 'Create' commands, and optional for 'Modify' commands.
The path for the file that will be created once the specified Praat command is executed.
This should be the full path on your hard drive (not merely the file name) and must not contain any spaces.
If omitted for a 'Modify' command, this will be assumed to be the same as the \code{input} argument.
}

  \item{overwrite}{
Only relevant for 'Create' and 'Modify' commands.
If the file specified in 'output' already exists, indicates whether that file should be over-written with the file created by executing the Praat command.
Defaults to \code{FALSE} for 'Create' commands and \code{TRUE} for 'Modify' commands.
}

  \item{filetype}{
Only relevant for 'Create' and 'Modify' commands.
Determines what file format the output text file will have.
All 'Create' and 'Modify' commands have three choices: \code{"text"}, \code{"short text"}, and \code{"binary"}.
(\code{"short"} can be used as an alias for \code{"short text"}.)
In addition, certain special file types are also allowed for certain commands (e.g. saving a Sound object in WAV format).
See the 'Supported Commands' page on the PraatR site for information on which file types are allowed for which commands.
When omitted, defaults to \code{filetype="text"}.
}

  \item{simplify}{
Only relevant for 'Query' commands.
If \code{TRUE}, the requested information will be brought into R as a text string (potentially along with additional information, e.g. the unit of a measurement).
If \code{FALSE}, any such additional information will be removed, hence only the number data itself is returned (as a text string, which can then be passed to \code{as.numeric()}, etc.)
When omitted, defaults to \code{simplify=FALSE}.
}
}

\details{
The only two arguments that are obligatory (i.e. required for every possible command) are \code{command} (which command to execute) and \code{input} (what file to work with).
Depending on the command, you may also need to provide the \code{arguments} required by Praat in order for it to know exactly how to execute that command.
The rest of the arguments depend on what kind of command you are executing:
\itemize{
\item{For a 'Play' command, no additional arguments need to be specified.}
\item{For a 'Create' command, you must specify \code{output}; \code{overwrite} and \code{filetype} are optional.}
\item{For a 'Modify' command, \code{output}, \code{overwrite}, and \code{filetype} are all optional.}
\item{For a 'Query' command, \code{simplify} is optional.}
}
}

\value{
This function is largely used for its 'side-effect' - controlling Praat.
The exception is 'Query' commands, which return a value to R in one of two different formats.
(See the documentation of the \code{simplify} argument above.)
}

\references{
Albin, A. (2014). PraatR: An architecture for controlling the phonetics software "Praat" with the R programming language. \emph{Journal of the Acoustical Society of America}, \emph{135}(4), 2198. \url{http://scitation.aip.org/content/asa/journal/jasa/135/4/10.1121/1.4877175}
}

\author{
Aaron Albin (\url{http://www.aaronalbin.com/})
}

\note{
PraatR is currently only supported on Windows and Mac.
\itemize{
\item{In order for PraatR to work on Windows, you must have downloaded a separate file called \code{praatcon.exe} and placed it your PraatR library folder.}
\item{In order for PraatR to work on Mac, Praat needs to be installed on your computer in the following location: \code{/Applications/Praat.app}.}
}
For details, see the 'Installation' section of the PraatR homepage.
}

\seealso{
Further information - including installation instructions, a basic tutorial, and the list of supported commands - is available at the PraatR homepage: \url{http://www.aaronalbin.com/praatr/}.
}

\examples{
# The following examples are commented out because the file paths need to be adjusted for your
#    computer (so the files and folders actually exist).

# Example 'Play' command
#praat( "Play", input="C:/sound.wav" )
# Has Praat read in the the soundfile 'C:/sound.wav' and then execute the 'Play' command on it

# Example 'Create' command
#praat( "Down to PitchTier", input="C:/sound.Pitch", output="C:/sound.PitchTier" )
# Takes a Pitch object, creates a PitchTier object out of it, and saves the result

# Example 'Modify' command
#praat( "Insert boundary...", input="C:/sound.TextGrid", arguments=list(1, 0.5) ) 
# Takes a TextGrid and inserts a boundary at 0.5 seconds ('0.5') on the first tier ('1')

# Example 'Query' command
#praat("Get mean...", input="C:/sound.Formant", arguments=list(1,0,0,"Hertz"), simplify=TRUE )
# Reads in a Formant object, retrieves the mean value of formant '1' from time '0' to time '0'
# (i.e. the entirety of the file), and returns only the number (omitting the suffix " Hertz")
}

\keyword{ interface }
\keyword{ file }
\keyword{ utilities }
