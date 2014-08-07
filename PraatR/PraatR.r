############
# PraatR.r #
############

# Created by: Aaron Albin
# http://www.aaronalbin.com/praatr/

################################################################################################
# This program is free software. You can redistribute it and/or modify it under the terms of   #
#    the GNU General Public License as published by the Free Software Foundation -             #
#    either version 3 of the License, or (at your option) any later version.                   #
# This program is distributed in the hope that it will be useful, but without any warranty -   #
#    without even the implied warranty of merchantability or fitness for a particular purpose. #
# For details on the GNU General Public License, see: http://www.gnu.org/licenses/             #
################################################################################################

###########################
# Load supported commands #
###########################

SupportedCommands = function(){
# Search for the 'SupportedCommands' file in all possible locations
LibraryDirectories = unique( c( R.home("library"), .libPaths() ) )

# For reference, on a Mac, R.home("library") is...
# "/Library/Frameworks/R.framework/Resources/library"
# ...whereas one of the directories included in the vector .libPaths() should be something like...
# "/Library/Frameworks/R.framework/Versions/3.0/Resources/library"

nDirectories = length(LibraryDirectories)

# Add slashes if necessary
LastCharacters = substring( LibraryDirectories,first=nchar(LibraryDirectories),last=nchar(LibraryDirectories) )
EndWithSlash = LastCharacters == "/" # | LastCharacters == "\\"
InterveningSlashes = rep("",times=)
InterveningSlashes[!EndWithSlash] <- "/"

# Make list of possible file paths
SupportedCommands_Paths = paste(LibraryDirectories,InterveningSlashes,"PraatR/SupportedCommands.txt",sep="")

# Find which ones exist
ExistingPaths = file.exists(SupportedCommands_Paths)

# Issue an error message if SupportedCommands.txt could not be found and stop computation
if( sum(ExistingPaths)==0 ){ stop("Could not find 'SupportedCommands.txt'. Make sure PraatR is properly installed.") }

# Use the first path that exists
# Note that the order is 'unique( c( R.home("library"), .libPaths() ) )' (cf. code above)
PathToLoad = SupportedCommands_Paths[min( which(ExistingPaths) )]

SupportedCommands = read.table(PathToLoad, sep="\t", header=TRUE, quote="")
assign(x="SupportedCommands",value=SupportedCommands,env=.GlobalEnv)
} # End function 'SupportedCommands'

# Now do a little trick - call the function, whose final line 'assign()' replaces the function with the actual data frame under the same object name
# This avoids the clutter of the various other variables created along the way to determine the SupportedCommands path, which could overlap and interfere with the user's variable names.
SupportedCommands()

####################
# praat() function #
####################

praat = function( # Begin argument list

# [1]
command,
# A character string indicating what command should be executed
# This will be checked against the 'SupportedCommands' database, and if it's not found there, an error message will be issued.

# [2]
arguments,
# A list() of arguments.
# This data structure is used because it is possible to mix of various classes, e.g. list("character", 12.34, 5, TRUE) 

# [3]
input,
# The (ideally full) path to the input file.

# [4]
output,
# The (ideally full) path to the output file.

# [5]
overwrite,
# Only applicable if the user chooses a 'Create' or 'Modify' command. (If they choose a 'Query' command, this will be ignored.)
# This indicates whether the command should be executed if the file specified in 'output' already exists (thereby overwriting it).
# By defaulting to FALSE, the user is protected from accidentally erasing data.
# It is very easy to turn this off, and the user feels like they have more control if they do.

# [6]
filetype,
# Only applicable if the user chooses a 'Create' or 'Modify' command.
# Determines what file format the output text file will have
# Three choices:
# "text"   = Save as text file...
# "short"  = Save as short text file...
# "binary" = Save as binary file...

# [7]
simplify
# Only applicable if the user chooses a 'Query' command.
# Determines whether the queried information will be coerced to numeric (i.e. just the core data) rather than remain as a text string (potentially along with various kinds of embellishing explanatory information).
# Either way, the result will (initially) be brought into R as a character string.
# The user specifies this as TRUE or FALSE. This later gets translated into 1 or 0 when passed to Praat.

){ # Begin actual body of function

####################
# VALIDATE COMMAND #
####################

# First check whether the command is one of the official list of supported ones
# (Note that, at present, this does not take into account the object type, and only considers the command itself.)

ValidCommand = command %in% SupportedCommands$CommandName

# Argument [1]
if( !ValidCommand ){ stop("This command is not supported by the present version of PraatR.\n       Double-check that this is indeed a real Praat command, and then\n       feel free to contact the creator of PraatR.") }

#############################
# CLASSIFY SUPPLIED COMMAND #
#############################

# Generally speaking, there is a one-to-one mapping between CommandNames and CommandTypes.
# Within the current set of supported commands, there are three exceptions.
# (1) The command 'Rotate...' is of type 'Modify' for a Configuration or Polygon object but of type 'Create' for a Permutation object.
# (2-3) The commands 'Filter (pass Hann band)...' and 'Filter (stop Hann band)...' are of type 'Modify' for a Spectrum object but of type 'Create' for a Sound object.
# Fortunately, these conflations do not involve queries.
# Thus, I can safely assume that randomly sampling one of the two in these ambiguous cases will not obscure the [+/-query] distinction.
# (I checked and everything I say above doesn't change based on the inclusion of 'Play' commands.)

RowIndex = which( command == SupportedCommands$CommandName )[1]
# For the three commands mentioned above, this will arbitrarily choose whichever comes first in terms of the ordering of the rows in the dataframe.

CommandType = as.character( SupportedCommands[RowIndex,"CommandType"] )
# At present, only three types are supported: Create, Modify, and Query.

############################################
# INPUT VALIDATION AND ARGUMENT DEFAULTING #
############################################

# Argument [2]
# Make sure that, if the 'arguments' argument(!) is provided, it is a list
if(!missing(arguments)){
     if( class(arguments) != "list" ){ stop("The 'arguments' function must be a list.")}
} # End 'if the user provided something as 'arguments'
# Ultimately, this will need to be MUCH more rigid - checking the user's specified arguments against those from the SupportedCommands database to make sure everything lines up OK.
# There *can* be arguments for 'Play' commands, so this is required even there.

# Argument [3]
# Make sure the input file actually exists on the user's hard drive.
if( !file.exists(input) ){ stop(paste("The file specified as the 'input' argument does not exist:\n       ",input,sep="")) }

# - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = 
# = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -

if(CommandType=="Create"){

# Argument [4]
# If the output is left unspecified, then issue an error and stop computation.
if( missing(output) ){ stop("For this command, you must specify a file path for the 'output' argument.") }
# The user can name the output file itself whatever they want.
# However, I need to make sure the folder where they indicated that it should be placed actually exists (beyond the mere checking whether it equals "R" just performed).
if( !file.exists( dirname(output) ) ){ stop(paste("The path in the 'output' argument includes a folder that does not exist:\n       ",dirname(output),sep="")) }
# file.exists() works fine with directories. However, on Windows it must NOT end in a slash. dirname() does things correctly, so I'm fine.

# Argument [5]
# Fill in 'overwrite' with its default of FALSE if left unspecified
if(missing(overwrite)){overwrite=FALSE}
# If the 'overwrite' argument is set to FALSE, then if the output file already exists, cease computation
if( overwrite==FALSE & file.exists(output) ){ stop(paste("This 'output' file already exists: (Consider setting overwrite=TRUE.)\n       ",output,sep="")) }

} # End if this is a Create command 

# - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = 
# = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -

if(CommandType=="Modify"){ # Then the interplay between 'overwrite' and 'output' is very complex...

if( missing( output ) ){ output <- input } # Assume that the user intends the output file to be the same as the input file

if( missing(overwrite) ){ # i.e. in the general case where the 'overwrite' argument is left unspecified

if(output==input){ #... either by manual specification or the assumption from above
overwrite=TRUE # Note that this is different from a 'Create' command
}else{ # i.e. if the output file is manually specified to be something other than the input file
if( file.exists(output) ){ stop(paste("This 'output' file already exists: (Consider setting overwrite=TRUE.)\n       ",output,sep="")) } # This is the normal stuff; same as 'Create'
} # End 'if/else output is the same as input'

}else{ # i.e. if the user explicitly mentions 'overwrite' and assign some value to it in their function call

# If overwrite is TRUE, then write to the output file regardless of whether it is the same as input or not. So no need to even check that.
# Rather, have an if statement to check if the user manually specified 'overwrite=FALSE' (for some reason)
if(overwrite==FALSE){ # i.e. if the user explicitly says 'overwrite=FALSE' in their function call

if(output==input){ # i.e. if they are one-and-the-same

stop("Either change 'overwrite' to TRUE or specify an output file different from the input.")

}else{ # i.e. if the output and input are different

if( file.exists(output) ){ stop(paste("This 'output' file already exists: (Consider setting overwrite=TRUE.)\n       ",output,sep="")) }

} # End if/else output is the same as input

} # End 'if overwrite is FALSE'

} # End 'if/else overwrite is left unspecified'

} # End if this is a Modify command 

# - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = 
# = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -

if(CommandType=="Create" | CommandType=="Modify"){

# Argument [6]
# Fill in 'filetype' with its default of "text" if left unspecified
if(missing(filetype)){
filetype="text"
}else{ # i.e. if something has been supplied
# Check to see whether it is a legal possibility for the file type
LegalFileType = ( filetype %in% c("text","short","binary") )
if(!LegalFileType){stop("The 'filetype' argument must be \"text\", \"short\", or \"binary\".")}
# If it is legal, do nothing and move on
} # End if/else filetype is missing

# Argument [7]
# Argument 'simplify' should be missing. If not, issue a warning. If it is, fill it with a dummy '0' so something can be passed to Praat. (It must be boolean in order for the form to work.)
if(!missing(simplify)){warning("For Create/Modify commands, leave 'simplify' unspecified; the supplied value has been ignored.")}else{simplify=FALSE}

} # End if this is a Create or Modify command 

# - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = 
# = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -

if(CommandType=="Query"){

# Argument [4]
# If something is specified for 'output', issue a warning that the specified output argument has been ignored, but still proceed normally.
if( !missing(output) ){
warning("For Query commands, leave 'output' unspecified; the supplied value has been ignored.")
}else{ # if it is indeed missing (as it should be), then fill it with a dummy 'X' just to make sure something gets passed to the Praat form.
output="X"
} # End if/else

# Argument [5]
# Argument 'overwrite' should be missing. If not, issue a warning.
if(!missing(overwrite)){warning("For Query commands, leave 'overwrite' unspecified; the supplied value has been ignored.")}
 # No need for dummy variable since not passed to Praat
 
# Argument [6]
# Argument 'filetype' should be missing. If not, issue a warning. If it is, fill it with a dummy 'X' so something can be passed to Praat
if(!missing(filetype)){warning("For Query commands, leave 'filetype' unspecified; the supplied value has been ignored.")}else{filetype="X"}

# Argument [7]
# Fill in 'simplify' with its default of FALSE if left unspecified
if(missing(simplify)){simplify=FALSE}

} # End if this is a Query command 

# - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = 
# = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -

if(CommandType=="Play"){
# All four of the remaining commands are moot, so do similar things to above

# Argument [4]
if( !missing(output) ){ warning("For Play commands, leave 'output' unspecified; the supplied value has been ignored.")
}else{ output="X" } # Dummy

# Argument [5]
if(!missing(overwrite)){warning("For Play commands, leave 'overwrite' unspecified; the supplied value has been ignored.")} # No need for dummy

# Argument [6]
if(!missing(filetype)){warning("For Play commands, leave 'filetype' unspecified; the supplied value has been ignored.")
}else{filetype="X"} # Dummy

# Argument [7]
if(!missing(simplify)){warning("For Play commands, leave 'simplify' unspecified; the supplied value has been ignored.")
}else{simplify=FALSE} # Dummy

} # End if this is a Play command 

# - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = 
# = - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -

############################
# ADD UNDERBARS TO COMMAND #
############################

# The command cannot have any spaces when passed to shell(), so replace them with double-underscores as a delimiter.
# These will then be converted back to spaces inside the Praat functions.
# I can't use a single underscore because of the following five command names:
# 1) "Get shimmer (local_dB)..."
# 2) "Get ln(determinant_group)..."
# 3) "Get ln(determinant_total)"
# 4) "To HMM_ObservationSequence..."
# 5) "To HMM_StateSequence"
UnderbarCommand = gsub(command, pattern=" ", replacement="__")

#####################
# PROCESS ARGUMENTS #
#####################

if(missing(arguments)){

TargetScriptName="ZeroArguments.praat"
ArgumentString = ""

}else{ # i.e. if there are any arguments

# Find out how many arguments there are
nArguments = length( arguments )

# First coerce any TRUE to "yes" and any FALSE to "no"
LogicalCoerced = lapply(arguments,FUN=function(x){
		if(is.logical(x)){
			return(c("no","yes")[as.integer(x)+1])
		}else{
			return(x)
		} # End if/else
	} # End function definition
) # End call to lapply()

# Now classify each argument in terms of whether it is a character string or not
# (At this point, it's assumed that anything that is not a string is numeric - in R terms, either 'integer' or 'numeric' proper.)
IsString = sapply(LogicalCoerced,FUN=is.character)
ArgumentClassifications = c("n","s")[as.integer(IsString)+1] # Only where needed

# Now use these classifications to determine which script to open
CollapsedClassifications = paste(ArgumentClassifications,collapse="")
TargetScriptName = paste(CollapsedClassifications,".praat",sep="")

# Here again, change spaces in the argument names to double-underscores
# They can't be single underscores again because of argument names like "Kirshenbaum_espeak".
UnderscoreSwapped = lapply(1:nArguments,FUN=function(n){
		if(IsString[n]){
			return(gsub(LogicalCoerced[[n]], pattern=" ", replacement="__"))
		}else{
			return(LogicalCoerced[[n]])
		} # End if/else
	} # End function definition
) # End call to lapply()

# Now separate the arguments by spaces (so as to fit the syntax of shell() )
ArgumentString = paste( UnderscoreSwapped, collapse=" ")

} # End if/else arguments are missing from the function call

##############################################
# Adjust based on user operating system (OS) #
##############################################

UserOS = .Platform$OS.type # "windows" for a Windows 8, "unix" for Linux or Mac.
# Linux vs. Mac can be distinguished by querying Sys.info()["sysname"], which returns "Windows", "Linux", or "Darwin" (=Mac).
# (When Linux support is added, perhaps just use this instead?)

# In Windows, R.home("library") is something like "C:/PROGRA~1/R/R-30~1.3/library", which lacks a space, so this should always work fine.
if( ! UserOS %in% c("windows","unix")){stop("Operating system not supported. .Platform$OS.type must be either 'windows' or 'unix'.")}
if(UserOS == "windows"){ PraatPath = paste(R.home("library"),"PraatR","praatcon.exe",sep="/") }
if(UserOS == "unix"   ){ PraatPath = "/Applications/Praat.app/Contents/MacOS/Praat" } # Presumably it will always be in this one fixed/stable location

# Do a similar string of checks to how the SupportedCommands path was found
# (Most of this code is copied from there.)
LibraryDirectories = unique( c( R.home("library"), .libPaths() ) )
nDirectories = length(LibraryDirectories)

# Add slashes if necessary
LastCharacters = substring( LibraryDirectories,first=nchar(LibraryDirectories),last=nchar(LibraryDirectories) )
EndWithSlash = LastCharacters == "/" # | LastCharacters == "\\"
InterveningSlashes = rep("",times=)
InterveningSlashes[!EndWithSlash] <- "/"

# Make list of possible file paths
PossibleScriptPaths = paste(LibraryDirectories,InterveningSlashes,"PraatR/PraatScripts/",TargetScriptName,sep="")

# Find which ones exist
ExistingPaths = file.exists(PossibleScriptPaths)

# Issue an error message if the appropriate Praat script can not be found and stop computation
if( sum(ExistingPaths)==0 ){ stop("Could not find the appropriate Praat script. Make sure PraatR is properly installed.") }

# Use the first path that exists
# Note that the order is 'unique( c( R.home("library"), .libPaths() ) )' (cf. code above)
ScriptPath = PossibleScriptPaths[min( which(ExistingPaths) )]

###########################
# Assemble command string #
###########################

# The checks performed above guarantee that the CommandType is one of the three I'm dealing with, so do simple if() statements to check which is the case, and react accordingly

# Don't make things if/else (between Create/Modify/Play and Query for right now because I'm not sure whether I'll include other CommandTypes in the future... And this is safer and more transparent anyway.

# Use shQuote() for all file paths to protect in case they contain spaces
if(CommandType == "Create" | CommandType == "Modify" | CommandType == "Play"){
# These three are treated 100% equivalently for the time being, but it's in principle possible to separate them down the road

CommandString = paste( PraatPath,
                       ScriptPath,
                       CommandType,
                       UnderbarCommand,
                       ArgumentString, # If empty, this will result in two neigboring spaces, but this isn't a problem
                       input,
                       output,
                       filetype,
                       as.integer(simplify), # Always logical until now; converted to integer at the last minute before going to Praat
                       sep=" ")

intern = FALSE # Do *not* capture anything from the Info Window of Praat (for bringing back into R)

# Now, finally issue the instruction to the OS
if(UserOS == "windows"){ shell(cmd=CommandString, intern=intern) } # For Windows
if(UserOS == "unix"){ system(command=CommandString, intern=intern) } # For Unix/Mac
# Eventually, somehow detailedly check the response status from this function call and issue custom messages accordingly to help the user troubleshoot if there are any problems?

} # End 'if this is a query command'

if(CommandType == "Query"){

CommandString = paste( PraatPath,
                       "-a", # This switch to the Praat program makes the output go back into R through the 'standard input' (stdin)
                       ScriptPath,
                       CommandType,
                       UnderbarCommand,
                       ArgumentString,
                       input,
                       output,
                       filetype,
                       as.integer(simplify),
                       sep=" ")

intern = TRUE # This indicates that the ultimate contents of the Info Window in Praat should be captured and brought back into R

# Now, finally issue the instruction to the OS, and return the result
if(UserOS == "windows"){ return(shell(cmd=CommandString, intern=intern)) } # For Windows
if(UserOS == "unix"){ return(system(command=CommandString, intern=intern)) } # For Unix/Mac

} # End 'if this is a query command'

} # End function

###################
# Welcome message #
###################

# Run this last in case something earlier above failed
# Eventually have multiple calls to stop() above to ensure this won't be shown if it failed to load.

cat("\n######################\n# Welcome to PraatR! #\n######################\n\nFor documentation on how to use PraatR and information on how to cite it,\n    visit the homepage at http://www.aaronalbin.com/praatr/\nPraatR is released under the the GNU General Public License: http://www.gnu.org/licenses/.\n\n\n")

