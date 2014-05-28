form PraatR
	word type
	word command
	word a1
	real a2
	sentence input
	sentence output
	word filetype
	boolean simplify
endform

do ( "Read from file...", input$ )

if type$="Create" | type$="Modify"
	do ( replace$ (command$, "__", " ", 0), replace$ (a1$, "__", " ", 0), a2 )
	if filetype$="binary"
		do ("Save as binary file...", output$)
	elsif filetype$="short"
		do ("Save as short text file...", output$)
	else
		do ("Save as text file...", output$)
	endif
elsif type$="Query"
	result_string$ = do$ ( replace$ (command$, "__", " ", 0), replace$ (a1$, "__", " ", 0), a2 )
	if simplify
		result_numeric = number( result_string$ )
		writeInfoLine( result_numeric )
	else
		writeInfoLine( result_string$ )
	endif
elsif type$="Play"
	do ( replace$ (command$, "__", " ", 0), replace$ (a1$, "__", " ", 0), a2 )
endif
