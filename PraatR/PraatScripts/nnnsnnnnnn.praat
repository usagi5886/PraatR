form PraatR
	word type
	word command
	real a1
	real a2
	real a3
	word a4
	real a5
	real a6
	real a7
	real a8
	real a9
	real a10
	sentence input
	sentence output
	word filetype
	boolean simplify
endform

do ( "Read from file...", input$ )

if type$="Create" | type$="Modify"
	do ( replace$ (command$, "__", " ", 0), a1, a2, a3, replace$ (a4$, "__", " ", 0), a5, a6, a7, a8, a9, a10 )
	if filetype$="binary"
		do ("Save as binary file...", output$)
	elsif filetype$="short"
		do ("Save as short text file...", output$)
	else
		do ("Save as text file...", output$)
	endif
elsif type$="Query"
	result_string$ = do$ ( replace$ (command$, "__", " ", 0), a1, a2, a3, replace$ (a4$, "__", " ", 0), a5, a6, a7, a8, a9, a10 )
	if simplify
		result_numeric = number( result_string$ )
		writeInfoLine( result_numeric )
	else
		writeInfoLine( result_string$ )
	endif
elsif type$="Play"
	do ( replace$ (command$, "__", " ", 0), a1, a2, a3, replace$ (a4$, "__", " ", 0), a5, a6, a7, a8, a9, a10 )
endif
