form PraatR
	word type
	word command
	word a1
	word a2
	word a3
	word a4
	sentence input
	sentence output
	word filetype
	boolean simplify
endform

do ( "Read from file...", input$ )

if type$="Create" | type$="Modify"
	do ( replace$ (command$, "__", " ", 0), replace$ (a1$, "__", " ", 0), replace$ (a2$, "__", " ", 0), replace$ (a3$, "__", " ", 0), replace$ (a4$, "__", " ", 0) )
	if filetype$="binary"
		do ("Save as binary file...", output$)
	elsif filetype$="short"
		do ("Save as short text file...", output$)
	else
		do ("Save as text file...", output$)
	endif
elsif type$="Query"
	result_string$ = do$ ( replace$ (command$, "__", " ", 0), replace$ (a1$, "__", " ", 0), replace$ (a2$, "__", " ", 0), replace$ (a3$, "__", " ", 0), replace$ (a4$, "__", " ", 0) )
	if simplify
		result_numeric = number( result_string$ )
		writeInfoLine( result_numeric )
	else
		writeInfoLine( result_string$ )
	endif
elsif type$="Play"
	do ( replace$ (command$, "__", " ", 0), replace$ (a1$, "__", " ", 0), replace$ (a2$, "__", " ", 0), replace$ (a3$, "__", " ", 0), replace$ (a4$, "__", " ", 0) )
endif
