form PraatR
	word type
	word command
	word a1
	word a2
	word a3
	word a4
	word a5
	word a6
	sentence input
	sentence output
	word filetype
	boolean simplify
endform

do ( "Read from file...", input$ )

if type$="Create" | type$="Modify"
	do ( replace$ (command$, "__", " ", 0), replace$ (a1$, "__", " ", 0), replace$ (a2$, "__", " ", 0), replace$ (a3$, "__", " ", 0), replace$ (a4$, "__", " ", 0), replace$ (a5$, "__", " ", 0), replace$ (a6$, "__", " ", 0) )
	do ("Save as " + replace$ ( replace$ (filetype$, "_", " ", 0), ".", "/", 0) + " file...", output$)
elsif type$="Query"
	result_string$ = do$ ( replace$ (command$, "__", " ", 0), replace$ (a1$, "__", " ", 0), replace$ (a2$, "__", " ", 0), replace$ (a3$, "__", " ", 0), replace$ (a4$, "__", " ", 0), replace$ (a5$, "__", " ", 0), replace$ (a6$, "__", " ", 0) )
	if simplify
		result_numeric = number( result_string$ )
		writeInfoLine( result_numeric )
	else
		writeInfoLine( result_string$ )
	endif
elsif type$="Play"
	do ( replace$ (command$, "__", " ", 0), replace$ (a1$, "__", " ", 0), replace$ (a2$, "__", " ", 0), replace$ (a3$, "__", " ", 0), replace$ (a4$, "__", " ", 0), replace$ (a5$, "__", " ", 0), replace$ (a6$, "__", " ", 0) )
endif
