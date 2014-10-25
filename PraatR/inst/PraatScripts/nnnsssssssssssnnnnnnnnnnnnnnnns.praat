form PraatR
	word type
	word command
	real a1
	real a2
	real a3
	word a4
	word a5
	word a6
	word a7
	word a8
	word a9
	word a10
	word a11
	word a12
	word a13
	word a14
	real a15
	real a16
	real a17
	real a18
	real a19
	real a20
	real a21
	real a22
	real a23
	real a24
	real a25
	real a26
	real a27
	real a28
	real a29
	real a30
	word a31
	sentence input
	sentence output
	word filetype
	boolean simplify
endform

do ( "Read from file...", input$ )

if type$="Create" | type$="Modify"
	do ( replace$ (command$, "__", " ", 0), a1, a2, a3, replace$ (a4$, "__", " ", 0), replace$ (a5$, "__", " ", 0), replace$ (a6$, "__", " ", 0), replace$ (a7$, "__", " ", 0), replace$ (a8$, "__", " ", 0), replace$ (a9$, "__", " ", 0), replace$ (a10$, "__", " ", 0), replace$ (a11$, "__", " ", 0), replace$ (a12$, "__", " ", 0), replace$ (a13$, "__", " ", 0), replace$ (a14$, "__", " ", 0), a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, replace$ (a31$, "__", " ", 0) )
	do ("Save as " + replace$ ( replace$ (filetype$, "_", " ", 0), ".", "/", 0) + " file...", output$)
elsif type$="Query"
	result_string$ = do$ ( replace$ (command$, "__", " ", 0), a1, a2, a3, replace$ (a4$, "__", " ", 0), replace$ (a5$, "__", " ", 0), replace$ (a6$, "__", " ", 0), replace$ (a7$, "__", " ", 0), replace$ (a8$, "__", " ", 0), replace$ (a9$, "__", " ", 0), replace$ (a10$, "__", " ", 0), replace$ (a11$, "__", " ", 0), replace$ (a12$, "__", " ", 0), replace$ (a13$, "__", " ", 0), replace$ (a14$, "__", " ", 0), a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, replace$ (a31$, "__", " ", 0) )
	if simplify
		result_numeric = number( result_string$ )
		writeInfoLine( result_numeric )
	else
		writeInfoLine( result_string$ )
	endif
elsif type$="Play"
	do ( replace$ (command$, "__", " ", 0), a1, a2, a3, replace$ (a4$, "__", " ", 0), replace$ (a5$, "__", " ", 0), replace$ (a6$, "__", " ", 0), replace$ (a7$, "__", " ", 0), replace$ (a8$, "__", " ", 0), replace$ (a9$, "__", " ", 0), replace$ (a10$, "__", " ", 0), replace$ (a11$, "__", " ", 0), replace$ (a12$, "__", " ", 0), replace$ (a13$, "__", " ", 0), replace$ (a14$, "__", " ", 0), a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, replace$ (a31$, "__", " ", 0) )
endif
