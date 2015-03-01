	.ORIG	x3000
        unknown r1, r1, r2
	AND	R1, R1, x0	;comment
	AND	R4, R4, x0	;comment
	ADD	R4, R4, xA	;comment
LOOP	LDR	R2, R2, #0
	ADD	R2, R2, 1	;comment
	ADD	R1, R1, R3	;comment
	ADD	R4  R4  x-1	;comment

	BRp	LOOP		;comment
	HALT
ARRAY   .BLKW 4 0
STRING  .STRINGZ "hello\""
STRING  .STRINGZ "world"        
	.END
        don't care
