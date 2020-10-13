DEFINE CLASS conexion as base
	FUNCTION conecta
		LOCAL _ncon,_cadena
		WITH this

		_cadena=.ruta + .db+".dbc"
			IF FILE(_cadena)
				IF DBUSED(.db)
					SET DATABASE to (.db)
				ELSE
					*!*						.ncon= sqlstringconnect(_cadena)
					open data (_cadena)
					SET DATABASE to (.db)
					*!*	IF .ncon>0
					*!*							******YA ESTAS CONECTADO.
					*!*						endif
				ENDIF
				.ncon=1
			ELSE
				.ncon=-1
			ENDIF
			RETURN .ncon
		ENDWITH
	ENDFUNC
	PROCEDURE getncon
		RETURN this.ncon
	ENDPROC
	FUNCTION desconecta
		IF this.ncon>0
			SET DATABASE to (this.db)
			CLOSE DATABASES 
		endif
		this.ncon=0
	ENDFUNC
ENDDEFINE
