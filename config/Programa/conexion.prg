*** 
*** ReFox XI+  #PT319423  desarrollo.comisesa  EMBRACE [VFP90]
***
**
DEFINE CLASS conexion AS base
**
   FUNCTION conecta
    LOCAL _ncon, _cadena
    WITH this
       _cadena = .ruta+.db+".dbc"
       IF FILE(_cadena)
          IF DBUSED(.db)
             SET DATABASE TO (.db)
          ELSE
             OPEN DATABASE (_cadena)
             SET DATABASE TO (.db)
          ENDIF
          .ncon = 1
       ELSE
          .ncon = -1
       ENDIF
       RETURN .ncon
    ENDWITH
   ENDFUNC
**
   FUNCTION getncon
    RETURN this.ncon
   ENDFUNC
**
   PROCEDURE desconecta
    IF this.ncon>0
       SET DATABASE TO (this.db)
       CLOSE DATABASES
    ENDIF
    this.ncon = 0
   ENDPROC
**
ENDDEFINE
**
*** 
*** ReFox - todo no se pierde 
***
