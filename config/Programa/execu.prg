*** 
*** ReFox XI+  #PT319423  desarrollo.comisesa  EMBRACE [VFP90]
***
**
DEFINE CLASS EXECU AS conexion
 PROTECTED _sql, _cursor, mensaje
**
   FUNCTION ExecSp
    LPARAMETERS _sp AS STRING, _pmtos AS STRING, _cursor AS STRING, _tipo
    IF EMPTY(_sp)
       this.mensaje = "SQL VACIO"
       RETURN -1
    ENDIF
    IF EMPTY(_pmtos)
       _pmtos = ''
    ENDIF
    this.conecta()
    IF EMPTY(_cursor)
       _dev = SQLEXEC(this.ncon, "call "+_sp+" ("+_pmtos+")")
       IF _dev<0
          AERROR(_err)
          this.mensaje = _err(2)
       ELSE
          this.mensaje = ""
       ENDIF
       this.desconecta()
       RETURN _dev
    ELSE
       _dev = SQLEXEC(this.ncon, "call "+_sp+" ("+_pmtos+")", _cursor)
       IF _dev<0
          AERROR(_err)
          this.mensaje = _err(2)
       ELSE
          this.mensaje = ""
       ENDIF
       this.desconecta()
       RETURN _dev
    ENDIF
   ENDFUNC
**
   FUNCTION getmensaje
    RETURN this.mensaje
   ENDFUNC
**
   FUNCTION ExecSQL
    LPARAMETERS _sql AS STRING, _cursor AS STRING
    IF EMPTY(_sql)
       this.mensaje = "SQL VACIO"
       RETURN -1
    ENDIF
    this.conecta()
    IF EMPTY(_cursor)
       _dev = SQLEXEC(this.ncon, _sql)
       IF _dev<0
          AERROR(_err)
          this.mensaje = _err(2)
       ELSE
          this.mensaje = ""
       ENDIF
       this.desconecta()
       RETURN _dev
    ELSE
       _dev = SQLEXEC(this.ncon, _sql, _cursor)
       IF _dev<0
          AERROR(_err)
          this.mensaje = _err(2)
       ELSE
          this.mensaje = ""
       ENDIF
       this.desconecta()
       RETURN _dev
    ENDIF
   ENDFUNC
**
   FUNCTION doSpdbc
    LPARAMETERS ccadena AS STRING
    PRIVATE _dev
    this.conecta()
    _dev = this.ncon
    IF _dev>0
       &ccadena
    ENDIF
    this.desconecta()
    RETURN _dev
   ENDFUNC
**
ENDDEFINE
**
*** 
*** ReFox - todo no se pierde 
***
