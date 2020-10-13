*** 
*** ReFox XI+  #PT319423  desarrollo.comisesa  EMBRACE [VFP90]
***
**
DEFINE CLASS Base AS Session
 PROTECTED sourcetype, driver, db, ruta, adi, ncon
**
   PROCEDURE Init
    LPARAMETERS _sourcetype AS STRING, _drv AS STRING, _db AS STRING, _ruta AS STRING, _add AS STRING
    this.sourcetype = ALLTRIM(_sourcetype)
    this.driver = ALLTRIM(_drv)
    this.db = ALLTRIM(_db)
    this.ruta = ALLTRIM(_ruta)
    this.adi = ALLTRIM(_add)
    this.ncon = 0
    SET TALK OFF
    SET SAFETY OFF
    SET ECHO OFF
    SET NOTIFY OFF
    SET STATUS OFF
    SET BRSTATUS OFF
    SET BELL OFF
    SET CARRY OFF
    SET DELETED ON
    SET EXACT ON
    SET NEAR OFF
    SET EXCLUSIVE OFF
    SET MULTILOCKS ON
    SET CURSOR ON
    SET CENTURY ON
    SET CENTURY TO 19 ROLLOVER 50
    SET DATE TO Dmy
    SET HOURS TO 24
    SET POINT TO "."
    SET SEPARATOR TO ","
    SET TEXTMERGE ON
   ENDPROC
**
   FUNCTION TmpName
    LOCAL _temp AS STRING
    _temp = "c_"+SYS(2015)
    DO WHILE USED(_temp)
       _temp = "c_"+SYS(2015)
    ENDDO
    RETURN (_temp)
   ENDFUNC
**
   FUNCTION fileTemp
    RETURN SYS(2023)+"\"+cone.tmpname()
   ENDFUNC
**
   FUNCTION BaseDatos
    RETURN this.db
   ENDFUNC
**
   FUNCTION IPAddress
    LOCAL lowsock, lcip
    lowsock = CREATEOBJECTEX("{248DD896-BB45-11CF-9ABC-0080C7E7B78D}", "", "")
    lcip = lowsock.localip
    RETURN (lcip)
   ENDFUNC
**
ENDDEFINE
**
*** 
*** ReFox - todo no se pierde 
***
