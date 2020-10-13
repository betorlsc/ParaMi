*** 
*** ReFox XI+  #PT319423  desarrollo.comisesa  EMBRACE [VFP90]
***
**
PROCEDURE _Ins
 PARAMETER cnotab, clistcamp, cvalorcampo
 TEXT TO cej NOSHOW
	Insert Into  <cNoTab>>( <<cListCAmp>>) Values ( <<cValorCAmpo>> )
 ENDTEXT
 &cej
 IF USED(cnotab)
    USE IN (cnotab)
 ENDIF
ENDPROC
**
PROCEDURE _Upd
 PARAMETER cnotab, cvalorcampo, ccondi
 TEXT TO cej NOSHOW
	Update  <<cNoTab>> Set <<cValorCAmpo>>	where <<cCondi >>
 ENDTEXT
 &cej
 SELECT (cnotab)
 IF DELETED()
    RECALL
 ENDIF
 IF USED(cnotab)
    USE IN (cnotab)
 ENDIF
ENDPROC
**
PROCEDURE _Dele
 PARAMETER cnotab, ccondi
 TEXT TO cej NOSHOW
	Delete  From <<cNoTab>>  WHERE <<cCondi>>
 ENDTEXT
 &cej
 IF USED(cnotab)
    USE IN (cnotab)
 ENDIF
ENDPROC
**
FUNCTION _Select
 PARAMETER cnotab, ccondi
 LOCAL _alias
 _alias = SYS(2015)
 TEXT TO cej NOSHOW
	Select *  FROM <<cNoTab>>  WHERE <<cCondi>> Into Cursor (_alias) Readwrite
 ENDTEXT
 &cej
 IF USED(cnotab)
    USE IN (cnotab)
 ENDIF
 RETURN _alias
ENDFUNC
**
FUNCTION _Select_All
 PARAMETER cnobd, cnotab
 LOCAL _alias
 _alias = SYS(2015)
 TEXT TO cej NOSHOW
	Select *  FROM <<cNoBD>>!<<cNoTab>> Into Cursor (_alias) Readwrite
 ENDTEXT
 &cej
 IF USED(cnotab)
    USE IN (cnotab)
 ENDIF
 RETURN _alias
ENDFUNC
**
FUNCTION _Corre
 PARAMETER cnotab, cnocamp
 LOCAL _alias
 _alias = SYS(2015)
 TEXT TO cej NOSHOW
	Select Max(<<cNoCamp>>) As ulti From <<cNoTab>>  Into Cursor (_alias)
 ENDTEXT
 &cej
 nid = &_alias..ulti
 IF USED(cnotab)
    USE IN (cnotab)
 ENDIF
 IF USED(_alias)
    USE IN (_alias)
 ENDIF
 RETURN nid
ENDFUNC
**
*** 
*** ReFox - todo no se pierde 
***
