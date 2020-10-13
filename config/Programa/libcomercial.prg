*** 
*** ReFox XI+  #PT319423  desarrollo.comisesa  EMBRACE [VFP90]
***
**
DEFINE CLASS M_Dist AS cstm OF base.vcx
 ccdgdist = ''
 ccdgprovi = ''
 cdesdist = ''
 tfechora = CTOD('//')
 cipequi = ''
 cnomequi = ''
 cnomuser = ''
 cubgdist = ''
 cnombretabla = 'M_DIST'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_dist = .ccdgdist
       m.cdg_provi = .ccdgprovi
       m.des_dist = .cdesdist
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.ubg_dist = .cubgdist
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_dist = .ccdgdist
       m.cdg_provi = .ccdgprovi
       m.des_dist = .cdesdist
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.ubg_dist = .cubgdist
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_dist = .ccdgdist
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_dist = .ccdgdist
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.cdg_dist = .ccdgdist
       cnocamp = 'CDG_DIST'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 5
          nval = INT(VAL(nvalor))+1
          .ccdgdist = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ccdgdist = &ccurtempo..cdg_dist
       .ccdgprovi = &ccurtempo..cdg_provi
       .cdesdist = &ccurtempo..des_dist
       .tfechora = &ccurtempo..fec_hora
       .cipequi = &ccurtempo..ip_equi
       .cnomequi = &ccurtempo..nom_equi
       .cnomuser = &ccurtempo..nom_user
       .cubgdist = &ccurtempo..ubg_dist
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ccdgdist = ''
       .ccdgprovi = ''
       .cdesdist = ''
       .tfechora = DATETIME()
       .cipequi = ''
       .cnomequi = ''
       .cnomuser = ''
       .cubgdist = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS M_Opciones AS cstm OF base.vcx
 ncdgopc = 0
 cdesopc = ''
 cfrmopc = ''
 npadopc = 0
 lswtsys = .F.
 cnombretabla = 'M_OPCIONES'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_opc = .ncdgopc
       m.des_opc = .cdesopc
       m.frm_opc = .cfrmopc
       m.pad_opc = .npadopc
       m.swt_sys = .lswtsys
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          .ncdgopc = nvalor
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_opc = .ncdgopc
       m.des_opc = .cdesopc
       m.frm_opc = .cfrmopc
       m.pad_opc = .npadopc
       m.swt_sys = .lswtsys
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_opc = .ncdgopc
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_opc = .ncdgopc
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ncdgopc = &ccurtempo..cdg_opc
       .cdesopc = &ccurtempo..des_opc
       .cfrmopc = &ccurtempo..frm_opc
       .npadopc = &ccurtempo..pad_opc
       .lswtsys = &ccurtempo..swt_sys
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ncdgopc = 0
       .cdesopc = ''
       .cfrmopc = ''
       .npadopc = 0
       .lswtsys = .T.
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS M_Tablas AS cstm OF base.vcx
 ccdgmod = ''
 ccdgtab = ''
 cdesfrm = ''
 cdestab = ''
 tfechora = CTOD('//')
 cipequi = ''
 cnomequi = ''
 cnomfrm = ''
 cnomtab = ''
 cnomuser = ''
 cordtab = ''
 cnombretabla = 'M_TABLAS'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_mod = .ccdgmod
       m.cdg_tab = .ccdgtab
       m.des_frm = .cdesfrm
       m.des_tab = .cdestab
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_frm = .cnomfrm
       m.nom_tab = .cnomtab
       m.nom_user = .cnomuser
       m.ord_tab = .cordtab
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_mod = .ccdgmod
       m.cdg_tab = .ccdgtab
       m.des_frm = .cdesfrm
       m.des_tab = .cdestab
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_frm = .cnomfrm
       m.nom_tab = .cnomtab
       m.nom_user = .cnomuser
       m.ord_tab = .cordtab
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_tab = .ccdgtab
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_tab = .ccdgtab
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   PROCEDURE Select_Grid
    PARAMETER cval
    WITH this
       nvalo = ''
       IF cone.dospdbc("nVAlo = M_tablas_sp_Select_Grid("+cval+")")>0
          SELECT &nvalo
          ._alias = nvalo
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.cdg_tab = .ccdgtab
       cnocamp = 'CDG_TAB'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 3
          nval = INT(VAL(nvalor))+1
          .ccdgtab = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ccdgmod = &ccurtempo..cdg_mod
       .ccdgtab = &ccurtempo..cdg_tab
       .cdesfrm = &ccurtempo..des_frm
       .cdestab = &ccurtempo..des_tab
       .tfechora = &ccurtempo..fec_hora
       .cipequi = &ccurtempo..ip_equi
       .cnomequi = &ccurtempo..nom_equi
       .cnomfrm = &ccurtempo..nom_frm
       .cnomtab = &ccurtempo..nom_tab
       .cnomuser = &ccurtempo..nom_user
       .cordtab = &ccurtempo..ord_tab
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ccdgmod = ''
       .ccdgtab = ''
       .cdesfrm = ''
       .cdestab = ''
       .tfechora = DATETIME()
       .cipequi = ''
       .cnomequi = ''
       .cnomfrm = ''
       .cnomtab = ''
       .cnomuser = ''
       .cordtab = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS M_Usuario AS cstm OF base.vcx
 ccdguser = ''
 cclvuser = ''
 cdesuser = ''
 lestuser = .F.
 tfechora = CTOD('//')
 cipequi = ''
 cnomequi = ''
 cnomuser = ''
 cruttmp = ''
 cnombretabla = 'M_USUARIO'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_user = .ccdguser
       m.clv_user = .cclvuser
       m.des_user = .cdesuser
       m.est_user = .lestuser
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.rut_tmp = .cruttmp
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_user = .ccdguser
       m.clv_user = .cclvuser
       m.des_user = .cdesuser
       m.est_user = .lestuser
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.rut_tmp = .cruttmp
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_user = .ccdguser
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_user = .ccdguser
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.cdg_user = .ccdguser
       cnocamp = 'CDG_USER'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 15
          nval = INT(VAL(nvalor))+1
          .ccdguser = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ccdguser = &ccurtempo..cdg_user
       .cclvuser = &ccurtempo..clv_user
       .cdesuser = &ccurtempo..des_user
       .lestuser = &ccurtempo..est_user
       .tfechora = &ccurtempo..fec_hora
       .cipequi = &ccurtempo..ip_equi
       .cnomequi = &ccurtempo..nom_equi
       .cnomuser = &ccurtempo..nom_user
       .cruttmp = &ccurtempo..rut_tmp
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ccdguser = ''
       .cclvuser = ''
       .cdesuser = ''
       .lestuser = .T.
       .tfechora = DATETIME()
       .cipequi = ''
       .cnomequi = ''
       .cnomuser = ''
       .cruttmp = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS M_Depa AS cstm OF base.vcx
 ccdgdepa = ''
 cdesdepa = ''
 tfechora = CTOD('//')
 cipequi = ''
 cnomequi = ''
 cnomuser = ''
 cubgdepa = ''
 cnombretabla = 'M_DEPA'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_depa = .ccdgdepa
       m.des_depa = .cdesdepa
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.ubg_depa = .cubgdepa
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_depa = .ccdgdepa
       m.des_depa = .cdesdepa
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.ubg_depa = .cubgdepa
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_depa = .ccdgdepa
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_depa = .ccdgdepa
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.cdg_depa = .ccdgdepa
       cnocamp = 'CDG_DEPA'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 2
          nval = INT(VAL(nvalor))+1
          .ccdgdepa = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ccdgdepa = &ccurtempo..cdg_depa
       .cdesdepa = &ccurtempo..des_depa
       .tfechora = &ccurtempo..fec_hora
       .cipequi = &ccurtempo..ip_equi
       .cnomequi = &ccurtempo..nom_equi
       .cnomuser = &ccurtempo..nom_user
       .cubgdepa = &ccurtempo..ubg_depa
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ccdgdepa = ''
       .cdesdepa = ''
       .tfechora = DATETIME()
       .cipequi = ''
       .cnomequi = ''
       .cnomuser = ''
       .cubgdepa = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS M_Provi AS cstm OF base.vcx
 ccdgdepa = ''
 ccdgprovi = ''
 cdesprovi = ''
 tfechora = CTOD('//')
 cipequi = ''
 cnomequi = ''
 cnomuser = ''
 cubgprovi = ''
 cnombretabla = 'M_PROVI'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_depa = .ccdgdepa
       m.cdg_provi = .ccdgprovi
       m.des_provi = .cdesprovi
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.ubg_provi = .cubgprovi
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_depa = .ccdgdepa
       m.cdg_provi = .ccdgprovi
       m.des_provi = .cdesprovi
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.ubg_provi = .cubgprovi
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_provi = .ccdgprovi
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_provi = .ccdgprovi
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.cdg_provi = .ccdgprovi
       cnocamp = 'CDG_PROVI'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 4
          nval = INT(VAL(nvalor))+1
          .ccdgprovi = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ccdgdepa = &ccurtempo..cdg_depa
       .ccdgprovi = &ccurtempo..cdg_provi
       .cdesprovi = &ccurtempo..des_provi
       .tfechora = &ccurtempo..fec_hora
       .cipequi = &ccurtempo..ip_equi
       .cnomequi = &ccurtempo..nom_equi
       .cnomuser = &ccurtempo..nom_user
       .cubgprovi = &ccurtempo..ubg_provi
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ccdgdepa = ''
       .ccdgprovi = ''
       .cdesprovi = ''
       .tfechora = DATETIME()
       .cipequi = ''
       .cnomequi = ''
       .cnomuser = ''
       .cubgprovi = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS T_Index AS cstm OF base.vcx
 cnombretabla = 'T_INDEX'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS T_Struct AS cstm OF base.vcx
 cnombretabla = 'T_STRUCT'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS T_Tipmod AS cstm OF base.vcx
 cabrtab = ''
 ccdgtab = ''
 cdestab = ''
 lesttab = .F.
 tfechora = CTOD('//')
 cfrmtab = ''
 cipequi = ''
 cnomequi = ''
 cnomuser = ''
 nnrotab = 0.00 
 nordtab = 0
 ctiptab = ''
 ctittab = ''
 ctxttab = ''
 cnombretabla = 'T_TIPMOD'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.abr_tab = .cabrtab
       m.cdg_tab = .ccdgtab
       m.des_tab = .cdestab
       m.est_tab = .lesttab
       m.fec_hora = .tfechora
       m.frm_tab = .cfrmtab
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.nro_tab = .nnrotab
       m.ord_tab = .nordtab
       m.tip_tab = .ctiptab
       m.tit_tab = .ctittab
       m.txt_tab = .ctxttab
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.abr_tab = .cabrtab
       m.cdg_tab = .ccdgtab
       m.des_tab = .cdestab
       m.est_tab = .lesttab
       m.fec_hora = .tfechora
       m.frm_tab = .cfrmtab
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.nro_tab = .nnrotab
       m.ord_tab = .nordtab
       m.tip_tab = .ctiptab
       m.tit_tab = .ctittab
       m.txt_tab = .ctxttab
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_tab = .ccdgtab
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_tab = .ccdgtab
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.cdg_tab = .ccdgtab
       cnocamp = 'CDG_TAB'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 2
          nval = INT(VAL(nvalor))+1
          .ccdgtab = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .cabrtab = &ccurtempo..abr_tab
       .ccdgtab = &ccurtempo..cdg_tab
       .cdestab = &ccurtempo..des_tab
       .lesttab = &ccurtempo..est_tab
       .tfechora = &ccurtempo..fec_hora
       .cfrmtab = &ccurtempo..frm_tab
       .cipequi = &ccurtempo..ip_equi
       .cnomequi = &ccurtempo..nom_equi
       .cnomuser = &ccurtempo..nom_user
       .nnrotab = &ccurtempo..nro_tab
       .nordtab = &ccurtempo..ord_tab
       .ctiptab = &ccurtempo..tip_tab
       .ctittab = &ccurtempo..tit_tab
       .ctxttab = &ccurtempo..txt_tab
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .cabrtab = ''
       .ccdgtab = ''
       .cdestab = ''
       .lesttab = .T.
       .tfechora = DATETIME()
       .cfrmtab = ''
       .cipequi = ''
       .cnomequi = ''
       .cnomuser = ''
       .nnrotab = 0.00 
       .nordtab = 0
       .ctiptab = ''
       .ctittab = ''
       .ctxttab = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS M_Cmptab AS cstm OF base.vcx
 cconditab = ''
 clistcmpmod = ''
 clistvar = ''
 cnomtab = ''
 cnombretabla = 'M_CMPTAB'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.condi_tab = .cconditab
       m.list_cmpmod = .clistcmpmod
       m.list_var = .clistvar
       m.nom_tab = .cnomtab
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.condi_tab = .cconditab
       m.list_cmpmod = .clistcmpmod
       m.list_var = .clistvar
       m.nom_tab = .cnomtab
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.nom_tab = .cnomtab
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.nom_tab = .cnomtab
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.nom_tab = .cnomtab
       cnocamp = 'NOM_TAB'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 15
          nval = INT(VAL(nvalor))+1
          .cnomtab = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .cconditab = &ccurtempo..condi_tab
       .clistcmpmod = &ccurtempo..list_cmpmod
       .clistvar = &ccurtempo..list_var
       .cnomtab = &ccurtempo..nom_tab
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .cconditab = ''
       .clistcmpmod = ''
       .clistvar = ''
       .cnomtab = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS D_Perfil AS cstm OF base.vcx
 ncdgopc = 0
 ccdgperf = ''
 lswtperf = .F.
 cnombretabla = 'D_PERFIL'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_opc = .ncdgopc
       m.cdg_perf = .ccdgperf
       m.swt_perf = .lswtperf
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_opc = .ncdgopc
       m.cdg_perf = .ccdgperf
       m.swt_perf = .lswtperf
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_opc = .ncdgopc
       m.cdg_perf = .ccdgperf
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_opc = .ncdgopc
       m.cdg_perf = .ccdgperf
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ncdgopc = &ccurtempo..cdg_opc
       .ccdgperf = &ccurtempo..cdg_perf
       .lswtperf = &ccurtempo..swt_perf
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ncdgopc = 0
       .ccdgperf = ''
       .lswtperf = .T.
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS M_Perfil AS cstm OF base.vcx
 ccdgperf = ''
 cdesperf = ''
 lestperf = .F.
 tfechora = CTOD('//')
 cipequi = ''
 cnomequi = ''
 cnomuser = ''
 cnombretabla = 'M_PERFIL'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_perf = .ccdgperf
       m.des_perf = .cdesperf
       m.est_perf = .lestperf
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_perf = .ccdgperf
       m.des_perf = .cdesperf
       m.est_perf = .lestperf
       m.fec_hora = .tfechora
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_perf = .ccdgperf
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_perf = .ccdgperf
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.cdg_perf = .ccdgperf
       cnocamp = 'CDG_PERF'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 3
          nval = INT(VAL(nvalor))+1
          .ccdgperf = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ccdgperf = &ccurtempo..cdg_perf
       .cdesperf = &ccurtempo..des_perf
       .lestperf = &ccurtempo..est_perf
       .tfechora = &ccurtempo..fec_hora
       .cipequi = &ccurtempo..ip_equi
       .cnomequi = &ccurtempo..nom_equi
       .cnomuser = &ccurtempo..nom_user
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ccdgperf = ''
       .cdesperf = ''
       .lestperf = .T.
       .tfechora = DATETIME()
       .cipequi = ''
       .cnomequi = ''
       .cnomuser = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS D_Usuario AS cstm OF base.vcx
 ccdgperf = ''
 ccdguser = ''
 lperfprinc = .F.
 cnombretabla = 'D_USUARIO'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_perf = .ccdgperf
       m.cdg_user = .ccdguser
       m.perf_princ = .lperfprinc
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_perf = .ccdgperf
       m.cdg_user = .ccdguser
       m.perf_princ = .lperfprinc
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_perf = .ccdgperf
       m.cdg_user = .ccdguser
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_perf = .ccdgperf
       m.cdg_user = .ccdguser
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   PROCEDURE Select_Grid
    PARAMETER cfiltro, climite, corder
    WITH this
       m.pa1 = cfiltro
       m.pa2 = climite
       m.pa3 = corder
       TEXT TO _sql NOSHOW
m.pa1,m.pa2,m.pa3
       ENDTEXT
       nvalo = ''
       IF cone.dospdbc('nVAlo = d_usuario_sp_Select_Grid('+_sql+')')>0
          SELECT &nvalo
          ._alias = nvalo
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ccdgperf = &ccurtempo..cdg_perf
       .ccdguser = &ccurtempo..cdg_user
       .lperfprinc = &ccurtempo..perf_princ
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ccdgperf = ''
       .ccdguser = ''
       .lperfprinc = .T.
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS M_Reqtras AS cstm OF base.vcx
 cadj001 = ''
 cadj002 = ''
 cadj003 = ''
 casuntenv = ''
 cconteenv = ''
 tfecenv = CTOD('//')
 tfechora = CTOD('//')
 dfecreq = CTOD('//')
 cipequi = ''
 cnomequi = ''
 cnomuser = ''
 cnuemreg = ''
 cparaenv = ''
 cnombretabla = 'M_REQTRAS'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.adj_001 = .cadj001
       m.adj_002 = .cadj002
       m.adj_003 = .cadj003
       m.asunt_env = .casuntenv
       m.conte_env = .cconteenv
       m.fec_env = .tfecenv
       m.fec_hora = .tfechora
       m.fec_req = .dfecreq
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.nuem_reg = .cnuemreg
       m.para_env = .cparaenv
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.adj_001 = .cadj001
       m.adj_002 = .cadj002
       m.adj_003 = .cadj003
       m.asunt_env = .casuntenv
       m.conte_env = .cconteenv
       m.fec_env = .tfecenv
       m.fec_hora = .tfechora
       m.fec_req = .dfecreq
       m.ip_equi = .cipequi
       m.nom_equi = .cnomequi
       m.nom_user = .cnomuser
       m.nuem_reg = .cnuemreg
       m.para_env = .cparaenv
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.nuem_reg = .cnuemreg
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.nuem_reg = .cnuemreg
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.nuem_reg = .cnuemreg
       cnocamp = 'NUEM_REG'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 10
          nval = INT(VAL(nvalor))+1
          .cnuemreg = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .cadj001 = &ccurtempo..adj_001
       .cadj002 = &ccurtempo..adj_002
       .cadj003 = &ccurtempo..adj_003
       .casuntenv = &ccurtempo..asunt_env
       .cconteenv = &ccurtempo..conte_env
       .tfecenv = &ccurtempo..fec_env
       .tfechora = &ccurtempo..fec_hora
       .dfecreq = &ccurtempo..fec_req
       .cipequi = &ccurtempo..ip_equi
       .cnomequi = &ccurtempo..nom_equi
       .cnomuser = &ccurtempo..nom_user
       .cnuemreg = &ccurtempo..nuem_reg
       .cparaenv = &ccurtempo..para_env
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .cadj001 = ''
       .cadj002 = ''
       .cadj003 = ''
       .casuntenv = ''
       .cconteenv = ''
       .tfecenv = DATETIME()
       .tfechora = DATETIME()
       .dfecreq = DATE()
       .cipequi = ''
       .cnomequi = ''
       .cnomuser = ''
       .cnuemreg = ''
       .cparaenv = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
DEFINE CLASS C_Emailusu AS cstm OF base.vcx
 ccdguser = ''
 cctaemail = ''
 cobsfrim = ''
 cpswdemail = ''
 csrvpuert = ''
 csvrsmtp = ''
 cnombretabla = 'C_EMAILUSU'
**
   FUNCTION Insertar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_user = .ccdguser
       m.cta_email = .cctaemail
       m.obs_frim = .cobsfrim
       m.pswd_email = .cpswdemail
       m.srv_puert = .csrvpuert
       m.svr_smtp = .csvrsmtp
       m.nvalor = ""
       x = CHRTRAN(ALLTRIM(&ctempo..list_var),"m.","")
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..list_var)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Ins(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Editar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_user = .ccdguser
       m.cta_email = .cctaemail
       m.obs_frim = .cobsfrim
       m.pswd_email = .cpswdemail
       m.srv_puert = .csrvpuert
       m.svr_smtp = .csvrsmtp
       x = ALLTRIM(&ctempo..list_cmpmod)
       TEXT TO cvar1 NOSHOW
	<<x>>
       ENDTEXT
       x=  ALLTRIM(&ctempo..condi_tab)
       TEXT TO cvar2 NOSHOW
	<<x>>
       ENDTEXT
       m.nvalor = ""
       IF cone.dospdbc("nValor = _Upd(cNoBD, cNomTab , Cvar1,Cvar2)")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   FUNCTION Eliminar
    _hecho = .F.
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_user = .ccdguser
       IF cone.dospdbc("nValor = _Dele(cNoBD, cNomTab ,  ALLTRIM(&Ctempo..condi_tab))")>0
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Select
    PARAMETER _cursor
    WITH this
       IF EMPTY(_cursor)
          _cursor = cone.tmpname()
       ENDIF
       IF USED(_cursor)
          USE IN &_cursor
       ENDIF
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       ctempo = SYS(2015)
       .select_sql("M_CMPTAB", "*", "nom_tab='"+cnomtab+"'", "1", ctempo)
       m.cdg_user = .ccdguser
       IF cone.dospdbc("nVAlor = _Select(cNoBD, cNomTab , ALLTRIM(&Ctempo..condi_tab))")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    IF USED(ctempo)
       USE IN &ctempo
    ENDIF
   ENDPROC
**
   PROCEDURE Select_All
    WITH this
       m.nvalor = ""
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       IF cone.dospdbc("nVAlor = _Select_All(cNoBD, cNomTab )")>0
          SELECT &nvalor
          ._alias = nvalor
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
   ENDPROC
**
   FUNCTION Correlativo
    _hecho = .F.
    WITH this
       cnomtab = .cnombretabla
       cnobd = cone.basedatos()
       m.cdg_user = .ccdguser
       cnocamp = 'CDG_USER'
       m.nvalor = ''
       IF cone.dospdbc("nVAlor = _corre(cNoBD, cNomTab , cNoCamp)")>0
          nvalor = IIF(ISNULL(nvalor), "", nvalor)
          nlen = 10
          nval = INT(VAL(nvalor))+1
          .ccdguser = PADL(nval, nlen, '0')
          _hecho = .T.
       ELSE
          .mensaje = cone.getmensaje()
       ENDIF
    ENDWITH
    RETURN _hecho
   ENDFUNC
**
   PROCEDURE Llenar
    LPARAMETERS ccurtempo
    SELECT (ccurtempo)
    WITH this
       .ccdguser = &ccurtempo..cdg_user
       .cctaemail = &ccurtempo..cta_email
       .cobsfrim = &ccurtempo..obs_frim
       .cpswdemail = &ccurtempo..pswd_email
       .csrvpuert = &ccurtempo..srv_puert
       .csvrsmtp = &ccurtempo..svr_smtp
    ENDWITH
   ENDPROC
**
   PROCEDURE Limpiar
    WITH this
       .ccdguser = ''
       .cctaemail = ''
       .cobsfrim = ''
       .cpswdemail = ''
       .csrvpuert = ''
       .csvrsmtp = ''
    ENDWITH
   ENDPROC
**
ENDDEFINE
**
*** 
*** ReFox - todo no se pierde 
***
