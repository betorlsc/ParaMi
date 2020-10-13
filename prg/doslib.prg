***
*** ReFox XI+  #TB431873  desarrollo.comisesa  EMBRACE [VFP90]
***
**
Function Dia
	Parameter dfecha
	Private adias
	Dimension adias(7)
	adias(1) = "Domingo"
	adias(2) = "Lunes"
	adias(3) = "Martes"
	adias(4) = "Miércoles"
	adias(5) = "Jueves"
	adias(6) = "Viernes"
	adias(7) = "Sábado"
	Return (Iif( .Not. Empty(dfecha), adias(Dow(dfecha)), ""))
Endfunc
**
Function Mes
	Parameter dfecha
	Private ameses
	Dimension ameses(12)
	ameses(1) = "Enero"
	ameses(2) = "Febrero"
	ameses(3) = "Marzo"
	ameses(4) = "Abril"
	ameses(5) = "Mayo"
	ameses(6) = "Junio"
	ameses(7) = "Julio"
	ameses(8) = "Agosto"
	ameses(9) = "Setiembre"
	ameses(10) = "Octubre"
	ameses(11) = "Noviembre"
	ameses(12) = "Diciembre"
	Return (Iif( .Not. Empty(dfecha), ameses(Month(dfecha)), ""))
Endfunc
**
Function Mes2
	Parameter cmes
	Private ameses
	Dimension ameses(12)
	ameses(1) = "Enero"
	ameses(2) = "Febrero"
	ameses(3) = "Marzo"
	ameses(4) = "Abril"
	ameses(5) = "Mayo"
	ameses(6) = "Junio"
	ameses(7) = "Julio"
	ameses(8) = "Agosto"
	ameses(9) = "Setiembre"
	ameses(10) = "Octubre"
	ameses(11) = "Noviembre"
	ameses(12) = "Diciembre"
	Return (Iif(cmes>="01" .And. cmes<="12", ameses(Val(cmes)), "Apertura"))
Endfunc
**
Function FecText
	Parameter dfecha
	Return (Iif( .Not. Empty(dfecha), Str(Day(dfecha), 2)+" de "+Mes(dfecha)+" de "+Str(Year(dfecha), 4), ""))
Endfunc
**
Function FecTxt
	Parameter dfecha
	Return (Str(Day(dfecha), 2)+" "+Lower(Mes(dfecha))+" "+Right(Str(Year(dfecha), 4), 2))
Endfunc
**
Function FecTextDia
	Parameter dfecha
	Return (Lower(Dia(dfecha))+" "+Str(Day(dfecha), 2)+" de "+Mes(dfecha))
Endfunc
**
Function CerosIzq
	Parameter ccadnum
	Return (Padl(Alltrim(ccadnum), Len(ccadnum), '0'))
Endfunc
**
Function Encripta
	Parameter cadena
	Private w_result
	w_result = Space(1)
	w_result = Chr(Asc(Substr(cadena, 1, 1))*2-2)
	For i = 2 To Len(cadena)
		w_result = w_result+Chr(Asc(Substr(cadena, i, 1))*2-2)
	Endfor
	Return (w_result)
Endfunc
**
Function Des_Encripta
	Parameter cadena
	Private w_result
	w_result = Space(1)
	w_result = Chr((Asc(Substr(cadena, 1, 1))+2)/2)
	For i = 2 To Len(cadena)
		w_result = w_result+Chr((Asc(Substr(cadena, i, 1))+2)/2)
	Endfor
	Return (w_result)
Endfunc
**
Function REDONDEA
	Lparameters p1
	p1 = p1+0.0025
	p1 = Int(p1*20+0.5 )/20
	Return p1
Endfunc
**
Function opendbf
	Parameter dbname, fname, fldname
	Private stem, tagnum, tabla
	If  .Not. Empty(dbname)
		dbname = dbname+".dbc"
		If  .Not. Dbused(dbname)
			Open Database (dbname)
		Endif
		Set Database To (dbname)
		stem = Juststem(fname)
		If Indbc(stem, 'TABLE')
			tabla = Juststem(dbname)+"!"+stem
			If Used(stem)
				Select (stem)
			Else
				Select 0
				If Empty(fname)
					Return ''
				Else
					Use (tabla) Again Alias (stem)
				Endif
			Endif
			tagnum = gettag(fldname)
			If m.tagnum>0
				Set Order To (tagnum)
			Else
				Set Order To 0
			Endif
		Endif
	Else
		If File(fname)
			stem = Juststem(fname)
			If Used(stem)
				Select (stem)
			Else
				Select 0
				fname = Locfile(fname, 'DBF', '"Donde esta '+Juststem(fname)+' database')
				If Empty(fname)
					Return ''
				Else
					Use (fname) Again Alias (stem)
				Endif
			Endif
			tagnum = gettag(fldname)
			If m.tagnum>0
				Set Order To (tagnum)
			Else
				Set Order To 0
			Endif
			Return Alias()
		Else
			Return ''
		Endif
	Endif
Endfunc
**
Procedure CloseTab
	Lparameters cNombreTabla
	If Used(cNombreTabla)
		Use In (cNombreTabla)
	Endif
Endproc
***
Procedure CloseDbf
	Parameter flname
	If File(flname)
		stem = Juststem(flname)
		If Used(stem)
			Select (stem)
			Use
		Endif
	Else
		Wait Window "Base no encontrada"
	Endif
	Return
Endproc
**
Function Juststem
	Parameter filname
	Private All
	If Rat('\', m.filname)>0
		m.filname = Substr(m.filname, Rat('\', m.filname)+1, 255)
	Endif
	If At(':', m.filname)>0
		m.filname = Substr(m.filname, At(':', m.filname)+1, 255)
	Endif
	If At('.', m.filname)>0
		m.filname = Substr(m.filname, 1, At('.', m.filname)-1)
	Endif
	Return Alltrim(Upper(m.filname))
Endfunc
**
Function gettag
	Parameter fldname
	Private All
	m.fldname = Upper(Alltrim(m.fldname))
	i = 1
	Do While  .Not. Empty(Tag(i)) .And. i<1000
		If Upper(Tag(i))==m.fldname
			Return i
		Endif
		i = i+1
	Enddo
	Return 0
Endfunc
**
Function RBloquea
	Parameter nsegundos
	If Rlock()
		Return (.T.)
	Endif
	Wait Window Nowait "Registro en Uso. Intentando Bloquearlo"
	Do While (nsegundos>0)
		= Inkey(0.1 )
		nsegundos = nsegundos-1
		If Rlock()
			Return (.T.)
		Endif
	Enddo
	Return (.F.)
Endfunc
**
Function ActReg
	Parameter cswt
	Private lswt
	lswt = .F.
	Do Case
		Case Upper(cswt)="C" .Or. Upper(cswt)="M"
			If RBloquea(5)=.T.
				Gather Memo Memvar
				Unlock
				Wait Window Nowait " Registro Actualizado "
			Endif
			lswt = .T.
		Case Upper(cswt)="E"
			If RBloquea(5)=.T.
				Delete
				Unlock
				Wait Window Nowait " Registro Eliminado "
			Endif
			lswt = .T.
	Endcase
	Return (lswt)
Endfunc
**
Function ActRec
	Parameter cswt
	Private lswt
	lswt = .F.
	Do Case
		Case Upper(cswt)="C" .Or. Upper(cswt)="M"
			If RBloquea(5)=.T.
				Gather Memo Memvar
				Unlock
			Endif
			lswt = .T.
		Case Upper(cswt)="E"
			If RBloquea(5)=.T.
				Delete
				Unlock
			Endif
			lswt = .T.
	Endcase
	Return (lswt)
Endfunc
**
Function Salir
	Close All
	Set Sysmenu To Default
	Set Exclusive On
	With _Screen
		.Closable = .T.
		.MaxButton = .T.
		.Picture = ""
	Endwith
	Clear All
	Clear
	Clear Events
	Return 0
Endfunc
**
Function VarMemB
	Parameter cdbname, cnametable
	= opendbf(cdbname, cnametable, "")
	Scatter Blank Memo Memvar
	= CloseDbf(cnametable)
	Return 0
Endfunc
**
Function WaitWind
	Parameter ctexto
	Wait Window Nowait (ctexto)
	Return 0
Endfunc
**
Function NumCor
	Parameter cdbname, ctabla, ctag
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	= opendbf(cdbname, ctabla, ctag)
	nlen = Fsize(ctag)
	Goto Bottom
	ccod = CerosIzq( Str( Val(&ctag) + 1, nlen) )
	= CloseDbf(ctabla)
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function NumGuia
	Parameter ctipo
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select num_guia From sico!m_guia Where  Not Deleted() And m_guia.cdg_tpg=ctipo Order By num_guia Into Cursor curG
	nlen = Iif(Eof("curG"), 10, Len(curG.num_guia))
	Goto Bottom
	ccod = CerosIzq(Str(Val(curG.num_guia)+1, nlen))
	Use In curG
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function Estado_Orden
	Parameter corden
	Private cdel, cest
	cdel = Set("DELETED")
	Set Deleted On
	Select Sum(d_ordcom.can_oins) As pedido, Sum(d_ordcom.can_ialm) As ingresado, d_ordcom.num_ocom, m_ordcom.swt_est, m_ordcom.swt_term From sico!d_ordcom, sico!m_ordcom Where m_ordcom.num_ocom=corden And d_ordcom.num_ocom=m_ordcom.num_ocom Group By d_ordcom.num_ocom Into Cursor curEstado
	If  .Not. Eof()
		If curEstado.swt_est="A"
			cest = "A"
		Else
			If  .Not. Empty(curEstado.swt_term)
				cest = "T"
			Else
				cest = Iif(ingresado>=pedido, "T", "")
			Endif
		Endif
	Else
		cest = ""
	Endif
	Use In curEstado
	Set Deleted &cdel
	Return (cest)
Endfunc
**
Function Estado_Pedido
	Parameter cpedido
	Private cdel, cest
	cdel = Set("DELETED")
	Set Deleted On
	Select Sum(d_pedido.can_pprd) As pedido, Sum(d_pedido.can_dprd) As despachado, Sum(d_pedido.can_fprd) As facturado, d_pedido.num_ped, m_pedido.swt_ped From sico!d_pedido, sico!m_pedido Where m_pedido.num_ped=cpedido And d_pedido.num_ped=m_pedido.num_ped And  Not Deleted() Group By d_pedido.num_ped Into Cursor curEstado
	If  .Not. Eof()
		If curEstado.swt_ped="A"
			cest = "A"
		Else
			If curEstado.swt_ped="C"
				cest = "C"
			Else
				cest = Iif(curEstado.facturado>=curEstado.pedido .And. curEstado.despachado>=curEstado.pedido, "T", " ")
			Endif
		Endif
	Else
		cest = " "
	Endif
	Use In curEstado
	Set Deleted &cdel
	Return (cest)
Endfunc
**
Function Productos_Ingresados
	Parameter corden, cprod
	Private nins, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select m_guia.num_guia, d_guia.cdg_tpg, d_guia.cdg_prod, d_guia.can_dgui From sico!m_guia, sico!d_guia Where  Not Deleted() And m_guia.num_ocom=corden And d_guia.cdg_prod=cprod And m_guia.num_guia=d_guia.num_guia And m_guia.cdg_tpg="001" And d_guia.cdg_tpg="001" And m_guia.swt_est<>"A" Into Cursor curG
	Goto Top
	nins = 0
	Do While  .Not. Eof("curG")
		nins = nins+curG.can_dgui
		Skip In curG
	Enddo
	Use In curG
	Set Deleted &cdel
	Return (nins)
Endfunc
**
Function producto_Guia
	Parameter cguia, cprod
	Private nins, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select d_guia.num_guia, d_guia.cdg_tpg, d_guia.cdg_prod, d_guia.can_dgui, m_guia.swt_est From sico!d_guia, sico!m_guia Where  Not Deleted() And d_guia.cdg_prod=cprod And d_guia.num_guia=cguia And d_guia.cdg_tpg="001" And m_guia.num_guia=cguia And m_guia.swt_est<>"A" And m_guia.cdg_tpg="001" Into Cursor curG
	Goto Top
	nins = 0
	Do While  .Not. Eof("curG")
		nins = nins+curG.can_dgui
		Skip In curG
	Enddo
	Use In curG
	Set Deleted &cdel
	Return (nins)
Endfunc
**
Function Stock_Ingresado
	Parameter carea, cprod
	Private nins, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select m_guia.num_guia, d_guia.cdg_tpg, d_guia.cdg_prod, d_guia.can_dgui From sico!m_guia, sico!d_guia Where  Not Deleted() And m_guia.num_guia=d_guia.num_guia And m_guia.cdg_tpg="001" And d_guia.cdg_tpg="001" And d_guia.cdg_prod=cprod And m_guia.cdg_area=carea And m_guia.swt_est<>"A" Into Cursor curG
	Goto Top
	nins = 0
	Do While  .Not. Eof("curG")
		nins = nins+curG.can_dgui
		Skip In curG
	Enddo
	Set Deleted &cdel
	Return (nins)
Endfunc
**
Function Traslados
	Parameter cdestino, cprod
	Private nins, cdel
	cdel = Set("DELETED")
	nins = 0
	Set Deleted &cdel
	Return (nins)
Endfunc
**
Function Stock_Salido_Origen
	Parameter carea, cprod
	Private nins, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select m_guia.num_guia, d_guia.cdg_tpg, d_guia.cdg_prod, d_guia.can_dgui From sico!m_guia, sico!d_guia Where  Not Deleted() And m_guia.num_guia=d_guia.num_guia And m_guia.cdg_tpg<>"001" And d_guia.cdg_tpg<>"001" And m_guia.cdg_tpg=d_guia.cdg_tpg And d_guia.cdg_prod=cprod And m_guia.ori_area=carea And m_guia.swt_est<>"A" Into Cursor curG
	Goto Top
	nins = 0
	Do While  .Not. Eof("curG")
		nins = nins+curG.can_dgui
		Skip In curG
	Enddo
	Set Deleted &cdel
	Return (nins)
Endfunc
**
Function Stock_Salido_Destino
	Parameter carea, cprod
	Private nins, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select m_guia.num_guia, d_guia.cdg_tpg, d_guia.cdg_prod, d_guia.can_dgui From sico!m_guia, sico!d_guia Where  Not Deleted() And m_guia.num_guia=d_guia.num_guia And m_guia.cdg_tpg<>"001" And d_guia.cdg_tpg<>"001" And m_guia.cdg_tpg=d_guia.cdg_tpg And d_guia.cdg_prod=cprod And m_guia.cdg_area=carea And m_guia.swt_est<>"A" Into Cursor curG
	Goto Top
	nins = 0
	Do While  .Not. Eof("curG")
		nins = nins+curG.can_dgui
		Skip In curG
	Enddo
	Set Deleted &cdel
	Return (nins)
Endfunc
**
Function Precio_Promedio
	Parameter cprod, corigen, cmoneda
	Private nins, cdel
	cdel = Set("DELETED")
	nins = 0
	Set Deleted On
	Select d_guia.pre_guia, d_guia.cdg_prod, m_guia.num_guia, m_guia.cdg_mon, m_guia.fec_guia, t_cambio.tip_cmb From sico!d_guia, sico!m_guia, sico!t_cambio Where  Not Deleted() And d_guia.cdg_tpg="001" And m_guia.cdg_tpg="001" And d_guia.cdg_prod=cprod And d_guia.num_guia=m_guia.num_guia And m_guia.swt_est<>"A" And m_guia.cdg_area=corigen And Dtos(m_guia.fec_guia)=Dtos(t_cambio.fec_cmb) Into Cursor curPP
	Goto Top
	Do While  .Not. Eof("curPP")
		If cmoneda="001"
			If curPP.cdg_mon="001"
				nins = nins+curPP.pre_guia
			Else
				nins = nins+Round(curPP.pre_guia*curPP.tip_cmb, 2)
			Endif
		Else
			If curPP.cdg_mon="002"
				nins = nins+curPP.pre_guia
			Else
				nins = nins+Round(curPP.pre_guia/curPP.tip_cmb, 2)
			Endif
		Endif
		Skip In curPP
	Enddo
	Set Deleted &cdel
	Return (nins)
Endfunc
**
Function NumLetNew
	Parameter nimporte
	Private cifra
	Store Space(1) To c1, c2, c3, c4, cifra
	z1 = Trim(Str(Int(nimporte), 12))
	k = 1
	Y = 10
	N = 3
	Do While k<=4
		z = Substr(z1, Y, 3)
		s = Substr(z, N, 1)
		If s='1'
			u = Iif(k=2 .Or. k=3, 'UN', 'UNO')
		Else
			u = Iif(s='2', 'DOS', Iif(s='3', 'TRES', Iif(s='4', 'CUATRO', Iif(s='5', 'CINCO', Iif(s='6', 'SEIS', Iif(s='7', 'SIETE', Iif(s='8', 'OCHO', Iif(s='9', 'NUEVE', ''))))))))
		Endif
		Do Case
			Case Substr(z, N-1, 1)='1'
				If Substr(z, N, 1)='0'
					d = 'DIEZ'
				Else
					Do Case
						Case Substr(z, N, 1)='1'
							d = 'ONCE'
							u = ''
						Case Substr(z, N, 1)='2'
							d = 'DOCE'
							u = ''
						Case Substr(z, N, 1)='3'
							d = 'TRECE'
							u = ''
						Case Substr(z, N, 1)='4'
							d = 'CATORCE'
							u = ''
						Case Substr(z, N, 1)='5'
							d = 'QUINCE'
							u = ''
						Otherwise
							d = 'DIECI'
					Endcase
				Endif
			Case Substr(z, N-1, 1)='2'
				d = Iif(Substr(z, N, 1)='0', 'VEINTE', 'VEINTI')
			Case Substr(z, N-1, 1)='3'
				d = Iif(Substr(z, N, 1)='0', 'TREINTA', 'TRENTI')
			Case Substr(z, N-1, 1)='4'
				d = Iif(Substr(z, N, 1)='0', 'CUARENTA', 'CUARENTI')
			Case Substr(z, N-1, 1)='5'
				d = Iif(Substr(z, N, 1)='0', 'CINCUENTA', 'CINCUENTI')
			Case Substr(z, N-1, 1)='6'
				d = Iif(Substr(z, N, 1)='0', 'SESENTA', 'SESENTI')
			Case Substr(z, N-1, 1)='7'
				d = Iif(Substr(z, N, 1)='0', 'SETENTA', 'SETENTI')
			Case Substr(z, N-1, 1)='8'
				d = Iif(Substr(z, N, 1)='0', 'OCHENTA', 'OCHENTI')
			Case Substr(z, N-1, 1)='9'
				d = Iif(Substr(z, N, 1)='0', 'NOVENTA', 'NOVENTI')
			Otherwise
				d = ''
		Endcase
		Do Case
			Case Substr(z, N-2, 1)='1'
				c = Iif(Substr(z, N-1, 1)='0' .And. Substr(z, N, 1)='0', 'CIEN', 'CIENTO')
			Case Substr(z, N-2, 1)='2'
				c = 'DOSCIENTOS'
			Case Substr(z, N-2, 1)='3'
				c = 'TRESCIENTOS'
			Case Substr(z, N-2, 1)='4'
				c = 'CUATROCIENTOS'
			Case Substr(z, N-2, 1)='5'
				c = 'QUINIENTOS'
			Case Substr(z, N-2, 1)='6'
				c = 'SEISCIENTOS'
			Case Substr(z, N-2, 1)='7'
				c = 'SETECIENTOS'
			Case Substr(z, N-2, 1)='8'
				c = 'OCHOCIENTOS'
			Case Substr(z, N-2, 1)='9'
				c = 'NOVECIENTOS'
			Otherwise
				c = ''
		Endcase
		Do Case
			Case k=1
				c1 = c+" "+d+u
			Case k=2
				c2 = c+" "+d+u
			Case k=3
				c3 = c+" "+d+u
			Case k=4
				c4 = c+" "+d+u
		Endcase
		k = k+1
		Y = Y-3
	Enddo
	Do Case
		Case (Len(c4)-1)<>0
			Sys = Iif(c4='UN', ' MILLON ', ' MILLONES ')
			cifra = c4+c3+Sys+c2+c1
		Case (Len(c3)-1)<>0
			Sys = Iif(Alltrim(c3)='UN', ' MILLON ', ' MILLONES ')
			cifra = c3+Sys+c2+Iif(Empty(c2), '', ' MIL ')+c1
		Case (Len(c2)-1)<>0
			cifra = c2+' MIL '+c1
		Case Len(c1)<>0
			cifra = c1
	Endcase
	Return (Alltrim(cifra))
Endfunc
**
Function UbiApp
	Parameter cnomapp
	Private cdirapp
	cdirapp = Locfile(cnomapp, "¿Dónde se encuentra "+Upper(cnomapp)+"?")
	cdirapp = Iif(Empty(cdirapp), Sys(5)+Curdir(), Left(cdirapp, Rat("\", cdirapp)-1))
	Return cdirapp
Endfunc
**
Function UbiDbf
	Parameter cdirapp, cfileini
	If !File("&cDirApp\&cFileIni..ini")
		Create Table &cdirapp\&cfileini..ini (dir_dbf c(60), nom_dbf c(10))
		m.dir_dbf = Iif(gdemo=1, cdirapp+"\dataSico", Getdir(cdirapp, "Ubicación de Tablas de "+cfileini))
		m.nom_dbf = Encripta(Dtoc(Date()))
		Append Blank
		Replace dir_dbf With m.dir_dbf
		Replace nom_dbf With m.nom_dbf
		=CloseDbf("&cDirApp\&cFileIni..ini")
	Else
		=opendbf("","&cDirApp\&cFileIni..ini", "")
		m.dir_dbf = Alltrim(dir_dbf)
		m.nom_dbf = Alltrim(nom_dbf)
		=CloseDbf("&cDirApp\&cFileIni..ini")
	Endif
	m.dir_dbf = Iif(Empty(m.dir_dbf), cdirapp+"\datasico", m.dir_dbf)
	Return m.dir_dbf
Endfunc
**
Function UbiDemo
	Parameter cdirapp, cfileini
	If !File("&cDirApp\&cFileIni..ini")
		Create Table &cdirapp\&cfileini..ini (dir_dbf c(60), nom_dbf c(10))
		m.dir_dbf = Getdir(cdirapp, "Ubicación de Tablas de "+cfileini)
		m.nom_dbf = Encripta(Dtoc(Date()))
		Append Blank
		Replace dir_dbf With m.dir_dbf
		Replace nom_dbf With m.nom_dbf
		=CloseDbf("&cDirApp\&cFileIni..ini")
	Else
		=opendbf("","&cDirApp\&cFileIni..ini", "")
		m.dir_dbf = Alltrim(dir_dbf)
		m.nom_dbf = Alltrim(nom_dbf)
		=CloseDbf("&cDirApp\&cFileIni..ini")
	Endif
	Return m.nom_dbf
Endfunc
**
Function NumItem
	Parameter ctabla
	Private ccod, nlen
	If Set('DELETED')='OFF'
		Set Deleted On
	Endif
	Select num_item From sico!d_tablas Where cdg_tab=ctabla Order By num_item Into Cursor curItem
	nlen = 3
	Goto Bottom
	If Eof()
		ccod = CerosIzq(Str(1, nlen))
	Else
		ccod = CerosIzq(Str(Val(curItem.num_item)+1, nlen))
	Endif
	Use In curItem
	Close Table
	If Set('DELETED')='ON'
		Set Deleted Off
	Endif
	Return (ccod)
Endfunc
**
Function Pendiente_Facturar
	Parameter corden, cprod
	Private npend, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select (d_ordcom.can_oins-d_ordcom.can_fact) As pendiente From sico!d_ordcom Where d_ordcom.num_ocom=corden And d_ordcom.cdg_prod=cprod Into Cursor curPF
	Select (d_ordcom.can_ialm-d_ordcom.can_fact) As pendiente From sico!d_ordcom Where d_ordcom.num_ocom=corden And d_ordcom.cdg_prod=cprod Into Cursor curPF
	Goto Top
	npend = Iif(Eof() .Or. pendiente<=0, 0, curPF.pendiente)
	Set Deleted &cdel
	Return (npend)
Endfunc
**
Function Cantidad_Facturada
	Parameter corden, cprod
	Private nfact, cdel
	cdel = Set("DELETED")
	nfact = 0
	Set Deleted On
	Select d_docprv.can_dfac As facturado, d_docprv.ruc_prv, d_docprv.cdg_tdoc, d_docprv.num_docu From sico!d_docprv, sico!m_docprv Where d_docprv.num_ocom=corden And d_docprv.cdg_prod=cprod And d_docprv.ruc_prv=m_docprv.ruc_prv And d_docprv.cdg_tdoc=m_docprv.cdg_tdoc And d_docprv.num_docu=m_docprv.num_docu And m_docprv.swt_est<>"A" And  Not Deleted() Into Cursor curCF
	Select curCF
	Goto Top
	Do While  .Not. Eof("curCF")
		nfact = nfact+curCF.facturado
		Skip In curCF
	Enddo
	Set Deleted &cdel
	Return (nfact)
Endfunc
**
Function CorBco
	Parameter cbanco, ccuenta
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select num_cor From sico!m_libbco Where  Not Deleted() And m_libbco.cdg_bco=cbanco And m_libbco.num_cta=ccuenta Order By num_cor Into Cursor curC
	nlen = 10
	Goto Bottom
	ccod = CerosIzq(Str(Val(curC.num_cor)+1, nlen))
	Use In curC
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function CorPgo
	Parameter crucprv, ctipdoc, cnumdoc
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select num_cor From sico!m_pagprv Where  Not Deleted() And m_pagprv.ruc_prv=crucprv And m_pagprv.cdg_tdoc=ctipdoc And m_pagprv.num_docu=cnumdoc Order By num_cor Into Cursor curP
	nlen = 6
	Goto Bottom
	ccod = CerosIzq(Str(Val(curP.num_cor)+1, nlen))
	Use In curP
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function Saldo
	Parameter cbanco, ccuenta, ddesde
	Private nsal, cdel
	cdel = Set("DELETED")
	Set Deleted On
	nsal = 0
	If Parameters()<3
		Select Sum(m_libbco.imp_debe) As debe, Sum(m_libbco.imp_haber) As haber From sico!m_libbco Where  Not Deleted() And m_libbco.swt_est<>"A" And m_libbco.cdg_bco=cbanco And m_libbco.num_cta=ccuenta Into Cursor Curs
	Else
		Select Sum(m_libbco.imp_debe) As debe, Sum(m_libbco.imp_haber) As haber From sico!m_libbco Where  Not Deleted() And m_libbco.cdg_bco=cbanco And m_libbco.num_cta=ccuenta And m_libbco.swt_est<>"A" And m_libbco.fec_docu<ddesde Into Cursor Curs
	Endif
	nsal = Iif(Eof("curS"), 0, Curs.debe-Curs.haber)
	Use In Curs
	Set Deleted &cdel
	Return (nsal)
Endfunc
**
Function CorProd
	Parameter clinea, ctipo
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select m_produc.cdg_prod, m_produc.cdg_tprd, lineas.abr_item As linea From sico!m_produc, sico!d_tablas lineas Where lineas.cdg_tab="LIN" And lineas.num_item=m_produc.cdg_linp And m_produc.cdg_linp=clinea And m_produc.cdg_tprd=ctipo And  Not Deleted() Order By cdg_prod Into Cursor curP
	nlen = 10-(Len(Alltrim(curP.linea))+Len(curP.cdg_tprd))
	Goto Bottom
	ccod = Alltrim(curP.linea)+curP.cdg_tprd+CerosIzq(Str(Val(Right(curP.cdg_prod, 5))+1, nlen))
	Use In curP
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function Productos_Despachados
	Parameter cpedido, cprod
	Private nprod, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select m_guia.num_guia, d_guia.cdg_tpg, d_guia.cdg_prod, Sum(d_guia.can_dgui) As desp From sico!m_guia, sico!d_guia Where  Not Deleted() And m_guia.num_ped=cpedido And d_guia.cdg_prod=cprod And m_guia.num_guia=d_guia.num_guia And m_guia.cdg_tpg=d_guia.cdg_tpg And m_guia.cdg_tpg<>"001" And d_guia.cdg_tpg<>"001" And m_guia.swt_est<>"A" Into Cursor curG
	Goto Top
	nprod = Iif( .Not. Eof("curG"), curG.desp, 0)
	Use In curG
	Set Deleted &cdel
	Return (nprod)
Endfunc
**
Function Estado_Guia
	Parameter ctipo, cguia
	Private cdel, cest
	cdel = Set("DELETED")
	Set Deleted On
	Select m_guia.cdg_tpg, m_guia.num_guia, m_guia.swt_est, Sum(d_guia.can_pgui) As pedido, Sum(d_guia.can_dgui) As despachado, Sum(d_guia.can_fgui) As facturado From sico!m_guia, sico!d_guia Where m_guia.cdg_tpg=ctipo And m_guia.num_guia=cguia And d_guia.cdg_tpg=ctipo And d_guia.num_guia=cguia And m_guia.swt_est<>"A" And  Not Deleted() Group By d_guia.num_guia Into Cursor curEstado
	Goto Top
	If  .Not. Eof()
		If curEstado.swt_est="A"
			cest = "A"
		Else
			If curEstado.swt_est="T"
				cest = "T"
			Else
				If curEstado.swt_est="C"
					cest = "C"
				Else
					cest = Iif(curEstado.pedido=curEstado.despachado .And. curEstado.despachado=curEstado.facturado, "T", "")
				Endif
			Endif
		Endif
	Else
		cest = ""
	Endif
	Use In curEstado
	Set Deleted &cdel
	Return (cest)
Endfunc
**
Function Productos_Facturados
	Parameter cpedido, cprod
	Private nfact, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select Sum(d_doccli.can_dfac) As facturado, d_doccli.num_docu, d_doccli.cdg_tdoc From sico!d_doccli, sico!m_doccli Where  Not Deleted() And d_doccli.num_ped=cpedido And d_doccli.cdg_prod=cprod And d_doccli.cdg_tdoc=m_doccli.cdg_tdoc And d_doccli.num_docu=m_doccli.num_docu And m_doccli.swt_est<>"A" Into Cursor curCF
	Goto Top
	nfact = Iif(Eof("curCF") .Or. facturado<=0, 0, curCF.facturado)
	Set Deleted &cdel
	Return (nfact)
Endfunc
**
Function Total_Cobrado
	Parameter ctipo, cdocum
	Private ncobr, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select d_pagcli.num_inf, d_pagcli.imp_cob, d_pagcli.cdg_mon As mondocu, m_pagcli.cdg_mon As monpago, d_pagcli.fec_cob, d_pagcli.cob_cmb As tip_cmb, d_pagcli.cdg_tdoc, d_pagcli.num_docu From sico!d_pagcli, sico!m_pagcli Where  Not Deleted() And d_pagcli.num_inf=m_pagcli.num_inf And d_pagcli.cdg_tdoc=ctipo And d_pagcli.num_docu=cdocum Order By d_pagcli.num_inf, d_pagcli.cdg_tdoc, d_pagcli.num_docu Into Cursor curTC
	ncobr = 0
	Goto Top
	Do While  .Not. Eof("curTC")
		Do Case
			Case curTC.monpago="001" .And. curTC.mondocu="002"
				nimpcob = Round((curTC.imp_cob/curTC.tip_cmb), 2)
			Case curTC.monpago="002" .And. curTC.mondocu="001"
				nimpcob = Round((curTC.imp_cob*curTC.tip_cmb), 2)
			Otherwise
				nimpcob = curTC.imp_cob
		Endcase
		ncobr = ncobr+nimpcob
		Skip In curTC
	Enddo
	Set Deleted &cdel
	Return (ncobr)
Endfunc
**
Function Total_Pagado
	Parameter cprov, ctipo, cdocum
	Private ncobr, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select m_pagprv.num_cor, m_pagprv.imp_pago, m_cteprv.cdg_mon As mondocu, m_pagprv.cdg_mon As monpago, m_pagprv.fec_pago, m_pagprv.ruc_prv, m_pagprv.pag_cmb As tip_cmb, m_pagprv.cdg_tdoc, m_pagprv.num_docu From sico!m_pagprv, sico!m_cteprv Where m_pagprv.ruc_prv=m_cteprv.ruc_prv And m_pagprv.cdg_tdoc=m_cteprv.cdg_tdoc And m_pagprv.num_docu=m_cteprv.num_docu And m_pagprv.ruc_prv=cprov And m_pagprv.cdg_tdoc=ctipo And m_pagprv.num_docu=cdocum And Empty(m_pagprv.swt_pago) And  Not Deleted() Order By m_pagprv.num_cor Into Cursor curTC
	ncobr = 0
	nimpcob = 0
	Goto Top
	Do While  .Not. Eof("curTC")
		Do Case
			Case curTC.monpago="001" .And. curTC.mondocu="002"
				nimpcob = Round((curTC.imp_pago/curTC.tip_cmb), 2)
			Case curTC.monpago="002" .And. curTC.mondocu="001"
				nimpcob = Round((curTC.imp_pago*curTC.tip_cmb), 2)
			Otherwise
				nimpcob = curTC.imp_pago
		Endcase
		ncobr = ncobr+nimpcob
		Skip In curTC
	Enddo
	Set Deleted &cdel
	Return (ncobr)
Endfunc
**
Function CorGuia
	Parameter ctipo
	Private ccod, nlen, cdel
	nlen = 10
	cdel = Set("DELETED")
	Set Deleted On
	=opendbf("&gDirDbf\sico", "t_guia.dbf", "cdg_tpg")
	If Seek(ctipo, "t_guia")
		ccod = CerosIzq(Str(Val(t_guia.num_guia)+1, nlen))
	Else
		ccod = CerosIzq(Str(1, nlen))
	Endif
	=CloseDbf("&gDirDbf\t_guia.dbf")
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function CorDocu
	Parameter ctipo
	Private ccod, nlen, cdel
	nlen = 10
	cdel = Set("DELETED")
	Set Deleted On
	=opendbf("&gDirDbf\sico", "t_doccli.dbf", "cdg_tdoc")
	If Seek(ctipo, "t_doccli")
		ccod = CerosIzq(Str(Val(t_doccli.num_docu)+1, nlen))
	Else
		ccod = CerosIzq(Str(1, nlen))
	Endif
	=CloseDbf("&gDirDbf\t_doccli.dbf")
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function Precio_Prod
	Parameter cprod, cmoneda
	Private nprec, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select m_produc.val_sol, m_produc.val_dol, t_cambio.tip_cmb From sico!m_produc, sico!t_cambio Where  Not Deleted() And m_produc.cdg_prod=cprod And Dtos(t_cambio.fec_cmb)=Dtos(Date()) Into Cursor curPrec
	nprec = 0
	Goto Top
	If  .Not. Eof("curPrec")
		Do Case
			Case cmoneda="002"
				nprec = Iif( .Not. Empty(curPrec.val_dol), curPrec.val_dol, Round((curPrec.val_sol/curPrec.tip_cmb), 2))
			Case cmoneda="001"
				nprec = Iif( .Not. Empty(curPrec.val_sol), curPrec.val_sol, Round((curPrec.val_dol*curPrec.tip_cmb), 2))
		Endcase
	Endif
	Set Deleted &cdel
	Return (nprec)
Endfunc
**
Procedure RepGuia
	Parameter ndocumento, ntipo
	If Parameters()=1
		ntipo = "003"
	Endif
	=opendbf("&gDirDbf\sico", "m_guia.dbf", "num_guia")
	dfecguia = Ctod("  /  /    ")
	cruccli = ""
	cnumped = ""
	ccdgchof = ""
	ccarplac = ""
	ccdgtmov = ""
	cpartida = ""
	cdestino = ""
	ncpact = 1252
	ncpnew = 850
	If Seek(ntipo+ndocumento, "m_guia")
		ncpact = Cpdbf()
		dfecguia = m_guia.fec_guia
		cruccli = m_guia.ruc_cli
		cnumped = m_guia.num_ped
		ccdgchof = m_guia.cdg_chof
		ccarplac = m_guia.car_plac
		ccdgtmov = m_guia.cdg_tmov
		cpartida = m_guia.partida
		cdestino = m_guia.destino
	Endif
	=opendbf("&gDirDbf\sico", "m_client.dbf", "ruc_cli")
	cdescli = ""
	cdircli = ""
	ccdgudis = ""
	ccdgalt = ""
	nswtalt = 0
	If Seek(cruccli, "m_client")
		cdescli = Alltrim(m_client.des_cli)
		cdircli = Alltrim(m_client.dir_cli)
		ccdgudis = m_client.cdg_udis
		ccdgalt = Alltrim(m_client.cdg_alt)
		nswtalt = m_client.swt_alt
	Endif
	=opendbf("&gDirDbf\sico", "m_chofer.dbf", "cdg_chof")
	cdeschof = ""
	cdirchof = ""
	crucchof = ""
	If Seek(ccdgchof, "m_chofer")
		cdeschof = m_chofer.des_chof
		cdirchof = m_chofer.dir_chof
		crucchof = m_chofer.ruc_chof
	Endif
	=opendbf("&gDirDbf\sico", "d_tablas.dbf", "tab_item")
	cdestmov = Iif(Seek("TMV"+ccdgtmov, "d_tablas"), Alltrim(d_tablas.des_item), "")
	cdesudis = Iif(Seek("DST"+ccdgudis, "d_tablas"), Alltrim(d_tablas.des_item), "")
	cdireccion = cdircli+Iif(ccdgudis<>"000", " - "+cdesudis, "")
	Set Device To Printer
	cprintini = Chr(27)+Chr(64)+Chr(27)+Chr(67)+Chr(50)+Chr(27)+Chr(15)
	??? cprintini
	nfil = 0
	Set Margin To 4
	@ 09, 27 Say Day(dfecguia)
	@ 09, 37 Say Mes(dfecguia)
	@ 09, 59 Say Str(Year(dfecguia), 4)
	@ 13, 27 Say Iif( .Not. Empty(cnumped), "X", " ")
	@ 14, 125 Say Iif(Empty(cnumped), "X", " ")
	@ 15, 22 Say Cpconvert(ncpact, ncpnew, cdescli)
	@ 17, 18 Say Cpconvert(ncpact, ncpnew, Iif(nswtalt=1, ccdgalt, cruccli))
	@ 18, 20 Say Cpconvert(ncpact, ncpnew, Iif( .Not. Empty(cdestino), cdestino, cdireccion))
	@ 20, 112 Say Cpconvert(ncpact, ncpnew, crucchof)
	@ 21, 16 Say Cpconvert(ncpact, ncpnew, ccarplac)
	@ 21, 50 Say Cpconvert(ncpact, ncpnew, cdeschof)
	nlin = 25
	=opendbf("&gDirDbf\sico", "m_produc.dbf", "cdg_prod")
	=opendbf("&gDirDbf\sico", "d_guia.dbf", "guia_prod")
	If Seek(ntipo+ndocumento, "d_guia")
		Do While d_guia.cdg_tpg+d_guia.num_guia=ntipo+ndocumento
			If d_guia.can_dgui<=0
				Skip In d_guia
				Loop
			Endif
			ccdgprod = d_guia.cdg_prod
			cdesprod = Iif(Seek(ccdgprod, "m_produc"), Alltrim(m_produc.des_prod), "")
			ccdgumed = Iif(Seek(ccdgprod, "m_produc"), m_produc.cdg_umed, "")
			ntotal = Round(d_guia.can_dgui*d_guia.pre_guia, 2)
			@ nlin, 08 Say Transform(d_guia.can_dgui, "999 999.99")
			@ nlin, 20 Say Cpconvert(ncpact, ncpnew, cdesprod)
			nlin = nlin+1
			Select d_guia
			Skip In d_guia
		Enddo
	Endif
	@ 43, 01 Say " "+Chr(27)+Chr(18)
	Eject
	Set Printer To
	Set Device To Screen
	Return
Endproc
**
Procedure RepFact
	Parameter nrofactura, ntipo
	If Parameters()=1
		ntipo = "002"
	Endif
	=opendbf("&gDirDbf\sico", "m_doccli.dbf", "doc_cli")
	dfecdocu = Ctod("  /  /    ")
	cruccli = ""
	cdescli = ""
	cdireccion = ""
	ccdgmon = ""
	cobsdocu = ""
	nportigv = 0
	nsubtotal = 0
	ntotigv = 0
	ntotal = 0
	ncpact = 1252
	ncpnew = 850
	If Seek(ntipo+nrofactura, "m_doccli")
		ncpact = Cpdbf()
		dfecdocu = m_doccli.fec_docu
		cruccli = m_doccli.ruc_cli
		cdescli = Alltrim(m_doccli.des_cli)
		cdireccion = Alltrim(m_doccli.direccion)
		ccdgmon = m_doccli.cdg_mon
		cobsdocu = m_doccli.obs_fcli
		nportigv = m_doccli.por_tigv
		nsubtotal = m_doccli.val_fsub
		ntotigv = m_doccli.val_figv
		ntotal = m_doccli.val_ftot
	Endif
	=opendbf("&gDirDbf\sico", "m_client.dbf", "ruc_cli")
	ccdgudis = ""
	ccdgalt = ""
	nswtalt = 0
	If Seek(cruccli, "m_client")
		ccdgudis = m_client.cdg_udis
		ccdgalt = Alltrim(m_client.cdg_alt)
		nswtalt = m_client.swt_alt
	Endif
	=opendbf("&gDirDbf\sico", "d_tablas.dbf", "tab_item")
	cdesudis = Iif(Seek("DST"+ccdgudis, "d_tablas"), Alltrim(d_tablas.des_item), "")
	cdircli = cdireccion+Iif(ccdgudis<>"000", " - "+cdesudis, "")
	cmoneda = Iif(ccdgmon="002", "US$", "S/.")
	Set Device To Printer
	cprintini = Chr(27)+Chr(64)+Chr(27)+Chr(67)+Chr(37)+Chr(27)+Chr(15)
	??? cprintini
	Set Margin To 4
	Set Memowidth To 72
	@ 10, 15 Say Cpconvert(ncpact, ncpnew, cdescli)
	@ 11, 15 Say Cpconvert(ncpact, ncpnew, Iif(nswtalt=1, ccdgalt, cruccli))
	@ 11, 87 Say CerosIzq(Str(Day(dfecdocu), 2))
	@ 11, 100 Say Mes(dfecdocu)
	@ 11, 123 Say Str(Year(dfecdocu), 4)
	@ 12, 15 Say Cpconvert(ncpact, ncpnew, Left(cdircli, 110))
	nlin = 16
	Select m_doccli
	For ni = 1 To Memlines(m_doccli.obs_fcli)
		If ni>10
			Exit
		Endif
		@ nlin, 18 Say Cpconvert(ncpact, ncpnew, Mline(m_doccli.obs_fcli, ni))
		If ni=1
			@ nlin, 110 Say cmoneda+Transform(nsubtotal, "999 999.99")
		Endif
		nlin = nlin+1
	Endfor
	cletras = NumLetNew(ntotal)+" Y "+CerosIzq(Str(100*(ntotal-Int(ntotal)), 2))+'/100'+Iif(ccdgmon="002", " DOLARES AMERICANOS", " NUEVOS SOLES")+" S.E.U.O."
	@ 26, 05 Say "SON: "+cletras
	@ 27, 110 Say cmoneda+Transform(nsubtotal, "999 999.99")
	@ 29, 99 Say Transform(nportigv, "999")
	@ 29, 110 Say cmoneda+Transform(ntotigv, "999 999.99")
	@ 30, 110 Say cmoneda+Transform(ntotal, "999 999.99")
	@ 31, 01 Say " "
	@ 31, 05 Say Chr(27)+Chr(18)
	Eject
	Set Printer To
	Set Device To Screen
	Return
Endproc
**
Procedure RepBole
	Parameter nrofactura, ntipo
	If Parameters()=1
		ntipo = "001"
	Endif
	=opendbf("&gDirDbf\sico", "m_doccli.dbf", "doc_cli")
	dfecdocu = Ctod("  /  /    ")
	cruccli = ""
	cdescli = ""
	cdireccion = ""
	cmoneda = ""
	nportigv = 0
	nsubtotal = 0
	ntotdcto = 0
	ntotigv = 0
	ntotal = 0
	cobsdocu = ""
	ncpact = 1252
	ncpnew = 850
	If Seek(ntipo+nrofactura, "m_doccli")
		ncpact = Cpdbf()
		dfecdocu = m_doccli.fec_docu
		cruccli = m_doccli.ruc_cli
		cdescli = Alltrim(m_doccli.des_cli)
		cdireccion = Alltrim(m_doccli.direccion)
		nsubtotal = m_doccli.val_fsub
		nportigv = m_doccli.por_tigv
		ntotdcto = m_doccli.val_fdes
		ntotigv = m_doccli.val_figv
		ntotal = m_doccli.val_ftot
		ccdgmon = m_doccli.cdg_mon
		cobsdocu = m_doccli.obs_fcli
	Endif
	=opendbf("&gDirDbf\sico", "m_client.dbf", "ruc_cli")
	ccdgudis = ""
	ccdgalt = ""
	nswtalt = 0
	If Seek(cruccli, "m_client")
		ccdgudis = m_client.cdg_udis
		ccdgalt = Alltrim(m_client.cdg_alt)
		nswtalt = m_client.swt_alt
	Endif
	=opendbf("&gDirDbf\sico", "d_tablas.dbf", "tab_item")
	cdesudis = Iif(Seek("DST"+ccdgudis, "d_tablas"), Alltrim(d_tablas.des_item), "")
	cdircli = cdireccion+Iif(ccdgudis<>"000", " - "+cdesudis, "")
	cmoneda = Iif(ccdgmon="002", "US$", "S/.")
	Set Device To Printer
	cprintini = Chr(27)+Chr(64)+Chr(27)+Chr(67)+Chr(36)+Chr(27)+Chr(15)
	??? cprintini
	Set Margin To 4
	@ 11, 15 Say Day(dfecdocu)
	@ 11, 32 Say Mes(dfecdocu)
	@ 11, 56 Say Str(Year(dfecdocu), 4)
	@ 13, 16 Say Cpconvert(ncpact, ncpnew, cdescli)
	@ 14, 16 Say Cpconvert(ncpact, ncpnew, cdircli)
	ntotal = 0
	npredcto = 0
	nlin = 18
	=opendbf("&gDirDbf\sico", "m_produc.dbf", "cdg_prod")
	=opendbf("&gDirDbf\sico", "d_doccli.dbf", "doc_prod")
	If Seek(ntipo+nrofactura, "d_doccli")
		Do While d_doccli.cdg_tdoc+d_doccli.num_docu=ntipo+nrofactura
			ccdgprod = d_doccli.cdg_prod
			cswtigv = d_doccli.swt_igv
			cdesprod = Iif(Seek(ccdgprod, "m_produc"), Alltrim(m_produc.des_prod), "")
			@ nlin, 04 Say Transform(d_doccli.can_dfac, "999 999.99")
			@ nlin, 15 Say Cpconvert(ncpact, ncpnew, Left(cdesprod, 75))
			@ nlin, 96 Say Transform(d_doccli.pre_igv, "999 999.99")
			@ nlin, 109 Say cmoneda
			@ nlin, 112 Say Transform(d_doccli.imp_igv, "999 999.99")
			nlin = nlin+1
			Select d_doccli
			Skip In d_doccli
		Enddo
	Endif
	cletras = NumLetNew(ntotal)+" Y "+CerosIzq(Str(100*(ntotal-Int(ntotal)), 2))+'/100'+Iif(ccdgmon="002", " DOLARES AMERICANOS", " NUEVOS SOLES")
	@ 29, 04 Say "SON: "+cletras
	@ 30, 109 Say cmoneda+Transform(ntotal, "999 999.99")
	@ 31, 01 Say " "+Chr(27)+Chr(18)
	Eject
	Set Printer To
	Set Device To Screen
	Return
Endproc
**
Function Cobrado_En
	Parameter ctipo, cdocum, ddesde, dhasta
	Private ncobr, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select d_pagcli.num_inf, d_pagcli.imp_cob, d_pagcli.cdg_mon As mondocu, m_pagcli.cdg_mon As monpago, d_pagcli.fec_cob, d_pagcli.cob_cmb As tip_cmb, d_pagcli.cdg_tdoc, d_pagcli.num_docu From sico!d_pagcli, sico!m_pagcli Where  Not Deleted() And d_pagcli.num_inf=m_pagcli.num_inf And Dtos(d_pagcli.fec_cob)>=Dtos(ddesde) And Dtos(d_pagcli.fec_cob)<=Dtos(dhasta) And d_pagcli.cdg_tdoc=ctipo And d_pagcli.num_docu=cdocum Order By d_pagcli.num_inf, d_pagcli.cdg_tdoc, d_pagcli.num_docu Into Cursor curTC
	ncobr = 0
	Goto Top
	Do While  .Not. Eof("curTC")
		Do Case
			Case curTC.monpago="001" .And. curTC.mondocu="002"
				nimpcob = Round((curTC.imp_cob/curTC.tip_cmb), 2)
			Case curTC.monpago="002" .And. curTC.mondocu="001"
				nimpcob = Round((curTC.imp_cob*curTC.tip_cmb), 2)
			Otherwise
				nimpcob = curTC.imp_cob
		Endcase
		ncobr = ncobr+nimpcob
		Skip In curTC
	Enddo
	Set Deleted &cdel
	Return (ncobr)
Endfunc
**
Function Cobrado_Al
	Parameter ctipo, cdocum, cmes, cano
	Private ncobr, cdel, cmondocu, cmonpago
	cdel = Set("DELETED")
	ncobr = 0
	Set Deleted On
	=opendbf("&gDirDbf\sico", "m_pagcli.dbf", "num_inf")
	=opendbf("&gDirDbf\sico", "d_pagcli.dbf", "docu_cob")
	If Seek(ctipo+cdocum, "d_pagcli")
		Do While d_pagcli.cdg_tdoc+d_pagcli.num_docu=ctipo+cdocum
			If Left(Dtos(d_pagcli.fec_cob), 6)>cano+cmes
				Skip In d_pagcli
				Loop
			Endif
			cnuminf = d_pagcli.num_inf
			cmondocu = d_pagcli.cdg_mon
			cmonpago = Iif(Seek(cnuminf, "m_pagcli"), m_pagcli.cdg_mon, "001")
			ntipcmb = d_pagcli.cob_cmb
			Do Case
				Case cmonpago="001" .And. cmondocu="002"
					nimpcob = Round((d_pagcli.imp_cob/ntipcmb), 2)
				Case cmonpago="002" .And. cmondocu="001"
					nimpcob = Round((d_pagcli.imp_cob*ntipcmb), 2)
				Otherwise
					nimpcob = d_pagcli.imp_cob
			Endcase
			ncobr = ncobr+nimpcob
			Skip In d_pagcli
		Enddo
	Endif
	Set Deleted &cdel
	Return (ncobr)
Endfunc
**
Function Pagado_al
	Parameter cruc, ctipo, cdocum, cmes, cano
	Private ncobr, cdel
	cdel = Set("DELETED")
	ncobr = 0
	Set Deleted On
	=opendbf("&gDirDbf\sico", "t_cambio.dbf", "fec_cmb")
	=opendbf("&gDirDbf\sico", "m_docprv.dbf", "prv_docu")
	=opendbf("&gDirDbf\sico", "m_pagprv.dbf", "prv_docu")
	Select m_pagprv
	If Seek(cruc+ctipo+cdocum, "m_pagprv")
		Do While m_pagprv.ruc_prv+m_pagprv.cdg_tdoc+m_pagprv.num_docu=cruc+ctipo+cdocum
			If Left(Dtos(m_pagprv.fec_pago), 6)>cano+cmes
				Skip In m_pagprv
				Loop
			Endif
			If m_pagprv.swt_pago="A"
				Skip In m_pagprv
				Loop
			Endif
			cmonpago = m_pagprv.cdg_mon
			cmondocu = Iif(Seek(cruc+ctipo+cdocum, "m_docprv"), m_docprv.cdg_mon, "001")
			ntipcmb = Iif(Seek(Dtos(m_pagprv.fec_pago), "t_cambio"), t_cambio.tip_cmb, 1)
			Do Case
				Case cmonpago="001" .And. cmondocu="002"
					nimpcob = Round((m_pagprv.imp_pago/ntipcmb), 2)
				Case cmonpago="002" .And. cmondocu="001"
					nimpcob = Round((m_pagprv.imp_pago*ntipcmb), 2)
				Otherwise
					nimpcob = m_pagprv.imp_pago
			Endcase
			ncobr = ncobr+nimpcob
			Select m_pagprv
			Skip In m_pagprv
		Enddo
	Endif
	Set Deleted &cdel
	Return (ncobr)
Endfunc
**
Function Limpieza
	Private cexcl
	cexcl = Set("EXCLUSIVE")
	Set Exclusive On
	Dimension gadatabase(1, 5)
	gndbcnumber = Adir(gadatabase, '&gDirDbf\*.DBF')
	For ncount = 1 To gndbcnumber
		cnomdbf = Upper(Alltrim(gadatabase(ncount, 1)))
		ctabla = Left(cnomdbf, Rat(".", cnomdbf)-1)
		If cnomdbf="M_USUARI.DBF" .Or. cnomdbf="D_USUARI.DBF" .Or. cnomdbf="M_OPCION.DBF" .Or. cnomdbf="D_OPCION.DBF"
			=opendbf("&gDirDbf\Sico",cnomdbf, "")
			Select &ctabla
			Delete All
			=CloseDbf("&gDirDbf\&cNomDbf")
		Endif
	Endfor
	Set Exclusive &cexcl
	Return 0
Endfunc
**
Function CorCaja
	Parameter carea
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select Max(num_caja) As num_cor From sico!m_caja Where m_caja.cdg_area=carea Into Cursor curCj
	nlen = 6
	ccod = CerosIzq(Str(Val(curCj.num_cor)+1, nlen))
	Use In curCj
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function DetCaja
	Parameter carea, ccaja
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select num_cor From sico!d_caja Where d_caja.cdg_area=carea And d_caja.num_caja=ccaja Order By num_cor Into Cursor curC
	nlen = 3
	Goto Bottom
	ccod = CerosIzq(Str(Val(curC.num_cor)+1, nlen))
	Use In curC
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function Estado_Caja
	Parameter carea, ccaja
	Private cest, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select * From sico!m_caja Where m_caja.cdg_area=carea And m_caja.num_caja<>ccaja Order By num_caja Into Cursor curEstado
	Goto Bottom
	cest = Iif(Eof("curEstado"), 1, curEstado.swt_caja)
	Use In curEstado
	Set Deleted &cdel
	Return (cest)
Endfunc
**
Function Saldo_Caja
	Parameter carea, ccaja, cmoneda
	Private nsaldo, cdel, cantcaja, cmoncaja, ntotcaja, ncambio
	cdel = Set("DELETED")
	Set Deleted On
	=opendbf("&gDirDbf\Sico","t_cambio.dbf", "fec_cmb")
	Goto Bottom
	ncambio = t_cambio.tip_cmb
	= CloseDbf("t_cambio.dbf")
	Select * From sico!m_caja Where m_caja.cdg_area=carea And m_caja.num_caja<>ccaja Order By num_caja Desc Into Cursor curCaja
	Select curCaja
	Goto Top
	nsaldo = 0
	ntotcaja = 0
	Do While  .Not. Eof("curCaja")
		If curCaja.est_caja="A"
			Skip In curCaja
			Loop
		Endif
		cantcaja = curCaja.num_caja
		cmoncaja = curCaja.cdg_mon
		ntotcaja = curCaja.tot_caja
		Select Sum(imp_dcja) As imp_caja From sico!d_caja Where d_caja.cdg_area=carea And d_caja.num_caja=cantcaja Group By num_caja Into Cursor curDet
		Do Case
			Case cmoneda="001" .And. cmoncaja<>"001"
				nsaldo = Round((ntotcaja-curDet.imp_caja)*ncambio, 2)
			Case cmoneda<>"001" .And. cmoncaja="001"
				nsaldo = Round((ntotcaja-curDet.imp_caja)/ncambio, 2)
			Otherwise
				nsaldo = (ntotcaja-curDet.imp_caja)
		Endcase
		Use In curDet
		Exit
	Enddo
	Use In curCaja
	Set Deleted &cdel
	Return (nsaldo)
Endfunc
**
Function UltCaja
	Parameter carea
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select Max(num_caja) As num_cor From sico!m_caja Where m_caja.cdg_area=carea Into Cursor curCj
	nlen = 6
	ccod = Iif(Eof("curCj"), "", CerosIzq(Str(Val(curCj.num_cor), nlen)))
	Use In curCj
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function CorCope
	Parameter ctope
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select Max(cdg_cope) As num_cor From sico!t_tipope Where  Not Deleted() And t_tipope.cdg_tope=ctope Into Cursor curCope
	nlen = 3
	ccod = Iif(Eof("curCope"), "001", CerosIzq(Str(Val(curCope.num_cor)+1, nlen)))
	Use In curCope
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function NumOpc
	Parameter cmod
	Private ccod, nlen
	If Set('DELETED')='OFF'
		Set Deleted On
	Endif
	Select num_item From sico!d_opcion Where cdg_opc=cmod Order By num_item Into Cursor curItem
	nlen = 3
	Goto Bottom
	If Eof()
		ccod = CerosIzq(Str(1, nlen))
	Else
		ccod = CerosIzq(Str(Val(curItem.num_item)+1, nlen))
	Endif
	Use In curItem
	Close Table
	Return (ccod)
Endfunc
**
Function CorRub
	Parameter cestfin
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select Max(cdg_rub) As num_cor From sico!t_rubros Where  Not Deleted() And t_rubros.swt_ef=cestfin Into Cursor curRb
	nlen = 3
	ccod = CerosIzq(Str(Val(curRb.num_cor)+1, nlen))
	Use In curRb
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function CorLin
	Parameter cestfin, crubro
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select Max(cdg_lin) As num_cor From sico!t_lineas Where  Not Deleted() And t_lineas.swt_ef=cestfin And t_lineas.cdg_rub=crubro Into Cursor curLn
	nlen = 3
	ccod = CerosIzq(Str(Val(curLn.num_cor)+1, nlen))
	Use In curLn
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function CorVouc
	Parameter cano, cmes, ctas
	Private ccod, nlen, cdel, cnumvouc
	cdel = Set("DELETED")
	nlen = 10
	cnumvouc = ""
	Set Deleted On
	=opendbf("&gDirDbf\sico", "m_vouche.dbf", "mes_vouc")
	If Seek(cano+cmes+ctas, "m_vouche")
		Do While m_vouche.ano_vouc+m_vouche.mes_vouc+m_vouche.cdg_tas=cano+cmes+ctas
			cnumvouc = m_vouche.num_vouc
			Skip In m_vouche
		Enddo
	Endif
	ccod = CerosIzq(Str(Val(cnumvouc)+1, nlen))
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function CorCtaBco
	Parameter cbanco
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select Max(cdg_cta) As num_cor From sico!t_ctabco Where  Not Deleted() And t_ctabco.cdg_bco=cbanco Into Cursor curCB
	nlen = 4
	ccod = CerosIzq(Str(Val(curCB.num_cor)+1, nlen))
	Use In curCB
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function Usado_Anticipo
	Parameter canticipo
	Private ncobr, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select d_pagcli.num_inf, d_pagcli.imp_cob, d_pagcli.num_cheq, forpag.tip_item As swt_fpag, m_antcli.cdg_mon As mondocu, m_pagcli.cdg_mon As monpago, d_pagcli.fec_cob, d_pagcli.cob_cmb As tip_cmb From sico!d_pagcli, sico!m_pagcli, sico!d_tablas forpag, sico!m_antcli Where  Not Deleted() And forpag.cdg_tab="FPA" And d_pagcli.num_inf=m_pagcli.num_inf And d_pagcli.cdg_fpag=forpag.num_item And forpag.tip_item="A" And Left(d_pagcli.num_cheq, 10)=m_antcli.num_docu And m_antcli.num_docu=canticipo Order By d_pagcli.num_inf, d_pagcli.num_cheq Into Cursor curUA
	ncobr = 0
	Goto Top
	Do While  .Not. Eof("curUA")
		Do Case
			Case curUA.monpago="001" .And. curUA.mondocu="002"
				nimpcob = Round((curUA.imp_cob/curUA.tip_cmb), 2)
			Case curUA.monpago="002" .And. curUA.mondocu="001"
				nimpcob = Round((curUA.imp_cob*curUA.tip_cmb), 2)
			Otherwise
				nimpcob = curUA.imp_cob
		Endcase
		ncobr = ncobr+nimpcob
		Skip In curUA
	Enddo
	Set Deleted &cdel
	Return (ncobr)
Endfunc
**
Function CorCte
	Parameter cemple, cconcep
	Private ccod, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select num_cor From sico!m_ctetrb Where  Not Deleted() And m_ctetrb.cdg_emp=cemple And m_ctetrb.cdg_conc=cconcep Order By num_cor Into Cursor curC
	nlen = 5
	Goto Bottom
	ccod = CerosIzq(Str(Val(curC.num_cor)+1, nlen))
	Use In curC
	Set Deleted &cdel
	Return (ccod)
Endfunc
**
Function HistCli
	Private chist, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	=opendbf("&gDirDbf\sico", "h_letcli.dbf", "hist_docu")
	nlen = 10
	Goto Bottom
	chist = CerosIzq(Str(Val(h_letcli.num_hist)+1, nlen))
	Set Deleted &cdel
	Return (chist)
Endfunc
**
Function HistPrv
	Private chist, nlen, cdel
	cdel = Set("DELETED")
	Set Deleted On
	Select num_hist From sico!m_letprv Where  Not Deleted() Order By num_hist Into Cursor curHisp
	nlen = 10
	Goto Bottom
	chist = CerosIzq(Str(Val(curHisp.num_hist)+1, nlen))
	Use In curHisp
	Set Deleted &cdel
	Return (chist)
Endfunc
**
Function DiasMes
	Parameter cyear, cmes
	Private ndias
	Do Case
		Case cmes="04" .Or. cmes="06" .Or. cmes="09" .Or. cmes="11"
			ndias = 30
		Case cmes="02"
			ndias = Iif(Mod(Val(cyear), 4)=0, 29, 28)
		Otherwise
			ndias = 31
	Endcase
	Return (ndias)
Endfunc
**
Function Costo_Venta
	Parameter cprod, carea, Cmon, ddesde, dhasta
	Private ncanant, npreant, ntotant, nctovta, ncambio, ncosto
	ncanant = 0
	npreant = 0
	ntotant = 0
	nctovta = 0
	ncosto = 0
	ncambio = 1
	Select d_guia.cdg_tpg, d_guia.num_guia, m_guia.fec_guia, d_guia.can_dgui, d_guia.pre_guia, d_guia.cdg_prod, m_guia.cdg_mon, t_cambio.tip_cmb, m_guia.num_ped, m_guia.cdg_tdoc, m_guia.num_docu, m_guia.tip_cmb As cambio From sico!d_guia, sico!m_guia, sico!t_cambio Where d_guia.cdg_tpg="001" And m_guia.cdg_tpg="001" And d_guia.cdg_tpg=m_guia.cdg_tpg And d_guia.num_guia=m_guia.num_guia And Dtos(m_guia.fec_guia)=Dtos(t_cambio.fec_cmb) And m_guia.swt_est<>"A" And m_guia.fec_guia<=dhasta And m_guia.cdg_area=carea And d_guia.cdg_prod=cprod Union Select d_guia.cdg_tpg, d_guia.num_guia, m_guia.fec_guia, d_guia.can_dgui, d_guia.pre_guia, d_guia.cdg_prod, m_guia.cdg_mon, t_cambio.tip_cmb, m_guia.num_ped, m_guia.cdg_tdoc, m_guia.num_docu, 1.000  As cambio From sico!d_guia, sico!m_guia, sico!t_cambio Where d_guia.cdg_tpg<>"001" And m_guia.cdg_tpg<>"001" And d_guia.cdg_tpg=m_guia.cdg_tpg And d_guia.num_guia=m_guia.num_guia And Dtos(m_guia.fec_guia)=Dtos(t_cambio.fec_cmb) And m_guia.swt_est<>"A" And m_guia.fec_guia<=dhasta And m_guia.ori_area=carea And d_guia.cdg_prod=cprod Order By 3, 1, 2 Into Cursor curCvta
 SELECT curcvta
 GOTO TOP
 DO WHILE  .NOT. EOF("curCvta")
    IF curcvta.cdg_tpg<>"001"
       ncambio = IIF(curcvta.tip_cmb>0, curcvta.tip_cmb, curcvta.cambio)
    ELSE
       ncambio = IIF(curcvta.cambio>0, curcvta.cambio, curcvta.tip_cmb)
    ENDIF
    IF curcvta.cdg_mon<>cmon
       DO CASE
          CASE curcvta.cdg_mon="001" .AND. cmon<>"001"
             ncosto = ROUND(curcvta.pre_guia/ncambio, 3)
          CASE curcvta.cdg_mon<>"001" .AND. cmon="001"
             ncosto = ROUND(curcvta.pre_guia*ncambio, 3)
       ENDCASE
    ELSE
       ncosto = curcvta.pre_guia
    ENDIF
    m.can_ing = IIF(curcvta.cdg_tpg="001", curcvta.can_dgui, 0)
    m.pre_ing = IIF(curcvta.cdg_tpg="001", IIF(ncosto=0, npreant, ncosto), 0)
    m.tot_ing = IIF(curcvta.cdg_tpg="001", ROUND(m.can_ing*m.pre_ing, 2), 0)
    m.can_sal = IIF(curcvta.cdg_tpg<>"001", curcvta.can_dgui, 0)
    m.pre_sal = IIF(curcvta.cdg_tpg<>"001", npreant, 0)
    m.tot_sal = IIF(curcvta.cdg_tpg<>"001", ROUND(m.can_sal*m.pre_sal, 2), 0)
    m.can_sld = ncanant+m.can_ing-m.can_sal
    m.tot_sld = ntotant+m.tot_ing-m.tot_sal
    IF ncanant<0 .OR. ntotant<0
       ncansld = ABS(ncanant)+m.can_ing-m.can_sal
       ntotsld = ABS(ntotant)+m.tot_ing-m.tot_sal
       m.pre_sld = IIF(ncansld=0, npreant, ROUND(ntotsld/ncansld, 3))
       m.tot_sld = ROUND(m.can_sld*m.pre_sld, 2)
    ENDIF
    IF m.can_sld<=0 .OR. m.tot_sld<=0
       m.pre_sld = npreant
    ELSE
       IF ncanant<0 .OR. ntotant<0
       ELSE
          m.pre_sld = ROUND(m.tot_sld/m.can_sld, 3)
       ENDIF
    ENDIF
    ncanant = m.can_sld
    npreant = m.pre_sld
    ntotant = m.tot_sld
    IF (curcvta.cdg_tpg="002" .AND.  .NOT. EMPTY(curcvta.num_docu) .AND.  .NOT. EMPTY(curcvta.cdg_tdoc)) .OR. (curcvta.cdg_tpg>="003" .AND.  .NOT. EMPTY(curcvta.num_ped))
       nctovta = nctovta+IIF(curcvta.fec_guia>=ddesde .AND. curcvta.fec_guia<=dhasta, m.tot_sal, 0)
    ENDIF
    SELECT curcvta
    SKIP IN curcvta
 ENDDO
 IF USED("curCvta")
    USE IN curcvta
 ENDIF
 RETURN (nctovta)
ENDFUNC
**
FUNCTION Ultimo_Precio
 PARAMETER ccli, cprod, cmon
 PRIVATE nultpre, ncambio
 nultpre = 0
 ncambio = 1
 SELECT d_doccli.cdg_tdoc, d_doccli.num_docu, m_doccli.fec_docu, d_doccli.pre_dfac, d_doccli.cdg_prod, m_doccli.ruc_cli, m_doccli.cdg_mon, t_cambio.tip_cmb FROM Sico!d_doccli, Sico!m_doccli, sico!t_cambio WHERE  NOT DELETED() AND (m_doccli.cdg_tdoc="001" OR m_doccli.cdg_tdoc="002") AND m_doccli.cdg_tdoc=d_doccli.cdg_tdoc AND m_doccli.num_docu=d_doccli.num_docu AND DTOS(m_doccli.fec_docu)=DTOS(t_cambio.fec_cmb) AND m_doccli.swt_est<>"A" AND m_doccli.ruc_cli=ccli AND d_doccli.cdg_prod=cprod ORDER BY m_doccli.fec_docu INTO CURSOR curPrecio
 SELECT curprecio
 GOTO TOP
 DO WHILE  .NOT. EOF("curPrecio")
    ncambio = IIF(curprecio.tip_cmb<>0, curprecio.tip_cmb, 1)
    IF curprecio.cdg_mon<>cmon
       DO CASE
          CASE curprecio.cdg_mon="001" .AND. cmon>"001"
             nultpre = ROUND(curprecio.pre_dfac/ncambio, 2)
          CASE curprecio.cdg_mon>"001" .AND. cmon="001"
             nultpre = ROUND(curprecio.pre_dfac*ncambio, 2)
       ENDCASE
    ELSE
       nultpre = curprecio.pre_dfac
    ENDIF
    SELECT curprecio
    SKIP IN curprecio
 ENDDO
 IF USED("curPrecio")
    USE IN curprecio
 ENDIF
 RETURN (nultpre)
ENDFUNC
**
FUNCTION CorAct
 PARAMETER ctipo
 PRIVATE ccod, nlen, cdel
 cdel = SET("DELETED")
 SET DELETED ON
 SELECT m_actfij.cdg_act, tipact.abr_item AS tipo FROM sico!m_actfij, sico!d_tablas tipact WHERE tipact.cdg_tab="TAC" AND m_actfij.cdg_tact=ctipo AND m_actfij.cdg_tact=tipact.num_item AND  NOT DELETED() ORDER BY cdg_act INTO CURSOR curA
 nlen = 10-LEN(ALLTRIM(cura.tipo))
 GOTO BOTTOM
 ccod = ALLTRIM(cura.tipo)+cerosizq(STR(VAL(RIGHT(cura.cdg_act, 5))+1, nlen))
 USE IN cura
 SET DELETED &cdel
 RETURN (ccod)
ENDFUNC
**
PROCEDURE Boleta_Empleado
 SET DEVICE TO PRINTER
 cprintini = CHR(27)+CHR(64)+CHR(27)+CHR(67)+CHR(32)+CHR(27)+CHR(15)+CHR(27)+CHR(77)
 ??? cprintini
 ncpact = 1252
 ncpnew = 850
 =opendbf("&gDirDbf\sico", "m_remfij.dbf", "emp_conc")
 =opendbf("&gDirDbf\sico", "t_planil.dbf", "")
 GOTO TOP
 DO WHILE  .NOT. EOF("t_planil")
    ccdgemp = t_planil.cdg_emp
    ningemp = 0
    ndctemp = 0
    napoemp = 0
    nneto = 0
    nbasico = IIF(SEEK(ccdgemp+"BASIC", "m_remfij"), m_remfij.imp_conc, 0)
    @ 01, 01 SAY CPCONVERT(ncpact, ncpnew, gnomemp)
    @ 01, 130 SAY CPCONVERT(ncpact, ncpnew, "R.U.C. "+grucemp)
    @ 02, 01 SAY CPCONVERT(ncpact, ncpnew, gdiremp)
    @ 02, 130 SAY CPCONVERT(ncpact, ncpnew, "R.P. "+gpatemp)
    @ 04, 01 SAY " CODIGO: "+CPCONVERT(ncpact, ncpnew, t_planil.cdg_emp)
    @ 04, 110 SAY " FECHA ING.: "+DTOC(t_planil.ing_emp)
    @ 05, 01 SAY " NOMBRE: "+CPCONVERT(ncpact, ncpnew, t_planil.des_emp)
    @ 05, 110 SAY "CARNET IPSS: "+CPCONVERT(ncpact, ncpnew, t_planil.ips_emp)
    @ 06, 01 SAY "  CARGO: "+CPCONVERT(ncpact, ncpnew, t_planil.des_cgo)
    @ 06, 110 SAY "     A.F.P.: "+IIF(ALLTRIM(t_planil.des_afp)="NO DEFINIDO", "", CPCONVERT(ncpact, ncpnew, ALLTRIM(t_planil.des_afp)))
    @ 07, 01 SAY "PERIODO: "+cperiodo+"-"+cano
    @ 07, 110 SAY "COD. A.F.P.: "+CPCONVERT(ncpact, ncpnew, t_planil.afp_emp)
    @ 08, 110 SAY "     BASICO: "+ALLTRIM(TRANSFORM(nbasico, "999 999 999.99"))+" NUEVOS SOLES"
    @ 09, 01 SAY REPLICATE(CHR(196), 159)
    @ 10, 01 SAY "------------------ INGRESOS ------------------"
    @ 10, 57 SAY "----------- RETENCIONES Y DESCUENTOS ------------"
    @ 10, 111 SAY "------------- APORTES DEL EMPLEADOR -------------"
    @ 11, 01 SAY REPLICATE(CHR(196), 159)
    nfila = 12
    DO WHILE t_planil.cdg_emp=ccdgemp
       IF nfila>=24
          EXIT
       ENDIF
       ningemp = ningemp+t_planil.imp_pln1
       ndctemp = ndctemp+t_planil.imp_pln2
       napoemp = napoemp+t_planil.imp_pln3
       IF  .NOT. EMPTY(t_planil.cdg_cnc1)
          @ nfila, 01 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_cnc1)
          IF  .NOT. EMPTY(t_planil.cdg_aso1) .AND. t_planil.cdg_aso1<>"NODEF"
             @ nfila, 15 SAY TRANSFORM(t_planil.imp_aso1, "@Z 999 999.99")
             @ nfila, 25 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_aso1)
          ENDIF
          @ nfila, 33 SAY TRANSFORM(t_planil.imp_pln1, "999 999 999.99")
       ENDIF
       IF  .NOT. EMPTY(t_planil.cdg_cnc2)
          @ nfila, 57 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_cnc2)
          IF  .NOT. EMPTY(t_planil.cdg_aso2) .AND. t_planil.cdg_aso2<>"NODEF"
             @ nfila, 74 SAY TRANSFORM(t_planil.imp_aso2, "@Z 999 999.99")
             @ nfila, 84 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_aso2)
          ENDIF
          @ nfila, 92 SAY TRANSFORM(t_planil.imp_pln2, "999 999 999.99")
       ENDIF
       IF  .NOT. EMPTY(t_planil.cdg_cnc3)
          @ nfila, 111 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_cnc3)
          IF  .NOT. EMPTY(t_planil.cdg_aso3) .AND. t_planil.cdg_aso3<>"NODEF"
             @ nfila, 129 SAY TRANSFORM(t_planil.imp_aso3, "@Z 999 999.99")
             @ nfila, 139 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_aso3)
          ENDIF
          @ nfila, 146 SAY TRANSFORM(t_planil.imp_pln3, "999 999 999.99")
       ENDIF
       nfila = nfila+1
       SELECT t_planil
       SKIP IN t_planil
    ENDDO
    @ 24, 16 SAY "TOTAL INGRESOS"
    @ 24, 33 SAY TRANSFORM(ningemp, "999 999 999.99")
    @ 24, 75 SAY "TOTAL EGRESOS"
    @ 24, 92 SAY TRANSFORM(ndctemp, "999 999 999.99")
    @ 24, 129 SAY "TOTAL APORTES"
    @ 24, 146 SAY TRANSFORM(napoemp, "999 999 999.99")
    nneto = ningemp-ndctemp
    @ 26, 129 SAY "NETO A PAGAR S/."
    @ 26, 146 SAY TRANSFORM(nneto, "999 999 999.99")
    @ 27, 01 SAY REPLICATE("_", 30)
    @ 28, 08 SAY "RECIBI CONFORME"
 ENDDO
 USE IN t_planil
 @ 29, 00 SAY " "
 EJECT
 SET PRINTER TO
 SET DEVICE TO SCREEN
 RETURN
ENDPROC
**
PROCEDURE Boleta_Obrero
 SET DEVICE TO PRINTER
 cprintini = CHR(27)+CHR(64)+CHR(27)+CHR(67)+CHR(32)+CHR(27)+CHR(15)+CHR(27)+CHR(77)
 ??? cprintini
 ncpact = 1252
 ncpnew = 850
 =opendbf("&gDirDbf\sico", "m_remfij.dbf", "emp_conc")
 =opendbf("&gDirDbf\sico", "t_planil.dbf", "")
 GOTO TOP
 DO WHILE  .NOT. EOF("t_planil")
    ccdgemp = t_planil.cdg_emp
    ningemp = 0
    ndctemp = 0
    napoemp = 0
    nneto = 0
    nbasico = IIF(SEEK(ccdgemp+"BASIC", "m_remfij"), m_remfij.imp_conc, 0)
    @ 01, 01 SAY CPCONVERT(ncpact, ncpnew, gnomemp)
    @ 01, 130 SAY CPCONVERT(ncpact, ncpnew, "R.U.C. "+grucemp)
    @ 02, 01 SAY CPCONVERT(ncpact, ncpnew, gdiremp)
    @ 02, 130 SAY CPCONVERT(ncpact, ncpnew, "R.P. "+gpatemp)
    @ 04, 01 SAY " CODIGO: "+CPCONVERT(ncpact, ncpnew, t_planil.cdg_emp)
    @ 04, 110 SAY " FECHA ING.: "+DTOC(t_planil.ing_emp)
    @ 05, 01 SAY " NOMBRE: "+CPCONVERT(ncpact, ncpnew, t_planil.des_emp)
    @ 05, 110 SAY "CARNET IPSS: "+CPCONVERT(ncpact, ncpnew, t_planil.ips_emp)
    @ 06, 01 SAY "  CARGO: "+CPCONVERT(ncpact, ncpnew, t_planil.des_cgo)
    @ 06, 110 SAY "     A.F.P.: "+IIF(ALLTRIM(t_planil.des_afp)="NO DEFINIDO", "", CPCONVERT(ncpact, ncpnew, ALLTRIM(t_planil.des_afp)))
    @ 07, 01 SAY "PERIODO: "+cperiodo+"-"+cano
    @ 07, 110 SAY "COD. A.F.P.: "+CPCONVERT(ncpact, ncpnew, t_planil.afp_emp)
    @ 08, 110 SAY "     BASICO: "+ALLTRIM(TRANSFORM(nbasico, "999 999 999.99"))+" NUEVOS SOLES"
    @ 09, 01 SAY REPLICATE(CHR(196), 159)
    @ 10, 01 SAY "------------------ INGRESOS ------------------"
    @ 10, 57 SAY "----------- RETENCIONES Y DESCUENTOS ------------"
    @ 10, 111 SAY "------------- APORTES DEL EMPLEADOR -------------"
    @ 11, 01 SAY REPLICATE(CHR(196), 159)
    nfila = 12
    DO WHILE t_planil.cdg_emp=ccdgemp
       IF nfila>=24
          EXIT
       ENDIF
       ningemp = ningemp+t_planil.imp_pln1
       ndctemp = ndctemp+t_planil.imp_pln2
       napoemp = napoemp+t_planil.imp_pln3
       IF  .NOT. EMPTY(t_planil.cdg_cnc1)
          @ nfila, 01 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_cnc1)
          IF  .NOT. EMPTY(t_planil.cdg_aso1) .AND. t_planil.cdg_aso1<>"NODEF"
             @ nfila, 15 SAY TRANSFORM(t_planil.imp_aso1, "@Z 999 999.99")
             @ nfila, 25 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_aso1)
          ENDIF
          @ nfila, 33 SAY TRANSFORM(t_planil.imp_pln1, "999 999 999.99")
       ENDIF
       IF  .NOT. EMPTY(t_planil.cdg_cnc2)
          @ nfila, 57 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_cnc2)
          IF  .NOT. EMPTY(t_planil.cdg_aso2) .AND. t_planil.cdg_aso2<>"NODEF"
             @ nfila, 74 SAY TRANSFORM(t_planil.imp_aso2, "@Z 999 999.99")
             @ nfila, 84 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_aso2)
          ENDIF
          @ nfila, 92 SAY TRANSFORM(t_planil.imp_pln2, "999 999 999.99")
       ENDIF
       IF  .NOT. EMPTY(t_planil.cdg_cnc3)
          @ nfila, 111 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_cnc3)
          IF  .NOT. EMPTY(t_planil.cdg_aso3) .AND. t_planil.cdg_aso3<>"NODEF"
             @ nfila, 129 SAY TRANSFORM(t_planil.imp_aso3, "@Z 999 999.99")
             @ nfila, 139 SAY CPCONVERT(ncpact, ncpnew, t_planil.abr_aso3)
          ENDIF
          @ nfila, 146 SAY TRANSFORM(t_planil.imp_pln3, "999 999 999.99")
       ENDIF
       nfila = nfila+1
       SELECT t_planil
       SKIP IN t_planil
    ENDDO
    @ 24, 16 SAY "TOTAL INGRESOS"
    @ 24, 33 SAY TRANSFORM(ningemp, "999 999 999.99")
    @ 24, 75 SAY "TOTAL EGRESOS"
    @ 24, 92 SAY TRANSFORM(ndctemp, "999 999 999.99")
    @ 24, 129 SAY "TOTAL APORTES"
    @ 24, 146 SAY TRANSFORM(napoemp, "999 999 999.99")
    nneto = ningemp-ndctemp
    @ 26, 129 SAY "NETO A PAGAR S/."
    @ 26, 146 SAY TRANSFORM(nneto, "999 999 999.99")
    @ 27, 01 SAY REPLICATE("_", 30)
    @ 28, 08 SAY "RECIBI CONFORME"
 ENDDO
 USE IN t_planil
 @ 29, 00 SAY " "
 EJECT
 SET PRINTER TO
 SET DEVICE TO SCREEN
 RETURN
ENDPROC
**
FUNCTION NumReg
 PARAMETER cano, cmes
 PRIVATE ccod, nlen, cdel, cnumreg
 cdel = SET("DELETED")
 nlen = 6
 cnumreg = ""
 SET DELETED ON
 =opendbf("&gDirDbf\sico", "m_docprv.dbf", "num_reg")
 IF SEEK(cano+cmes, "m_docprv")
    DO WHILE m_docprv.ano_reg+m_docprv.mes_reg=cano+cmes
       cnumreg = m_docprv.num_reg
       SKIP IN m_docprv
    ENDDO
 ENDIF
 ccod = cerosizq(STR(VAL(cnumreg)+1, nlen))
 SET DELETED &cdel
 RETURN (ccod)
ENDFUNC
**
FUNCTION ItemCor
 PARAMETER copc, citem
 PRIVATE ccod, nlen
 IF SET('DELETED')='OFF'
    SET DELETED ON
 ENDIF
 SELECT num_cor FROM sico!t_opcion WHERE cdg_opc=copc AND num_item=citem ORDER BY num_cor INTO CURSOR curCor
 nlen = 3
 GOTO BOTTOM
 IF EOF()
    ccod = cerosizq(STR(1, nlen))
 ELSE
    ccod = cerosizq(STR(VAL(curcor.num_cor)+1, nlen))
 ENDIF
 USE IN curcor
 RETURN (ccod)
ENDFUNC
**
FUNCTION CorLiq
 PARAMETER cnumliq
 PRIVATE ccod, nlen, cdel
 cdel = SET("DELETED")
 SET DELETED ON
 SELECT num_cor FROM sico!d_liqdoc WHERE d_liqdoc.num_liq=cnumliq AND LEFT(d_liqdoc.num_cor, 1)<>"A" AND  NOT DELETED() ORDER BY num_cor INTO CURSOR curC
 nlen = 5
 GOTO BOTTOM
 ccod = cerosizq(STR(VAL(curc.num_cor)+1, nlen))
 USE IN curc
 SET DELETED &cdel
 RETURN (ccod)
ENDFUNC
**
FUNCTION Ultimo_Costo
 PARAMETER cprod, cmon
 PRIVATE nultcto, ncambio
 nultcto = 0
 ncambio = 1
 SELECT d_ordcom.num_ocom, d_ordcom.pre_oins, d_ordcom.dct_oins, m_ordcom.fec_ocom, m_ordcom.cdg_mon, t_cambio.tip_cmb FROM Sico!d_ordcom, Sico!m_ordcom, sico!t_cambio WHERE m_ordcom.num_ocom=d_ordcom.num_ocom AND d_ordcom.cdg_prod=cprod AND DTOS(m_ordcom.fec_ocom)=DTOS(t_cambio.fec_cmb) AND m_ordcom.swt_est<>"A" AND d_ordcom.can_ialm>0 ORDER BY d_ordcom.num_ocom INTO CURSOR curUcto
 SELECT curucto
 GOTO TOP
 DO WHILE  .NOT. EOF("curUcto")
    ncambio = IIF(curucto.tip_cmb<>0, curucto.tip_cmb, 1)
    IF curucto.cdg_mon<>cmon
       DO CASE
          CASE curucto.cdg_mon="001" .AND. cmon<>"001"
             nultcto = ROUND(curucto.pre_oins*(1-(curucto.dct_oins/100))/ncambio, 4)
          CASE curucto.cdg_mon<>"001" .AND. cmon="001"
             nultcto = ROUND(curucto.pre_oins*(1-(curucto.dct_oins/100))*ncambio, 4)
       ENDCASE
    ELSE
       nultcto = ROUND(curucto.pre_oins*(1-(curucto.dct_oins/100)), 4)
    ENDIF
    SELECT curucto
    SKIP IN curucto
 ENDDO
 IF USED("curUcto")
    USE IN curucto
 ENDIF
 RETURN (nultcto)
ENDFUNC
**
PROCEDURE Errores
 PARAMETER merror, mess, mess1, mprog, mlineno
 gerror = ""
 IF merror=1569 .OR. merror=1705
    gerror = "Acceso Denegado"
 ENDIF
 RETURN
ENDPROC
**
PROCEDURE Nota_Cargo
 PARAMETER nrodocum, ntipo
 IF PARAMETERS()=1
    ntipo = "004"
 ENDIF
 =opendbf("&gDirDbf\sico", "m_doccli.dbf", "doc_cli")
 dfecdocu = CTOD("  /  /    ")
 cruccli = ""
 cdescli = ""
 cdireccion = ""
 crefotro = ""
 cmon = ""
 cobsdocu = ""
 nportigv = 0
 nsubtotal = 0
 ntotigv = 0
 ntotal = 0
 ncpact = 1252
 ncpnew = 850
 IF SEEK(ntipo+nrodocum, "m_doccli")
    ncpact = CPDBF()
    dfecdocu = m_doccli.fec_docu
    cruccli = m_doccli.ruc_cli
    cdescli = ALLTRIM(m_doccli.des_cli)
    cdireccion = ALLTRIM(m_doccli.direccion)
    crefotro = ALLTRIM(m_doccli.ref_otro)
    cobsdocu = ALLTRIM(m_doccli.obs_fcli)
    nportigv = m_doccli.por_tigv
    nsubtotal = m_doccli.val_fsub
    ntotigv = m_doccli.val_figv
    ntotal = m_doccli.val_ftot
    cmon = m_doccli.cdg_mon
 ENDIF
 =opendbf("&gDirDbf\sico", "m_client.dbf", "ruc_cli")
 ccdgudis = ""
 nswtalt = 0
 ccdgalt = ""
 IF SEEK(cruccli, "m_client")
    ccdgudis = m_client.cdg_udis
    nswtalt = m_client.swt_alt
    ccdgalt = ALLTRIM(m_client.cdg_alt)
 ENDIF
 cserdocu = ""
 =opendbf("&gDirDbf\sico", "t_doccli.dbf", "cdg_tdoc")
 IF SEEK(ntipo, "t_doccli")
    cserdocu = ALLTRIM(t_doccli.num_ser)
 ENDIF
 =opendbf("&gDirDbf\sico", "d_tablas.dbf", "tab_item")
 cdesudis = IIF(SEEK("DST"+ccdgudis, "d_tablas"), ALLTRIM(d_tablas.des_item), "")
 cmoneda = IIF(cmon="002", "US$", "S/.")
 cdircli = cdireccion+IIF(ccdgudis<>"000", " - "+cdesudis, "")
 cletras = numletnew(ntotal)+" Y "+STR(100*(ntotal-INT(ntotal)), 2)+'/100'+IIF(cmon="002", " DOLARES AMERICANOS", " NUEVOS SOLES")
 SET DEVICE TO PRINTER
 cprintini = CHR(27)+CHR(64)+CHR(27)+CHR(18)+CHR(27)+CHR(15)
 ??? cprintini
 SET MARGIN TO 4
 SET MEMOWIDTH TO 78
 @ 10, 98 SAY cserdocu+"-"+nrodocum
 @ 12, 17 SAY CPCONVERT(ncpact, ncpnew, LEFT(cdescli, 60))
 @ 13, 17 SAY CPCONVERT(ncpact, ncpnew, LEFT(cdircli, 60))
 @ 15, 17 SAY CPCONVERT(ncpact, ncpnew, IIF(nswtalt=1, ccdgalt, cruccli))
 @ 15, 49 SAY DTOC(dfecdocu)
 nlin = 20
 =opendbf("&gDirDbf\sico", "m_produc.dbf", "cdg_prod")
 =opendbf("&gDirDbf\sico", "d_doccli.dbf", "doc_prod")
 IF SEEK(ntipo+nrodocum, "d_doccli")
    DO WHILE d_doccli.cdg_tdoc+d_doccli.num_docu=ntipo+nrodocum
       ccdgprod = d_doccli.cdg_prod
       cdesprod = IIF(SEEK(ccdgprod, "m_produc"), ALLTRIM(m_produc.des_prod), "")
       ccdgprv = IIF(SEEK(ccdgprod, "m_produc"), ALLTRIM(m_produc.cdg_prv), "")
       nprecio = ROUND(d_doccli.imp_dfac/d_doccli.can_dfac, 2)
       @ nlin, 05 SAY CPCONVERT(ncpact, ncpnew, LEFT(cdesprod, 45))
       @ nlin, 52 SAY CPCONVERT(ncpact, ncpnew, LEFT(ccdgprv, 30))
       @ nlin, 84 SAY TRANSFORM(d_doccli.can_dfac, "9999 999.99")
       @ nlin, 97 SAY TRANSFORM(nprecio, "999999 999.99")
       @ nlin, 117 SAY cmoneda
       @ nlin, 120 SAY TRANSFORM(d_doccli.imp_dfac, "9999 999.99")
       nlin = nlin+1
       IF LEN(ALLTRIM(d_doccli.obs_dfac))<>0
          FOR ni = 1 TO MEMLINES(d_doccli.obs_dfac)
             @ nlin, 05 SAY CPCONVERT(ncpact, ncpnew, MLINE(d_doccli.obs_dfac, ni))
             nlin = nlin+1
          ENDFOR
       ENDIF
       SKIP IN d_doccli
    ENDDO
 ENDIF
 @ 46, 03 SAY CPCONVERT(ncpact, ncpnew, MLINE(cobsdocu, 1))
 @ 46, 107 SAY "SUBTOTAL"
 @ 46, 117 SAY cmoneda
 @ 46, 120 SAY TRANSFORM(nsubtotal, "9999 999.99")
 @ 47, 03 SAY CPCONVERT(ncpact, ncpnew, MLINE(cobsdocu, 2))
 @ 47, 104 SAY TRANSFORM(nportigv, "999")+"%"
 @ 47, 109 SAY "I.G.V."
 @ 47, 117 SAY cmoneda
 @ 47, 120 SAY TRANSFORM(ntotigv, "9999 999.99")
 @ 48, 03 SAY CPCONVERT(ncpact, ncpnew, MLINE(cobsdocu, 3))
 @ 48, 110 SAY "TOTAL"
 @ 48, 117 SAY cmoneda
 @ 48, 120 SAY TRANSFORM(ntotal, "9999 999.99")
 @ 49, 03 SAY "SON: "+cletras
 @ 50, 01 SAY " "
 @ 50, 05 SAY CHR(27)+CHR(18)
 SET MARGIN TO 0
 EJECT
 SET PRINTER TO
 SET DEVICE TO SCREEN
 RETURN
ENDPROC
**
PROCEDURE Nota_Abono
 PARAMETER nrodocum, ntipo
 IF PARAMETERS()=1
    ntipo = "005"
 ENDIF
 =opendbf("&gDirDbf\sico", "m_doccli.dbf", "doc_cli")
 dfecdocu = CTOD("  /  /    ")
 cruccli = ""
 cdescli = ""
 cdireccion = ""
 crefotro = ""
 cmon = ""
 cobsdocu = ""
 ntipcmb = 0
 nportigv = 0
 nsubtotal = 0
 ntotigv = 0
 ntotal = 0
 ncpact = 1252
 ncpnew = 850
 IF SEEK(ntipo+nrodocum, "m_doccli")
    ncpact = CPDBF()
    dfecdocu = m_doccli.fec_docu
    cruccli = m_doccli.ruc_cli
    cdescli = ALLTRIM(m_doccli.des_cli)
    cdireccion = ALLTRIM(m_doccli.direccion)
    crefotro = ALLTRIM(m_doccli.ref_otro)
    cobsdocu = ALLTRIM(m_doccli.obs_fcli)
    nportigv = m_doccli.por_tigv
    nsubtotal = m_doccli.val_fsub
    ntotigv = m_doccli.val_figv
    ntotal = m_doccli.val_ftot
    cmon = m_doccli.cdg_mon
    ntipcmb = m_doccli.cmb_docu
 ENDIF
 =opendbf("&gDirDbf\sico", "m_client.dbf", "ruc_cli")
 ccdgudis = ""
 nswtalt = 0
 ccdgalt = ""
 IF SEEK(cruccli, "m_client")
    ccdgudis = m_client.cdg_udis
    nswtalt = m_client.swt_alt
    ccdgalt = ALLTRIM(m_client.cdg_alt)
 ENDIF
 cserdocu = ""
 =opendbf("&gDirDbf\sico", "t_doccli.dbf", "cdg_tdoc")
 IF SEEK(ntipo, "t_doccli")
    cserdocu = ALLTRIM(t_doccli.num_ser)
 ENDIF
 =opendbf("&gDirDbf\sico", "d_tablas.dbf", "tab_item")
 cdesudis = IIF(SEEK("DST"+ccdgudis, "d_tablas"), ALLTRIM(d_tablas.des_item), "")
 cmoneda = IIF(cmon="002", "US$", "S/.")
 cdircli = cdireccion+IIF(ccdgudis<>"000", " - "+cdesudis, "")
 cletras = numletnew(ntotal)+" Y "+STR(100*(ntotal-INT(ntotal)), 2)+'/100'+IIF(cmon="002", " DOLARES AMERICANOS", " NUEVOS SOLES")
 SET DEVICE TO PRINTER
 cprintini = CHR(27)+CHR(64)+CHR(27)+CHR(18)+CHR(27)+CHR(15)
 ??? cprintini
 SET MARGIN TO 4
 SET MEMOWIDTH TO 78
 @ 10, 98 SAY cserdocu+"-"+nrodocum
 @ 12, 17 SAY CPCONVERT(ncpact, ncpnew, LEFT(cdescli, 60))
 @ 13, 17 SAY CPCONVERT(ncpact, ncpnew, LEFT(cdircli, 60))
 @ 15, 17 SAY CPCONVERT(ncpact, ncpnew, IIF(nswtalt=1, ccdgalt, cruccli))
 @ 15, 49 SAY DTOC(dfecdocu)
 @ 15, 70 SAY CPCONVERT(ncpact, ncpnew, "T.C. "+ALLTRIM(TRANSFORM(ntipcmb, "999.999")))
 nlin = 20
 =opendbf("&gDirDbf\sico", "m_produc.dbf", "cdg_prod")
 =opendbf("&gDirDbf\sico", "d_doccli.dbf", "doc_prod")
 IF SEEK(ntipo+nrodocum, "d_doccli")
    DO WHILE d_doccli.cdg_tdoc+d_doccli.num_docu=ntipo+nrodocum
       ccdgprod = d_doccli.cdg_prod
       cdesprod = IIF(SEEK(ccdgprod, "m_produc"), ALLTRIM(m_produc.des_prod), "")
       ccdgprv = IIF(SEEK(ccdgprod, "m_produc"), ALLTRIM(m_produc.cdg_prv), "")
       nprecio = ROUND(d_doccli.imp_dfac/d_doccli.can_dfac, 2)
       @ nlin, 05 SAY CPCONVERT(ncpact, ncpnew, LEFT(cdesprod, 45))
       @ nlin, 52 SAY CPCONVERT(ncpact, ncpnew, LEFT(ccdgprv, 30))
       @ nlin, 84 SAY TRANSFORM(d_doccli.can_dfac, "9999 999.99")
       @ nlin, 97 SAY TRANSFORM(nprecio, "999999 999.99")
       @ nlin, 117 SAY cmoneda
       @ nlin, 120 SAY TRANSFORM(d_doccli.imp_dfac, "9999 999.99")
       nlin = nlin+1
       IF LEN(ALLTRIM(d_doccli.obs_dfac))<>0
          FOR ni = 1 TO MEMLINES(d_doccli.obs_dfac)
             @ nlin, 05 SAY CPCONVERT(ncpact, ncpnew, MLINE(d_doccli.obs_dfac, ni))
             nlin = nlin+1
          ENDFOR
       ENDIF
       SKIP IN d_doccli
    ENDDO
 ENDIF
 @ 45, 03 SAY CPCONVERT(ncpact, ncpnew, IIF( .NOT. EMPTY(crefotro), "REFERENCIA: "+crefotro, ""))
 @ 46, 03 SAY CPCONVERT(ncpact, ncpnew, MLINE(cobsdocu, 1))
 @ 46, 107 SAY "SUBTOTAL"
 @ 46, 117 SAY cmoneda
 @ 46, 120 SAY TRANSFORM(nsubtotal, "9999 999.99")
 @ 47, 03 SAY CPCONVERT(ncpact, ncpnew, MLINE(cobsdocu, 2))
 @ 47, 104 SAY TRANSFORM(nportigv, "999")+"%"
 @ 47, 109 SAY "I.G.V."
 @ 47, 117 SAY cmoneda
 @ 47, 120 SAY TRANSFORM(ntotigv, "9999 999.99")
 @ 48, 03 SAY CPCONVERT(ncpact, ncpnew, MLINE(cobsdocu, 3))
 @ 48, 110 SAY "TOTAL"
 @ 48, 117 SAY cmoneda
 @ 48, 120 SAY TRANSFORM(ntotal, "9999 999.99")
 @ 49, 03 SAY "SON: "+cletras
 @ 50, 01 SAY " "
 @ 50, 05 SAY CHR(27)+CHR(18)
 SET MARGIN TO 0
 EJECT
 SET PRINTER TO
 SET DEVICE TO SCREEN
 RETURN
ENDPROC
**
PROCEDURE RepForm
 PARAMETER ccdgform, ccdgtdoc, cnumdocu, cemi
 IF EMPTY(ccdgform)
    WAIT WINDOW "No puedo hacerlo"
    RETURN
 ENDIF
 IF EMPTY(cemi)
 ENDIF
 =opendbf("&gDirDbf\sico", "m_frmimp.dbf", "cdg_form")
 IF SEEK(ccdgform, "m_frmimp")
    cimpform = m_frmimp.imp_form
    ctipform = m_frmimp.tip_form
    nmaxlin = m_frmimp.lin_form
    ctipimpr = ALLTRIM(m_frmimp.tip_impr)
 ENDIF
 =opendbf("&gDirDbf\sico", "d_tablas.dbf", "tab_item")
 IF SEEK("IMP"+cimpform, "d_tablas")
    cchar1 = ALLTRIM(d_tablas.cam_item)
    cchar2 = ALLTRIM(d_tablas.anu_item)
 ENDIF
 IF ctipform=1
    cprintini = CHR(27)+CHR(64)+IIF( .NOT. EMPTY(cchar1), CHR(27)+CHR(VAL(cchar1)), "")+IIF( .NOT. EMPTY(cchar2), CHR(27)+CHR(VAL(cchar2)), "")
 ELSE
    cprintini = CHR(27)+CHR(64)+CHR(27)+CHR(67)+CHR(nmaxlin)+IIF( .NOT. EMPTY(cchar1), CHR(27)+CHR(VAL(cchar1)), "")+IIF( .NOT. EMPTY(cchar2), CHR(27)+CHR(VAL(cchar2)), "")
 ENDIF
 IF  .NOT. EMPTY(ctipimpr)
    SET PRINTER TO NAME "&cTipImpr"
 ENDIF
 SET DEVICE TO PRINTER
 ??? cprintini
 ncpact = 1252
 ncpnew = 850
 =opendbf("&gDirDbf\sico", "d_frmimp.dbf", "form_sec")
 SELECT d_frmimp
 GOTO TOP
 IF SEEK(ccdgform, "d_frmimp")
    DO WHILE d_frmimp.cdg_form=ccdgform
       IF EMPTY(d_frmimp.swt_var)
          SKIP IN d_frmimp
          LOOP
       ENDIF
       IF d_frmimp.tip_var<>"001"
          SKIP IN d_frmimp
          LOOP
       ENDIF
       ctabla = ALLTRIM(d_frmimp.tab_var)
       cindice = ALLTRIM(d_frmimp.tag_var)
       cvariable = ALLTRIM(d_frmimp.nom_var)
       ccondicion = ALLTRIM(d_frmimp.cnd_var)
       nfila = VAL(d_frmimp.fil_var)
       ncolumna = VAL(d_frmimp.col_var)
       cformato = ALLTRIM(d_frmimp.for_var)
       nswtimp = d_frmimp.swt_imp
       IF  .NOT. EMPTY(ctabla)
          =opendbf("&gDirDbf\sico", "&cTabla..dbf", "&cIndice")
       ENDIF
       &cvariable = &ccondicion
       IF nswtimp=1
          @ nfila, ncolumna SAY &cformato
       ENDIF
       SELECT d_frmimp
       SKIP IN d_frmimp
    ENDDO
 ENDIF
 SELECT m_frmimp
 GOTO TOP
 IF SEEK(ccdgform, "m_frmimp")
    DO WHILE m_frmimp.cdg_form=ccdgform
       ctabdet = ALLTRIM(m_frmimp.nom_tab)
       cinddet = ALLTRIM(m_frmimp.nom_tag)
       ccondet = ALLTRIM(m_frmimp.cnd_form)
       nfiladet = m_frmimp.ini_form
       nmax = m_frmimp.max_form
       =opendbf("&gDirDbf\sico", "&cTabDet..dbf", "&cIndDet")
       IF SEEK(ccdgtdoc + cnumdocu, "&cTabDet")
          nfila = nfiladet
          DO WHILE &cConDet
             SELECT d_frmimp
             GOTO TOP
             IF SEEK(ccdgform, "d_frmimp")
                DO WHILE d_frmimp.cdg_form=ccdgform
                   IF EMPTY(d_frmimp.swt_var)
                      SKIP IN d_frmimp
                      LOOP
                   ENDIF
                   IF d_frmimp.tip_var<>"002"
                      SKIP IN d_frmimp
                      LOOP
                   ENDIF
                   ctabla = ALLTRIM(d_frmimp.tab_var)
                   cindice = ALLTRIM(d_frmimp.tag_var)
                   cvariable = ALLTRIM(d_frmimp.nom_var)
                   ccondicion = ALLTRIM(d_frmimp.cnd_var)
                   ncolumna = VAL(d_frmimp.col_var)
                   cformato = ALLTRIM(d_frmimp.for_var)
                   nswtimp = d_frmimp.swt_imp
                   nswtmemo = d_frmimp.swt_memo
                   nwithmemo = d_frmimp.len_memo
                   IF  .NOT. EMPTY(ctabla)
                      =opendbf("&gDirDbf\sico", "&cTabla..dbf", "&cIndice")
                   ENDIF
                   &cvariable = &ccondicion
                   IF EMPTY(nswtmemo)
                      IF nswtimp=1
                         @ nfila, ncolumna SAY &cformato
                      ENDIF
                   ELSE
                      IF LEN(&cvariable) <> 0
                         SET MEMOWIDTH TO nwithmemo
                         FOR ni = 1 TO MEMLINES(&cvariable)
                            nfila = nfila+1
                            IF nfila>nmax
                               EXIT
                            ENDIF
                            @ nfila, ncolumna SAY MLINE(&cformato, ni)
                         ENDFOR
                      ENDIF
                   ENDIF
                   SELECT d_frmimp
                   SKIP IN d_frmimp
                ENDDO
             ENDIF
             nfila = nfila+1
             IF nfila>nmax
                EXIT
             ENDIF
             SELECT &ctabdet
             SKIP IN &ctabdet
          ENDDO
       ENDIF
       SELECT m_frmimp
       SKIP IN m_frmimp
    ENDDO
 ENDIF
 SELECT d_frmimp
 GOTO TOP
 nfila = nfila-1
 IF SEEK(ccdgform, "d_frmimp")
    DO WHILE d_frmimp.cdg_form=ccdgform
       IF EMPTY(d_frmimp.swt_var)
          SKIP IN d_frmimp
          LOOP
       ENDIF
       IF d_frmimp.swt_imp<>1
          SKIP IN d_frmimp
          LOOP
       ENDIF
       IF d_frmimp.tip_var<>"003"
          SKIP IN d_frmimp
          LOOP
       ENDIF
       ctabla = ALLTRIM(d_frmimp.tab_var)
       cindice = ALLTRIM(d_frmimp.tag_var)
       cvariable = ALLTRIM(d_frmimp.nom_var)
       ccondicion = ALLTRIM(d_frmimp.cnd_var)
       nfila = IIF(VAL(d_frmimp.fil_var)<>0, VAL(d_frmimp.fil_var), nfila+1)
       ncolumna = VAL(d_frmimp.col_var)
       cformato = ALLTRIM(d_frmimp.for_var)
       nswtimp = d_frmimp.swt_imp
       IF  .NOT. EMPTY(ctabla)
          =opendbf("&gDirDbf\sico", "&cTabla..dbf", "&cIndice")
       ENDIF
       &cvariable = &ccondicion
       IF nswtimp=1
          @ nfila, ncolumna SAY &cformato
       ENDIF
       SELECT d_frmimp
       SKIP IN d_frmimp
    ENDDO
 ENDIF
 SET PRINTER TO DEFAULT
 SET DEVICE TO SCREEN
 IF USED("d_frmimp")
    USE IN d_frmimp
 ENDIF
 IF USED("m_frmimp")
    USE IN m_frmimp
 ENDIF
 RETURN
ENDPROC
**
PROCEDURE Frx2Xls
 PARAMETER tcfrxfullfilename, tcoutputfilename, tnoutputfiletype, tnexcelouputtype, tnoutput
 IF EMPTY(tcoutputfilename)
    RETURN
 ENDIF
 IF EMPTY(tnoutputfiletype)
    tnoutputfiletype = 4
 ENDIF
 IF EMPTY(tnexcelouputtype)
    tnexcelouputtype = 2
 ENDIF
 IF EMPTY(tnoutput)
    tnoutput = 1
 ENDIF
 LOCAL lofile
 LOCAL lcdefaultpath
 LOCAL lcreportfullname
 LOCAL lcoutputfilename
 LOCAL lcoutputfilepath
 LOCAL lcerrormessage
 LOCAL lcclassname
 LOCAL lcmodule
 LOCAL lcinapplication
 IF EMPTY(tcfrxfullfilename) .OR.  .NOT. VARTYPE(tcfrxfullfilename)$'C'
    lcerrormessage = 'Nombre del FRX requerido'
    = MESSAGEBOX(lcerrormessage, 16, 'Error')
    RETURN
 ENDIF
 IF  .NOT. (VARTYPE(tnoutputfiletype)$'NFIBY' .AND. INLIST(tnoutputfiletype, 1, 2, 3, 4, 5, 6, 7, 8))
    tnoutputfiletype = 6
 ENDIF
 IF  .NOT. (VARTYPE(tnscope)$'NFIBY' .AND. INLIST(tnscope, 1, 2, 3, 4))
    tnscope = 1
 ENDIF
 tnoutput = IIF(INLIST(tnoutput, 1, 2, 3, 4), tnoutput, 1)
 lcalias = 'FrxOrigen'
 USE SHARED (tcfrxfullfilename) AGAIN ALIAS (lcalias)
 lcreportfullname = gdirusu+'TempReport.FRX'
 SELECT (lcalias)
 COPY TO &lcreportfullname
 USE IN SELECT(lcalias)
 lcoutputfilename = tcoutputfilename
 lcoutputfilepath = gdirusu
 IF  .NOT. FILE(lcreportfullname)
    lcerrormessage = 'Archivo '+lcreportfullname+' no existe '
    = MESSAGEBOX(lcerrormessage, 16, 'Error')
 ELSE
    SET CLASSLIB TO
    SET CLASSLIB TO frx2any IN &gdirdbf\frx2any.APP ALIAS frx2any
    DO CASE
       CASE tnoutputfiletype=1
          lofile = CREATEOBJECT('FRX2Any.WordFile')
       CASE tnoutputfiletype=3
          lofile = CREATEOBJECT('FRX2Any.RTFFile')
       CASE tnoutputfiletype=2
          lofile = CREATEOBJECT('FRX2Any.HTMLFile')
       CASE tnoutputfiletype=4
          lofile = CREATEOBJECT('FRX2Any.EXCELFile')
       CASE tnoutputfiletype=6
          lofile = CREATEOBJECT('FRX2Any.PREVIEWFile')
       CASE tnoutputfiletype=7
          lofile = CREATEOBJECT('FRX2Any.PDFFile')
       CASE tnoutputfiletype=5
          lofile = CREATEOBJECT('FRX2Any.XMLFile')
       CASE tnoutputfiletype=8
          lofile = CREATEOBJECT('FRX2Any.IMAGEFile')
    ENDCASE
    IF TYPE('loFile')='O'
       lofile.unlock('100100-050308-000008-LINPAJ-KOIVGR-AKNIGN-TJMFER-OJFFFB')
       lofile.cexportfilename = lcoutputfilename
       lofile.csavefolder = lcoutputfilepath
       lcversion = lofile.getversion()
       DO CASE
          CASE tnoutputfiletype=4
             DO CASE
                CASE tnexcelouputtype=1
                   lofile.nexceloutputtype = 1
                CASE tnexcelouputtype=2
                   lofile.nexceloutputtype = 2
                CASE tnexcelouputtype=3
                   lofile.nexceloutputtype = 3
                   lofile.lmultiworksheet = .T.
                CASE tnexcelouputtype=4
                   lofile.nexceloutputtype = 4
                   lofile.lsuppressextrarow = .T.
                CASE tnexcelouputtype=5
                   lofile.nexceloutputtype = 5
             ENDCASE
       ENDCASE
       lofile.ldisplaystatus = .T.
       lofile.noutputtype = tnoutput
       lnsuccess = lofile.save(lcreportfullname)
       IF tnoutput=4
          IF lnsuccess<>0
             WAIT WINDOW 'No se genero el archivo EXCEL'
          ELSE
             WAIT WINDOW 'Se genero el archivo '+lcoutputfilename+'.XLS'+' en '+gdirusu
          ENDIF
       ENDIF
       lofile.release()
       lofile = .NULL.
       SET CLASSLIB TO
    ELSE
       lcerrormessage = 'No se puede inicializar el objeto Frx2Any.'
       = MESSAGEBOX(lcerrormessage, 16, 'Error')
    ENDIF
 ENDIF
 IF FILE('&lcReportFullName')
    DELETE FILE &lcreportfullname
 ENDIF
 RETURN
ENDPROC
**
FUNCTION Costo_Promedio
 PARAMETER cprod, carea, cmon, dhasta
 PRIVATE ncanant, npreant, ntotant, nctovta, ncambio, ncosto
 ncanant = 0
 npreant = 0
 ntotant = 0
 nctovta = 0
 ncosto = 0
 ncambio = 1
 SELECT d_guia.cdg_tpg, d_guia.num_guia, m_guia.fec_guia, d_guia.can_dgui, d_guia.pre_guia, d_guia.cdg_prod, m_guia.cdg_mon, t_cambio.tip_cmb, m_guia.num_ped, m_guia.cdg_tdoc, m_guia.num_docu, m_guia.tip_cmb AS cambio FROM Sico!d_guia, Sico!m_guia, sico!t_cambio WHERE d_guia.cdg_tpg="001" AND m_guia.cdg_tpg="001" AND d_guia.cdg_tpg=m_guia.cdg_tpg AND d_guia.num_guia=m_guia.num_guia AND DTOS(m_guia.fec_guia)=DTOS(t_cambio.fec_cmb) AND m_guia.swt_est<>"A" AND m_guia.fec_guia<=dhasta AND m_guia.cdg_area=carea AND d_guia.cdg_prod=cprod UNION SELECT d_guia.cdg_tpg, d_guia.num_guia, m_guia.fec_guia, d_guia.can_dgui, d_guia.pre_guia, d_guia.cdg_prod, m_guia.cdg_mon, t_cambio.tip_cmb, m_guia.num_ped, m_guia.cdg_tdoc, m_guia.num_docu, 1.000  AS cambio FROM Sico!d_guia, Sico!m_guia, sico!t_cambio WHERE d_guia.cdg_tpg<>"001" AND m_guia.cdg_tpg<>"001" AND d_guia.cdg_tpg=m_guia.cdg_tpg AND d_guia.num_guia=m_guia.num_guia AND DTOS(m_guia.fec_guia)=DTOS(t_cambio.fec_cmb) AND m_guia.swt_est<>"A" AND m_guia.fec_guia<=dhasta AND m_guia.ori_area=carea AND d_guia.cdg_prod=cprod ORDER BY 3, 1, 2 INTO CURSOR curPProm
 SELECT curpprom
 GOTO TOP
 DO WHILE  .NOT. EOF("curPProm")
    IF curpprom.cdg_tpg<>"001"
       ncambio = IIF(curpprom.tip_cmb>0, curpprom.tip_cmb, curpprom.cambio)
    ELSE
       ncambio = IIF(curpprom.cambio>0, curpprom.cambio, curpprom.tip_cmb)
    ENDIF
    IF curpprom.cdg_mon<>cmon
       DO CASE
          CASE curpprom.cdg_mon="001" .AND. cmon<>"001"
             ncosto = ROUND(curpprom.pre_guia/ncambio, 4)
          CASE curpprom.cdg_mon<>"001" .AND. cmon="001"
             ncosto = ROUND(curpprom.pre_guia*ncambio, 4)
       ENDCASE
    ELSE
       ncosto = curpprom.pre_guia
    ENDIF
    m.can_ing = IIF(curpprom.cdg_tpg="001", curpprom.can_dgui, 0)
    m.pre_ing = IIF(curpprom.cdg_tpg="001", IIF(ncosto=0, npreant, ncosto), 0)
    m.tot_ing = IIF(curpprom.cdg_tpg="001", ROUND(m.can_ing*m.pre_ing, 2), 0)
    m.can_sal = IIF(curpprom.cdg_tpg<>"001", curpprom.can_dgui, 0)
    m.pre_sal = IIF(curpprom.cdg_tpg<>"001", npreant, 0)
    m.tot_sal = IIF(curpprom.cdg_tpg<>"001", ROUND(m.can_sal*m.pre_sal, 2), 0)
    m.can_sld = ncanant+m.can_ing-m.can_sal
    m.tot_sld = ntotant+m.tot_ing-m.tot_sal
    IF ncanant<0 .OR. ntotant<0
       ncansld = ABS(ncanant)+m.can_ing-m.can_sal
       ntotsld = ABS(ntotant)+m.tot_ing-m.tot_sal
       m.pre_sld = IIF(ncansld=0, npreant, ROUND(ntotsld/ncansld, 4))
       m.tot_sld = ROUND(m.can_sld*m.pre_sld, 2)
    ENDIF
    IF m.can_sld<=0 .OR. m.tot_sld<=0
       m.pre_sld = npreant
    ELSE
       IF ncanant<0 .OR. ntotant<0
       ELSE
          m.pre_sld = ROUND(m.tot_sld/m.can_sld, 4)
       ENDIF
    ENDIF
    ncanant = m.can_sld
    npreant = m.pre_sld
    ntotant = m.tot_sld
    SELECT curpprom
    SKIP IN curpprom
 ENDDO
 IF USED("curPProm")
    USE IN curpprom
 ENDIF
 RETURN (npreant)
ENDFUNC
**
FUNCTION CorHelp
 PARAMETER copc, citem
 PRIVATE ccod, nlen
 IF SET('DELETED')='OFF'
    SET DELETED ON
 ENDIF
 SELECT num_help FROM sico!m_ayuda WHERE cdg_opc=copc AND num_item=citem ORDER BY num_help INTO CURSOR curHelp
 nlen = 10
 GOTO BOTTOM
 IF EOF()
    ccod = cerosizq(STR(1, nlen))
 ELSE
    ccod = cerosizq(STR(VAL(curhelp.num_help)+1, nlen))
 ENDIF
 USE IN curhelp
 RETURN (ccod)
ENDFUNC
**
PROCEDURE salta
_screen.ActiveForm.txtDni.SetFocus
ENDPROC

PROCEDURE errSico
 PARAMETER merror, mess, mess1, mprog, mlineno
 LOCAL cerror
 =opendbf("&gDirDbf\Sico","m_usuari.dbf", "cdg_usr")
 IF SEEK(gcodusu, "m_usuari")
    nuser = VAL(des_encripta(m_usuari.dat_usr))
    IF nuser>0
       REPLACE m_usuari.dat_usr WITH encripta(CPCONVERT(1252, 850, "000"))
    ENDIF
 ENDIF
 =closedbf("&gDirDbf\m_usuari.dbf")
 cerror = ""
 cerror = cerror+'Fecha/Hora: '+DTOC(DATE())+" - "+TIME()+" - Usuario: "+gcodusu+CHR(13)
 cerror = cerror+'Nombre o Razón Social: '+gnomemp+CHR(13)
 cerror = cerror+'Número de error: '+LTRIM(STR(merror))+CHR(13)
 cerror = cerror+'Mensaje de error: '+mess+CHR(13)
 cerror = cerror+'Línea de código con error: '+mess1+CHR(13)
 cerror = cerror+'Número de línea del error: '+LTRIM(STR(mlineno))+CHR(13)
 cerror = cerror+'Programa con error: '+mprog+CHR(13)
 cerror = cerror+CHR(13)
 cerror = cerror+'Se ha generado el archivo SICO.ERR en la carpeta '+gdirusu+CHR(13)
 cerror = cerror+'Favor enviar el archivo a soporte@soinfosa.com a la brevedad'+CHR(13)
 cerror = cerror+'posible para su revisión o comunicarse al 263-9431. Gracias'+CHR(13)
 naccion = MESSAGEBOX(cerror, 0305, "Mensaje de Error")
 cpath = SYS(5)+CURDIR()
 SET DEFAULT TO &gdirusu
 IF FILE('&gDirUsu\Sico.err')
    gnerrfile = FOPEN('Sico.err', 12)
    ntamaño = FSEEK(gnerrfile, 0, 2)
 ELSE
    gnerrfile = FCREATE('Sico.err')
 ENDIF
 IF gnerrfile<0
    WAIT WINDOW NOWAIT 'No se puede abrir o crear el archivo de resultado'
 ELSE
    cerror = 'Fecha/Hora: '+DTOC(DATE())+" - "+TIME()+" - Usuario: "+gcodusu
    = FPUTS(gnerrfile, cerror)
    cerror = 'Nombre o Razón Social: '+gnomemp+CHR(13)
    = FPUTS(gnerrfile, cerror)
    cerror = 'Número de error: '+LTRIM(STR(merror))+CHR(13)
    = FPUTS(gnerrfile, cerror)
    cerror = 'Mensaje de error: '+mess+CHR(13)
    = FPUTS(gnerrfile, cerror)
    cerror = 'Línea de código con error: '+mess1+CHR(13)
    = FPUTS(gnerrfile, cerror)
    cerror = 'Número de línea del error: '+LTRIM(STR(mlineno))+CHR(13)
    = FPUTS(gnerrfile, cerror)
    cerror = 'Programa con error: '+mprog+CHR(13)
    = FPUTS(gnerrfile, cerror)
    cerror = REPLICATE("-", 80)+CHR(13)
    = FPUTS(gnerrfile, cerror)
 ENDIF
 = FCLOSE(gnerrfile)
 SET DEFAULT TO &cpath
 IF naccion=2
    CLOSE ALL
    CLEAR EVENTS
    CLEAR DLLS
    CLEAR ALL
    RELEASE ALL EXTENDED
    SET HELP ON
    ON KEY LABEL F2 ON KEY
    ON KEY LABEL F10 ON KEY
    ON ERROR
    SET DEBUG ON
    SET SYSMENU TO DEFAULT
    SET EXCLUSIVE ON
 ELSE
    RETRY
 ENDIF
 RETURN
ENDPROC


Function ListCampoTab
	Lparameters cTable
	Create Cursor g_texto (cmsql M Null)
	m.cmsql = ""
	If Parameters()<1
		Return m.cmsql
	Endif

	cone.conecta()


	cCurTmpTab = sp_SELECT(CONE.BaseDatos() +"!"+cTable, "", "","1")
	cone.desconecta()
*!*		c = "SELECT top 1 * FROM " +CONE.BaseDatos() +"!"+cTable + " order by 1 into cursor cCurTmpTab"
*!*		&c

	Select g_texto
	Append Blank
	Replace g_texto.cmsql With ""
	Dimension acampos(1, 18)
	ncampos = Afield(acampos, "&cCurTmpTab")
	Use In "&cCurTmpTab"
	cmcampo = ""
	ncont = 0
	For i = 1 To ncampos
		ncont = ncont+1
		cmcampo = cmcampo+Iif(ncont=1, "", ",")+Alltrim(acampos(i, 1))
		Select g_texto
		Replace g_texto.cmsql With g_texto.cmsql+Iif(ncont=1, "", ",")+Alltrim(acampos(i, 1))
	Endfor
	cmsql = cmsql+cmcampo
	Select g_texto
	Replace g_texto.cmsql With g_texto.cmsql
	Scatter Memo Memvar
	Use In g_texto
	Return m.cmsql


Function ListValoresCampo
	Lparameters cTable
	Create Cursor g_texto (cmsql M Null)
	cmvalor  = ""
	cmsql= " "
	If Parameters()<1
		Return m.cmsql
	Endif
	Select g_texto
	Append Blank
	Replace g_texto.cmsql With ""
	Dimension acampos(1, 18)
	cone.conecta()
	cCurTmpTab = sp_SELECT(CONE.BaseDatos() +"!"+cTable, "", "","1")
	cone.desconecta()
	ncampos = Afield(acampos, "&cCurTmpTab")
	Use In "&cCurTmpTab"
	cmcampo = ""
	ncont = 0
	For i = 1 To ncampos
		ncont = ncont+1
		cnomfld = "m."+acampos(i, 1)
		ctipfld = acampos(i, 2)
		nlenfld = acampos(i, 3)
		ndecfld = acampos(i, 4)
		cvalfld = &cnomfld
		Do Case
			Case ctipfld="V"
				cvalfld = Iif(Isnull(cvalfld), "", cvalfld )
				cvalfld = Chrtranc(cvalfld, "'", " ")
				cvalor = "'"+Left(cvalfld+Space(nlenfld), nlenfld)+"'"
			Case ctipfld="C"
				cvalfld = Iif(Isnull(cvalfld), "", cvalfld )
				cvalfld = Chrtranc(cvalfld, "'", " ")
				cvalor = "'"+Left(Alltrim(cvalfld), nlenfld)+"'"
			Case ctipfld="I"
				cvalfld = Iif(Isnull(cvalfld), 0, cvalfld )
				cvalor = Alltrim(Str(cvalfld, nlenfld))
			Case ctipfld="N"
				cvalfld = Iif(Isnull(cvalfld), 0, cvalfld )
				cvalor = Alltrim(Str(cvalfld, nlenfld, ndecfld))
			Case ctipfld="T"
				cvalfld = Iif(Isnull(cvalfld), CtoT("01/01/1900"), cvalfld )
				cvalor = Iif( .Not. Empty(cvalfld) .And.  .Not. Isnull(cvalfld), "CTOT('"+TTOC(cvalfld)+"')", "NULL")

			Case ctipfld="D"
				cvalfld = Iif(Isnull(cvalfld), Ctod("01/01/1900"), cvalfld )
				cvalor = Iif( .Not. Empty(cvalfld) .And.  .Not. Isnull(cvalfld), "CTOD('"+DTOC(cvalfld)+"')", "NULL")

			Case ctipfld="L"


				cvalfld = Iif(Isnull(cvalfld), 0, Iif(cvalfld,1,0) )

				cvalor = Alltrim(Str(cvalfld, nlenfld, ndecfld))

			Case ctipfld="M"
				cvalfld = Iif(Isnull(cvalfld), "", cvalfld )
				cvalfld = Chrtranc(cvalfld, "'", " ")
				cvalor = "'"+Alltrim(cvalfld)+"'"
		Endcase
		cmvalor = cmvalor+Iif(ncont=1, "", ",")+cvalor
		Select g_texto
		Replace g_texto.cmsql With g_texto.cmsql+Iif(ncont=1, "", ",")+cvalor
	Endfor
	cmsql = cmvalor
	Select g_texto
	Replace g_texto.cmsql With g_texto.cmsql
	Scatter Memo Memvar
	Use In g_texto
	Return m.cmsql
Endfunc



Function ListCampoModi
	Lparameters  cTable
	Create Cursor g_texto (cmsql M Null)
	m.cmsql = ""
	If Parameters()<1
		Return nres
	Endif
	cone.conecta()
	cCurTmpTab = sp_SELECT(CONE.BaseDatos() +"!"+cTable, "", "","1")
	cone.desconecta()
	Select g_texto
	Append Blank
	Replace g_texto.cmsql With ""
	ncampos = Fcount('&cCurTmpTab')
	Dimension gaupdate(ncampos, 4)
	Select '&cCurTmpTab'
	nupdate = Afield(gaupdate, '&cCurTmpTab')
	Use In "&cCurTmpTab"
	ncont = 0
	For i = 1 To nupdate
		ncont = ncont+1
		cnomfld = "m."+gaupdate(i, 1)
		ctipfld = gaupdate(i, 2)
		nlenfld = gaupdate(i, 3)
		ndecfld = gaupdate(i, 4)
		cvalfld = &cnomfld
		Do Case
			Case ctipfld="V"
				cvalfld = Chrtranc(cvalfld, "'", " ")
				cvalor = Iif( .Not. Isnull(cvalfld), "'"+Left(cvalfld+Space(nlenfld), nlenfld)+"'", "NULL")
			Case ctipfld="C"
				cvalfld = Chrtranc(cvalfld, "'", " ")
				cvalor = Iif( .Not. Isnull(cvalfld), "'"+Left(Alltrim(cvalfld), nlenfld)+"'", "NULL")

			Case ctipfld="N"
				cvalor = Iif( .Not. Isnull(cvalfld), Alltrim(Str(cvalfld, nlenfld, ndecfld)), "NULL")
			Case ctipfld="T"
				cvalor = Iif( .Not. Empty(cvalfld) .And.  .Not. Isnull(cvalfld), "ctoT('"+Ttoc(cvalfld)+"')", "CTOD('')")

			Case ctipfld="D"
				cvalor = Iif( .Not. Empty(cvalfld) .And.  .Not. Isnull(cvalfld), "ctod('"+dtoc(cvalfld)+"')", "CTOD('')")

			Case ctipfld="M"
				cvalfld = Chrtranc(cvalfld, "'", " ")
				cvalor = Iif( .Not. Isnull(cvalfld), "'"+Alltrim(cvalfld)+"'", "NULL")
		Endcase
		Select g_texto
		Replace g_texto.cmsql With g_texto.cmsql+Iif(ncont=1, "", ",")+Alltrim(gaupdate(i, 1))+"="+cvalor
	Endfor
	Select g_texto
	Replace g_texto.cmsql With g_texto.cmsql
	Scatter Memo Memvar
	Use In g_texto
	Return m.cmsql
Endfunc
