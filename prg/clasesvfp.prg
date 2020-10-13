
Define Class M_Emple As  cstm Of "base.vcx"

	cApellidos = ''
	cCargo = ''
	cCdgEmp = ''
	cCdgLoc = ''
	cCdgPlan = ''
	cDirecc = ''
	cDni = ''
	dFecCese = Ctod('//')
	dFecIngre = Ctod('//')
	nAutoHe = 1
	cNombres = ''
	cNombtabla = 'M_EMPLE'

	Procedure Insertar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cCampTab NOSHOW TEXTMERGE
<<ListCampoTab( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.cValTab NOSHOW TEXTMERGE
<<ListValoresCampo( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cCampTab, m.cValTab
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Insertar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_PLAN = '<<m.CDG_PLAN>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor =  sp_Eliminar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Modificar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cModiCampTab NOSHOW TEXTMERGE
<<ListCampoModi( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_PLAN = '<<m.CDG_PLAN>>'
		ENDTEXT
		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
		Release  APELLIDOS,CARGO,CDG_EMP,CDG_LOC,CDG_PLAN,DIRECC,DNI,FEC_CESE,FEC_INGRE,NOMBRES,AUTO_HE
	Endproc

	Procedure CargaVar
		Public APELLIDOS,CARGO,CDG_EMP,CDG_LOC,CDG_PLAN,DIRECC,DNI,FEC_CESE,FEC_INGRE,NOMBRES,AUTO_HE
		With This
			m.APELLIDOS = Alltrim(.cApellidos)
			m.CARGO = Alltrim(.cCargo)
			m.CDG_EMP = Alltrim(.cCdgEmp)
			m.CDG_LOC = Alltrim(.cCdgLoc)
			m.CDG_PLAN = Alltrim(.cCdgPlan)
			m.DIRECC = Alltrim(.cDirecc)
			m.DNI = Alltrim(.cDni)
			m.AUTO_HE = .nAutoHe
			m.FEC_CESE = Iif(Isnull(.dFecCese), Ctod(""), .dFecCese)
			m.FEC_INGRE = Iif(Isnull(.dFecIngre), Ctod(""),.dFecIngre)
			m.NOMBRES = Alltrim(.cNombres)
		Endwith
	Endproc

	Procedure Llenar
		Lparameters ccurtempo
		With This
			.cApellidos = Alltrim(&ccurtempo..APELLIDOS)
			.cCargo = Alltrim(&ccurtempo..CARGO)
			.cCdgEmp = Alltrim(&ccurtempo..CDG_EMP)
			.cCdgLoc = Alltrim(&ccurtempo..CDG_LOC)
			.cCdgPlan = Alltrim(&ccurtempo..CDG_PLAN)
			.cDirecc = Alltrim(&ccurtempo..DIRECC)
			.nAutoHe = &ccurtempo..AUTO_HE
			.cDni = Alltrim(&ccurtempo..DNI)
			.dFecCese = Iif(Isnull(&ccurtempo..FEC_CESE), Ctod(""),&ccurtempo..FEC_CESE)
			.dFecIngre = Iif(Isnull(&ccurtempo..FEC_INGRE), Ctod(""),&ccurtempo..FEC_INGRE)
			.cNombres = Alltrim(&ccurtempo..NOMBRES)
		Endwith
	Endproc
	Procedure Guardar
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
 CDG_PLAN = '<<m.CDG_PLAN>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT
		nValor =""
		If cone.doSpdbc("nValor =  sp_ActivaReg("+_Sql+")")>0

		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return nValor
	Endproc
	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
 CDG_PLAN = '<<m.CDG_PLAN>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT
		nValor =""
		If cone.doSpdbc("nValor =  sp_ActivaReg("+_Sql+")")>0

		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return nValor
	Endproc

	Procedure Limpiar
		With This
			.cApellidos= ''
			.cCargo= ''
			.cCdgEmp= ''
			.cCdgLoc= ''
			.cCdgPlan= ''
			.cDirecc= ''
			.cDni= ''
			.nAutoHe = 1
			.dFecCese= Ctod('//')
			.dFecIngre= Ctod('//')
			.cNombres= ''
		Endwith

	Endproc
Enddefine

Define Class M_Hrloca As  cstm Of "base.vcx"

	cCdgEmp = ''
	cCdgLoc = ''
	cCdgMot = ''
	cCdgTur = ''
	cHora = ''
	cTipo = ''
	cNombtabla = 'M_HRLOCA'

	Procedure Insertar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cCampTab NOSHOW TEXTMERGE
<<ListCampoTab( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.cValTab NOSHOW TEXTMERGE
<<ListValoresCampo( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cCampTab, m.cValTab
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Insertar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_EMP = '<<m.CDG_EMP>>' and  CDG_LOC = '<<m.CDG_LOC>>' and  CDG_MOT = '<<m.CDG_MOT>>' and  CDG_TUR = '<<m.CDG_TUR>>' and  TIPO = '<<m.TIPO>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor =  sp_Eliminar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Modificar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cModiCampTab NOSHOW TEXTMERGE
<<ListCampoModi( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_EMP = '<<m.CDG_EMP>>' and  CDG_LOC = '<<m.CDG_LOC>>' and  CDG_MOT = '<<m.CDG_MOT>>' and  CDG_TUR = '<<m.CDG_TUR>>' and  TIPO = '<<m.TIPO>>'
		ENDTEXT
		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
		Release  CDG_EMP,CDG_LOC,CDG_MOT,CDG_TUR,HORA,TIPO
	Endproc

	Procedure CargaVar
		Public CDG_EMP,CDG_LOC,CDG_MOT,CDG_TUR,HORA,TIPO
		With This
			m.CDG_EMP = Alltrim(.cCdgEmp)
			m.CDG_LOC = Alltrim(.cCdgLoc)
			m.CDG_MOT = Alltrim(.cCdgMot)
			m.CDG_TUR = Alltrim(.cCdgTur)
			m.HORA = Alltrim(.cHora)
			m.TIPO = Alltrim(.cTipo)
		Endwith
	Endproc

	Procedure Llenar
		Lparameters ccurtempo
		With This
			.cCdgEmp = Alltrim(&ccurtempo..CDG_EMP)
			.cCdgLoc = Alltrim(&ccurtempo..CDG_LOC)
			.cCdgMot = Alltrim(&ccurtempo..CDG_MOT)
			.cCdgTur = Alltrim(&ccurtempo..CDG_TUR)
			.cHora = Alltrim(&ccurtempo..HORA)
			.cTipo = Alltrim(&ccurtempo..TIPO)
		Endwith
	Endproc

	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_EMP = '<<m.CDG_EMP>>' and  CDG_LOC = '<<m.CDG_LOC>>' and  CDG_MOT = '<<m.CDG_MOT>>' and  CDG_TUR = '<<m.CDG_TUR>>' and  TIPO = '<<m.TIPO>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT
		nValor =""
		If cone.doSpdbc("nValor =  sp_ActivaReg("+_Sql+")")>0

		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return nValor
	Endproc

	Procedure Limpiar
		With This
			.cCdgEmp= ''
			.cCdgLoc= ''
			.cCdgMot= ''
			.cCdgTur= ''
			.cHora= ''
			.cTipo= ''
		Endwith

	Endproc
Enddefine

Define Class D_Local As  cstm Of "base.vcx"

	cCdgLoc = ''
	cDesLoc = ''
	cNombtabla = 'D_LOCAL'

	Procedure Insertar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cCampTab NOSHOW TEXTMERGE
<<ListCampoTab( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.cValTab NOSHOW TEXTMERGE
<<ListValoresCampo( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cCampTab, m.cValTab
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Insertar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_LOC = '<<m.CDG_LOC>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor =  sp_Eliminar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Modificar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cModiCampTab NOSHOW TEXTMERGE
<<ListCampoModi( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_LOC = '<<m.CDG_LOC>>'
		ENDTEXT
		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
		Release  CDG_LOC,DES_LOC
	Endproc

	Procedure CargaVar
		Public CDG_LOC,DES_LOC
		With This
			m.CDG_LOC = Alltrim(.cCdgLoc)
			m.DES_LOC = Alltrim(.cDesLoc)
		Endwith
	Endproc

	Procedure Llenar
		Lparameters ccurtempo
		With This
			.cCdgLoc = Alltrim(&ccurtempo..CDG_LOC)
			.cDesLoc = Alltrim(&ccurtempo..DES_LOC)
		Endwith
	Endproc

	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_LOC = '<<m.CDG_LOC>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT
		nValor =""
		If cone.doSpdbc("nValor =  sp_ActivaReg("+_Sql+")")>0

		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return nValor
	Endproc

	Procedure Limpiar
		With This
			.cCdgLoc= ''
			.cDesLoc= ''
		Endwith

	Endproc
Enddefine

Define Class M_Empre As  cstm Of "base.vcx"

	cCdgEmp = ''
	cDesEmp = ''
	cNombtabla = 'M_EMPRE'

	Procedure Insertar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cCampTab NOSHOW TEXTMERGE
<<ListCampoTab( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.cValTab NOSHOW TEXTMERGE
<<ListValoresCampo( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cCampTab, m.cValTab
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Insertar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_EMP = '<<m.CDG_EMP>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor =  sp_Eliminar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Modificar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cModiCampTab NOSHOW TEXTMERGE
<<ListCampoModi( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_EMP = '<<m.CDG_EMP>>'
		ENDTEXT
		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
		Release  CDG_EMP,DES_EMP
	Endproc

	Procedure CargaVar
		Public CDG_EMP,DES_EMP
		With This
			m.CDG_EMP = Alltrim(.cCdgEmp)
			m.DES_EMP = Alltrim(.cDesEmp)
		Endwith
	Endproc

	Procedure Llenar
		Lparameters ccurtempo
		With This
			.cCdgEmp = Alltrim(&ccurtempo..CDG_EMP)
			.cDesEmp = Alltrim(&ccurtempo..DES_EMP)
		Endwith
	Endproc

	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_EMP = '<<m.CDG_EMP>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT
		nValor =""
		If cone.doSpdbc("nValor =  sp_ActivaReg("+_Sql+")")>0

		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return nValor
	Endproc

	Procedure Limpiar
		With This
			.cCdgEmp= ''
			.cDesEmp= ''
		Endwith

	Endproc
Enddefine

Define Class M_Reghor As  cstm Of "base.vcx"

	cCdgEmp = ''
	cCdgLoc = ''
	cCdgMot = ''
	cCdgPlan = ''
	cCdgTur = ''
	tHrMarca = Ctod('//')
	tHrTur = Ctod('//')
	cIdReg = ''
	cPc = ''
	cNombtabla = 'M_REGHOR'

	Procedure Insertar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cCampTab NOSHOW TEXTMERGE
<<ListCampoTab( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.cValTab NOSHOW TEXTMERGE
<<ListValoresCampo( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cCampTab, m.cValTab
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Insertar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
ID_REG = '<<m.ID_REG>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor =  sp_Eliminar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Modificar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cModiCampTab NOSHOW TEXTMERGE
<<ListCampoModi( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.Condi NOSHOW TEXTMERGE
ID_REG = '<<m.ID_REG>>'
		ENDTEXT
		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
		Release  CDG_EMP,CDG_LOC,CDG_MOT,CDG_PLAN,CDG_TUR,HR_MARCA,HR_TUR,ID_REG,PC
	Endproc

	Procedure CargaVar
		Public CDG_EMP,CDG_LOC,CDG_MOT,CDG_PLAN,CDG_TUR,HR_MARCA,HR_TUR,ID_REG,PC
		With This
			m.CDG_EMP = Alltrim(.cCdgEmp)
			m.CDG_LOC = Alltrim(.cCdgLoc)
			m.CDG_MOT = Alltrim(.cCdgMot)
			m.CDG_PLAN = Alltrim(.cCdgPlan)
			m.CDG_TUR = Alltrim(.cCdgTur)
			m.HR_MARCA = .tHrMarca
			m.HR_TUR = .tHrTur
			m.ID_REG = Alltrim(.cIdReg)
			m.PC = Alltrim(.cPc)
		Endwith
	Endproc

	Procedure Llenar
		Lparameters ccurtempo
		With This
			.cCdgEmp = Alltrim(&ccurtempo..CDG_EMP)
			.cCdgLoc = Alltrim(&ccurtempo..CDG_LOC)
			.cCdgMot = Alltrim(&ccurtempo..CDG_MOT)
			.cCdgPlan = Alltrim(&ccurtempo..CDG_PLAN)
			.cCdgTur = Alltrim(&ccurtempo..CDG_TUR)
			.tHrMarca = &ccurtempo..HR_MARCA
			.tHrTur = &ccurtempo..HR_TUR
			.cIdReg = Alltrim(&ccurtempo..ID_REG)
			.cPc = Alltrim(&ccurtempo..PC)
		Endwith
	Endproc

	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
ID_REG = '<<m.ID_REG>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT
		nValor =""
		If cone.doSpdbc("nValor =  sp_ActivaReg("+_Sql+")")>0

		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return nValor
	Endproc



	Procedure Correla
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
cdg_emp + cdg_loc = '<<m.cdg_emp>><<m.cdg_loc>>'
		ENDTEXT
		CCurso = This.Select("" , "Max(M_reghor.id_reg) As unti",  m.Condi , "")

		nVarUlt = Iif(Isnull(&CCurso..unti), "0", Right(&CCurso..unti,5))
		nNumer = Int(Val(nVarUlt )) + 1
		Use In &CCurso
		Return nNumer
	Endproc


	Procedure Grabar
		This.CargaVar()

		nValor = .F.

		If  This.ActivaReg() = "1"
			nValor = This.Modificar()
		Else
			This.cIdReg =  Right(This.cCdgLoc,2) + Padl(This.Correla(),5,'0')
			nValor = This.Insertar()

		Endif

		Return nValor
	Endproc

	Procedure Limpiar
		With This
			.cCdgEmp= ''
			.cCdgLoc= ''
			.cCdgMot= ''
			.cCdgPlan= ''
			.cCdgTur= ''
			.tHrMarca= Ctod('//')
			.tHrTur= Ctod('//')
			.cIdReg= ''
			.cPc= ''
		Endwith

	Endproc
Enddefine

Define Class D_Periodo As  cstm Of "base.vcx"

	cDesPerio = ''
	dFinPerio = Ctod('//')
	cIdPerio = ''
	dIniPerio = Ctod('//')
	cNombtabla = 'D_PERIODO'

	Procedure Insertar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cCampTab NOSHOW TEXTMERGE
<<ListCampoTab( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.cValTab NOSHOW TEXTMERGE
<<ListValoresCampo( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cCampTab, m.cValTab
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Insertar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Grabar
		This.CargaVar()

		nValor = .F.

		If  This.ActivaReg() = "1"
			nValor = This.Modificar()
		Else
			This.cIdPerio =  Padl(This.Correla(),5,'0')
			nValor = This.Insertar()

		Endif

		Return nValor
	Endproc

	Procedure Correla
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		m.Condi =""
		CCurso = This.Select("" , "Max(id_perio) As unti",  m.Condi , "")
		nVarUlt = Iif(Isnull(&CCurso..unti), "0", Right(&CCurso..unti,5))
		nNumer = Int(Val(nVarUlt )) + 1
		Use In &CCurso
		Return nNumer
	Endproc

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
ID_PERIO = '<<m.ID_PERIO>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor =  sp_Eliminar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Modificar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cModiCampTab NOSHOW TEXTMERGE
<<ListCampoModi( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.Condi NOSHOW TEXTMERGE
ID_PERIO = '<<m.ID_PERIO>>'
		ENDTEXT
		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
		Release  DES_PERIO,FIN_PERIO,ID_PERIO,INI_PERIO
	Endproc

	Procedure CargaVar
		Public DES_PERIO,FIN_PERIO,ID_PERIO,INI_PERIO
		With This
			m.DES_PERIO = Alltrim(.cDesPerio)
			m.FIN_PERIO = .dFinPerio
			m.ID_PERIO = Alltrim(.cIdPerio)
			m.INI_PERIO = .dIniPerio
		Endwith
	Endproc

	Procedure Llenar
		Lparameters ccurtempo
		With This
			.cDesPerio = Alltrim(&ccurtempo..DES_PERIO)
			.dFinPerio = &ccurtempo..FIN_PERIO
			.cIdPerio = Alltrim(&ccurtempo..ID_PERIO)
			.dIniPerio = &ccurtempo..INI_PERIO
		Endwith
	Endproc

	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
ID_PERIO = '<<m.ID_PERIO>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT
		nValor =""
		If cone.doSpdbc("nValor =  sp_ActivaReg("+_Sql+")")>0

		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return nValor
	Endproc

	Procedure Limpiar
		With This
			.cDesPerio= ''
			.dFinPerio= Ctod('//')
			.cIdPerio= ''
			.dIniPerio= Ctod('//')
		Endwith

	Endproc
Enddefine



Define Class D_Reghor As  cstm Of "base.vcx"

	cCdgPlan = ''
	cIdPerio = ''
	cCdgLoc = ''
	cObsReghor = ''
	cNombtabla = 'D_REGHOR'

	Procedure Insertar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cCampTab NOSHOW TEXTMERGE
<<ListCampoTab( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.cValTab NOSHOW TEXTMERGE
<<ListValoresCampo( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cCampTab, m.cValTab
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Insertar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_PLAN = '<<m.CDG_PLAN>>' and  ID_PERIO = '<<m.ID_PERIO>>' and  CDG_LOC = '<<m.CDG_LOC>>'
		ENDTEXT

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor =  sp_Eliminar("+_Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure Modificar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.cModiCampTab NOSHOW TEXTMERGE
<<ListCampoModi( m.cNmbTab)>>
		ENDTEXT

		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_PLAN = '<<m.CDG_PLAN>>' and  ID_PERIO = '<<m.ID_PERIO>>' and  CDG_LOC = '<<m.CDG_LOC>>'
		ENDTEXT
		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
		ENDTEXT

		If cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
		Release  CDG_PLAN,ID_PERIO,OBS_REGHOR, CDG_LOC
	Endproc

	Procedure Grabar
		This.CargaVar()

		nValor = .F.

		If  This.ActivaReg() = "1"
			nValor = This.Modificar()
		Else
			nValor = This.Insertar()

		Endif

		Return nValor
	Endproc

	Procedure CargaVar
		Public CDG_PLAN,ID_PERIO,OBS_REGHOR, CDG_LOC
		With This
			m.CDG_PLAN   = Alltrim(.cCdgPlan)
			m.ID_PERIO   = Alltrim(.cIdPerio)
			m.CDG_LOC    =  Alltrim(.cCdgLoc)
			m.OBS_REGHOR = Alltrim(.cObsReghor)
		Endwith
	Endproc

	Procedure Llenar
		Lparameters ccurtempo
		With This
			.cCdgPlan = Alltrim(&ccurtempo..CDG_PLAN)
			.cIdPerio = Alltrim(&ccurtempo..ID_PERIO)
			.cCdgLoc  = Alltrim(&ccurtempo..CDG_LOC )
			.cObsReghor = Alltrim(&ccurtempo..OBS_REGHOR)
		Endwith
	Endproc

	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
		TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_PLAN = '<<m.CDG_PLAN>>' and  ID_PERIO = '<<m.ID_PERIO>>'and  CDG_LOC = '<<m.CDG_LOC>>'
		ENDTEXT    

		TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab,  m.Condi
		ENDTEXT
		nValor =""
		If cone.doSpdbc("nValor =  sp_ActivaReg("+_Sql+")")>0

		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return nValor
	Endproc

	Procedure Limpiar
		With This
			.cCdgPlan   = ''
			.cIdPerio   = ''
			.cObsReghor = ''
			.cCdgLoc    = ''
		Endwith

	Endproc
Enddefine
