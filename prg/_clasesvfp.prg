
Define Class M_Emple As  cstm Of "base.vcx"

	cApellidos = ''
	cCargo = ''
	cCdgEmp = ''
	cCdgLoc = ''
	cCdgPlan = ''
	cDirecc = ''
	cDni = ''
		dFecIngre = Ctod('//')
		dFecCese = Ctod('//')
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
		Release  APELLIDOS,CARGO,CDG_EMP,CDG_LOC,CDG_PLAN,DIRECC,DNI,FEC_INGRE,FEC_CESE,NOMBRES
	Endproc

	Procedure CargaVar
		Public APELLIDOS,CARGO,CDG_EMP,CDG_LOC,CDG_PLAN,DIRECC,DNI,FEC_INGRE,FEC_CESE,NOMBRES
		With This
			m.APELLIDOS = Alltrim(.cApellidos)
			m.CARGO = Alltrim(.cCargo)
			m.CDG_EMP = Alltrim(.cCdgEmp)
			m.CDG_LOC = Alltrim(.cCdgLoc)
			m.CDG_PLAN = Alltrim(.cCdgPlan)
			m.DIRECC = Alltrim(.cDirecc)
			m.DNI = Alltrim(.cDni)
			m.FEC_INGRE = .dFecIngre
			m.FEC_CESE = .dFecCese
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
			.cDni = Alltrim(&ccurtempo..DNI)
			.dFecIngre = &ccurtempo..FEC_INGRE
			.dFecCese = &ccurtempo..FEC_CESE
			.cNombres = Alltrim(&ccurtempo..NOMBRES)
		Endwith
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
			.dFecIngre= Ctod('//')
			.dFecCese= Ctod('//')
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
