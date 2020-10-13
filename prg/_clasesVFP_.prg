
DEFINE CLASS M_Emple AS  cstm OF "base.vcx"

cApellidos = ''
cCargo = ''
cCdgEmp = ''
cCdgLoc = ''
cCdgPlan = ''
cDirecc = ''
cDni = ''
dFecCese = ctod('//')
dFecIngre = ctod('//')
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
	ENDPROC

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
	TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_EMP = '<<m.CDG_EMP>>' and  CDG_LOC = '<<m.CDG_LOC>>' and  CDG_PLAN = '<<m.CDG_PLAN>>'
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
CDG_EMP = '<<m.CDG_EMP>>' and  CDG_LOC = '<<m.CDG_LOC>>' and  CDG_PLAN = '<<m.CDG_PLAN>>'
ENDTEXT
TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
ENDTEXT

IF cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
RELEASE  APELLIDOS,CARGO,CDG_EMP,CDG_LOC,CDG_PLAN,DIRECC,DNI,FEC_CESE,FEC_INGRE,NOMBRES
	Endproc

	Procedure CargaVar
PUBLIC APELLIDOS,CARGO,CDG_EMP,CDG_LOC,CDG_PLAN,DIRECC,DNI,FEC_CESE,FEC_INGRE,NOMBRES
	WITH THIS
m.APELLIDOS = ALLTRIM(.cApellidos)
m.CARGO = ALLTRIM(.cCargo)
m.CDG_EMP = ALLTRIM(.cCdgEmp)
m.CDG_LOC = ALLTRIM(.cCdgLoc)
m.CDG_PLAN = ALLTRIM(.cCdgPlan)
m.DIRECC = ALLTRIM(.cDirecc)
m.DNI = ALLTRIM(.cDni)
m.FEC_CESE = .dFecCese
m.FEC_INGRE = .dFecIngre
m.NOMBRES = ALLTRIM(.cNombres)
	ENDWITH
	ENDPROC

		Procedure Llenar
		Lparameters ccurtempo
		With This
.cApellidos = ALLTRIM(&ccurtempo..APELLIDOS)
.cCargo = ALLTRIM(&ccurtempo..CARGO)
.cCdgEmp = ALLTRIM(&ccurtempo..CDG_EMP)
.cCdgLoc = ALLTRIM(&ccurtempo..CDG_LOC)
.cCdgPlan = ALLTRIM(&ccurtempo..CDG_PLAN)
.cDirecc = ALLTRIM(&ccurtempo..DIRECC)
.cDni = ALLTRIM(&ccurtempo..DNI)
.dFecCese = &ccurtempo..FEC_CESE
.dFecIngre = &ccurtempo..FEC_INGRE
.cNombres = ALLTRIM(&ccurtempo..NOMBRES)
	ENDWITH
	ENDPROC

	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
	TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_EMP = '<<m.CDG_EMP>>' and  CDG_LOC = '<<m.CDG_LOC>>' and  CDG_PLAN = '<<m.CDG_PLAN>>'
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
.dFecCese= ctod('//')
.dFecIngre= ctod('//')
.cNombres= ''
ENDWITH

Endproc
ENDDEFINE

DEFINE CLASS M_Hrloca AS  cstm OF "base.vcx"

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
	ENDPROC

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

IF cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
RELEASE  CDG_EMP,CDG_LOC,CDG_MOT,CDG_TUR,HORA,TIPO
	Endproc

	Procedure CargaVar
PUBLIC CDG_EMP,CDG_LOC,CDG_MOT,CDG_TUR,HORA,TIPO
	WITH THIS
m.CDG_EMP = ALLTRIM(.cCdgEmp)
m.CDG_LOC = ALLTRIM(.cCdgLoc)
m.CDG_MOT = ALLTRIM(.cCdgMot)
m.CDG_TUR = ALLTRIM(.cCdgTur)
m.HORA = ALLTRIM(.cHora)
m.TIPO = ALLTRIM(.cTipo)
	ENDWITH
	ENDPROC

		Procedure Llenar
		Lparameters ccurtempo
		With This
.cCdgEmp = ALLTRIM(&ccurtempo..CDG_EMP)
.cCdgLoc = ALLTRIM(&ccurtempo..CDG_LOC)
.cCdgMot = ALLTRIM(&ccurtempo..CDG_MOT)
.cCdgTur = ALLTRIM(&ccurtempo..CDG_TUR)
.cHora = ALLTRIM(&ccurtempo..HORA)
.cTipo = ALLTRIM(&ccurtempo..TIPO)
	ENDWITH
	ENDPROC

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
ENDWITH

Endproc
ENDDEFINE

DEFINE CLASS D_Local AS  cstm OF "base.vcx"

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
	ENDPROC

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
	TEXT TO m.Condi NOSHOW TEXTMERGE

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

ENDTEXT
TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
ENDTEXT

IF cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
RELEASE  CDG_LOC,DES_LOC
	Endproc

	Procedure CargaVar
PUBLIC CDG_LOC,DES_LOC
	WITH THIS
m.CDG_LOC = ALLTRIM(.cCdgLoc)
m.DES_LOC = ALLTRIM(.cDesLoc)
	ENDWITH
	ENDPROC

		Procedure Llenar
		Lparameters ccurtempo
		With This
.cCdgLoc = ALLTRIM(&ccurtempo..CDG_LOC)
.cDesLoc = ALLTRIM(&ccurtempo..DES_LOC)
	ENDWITH
	ENDPROC

	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
	TEXT TO m.Condi NOSHOW TEXTMERGE

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
ENDWITH

Endproc
ENDDEFINE

DEFINE CLASS M_Empre AS  cstm OF "base.vcx"

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
	ENDPROC

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

IF cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
RELEASE  CDG_EMP,DES_EMP
	Endproc

	Procedure CargaVar
PUBLIC CDG_EMP,DES_EMP
	WITH THIS
m.CDG_EMP = ALLTRIM(.cCdgEmp)
m.DES_EMP = ALLTRIM(.cDesEmp)
	ENDWITH
	ENDPROC

		Procedure Llenar
		Lparameters ccurtempo
		With This
.cCdgEmp = ALLTRIM(&ccurtempo..CDG_EMP)
.cDesEmp = ALLTRIM(&ccurtempo..DES_EMP)
	ENDWITH
	ENDPROC

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
ENDWITH

Endproc
ENDDEFINE

DEFINE CLASS M_Reghor AS  cstm OF "base.vcx"

cCdgEmp = ''
cCdgLoc = ''
cCdgMot = ''
cCdgPlan = ''
cCdgTur = ''
tHrMarca = ctod('//')
tHrTur = ctod('//')
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
	ENDPROC

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

IF cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
RELEASE  CDG_EMP,CDG_LOC,CDG_MOT,CDG_PLAN,CDG_TUR,HR_MARCA,HR_TUR,ID_REG,PC
	Endproc

	Procedure CargaVar
PUBLIC CDG_EMP,CDG_LOC,CDG_MOT,CDG_PLAN,CDG_TUR,HR_MARCA,HR_TUR,ID_REG,PC
	WITH THIS
m.CDG_EMP = ALLTRIM(.cCdgEmp)
m.CDG_LOC = ALLTRIM(.cCdgLoc)
m.CDG_MOT = ALLTRIM(.cCdgMot)
m.CDG_PLAN = ALLTRIM(.cCdgPlan)
m.CDG_TUR = ALLTRIM(.cCdgTur)
m.HR_MARCA = .tHrMarca
m.HR_TUR = .tHrTur
m.ID_REG = ALLTRIM(.cIdReg)
m.PC = ALLTRIM(.cPc)
	ENDWITH
	ENDPROC

		Procedure Llenar
		Lparameters ccurtempo
		With This
.cCdgEmp = ALLTRIM(&ccurtempo..CDG_EMP)
.cCdgLoc = ALLTRIM(&ccurtempo..CDG_LOC)
.cCdgMot = ALLTRIM(&ccurtempo..CDG_MOT)
.cCdgPlan = ALLTRIM(&ccurtempo..CDG_PLAN)
.cCdgTur = ALLTRIM(&ccurtempo..CDG_TUR)
.tHrMarca = &ccurtempo..HR_MARCA
.tHrTur = &ccurtempo..HR_TUR
.cIdReg = ALLTRIM(&ccurtempo..ID_REG)
.cPc = ALLTRIM(&ccurtempo..PC)
	ENDWITH
	ENDPROC

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
.tHrMarca= ctod('//')
.tHrTur= ctod('//')
.cIdReg= ''
.cPc= ''
ENDWITH

Endproc
ENDDEFINE

DEFINE CLASS D_Periodo AS  cstm OF "base.vcx"

cDesPerio = ''
dFinPerio = ctod('//')
cIdPerio = ''
dIniPerio = ctod('//')
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
	ENDPROC

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

IF cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
RELEASE  DES_PERIO,FIN_PERIO,ID_PERIO,INI_PERIO
	Endproc

	Procedure CargaVar
PUBLIC DES_PERIO,FIN_PERIO,ID_PERIO,INI_PERIO
	WITH THIS
m.DES_PERIO = ALLTRIM(.cDesPerio)
m.FIN_PERIO = .dFinPerio
m.ID_PERIO = ALLTRIM(.cIdPerio)
m.INI_PERIO = .dIniPerio
	ENDWITH
	ENDPROC

		Procedure Llenar
		Lparameters ccurtempo
		With This
.cDesPerio = ALLTRIM(&ccurtempo..DES_PERIO)
.dFinPerio = &ccurtempo..FIN_PERIO
.cIdPerio = ALLTRIM(&ccurtempo..ID_PERIO)
.dIniPerio = &ccurtempo..INI_PERIO
	ENDWITH
	ENDPROC

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
.dFinPerio= ctod('//')
.cIdPerio= ''
.dIniPerio= ctod('//')
ENDWITH

Endproc
ENDDEFINE

DEFINE CLASS D_Reghor AS  cstm OF "base.vcx"

cCdgPlan = ''
cIdPerio = ''
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
	ENDPROC

	Procedure Eliminar
		_hecho = .F.
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
	TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_PLAN = '<<m.CDG_PLAN>>' and  ID_PERIO = '<<m.ID_PERIO>>'
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
CDG_PLAN = '<<m.CDG_PLAN>>' and  ID_PERIO = '<<m.ID_PERIO>>'
ENDTEXT
TEXT TO m._sql NOSHOW TEXTMERGE
m.cNmbTab, m.cModiCampTab , m.Condi
ENDTEXT

IF cone.doSpdbc("nValor = sp_Modificar("+ _Sql+")")>0
			_hecho = .T.
		Else
			This.mensaje=cone.getmensaje()
		Endif
		This.LiberaVar()
		Return _hecho
	Endproc

	Procedure LiberaVar
RELEASE  CDG_PLAN,ID_PERIO,OBS_REGHOR
	Endproc

	Procedure CargaVar
PUBLIC CDG_PLAN,ID_PERIO,OBS_REGHOR
	WITH THIS
m.CDG_PLAN = ALLTRIM(.cCdgPlan)
m.ID_PERIO = ALLTRIM(.cIdPerio)
m.OBS_REGHOR = ALLTRIM(.cObsReghor)
	ENDWITH
	ENDPROC

		Procedure Llenar
		Lparameters ccurtempo
		With This
.cCdgPlan = ALLTRIM(&ccurtempo..CDG_PLAN)
.cIdPerio = ALLTRIM(&ccurtempo..ID_PERIO)
.cObsReghor = ALLTRIM(&ccurtempo..OBS_REGHOR)
	ENDWITH
	ENDPROC

	Procedure ActivaReg
		This.CargaVar()
		m.cNmbTab = This.cNombtabla
	TEXT TO m.Condi NOSHOW TEXTMERGE
CDG_PLAN = '<<m.CDG_PLAN>>' and  ID_PERIO = '<<m.ID_PERIO>>'
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
.cCdgPlan= ''
.cIdPerio= ''
.cObsReghor= ''
ENDWITH

Endproc
ENDDEFINE
