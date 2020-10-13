Public cDir, gDirApp, gDirDbf , cone
Set Exclusive Off

Set Date Dmy
Set Century On
Set Safety On
Set Sysmenu Off
SET ENGINEBEHAVIOR 70
*!*	_Screen.TitleBar = 0
*!*	_Screen.WindowState = 2
_Screen.AddProperty("cNomUser", "")
_Screen.AddProperty("cRutaApli", "")
_Screen.AddProperty("cDesEmpresa", "")
_Screen.AddProperty("cAbrEmpresa", "")
_Screen.AddProperty("cDirEmpresa", "")
_Screen.AddProperty("cRUCEmpresa", "")

If  SetPath()

	Set Path To  clases, Form, img, prg, Menu
	Set Classlib To  Base.vcx, ctrlsistencia
	Set Procedure To doslib.prg, clasesvfp.prg , execu, conexion, Base
	Set Deleted On
*!*		If Type("_Screen.oVfpStretch") = "O"
*!*			Removeproperty(_Screen, "oVfpStretch")
*!*		Endif


*!*		=AddProperty(_Screen, "oVfpStretch", Newobject("VfpStretch"))

	If File("&gDirApp\config.ini")
		objLibra = Createobject("cstm")

		With objLibra
			cValue = "001"
			If Used("config")
				Use In "config"
			Endif

			Use In 0 "&gDirApp\config.ini" Shared
			Select config
			cCurConfig ="config"
			Locate All For Alltrim(config.cdg_emp)  =  cValue
			If Found()

				.desenccontraena(&cCurConfig..des_emp)
				_Screen.cDesEmpresa = .getdesenccontraena()
				.desenccontraena(&cCurConfig..abr_emp)
				_Screen.cAbrEmpresa = .getdesenccontraena()
				.desenccontraena(&cCurConfig..dir_emp)
				_Screen.cDirEmpresa = .getdesenccontraena()
				.desenccontraena(&cCurConfig..ruc_emp)
				_Screen.cRUCEmpresa = .getdesenccontraena()


				m.SourceType ="DBC"
				m.drv = "Microsoft Visual FoxPro Driver"
				m.ruta = Alltrim(&cCurConfig..rut_db)
				.desenccontraena(&cCurConfig..nom_db)
				m.db=.getdesenccontraena()
				gDirDbf = m.ruta
				m.add= "Exclusive=No;NULL=NO;Collate=Machine;BACKGROUNDFETCH=NO;DELETED=NO"
				If Used(cCurConfig )
					Use In (cCurConfig)
				Endif
			Endif

		Endwith
	Else
		Wait Window Timeout 1 "No se encuentra Config.INI"
		Set Sysmenu To Default
		Return
	Endif

	cone=Createobject("execu",m.SourceType, m.drv,m.db,m.ruta,m.add)
	Set DataSession To cone.DataSessionId

	_Screen.Caption ="Control de Asistencia [" + Alltrim(_Screen.cDesEmpresa)+"]"
	_Screen.WindowState= 2

*Do Form frm_marcado	
*!*	Do Main.MPR
*!*		Read Events

	_Screen.TitleBar = 1
	Set Sysmenu On
Endif


Function SetPath()
	Local lcSys16, ;
		lcProgram

	lcSys16 = Sys(16)
	lcProgram = Substr(lcSys16, At(":", lcSys16) - 1)

	Cd Left(lcProgram, Rat("\", lcProgram))
	If Right(lcProgram, 3) = "FXP"
		Cd ..
	Endif
	gDirApp = Sys(5)+ Sys(2003)
	_Screen.crutaapli= gDirApp
	gDirDbf  = gDirApp +"\data"
	Set Path To clases, Form, img, ;
		prg, Data, Other

	Set Classlib To ctrlsistencia
	Set Procedure To doslib
Endfunc
