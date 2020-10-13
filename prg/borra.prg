PUBLIC cDir, gDirApp, gDirDbf  


SET CENTURY ON 
SET SAFETY off

IF  SetPath()

IF USED("m_reghor")
	USE IN m_reghor
ENDIF 
*
USE gDirDbf+"\m_reghor.dbf" IN 0 excl 

SELECT m_reghor
ZAP 

IF USED("m_reghor")
	USE IN m_reghor
ENDIF 
 
ENDIF 


Function SetPath()
	Local lcSys16, ;
		lcProgram

	lcSys16 = Sys(16)
	lcProgram = Substr(lcSys16, At(":", lcSys16) - 1)

	Cd Left(lcProgram, Rat("\", lcProgram))
*-- Si estamos ejecutando directamente INICIO.PRG,
*-- usar CD para ir hasta el directorio primario
	If Right(lcProgram, 3) = "FXP"
		Cd ..
	ENDIF
	gDirApp = SYS(5)+ SYS(2003)
	gDirDbf  = gDirApp +"\data"
	Set Path To clases, form, img, ;
		prg, Data, Other

	Set Classlib TO ctrlsistencia
	SET PROCEDURE TO doslib 
Endfunc