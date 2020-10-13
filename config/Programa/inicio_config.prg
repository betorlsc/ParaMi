SET STEP ON 
 PUBLIC cone
 _SCREEN.addproperty("cNomUser", "")
 _SCREEN.addproperty("cRutaApli", "")
 IF setpath()
    SET PATH TO Programa, bd, clases, Formulario, Imagenes
    SET PROCEDURE TO conexion, Base, execu, libcomercial, libvarios, libraria_config
    SET CLASSLIB TO aplica.vcx, Base.vcx, miscontroles.vcx
    gdirapp = ALLTRIM(_SCREEN.crutaapli)
    IF !FILE("&gDirApp\Config.ini")
       CREATE TABLE "&gdirapp\config.ini" FREE (cdg_emp c(5), des_emp c(180), abr_emp c(60),  dir_emp c(220) NULL, ruc_emp c(30) NULL,  srv_bd v(80) NULL, usr_db c(30) NULL, psw_db v(20) NULL,  drv_db v(60) NULL, nom_db v(100) NULL,  swt_emp c(1) NULL, pat_emp c(20) NULL, rut_db v(180) )
       USE IN config
    ENDIF
    DO FORM cnf_princ
    ON KEY LABEL CTRL+F1 ON KEY
    SET SYSMENU ON
 ENDIF
ENDPROC
**
PROCEDURE SetPath
 LOCAL lcsys16, lcprogram
 lcsys16 = SYS(16)
 lcprogram = SUBSTR(lcsys16, AT(":", lcsys16)-1)
 CD LEFT(lcprogram, RAT("\", lcprogram))
 IF RIGHT(lcprogram, 3)="FXP"
    CD ..
 ENDIF
 _SCREEN.crutaapli = SYS(5)+SYS(2003)
ENDPROC
**

