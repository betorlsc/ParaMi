Define Class Base As Session


	Protected  SourceType,Driver,db,ruta,adi,ncon
	Function Init(_SourceType As String ,_drv As String,_db As String,_ruta As String,_add As String)
		This.SourceType=Alltrim(_SourceType)
		This.Driver=Alltrim(_drv)
		This.db=Alltrim(_db)
		This.ruta=Alltrim(_ruta)
		This.adi=Alltrim(_add)
		This.ncon=0
		Set Talk          Off
		Set Safety        Off
		Set Echo          Off
		Set Notify        Off
		Set Status        Off
		Set Brstatus      Off
		Set Bell          Off
		*
		Set Carry         Off
		Set Delete        On
		Set Exact         On
		Set Near          Off
		Set Exclusive     Off
		Set Multilock     On
		Set Cursor        On
		*
		Set Century       On
		Set Century       To 19 ROLLOVER 50
		Set Date          To Dmy
		Set Hours         To 24
		Set Point         To "."
		Set Separator     To ","
		SET TEXTMERGE ON 
	Endfunc

	Function TmpName ()As String
		Local _Temp As String
		_Temp = "c_" + Sys( 2015 )
		Do While Used(_Temp )
			_Temp = "c_" + Sys( 2015 )
		Enddo
		Return ( _Temp )
	Endfunc

	Function fileTemp() As String
		Return Sys(2023)+"\"+cone.TmpName()
	Endfunc

	Procedure BaseDatos As String
		Return This.db


	Procedure IPAddress
		Local lowsock, lcip
		lowsock = Createobjectex("{248DD896-BB45-11CF-9ABC-0080C7E7B78D}", "", "")
		lcip = lowsock.LocalIP
		Return (lcip)
	Endfunc

Enddefine
