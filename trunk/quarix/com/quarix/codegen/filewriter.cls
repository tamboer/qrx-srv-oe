/*------------------------------------------------------------------------
   File        : filewriter.cls
   Purpose     :
   Syntax      :
   Description :handles file writing of specific buffer objects
   Author(s)   : adam
   Created     : Fri Nov 14 11:03:02 EEST 2013
   Notes       :
 ----------------------------------------------------------------------*/

 class com.quarix.codegen.filewriter:

	define public property FileName as character no-undo
        get.
        set.

	define public property TabLength as integer no-undo
        get.
        set.

 	define private variable cCurrentLine as character no-undo.
 	define private stream strOutput.

 	method public void DeleteFile():

 		os-delete value(FileName).

 	end method.

 	method private void addText(input cText as character):
 		cCurrentLine = cCurrentLine + cText.
 	end method.

 	method private logical writeLine():

 		if filename = ? or
 			filename = ''
 		then return false.

 		output stream strOutput to value(filename) append.

		if cCurrentLine = '' or
			cCurrentLine= ?
		then
			put stream strOutput unformatted chr(10).
		else
 			put stream strOutput unformatted cCurrentLine skip.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.
	 	finally:
		 	output stream strOutput close.

		 	cCurrentLine = ''.
	 	end finally.

 	end method.

 	method public logical generateHeader(input cClassName as character):

 		addText('class ').
 		addText(cClassName).
 		addText(' final:').

 		if not writeLine()
 		then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method public logical generateClosingStatement():

 		addText('end class.').

 		if not writeLine()
 		then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method private logical addEmptyLine():

 		cCurrentLine = ''.

 		if not writeLine()
 		then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method private void addSpace():
 		cCurrentLine = cCurrentLine + ' '.
 	end method.

 	method private void addTab(input iNumberOfTabs as integer):

 		define variable icountTabs		as integer	no-undo.
 		define variable icountSpaces	as integer	no-undo.

 		do icountTabs = 1 to iNumberOfTabs:
 			do icountSpaces = 1 to TabLength:
 				addSpace().
 			end.
 		end. /* do icountTabs = 1 to iNumberOfTabs */

 	end method.

 	method public logical generateProperty(input PropName as character):

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(1).
	 	addText('define public static property ').
	 	addText(PropName).
	 	addText(' as character ').
	 	addText('no-undo initial ').
	 	addText(quoter(PropName)).

	 	if not writeLine()
	 	then return false.

	 	addTab(2).
	 	addText('get.').

	 	if not writeLine()
	 	then return false.

	 	addTab(2).
	 	addText('private set.').

	 	if not writeLine()
	 	then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

end class.
