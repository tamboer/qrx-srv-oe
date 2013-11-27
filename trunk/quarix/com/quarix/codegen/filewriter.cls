/*------------------------------------------------------------------------
   File        : filewriter.cls
   Purpose     :
   Syntax      :
   Description :handles file writing of specific buffer objects
   Author(s)   : adam
   Created     : Fri Nov 14 11:03:02 EEST 2013
   Notes       :
 ----------------------------------------------------------------------*/

 class com.quarix.codegen.filewriter final:

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

 		if cClassName = '' or
			 cClassName = ?
		then return false.

		addText(substitute('class &1 inherits com.quarix.data.tableobject final:', cClassName)).

 		if not writeLine()
 		then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method public logical generateConstructor(input cClassName as character, input cObjName as character):

	 	if cClassName = ''	or
	 		cClassName = ?	or
		 	cObjName = ''	or
		 	cObjName = ?
		then return false.

	 	if not addEmptyLine()
	 	then return false.

 		addTab(1).
 		addText('constructor static ').
	 	addText(cClassName).
	 	addText('():').

	 	if not writeLine()
	 	then return false.

	 	addTab(1).
 		addText('end constructor.').

	 	if not writeLine()
	 	then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method public logical generateClosingStatement():

	 	if not addEmptyLine()
	 	then return false.

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

 	method public logical generateProperty(input PropName as character, input InitialValue as character):

	 	if PropName = ''		or
		 	PropName = ?		or
	 		InitialValue = ''	or
		 	InitialValue = ?
		then return false.

 		if not addEmptyLine()
 		then return false.

 		addTab(1).
 		addText('define public static property ').
 		addText(PropName).
 		addText(' as character ').
 		addText('no-undo initial ').
 		addText(quoter(InitialValue)).

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

 	method public logical generateProperty(input PropName as character):

	 	generateProperty(input PropName, input PropName).

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method private logical generatePropertyField(input PropName as character, input cType as character, input cObjectName as character):

	 	if PropName = ''	or
	 		PropName = ?	or
		 	cType = ''		or
		 	cType = ?
	 	then return false.

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(1).
	 	addText('define public static property ').
	 	addText(PropName).
	 	addText(substitute(' as &1 ', cType)).
	 	addText('no-undo').

	 	if not writeLine()
	 	then return false.

	 	addTab(2).
	 	addText('get:').

	 	if not writeLine()
	 	then return false.

	 	addTab(3).
	 	addText(substitute('if not valid-object(&1) then &1 = new &2("&1", "&3").', PropName, cType, cObjectName)).

	 	if not writeLine()
	 	then return false.

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(3).
	 	addText(substitute('return &1.', PropName)).

	 	if not writeLine()
	 	then return false.

	 	addTab(2).
	 	addText('end.').

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

 	method public logical generatePropertyDbField(input PropName as character, input cObjectName as character):

 		return generatePropertyField(input PropName, input 'com.quarix.data.dbfield', input cObjectName).

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method public logical generatePropertyTtField(input PropName as character, input cObjectName as character):

 		return generatePropertyField(input PropName, input 'com.quarix.data.ttfield', input cObjectName).

 		catch appError as Progress.Lang.Error :
 			delete object appError.
 			return false.
 		end catch.

 	end method.

 	method public logical generateDestructorHeader(input cClassName as character):

	 	if cClassName = '' or
	 		cClassName = ?
	 	then return false.

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(1).
	 	addText(substitute('destructor public &1():', cClassName)).

	 	if not writeLine()
	 	then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method public logical generateDestructorClosingStatement():

	 	addTab(1).
 		addText('end destructor.').

 		if not writeLine()
 		then return false.

 		return true.

 		catch appError as Progress.Lang.Error :
 			delete object appError.
 			return false.
 		end catch.

 	end method.

 	method public logical generateDeleteProperty(input PropName as character):

	 	addTab(2).
	 	addText(substitute('delete object &1 no-error.', PropName)).

	 	if not writeLine()
	 	then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

end class.
