/*------------------------------------------------------------------------
   File        : filewriter.cls
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : adam
   Created     : Fri Nov 14 11:03:02 EEST 2013
   Notes       :
   License     :
    This file is part of the QRX-SRV-OE software framework.
    Copyright (C) 2011, SC Yonder SRL (http://www.tss-yonder.com)

    The QRX-SRV-OE software framework is free software; you can redistribute
    it and/or modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either version 2.1
    of the License, or (at your option) any later version.

    The QRX-SRV-OE software framework is distributed in the hope that it will
    be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
    General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with the QRX-SRV-OE software framework; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA or on the internet at the following address:
    http://www.gnu.org/licenses/lgpl-2.1.txt
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

 		define variable icountTabs   as integer	no-undo.
 		define variable icountSpaces	as integer	no-undo.

 		do icountTabs = 1 to iNumberOfTabs:
 			do icountSpaces = 1 to TabLength:
 				addSpace().
 			end.
 		end. /* do icountTabs = 1 to iNumberOfTabs */

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

 	method private logical generateHeader(input cClassName as character, input cBaseClass as character):

 		if cClassName = '' or
 			cClassName = ?
 		then return false.

 		addText(substitute('class &1 inherits &2 final:', cClassName, cBaseClass)).

 		if not writeLine()
 		then return false.

 		return true.

 		catch appError as Progress.Lang.Error :
 			delete object appError.
 			return false.
 		end catch.

 	end method.

 	method public logical generateDbHeader(input cClassName as character):

	 	return generateHeader(input cClassName, input 'com.quarix.data.dbtableobject').

 	end method.

 	method public logical generatettHeader(input cClassName as character):

 		return generateHeader(input cClassName, input 'com.quarix.data.tttableobject').

 	end method.

 	method public logical generatePropertyField(input PropName as character, input cType as character):

 		if PropName = ''	or
 			PropName = ?	or
 			cType = ''		or
 			cType = ?
 		then return false.

 		if not addEmptyLine()
 		then return false.

 		addTab(1).
 		addText('define private static property ').
 		addText(PropName).
 		addText(substitute(' as &1 ', cType)).
 		addText('no-undo').

 		if not writeLine()
 		then return false.

 		addTab(2).
 		addText('private get:').

 		if not writeLine()
 		then return false.

 		addTab(3).
 		addText(substitute('if not valid-object(&1) then &1 = new &2().', PropName, cType)).

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

 	method public logical generateProperty(input PropName as character, input InitialValue as character):

 		if PropName = '' or
 			PropName = ?
 		then return false.

 		if not addEmptyLine()
 		then return false.

 		addTab(1).
 		addText(substitute('define public static property &1 as character no-undo', PropName)).

 		if InitialValue <> '' and
 			InitialValue <> ?
 		then do:
 			addText(' initial ').
 			addText(quoter(InitialValue)).
 		end.

 		if not writeLine()
 		then return false.

 		addTab(2).
 		addText('get:').

 		if not writeLine()
 		then return false.

 		addTab(3).
 		addText('return Instance:TableObjectName.').

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

 	method public logical generateScopedDefine(input PropName as character, input PropIdx as integer, input PropDbPos as integer):

	 	if PropName = ''	or
	 		PropName = ?	or
	 		PropIdx = ?		or
	 		PropIdx = 0
	 		then return false.

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(1).
	 	addText(substitute('&&scoped-define idx&1 &2', PropName, string(PropIdx))).

	 	if not writeLine()
	 	then return false.

	 	addTab(1).
	 	addText(substitute('&&scoped-define idx&1DbPos &2', PropName, string(PropDbPos))).

	 	if not writeLine()
	 	then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method public logical generateScopedDefine(input PropName as character, input PropIdx as integer):

 		if PropName = ''	or
 			PropName = ?	or
		 	PropIdx = ?		or
		 	PropIdx = 0
 		then return false.

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(1).
	 	addText(substitute('&&scoped-define idx&1 &2', PropName, string(PropIdx))).

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

 	method private logical generatePropertyFieldInstance(input PropName as character, input cType as character):

 		if PropName = ''	or
 			PropName = ?	or
		 	cType = ''		or
		 	cType = ?
		then return false.

 		if not addEmptyLine()
 		then return false.

 		addTab(1).
 		addText(substitute('define public static property &1 as &2 no-undo', PropName, cType)).

 		if not writeLine()
 		then return false.

 		addTab(2).
 		addText('get:').

 		if not writeLine()
 		then return false.

 		addTab(3).
 		addText(substitute('return Instance:FieldInstanceList#[~{&&idx&1~}].', PropName)).

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

 	method public logical generatePropertyDbField(input PropName as character):

 		return generatePropertyFieldInstance(input PropName, input 'com.quarix.data.dbfield').

 		catch appError as Progress.Lang.Error :
 			delete object appError.
 			return false.
 		end catch.

 	end method.

 	method public logical generatePropertyTtField(input PropName as character):

 		return generatePropertyFieldInstance(input PropName, input 'com.quarix.data.ttfield').

 		catch appError as Progress.Lang.Error :
 			delete object appError.
 			return false.
 		end catch.

 	end method.

 	method public logical generateConstructorHeader(input cClassName as character, input cTableName as character):

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(1).
	 	addText('constructor private ').
	 	addText(cClassName).
	 	addText('():').

	 	if not writeLine()
	 	then return false.

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(2).
	 	addText(substitute('TableObjectName = "&1".', cTableName)).

	 	if not writeLine()
	 	then return false.

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(2).
	 	addText('extent(FieldInstanceList#) = ~{&idxNumFields~}.').

	 	if not writeLine()
	 	then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

 	method public logical generateConstructorClosingStatement():

 		if not addEmptyLine()
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

 	method private logical generateFieldInstance(input PropName as character, input cType as character):

 		if PropName = ''	or
 			PropName = ?	or
 			cType = ''		or
 			cType = ?
 		then return false.

 		if not addEmptyLine()
 		then return false.

	 	addTab(2).

 		if cType = 'com.quarix.data.dbfield'
 		then
		 	addText(substitute('FieldInstanceList#[~{&&idx&1~}] = &2:newField(input "&1", input ~{&&idx&1DbPos~}, input this-object).', PropName, cType)).
		else
 			addText(substitute('FieldInstanceList#[~{&&idx&1~}] = &2:newField(input "&1", input this-object).', PropName, cType)).

 		if not writeLine()
 		then return false.

 		addTab(2).
 		addText(substitute('if not valid-object(FieldInstanceList#[~{&&idx&1~}]) then return error.', PropName)).

 		if not writeLine()
 		then return false.

 		return true.

 		catch appError as Progress.Lang.Error :
 			delete object appError.
 			return false.
 		end catch.

 	end method.

 	method public logical generateDbFieldInstance(input PropName as character):

 		return generateFieldInstance(input PropName, input 'com.quarix.data.dbfield').

 		catch appError as Progress.Lang.Error :
 			delete object appError.
 			return false.
 		end catch.

 	end method.

 	method public logical generateTtFieldInstance(input PropName as character):

 		return generateFieldInstance(input PropName, input 'com.quarix.data.ttfield').

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
 		addText(substitute('delete object FieldInstanceList#[~{&&idx&1~}] no-error.', PropName)).

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

 	method public logical generateNumberOfFields():

	 	if not addEmptyLine()
	 	then return false.

	 	addTab(1).
	 	addText('method public static integer NumberOfFields():').

	 	if not writeLine()
	 	then return false.

	 	addTab(2).
	 	addText('return Instance:FieldCount().').

	 	if not writeLine()
	 	then return false.

	 	addTab(1).
	 	addText('end method.').

	 	if not writeLine()
	 	then return false.

	 	return true.

	 	catch appError as Progress.Lang.Error :
	 		delete object appError.
	 		return false.
	 	end catch.

 	end method.

end class.
