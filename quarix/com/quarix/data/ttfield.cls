
 /*------------------------------------------------------------------------
    File        : ttfield
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Adam
    Created     : Thu Jun 30 14:58:01 EEST 2011
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

routine-level on error undo, throw.

using Progress.Lang.*.

class com.quarix.data.ttfield inherits com.quarix.data.tablefield final:

	constructor	private ttfield():
	end constructor.

	constructor	private ttfield(input cFieldName as character, input ottTableObject as com.quarix.data.tttableobject):

		if cFieldName = ? or
			cFieldName = '' or
			not valid-object(ottTableObject)
		then return error.

		assign
			FieldName		= cFieldName
			oTableObject	= ottTableObject.

	end constructor.

	method public static com.quarix.data.ttfield newField(input cFieldName as character, input ottTableObject as com.quarix.data.tttableobject):
		return new com.quarix.data.ttfield(input cFieldName, input ottTableObject).
	end method.

end class.
