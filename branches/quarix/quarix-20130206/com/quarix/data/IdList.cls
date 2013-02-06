
/*------------------------------------------------------------------------
   File        : IdList
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Adam
   Created     : Mon Jul 04 11:53:00 EEST 2011
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

class com.quarix.data.IdList
    inherits com.quarix.base.BaseObject
    implements com.quarix.base.iDisposable:

	define protected property hbttHandle as handle no-undo
		get.
		set.

	method public void Clear().

		if valid-handle(hbttHandle)
		then hbttHandle:empty-temp-table().

	end method.

	method public handle getTableHandle():

		if valid-handle(hbttHandle)
		then return hbttHandle:table-handle.

		return ?.

	end method.

	method public void setTableHandle(input phbttHandle as handle):

		define variable htt as handle no-undo.

		if valid-handle(hbttHandle)
		then do:
			hbttHandle:empty-temp-table().

			htt = hbttHandle:table-handle.

			delete object hbttHandle no-error.

			delete object htt no-error.

			if not valid-handle(hbttHandle)
			then hbttHandle = ?.
		end.

		if valid-handle(phbttHandle)
		then hbttHandle = phbttHandle.

	end method.

	method private logical SetBufferFieldValue(input hBufferField as handle, input cFieldValue as character):

		if not valid-handle(hBufferField)
		then return false.

		if cFieldValue = '?':U
		then hBufferField:buffer-value = ?.

		case hBufferField:data-type:

			when 'integer':U
			then hBufferField:buffer-value = integer(cFieldValue).

			when 'decimal':U
			then hBufferField:buffer-value = decimal(cFieldValue).

			otherwise
				hBufferField:buffer-value = cFieldValue.

		end case. /* case hBufferField:data-type*/

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	method private integer GetLastIdListOrder():

		define variable hQuery			as handle	no-undo.
		define variable lOk				as logical	no-undo.
		define variable iIdListOrder	as integer	no-undo.
		define variable hField			as handle	no-undo.

		assign hField = hbttHandle:buffer-field ('IdListOrder':u) no-error.

		if not valid-handle(hField)
		then return 0.

		create query hQuery.

		hQuery:set-buffers (hbttHandle).

		lOk = hQuery:query-prepare (substitute('for each &1 no-lock by &1.IdListOrder descending':u, hbttHandle:name)).

		if not lOk
		then do:
			delete object hQuery no-error.

			return ?.
		end.

		hQuery:query-open ().

		hQuery:get-first ().

		if not hQuery:query-off-end
		then iIdListOrder = hbttHandle:buffer-field ('IdListOrder':u):buffer-value ().

		hQuery:query-close ().

		delete object hQuery no-error.

		return iIdListOrder.

		catch appError as Progress.Lang.Error :

			delete object hQuery no-error.

			ThrowError(input appError).
			delete object appError.
			return ?.
		end catch.

	end method.

	method public rowid AddRecord(input cKeyFields as character, input cKeyValues as character):

		define variable hQuery			as handle    	no-undo.
		define variable cQuery			as character	no-undo.
		define variable lOk				as logical   	no-undo.
		define variable iNum			as integer		no-undo.
		define variable cFieldName		as character	no-undo.
		define variable cFieldValue		as character	no-undo.
		define variable cWhere			as character	no-undo.
		define variable cWhereClause	as character	no-undo.
		define variable lAvailable		as logical		no-undo.
		define variable hBufferField	as handle		no-undo.
		define variable rRowid			as rowid		no-undo.
		define variable iIdListOrder	as integer		no-undo.
		define variable cKey			as longchar		no-undo.
		define variable rKey			as raw			no-undo.
		define variable cHashCode		as character	no-undo.

		if not valid-handle(hbttHandle)	or
			cKeyFields = '':U			or
			cKeyFields = ?				or
			num-entries(cKeyFields, '|':U) <> num-entries(cKeyValues, '|':U)
		then return rRowid.

		do iNum = 1 to num-entries(cKeyFields, '|':U):

			assign
				cFieldName	= entry(iNum, cKeyFields, '|':U)
				cFieldName	= trim(cFieldName)
				cFieldName	= substitute('&1.&2':U, hbttHandle:name, cFieldName).

			cFieldValue	= entry(iNum, cKeyValues, '|':U).

			if cFieldValue = '?':U
			then cWhere = substitute('&1 = ?':U, cFieldName).
			else cWhere = substitute('&1 = &2':U, cFieldName, quoter(cFieldValue)).

			cWhereClause =	if (cWhereClause = '':U or cWhereClause = ?)
							then cWhere
							else cWhereClause + ' and ':U + cWhere.

		end. /* do iNum = 1 to num-entries(cKeyFields, '|':U) */

		cWhereClause = '(':U + cWhereClause + ')':U.

		cQuery = substitute('where &1':U, cWhereClause).

		hbttHandle:find-first (cQuery, no-lock) no-error.

		lAvailable = hbttHandle:available.
/*
		cQuery = substitute('for each &1 no-lock where &2':U, hbttHandle:name, cWhereClause).

		create query hQuery.

		hQuery:set-buffers (hbttHandle).

		lOk = hQuery:query-prepare (cQuery).

		if not lOk
		then do:
			delete object hQuery no-error.

			return rRowid.
		end.

		hQuery:query-open().

		hQuery:get-first().

		lAvailable = not hQuery:query-off-end.

		hQuery:query-close ().

		delete object hQuery no-error.
*/
		if not lAvailable
		then do:
			iIdListOrder = GetLastIdListOrder().

			if iIdListOrder = ?
			then return rRowid.

			iIdListOrder = iIdListOrder + 1.

			do transaction
				on error undo, throw
				on stop undo, leave:

				hbttHandle:buffer-create().

				assign hBufferField = hbttHandle:buffer-field ('IdListOrder':U) no-error.

				if valid-handle(hBufferField)
				then SetBufferFieldValue(hBufferField, string(iIdListOrder)).

				do iNum = 1 to num-entries(cKeyFields, '|':U):

					assign
						cFieldName		= entry(iNum, cKeyFields, '|':U)
						cFieldName		= trim(cFieldName)
						cFieldValue		= entry(iNum, cKeyValues, '|':U)
						hBufferField	= hbttHandle:buffer-field(cFieldName).

					SetBufferFieldValue(hBufferField, cFieldValue).

					cKey =	if Util:IsEmpty(cKey)
							then cFieldValue
							else cKey + chr(3) + cFieldValue.

				end. /* do iNum = 1 to num-entries(cKeyFields, '|':U) */

				assign hBufferField = hbttHandle:buffer-field ('HashCode':U) no-error.

				if valid-handle(hBufferField)
				then do:
					rKey = md5-digest(cKey, 'IDLIST':u).

					cHashCode = hex-encode(rKey).

					SetBufferFieldValue(hBufferField, cHashCode).
				end.

			end. /* do transaction */

			rRowid = hbttHandle:rowid.

		end. /* if not lAvailable */

		return rRowid.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return rRowid.
		end catch.

	end method.

	method private void DisplayBuffer():

		define variable iNum			as integer		no-undo.
		define variable hBufferField	as handle		no-undo.
		define variable cMessage		as character	no-undo.
		define variable cBufferValue	as character	no-undo.
		define variable cMsg			as character	no-undo.

		if not valid-handle(hbttHandle) or
			not hbttHandle:available
		then return.

		do iNum = 1 to hbttHandle:num-fields:

			hBufferField = hbttHandle:buffer-field (iNum).

			cBufferValue = string(hBufferField:buffer-value ()).

			cMsg = hBufferField:name + ': ' + cBufferValue.

			cMessage =	if (cMessage = '':U or cMessage = ?)
						then cMsg
						else cMessage + chr(10) + cMsg.

		end. /* do iNum = 1 to hbttHandle:num-fields */

		cMessage = chr(10) + cMessage + chr(10).

		message
			hbttHandle:name skip
			cMessage.

	end method.

	method public void dumpToLog():

		define variable hQuery	as handle		no-undo.
		define variable cQuery	as character	no-undo.
		define variable lOk		as logical		no-undo.

		if not valid-handle(hbttHandle)
		then return.

		create query hQuery.

		hQuery:set-buffers(hbttHandle).

		cQuery = substitute('for each &1 no-lock':U, hbttHandle:name).

		lOk = hQuery:query-prepare (cQuery).

		if not lOk
		then do:
			delete object hQuery no-error.

			return.
		end.

		hQuery:query-open().

		hQuery:get-first().

		do while not hQuery:query-off-end:

			DisplayBuffer().

			hQuery:get-next ().
		end.

		hQuery:query-close ().

		delete object hQuery no-error.

	end method.

	destructor public idlist():

		define variable htt as handle no-undo.

		if valid-handle(hbttHandle)
		then do:
			hbttHandle:table-handle:tracking-changes = false.

			hbttHandle:empty-temp-table().

			htt = hbttHandle:table-handle.

			delete object hbttHandle no-error.

			delete object htt no-error.

			if not valid-handle(hbttHandle)
			then hbttHandle = ?.
		end.

	end destructor.

	method public void DumpToXml():

		define variable cLogDirectory		as character	no-undo.
    	define variable cLogFileName		as character	no-undo.
    	define variable cFullLogFileName	as character	no-undo.
    	define variable iNum				as integer		no-undo.
    	define variable cFileName			as character	no-undo.

    	assign
			cLogDirectory	= Util:GetLogDirectory()
			cFileName		= hbttHandle:name.

		if cFileName = '' or
			cFileName = ? or
			cLogDirectory = '' or
			cLogDirectory = ?
    	then return.

    	assign
			cLogFileName		= cFileName + '.xml':U
    		cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	do while search(cFullLogFileName) <> ?:

    		iNum = iNum + 1.

    		assign
				cLogFileName		= cFileName + '_':U + string(iNum) + '.xml':U
    			cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	end. /* do while search(cFullLogFileName) = ? */

    	hbttHandle:write-xml('file':U, cFullLogFileName, yes).

	end method.

	method public logical IdListHasData():

		if not valid-handle(hbttHandle)
		then return false.

		hbttHandle:find-first ('':U, no-lock) no-error.

		if hbttHandle:available
		then return true.

		return false.

	end method.

end class.
