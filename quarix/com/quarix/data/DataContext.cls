/*------------------------------------------------------------------------
   File        : DataContext
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Andriuhan
   Created     : Thu Dec 02 13:38:48 EET 2010
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
using com.quarix.base.BaseObject.
using com.quarix.data.*.

class com.quarix.data.DataContext
    inherits BaseObject
    implements com.quarix.base.iDisposable:

    {com/quarix/data/dscontext.i &scope=private}

    define private variable lastSortOrder	as integer	no-undo.
    define private variable iNumRec			as integer	no-undo.

    define public property dsContextHandle as handle no-undo
      get:
      	dsContextHandle = dataset dscontext:handle.

      	return dsContextHandle.
      end get.
      private set.

    define private property allRecords	as logical		no-undo init false
      get.
      set.

    constructor public DataContext():
        super().
    end constructor.

    /* DataContext constructor that takes a dsContext:write-json() result as input */
    constructor public DataContext(input icJson as longchar):
      super().
      loadContextFromJSON(icJson).
    end constructor.

    method public void loadContextFromJSON(input icJson as longchar):
      if icJson <> "" and icJson <> ? and icJson <>  '~{~"dsContext~":~{~}~}' then /* This could be the case if there are no query parameters */
        dataset dsContext:READ-JSON ("longchar", icJson).
      else
        dataset dsContext:EMPTY-DATASET ( ).
    end method.


    method public void ClearFilters():
        empty temp-table ttFilter.
    end method.

    method public void ClearSort():
        empty temp-table ttSort.
        lastSortOrder = 0.
    end method.

    method public final character getDbRowid (hBuf as handle):

        if valid-handle(hBuf) and hBuf:type eq 'buffer':u then
            return getDbRowid (hBuf:table, string(hBuf:rowid)).

        return ?.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.


    method public character getDbRowid (bufName as character, bufRowId as character):

        define buffer ttRowId for ttRowId.

        for each ttRowid
           where ttRowId.ttName  eq bufName
             and ttRowId.ttRowId eq bufRowId:

            return ttRowId.dbRowId.
        end.

        return ?.

    end method.


    method public character getFieldFilterCondition (fieldName as character, fieldOper as character, fieldVal as character):

        if fieldOper eq 'between':u then
            return substitute('&1 ge &2 and &1 lt &3':u, fieldName,
                quoter(entry(1, fieldVal, chr(1))),
                quoter(entry(2, fieldVal, chr(1)))).
        else
            return substitute('&1 &2 &3':u, fieldName, fieldOper, quoter(fieldVal)).

    end method.


    method public final character getFilterArray(tableName as character) :

        define buffer   bufFilter  for ttFilter.

        define variable filterArray   as character no-undo.
        define variable numFilters    as integer   no-undo.
        define variable i             as integer   no-undo.


        for each bufFilter
            where bufFilter.tableName = tableName
            and bufFilter.FieldName <> ?
                on error undo, throw:

            assign
                filterArray = filterArray + quoter(bufFilter.operName) + ', ' + quoter(bufFilter.FieldName) + ', '
                            + quoter(bufFilter.fieldValue) + ', '
                numFilters  = numFilters + 1.

        end.

        if numFilters > 0 then
            filterArray = substr(filterArray, 1, length(filterArray) - 2).

        do i = 1 to numFilters - 1:
            filterArray = quoter("AND") + ", " + filterArray.
        end.

        return filterArray.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method public final character getPropertyArray() :
        define buffer bufProperty for ttProperty.

        define variable propArray as character no-undo.

        for each bufProperty
            on error undo, throw:

            if Util:IsEmpty(bufProperty.propertyName) or
               Util:IsEmpty(bufProperty.propertyValue)
            then next.

            propArray = propArray + substitute ('&3"name": "&1", "value": "&2"&4',bufProperty.propertyName,bufProperty.propertyValue,chr(123),chr(125)) + ', '.
        end.

        return trim(trim(propArray),',').

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.
    end method.

   method public final character getSortArray(tableName as character) :
      define buffer   bufSort  for ttSort.

      define variable sortArray   as character no-undo.

        for each bufSort
            where bufSort.tableName = tableName
                on error undo, throw:

         assign sortArray = sortArray + quoter(substitute('&1&2', if bufSort.descOrder then '-' else '',bufSort.fieldName)) + ', '.

        end.

        return trim(trim(sortArray),',').

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

   method public final character getLookupFilter (lookupTable as character, bufferName as character, xmlFilter as memptr):

        define variable qryString as character no-undo.
        define variable fieldName as character no-undo.
        define variable ttFilter  as handle    no-undo.

        define buffer bufFilter  for ttFilter.


        /* add server-side filters that might have been added */
        for each bufFilter
            where bufFilter.tableName eq tableName
            and bufFilter.dbFieldName begins substitute('&1.':u, bufferName)
                on error undo, throw:

            /* skip table join conditions */
            if bufFilter.fieldName eq ? and logical(bufFilter.operName) then
                next.

            qryString = substitute ('&1 and &2', qryString,
                if bufFilter.fieldName eq ? then
                    bufFilter.fieldValue
                else
                    getFieldFilterCondition(bufFilter.dbFieldName, bufFilter.operName, bufFilter.fieldValue)).
        end.

        /* filter request sent from client */
        if xmlFilter eq ? then
            return trim(qryString).

        if get-size(xmlFilter) gt 0 then do:

            create temp-table ttFilter .

            ttFilter:read-xml('memptr':u, xmlFilter, 'empty':u, ?, ?).

            do while ttFilter:has-records
                on error undo, throw:

                ttFilter:default-buffer-handle:find-first().

                /* apply filters only on given temp-table */
/*                fieldName = dataAccess:GetPhysicalFieldName(substitute('&1.&2':u, lookupTable, ttFilter::fld)).*/

                if fieldName ne ? and entry(1, fieldName, '.':u) eq bufferName then
                    qryString = substitute ('&1 and &2', qryString, getFieldFilterCondition(fieldName, ttFilter::op, ttFilter::val)).

                ttFilter:default-buffer-handle:buffer-delete().

            end.

            set-size(xmlFilter) = 0.
        end.

        return trim(qryString).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.
        finally:
            if valid-handle(ttFilter) then
                delete object ttFilter.
        end finally.

    end method.


    method public final character getNewPosition (tableName as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName:
            return ttInfo.ttRowPos.
        end.

        return ?.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method public final character getUseIndex (tableName as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName:
            return ttInfo.useIndex.
        end.

        return ?.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method public final integer getBatchSize (tableName as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName:
            return ttInfo.batchSize.
        end.

        return 0.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.


    method public final logical getFirstBatch (tableName as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo
           where ttInfo.ttName eq tableName:

            return ttInfo.firstBatch.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public final logical getLastBatch (tableName as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo where
            ttInfo.ttName eq tableName:

            return ttInfo.lastBatch.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public final character getTableSort (tableName as character):

        define variable sortBy    as character no-undo.

        define buffer   ttSort    for ttSort.


        for each ttSort
            where ttSort.tableName eq tableName
            by ttSort.sortOrder
                on error undo, throw:

            sortBy = substitute('&1 by &2 &3':u, sortBy, ttSort.fieldName, if ttSort.descOrder then 'descending':u else '':u).
        end.

        return sortBy.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.


    method public logical RemoveFilter (tableName as character, fieldName as character):

        define buffer bufFilter for ttFilter.

        if Util:IsEmpty(fieldName) or
        	Util:IsEmpty(tableName)
        then return false.

		for each bufFilter
			where bufFilter.tableName	= tableName
			  and bufFilter.fieldName	= fieldName
			on error undo, throw:

            delete bufFilter.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public logical RemoveTableFilters (tableName as character):

        define buffer   bufFilter  for ttFilter.


        if Util:IsEmpty(tableName) then
            return false.

        for each bufFilter
            where bufFilter.tableName eq tableName
                on error undo, throw:

            delete bufFilter.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public logical HasSortOnField(tableName as character, fieldName as character):

    	define buffer bufSort for ttSort.

    	if Util:IsEmpty(fieldName) or
    		Util:IsEmpty(tableName)
    	then return false.

    	find first bufSort
			where bufSort.tableName eq tableName
              and bufSort.fieldName eq fieldName
            no-lock no-error.

        return available(bufSort).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public logical RemoveSort (tableName as character, fieldName as character):

        define buffer bufSort for ttSort.


        if Util:IsEmpty(fieldName) or Util:IsEmpty(tableName) then
            return false.

        for each bufSort
            where bufSort.tableName eq tableName
            and bufSort.fieldName eq fieldName
                on error undo, throw:

            delete bufSort.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public logical RemoveTableSort (tableName as character):

        define buffer bufSort for ttSort.

        if Util:IsEmpty(tableName) then
            return false.

        for each bufSort
            where bufSort.tableName eq tableName
                on error undo, throw:

            delete bufSort.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical CreateRowId(input tableName as character, input ttRowId as character, input dbRowId as character):

    	find first ttRowId
    		where ttRowId.ttName	= tableName
    		  and ttRowId.ttRowId	= ttRowId
    		no-lock no-error.

    	if not available(ttRowId)
    	then do:
    		create ttRowId.

    		iNumRec = iNumRec + 1.
    	end.

    	assign
    		ttRowId.ttName	= tableName
    		ttRowId.ttRowId	= ttRowId
    		ttRowId.dbRowId	= dbRowId
    		ttRowId.NumRec	= iNumRec.

    	return true.

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical CopyRowId(input targetTable as character, input tableName as character, input dataset-handle hdscontext):

    	define variable hBufttRowId	as handle		no-undo.
    	define variable hQuery		as handle		no-undo.
    	define variable cQuery		as character	no-undo.
    	define variable lOk			as logical		no-undo.

    	if not valid-handle(hdscontext)
    	then return false.

    	hBufttRowId = hdscontext:get-buffer-handle('ttRowId') no-error.

    	if not valid-handle(hBufttRowId)
    	then return false.

    	create query hQuery.

    	hQuery:set-buffers(hBufttRowId).

    	cQuery = substitute('for each &1 no-lock by ttRowId.NumRec', hBufttRowId:name).

    	lOk = hQuery:query-prepare (cQuery).

    	if not lOk
    	then do:
    		delete object hQuery no-error.

    		return false.
    	end.

    	hQuery:query-open().

    	hQuery:get-first().

    	do transaction on error undo, throw:

            do while not hQuery:query-off-end:

                if not CreateRowId(tableName, hBufttRowId:buffer-field('ttRowId':U):buffer-value, hBufttRowId:buffer-field('dbRowId':U):buffer-value)
                then do:
                    hQuery:query-close ().

                    delete object hQuery no-error.

                    undo, return false.
                end.

                hQuery:get-next().

            end. /* do while not hQuery:query-off-end */

        end. /* do transaction on error undo, throw */

    	hQuery:query-close ().

    	delete object hQuery no-error.

    	return true.

    	catch appError as Progress.Lang.Error :

    	    hQuery:query-close ().

            delete object hQuery no-error.

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
        finally:
        	delete object hdscontext no-error.
        end finally.

    end method.

    method public final logical setBatchInfo (input tableName as character, input firstBatch as logical, input lastBatch as logical):

        define buffer ttInfo for ttInfo.


        for each ttInfo where ttInfo.ttName eq tableName
            on error undo, throw:
            assign
                ttInfo.firstBatch = firstBatch
                ttInfo.lastBatch  = lastBatch.

            return true.
        end.

        create ttInfo.
        assign
            ttInfo.ttName     = tableName
            ttInfo.firstBatch = firstBatch
            ttInfo.lastBatch  = lastBatch.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public final void setDbRowid (hBuf as handle, dbRowid as character):

        define variable hRowidField as handle no-undo.

        for each ttRowid
            where ttRowId.ttName  eq hBuf:table
            and ttRowId.ttRowId eq string(hBuf:rowid)
                on error undo, throw:

            ttRowId.dbRowId = dbRowid.
            return.
        end.

        find last ttRowid
            where ttRowId.ttName  eq hBuf:table
            use-index idxNumRec no-lock no-error.

        if available ttRowid then
        iNumRec = ttRowid.NumRec.

        iNumRec = iNumRec + 1.

        create ttRowId.

        assign
            ttRowId.dbRowId = dbRowid
            ttRowId.ttName  = hBuf:table
            ttRowId.ttRowId = string(hBuf:rowid)
            ttRowId.NumRec	= iNumRec.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.

    end method.


    method public logical SetFilter (tableName as character, fieldName as character, operName as character, fieldValue as character, dbFieldName as character):

		define variable cOperAlias as character no-undo.

        define buffer bufFilter for ttFilter.

        if Util:IsEmpty(tableName)
        then return false.

       	cOperAlias = GetOperatorAlias(operName).

		for each bufFilter
			where bufFilter.tableName	= tableName
			  and bufFilter.fieldName	= fieldName
			  and (bufFilter.operName	= operName
			   or bufFilter.operName	= cOperAlias)
			on error undo, throw:

            assign
                bufFilter.dbFieldName = dbFieldName
                bufFilter.fieldValue  = fieldValue.

            return true.
        end.

        create bufFilter.

        assign
            bufFilter.dbFieldName	= dbFieldName
            bufFilter.tableName		= tableName
            bufFilter.fieldName		= fieldName
            bufFilter.operName		= operName
            bufFilter.fieldValue	= fieldValue.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public final logical setNewPosition (tableName as character, newRowid as character):

        define buffer ttInfo for ttInfo.


        newRowid = Util:Nvl(getDbRowid(tableName, newRowid), newRowid).

        for each ttInfo where ttInfo.ttName eq tableName
            on error undo, throw:

            ttInfo.ttRowPos = newRowid.
            return true.
        end.

        create ttInfo.
        assign
            ttInfo.ttName   = tableName
            ttInfo.ttRowPos = newRowid.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.

    method public final logical setUseIndex (tableName as character, useIndex as character):

        define buffer ttInfo for ttInfo.


        if Util:IsEmpty(useIndex) then return false.

        for each ttInfo where ttInfo.ttName eq tableName
            on error undo, throw:

            ttInfo.useIndex = useIndex.
            return true.
        end.

        create ttInfo.
        assign
            ttInfo.ttName   = tableName
            ttInfo.useIndex = useIndex.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.

    method public final logical setBatchSize (tableName as character, batchSize as integer):

        define buffer ttInfo for ttInfo.


        if batchSize eq ? then return false.

        for each ttInfo where ttInfo.ttName eq tableName
            on error undo, throw:

            ttInfo.batchSize = batchSize.
            return true.
        end.

        create ttInfo.
        assign
            ttInfo.ttName    = tableName
            ttInfo.batchSize = batchSize.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical SetSort (tableName as character, fieldName as character, descendingSort as logical):

        define buffer bufSort for ttSort.


        if Util:IsEmpty(fieldName)or Util:IsEmpty(tableName) then
            return false.

        for each bufSort
            where bufSort.tableName eq tableName
            and bufSort.fieldName eq fieldName
                on error undo, throw:

            bufSort.descOrder = descendingSort.
            return true.
        end.

        create bufSort.
        assign
            lastSortOrder     = lastSortOrder + 1
            bufSort.tableName = tableName
            bufSort.fieldName = fieldName
            bufSort.descOrder = descendingSort
            bufSort.sortOrder = lastSortOrder.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.

    method public final character GetPrivateData ():
        return string(temp-table ttRowId:handle).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.
    end method.


    method public final character GetFilterOperator (tableName as character, fieldName as character, fieldValue as character):

        define buffer bufFilter for ttFilter.

        for each bufFilter
           where bufFilter.tableName  eq tableName
             and bufFilter.fieldName  eq fieldName
             and bufFilter.fieldValue eq fieldValue:

            return bufFilter.operName.
        end.

        return ?.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method private character GetOperatorAlias(input operName as character):

    	case operName:

    		when 'ge'
    		then return '>='.

    		when '>='
    		then return 'ge'.

    		when 'gt'
    		then return '>'.

    		when '>'
    		then return 'gt'.

    		when 'le'
    		then return '<='.

    		when '<='
    		then return 'le'.

    		when 'lt'
    		then return '<'.

    		when '<'
    		then return 'lt'.

    		when 'ne'
    		then return '<>'.

    		when '<>'
    		then return 'ne'.

    		when 'eq'
    		then return '='.

    		when '='
    		then return 'eq'.

    		otherwise return operName.

    	end case. /* case operName */

    end method.

    method public final character GetFilterValue (tableName as character, fieldname as character, operName as character ):

		define buffer bufFilter for ttFilter.
		define variable cOperAlias as character no-undo.

		cOperAlias = GetOperatorAlias(operName).

        for each bufFilter
           where bufFilter.tableName  eq tableName
             and bufFilter.fieldName  eq fieldName
             and (bufFilter.operName  eq operName
               or bufFilter.operName  eq cOperAlias):

            return bufFilter.fieldValue.
        end.

        return ?.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method public logical AddWhereClause (tableName as character, bufferName as character, whereClause as character, isJoin as logical):

        if entry(1, whereClause, ' ':u) eq 'where':u then
            whereClause = trim(substring(whereClause, 6, -1)).

        return SetFilter(tableName, ?, string(isJoin), whereClause, substitute('&1.':u, bufferName)).
        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.

    method public void ClearProperties():

    	empty temp-table ttProperty.

    end method.

    method public void ClearProperty(input PropertyName as character):

    	find first ttProperty
    		where ttProperty.propertyName = PropertyName
    		no-lock no-error.

    	if available(ttProperty)
    	then delete ttProperty.

    end method.

    method public void SetProperty(input PropertyName as character, input PropertyValue as character):

    	find first ttProperty
    		where ttProperty.propertyName = PropertyName
    		no-lock no-error.

    	if not available(ttProperty)
    	then do:
    		create ttProperty.

	    	ttProperty.propertyName = PropertyName.
    	end.

	    ttProperty.propertyValue = PropertyValue.

	    release ttProperty.

    end method.

    method public character GetProperty(input PropertyName as character):

    	define variable cPropertyValue as character no-undo.

    	find first ttProperty
    		where ttProperty.propertyName = PropertyName
    		no-lock no-error.

    	if available(ttProperty)
    	then do:
    		cPropertyValue = ttProperty.propertyValue.

	    	release ttProperty.

    		return cPropertyValue.
    	end.

    	return ?.

    end method.

    method public logical PropertyAvailable(input PropertyName as character):

    	find first ttProperty
    		where ttProperty.propertyName = PropertyName
    		no-lock no-error.

    	if available(ttProperty)
    	then do:
	    	release ttProperty.

    		return true.
    	end.

    	return false.

    end method.

	method public logical PropertyAvailable():

		find first ttProperty
			no-lock no-error.

		if available(ttProperty)
		then do:
			release ttProperty.

			return true.
		end.

		return false.

	end method.

	method public void SetQueryName(input PropertyValue as character):

		SetProperty(input 'QueryName':U, input PropertyValue).

	end method.

	method public void SetIdListTmpHandle(input PropertyValue as handle):

		SetProperty(input 'IdListTmpHandle':U, input string(PropertyValue)).

	end method.

	method public character GetQueryName():

		return GetProperty(input 'QueryName':U).

	end method.

	method public handle GetIdListTmpHandle():

	    define variable hTmpHandle as handle no-undo.

	    hTmpHandle = handle(GetProperty(input 'IdListTmpHandle':U)) no-error.

	    if valid-handle(hTmpHandle)
	    then return hTmpHandle.

		return ?.

	end method.

	method public logical CopyPropertyIdList(input DataObject as com.quarix.data.DataObject, input QueryName as character):

		define variable hTmpHandle as handle no-undo.

		if not valid-object(DataObject)
		then return false.

		DataObject:ClearProperty('QueryName':U).

		DataObject:ClearProperty('IdListTmpHandle':U).

		hTmpHandle = GetIdListTmpHandle().

		if not valid-handle(hTmpHandle)
		then return false.

		DataObject:SetQueryName(QueryName).

		DataObject:SetIdListTmpHandle(hTmpHandle).

		return true.

		catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

	end method.

	method public logical CopyPropertyIdList(input DataObject as com.quarix.data.DataObject):

		return CopyPropertyIdList(input DataObject, 'FetchDataByIdList':U).

	end method.

	method public void ClearContext():

		dataset dscontext:empty-dataset().

		assign
			lastSortOrder	= 0
			iNumRec			= 0.

	end method.

	method public logical CopyFilters(
		input DataObject	as com.quarix.data.DataObject,
		input srctableName	as character
		):

		return CopyFilters(
			input DataObject,
			input srctableName,
			input srctableName,
			input '',
			input '',
			input ''
			).

	end method.

	method public logical CopyFilters(
		input DataObject	as com.quarix.data.DataObject,
		input srctableName	as character,
		input tableName		as character
		):

		return CopyFilters(
			input DataObject,
			input srctableName,
			input tableName,
			input '',
			input '',
			input ''
			).

	end method.

	method public logical CopyFilters(
		input DataObject	as com.quarix.data.DataObject,
		input srctableName	as character,
		input tableName		as character,
		input srcfieldName	as character
		):

		return CopyFilters(
			input DataObject,
			input srctableName,
			input tableName,
			input srcfieldName,
			input srcfieldName,
			input ''
			).

	end method.

	method public logical  CopyFilters(
		input DataObject	as com.quarix.data.DataObject,
		input srctableName	as character,
		input tableName		as character,
		input srcfieldName	as character,
		input operName		as character
		):

		return CopyFilters(
			input DataObject,
			input srctableName,
			input tableName,
			input srcfieldName,
			input srcfieldName,
			input operName
			).

	end method.

	method public logical CopyFilters(
		input DataObject	as com.quarix.data.DataObject,
		input srctableName	as character,
		input tableName		as character,
		input srcfieldName	as character,
		input fieldName		as character,
		input operName		as character
		):

		if Util:IsEmpty(srctableName)
		then return false.

		if Util:IsEmpty(tableName)
		then tableName = srctableName.

		if valid-object(DataObject) and
			type-of (DataObject,'com.quarix.data.DataObject')
		then do:
			for each ttFilter
				where ttFilter.tableName	= srctableName
			  	  and ttFilter.fieldName	= (if srcfieldName = ? or srcfieldName = '' then ttFilter.fieldName else srcfieldName)
			  	  and ttFilter.operName		= (if operName = ? or operName = '' then ttFilter.operName else operName)
				no-lock:

				if Util:IsEmpty(srcfieldName)
				then fieldName = ttFilter.fieldName.
				else
					if Util:IsEmpty(fieldName)
					then fieldName = ttFilter.fieldName.

				DataObject:SetFilter (tableName, fieldName, ttFilter.operName, ttFilter.fieldValue).
			end.

			return true.
		end.

		return false.

		catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

	end method.

	method public logical CopyProperties(input DataObject as com.quarix.data.DataObject, input propertyName as character):

		if valid-object(DataObject) and
			type-of (DataObject,'com.quarix.data.DataObject')
		then do:
			for each ttProperty
				where ttProperty.propertyName = propertyName
				no-lock:

				DataObject:SetProperty(propertyName, ttProperty.propertyValue).

			end.

			return true.
		end.

		return false.

		catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

	end method.

	method public void DumpToXml():

        Util:LogDataset(input dataset dscontext by-reference).

	end method.

	method public void DumpToXml(input pcName as character):

        Util:LogDataset(input dataset dscontext by-reference, pcName).

	end method.

	method public void ClearQueryData():

		ClearFilters ().
		ClearProperties ().

	end method.

	method public logical RemoveTtRowId(input tableName as character, input ttRowId as character):

		find first ttRowId
			where ttRowId.ttName	= tableName
			  and ttRowId.ttRowId	= ttRowId
			no-lock no-error.

		if not available(ttRowId)
		then return false.

		delete ttRowId.

		return true.

	end method.

	method public character getLastRowid(input tablename as character):
	  find last ttRowId where ttRowId.ttName = tableName use-index idxNumRec no-error.
	  if available ttRowId then return ttRowId.dbrowid.
	  else return ?.
  end method.

	method public logical UpdateTtRowId(input tableName as character, input ttRowId as character, input newttRowId as character):

		define buffer BufttRowId for ttRowId.

		define variable iNumRec as integer no-undo.

		find first ttRowId
			where ttRowId.ttName	= tableName
			  and ttRowId.ttRowId	= ttRowId
			no-lock no-error.

		if not available(ttRowId)
		then return false.

		find first BufttRowId
			where BufttRowId.ttName		= tableName
			  and BufttRowId.ttRowId	= newttRowId
			no-lock no-error.

		if not available(BufttRowId)
		then
			ttRowId.ttRowId = newttRowId.
		else do:
			iNumRec = ttRowId.NumRec.

			delete ttRowId.

			BufttRowId.NumRec = iNumRec.
		end.

		return true.

	end method.

	method public logical getAllRecords():
	    return allRecords.
    end method.

    method public void setAllRecords (input lAllRecords as logical):
        allRecords = lAllRecords.
    end method.

    method public logical hasFilters ():
        define buffer bufFilter for ttFilter.

        find first bufFilter no-lock no-error.
        if available bufFilter then
        return true.

        return false.
    end method.

    method public final logical setStartRowid (input tableName as character, input startRowid as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName
            on error undo, throw:

            assign ttInfo.startRowid = startRowid.

            return true.
        end.

        create ttInfo.

        assign
            ttInfo.ttName     	= tableName
            ttInfo.startRowid	= startRowid.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public final character getStartRowid (tableName as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName:
            return ttInfo.startRowid.
        end.

        return 'first':u.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method public final logical setStartRow (input tableName as character, input startRow as integer):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName
            on error undo, throw:

            assign ttInfo.startRow = startRow.

            return true.
        end.

        create ttInfo.

        assign
            ttInfo.ttName   = tableName
            ttInfo.startRow	= startRow.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public final integer getStartRow (tableName as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName:
            return ttInfo.startRow.
        end.

        return ?.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

	method public final logical setSkipRow (input tableName as character, input skipRow as logical):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName
            on error undo, throw:

            assign ttInfo.skipRow = skipRow.

            return true.
        end.

        create ttInfo.

        assign
            ttInfo.ttName   = tableName
            ttInfo.skipRow	= skipRow.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public final logical getSkipRow (tableName as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName:
            return ttInfo.skipRow.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return true.
        end catch.

    end method.

    method public final logical setIsSearchRequest (input tableName as character, input isSearchReq as logical):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName
            on error undo, throw:

            assign ttInfo.isSearchReq = isSearchReq.

            return true.
        end.

        create ttInfo.

        assign
            ttInfo.ttName   	= tableName
            ttInfo.isSearchReq	= isSearchReq.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public final logical getIsSearchRequest (tableName as character):

        define buffer ttInfo for ttInfo.

        for each ttInfo where ttInfo.ttName eq tableName:
            return ttInfo.isSearchReq.
        end.

        return false.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public final void SetQueryRepositionFailed(input lQueryRepositionFailed as logical):

    	SetProperty('QueryRepositionFailed':u, string(lQueryRepositionFailed)).

    end method.

    method public final logical GetQueryRepositionFailed():

    	define variable cValue	as character no-undo.
    	define variable lValue	as logical no-undo.

    	cValue = GetProperty('QueryRepositionFailed':u).

    	if Util:IsEmpty(cValue)
    	then return false.

    	lValue = logical(cValue) no-error.

    	if lValue = ?
    	then lValue = false.

    	return lValue.

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public final void SetRepositionToRowFailed(input lRepositionToRowFailed as logical):

    	SetProperty('RepositionToRowFailed':u, string(lRepositionToRowFailed)).

    end method.

    method public final logical GetRepositionToRowFailed():

    	define variable cValue	as character no-undo.
    	define variable lValue	as logical no-undo.

    	cValue = GetProperty('RepositionToRowFailed':u).

    	if Util:IsEmpty(cValue)
    	then return false.

    	lValue = logical(cValue) no-error.

    	if lValue = ?
    	then lValue = false.

    	return lValue.

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public final void ClearTtRowId():

    	empty temp-table ttRowId.

    	iNumRec = 0.

    end method.

    destructor DataContext():

    	if valid-handle(dsContextHandle)
    	then dsContextHandle:empty-dataset ().

    end destructor.

end class.
