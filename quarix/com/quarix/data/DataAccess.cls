/*------------------------------------------------------------------------
   File        : DataAccess
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Andriuhan
   Created     : Mon Nov 29 14:24:57 EET 2010
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
using com.quarix.data.iDataAccess.
using com.quarix.data.*.
using com.quarix.web.Request.
using com.quarix.web.Response.


&scoped-define req-var-xml-data         'xml':u
&scoped-define req-var-lookup-table     'table':u
&scoped-define req-var-lookup-col       'col':u
&scoped-define req-var-lookup-val       'val':u
&scoped-define req-var-lookup-desc      'desc':u
&scoped-define req-var-lookup-filter    'filter':u
&scoped-define req-var-lookup-max-rec   10

class com.quarix.data.DataAccess
    inherits BaseObject
    implements iDataAccess, com.quarix.base.iDisposable:

    {com/quarix/data/dscontext.i &scope=private}

    {com/quarix/data/dsindexinformation.i}

    define public property ID as character no-undo
        get.
        protected set.

    define protected property datasetHandle as handle no-undo
        get.
        set.

    define public property DataContext as DataContext no-undo
        get.
        set.

    define public property DataRequest as DataRequest no-undo
        get.
        set.

    define public property ReadOnlyFields as character no-undo
        get.
        protected set (roFields as character):
            if setReadOnlyFields (input-output roFields) then
                ReadOnlyFields = roFields.
        end set.

    define public property RemoteChildFilter as logical no-undo
        get.
        set.

    define private variable dataModel  as DataModel no-undo.

    define private variable iSortOrder as integer   no-undo.

    define public property LogQueryString as logical no-undo initial false
        get.
        set.

    define protected property FieldMapping as fieldmapping no-undo
        get:
        	if not valid-object(FieldMapping) then FieldMapping = cast (GetInstance ('com.quarix.data.fieldmapping':u), com.quarix.data.fieldmapping).
        	return FieldMapping.
        end.
        private set.

    define private variable hTtTemp    as handle    no-undo.
    define private variable hBufTtTemp as handle    no-undo.
    define private variable fieldMap   as character no-undo extent 16.

    constructor public DataAccess():

        super().

        if Application:LogLevel ge 4
        then LogQueryString = true.

    end constructor.

    destructor public DataAccess():

        DetachDataSource().

        delete object hTtTemp no-error.

		if valid-object(FieldMapping)
		then FieldMapping:ClearFieldMap().

    end destructor.

    method public logical AddWhereClause (tableName as character, bufferName as character, whereClause as character):

        if not valid-object(DataContext)
        then return false.

        if not ValidateWhereClause(tableName, bufferName, input-output whereClause)
        then return false.

        return DataContext:AddWhereClause (tableName, bufferName, whereClause, false).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public logical AddJoinClause (tableName as character, bufferName as character, joinClause as character):

        if not valid-object(DataContext)
        then return false.

        if not ValidateWhereClause(tableName, bufferName, joinClause)
        then return false.

        return DataContext:AddWhereClause (tableName, bufferName, joinClause, true).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private logical ValidateWhereClause (tableName as character, bufferName as character, input-output whereClause as character):

        define variable tableHandle  as handle    no-undo.
        define variable bufferHandle as handle    no-undo.
        define variable numBuf       as integer   no-undo.

        define variable numToken     as integer   no-undo.
        define variable token        as character no-undo.
        define variable sourceField  as character no-undo.
        define variable targetField  as character no-undo.

        if not valid-handle(datasetHandle)
        then return false.

        whereClause = Util:DelSpace(whereClause).

        assign
            whereClause = trim(whereClause)
            tableHandle = datasetHandle:get-buffer-handle(tableName)
            no-error.

        if  Util:IsEmpty(whereClause)     or
            not valid-handle(tableHandle) or
            not valid-handle(tableHandle:data-source)
        then return false.

        do numBuf = 1 to tableHandle:data-source:num-source-buffers:

            bufferHandle = tableHandle:data-source:get-source-buffer (numBuf).

            if bufferHandle:name eq bufferName
            then leave.
        end.

        if not valid-handle(bufferHandle) or bufferHandle:name ne bufferName
        then return false.

        tableName = tableName + '.':U.

		whereClause = whereClause + ' '.

        repeat:
            numToken = index(whereClause, tableName, numToken + 1).

            if numToken eq 0 then leave.

            token = substring (whereClause, numToken).

            assign
                sourceField = substring(token,1,index(token,' ':U))
                sourceField = trim(sourceField).

            targetField = GetPhysicalFieldName(sourceField).

            if Util:IsEmpty(targetField) then
                return false.

            whereClause = replace(whereClause, sourceField + ' ', targetField + ' ').

        end. /* repeat */

        whereClause = trim(whereClause).

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method protected logical RepositionToRow (bufferName as character, repositionRow as integer):
        return RepositionToRow (bufferName, repositionRow, false, false).
    end method.


    method protected logical RepositionToRow (bufferName as character, repositionRow as integer, goBackward as logical):
        return RepositionToRow (bufferName, repositionRow, goBackward, false).
    end method.


    method private logical RepositionFirst(input hQuery as handle, input cPrepString as character):

    	define variable lOk as logical no-undo.

    	if not valid-handle(hQuery) or
    		Util:IsEmpty(cPrepString)
    	then return false.

    	if hQuery:is-open
    	then hQuery:query-close() no-error.

		lOk = hQuery:query-prepare(cPrepString) no-error.

		if Util:IsError() or
			not lOk
		then return false.

		lOk = hQuery:query-open() no-error.

		if Util:IsError() or
			not lOk
        then return false.

		return true.

		catch appError as Progress.Lang.Error :

			ThrowError(input appError).

            delete object appError.

            return false.

        end catch.

    end method.


    method protected logical RepositionToRow (  bufferName as character, repositionRow   as integer,
        goBackward as logical,   skipRecord      as logical):

        define variable hDatasource		as handle    no-undo.
        define variable hBuf         as handle    no-undo.
        define variable hQuery       as handle    no-undo.
        define variable iBuf         as integer   no-undo.
        define variable lOk          as logical   no-undo.
        define variable cPrepString		as character	no-undo.
        define variable iNumStepBack	as integer   no-undo.
        define variable iNum         as integer   no-undo.

        if Util:IsEmpty(bufferName)
        then return false.

        if not valid-handle(datasetHandle)
        then return false.

        assign hBuf = datasetHandle:get-buffer-handle(bufferName) no-error.

        if not valid-handle(hBuf)
        then return false.

        hDatasource = hBuf:data-source.

        if not valid-handle(hDatasource)
        then return false.

        hQuery = hDatasource:query.

        if not valid-handle(hQuery)
        then return false.

        if not hQuery:is-open
        then do:
            lOk = hQuery:query-open() no-error.

            if Util:IsError() or
                not lOk
            then return false.
        end.

        cPrepString = hQuery:prepare-string.

        /* the query is opened,
        we should move the cursor to the first record because
        maybe the query was opened before and we do not know the current position of the cursor */

        hQuery:get-first ().

        /* No records are available, we cannot reposition */
        if hQuery:query-off-end
        then return true.

        hQuery:reposition-backward(1). /* place the cursor before the first record */

        /* -1 - last; 0 - first */
        if repositionRow eq 0
        then return true. /* we are already before the first record, nothing to do */

        if repositionRow eq -1
        then do:
        	hQuery:get-last(no-lock).

        	hQuery:reposition-backward(1). /* place the cursor before the last record */
        end.
        else do:
        	repositionRow = abs(repositionRow).

        	lOk = hQuery:reposition-to-row(repositionRow) no-error. /* the cursor is placed before the record */

        	if not lOk
        	then do:
        		hQuery:get-first ().

        		hQuery:reposition-backward(1). /* place the cursor before the first record */

        		ThrowWarning(9991, 'Cannot reposition to row, query opened with first batch!':u).

        		DataContext:SetQueryRepositionFailed(true).

        		DataContext:SetRepositionToRowFailed(true).

            	return true.
        	end.

        	/* The cursor is before the record so we have to position the cursor on the record */
        	hQuery:get-next().

        	/* If no data is available than we do not have so many records to be able to reposition, we notify the UI about this */
        	if hQuery:query-off-end
        	then do:
        		hQuery:get-first ().

        		hQuery:reposition-backward(1). /* place the cursor before the first record */

        		ThrowWarning(9991, 'Cannot reposition to row, query opened with first batch!':u).

        		DataContext:SetQueryRepositionFailed(true).

        		DataContext:SetRepositionToRowFailed(true).

            	return true.
        	end.

        	hQuery:reposition-backward(1). /* place back the cursor before the record */

        end.

        /* The cursor must be positioned backward from the current location with the batch size */
        if repositionRow eq -1 or
        	goBackward
        then do:
        	iNumStepBack = abs(hBuf:batch-size).

        	if not skipRecord
        	then iNumStepBack = iNumStepBack - 1.

        	if iNumStepBack = 0 /* stay at the current position, nothing to do  */
        	then return true.

        	/* Check if we can do any steps back, maybe we already at the beginning of the query  */

        	hQuery:get-prev().

        	/* If no data is available than we do not have so many records to be able to reposition, we notify the UI about this */
        	if hQuery:query-off-end
        	then do:
        		hQuery:get-first ().

        		hQuery:reposition-backward(1). /* place the cursor before the first record */

        		ThrowWarning(9991, 'Cannot reposition to row, query opened with first batch!':u).

        		DataContext:SetQueryRepositionFailed(true).

            	return true.
        	end.

        	/* We have another record before the current one, so we have to position the cursor back before the current record where it was */
        	hQuery:get-prev().

        	hQuery:reposition-forward(1).

        	/* Check if we can do enough steps back in the query, if not we reposition at the beginning of the query and notify the UI */
        	#Step:
        	do iNum = 1 to iNumStepBack:

        		hQuery:reposition-backward(1).

        		/* Check if we can do another step back */
        		if iNum < iNumStepBack
        		then do:
        			/* Check if we have a valid record before the current one */
        			hQuery:get-prev().

        			/* We do not have another record before the current one, we cannot do another step back */
        			if hQuery:query-off-end
        			then do:
        				hQuery:get-first ().

        				hQuery:reposition-backward(1). /* place the cursor before the first record */

        				leave #Step.
        			end.

        			/* We have another record before the current one, so we have to position the cursor back before the current record where it was */
        			hQuery:get-prev().

        			hQuery:reposition-forward(1).

        		end. /* if iNum < iNumStepBack */

        	end. /* do iNum = 1 to iNumStepBack */

        	/* Update the batch size in order to return only the available number of records from the starting point */
        	if iNum < iNumStepBack
        	then do:
        		if not skipRecord
        		then iNum = iNum + 1.

        		hBuf:batch-size = iNum.
        	end.
        end.
        else do:
        	/* Start fetching the records from the current location */

        	/* skip the current record when the data is fetched, so we should move one step ahead */
        	if skipRecord
        	then hQuery:reposition-forward(1).
        end.

        return true.

        catch appError as Progress.Lang.Error :

        	/* If something happened than we do not log the error right away,
        	first we have to try to open the query by positioning to the first record,
        	if we cannot open the query than we log the error */

        	if not RepositionFirst(input hQuery, input cPrepString)
        	then do:
            	ThrowError(input appError).

                delete object appError.

                return false.
            end.

            delete object appError.

            /* If we managed to open the query
            we have to notify the UI about the fact that because of some error
            we are positioned at the beginning of the query */

            ThrowWarning(9991, 'Cannot reposition to row, query opened with first batch!':u).

            DataContext:SetQueryRepositionFailed(true).

            return true.

        end catch.

    end method.

    method protected final logical RepositionToRow (bufferName as character, repositionRowid as character):
        return RepositionToRow (bufferName, repositionRowid, false, false).
    end method.


    method protected final logical RepositionToRow (bufferName as character, repositionRowid as character, goBackward as logical):
        return RepositionToRow (bufferName, repositionRowid, goBackward, false).
    end method.


    method protected logical RepositionToRow (  bufferName as character, repositionRowid as character,
        goBackward as logical,   skipRecord      as logical):

        define variable hDatasource		as handle    no-undo.
        define variable hQuery       as handle    no-undo.
        define variable hBuf         as handle    no-undo.
        define variable iBuf         as integer   no-undo.
        define variable posRowid     as rowid     no-undo extent 18.
        define variable cQuery       as character no-undo.
        define variable lOk          as logical   no-undo.
        define variable cPrepString		as character no-undo.
        define variable iNumStepBack	as integer   no-undo.
        define variable iNum         as integer   no-undo.

        if Util:IsEmpty(bufferName)
        then return false.

        if not valid-handle(datasetHandle)
        then return false.

        assign hBuf = datasetHandle:get-buffer-handle(bufferName) no-error.

        if not valid-handle(hBuf)
        then return false.

        hDatasource = hBuf:data-source.

        if not valid-handle(hDatasource)
        then return false.

        hQuery = hDatasource:query.

        if not valid-handle(hQuery)
        then return false.

        if not hQuery:is-open
        then do:
            lOk = hQuery:query-open() no-error.

            if Util:IsError() or
                not lOk
            then return false.
        end.

        cPrepString = hQuery:prepare-string.

        /* the query is opened,
        we should move the cursor to the first record because
        maybe the query was opened before and we do not know the current position of the cursor */

        hQuery:get-first ().

        /* No records are available, we cannot reposition */
        if hQuery:query-off-end
        then return true.

        hQuery:reposition-backward(1). /* place the cursor before the first record */

        if repositionRowid eq ? or
            repositionRowid eq 'first':u
        then return true. /* we are already before the first record, nothing to do */

        if repositionRowid eq 'last':u
        then return RepositionToRow(bufferName, -1, true, false).

        repositionRowid = ConvertDataToDbRowId(input hBuf, input repositionRowid).

		if Util:IsEmpty(repositionRowid)
        then do:
            /* This is necessary because the above methode call moved the buffers */
            hQuery:get-first ().

        	hQuery:reposition-backward(1). /* place the cursor before the first record */

        	ThrowWarning(9991, 'Cannot reposition to row, query opened with first batch!':u).

        	DataContext:SetQueryRepositionFailed(true).

        	DataContext:SetRepositionToRowFailed(true).

        	return true.
        end.

		do iBuf = 1 to num-entries(repositionRowid, ',':U):
            posRowid[iBuf] = to-rowid(entry(iBuf, repositionRowid)) .
        end.

        cQuery = trim(cPrepString, ' .':u).

        if index (cQuery, 'indexed-reposition':u) = 0
        then do:
        	if hQuery:is-open
            then hQuery:query-close ().

            hQuery:forward-only = false.

            hQuery:query-prepare(cQuery + ' indexed-reposition':u) .
        end.

        if not hQuery:is-open
        then hQuery:query-open().

        lOk = hQuery:reposition-to-rowid(posRowid) no-error. /* the cursor is placed before the record */

        if not lOk
        then do:
        	hQuery:get-first ().

        	hQuery:reposition-backward(1). /* place the cursor before the first record */

        	ThrowWarning(9991, 'Cannot reposition to row, query opened with first batch!':u).

        	DataContext:SetQueryRepositionFailed(true).

        	DataContext:SetRepositionToRowFailed(true).

           	return true.
        end.

        /* The cursor is before the record so we have to position the cursor on the record */
		hQuery:get-next().

		/* If no data is available than we do not have so many records to be able to reposition, we notify the UI about this */
		if hQuery:query-off-end
        then do:
        	hQuery:get-first ().

        	hQuery:reposition-backward(1). /* place the cursor before the first record */

        	ThrowWarning(9991, 'Cannot reposition to row, query opened with first batch!':u).

        	DataContext:SetQueryRepositionFailed(true).

        	DataContext:SetRepositionToRowFailed(true).

           	return true.
        end.

        hQuery:reposition-backward(1). /* place back the cursor before the record */

        /* The cursor must be positioned backward from the current location with the batch size */
        if goBackward
        then do:
        	iNumStepBack = abs(hBuf:batch-size).

        	/* skip the current record when the data is fetched, so we should move one step further back */
        	if not skipRecord
        	then iNumStepBack = iNumStepBack - 1.

        	if iNumStepBack = 0 /* stay at the current position, nothing to do  */
        	then return true.

        	/* Check if we can do any steps back, maybe we already at the beginning of the query  */

        	hQuery:get-prev().

        	/* If no data is available than we do not have so many records to be able to reposition, we notify the UI about this */
        	if hQuery:query-off-end
        	then do:
        		hQuery:get-first ().

        		hQuery:reposition-backward(1). /* place the cursor before the first record */

        		ThrowWarning(9991, 'Cannot reposition to row, query opened with first batch!':u).

        		DataContext:SetQueryRepositionFailed(true).

            	return true.
        	end.

        	/* We have another record before the current one, so we have to position the cursor back before the current record where it was */
        	hQuery:get-prev().

        	hQuery:reposition-forward(1).

        	/* Check if we can do enough steps back in the query, if not we reposition at the beginning of the query and notify the UI */
        	#Step:
        	do iNum = 1 to iNumStepBack:

        		hQuery:reposition-backward(1).

        		/* Check if we can do another step back */
        		if iNum < iNumStepBack
        		then do:
        			/* Check if we have a valid record before the current one */
        			hQuery:get-prev().

        			/* We do not have another record before the current one, we cannot do another step back */
        			if hQuery:query-off-end
        			then do:
        				hQuery:get-first ().

        				hQuery:reposition-backward(1). /* place the cursor before the first record */

        				leave #Step.
        			end.

        			/* We have another record before the current one, so we have to position the cursor back before the current record where it was */
        			hQuery:get-prev().

        			hQuery:reposition-forward(1).

        		end. /* if iNum < iNumStepBack */

        	end. /* do iNum = 1 to iNumStepBack */

        	/* Update the batch size in order to return only the available number of records from the starting point */
        	if iNum < iNumStepBack
        	then do:
        		if not skipRecord
        		then iNum = iNum + 1.

        		hBuf:batch-size = iNum.
        	end.

        end.
        else do:
        	/* Start fetching the records from the current location */

        	/* skip the current record when the data is fetched, so we should move one step ahead */
        	if skipRecord
        	then hQuery:reposition-forward(1).
        end.

        return true.

        catch appError as Progress.Lang.Error :

        	/* If something happened than we do not log the error right away,
        	first we have to try to open the query by positioning to the first record,
        	if we cannot open the query than we log the error */

        	if not RepositionFirst(input hQuery, input cPrepString)
        	then do:
            	ThrowError(input appError).

                delete object appError.

                return false.
            end.

            delete object appError.

            /* If we managed to open the query
            we have to notify the UI about the fact that because of some error
            we are positioned at the beginning of the query */

            ThrowWarning(9991, 'Cannot reposition to row, query opened with first batch!':u).

            DataContext:SetQueryRepositionFailed(true).

            return true.

        end catch.

    end method.

    method public logical LoadData ():

        iSortOrder = 0.

        if not BeforeLoadData() then return false.

        if not fillDataSet()
        then do:
            if valid-handle(datasetHandle)
            then datasetHandle:empty-dataset().

            return false.
        end.

        if not AfterLoadData() then return false.

        return true.

        catch appError as Progress.Lang.Error :

            if valid-handle(datasetHandle)
            then datasetHandle:empty-dataset().

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private logical fillDataSet():

        define variable numBuf as integer no-undo.

        if not valid-handle(datasetHandle)
        then return false.

        if not GetIndexInformation()
        then return false.

        DataContext:SetQueryRepositionFailed(false).

        DataContext:SetRepositionToRowFailed(false).

        do numBuf = 1 to datasetHandle:num-top-buffers:
            if not fillBuffer (datasetHandle:get-top-buffer(numBuf))
            then return false.
        end.

        return true.

    end method.


    method private logical fillBuffer(hBuf as handle):
        return fillBuffer(hBuf, true).
    end method.

    method public void EnableDebugQuery():

        LogQueryString = true.

    end method.

    method public void DisableDebugQuery():

        LogQueryString = false.

    end method.

    method protected character BeforePrepareQuery(input qryString as character):
    	return qryString.
    end method.

    method private logical prepareQuery(input hBuf as handle, input qryString as character):

        define variable hQry       as handle    no-undo.
        define variable lPrepareOk as logical   no-undo.

        define variable iNum       as integer   no-undo.
        define variable cIndex     as character no-undo.
        define variable cVal       as character no-undo.

        qryString = BeforePrepareQuery(qryString).

        if LogQueryString
        then do:
            ThrowDebug(100,substitute ('Object: &1, Query executed: &2',this-object:ToString(),qryString)).
        end.

        if not valid-handle(hBuf) or
            Util:IsEmpty(qryString)
        then return false.

        if not valid-handle(hBuf:data-source)
        then return false.

        hQry = hBuf:data-source:query.

        if not valid-handle(hQry)
        then return false.

        if hQry:is-open
        then hQry:query-close ().

        lPrepareOk = hQry:query-prepare(qryString) no-error.

        if Util:IsError() or
            not lPrepareOk
        then do:
            ThrowError(100, 'msg_err_data_invalid_query':u, hBuf:table-handle:name, qryString).

            undo, throw new Progress.Lang.AppError(error-status:get-message (1), error-status:get-number (1)).
        end.


        if LogQueryString
        then do:
            do iNum = 1 to hQry:num-buffers:
                assign
                    cIndex = hQry:index-information(iNum)
                    cVal   = substitute ('Buffer: &1, Used index: &2',hQry:get-buffer-handle(iNum):name,cIndex).
                ThrowDebug(100,cVal).
            end.

        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method private logical fillBuffer(hBuf as handle, useSearchFilter as logical):

        define variable numRel           as integer   no-undo.
        define variable numKey           as integer   no-undo.
        define variable numRec           as integer   no-undo.
        define variable keyName          as character no-undo.
        define variable dsQuery          as character no-undo.
        define variable qryString        as character no-undo.
        define variable startRowid       as character no-undo.
        define variable startRow         as integer   no-undo initial ?.
        define variable startTime        as integer   no-undo.
        define variable queryTime        as integer   no-undo.
        define variable hTT              as handle    no-undo.
        define variable hQry             as handle    no-undo.
        define variable hParentBuf       as handle    no-undo.
        define variable firstBatch       as logical   no-undo.
        define variable lastBatch        as logical   no-undo.
        define variable isSearchReq      as logical   no-undo.
        define variable hdsContextHandle	as handle    no-undo.

        if LogQueryString then
        startTime = etime.

        if not valid-handle(hBuf) then
            return false.

        /* fetch data only if requested... */
        if hBuf:fill-mode eq 'no-fill':u or
            not needFill(hBuf)
        then return true.

        /* no load can't be done without a valid data source */
        if not valid-handle(hBuf:data-source)
        then return false.

        /* if we don't use the search request we reset filters to use only regular ones */

        if DataContext:getIsSearchRequest(hBuf:table-handle:name)
        then do:
            isSearchReq = true.

            if useSearchFilter eq false
            then dataModel:SetRequestFilters (hBuf:table, false).
        end.

        /* update query string: filters and sort */
        assign
            hTT                  = hBuf:table-handle
            hTT:tracking-changes = false.

        hQry = hBuf:data-source:query.

        if not valid-handle(hQry)
        then return false.

        dsQuery = hQry:prepare-string.

        if Util:IsEmpty(dsQuery)
        then return false.

        hdsContextHandle = DataContext:dsContextHandle.

        qryString = updateDataSourceQuery(hBuf, input dataset-handle hdsContextHandle by-reference).

        if LogQueryString then
            message qryString.

        if Util:IsEmpty(qryString)
        then return false.

        /* use start rowid provided only if not search request */
        if isSearchReq eq false then
        do:
        	assign
            	startRowid = DataContext:getStartRowid(hTT:name)
            	startRow   = DataContext:getStartRow(hTT:name).
        end.
        else
        do:
        	/* if search request didn't found any record we send back the last batch */
            if useSearchFilter eq false then
            	startRowid = 'last':u.
       end.

        if hQry:is-open
        then hQry:query-close().

        if LogQueryString then
        ThrowDebug(100,'-----------------------------------------------------------------------------').

        if valid-handle(hBuf:parent-relation) and
            needFill(hBuf:parent-relation:parent-buffer)
        then do:
            hParentBuf = hBuf:parent-relation:parent-buffer.

            if hParentBuf:table-handle:has-records then
            do:
                if RemoteChildFilter eq true then
                do:

                    /* we always put all child records for each record in parent */
                    assign
                        firstBatch      = true
                        lastBatch       = true
                        hBuf:batch-size = 0.

                    hBuf:fill() .
                end.
                else
                do:
                    /* server-side child filter, send only records related to the first record from parent */
                    hParentBuf:find-first('':u, no-lock) .

                    do numKey = 1 to num-entries(hBuf:parent-relation:relation-fields) by 2:
                        assign
                            keyName   = entry(numKey, hBuf:parent-relation:relation-fields)
                            qryString = replace(qryString,
                                            substitute('&1.&2':u, hParentBuf:table, keyName),
                                            substitute('&1':u, quoter(hParentBuf:buffer-field(keyName):buffer-value))).
                    end.

                    if not prepareQuery(hBuf, qryString)
                    then return false.

                    if valid-object(ErrorManager) and ErrorManager:GetLogLevel() ge ErrorManager:DebugLevel() then
                    do numKey = 1 to hQry:num-buffers:
                        if index ('whole-index,':u, hQry:index-information(numKey)) eq 1 then
                        do:
                            ThrowDebug(100, 'msg_err_whole_index':u,
                                substitute('&1 - &2':u, hQry:get-buffer-handle(numKey):name, hQry:index-information(numKey)), '':u).
                        end.
                    end.

                    hQry:query-open().

                    /* reposition to start rowid, for search request don't skip first row */
                    if startRowid ne 'first':u
                    then do:
                        if not RepositionToRow(
                            hTT:name,
                            startRowid,
                            hBuf:batch-size lt 0,
                            isSearchReq eq false and DataContext:getSkipRow(hTT:name)
                            )
                        then return false.
                    end.
                    else do:
                        if startRow ne ?
                        then
                            if not RepositionToRow(
                                hTT:name,
                                startRow,
                                hBuf:batch-size lt 0,
                                isSearchReq eq false and DataContext:getSkipRow(hTT:name)
                                )
                            then return false.
                    end.

                    numRec = fillBuffer(hBuf, hQry, abs(hBuf:batch-size)).

                    if numRec = ?
                    then return false.
                end.
            end.
            else
                assign
                    firstBatch = true
                    lastBatch  = true.

        end. /* if valid-handle(hBuf:parent-relation) and needFill(hBuf:parent-relation:parent-buffer) */
        else
        do:
            if not prepareQuery(hBuf, qryString)
            then return false.

            hQry:query-open().

            /* reposition to start rowid, for search request don't skip first row */
            if startRowid ne 'first':u
            then do:
                /* don't reposition if batch-size is 0, all rows need to be loaded */
                if hBuf:batch-size ne 0
                then do:
                    if not RepositionToRow(
                        hTT:name,
                        startRowid,
                        hBuf:batch-size lt 0,
                        isSearchReq eq false and DataContext:getSkipRow(hTT:name)
                        )
                    then return false.
                end.
                else do:
                    if not valid-handle(datasetHandle:get-buffer-handle(hTT:name))
                    then return false.

                    DataContext:setNewPosition(hTT:name, startRowid).
                end.
            end.
            else do:
                if startRow ne ?
                then
                    if not RepositionToRow(
                        hTT:name,
                        startRow,
                        hBuf:batch-size lt 0,
                        isSearchReq eq false and DataContext:getSkipRow(hTT:name)
                        )
                    then return false.
            end.

            numRec = fillBuffer(hBuf, hQry, abs(hBuf:batch-size)).

            if numRec = ?
            then return false.
        end.

        /* if no record found using the search criteria, send the last batch - [less than] */
        if numRec eq 0          and
            isSearchReq eq true and
            useSearchFilter eq true
        then do:
            hQry:query-close().
            hQry:query-prepare(dsQuery).

            if not fillBuffer(hBuf, false)
            then return false.
        end.
        else
        do:
            /* see if we have the last batch  */
            if not lastBatch then
            do:
                if numRec lt abs(hBuf:batch-size)
                then lastBatch = true.
                else
                    assign
                        numRec    = numRec + 1
                        lastBatch = not hQry:reposition-forward(1) .
            end.

            /* see if we have the first batch */
            if not firstBatch then
            do:
                if startRowid eq 'first':u then
                    firstBatch = true.
                else
                do:
                    if isSearchReq eq false then
                        firstBatch = not hQry:reposition-backward(numRec + 1).
                    else
                    do:
                        hQry:query-prepare(dsQuery).

                        firstBatch = IsFirstRecord(hBuf, lastBatch, numRec).

                        if firstBatch = ?
                        then return false.
                    end.
                end.
            end. /* if not firstBatch */

            hQry:query-close() .

            if not valid-handle(datasetHandle:get-buffer-handle(hTT:name))
            then return false.

            if not DataContext:setBatchInfo(hTT:name, firstBatch , lastBatch) then
            do:
                ThrowError(700, 'cannot create ttinfo', 'fillbuffer', '').

                return false.
            end.
        end.

        /* fill all child tables */
        do numRel = 1 to hBuf:num-child-relations:
            if not fillBuffer(hBuf:get-child-relation(numRel):child-buffer)
            then return false.
        end.

        if LogQueryString then do:
            queryTime = etime - startTime.
            ThrowDebug(100,substitute ('Query for buffer &1 executed in &2 ms returned &3 records.',hBuf:name,string(queryTime),string(numRec))).
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method private logical IsFirstRecord(hBuf as handle, lastBatch as logical, numRecords as integer):

        define variable hQry             as handle    no-undo.
        define variable qryString        as character no-undo.
        define variable dsQuery          as character no-undo.
        define variable oldRowid         as rowid     no-undo.
        define variable newRowid         as rowid     no-undo.
        define variable hTT              as handle    no-undo.
        define variable fieldName        as character no-undo.
        define variable fieldValue       as character no-undo.
        define variable tableName        as character no-undo.
        define variable operName         as character no-undo.
        define variable lPrepareOk       as logical   no-undo.
        define variable hdsContextHandle	as handle    no-undo.

        if not valid-handle(hBuf)
        then return ?.

        if not valid-handle(hBuf:data-source)
        then return ?.

        hTT       = hBuf:table-handle.
        hQry      = hBuf:data-source:query .

        if not valid-handle(hQry)
        then return ?.

        dsQuery = hQry:prepare-string.

        if Util:IsEmpty(dsQuery)
        then return ?.

        hdsContextHandle = DataContext:dsContextHandle.

        qryString = updateDataSourceQuery(hBuf, input dataset-handle hdsContextHandle by-reference).

        if Util:IsEmpty(qryString)
        then return ?.

        if hQry:is-open
        then hQry:query-close ().

        /*get the rowid of the first record with the current query*/
        lPrepareOk = hQry:query-prepare(qryString) no-error.

        if Util:IsError() or
            not lPrepareOk
        then do:
            ThrowError(100, 'msg_err_data_invalid_query':u, hBuf:table-handle:name, qryString).

            undo, throw new Progress.Lang.AppError(error-status:get-message (1), error-status:get-number (1)).
        end.

        hQry:query-open().

        if  lastBatch
            and abs(hBuf:batch-size) lt numRecords
        then do:
            if hQry:get-last() then
            do:
                if hQry:reposition-backward (abs(hBuf:batch-size))
                then oldRowid = hQry:get-buffer-handle:rowid.
            end.
        end.
        else
        do:
            if hQry:reposition-to-row(1) then
            do:
                hQry:get-first().
                oldRowid = hQry:get-buffer-handle:rowid.
            end.
        end.

        hQry:query-close().

        lPrepareOk = hQry:query-prepare(dsQuery) no-error.

        if Util:IsError() or
            not lPrepareOk
        then do:
            ThrowError(100, 'msg_err_data_invalid_query':u, hBuf:table-handle:name, qryString).

            undo, throw new Progress.Lang.AppError(error-status:get-message (1), error-status:get-number (1)).
        end.

        /*get the rowid of the first record removing the search filter from the query*/
        if valid-object(DataRequest) then
        do:
            /*remove the search filter*/
            DataRequest:GetSearch(hTT:name, 1, output fieldName, output fieldValue).
            DataModel:RemoveFilter(fieldName).

            assign
                fieldName = getFullFieldName(fieldName)
                tableName = entry(1, fieldName, '.':u)
                fieldName = entry(2, fieldName, '.':u) .

            operName   = DataContext:GetFilterOperator(tableName, fieldName, fieldValue).
            qryString = updateDataSourceQuery(hBuf, input dataset-handle hdsContextHandle by-reference).

            if Util:IsEmpty(qryString)
            then return ?.

            if hQry:is-open
            then hQry:query-close ().

            lPrepareOk = hQry:query-prepare(qryString) no-error.

            if Util:IsError() or
                not lPrepareOk
            then do:
                ThrowError(100, 'msg_err_data_invalid_query':u, hBuf:table-handle:name, qryString).

                undo, throw new Progress.Lang.AppError(error-status:get-message (1), error-status:get-number (1)).
            end.

            hQry:query-open().

            if hQry:reposition-to-row(1) then
            do:
                hQry:get-first().
                newRowid = hQry:get-buffer-handle:rowid.
            end.

            hQry:query-close().

            DataModel:SetFilter(fieldName, operName, fieldValue).

        end. /* if valid-object(DataRequest) */

        /*if the rowids are equal than the record is the first from the table*/
        if newRowid = oldRowid
        then return true.

        return false.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.


    method private integer fillBuffer(hBuf as handle, hQry as handle, batchSize as integer):

        define variable recRowid   as character no-undo.
        define variable numRec     as integer   no-undo.
        define variable numBuf     as integer   no-undo.
        define variable hSrcBuf    as handle    no-undo.
        define variable isPrefetch as logical   no-undo.
        define variable lRetValue  as logical   no-undo.

        if not valid-handle(hBuf) or
            not valid-handle(hQry) or
            not valid-handle(hBuf:data-source)
        then return ?.

        if not hQry:is-open
        then hQry:query-open().

        /* get field map for each data source buffers */
        if hBuf:data-source:num-source-buffers eq 1 then
            fieldMap[1] = hBuf:data-source-complete-map.
        else
            do numBuf = 1 to hBuf:data-source:num-source-buffers:
                fieldMap[numBuf] = getBufferFieldMap(hBuf:data-source:get-source-buffer(numBuf):name, hBuf:data-source-complete-map).
            end.

        if not PrepareDynTT(hBuf) then do:
          return ?.
        end.

        fillBuff:
        do while hQry:is-open and
            (abs(hBuf:batch-size) eq 0 or numRec lt batchSize)
            and hQry:get-next(no-lock)
            on error undo, throw:
/*
			Adam: Commented for performance reason

            lRetValue = CheckIfRecordAvailable(input hBuf).

            if lRetValue = true
            then next fillBuff.
*/
            if not BeforeRowFill(input hBuf)
            then next fillBuff.

            recRowid = GetRecordDbRowId(hBuf).

            if Util:IsEmpty(recRowid)
            then next fillBuff.

            hBuf:buffer-create().

            do numBuf = 1 to hBuf:data-source:num-source-buffers:

                assign hSrcBuf = hBuf:data-source:get-source-buffer(numBuf).

                /* exclude fields list are set on source's buffer private-data */
                if hSrcBuf:available
                then hBuf:buffer-copy(hSrcBuf, hSrcBuf:private-data, fieldMap[numBuf]).

            end. /* do numBuf = 1 to hBuf:data-source:num-source-buffers*/

            SetSortOrder(input hBuf).

            AfterRowFill(input hBuf).

            DataContext:setDbRowid (hBuf, recRowid).

            hBuf:buffer-release ().

            numRec = numRec + 1.

        end. /* fillBuff */

        return numRec.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.
        finally:
           delete object hTtTemp no-error.
        end finally.

    end method.

    method public logical AfterRowFill(input hBuf as handle):

        return true.

    end method.

    method public logical BeforeRowFill(input hBuf as handle):

        return true.

    end method.

    method private void SetSortOrder(input hBuf as handle):

        define variable hBufField as handle no-undo.

        if not valid-handle(hBuf) or
            not hBuf:available
        then return.

        assign hBufField = hBuf:buffer-field('SortOrder':U) no-error.

        if not valid-handle(hBufField)
        then return.

        iSortOrder = iSortOrder + 1.

        hBufField:buffer-value = iSortOrder.

    end method.

    method public logical  SaveData ():

        define variable hBuf as handle  no-undo.
        define variable iBuf as integer no-undo.

        do transaction
            on error undo, throw:

            if not BeforeSaveData() then
            	undo, return false.

            do iBuf = 1 to datasetHandle:num-top-buffers:
                hBuf = datasetHandle:get-top-buffer(iBuf).
                if valid-object(DataRequest) and not DataRequest:HasRequest(hBuf:table) then
                    undo, return false.

                if not saveTable(hBuf) then
                    undo, return false.
            end.

            if not AfterSaveData()
                then undo, return false.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.


    method private character getDuplicateKeyField (hBuf as handle, hSrcBuf as handle):

        define variable fldName as character no-undo.


        if index(error-status:get-message(error-status:num-messages), 'already exists with':u) eq 0 then
            return ?.

        assign
            fldName = error-status:get-message(error-status:num-messages)
            fldName = entry(4, substring(fldName, index(fldName, 'already exists with':u), -1), ' ') .

        if hBuf:attached-pairlist ne ? and lookup(fldName, hBuf:attached-pairlist) gt 0 then
            assign
                fldName = substitute('&1.&2', hSrcBuf:name, fldName)
                fldName = entry(2, entry(lookup(fldName, hBuf:data-source-complete-map) - 1, ',':u), '.':u) .

        return fldName.

    end method.

    method private character GetModifiedFields (input phSource as handle, input phTarget as handle):

        define variable i            as integer   no-undo.
        define variable cDiff        as character no-undo.
        define variable hSourceField as handle    no-undo.
        define variable hTargetField as handle    no-undo.

        if valid-handle (phSource) and valid-handle (phTarget) then do:
            repeat i = 1 to phSource:num-fields:
                hSourceField = phSource:buffer-field (i).
                hTargetField = phTarget:buffer-field (hSourceField:name) no-error.
                if valid-handle (hTargetField) and hSourceField:buffer-value <> hTargetField:buffer-value then
                    cDiff = cDiff + ',' + phSource:name + '.':U + hSourceField:name
                        + ' (' + (if hSourceField:buffer-value = ? then '?' else hSourceField:buffer-value)
                        + ' - ' + (if hTargetField:buffer-value = ? then '?' else hTargetField:buffer-value)
                        + ')'.
            end.
            cDiff = substring (cDiff, 2).
        end.

        return cDiff.

    end method.

    method private logical saveTable (input hBuf as handle):

        define variable iRel     as integer   no-undo.
        define variable iFld     as integer   no-undo.
        define variable retVal   as logical   no-undo initial true.
        define variable startRow as character no-undo.
        define variable fldName  as character no-undo.
        define variable roFields as character no-undo.
        define variable fieldMap as character no-undo.
        define variable saveMap  as character no-undo.
        define variable hRel     as handle    no-undo.
        define variable hBi      as handle    no-undo.
        define variable hSrcBuf  as handle    no-undo.
        define variable hQry     as handle    no-undo.

        if not valid-handle(hBuf)
        then return false.

        if  not valid-handle(hBuf:before-buffer) or
            not hBuf:before-buffer:table-handle:has-records
        then return true.

        if not valid-handle(hBuf:data-source)
        then do:
            ThrowError(100, 'msg_err_data_no_source':u, hBuf:table, ?).
            return false.
        end.

        /* fields set in exclude list on AddDataSource are considered read-only as well */
        assign
            hBi      = hBuf:before-buffer
            hSrcBuf  = hBuf:data-source:get-source-buffer(1)
            roFields = Util:Nvl(hSrcBuf:private-data, '':u) .

        if not valid-handle(hSrcBuf)
        then return false.

        /* before-table and data-source defined for temp-table, we can save changes if any */
        /* if data-source not attached we can't save anything, but this is not necessary an error */
        if valid-handle(hBi) and valid-handle(hSrcBuf) and hBi:table-handle:has-records then
        do:

            /* check if valid key information available (provided or unique index exists) */
            if hBuf:data-source:save-where-string(1) eq ? then
            do:
                ThrowError(100, 'msg_err_data_no_primary_key':u, hBuf:name, ?).
                return false.
            end.

            /* get list of read-only fields to be excepted from assign */
            do iFld = 1 to num-entries(ReadOnlyFields)
                on error undo, throw:
                fldName = trim(entry(iFld, ReadOnlyFields)).
                if num-entries(fldName, '.':u) eq 2 and entry(1, fldName, '.':u) eq hBuf:table then
                do:
                    roFields = substitute('&1,&2':u, roFields, entry(2, fldName, '.':u)).
                end.
                roFields = trim(roFields, ',':u).
            end.

            /* get field map for first data source buffer */
            if hBuf:data-source:num-source-buffers eq 1 then
                fieldMap = hBuf:data-source-complete-map.
            else
                fieldMap = getBufferFieldMap(hSrcBuf:name, hBuf:data-source-complete-map).

            assign
                saveMap  = updateFieldMap (fieldMap, roFields)
                fieldMap = updateFieldMap (fieldMap, hSrcBuf:private-data).

            create query hQry .
            hQry:set-buffers(hBi) .
            hQry:forward-only = true.
            hQry:query-prepare(substitute('for each &1':u, hBi:name)) .
            hQry:query-open() .

            update_block:
            do while hQry:is-open and hQry:get-next(no-lock)
                on error undo, throw:

                hBuf:find-by-rowid(hBi:after-rowid) no-error.

                /* locate the record in the database, nothing to do for new records */
                if hBi:row-state ne row-created then
                do:
                    hSrcBuf:find-first(hBuf:data-source:save-where-string(1), exclusive-lock, no-wait) no-error.

                    /* record not found in database */
                    if not hSrcBuf:available then
                    do:
                        if hSrcBuf:locked then
                            ThrowClientError(101, 'msg_err_data_locked':u, hBuf:name, ?).
                        else
                            ThrowClientError(102, 'msg_err_data_not_found':u, hBuf:name, ?).

                        retVal = false.
                        undo, leave update_block.
                    end.
                end.

                case hBi:row-state:
                    /* new record */
                    when row-created then
                        do:

                            if not BeforeRowSave(hBuf, ?, ?) then
                            do:
                                retVal = false.
                                undo, leave update_block.
                            end.

                            hSrcBuf:buffer-create() .
                            hSrcBuf:buffer-copy(hBuf, roFields, saveMap) no-error.

                            if Util:IsError() then
                            do:
                                if error-status:get-number(1) eq 132 then
                                    ThrowClientError(106, 'msg_err_data_dupe':u, hBuf:name, getDuplicateKeyField(hBuf, hSrcBuf)).
                                else
                                    ThrowClientError(106, 'msg_err_data_update':u, hBuf:name, ?).

                                retVal = false.
                                undo, leave update_block.
                            end.

                            if not AfterRowSave(hBuf, hSrcBuf) then
                            do:
                                retVal = false.
                                undo, leave update_block.
                            end.

                            hSrcBuf:buffer-validate() no-error.

                            if Util:IsError() then
                            do:
                                if error-status:get-number(1) eq 132 then
                                    ThrowClientError(103, 'msg_err_data_dupe':u, hBuf:name, getDuplicateKeyField(hBuf, hSrcBuf)).
                                else
                                    ThrowClientError(103, 'msg_err_data_create':u, hBuf:name, ?).

                                retVal = false.
                                undo, leave update_block.
                            end.

                            startRow = string(hSrcBuf:rowid).

                            /* refresh client record, in case any database trigger updated other fields */
                            hBuf:buffer-copy(hSrcBuf, hSrcBuf:private-data, fieldMap) .

                            /* for new record update the rowid for the client, we only have the rowid of updated buffer now */
                            DataContext:setDbRowid(hBuf, startRow).

                        end.

                    /* delete record */
                    when row-deleted then
                        do:

                            if not BeforeRowDelete(hBi, hSrcBuf) then
                            do:
                                retVal = false.
                                undo, leave update_block.
                            end.

                            hSrcBuf:buffer-delete() no-error.
                            if Util:IsError() then
                            do:
                                ThrowClientError(104, 'msg_err_data_delete':u, hBuf:name, ?).
                                retVal = false.
                                undo, leave update_block.
                            end.

                            if not AfterRowDelete(hBi) then
                            do:
                                retVal = false.
                                undo, leave update_block.
                            end.
                        end.

                    /* update record */
                    when row-modified then
                        do:
                            if not BeforeRowSave(hBuf, hBi, hSrcBuf) then
                            do:
                                retVal = false.
                                undo, leave update_block.
                            end.

                            retVal = hSrcBuf:buffer-compare(hBi, 'binary':u, roFields, saveMap, true) .

                            /* compare before-image with 'current' data */
                            if retVal eq false then
                            do:
                                ThrowClientError(105, 'msg_err_data_compare':u, hBuf:name, GetModifiedFields (hSrcBuf, hBi)).
                                undo, leave update_block.
                            end.

                            /* save changes */
                            hSrcBuf:buffer-copy(hBuf, roFields, saveMap) no-error.
                            if Util:IsError() then
                            do:
                                if error-status:get-number(1) eq 132 then
                                    ThrowClientError(106, 'msg_err_data_dupe':u, hBuf:name, getDuplicateKeyField(hBuf, hSrcBuf)).
                                else
                                    ThrowClientError(106, 'msg_err_data_update':u, hBuf:name, ?).

                                retVal = false.
                                undo, leave update_block.
                            end.

                            if not AfterRowSave(hBuf, hSrcBuf) then
                            do:
                                retVal = false.
                                undo, leave update_block.
                            end.

                            hSrcBuf:buffer-validate() no-error.

                            if Util:IsError() then
                            do:
                                if error-status:get-number(1) eq 132 then
                                    ThrowClientError(106, 'msg_err_data_dupe':u, hBuf:name, getDuplicateKeyField(hBuf, hSrcBuf)).
                                else
                                    ThrowClientError(106, 'msg_err_data_update':u, hBuf:name, ?).

                                retVal = false.
                                undo, leave update_block.
                            end.

                            /* get full rowid in case datasource has more than one buffer */
                            startRow = DataContext:getDbRowid(hBuf).

                            /* refresh client record, in case any database trigger updated other fields */
                            hBuf:buffer-copy(hSrcBuf, hSrcBuf:private-data, fieldMap) .

                        end.
                end case.
            end.

            if retVal eq false then
                return false.

            /* set new position to latest changed record */
            if startRow ne '':u then
            do:
                if not valid-handle(datasetHandle:get-buffer-handle(hBuf:table)) then
                    return false.
                DataContext:setNewPosition(hBuf:table, startRow).
            end.
        end.

        do iRel = 1 to hBuf:num-child-relations:
            hRel = hBuf:get-child-relation(iRel) .

            if not saveTable(hRel:child-buffer) then
                return false.
        end.

        hSrcBuf:find-current (no-lock) no-error.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
        finally:
            if valid-handle(hQry) then
            do:
                hQry:query-close() .
                delete object hQry .
            end.
        end finally.

    end method.


    method public logical BeforeLoadData ():
        return true.
    end method.


    method public logical AfterLoadData ():
        return true.
    end method.


    method public logical BeforeSaveData ():
        return true.
    end method.


    method public logical AfterSaveData ():
        return true.
    end method.


    method public logical BeforeRowSave (hBuff as handle, hBuffBi as handle, hDbBuff as handle):
        return true.
    end method.


    method public logical AfterRowSave (hBuff as handle, hDbBuff as handle):
        return true.
    end method.


    method public logical BeforeRowDelete (hBuffBi as handle, hDbBuff as handle):
        return true.
    end method.


    method public logical AfterRowDelete  (hBuffBi as handle):
        return true.
    end method.

    method public final character GetLogicalFieldName (physicalName as character):

        define variable hBuf      as handle    no-undo.
        define variable hSrcBuf   as handle    no-undo.
        define variable numBuf    as integer   no-undo.
        define variable numSrcBuf as integer   no-undo.
        define variable fieldName as character no-undo.
        define variable fieldMap  as character no-undo.

        do numbuf = 1 to datasetHandle:num-buffers:
            hBuf = datasetHandle:get-buffer-handle (numBuf).
            if not valid-handle(hBuf) then next.

            fieldMap = hBuf:data-source-complete-map.
            if fieldMap eq ? then next.

            if index(physicalName, '.') eq 0 then
            do numSrcBuf = 1 to hBuf:data-source:num-source-buffers:
                hSrcBuf = hBuf:data-source:get-source-buffer (numSrcBuf).
                if valid-handle(hSrcBuf) then
                physicalName = substitute('&1.&2', hSrcBuf:name, physicalName).

                if lookup(physicalName, fieldMap) gt 0 then leave.
            end.

            if lookup(physicalName, fieldMap) = 0 then next.
            fieldName = entry(lookup(physicalName, fieldMap) - 1, fieldMap).
            return fieldName.
        end.

        return ?.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.


    method public final character GetPhysicalFieldName (logicalName as character):

        define variable hBuf      as handle    no-undo.
        define variable fieldName as character no-undo.
        define variable bufNum    as integer   no-undo.

        case num-entries(logicalName, '.':u):
            when 1 then
                do bufNum = 1 to datasetHandle:num-buffers:
                    hBuf = datasetHandle:get-buffer-handle(bufNum).
                    fieldName = getPhysicalFieldName(hBuf, logicalName).
                    if fieldName ne ? then
                        return fieldName.
                end.
            when 2 then
                do:
                    hBuf = datasetHandle:get-buffer-handle(entry(1, logicalName, '.':u)).

                    return getPhysicalFieldName(hBuf, logicalName).
                end.
        end case.

        return ?.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.


    method private character getPhysicalFieldName (hBuf as handle, logicalName as character):

        define variable fieldMap as character no-undo.

        if not valid-handle(hBuf) then
            return ?.

        if index(logicalName, '.') eq 0 then
            logicalName = substitute('&1.&2', hBuf:name, logicalName).

        assign
            fieldMap = hBuf:data-source-complete-map no-error.

        if fieldMap eq ? or lookup(logicalName, fieldMap) = 0 then
            return ?.

        return entry(lookup(logicalName, fieldMap) + 1, fieldMap).

    end method.


    method public final logical SetDataSource (pcTable as character, pcBuffer as character):
        return SetDataSource (pcTable, pcBuffer, ?, ?, ?).
    end method.


    method public final logical SetDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character):
        return SetDataSource (pcTable, pcBuffer, pcKeyFields, ?, ?).
    end method.


    method public final logical SetDataSource
        (pcTable    as character, pcBuffer as character,
        pcKeyFields as character, pcWhere  as character):

        return SetDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, ?).

    end method.


    method public final logical SetDataSource
        (pcTable    as character, pcBuffer as character,
        pcKeyFields as character, pcWhere  as character,
        pcFieldsMap as character):

        return SetDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, ?).

    end method.


    method public final logical SetDataSource
        (pcTable    as character, pcBuffer as character,
        pcKeyFields as character, pcWhere  as character,
        pcFieldsMap as character, excludeFields as character):

        return SetDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, ?).

    end method.


    method public final logical SetDataSource
        (pcTable    as character, pcBuffer as character,
        pcKeyFields as character, pcWhere  as character,
        pcFieldsMap as character, excludeFields as character, includeFields as character):

        return SetDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, includeFields, ?).

    end method.


    method public final logical SetDataSource
        (pcTable    as character, pcBuffer as character,
        pcKeyFields as character, pcWhere  as character,
        pcFieldsMap as character, excludeFields as character, includeFields as character, joinClause as character):

        define variable hBuffer as handle no-undo.

        if not valid-handle(datasetHandle)
        then return false.

        hBuffer = datasetHandle:get-buffer-handle (pcTable) no-error.

        if not valid-handle (hBuffer) then
        return false.

        /* reset buffer data-source if already set */
        DetachDataSource(hBuffer:table).

        if not AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, includeFields, joinClause)
        then return false.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public final logical AddDataSource (pcTable as character, pcBuffer as character):
        return AddDataSource (pcTable, pcBuffer, ?, ?, ?, ?, ?, ?, false).
    end method.

    method public final logical AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character):
        return AddDataSource (pcTable, pcBuffer, pcKeyFields, ?, ?, ?, ?, ?, false).
    end method.

    method public final logical AddDataSource
        (pcTable    as character, pcBuffer    as character,
        pcKeyFields as character, pcWhere     as character):

        return AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, ?, ?, ?, ?, false).

    end method.

    method public final logical AddDataSource
        (pcTable    as character, pcBuffer    as character,
        pcKeyFields as character, pcWhere     as character,
        pcFieldsMap as character):

        return AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, ?, ?, ?, false).

    end method.


    method public final logical AddDataSource
        (pcTable    as character, pcBuffer      as character,
        pcKeyFields as character, pcWhere       as character,
        pcFieldsMap as character, excludeFields as character):

        return AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, ?, ?, false).

    end method.


    method public final logical AddDataSource
        (pcTable     as character, pcBuffer     as character,
        pcKeyFields as character, pcWhere       as character,
        pcFieldsMap as character, excludeFields as character, includeFields as character):

        return AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, includeFields, ?, false).

    end method.

    method public final logical AddDataSource
        (pcTable      as character, phBuffer      as handle,
        pcKeyFields   as character, pcWhere       as character,
        pcFieldsMap   as character, excludeFields as character,
        includeFields as character, joinClause    as character):

        return AddDataSource (pcTable, phBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, includeFields, joinClause, false).

    end method.

    method private character GetFieldMappings(input hBuffer as handle, input hSrcBuffer as handle):

    	define variable iNumFld		as integer		no-undo.
    	define variable cFieldsMap	as character	no-undo.
    	define variable cFldName	as character	no-undo.
    	define variable hFld		as handle		no-undo.
    	define variable cVal		as character	no-undo.

    	if not valid-handle(hBuffer) or
    		not valid-handle(hSrcBuffer)
    	then return ?.

    	if hSrcBuffer:dbname = 'PROGRESST'
    	then return ?.

    	do iNumFld = 1 to hSrcBuffer:num-fields
            on error undo, throw:

    		cFldName = hSrcBuffer:buffer-field (iNumFld):name.

    		hFld = hBuffer:buffer-field (cFldName) no-error.

    		if valid-handle(hFld)
    		then do:
    			cVal = substitute('&1.&2,&3.&4':u, hSrcBuffer:name, cFldName, hBuffer:name, hFld:name).

    			cFieldsMap	=	if Util:IsEmpty(cFieldsMap)
    							then cVal
    							else substitute('&1,&2':u, cFieldsMap, cVal).
    		end.

    	end. /* do iNumFld = 1 to hSrcBuffer:num-fields */

    	return cFieldsMap.

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method public final logical AddDataSource(
    	pcTable			as character,
        phBuffer		as handle,
        pcKeyFields		as character,
        pcWhere			as character,
        pcFieldsMap		as character,
        excludeFields	as character,
        includeFields	as character,
        joinClause		as character,
        joinType		as logical):

        define variable hBuffer     as handle    no-undo.
        define variable hSrcBuffer  as handle    no-undo.
        define variable hDataSource as handle    no-undo.
        define variable hQuery      as handle    no-undo.
        define variable numBuf      as integer   no-undo.
        define variable cQryString  as character no-undo.
        define variable lPrepareOk  as logical   no-undo.

        hSrcBuffer = phBuffer.

        if not valid-handle (hSrcBuffer) or
            not valid-handle(datasetHandle)
        then return false.

        pcTable = trim(pcTable).

        assign hBuffer = datasetHandle:get-buffer-handle (pcTable) no-error.

        if not valid-handle (hBuffer)
        then return false.

        assign
            pcFieldsMap = trim(Util:Nvl(pcFieldsMap, '':u))
            pcWhere     = trim(Util:Nvl(pcWhere, '':u)).

        if Util:IsEmpty(pcFieldsMap)
        then do:
            pcFieldsMap = GetFieldMappings(input hBuffer, input hSrcBuffer).

            message 'pcFieldsMap' pcFieldsMap
        view-as alert-box.
        end.



        assign hDataSource = hBuffer:data-source no-error.

        /* exclude fields list are set on source's buffer private-data */
        assign
            cQryString              = substitute('for each &1 no-lock':u, hSrcBuffer:name)
            hSrcBuffer:private-data = getBufferExcludeFields (hBuffer, excludeFields, includeFields).

        if not valid-handle(hDataSource)
        then do:
        	create data-source hDataSource.

        	hDataSource:private-data = 'dynamic':U.
        end.
        else do:
            if hDataSource:num-source-buffers gt 0
            then do:
                do numBuf = 1 to hDataSource:num-source-buffers:
                    if hDataSource:get-source-buffer (numBuf):name eq hSrcBuffer:name
                    then do:
                        if hSrcBuffer:dynamic
                        then delete object hSrcBuffer no-error.

                        return true.
                    end.
                end.

                if valid-handle(hDataSource:query)
                then cQryString  = substitute('&1, &2 &3 no-lock':u, trim(hDataSource:query:prepare-string, ' .':u), if joinType then 'first' else 'each',hSrcBuffer:name).

                pcFieldsMap = trim(substitute('&1,&2', hBuffer:data-source-complete-map, pcFieldsMap), ',':u).

                hBuffer:detach-data-source().

            end. /* if hDataSource:num-source-buffers gt 0 */
        end.

        hDataSource:add-source-buffer (hSrcBuffer, pcKeyFields) no-error.

        if Util:IsError() then
        do:
            DetachDataSource(pcTable).

            ThrowError(100, 'msg_err_data_invalid_source':u, pcTable, ?).
            undo, throw new Progress.Lang.AppError(error-status:get-message (1), error-status:get-number (1)).
        end.

        if not valid-handle(hBuffer:data-source)
        then hBuffer:attach-data-source(hDataSource, pcFieldsMap, hSrcBuffer:private-data).

        if not valid-handle(hDataSource:query)
        then do:
            create query hQuery.

            do numBuf = 1 to hDataSource:num-source-buffers:
                hQuery:add-buffer(hDataSource:get-source-buffer(numBuf)).
            end.

            hDataSource:query = hQuery.
        end.
        else do:
            /* sometime the add-source-buffer method on data source does not add the new buffer to the query */
            if hDataSource:query:num-buffers lt hDataSource:num-source-buffers
            then hDataSource:query:add-buffer(hSrcBuffer).
        end.

        hQuery = hDataSource:query.

        lPrepareOk = hQuery:query-prepare (cQryString) no-error.

        if Util:IsError() or
            not lPrepareOk
        then do:
            DetachDataSource(pcTable).

            ThrowError(100, 'msg_err_data_invalid_query':u, pcTable, cQryString).
            undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
        end.

        if not Util:IsEmpty(pcWhere)
        then
            if not AddWhereClause(pcTable, hSrcBuffer:name, pcWhere)
            then do:
                DetachDataSource(pcTable).

                return false.
            end.

        if not Util:IsEmpty(joinClause)
        then
            if not AddJoinClause(pcTable, hSrcBuffer:name, joinClause)
            then do:
                DetachDataSource(pcTable).

                return false.
            end.

        return true.

        catch appError as Progress.Lang.Error :

            DetachDataSource(pcTable).

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method public final logical AddDataSource(
    	pcTable			as character,
    	pcBuffer		as character,
        pcKeyFields		as character,
        pcWhere			as character,
        pcFieldsMap		as character,
        excludeFields	as character,
        includeFields	as character,
        joinClause		as character,
        joinType		as logical):

        define variable hSrcBuffer as handle no-undo.

        pcBuffer = trim(pcBuffer).

        create buffer hSrcBuffer for table pcBuffer buffer-name pcBuffer no-error.

        if not valid-handle (hSrcBuffer)
        then return false.

        if not AddDataSource(
            pcTable,
            hSrcBuffer,
            pcKeyFields,
            pcWhere,
            pcFieldsMap,
            excludeFields,
            includeFields,
            joinClause,
            joinType
            )
        then do:
            DetachDataSource(pcTable).

            delete object hSrcBuffer no-error.

            return false.
        end.

        return true.

        catch appError as Progress.Lang.Error :

            DetachDataSource(pcTable).

            delete object hSrcBuffer no-error.

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public final logical AddDataSource(
    	pcTable			as character,
    	pcBuffer		as character,
        pcKeyFields		as character,
        pcWhere			as character,
        pcFieldsMap		as character,
        excludeFields	as character,
        includeFields	as character,
        joinClause		as character):

       	return AddDataSource(pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, includeFields, joinClause, false).

    end method.

    method public logical AttachDataSource ():

        ThrowError(100, 'msg_err_data_no_source':u, ?, ?).

        return false.

    end method.


    method public void DetachDataSource ():

        define variable numBuf as integer no-undo.
        define variable hBuf   as handle  no-undo.

        if not valid-handle(datasetHandle)
        then return.

        do numBuf = 1 to datasetHandle:num-buffers:

            hBuf = datasetHandle:get-buffer-handle(numBuf).

            DetachDataSource(hBuf:table).
        end.

        return.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.

    end method.


    method protected void DetachDataSource (pcTable as character):

        define variable hBuffer as handle no-undo.

        if not valid-handle(datasetHandle)
        then return.

        assign hBuffer = datasetHandle:get-buffer-handle(pcTable) no-error.

        if not valid-handle(hBuffer)
        then return.

        DetachDataSource(hBuffer).

        return.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.

    end method.


    method private void DetachDataSource (input hBuf as handle):

        define variable hDataSource as handle  no-undo.
        define variable hBuffer     as handle  no-undo extent 16.
        define variable hQuery      as handle  no-undo.
        define variable numBuf      as integer no-undo.

        if not valid-handle(hBuf)
        then return.

        assign hDataSource = hBuf:data-source no-error.

        if not valid-handle(hDataSource)
        then return.

        hBuf:detach-data-source() no-error.

		do numBuf = 1 to 16:
            if hBuffer[numBuf] <> ?				and
            	valid-handle(hBuffer[numBuf])	and
                hBuffer[numBuf]:dynamic
            then delete object hBuffer[numBuf] no-error.

            hBuffer[numBuf] = ?.
        end.

        do numBuf = 1 to hDataSource:num-source-buffers:
            hBuffer[numBuf] = hDataSource:get-source-buffer(numBuf).
        end.

        /* delete attached query if dynamicaly created */
        assign hQuery = hDataSource:query no-error.

        if valid-handle(hQuery)
        then do:
            if hQuery:is-open
            then hQuery:query-close ().

            if hQuery:dynamic
            then delete object hQuery no-error.
        end.

        /* delete the source buffers if dinamicaly created */
        do numBuf = 1 to 16:
            if hBuffer[numBuf] <> ?				and
            	valid-handle(hBuffer[numBuf])	and
                hBuffer[numBuf]:dynamic
            then delete object hBuffer[numBuf] no-error.

            hBuffer[numBuf] = ?.
        end.

        if valid-handle(hDataSource) and
            hDataSource:private-data = 'dynamic':U
        then delete object hDataSource no-error.

        return.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.

    end method.


    method public character GetKeyValue
        (input tableName as character, input keyField    as character,
        input keyValue   as character, input lookupField as character,
        input filterXml  as memptr):

        define variable hBuffer      as handle    no-undo.
        define variable hBuf         as handle    no-undo.
        define variable hFldKey      as handle    no-undo.
        define variable hFldDesc     as handle    no-undo.
        define variable fldName      as character no-undo.
        define variable cDescription as character no-undo.

        define variable numKeys      as integer   no-undo.
        define variable qryString    as character no-undo.
        define variable indexFld     as character no-undo.

        if not valid-handle(datasetHandle)
        then return ?.

        DetachDataSource().

        if not AttachDataSource() then
        do:
            ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            return ?.
        end.

        if Util:IsEmpty(keyField) or
            Util:IsEmpty(lookupField)
        then return '':u.

        if Util:IsEmpty(tableName) then
            tableName = datasetHandle:get-buffer-handle(1):table.

        assign hBuf = datasetHandle:get-buffer-handle(tableName) no-error.

        if not valid-handle(hBuf) or
            not valid-handle(hBuf:data-source)
        then return ?.

        hBuffer = hBuf:data-source:get-source-buffer(1).

        hFldKey = hBuffer:buffer-field(keyField) no-error.

        if not valid-handle(hFldKey)
        then do:
            do numKeys = 1 to num-entries(keyField,chr(3)):
                assign
                    fldName  = getPhysicalFieldName(hBuf, trim(entry(numKeys,keyField,chr(3))))
                    indexFld = entry(2, fldName, '.':u)
                    hBuffer  = hBuf:data-source:query:get-buffer-handle(entry(1, fldName, '.':u))
                    hFldKey  = hBuffer:buffer-field(indexFld) .

                if not valid-handle (hFldKey) or
                    hFldKey:key eq false
                then return '':u.

                if numKeys eq 1 then
                    qryString = substitute ('&1 = &2', indexFld, quoter(trim(entry(numKeys, keyValue, chr(3))))).
                else
                    qryString = substitute ('&1 and &2 = &3', qryString, indexFld, quoter(trim(entry(numKeys, keyValue,chr(3))))).
            end.

        end. /* if not valid-handle(hFldKey) */
        else do:
            qryString = substitute ('&1 = &2', keyField, quoter(keyValue)).
        end.

        hFldDesc = hBuffer:buffer-field(lookupField) no-error.

        if not valid-handle(hFldDesc)
        then
            assign
                lookupField = entry(2, getPhysicalFieldName(hBuf, lookupField), '.':u)
                hFldDesc    = hBuffer:buffer-field(lookupField)
                no-error.

        if not valid-handle (hFldDesc)
        then return '':u.

        hBuffer:find-first (substitute ('where &1 &2',
            qryString,
            DataContext:getLookupFilter(tableName, hBuffer:name, filterXml)
            ), no-lock) no-error.

        if hBuffer:available then
            cDescription = string(hFldDesc:buffer-value).
        else cDescription = ?.

        hBuffer:buffer-release().

        return cDescription.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.


    method public com.quarix.base.Collection AutocompleteKey(input tableName   as character, input keyField as character, input keyValue as character):
        return AutocompleteKey(tableName, keyField, keyValue, ?).
    end method.


    method public com.quarix.base.Collection AutocompleteKey
        (input tableName   as character, input keyField as character, input keyValue as character, input filterXml as memptr):

        define variable arrData as com.quarix.base.Collection no-undo.
        define variable hBuffer as handle                     no-undo.
        define variable hBuf    as handle                     no-undo.
        define variable hFld    as handle                     no-undo.
        define variable hQry    as handle                     no-undo.
        define variable fldName as character                  no-undo.
        define variable numRec  as integer                    no-undo initial {&req-var-lookup-max-rec}.


        if Util:IsEmpty(tableName) then
            tableName = datasetHandle:get-buffer-handle(1):table.

        assign
            hBuf    = datasetHandle:get-buffer-handle(tableName)
            hBuffer = hBuf:data-source:get-source-buffer(1)
            hFld    = hBuffer:buffer-field(keyField) .

        if not valid-handle(hFld) then
            assign
                fldName  = getPhysicalFieldName(hBuf, keyField)
                keyField = entry(2, fldName, '.':u)
                hBuffer  = hBuf:data-source:query:get-buffer-handle(entry(1, fldName, '.':u))
                hFld     = hBuffer:buffer-field(keyField) .

        if not valid-handle (hFld) or hFld:data-type ne 'character':u or hFld:key eq false then
            return ?.

        create query hQry.
        hQry:set-buffers(hBuffer).
        hQry:forward-only = true.

        hQry:query-prepare(substitute('for each &1 where &2 begins &3 &4', hBuffer:name, keyField,
            quoter(keyValue), DataContext:getLookupFilter(tableName, hBuffer:name, filterXml))) .
        hQry:query-open() .
        hQry:get-first(no-lock) .

        if hBuffer:available then
        do:
            arrData = new com.quarix.base.Collection().
            do while not hQry:query-off-end and numRec gt 0
                on error undo, throw:
                arrData:Push(string(hFld:buffer-value)).
                numRec = numRec - 1.
                hQry:get-next(no-lock) .
            end.
        end.

        return arrData.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.
        finally:
            if valid-handle(hQry) then
            do:
                hQry:query-close() .
                delete object hQry.
            end.
        end finally.
    end method.


    method protected final void AddPersistence(dataset-handle datasetHdl, variableName as character, includeData as logical):

        AddPersistence (dataset-handle datasetHdl by-reference, variableName, includeData, ?).

        finally:
        	delete object datasetHdl no-error.
        end finally.

    end method.


    method protected final void AddPersistence
        (dataset-handle datasetHdl, variableName as character, includeData as logical, instanceGuid as character):

        define variable mpXmlData   as memptr    no-undo.
        define variable sectionName as character no-undo.


        if not valid-handle(datasetHdl) or not valid-object(ContextManager) then
            return.


        sectionName = substitute('&1&2':u, ID, Util:Nvl(instanceGuid, '':u)).
        datasetHdl:write-xmlschema('memptr':u, mpXmlData, false, 'utf-8':u, false).
        ContextManager:SetValue(sectionName, variableName, mpXmlData).
        set-size(mpXmlData) = 0.

        if not includeData then
            return.

        datasetHdl:write-xml('memptr':u, mpXmlData, false, 'utf-8':u, ?, false, true, true) .
        ContextManager:SetValue(sectionName, substitute('&1-data':u, variableName), mpXmlData).

        set-size(mpXmlData) = 0.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.


    /* build the complete exclude fields list for a buffer given the exclude and include fields list
      if include fields are given the exclude option is not considered anymore, only one option is valid at a time*/
    method private character getBufferExcludeFields (hBuffer as handle, excludeFields as character, includeFields as character):

        define variable numFld as integer no-undo.


        if not valid-handle(hBuffer) then
            return ?.

        if excludeFields eq '' then
            excludeFields = ?.

        if not Util:IsEmpty(includeFields) then
        do:
            excludeFields = ''.

            do numFld = 1 to hBuffer:num-fields:
                if lookup(hBuffer:buffer-field(numFld):name, includeFields) eq 0 then
                    excludeFields = substitute('&1,&2':u, excludeFields, hBuffer:buffer-field(numFld):name).
            end.
        end.

        excludeFields = left-trim(excludeFields, ',':u).

        return excludeFields.

    end method.


    /* get only the field map for the given data source buffer */
    method private character getBufferFieldMap (bufferName as character, completeSourceMap as character):

        define variable numEntry as integer   no-undo.
        define variable fieldMap as character no-undo.


        do numEntry = 1 to num-entries(completeSourceMap) by 2:
            if entry(1, entry(numEntry + 1, completeSourceMap), '.':u) eq bufferName then
                fieldMap = substitute('&1,&2,&3':u, fieldMap, entry(numEntry, completeSourceMap), entry(numEntry + 1, completeSourceMap)).
        end.

        return left-trim (fieldMap, ',':u).

    end method.


    /* update data source complete field map to remove the fields to be excluded
        buffer-copy seems to ignore except list if fields to be excepted are still in field map */
    method private character updateFieldMap (fieldMap as character, excludeFields as character):

        define variable numEntry as integer   no-undo.
        define variable newMap   as character no-undo.
        define variable ttField  as character no-undo.
        define variable bufField as character no-undo.


        do numEntry = 1 to num-entries(fieldMap) by 2:
            assign
                ttField  = entry(numEntry, fieldMap)
                ttField  = entry(num-entries(ttField, '.':u), ttField, '.':u)
                bufField = entry(numEntry + 1, fieldMap)
                bufField = entry(num-entries(bufField, '.':u), bufField, '.':u) .

            if lookup(ttField, excludeFields) eq 0 then
                newMap = substitute('&1,&2,&3', newMap, ttField, bufField).
        end.

        return trim(newMap, ',':u).

    end method.


    method private logical setReadOnlyFields (input-output roFields as character):

        define variable numEntry  as integer   no-undo.
        define variable fieldName as character no-undo.
        define variable hdlBuffer as handle    no-undo.
        define variable hdlField  as handle    no-undo.

        /* set read only fields, dataset handle should have been set by now */
        if not valid-handle(datasetHandle) then
            return false.

        /* if not fully qualified the first buffer is assumed, if not valid field name returns false */
        do numEntry = 1 to num-entries(roFields):
            fieldName = entry(numEntry, roFields).
            if num-entries(fieldName, '.') eq 1 then
                assign
                    hdlBuffer = datasetHandle:get-buffer-handle(1)
                    hdlField  = hdlBuffer:buffer-field(fieldName) .
            else
                assign
                    hdlBuffer = datasetHandle:get-buffer-handle(entry(1, fieldName, '.':u))
                    hdlField  = hdlBuffer:buffer-field(entry(2, fieldName, '.':u)) .

            if not valid-handle(hdlField) then
                return false.

            entry(numEntry, roFields) = substitute('&1.&2':u, hdlBuffer:table, hdlField:name).
        end.

        return true.

    end method.

    method public final void LocalizeDataset (dataset-handle dsHandle):

        define variable numTbl as integer no-undo.
        define variable hTT    as handle  no-undo.

        if not valid-handle(dsHandle)
        then return.

        datasetHandle = dsHandle.

        if valid-object(Localization) then
        do:

            do numTbl = 1 to datasetHandle:num-buffers:
                hTT = datasetHandle:get-buffer-handle(numTbl):table-handle.

                if valid-handle(hTT) then
                do:
                    localizeTable (table-handle hTT by-reference).
                    hTT = hTT:before-table .
                    if valid-handle(hTT) then
                        localizeTable (table-handle hTT by-reference).
                end.

            end.

        end.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
        finally:
        	delete object dsHandle no-error.
        end finally.

    end method.

    method private void localizeTable (table-handle hTT):

        define variable numFld as integer no-undo.
        define variable hBuf   as handle  no-undo.

        if not valid-object(Localization) then
            return.

        hBuf = hTT:default-buffer-handle.

        do numFld = 1 to hBuf:num-fields:
            if hBuf:buffer-field(numFld):data-type ne 'logical':u then
                next.
            hBuf:buffer-field(numFld):format = Localization:GetFormat('logical':u, hBuf:buffer-field(numFld):format).
        end.

    end method.


    method private logical needFill (hBuf as handle):

        if not valid-object(DataRequest) or
            DataRequest:HasRequest(hBuf:table)
        then return true.

        if valid-handle(hBuf:parent-relation) then
            return needFill(hBuf:parent-relation:parent-buffer).

        return false.

    end method.


    method public final void SetDataModel (dtModel as DataModel):
        if valid-object(dtModel) then
            dataModel = dtModel.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.


    method public final character GetFullFieldName (fieldName as character):

        define variable fieldHandle as handle no-undo.


        fieldName = trim(fieldName).

        case num-entries(fieldName, '.':u):
            when 1 then
                fieldHandle = datasetHandle:get-top-buffer(1):buffer-field(fieldName) no-error.
            when 2 then
                fieldHandle = datasetHandle:get-buffer-handle(entry(1, fieldName, '.':u)):buffer-field(entry(2, fieldName, '.':u)) no-error.
            otherwise
            return ?.
        end case.

        if not valid-handle(fieldHandle) then
            return ?.

        return substitute('&1.&2':u, fieldHandle:table, fieldHandle:name).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method private character updateDataSourceQuery (hBuf as handle, input dataset dscontext):

        define variable hDatasource   as handle    no-undo.
        define variable hSrcBuffer    as handle    no-undo.
        define variable prepareString as character no-undo.
        define variable queryString   as character no-undo.
        define variable bufferQuery   as character no-undo.
        define variable useIndex      as character no-undo.
        define variable iSrcBuf       as integer   no-undo.
        define variable iIndex        as integer   no-undo.
        define variable cQueryWhere   as character no-undo.

        if not valid-handle(hBuf)
        then return ?.

        hDataSource = hBuf:data-source.

        if not valid-handle (hDataSource)
        then return ?.

        if not valid-handle(hDataSource:query)
        then return ?.

        if Util:IsEmpty(hDataSource:query:prepare-string)
        then return ?.

        if not valid-object(DataContext)
        then return ?.

        for each ttFilter
            where ttFilter.fieldName ne ?
              and ttFilter.fieldName <> '':U
            on error undo, throw:

            ttFilter.dbFieldName = GetPhysicalFieldName (substitute('&1.&2':u, ttFilter.tableName, ttFilter.fieldName)).
        end.

        prepareString = trim(replace(hDataSource:query:prepare-string, 'indexed-reposition':u, '':u), ' .':u) .

        if Util:IsEmpty(prepareString)
        then return ?.


        /* pass through all source buffers and update query on each */
        find first ttFilter where ttFilter.tableName eq hBuf:name no-lock no-error.

        if available ttFilter then
        do:

            do iSrcBuf = 1 to hDataSource:num-source-buffers:

                assign
                    hSrcBuffer  = hDataSource:get-source-buffer(iSrcBuf)
                    bufferQuery = getBufferQuery(prepareString, iSrcBuf).

                /* see first if we have an outer-join condition, this have to be taken first */
                for each ttFilter
                    where ttFilter.tableName eq hBuf:name
                    and ttFilter.dbFieldName begins substitute('&1.':u, hSrcBuffer:name)
                    and ttFilter.fieldName   eq ?
                    and ttFilter.fieldValue  begins 'outer-join':u
                    on error undo, throw:

                    bufferQuery = substitute('&1 &2', bufferQuery, NormalizeQuery(ttFilter.fieldValue, ttFilter.operName)).
                    /* we can only have/use one outer-join... */
                    leave.
                end.

                /* all filters set on this source buffer, skip all that have outer-join option */
                for each ttFilter
                    where ttFilter.tableName   eq hBuf:name
                    and ttFilter.dbFieldName begins substitute('&1.':u, hSrcBuffer:name)
                    and (ttFilter.fieldName  ne ?
                    or not ttFilter.fieldValue begins 'outer-join':u)
                    on error undo, throw:

                    bufferQuery = substitute('&1 &2 &3':u, bufferQuery,
                        if lookup('where':u, bufferQuery, ' ':u) eq 0 then
                        'where':u
                        else
                        'and':u,
                        if ttFilter.fieldName eq ? then
                        NormalizeQuery(ttFilter.fieldValue, ttFilter.operName)
                        else
                        DataContext:getFieldFilterCondition(NormalizeQuery(ttFilter.dbFieldName), NormalizeQuery(ttFilter.operName), NormalizeQuery(ttFilter.fieldValue, ttFilter.operName))).
                end.

                queryString = substitute('&1 &2 &3':u, queryString, if queryString eq '':u then '':u else ', ':u, bufferQuery).
            end.
        end.
        else
            queryString = prepareString.

        useIndex = DataContext:getUseIndex(hBuf:name).

        if not Util:IsEmpty(useIndex) then
            queryString = substitute ('&1 use-index &2':u, queryString,useIndex).

        /* all sort options set on buffer */
        for each ttSort
            where ttSort.tableName eq hBuf:name
            on error undo, throw:

            queryString = substitute('&1 by &2 &3':u, queryString,
                NormalizeQuery(GetPhysicalFieldName (substitute('&1.&2':u, ttSort.tableName, ttSort.fieldName))),
                if ttSort.descOrder then 'descending':u else '':u).
        end.

        return queryString.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method private character getBufferQuery (queryString as character, input iSrcBuf as integer):

        define variable indexPos   as integer   no-undo.
        define variable escapeNext as logical   no-undo.
        define variable quoteChar  as character no-undo.
        define variable thisChar   as character no-undo initial ?.
        define variable iNumBuf    as integer   no-undo.
        define variable iBufPos    as integer   no-undo.
        define variable iNumChar   as integer   no-undo.

        if index(queryString, ',') = 0 then
            return substring(queryString, 1, -1, 'character':u).

        if iSrcBuf = 0 or iSrcBuf = ? then
            return substring(queryString, 1, -1, 'character':u).

        do indexPos = 1 to length(queryString, 'character':u):

            thisChar = substring(queryString, indexPos, 1, 'character':u).

            iNumChar = iNumChar + 1.

            if quoteChar eq '':u and thisChar eq ','
                then
            do:
                iNumBuf = iNumBuf + 1.

                if iNumBuf = iSrcBuf
                    then
                do:
                    iNumChar = iNumChar - 1.

                    leave.
                end.
                else
                    assign
                        iNumChar = 0
                        iBufPos  = indexPos.
            end.

            if not escapeNext and index('~'"', thisChar) gt 0 then
            do:

                if quoteChar eq thisChar then
                    quoteChar = '':u.
                else
                    if quoteChar eq '':u then
                        quoteChar = thisChar.
                next.

            end.

            escapeNext = thisChar eq '~~':u.

        end.

        if thisChar eq ? then
            return ?.

        assign
            thisChar = trim(replace(substring(queryString, iBufPos + 1, iNumChar), 'indexed-reposition':u, '':u)).

        return thisChar.

    end method.

    method public character GetQueryName():

        if valid-object(DataContext)
        then return DataContext:GetQueryName().

        return ?.

    end method.

    method public handle GetIdListTmpHandle():

        if valid-object(DataContext)
        then return DataContext:GetIdListTmpHandle().

        return ?.

    end method.

    method private logical GetIndexInformation():

        define variable iNumBuffers as integer no-undo.
        define variable hBuf        as handle  no-undo.
        define variable lRetVal     as logical no-undo.

        if not valid-handle(datasetHandle)
        then return false.

        dataset dsIndexInformation:empty-dataset ().

        #transaction:
        do transaction
            on error undo, throw
            on stop undo:

            do iNumBuffers = 1 to datasetHandle:num-buffers
                on error undo, throw:

                hBuf = datasetHandle:get-buffer-handle (iNumBuffers).

                if not needFill(hBuf)
                then next.

                lRetVal = Util:GetIndexInformation(input hBuf, input dataset dsindexinformation by-reference).

                if not lRetVal
                then undo #transaction, leave #transaction.

            end. /* do iNumBuffers = 1 to datasetHandle:num-buffers */

        end. /* do transaction */

        return lRetVal.

        catch appError as Progress.Lang.Error :

            dataset dsIndexInformation:empty-dataset ().

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private logical PrepareDynTT (input hBuf as handle):

        if not valid-handle(hBuf)                      or
           not valid-handle(hBuf:table-handle)         or
           hBuf:table-handle:type <> 'TEMP-TABLE':U    or
           not valid-handle(hBuf:data-source)
        then return false.

        create temp-table hTtTemp.

        hTtTemp:create-like (hBuf).

        hTtTemp:temp-table-prepare (hBuf:table-handle:name).

        hBufTtTemp = hTtTemp:default-buffer-handle.

        if not Util:GetIndexInformation(hBuf, input dataset dsindexinformation by-reference)
        then return false.

        find first ttIndexInformation
            where ttIndexInformation.BufferHandle = hBuf
              and ttIndexInformation.UniqueIndex
            no-lock no-error.

        if not available(ttIndexInformation)
        then return false.

        return true.
    end method.

    method private logical CheckIfRecordAvailable(input hBuf as handle):

        define variable iNumBuf      as integer   no-undo.
        define variable hSrcBuf      as handle    no-undo.
        define variable hFieldTtTemp as handle    no-undo.
        define variable cCondition   as character no-undo.
        define variable cFieldValue  as character no-undo.
        define variable cWhere       as character no-undo.
        define variable lAvailable   as logical   no-undo.

        if not valid-handle(hBuf)       or
           not valid-handle(hBufTtTemp) or
           not valid-handle(hBuf:data-source)
        then return false.

        hBufTtTemp:empty-temp-table ().
        hBufTtTemp:buffer-create().

        /* get field map for each data source buffers */
        do iNumBuf = 1 to hBuf:data-source:num-source-buffers:
            assign hSrcBuf = hBuf:data-source:get-source-buffer(iNumBuf).

            /* exclude fields list are set on source's buffer private-data */
            if hSrcBuf:available
            then hBufTtTemp:buffer-copy(hSrcBuf, hSrcBuf:private-data, fieldMap[iNumBuf]).

        end. /* do iNumBuf = 1 to hBuf:data-source:num-source-buffers */

        for each ttIndexInformation
            where ttIndexInformation.BufferHandle = hBuf
              and ttIndexInformation.UniqueIndex
              and ttIndexInformation.IndexName <> 'PK_sortOrder':U
            no-lock:

            hBufTtTemp:find-first ().

            cWhere = '':U.

            for each ttIndexFields
                where ttIndexFields.IndexId = ttIndexInformation.IndexId
                no-lock:

                    hFieldTtTemp = hBufTtTemp:buffer-field (ttIndexFields.FieldName).

                    if hFieldTtTemp:buffer-value() = ?
                    then cFieldValue = '?':U.
                    else
                        assign
                            cFieldValue = string(hFieldTtTemp:buffer-value())
                            cFieldValue = quoter(cFieldValue).

                    cCondition = substitute('&1.&2 = &3':U, hBuf:name, ttIndexFields.FieldName, cFieldValue).

                    if Util:IsEmpty(cWhere)
                    then cWhere = cCondition.
                    else cWhere = substitute('&1 and &2':U, cWhere, cCondition).

            end. /* for each ttIndexFields*/

            lAvailable = hBuf:find-first (substitute ('where &1',cWhere),no-lock) no-error.

            if lAvailable
            then do:
                return true.
            end.

        end. /* for each ttIndexInformation */

        return false.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method private character GetSourceBufferDbRowID(input hSrcBuf as handle):

        define variable cRowId as character no-undo.

        if not valid-handle(hSrcBuf)
        then return ?.

        if valid-handle(hSrcBuf:table-handle) and
            hSrcBuf:table-handle:type ='TEMP-TABLE':U
        then
        	cRowId = Util:GetSourceBufferDbRowIDTempTable(input hSrcBuf).
        else
            cRowId  =   if hSrcBuf:available
                        then string(hSrcBuf:rowid)
                        else '?':u.

        return cRowId.

        catch appError as Progress.Lang.Error:
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method private character GetRecordDbRowId(input hBuf as handle):

        define variable iNumBuf  as integer   no-undo.
        define variable recRowid as character no-undo.
        define variable hSrcBuf  as handle    no-undo.
        define variable cRowId   as character no-undo.

        if not valid-handle(hBuf) or
            not valid-handle(hBuf:data-source)
        then return ?.

        do iNumBuf = 1 to hBuf:data-source:num-source-buffers:

            assign hSrcBuf = hBuf:data-source:get-source-buffer(iNumBuf).

            cRowId = GetSourceBufferDbRowID(hSrcBuf).

            if Util:IsEmpty(cRowId)
            then return ?.

            if Util:IsEmpty(recRowid)
            then recRowid = cRowId.
            else recRowid = substitute('&1,&2':u, recRowid, cRowId).

        end. /* do iNumBuf = 1 to hBuf:data-source:num-source-buffers */

        return recRowid.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method private character ConvertHexValueToRowId(input hSrcBuf as handle, input cHexVal as character):

        define variable rRawValue  as raw       no-undo.
        define variable hTtTemp    as handle    no-undo.
        define variable hBufTtTemp as handle    no-undo.
        define variable iNumFields as integer   no-undo.
        define variable hField     as handle    no-undo.
        define variable cFieldVal  as character no-undo.
        define variable cCondition as character no-undo.
        define variable cWhere     as character no-undo.
        define variable cRowId     as character no-undo.

        if not valid-handle(hSrcBuf)                    or
            not valid-handle(hSrcBuf:table-handle)      or
            hSrcBuf:table-handle:type <> 'TEMP-TABLE':U or
            Util:IsEmpty(cHexVal)
        then return ?.

        rRawValue = hex-decode(cHexVal).

        hTtTemp = Util:CreateTableFromUniqueFields(input hSrcBuf).

        if not valid-handle(hTtTemp)
        then return ?.

        hBufTtTemp = hTtTemp:default-buffer-handle.

        hBufTtTemp:empty-temp-table ().

        if not Util:StoreRawInBuffer(input hBufTtTemp, input rRawValue)
        then return ?.

        hBufTtTemp:find-first () no-error.

        if not hBufTtTemp:available
        then return ?.

        do iNumFields = 1 to hBufTtTemp:num-fields:

            hField = hBufTtTemp:buffer-field (iNumFields).

            if hField:buffer-value() = ?
            then cFieldVal = '?':U.
            else
                assign
                    cFieldVal = string(hField:buffer-value())
                    cFieldVal = quoter(cFieldVal).

            cCondition = substitute('&1.&2 = &3':U, hSrcBuf:name, hField:name, cFieldVal).

            if Util:IsEmpty(cWhere)
            then cWhere = cCondition.
            else cWhere = substitute('&1 and &2':U, cWhere, cCondition).

        end. /* do iNumFields = 1 to hBufTtTemp:num-fields */

        cWhere = substitute('where &1':U, cWhere).

        hSrcBuf:find-first(cWhere) no-error.

        if not hSrcBuf:available
        then return ?.

        cRowId = string(hSrcBuf:rowid).

        return cRowId.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.
        finally:
            delete object hTtTemp no-error.
        end finally.

    end method.

    method private character ConvertDataToDbRowId(input hBuf as handle, input repositionRowid as character):

        define variable iNumBuf  as integer   no-undo.
        define variable hSrcBuf  as handle    no-undo.
        define variable cRowId   as character no-undo.
        define variable recRowid as character no-undo.

        if not valid-handle(hBuf)               or
            not valid-handle(hBuf:data-source)  or
            Util:IsEmpty(repositionRowid)
        then return ?.

        if num-entries(repositionRowid, ',':U) <> hBuf:data-source:num-source-buffers
        then return ?.

        do iNumBuf = 1 to hBuf:data-source:num-source-buffers:

            hSrcBuf = hBuf:data-source:get-source-buffer(iNumBuf).

            cRowId = entry(iNumBuf, repositionRowid, ',':U).

            if cRowId <> '?':U and
                valid-handle(hSrcBuf:table-handle) and
                hSrcBuf:table-handle:type = 'TEMP-TABLE':U
            then do:
                cRowId = ConvertHexValueToRowId(hSrcBuf, cRowId).

                if Util:IsEmpty(cRowId)
                then return ?.
            end.

            if Util:IsEmpty(recRowid)
            then recRowid = cRowId.
            else recRowid = substitute('&1,&2':u, recRowid, cRowId).

        end. /* do iNumBuf = 1 to hBuf:data-source:num-source-buffers */

        return recRowid.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method public logical CheckMandatory(input hBuf as handle, input pcField as character):

		return dataModel:CheckMandatory(hBuf, pcField).

    	catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

    end method.

    method public character NormalizeQuery (input pcQueryString as character):

        return NormalizeQuery (input pcQueryString, '':U).

    end method.

    method public character NormalizeQuery (input pcQueryString as character, input pcOperator as character):
        define variable cExceptList   as character no-undo initial '~~176'.
        define variable cExceptString as character no-undo.
        define variable iNum          as integer   no-undo.

        do iNum = 1 to num-entries (cExceptList):
            cExceptString = entry (iNum, cExceptList).
            if index (pcQueryString, cExceptString) eq 0 then next.
            pcQueryString = Util:RemoveString (pcQueryString, cExceptString).
        end.

        ThrowDebug(100, 'normalizequery').
        ThrowDebug(100, 'operator ' + pcOperator).

        if pcOperator = 'matches':U then do:
            pcQueryString = replace (pcQueryString, '.':U, '~~~~.':U).
            /* pcQueryString = replace (pcQueryString, '*':U, '~~~~*':U). */
        end.

        return pcQueryString.

    end method.

    method protected logical AddFieldMap(input dbfield as dbfield, input ttfield as ttfield):

    	if not valid-object(FieldMapping)
    	then return false.

    	return FieldMapping:AddFieldMap(dbfield, ttfield).

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

    end method.

    method protected character GetFieldMapping():

    	if not valid-object(FieldMapping)
    	then return ''.

    	return FieldMapping:GetFieldMapping().

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return ''.
		end catch.

    end method.

    method protected void ClearFieldMap():

		if not valid-object(FieldMapping)
    	then return.

    	FieldMapping:ClearFieldMap().

	end method.

end class.
