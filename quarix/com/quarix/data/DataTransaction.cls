
 /*------------------------------------------------------------------------
    File        : DataTransaction
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Mon Nov 29 10:44:49 EET 2010
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
   using com.quarix.web.*.

class com.quarix.data.DataTransaction
    inherits DataObject
    implements iDataTransaction, iWebObject, com.quarix.base.iDisposable:

    define private temp-table ttModel no-undo
        field dataObject		as Progress.Lang.Object
        field isMain			as logical
        field dsHandle			as handle
        field requestMapping	as character
        field methodName		as character.

   	constructor public DataTransaction (  ):
		super().
   	end constructor.

   	method public final override logical HandleRequest( input actionName as character, input webRequest as com.quarix.web.Request, input webResponse as com.quarix.web.Response ):

   		define variable lRetValue as logical no-undo.

        assign
            Request					= webRequest
            Response				= webResponse
            Response:EnableCache	= actionName eq ACTION_PAINT and OpenOnInit eq false.

        if not BeforeRequest(actionName)
		then return false.

		lRetValue = true.

        case actionName:
            when ACTION_PAINT
            then lRetValue = HandleRequestPaint().

            when ACTION_DATA
            then lRetValue = HandleRequestData().

            when 'dummydata':u
            then lRetValue = HandleRequestDummyData(false).

            otherwise
                lRetValue = HandleRequestService(actionName).

        end case. /* case actionName */

        AfterRequest(actionName).

        return lRetValue.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

   	end method.

    method private logical HandleRequestPaint ():

        define variable mpResponse     as memptr    no-undo.
        define variable responseFormat as character no-undo.

        set-size(mpResponse) = 0.

        assign
            responseFormat = Request:GetVariable('responseFormat':u)
            mpResponse     = GetDescription(responseFormat, Request:ApplicationPath).

        if mpResponse eq ? or get-size(mpResponse) eq 0 then
            return false.

        assign
            Response:ResponseType = Response:RESPONSE_DIRECT
            Response:ContentType  = 'text/xml':u.

        Response:Out(mpResponse).
        set-size(mpResponse) = 0.

        return true.

        catch appError as Progress.Lang.Error :

        	set-size(mpResponse) = 0.

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private logical HandleRequestChild():

    	define variable dtObject	as com.quarix.data.DataObject	no-undo.
    	define variable dtModel		as com.quarix.data.DataModel	no-undo.

    	for each ttModel:

        	if type-of (ttModel.dataObject,'com.quarix.data.DataTransaction')
            then dtObject = cast(ttModel.dataObject,'com.quarix.data.DataTransaction').
            else
            	if type-of (ttModel.dataObject,'com.quarix.data.DataModel')
            	then do:
            		assign
            			dtObject	= cast(ttModel.dataObject,'com.quarix.data.DataModel')
            			dtModel		= cast(ttModel.dataObject,'com.quarix.data.DataModel').

            		if valid-object(dtModel) and
            			valid-object(dtModel:DataAccess)
            		then dtModel:DataAccess:DataRequest = ?.
            	end.
            	else next.

            if not valid-object(dtObject)
            then return false.

            dtObject:restoreDatasetHandle().

            if valid-object(dtObject:DataContext)
            then dtObject:DataContext:ClearContext().

            dtObject:DataRequest = ?.

            if valid-handle(ttModel.dsHandle)
            then dtObject:datasetHandle = ttModel.dsHandle.

			if ttModel.isMain
			then do:
				if valid-object(DataRequest)
				then do:
					if not ForwardRequest()
					then return false.

            		if not dtObject:HandleRequest('dummydata':U, Request, Response)
            		then return false.
            	end.
            	else dtObject:InitializeDataObject().
            end.
            else dtObject:DataContext:ClearContext().

		end. /* for each ttModel */

		return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private logical HandleRequestDummyData (input lReloadDataRequest as logical):

    	define variable mpReq as memptr no-undo.

    	if not InitializeDataObject()
    	then return false.

        DataRequest = cast(GetInstance('com.quarix.data.DataRequest':u), 'com.quarix.data.DataRequest':u).

        if not valid-object(DataRequest) then do:
            ThrowError(1000, 'msg_err_load_obj':u, ?, ?).
            return false.
        end.

        assign
            DataRequest:ObjectType = 'DataCollection':u
            mpReq = Request:GetBlobVariable('xml':u)
            Response:ResponseType = Response:RESPONSE_DIRECT
            Response:ContentType  = 'application/json':u.

        if get-size(mpReq) gt 0
        then do:
            if lReloadDataRequest
            then do:
                if not DataRequest:Parse(mpReq)
                then return false.
            end.

            set-size(mpReq) = 0.

			/* Store the data into dataset from request */

            if not LoadRequestData()
            then return false.

            /* Store the filters from request into DataCotext */

            if not SetFiltersFromRequest()
            then return false.

            /* Load some session parameters from DataContext into ContextManager */

            SetSessionParameters().

            /* Store different sort options into DataContext */

            if not SetSortFromRequest()
            then return false.

            /* Set the batchsize in DataContext */

            if not SetBatchSizeFromRequest()
            then return false.

            if DataRequest:ActionName = ACTION_DATA_FILL
            then do:
                if not HandleRequestChild()
                then return false.
            end.

        end. /* if get-size(mpReq) gt 0 */

        return true.

        catch appError as Progress.Lang.Error :

        	set-size(mpReq) = 0.

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private character GetTableFromMain(input cSourceTableParam as character):

    	define variable numBuf			as integer						no-undo.
    	define variable hTT				as handle						no-undo.
    	define variable lFound			as logical						no-undo.
    	define variable dtObject		as com.quarix.data.DataObject	no-undo.
    	define variable iNumMapping		as integer						no-undo.
    	define variable cRequestMapping	as character					no-undo.
    	define variable cSourceTable	as character					no-undo.
    	define variable cTargetTable	as character					no-undo.
    	define variable dsHandle		as handle						no-undo.

    	if Util:IsEmpty(cSourceTableParam)
    	then return ?.

    	if not valid-handle(datasetHandle)
    	then return ?.

    	lFound = false.

    	do numBuf = 1 to datasetHandle:num-buffers:

    		hTT = datasetHandle:get-buffer-handle(numBuf):table-handle.

    		if hTT:name = cSourceTableParam
    		then do:
    			lFound = true.

    			leave.
    		end.

    	end. /* do numBuf = 1 to datasetHandle:num-buffers */

    	if not lFound
    	then return ?.

    	find first ttModel
    		where ttModel.isMain
    		no-lock no-error.

    	if not available(ttModel)
    	then return ?.

    	if type-of (ttModel.dataObject,'com.quarix.data.DataTransaction')
		then dtObject = cast(ttModel.dataObject,'com.quarix.data.DataTransaction').
        else
			if type-of (ttModel.dataObject,'com.quarix.data.DataModel')
			then dtObject = cast(ttModel.dataObject,'com.quarix.data.DataModel').
			else return ?.

		if not valid-object(dtObject)
		then return ?.

		if not Util:IsEmpty(ttModel.requestMapping)
		then do:
			lFound = false.

			do iNumMapping = 1 to num-entries(ttModel.requestMapping, '|':U):

    			assign
    				cRequestMapping = entry(iNumMapping, ttModel.requestMapping, '|':U)
    				cRequestMapping = trim(cRequestMapping).

    			if Util:IsEmpty(cRequestMapping) or
	    			num-entries(cRequestMapping, ',':U) <> 2
    			then next.

    			assign
    				cSourceTable = entry(1, cRequestMapping, ',':U)
    				cSourceTable = trim(cSourceTable)
    				cTargetTable = entry(2, cRequestMapping, ',':U)
    				cTargetTable = trim(cTargetTable).

    			if Util:IsEmpty(cSourceTable) or
    				Util:IsEmpty(cTargetTable)
    			then next.

    			if cSourceTableParam = cSourceTable
    			then do:
    				lFound = true.

    				leave.
    			end.

    		end. /* do iNumMapping = 1 to num-entries(ttModel.requestMapping, '|':U) */

    		if lFound
    		then do:
    			dsHandle = dtObject:datasetHandle.

    			if not valid-handle(dsHandle)
    			then return ?.

    			lFound = false.

    			do numBuf = 1 to dsHandle:num-buffers:

    				hTT = dsHandle:get-buffer-handle(numBuf):table-handle.

    				if hTT:name = cTargetTable
    				then do:
    					lFound = true.

    					leave.
    				end.

    			end. /* do numBuf = 1 to dsHandle:num-buffers */

    			if lFound
    			then return cTargetTable.

    		end. /* if lFound */

		end. /* if not Util:IsEmpty(ttModel.requestMapping) */
		else do:
			dsHandle = dtObject:datasetHandle.

    		if not valid-handle(dsHandle)
    		then return ?.

    		lFound = false.

    		do numBuf = 1 to dsHandle:num-buffers:

    			hTT = dsHandle:get-buffer-handle(numBuf):table-handle.

    			if hTT:name = cSourceTableParam
    			then do:
    				lFound = true.

    				leave.
    			end.

    		end. /* do numBuf = 1 to dsHandle:num-buffers */

    		if lFound
    		then return cSourceTableParam.
    		else
    			if dsHandle:num-buffers = 1
    			then do:
    				hTT = dsHandle:get-buffer-handle(1):table-handle.

    				return hTT:name.
    			end.
		end.

    	return ?.

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method private logical ForwardRequest():

    	define variable numBuf			as integer		no-undo.
        define variable hTT				as handle		no-undo.
        define variable cTargetTable	as character	no-undo.

    	if not valid-handle(datasetHandle) or
    		not valid-object(DataRequest)
    	then return false.

    	do numBuf = 1 to datasetHandle:num-buffers:

    		hTT = datasetHandle:get-buffer-handle(numBuf):table-handle.

    		if DataRequest:HasRequest(hTT:name)
    		then do:
    			cTargetTable = GetTableFromMain(hTT:name).

    			if Util:IsEmpty(cTargetTable)
    			then next.

    			if hTT:name <> cTargetTable
    			then DataRequest:copyRequest(hTT:name, cTargetTable).

    		end. /* if DataRequest:HasRequest(hTT:name) */

    	end. /* do numBuf = 1 to datasetHandle:num-buffers */

    	return true.

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private logical HandleRequestData ():

        define variable lRetVal as logical no-undo.

        if not HandleRequestDummyData(true)
        then return false.

        lRetVal = true.

        case DataRequest:ActionName:

            when ACTION_DATA_FILL
            then lRetVal = HandleRequestDataFetch().

            when ACTION_DATA_SAVE
            then lRetVal = HandleRequestDataSave().

            otherwise
                lRetVal = HandleRequestDataService(DataRequest:ActionName).

        end case. /* case DataRequest:ActionName */

        lRetVal = BeforeDataOutput() and lRetVal.

        lRetVal = dataOutput() and lRetVal.

        UnloadInstance(DataRequest).

        return lRetVal.

        catch appError as Progress.Lang.Error :

        	UnloadInstance(DataRequest).

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method protected logical HandleRequestService (actionName as character):
        ThrowError(1000, 'msg_err_no_handler':u, ?, ?).
        return false.
    end method.

    method protected logical HandleRequestDataService(actionName as character):

        ThrowError(1000, 'msg_err_no_handler':u, ?, ?).

        return false.

    end method.

    method private logical HandleRequestDataFetch ():

        if not dataFetch()
        then do:
            if valid-handle(datasetHandle)
            then datasetHandle:empty-dataset().

            if valid-object(DataContext)
            then DataContext:ClearContext().

            return false.
        end.

        return true.

    end method.

    method private logical HandleRequestDataSave ():

        if not dataUpdate()
        then return false.

        return true.

    end method.

    method public override logical DataFetch (output dataset-handle dsHandle):

    	define variable dtObject           as com.quarix.data.DataObject   no-undo.
    	define variable hTT                as handle                       no-undo.
    	define variable dsHandleMain       as handle                       no-undo.
    	define variable cMethodeName       as character                    no-undo.
    	define variable lRetVal            as logical                      no-undo.

    	if not valid-handle(dsHandle)
    	then do:
            ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).

            return false.
        end.

    	dsHandle:empty-dataset().

    	if datasetHandle <> dsHandle
    	then do:
    		datasetHandle = dsHandle.

    		if valid-object(DataRequest)
    		then
    			if not LoadRequestData()
    			then do:
    				restoreDatasetHandle().

    				return false.
    			end.
    	end.

    	if not SetBatchSizeFromContext()
    	then do:
    		restoreDatasetHandle().

    		return false.
    	end.

    	if not BeforeDataFetch()
    	then do:
    		restoreDatasetHandle().

			return false.
    	end.

    	find first ttModel
            where ttModel.isMain = true
            no-lock no-error.

        if available ttModel and
        	valid-object(ttModel.dataObject)
        then do:
        	dtObject = cast(ttModel.dataObject,'com.quarix.data.DataObject').

        	dsHandleMain = ttModel.dsHandle.

        	if valid-handle(dsHandleMain)
        	then do:
        		lRetVal = false.

        		if Util:IsEmpty(ttModel.methodName)
        		then lRetVal = dtObject:dataFetch(output dataset-handle dsHandleMain by-reference).
        		else do:
        			cMethodeName = trim(ttModel.methodName).

        			lRetVal = dynamic-invoke(this-object, cMethodeName, output dataset-handle dsHandleMain by-reference) no-error.

        			if error-status:error
        			then do:
        				lRetVal = dynamic-invoke(dtObject, cMethodeName, output dataset-handle dsHandleMain by-reference) no-error.

        				if error-status:error
        				then lRetVal = dtObject:dataFetch(output dataset-handle dsHandleMain by-reference).
        			end.
				end.

				if lRetVal
				then lRetVal = setBatchInfoFromMain(dtObject).

        		dtObject:restoreDatasetHandle().

        		if dsHandleMain:dynamic
        		then Util:DisposeDataset(dsHandleMain).

        	end. /* if valid-handle(dsHandleMain) */
        	else do:
        	    lRetVal = false.

        		if Util:IsEmpty(ttModel.methodName)
        		then lRetVal = dtObject:dataFetch().
        		else do:
        			cMethodeName = trim(ttModel.methodName).

        			dsHandleMain = dtObject:datasetHandle.

        			lRetVal = dynamic-invoke(this-object, cMethodeName, output dataset-handle dsHandleMain by-reference) no-error.

        			if error-status:error
        			then do:
        				lRetVal = dynamic-invoke(dtObject, cMethodeName) no-error.

        				if error-status:error
        				then lRetVal = dtObject:dataFetch().
        			end.
				end.

				if lRetVal
				then setBatchInfoFromMain(dtObject).
        	end.

        	if not lRetVal
        	then do:
        	    restoreDatasetHandle().

        	    return false.
            end.

        end. /* if available ttModel */

        if not AfterDataFetch()
        then do:
            restoreDatasetHandle().

            return false.
        end.

        restoreDatasetHandle().

        return true.

        catch appError as Progress.Lang.Error :

			ThrowError(input appError).

			delete object appError.

			return false.

        end catch.
        finally:
        	restoreDatasetHandle().
        end finally.

    end method.

    method protected logical setBatchInfoFromMain(input dtObject as com.quarix.data.DataObject):

    	define variable numBuf				as integer		no-undo.
        define variable hTT					as handle		no-undo.
        define variable cTargetTable		as character	no-undo.
        define variable hdsContextHandle	as handle		no-undo.

    	if not valid-object(dtObject)		or
    		not valid-handle(datasetHandle)
    	then return false.

    	do numBuf = 1 to datasetHandle:num-buffers:

    		hTT = datasetHandle:get-buffer-handle(numBuf):table-handle.

    		if (valid-object(DataRequest) and DataRequest:HasRequest(hTT:name)) or
    			not valid-object(DataRequest)
    		then do:
    			cTargetTable = GetTableFromMain(hTT:name).

    			if Util:IsEmpty(cTargetTable)
    			then next.

    			DataContext:setBatchInfo(hTT:name, dtObject:DataContext:getFirstBatch(cTargetTable), dtObject:DataContext:getLastBatch(cTargetTable)).

    			hdsContextHandle = dtObject:DataContext:dsContextHandle.

    			if not DataContext:CopyRowId(cTargetTable, hTT:name, dataset-handle hdsContextHandle by-reference)
    			then return false.

    		end. /* if DataRequest:HasRequest(hTT:name) */

    	end. /* do numBuf = 1 to datasetHandle:num-buffers */

    	return true.

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical BeforeDataFetch ():
        return true.
    end method.

    method public logical AfterDataFetch ():
        return true.
    end method.

    method public logical BeforeDataUpdate ():
        return true.
    end method.

    method public logical AfterDataUpdate ():
        return true.
    end method.

    destructor public DataTransaction ( ):

    	for each ttModel
    		no-lock:
    		UnloadInstance(ttModel.dataObject).
    	end.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.

    end destructor.

    method protected override logical serializeBufferToJson (table-handle tableHandle):
        define variable hBiTable    as handle     no-undo.
        define variable lLastBatch  as logical    no-undo init true.
        define variable lFirstBatch as logical    no-undo init true.
        define variable currentPos  as character  no-undo.
        define variable filterArray as character  no-undo.
        define variable sortArray   as character  no-undo.
        define variable propArray   as character  no-undo.

        if not valid-object(JsonWriter) or not valid-handle(tableHandle) then do:
            return false.
        end.

        /* if we have errors we don't send any thing back beside those */
        if valid-object(ErrorManager) and ErrorManager:GetNumClientErrors(tableHandle:name) >  0 then do:
            /* send the buffer specific errors here */
            serializeErrors(tableHandle:name).
            return true.
        end.

        /* for manualy filled tables always send first/last to true */
        if not valid-handle(tableHandle:default-buffer-handle:data-source) then
            assign
                lLastBatch  = true
                lFirstBatch = true.

        assign
            lLastBatch  = DataContext:getLastBatch(tableHandle:name)
            lFirstBatch = DataContext:getFirstBatch(tableHandle:name)
            .

        /* current image */
        JsonWriter:Out('"rows":[':u).

        if tableHandle:has-records then do:
        	tableHandle:private-data = DataContext:GetPrivateData().
            JsonWriter:Serialize(table-handle tableHandle by-reference, false, ?, '*', ?, true).
        end.

        JsonWriter:Out(']').

        /* before image, if any */
        hBiTable = tableHandle:before-table no-error.
        if valid-handle(hBiTable) and
           hBiTable:has-records then
        do:
            JsonWriter:Out(',"beforeImage":[':u).
            JsonWriter:Serialize(table-handle hBiTable by-reference, false, ?, '*', ?, true).
            JsonWriter:Out(']').
        end.

        JsonWriter:Out(', "info":~{':u).

        JsonWriter:Out(substitute('"changesOnly": &1':u, string(SendChangesOnly, 'true/false':u))).

        JsonWriter:Out(substitute (', "hasFirstRow": &1':u,string(lFirstBatch, 'true/false':u))).
        JsonWriter:Out(substitute (', "hasLastRow": &1':u,string(lLastBatch, 'true/false':u))).

        filterArray =  DataContext:getFilterArray(tableHandle:name).
        if not Util:IsEmpty(filterArray) then do:
            JsonWriter:Out(', ').
            JsonWriter:AddArray("filter", filterArray). /*add filter array in Json*/
        end.

        sortArray =  DataContext:getSortArray(tableHandle:name).
        if not Util:IsEmpty(sortArray) then do:
            JsonWriter:Out(', ').
            JsonWriter:AddArray("sort", sortArray). /*add sort array in Json*/
        end.

        propArray =  DataContext:getPropertyArray().
        if not Util:IsEmpty(propArray) then do:
            JsonWriter:Out(', ').
            JsonWriter:AddArray("options", propArray). /*add properties array in Json*/
        end.

        JsonWriter:Out('~}':u).

        /* send the buffer specific messages here (warning, info)*/
        if valid-object(ErrorManager) and ErrorManager:GetNumMessages(tableHandle:name) > 0 then do:
            /* send the buffer specific errors here */
            serializeErrors(tableHandle:name).
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method protected logical AddDataModel (dataObject as com.quarix.data.DataObject, dsHandle as handle, isMain as logical, cRequestMapping as character, cMethodName as character):

        if not valid-object(dataObject)
        then return false.

        do transaction on error undo, throw:

        	if isMain
        	then do:
        		find first ttModel
    				where ttModel.isMain
    				no-lock no-error.

    			if available(ttModel)
    			then ttModel.isMain = false.
    		end.

        	find first ttModel
        		where ttModel.dataObject = dataObject
        		no-lock no-error.

        	if not available(ttModel)
        	then do:
        		create ttModel.

            	assign ttModel.dataObject = dataObject.
        	end.

			assign
				ttModel.dsHandle		= dsHandle
        		ttModel.isMain			= isMain
        		ttModel.requestMapping	= cRequestMapping
        		ttModel.methodName		= cMethodName.

        end. /* do transaction on error undo, throw */

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method protected logical AddDataModel (dataObject as com.quarix.data.DataObject, dsHandle as handle, isMain as logical, cRequestMapping as character):

    	return AddDataModel (dataObject, dsHandle, isMain, cRequestMapping, ?).

    end method.

    method protected logical AddDataModel (dataObject as com.quarix.data.DataObject, dsHandle as handle, isMain as logical):

    	return AddDataModel (dataObject, dsHandle, isMain, ?, ?).

    end method.

    method protected logical AddDataModel (dataObject as com.quarix.data.DataObject, isMain as logical, cRequestMapping as character, cMethodName as character):

    	return AddDataModel (dataObject, ?, isMain, cRequestMapping, cMethodName).

    end method.

    method protected logical  AddDataModel (dataObject as com.quarix.data.DataObject, isMain as logical, cRequestMapping as character):

    	return AddDataModel (dataObject, ?, isMain, cRequestMapping, ?).

    end method.

    method protected logical AddDataModel (dataObject as com.quarix.data.DataObject, isMain as logical):

    	return AddDataModel (dataObject, ?, isMain, ?, ?).

    end method.

    method protected logical SetMainDataModel(dataObject as com.quarix.data.DataObject, dsHandle as handle, cRequestMapping as character, cMethodName as character):

        if not valid-object(dataObject)
        then return false.

        do transaction on error undo, throw:

            find first ttModel
                where ttModel.isMain
                no-lock no-error.

            if available(ttModel)
            then ttModel.isMain = false.

            find first ttModel
                where ttModel.dataObject = dataObject
                no-lock no-error.

            if not available(ttModel)
            then do:
            	if not AddDataModel (dataObject, dsHandle, true, cRequestMapping, cMethodName)
            	then undo, return false.
            end.
            else
    			assign
    				ttModel.isMain			= true
    				ttModel.dsHandle		= dsHandle
    				ttModel.requestMapping	= cRequestMapping
    				ttModel.methodName		= cMethodName.

    		if not HandleRequestChild()
    		then undo, return false.

    	end. /* do transaction on error undo, throw */

    	return true.

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method protected logical UpdateTtRowId(input hBufTarget as handle, input hBufSrc as handle):
    	return UpdateTtRowId(hBufTarget, hBufSrc, '':U).
    end method.

    method protected logical UpdateTtRowId(input hBufTarget as handle, input hBufSrc as handle, input ttName as character):

    	define variable cttName as character no-undo.

    	if not valid-handle(hBufSrc)		or
    		not valid-handle(hBufTarget)	or
    		not hBufSrc:available			or
    		not hBufTarget:available
    	then return false.

    	if Util:IsEmpty(ttName)
    	then cttName = hBufSrc:name.
    	else cttName = ttName.

    	return DataContext:UpdateTtRowId(cttName, string(hBufTarget:rowid), string(hBufSrc:rowid)).

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public override logical DataUpdate(input-output dataset-handle dsHandle):

    	if not valid-handle(dsHandle)
    	then do:
            ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).

            return false.
        end.

        /* return error if read-only */
        if ReadOnly
        then do:
            ThrowError(100, 'msg_err_data_read-only':u, ?, ?).

            return false.
        end.

    	datasetHandle = dsHandle.

    	if not BeforeDataUpdate()
        then do:
            restoreDatasetHandle().

            return false.
        end.

    	if not SaveData()
    	then do:
    	    restoreDatasetHandle().

    	    return false.
    	end.

    	if not AfterDataUpdate()
        then do:
            restoreDatasetHandle().

            return false.
        end.

    	restoreDatasetHandle().

    	dsHandle:accept-changes().

    	return true.

    	catch appError as Progress.Lang.Error :

			ThrowError(input appError).

			delete object appError.

			return false.

        end catch.
        finally:
        	restoreDatasetHandle().
        end finally.

    end method.

    method protected final override void localizeDataset (input dataset-handle dsHandle):

    	define variable DataAccess as com.quarix.data.DataAccess no-undo.

    	DataAccess = cast(GetInstance('com.quarix.data.DataAccess':u), 'com.quarix.data.DataAccess':u).

    	if not valid-object(DataAccess)
    	then return.

    	DataAccess:localizeDataset(input dataset-handle dsHandle by-reference).

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return.

        end catch.
        finally:
        	UnloadInstance(DataAccess).
        end finally.

    end method.

end class.
