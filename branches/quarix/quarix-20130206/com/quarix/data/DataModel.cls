
 /*------------------------------------------------------------------------
    File        : DataModel
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Mon Nov 29 11:06:23 EET 2010
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
using com.quarix.data.access.*.
using com.quarix.web.*.


&scoped-define req-var-xml-data         'xml':u
&scoped-define req-var-lookup-table     'table':u
&scoped-define req-var-lookup-col       'col':u
&scoped-define req-var-lookup-val       'val':u
&scoped-define req-var-lookup-desc      'desc':u
&scoped-define req-var-lookup-filter    'filter':u
&scoped-define req-var-lookup-max-rec   10


class com.quarix.data.DataModel
    inherits DataObject
    implements iDataModel, iWebObject, com.quarix.base.iDisposable:

    define protected property EnableCache as logical no-undo
        get.
        set.

    define protected property ActionName as character no-undo
        get.
        private set.

    define public property DataAccess as DataAccess no-undo
        get.
        set (dtAccess as DataAccess):
			if valid-object(dtAccess) then
				DataAccess = dtAccess.

			if valid-object(DataAccess) then
				DataAccess:SetDataModel(this-object).

			if valid-object(DataAccess) then
				SetDataContext(DataContext).
        end set.

    constructor public DataModel():
	   super ().
	end constructor.

    destructor public DataModel():

        UnloadInstance(DataContext).
        UnloadInstance(DataAccess).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.

	end destructor.

    method public final integer GetBatchSize (bufferName as character):

        define variable bufferHdl as handle no-undo.


        if valid-handle(datasetHandle) then do:
            bufferHdl = datasetHandle:get-buffer-handle(bufferName) .
            if valid-handle(bufferHdl) then
                return bufferHdl:batch-size.
        end.

        return ?.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.


    method public logical LoadData ():

    	if not valid-object(DataAccess)
    	then do:
    		ThrowError(100, 'msg_err_data_source_return':u, ?, ?).

    		return false.
    	end.

    	DataAccess:DetachDataSource().

        if not DataAccess:AttachDataSource()
        then do:
            ThrowError(100, 'msg_err_data_source_return':u, ?, ?).

            return false.
        end.

        if not valid-object(DataAccess:DataRequest)
        then DataAccess:DataRequest = DataRequest.

        return DataAccess:LoadData().

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).

            delete object appError.

            return false.

        end catch.
        finally:
        	if valid-object(DataAccess)
        	then DataAccess:DetachDataSource().
        end finally.

    end method.


    method public override logical  SaveData ():

    	if not valid-object(DataAccess)
    	then do:
    		ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
    		return false.
    	end.

    	DataAccess:DetachDataSource().

        if not DataAccess:AttachDataSource()
        then do:
            ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            return false.
        end.

        return DataAccess:SaveData().

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public final override logical HandleRequest (methodName as character, webRequest as Request, webResponse as Response):

    	define variable lRetValue as logical no-undo.

        /* enable cache if defined as such or for paint requests when open on init set to false */
        assign
            Request    = webRequest
            Response   = webResponse
            ActionName = methodName
            Response:EnableCache = EnableCache or (methodName eq ACTION_PAINT and OpenOnInit eq false).

        if not BeforeRequest(methodName)
		then return false.

        lRetValue = true.

        case actionName:
             when ACTION_PAINT
            then lRetValue = HandleRequestPaint().

             when ACTION_DATA
            then lRetValue = HandleRequestData().

             when ACTION_LOOKUP
             then lRetValue = HandleRequestLookup().

             when ACTION_AUTOCOMPLETE
             then lRetValue = HandleRequestAutocomplete().

             when ACTION_REPORT
             then lRetValue = HandleRequestReport().

             when 'dummydata':u
             then lRetValue = HandleRequestDummyData(false).

             otherwise
                lRetValue = HandleRequestService().

        end case. /* case actionName */

        AfterRequest(methodName).

        return lRetValue.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical BeforeSaveData ():
        return true.
    end method.


    method public logical AfterSaveData ():
        return true.
    end method.

    method public logical BeforeLoadData ():
        return true.
    end method.

    method public logical AfterLoadData ():
        return true.
    end method.

	/* This method can be used from data transaction objects and pass the parameter by-reference */
    method public override logical dataFetch (output dataset-handle dsHandle):

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

		if not BeforeLoadData()
		then do:
			restoreDatasetHandle().

            return false.
		end.

    	if not LoadData()
		then do:
			restoreDatasetHandle().

            return false.
		end.

		if not AfterLoadData()
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

        	delete object dsHandle no-error.

        end finally.

    end method.

    method public override logical dataUpdate(input-output dataset-handle dsHandle):

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

        do transaction
            on error undo, throw:

            if not BeforeSaveData()	or	/* call developer hook, before data save - leave if canceled */
            	not SaveData()		or	/* leave if error ocured in data save */
                not AfterSaveData()		/* call developer hook, after data save */
            then do:
                restoreDatasetHandle().

                undo, return false.
            end.
        end.

        /* finally accept the dataset changes */

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

        	delete object dsHandle no-error.

        end finally.

    end method.

    method private logical HandleRequestAutocomplete ():

        define variable arrValue   as com.quarix.base.Collection no-undo.
        define variable keyValue   as character                  no-undo.

        if not valid-handle(datasetHandle) then do:
            ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).
            return false.
        end.

        if not valid-object(DataAccess)
    	then do:
    		ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
    		return false.
    	end.

    	DataAccess:DetachDataSource().

        if not DataAccess:AttachDataSource()
        then do:
            ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            return false.
        end.

        if not BeforeLoadData() then
            return false.

        assign
            Response:ResponseType = Response:RESPONSE_DIRECT
            Response:ContentType  = 'application/json':u
            arrValue              = DataAccess:AutocompleteKey (Request:GetVariable({&req-var-lookup-table}),
                                        Request:GetVariable({&req-var-lookup-col}),
                                        Request:GetVariable({&req-var-lookup-val}),
                                        Request:GetBlobVariable({&req-var-lookup-filter})).

        Response:Out('~{"values": [':u).

        if valid-object(arrValue) then do:
            arrValue:GetFirst().
            repeat
                on error undo, throw:
                if arrValue:GetValue(output keyValue) then
                    Response:Out(substitute('&1~{"value": &2~}':u, if arrValue:IndexPosition eq 1 then '':u else ',':u,
                JsonWriter:Quote(keyValue))).

                if not arrValue:GetNext() then
                    leave.
            end.
            delete object arrValue.
        end.

        Response:Out(']~}':u).

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method private logical HandleRequestLookup ():

        if not valid-handle(datasetHandle) then do:
            ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).
            return false.
        end.

        if not valid-object(DataAccess)
    	then do:
    		ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
    		return false.
    	end.

    	DataAccess:DetachDataSource().

        if not DataAccess:AttachDataSource() then do:
            ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            return false.
        end.

        if not BeforeLoadData() then
           return false.

        assign
            Response:ResponseType = Response:RESPONSE_DIRECT
            Response:ContentType  = 'text/html':u.

        Response:Out(GetKeyValue(Request:GetVariable({&req-var-lookup-table}),
            Request:GetVariable({&req-var-lookup-col}),
            Request:GetVariable({&req-var-lookup-val}),
            Request:GetVariable({&req-var-lookup-desc}))).

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical HandleRequestDummyData (input lReloadDataRequest as logical):

		define variable mpReq as memptr no-undo.

		if not InitializeDataObject()
    	then return false.

        DataRequest = cast(GetInstance('com.quarix.data.DataRequest':u), 'com.quarix.data.DataRequest':u).

        if not valid-object(DataRequest) then do:
            ThrowError(1000, 'msg_err_load_obj':u, ?, ?).
            return false.
        end.

        if not valid-object(DataAccess)
        then do:
        	ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            return false.
        end.

        DataAccess:DataRequest = DataRequest.

        DataAccess:DetachDataSource().

        if not DataAccess:AttachDataSource() then do:
            ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            return false.
        end.

        assign
            DataRequest:ObjectType = 'DataCollection':u
            mpReq = Request:GetBlobVariable({&req-var-xml-data})
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

        end. /* if get-size(mpReq) gt 0 */

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

        ActionName = DataRequest:ActionName.

        case ActionName:

            when ACTION_DATA_FILL
            then lRetVal = HandleRequestDataFetch().

            when ACTION_DATA_SAVE
            then lRetVal = HandleRequestDataSave().

            otherwise
                lRetVal = HandleRequestDataService().

        end case. /* case ActionName */

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


    method private logical HandleRequestReport():

        define variable mpReq	as memptr   no-undo.
        define variable lcRes	as longchar	no-undo.
        define variable lRelVal	as logical	no-undo.

        mpReq = Request:GetBlobVariable({&req-var-xml-data}).

        if get-size(mpReq) gt 0 then do:
            if not valid-object(DataRequest) then do:
                set-size(mpReq) = 0.
                ThrowError(1000, 'msg_err_load_obj':u, ?, ?).
                return false.
            end.

            DataRequest:ObjectType = 'DataCollection':u.
            DataRequest:Parse(mpReq).

            set-size(mpReq) = 0.

            if not LoadRequestData()
            then return false.
        end.

        if not valid-object(DataAccess)
        then do:
        	ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            return false.
        end.

        DataAccess:DetachDataSource().

        if not DataAccess:AttachDataSource() then do:
            ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            return false.
        end.

        /* no batch for reports, send all records */
        assign
            Response:ResponseType = Response:RESPONSE_REPORT
            Response:ContentType  = 'text/xml':u
            BatchSize             = 0.

        lRelVal = true.

        lRelVal = HandleRequestDataFetch() and lRelVal.

        lRelVal = BeforeDataOutput() and lRelVal.

        /* switch to american locale to send data, localization done in birt */
        Localization:SetNumericFormat('american':u).
        Localization:SetDateFormat('mdy':u).

        fix-codepage(lcRes) = 'utf-8':u.

        datasetHandle:write-xml('longchar':u, lcRes, false, 'utf-8':u, ?, false, true).
        Response:Out(lcRes).

        UnloadInstance(DataRequest).

        return lRelVal.

        catch appError as Progress.Lang.Error :

        	UnloadInstance(DataRequest).

        	set-size(mpReq) = 0.

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.


    method private logical HandleRequestDataService():

        if not BeforeDataService (ActionName)
        then return false.

        if not HandleRequestDataService(ActionName)
        then return false.

        if not AfterDataService (ActionName)
        then return false.

        return true.

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


    method public logical BeforeDataService (actionName as character):
        return true.
    end method.


    method public logical AfterDataService (actionName as character):
        return true.
    end method.


    method protected logical  HandleRequestDataService (actionName as character):

        ThrowError(1000, 'msg_err_no_handler':u, ?, ?).

        return false.

    end method.


    method private logical HandleRequestService ():

        if not BeforeService (ActionName)
        then return false.

        if not HandleRequestService (ActionName)
        then return false.

        if not AfterService (ActionName)
        then return false.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical BeforeService (actionName as character):
        return true.
    end method.

    method public logical AfterService (actionName as character):
        return true.
    end method.

    method protected logical HandleRequestService (actionName as character):
        ThrowError(1000, 'msg_err_no_handler':u, ?, ?).
        return false.
    end method.

    method private logical HandleRequestPaint ():

        define variable mpResponse     as memptr    no-undo.
        define variable responseFormat as character no-undo.

        if OpenOnInit
        then do:
        	if not valid-object(DataAccess)
        	then do:
        		ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            	return false.
        	end.

        	DataAccess:DetachDataSource().

        	if not DataAccess:AttachDataSource()
        	then do:
            	ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
            	return false.
        	end.
        end. /* if OpenOnInit */

        set-size(mpResponse) = 0.

        assign
            ActionName     = ACTION_DATA_FILL
            responseFormat = Request:GetVariable('responseFormat':u)
            mpResponse     = GetDescription(responseFormat, Request:ApplicationPath).

        if mpResponse eq ? or get-size(mpResponse) eq 0 then
            return false.

        if responseFormat eq 'xml':u then
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


    method private void paintTable (saxWriter as handle, saxAttr as handle, hBuf as handle):

        define variable hTable       as handle    no-undo.
        define variable hFld         as handle    no-undo.
        define variable numFld       as integer   no-undo.
        define variable numRel       as integer   no-undo.
        define variable iExt         as integer   no-undo.
        define variable lcData       as longchar  no-undo.
        define variable mpData       as memptr    no-undo.
        define variable fieldFormat  as character no-undo.
        define variable defaultVal   as longchar  no-undo.

        hTable = hBuf:table-handle.

        fix-codepage(defaultVal) = 'utf-8':u.
        fix-codepage(lcData)     = 'utf-8':u.

        saxWriter:start-element('table').
        saxWriter:insert-attribute('id', lower(hBuf:name)).
        saxWriter:start-element('columnNames').

        /* rowid metadata field */
        saxAttr:update-attribute('name', 'rowid':u).
        saxAttr:update-attribute('type', 'character':u).
        saxWriter:write-empty-element('col', ?, saxAttr).

        /* rowstate metadata field */
        saxAttr:update-attribute('name', 'rowstate':u).
        saxAttr:update-attribute('type', 'integer':u).
        saxAttr:update-attribute('defaultValue', '0').
        saxWriter:write-empty-element('col', ?, saxAttr).

        do transaction
            on error undo, throw:
            hBuf:buffer-create().

            do numFld = 1 to hBuf:num-fields:

                hFld = hBuf:buffer-field(numFld).

                if lookup(hFld:data-type, 'blob') gt 0 then
                    next.

                fieldFormat  =  if hFld:data-type eq 'logical' then
                                    hFld:format
                                else
                                    Localization:GetFormat(hFld:data-type, hFld:format).

                /* use substitute for default value to trap null value '?' */
                if lookup(hFld:data-type, 'character,clob':u) gt 0 then
                    defaultVal = substitute('&1':u, if hFld:extent gt 0 then hFld:buffer-value[1] else hFld:buffer-value).
                else
                    defaultVal = trim(substitute('&1':u, string(if hFld:extent gt 0 then hFld:buffer-value[1] else hFld:buffer-value, hFld:format))).

                /* not realy expected but just te make sure won't blast when converted to string
                update-attribute does not support longchar */
                if hFld:data-type eq 'clob':u and length(defaultVal, 'raw':u) gt 31000 then
                defaultVal = substring(defaultVal, 1, 31000).

                saxAttr:update-attribute('name', lower(hFld:name)).
                saxAttr:update-attribute('format', fieldFormat).
                saxAttr:update-attribute('type', lower(hFld:data-type)).
                saxAttr:update-attribute('sortable', string(hFld:key, 'true/false')).
                saxAttr:update-attribute('defaultValue', string(defaultVal)) no-error.
                saxAttr:update-attribute('required', string(hFld:mandatory, 'true/false')).
                saxAttr:update-attribute('view-as', lower(Util:Nvl(hFld:view-as, 'fill-in':u))).
                saxAttr:update-attribute('label', Util:Nvl(hFld:label, hFld:name)).
                saxAttr:update-attribute('tooltip', Util:Nvl(hFld:help, '')).

                if hFld:extent gt 0 then do iExt = 1 to hFld:extent:
                    saxAttr:update-attribute('name', substitute('&1_&2', lower(hFld:name), iExt)).
                    saxWriter:write-empty-element('col', ?, saxAttr).
                end.
                else
                    saxWriter:write-empty-element('col', ?, saxAttr).
            end.

            hBuf:buffer-delete().

        end.

        saxWriter:end-element('columnNames':u).

        if OpenOnInit and valid-object(JsonWriter) then do:
            JsonWriter:SetDestination(mpData).
            JsonWriter:OpenStream().
            JsonWriter:StartElement('').

            if not serializeBufferToJson(table-handle hTable by-reference) then
                delete object hTable .

            JsonWriter:EndElement('').
            JsonWriter:CloseStream().

            copy-lob mpData to lcData no-convert.
            set-size(mpData) = 0.

            saxWriter:write-data-element('initialValues', lcData).
        end.

        saxWriter:end-element('table').

        do numRel = 1 to hBuf:num-child-relations:
            paintTable (saxWriter, saxAttr, hBuf:get-child-relation(numRel):child-buffer).
        end.

        finally:
            if valid-handle(hTable) then
                delete object hTable .
        end finally.
    end method.


    method protected override logical serializeBufferToJson (table-handle tableHandle):

        define variable hBiTable    as handle     no-undo.
        define variable lLastBatch  as logical    no-undo.
        define variable lFirstBatch as logical    no-undo.
        define variable currentPos  as character  no-undo.
        define variable filterArray as character  no-undo.
        define variable sortArray   as character  no-undo.
        define variable propArray   as character  no-undo.

        if not valid-object(JsonWriter) or not valid-handle(tableHandle) then
            return false.

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
            currentPos  = DataContext:getNewPosition(tableHandle:name).

        /* current image */
        JsonWriter:Out('"rows":[':u).
        if tableHandle:has-records then do:
            tableHandle:private-data = DataContext:GetPrivateData().
            JsonWriter:Serialize(table-handle tableHandle by-reference, false, ?, '*', DataContext:getTableSort(tableHandle:name), true).
        end.

        JsonWriter:Out(']').

        /* before image, if any */
/*        hBiTable = tableHandle:before-table.                                                 */
/*        if valid-handle(hBiTable) and hBiTable:has-records then do:                          */
/*            hBiTable:private-data = DataContext:GetPrivateData().                            */
/*            JsonWriter:Out(',"beforeImage":[':u).                                            */
/*            JsonWriter:Serialize(table-handle hBiTable by-reference, false, ?, '*', ?, true).*/
/*            JsonWriter:Out(']').                                                             */
/*        end.                                                                                 */

        JsonWriter:Out(', "info":~{':u).

        JsonWriter:Out(substitute('"changesOnly": &1':u, string(SendChangesOnly, 'true/false':u))).

        JsonWriter:Out(substitute (', "hasFirstRow": &1':u,string(lFirstBatch, 'true/false':u))).
        JsonWriter:Out(substitute (', "hasLastRow": &1':u,string(lLastBatch, 'true/false':u))).

        if not Util:IsEmpty(currentPos) then
            JsonWriter:Out(substitute(', "newPosition": "&1"':u, currentPos)).

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

    method protected final override void localizeDataset (input dataset-handle dsHandle):

    	DataAccess:localizeDataset(input dataset-handle dsHandle by-reference).

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return.
        end catch.
        finally:
        	delete object dsHandle no-error.
        end finally.

    end method.

    method public character GetKeyValue
        (input tableName as character, input keyField    as character,
        input keyValue   as character, input lookupField as character):

        if valid-object(Request) then
            return GetKeyValue(tableName, keyField, keyValue, lookupField, Request:GetBlobVariable({&req-var-lookup-filter})).

        return GetKeyValue(tableName, keyField, keyValue, lookupField, ?).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.
    end method.


    method public character GetKeyValue
        (input tableName as character, input keyField    as character,
        input keyValue   as character, input lookupField as character,
        input filterXml  as memptr):

        return DataAccess:GetKeyValue(tableName, keyField, keyValue, lookupField, filterXml).
    end method.


    method protected final void AddPersistence
        (dataset-handle datasetHdl, variableName as character, includeData as logical):

        AddPersistence (dataset-handle datasetHdl by-reference, variableName, includeData, ?).
    end method.


    method protected final void AddPersistence
        (dataset-handle datasetHdl, variableName as character, includeData as logical, instanceGuid as character):

        define variable mpXmlData   as memptr    no-undo.
        define variable sectionName as character no-undo.


        if not valid-handle(datasetHdl) or not valid-object(ContextManager) then return.

        sectionName = substitute('&1&2':u, ID, Util:Nvl(instanceGuid, '':u)).
        datasetHdl:write-xmlschema('memptr':u, mpXmlData, false, 'utf-8':u, false).
        ContextManager:SetValue(sectionName, variableName, mpXmlData).
        set-size(mpXmlData) = 0.

        if not includeData then return.

        datasetHdl:write-xml('memptr':u, mpXmlData, false, 'utf-8':u, ?, false, true, true).
        ContextManager:SetValue(sectionName, substitute('&1-data':u, variableName), mpXmlData).

        set-size(mpXmlData) = 0.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.


    method protected final logical LoadPersistence
        (input-output dataset-handle datasetHdl, variableName as character, includeData as logical):

        define variable retVal as logical no-undo.


        retVal =  LoadPersistence (input-output dataset-handle datasetHdl by-reference, variableName, includeData, ?).
        return retVal.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.


    method protected final logical LoadPersistence
        (input-output dataset-handle datasetHdl, variableName as character, includeData as logical, instanceGuid as character):

        define variable mpXmlData   as memptr    no-undo.
        define variable datasetXml  as handle    no-undo.
        define variable retVal      as logical   no-undo.
        define variable sectionName as character no-undo.

        /* for already existent dataset we only load data */
        if valid-handle(datasetHdl) and includeData eq false then return true.

        if valid-object(ContextManager) then do:
            sectionName = substitute('&1&2':u, ID, Util:Nvl(instanceGuid, '':u)).

            if valid-handle(datasetHdl) then do:
                if ContextManager:GetValue(sectionName, substitute('&1-data':u, variableName), output mpXmlData) then do:
                    retVal = datasetHdl:read-xml('memptr':u, mpXmlData, 'empty':u, ?, false).
                    if retVal eq false then
                    ThrowDebug(500, 'msg_err_load_persist_xml':u, '':u, '':u).
                end.
            end.
            else do:
                if ContextManager:GetValue(sectionName, variableName, output mpXmlData) then do:
                    create dataset datasetXml.
                    retVal = datasetXml:read-xmlschema('memptr':u, mpXmlData, false).
                    if retVal eq true then do:
                        datasetHdl = datasetXml.
                        set-size(mpXmlData) = 0.
                        if includeData and ContextManager:GetValue(sectionName, substitute('&1-data':u, variableName), output mpXmlData) then do:
                            retVal = datasetXml:read-xml('memptr':u, mpXmlData, 'empty':u, ?, false).
                            if retVal eq false then
                                ThrowDebug(500, 'msg_err_load_persist_xml':u, '':u, '':u).
                        end.
                    end.

                set-size(mpXmlData) = 0.
                end.
            end.
        end.

        return retVal.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
        finally:
            if valid-handle(datasetXml) then
                delete object datasetXml.
        end finally.
    end method.


    method protected final void RemovePersistence (variableName as character):
        RemovePersistence (variableName, false).
    end method.


    method protected final void RemovePersistence (variableName as character, instanceGuid as character):
        RemovePersistence (variableName, false, instanceGuid).
    end method.


    method protected final void RemovePersistence (variableName as character, onlyData as logical):
        RemovePersistence (variableName, onlyData, ?).
    end method.


    method protected final void RemovePersistence (variableName as character, onlyData as logical, instanceGuid as character):

        define variable sectionName as character no-undo.


        if valid-object(ContextManager) then do:
            sectionName = substitute('&1&2':u, ID, Util:Nvl(instanceGuid, '':u)).
            ContextManager:RemoveValue(sectionName, substitute('&1-data':u, variableName)).
            if onlyData then
                return.
            ContextManager:RemoveValue(sectionName, variableName).
        end.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.

    method public logical BeforeSetFilter (fieldName as character, operName as character, fieldValue as character):
        return true.
    end method.


    method public logical BeforeRemoveFilter (fieldName as character):
        return true.
    end method.


    method public logical BeforeSetSort (fieldName as character, descendingSort as logical):
        return true.
    end method.


    method public logical BeforeRemoveSort (fieldName as character):
        return true.
    end method.


    method public void RemoveFilter (fieldName as character):

        define variable tableName as character no-undo.


        if not valid-handle(datasetHandle) or not BeforeRemoveFilter (fieldName) then
            return.

        assign
            fieldName = DataAccess:getFullFieldName(fieldName)
            tableName = entry(1, fieldName, '.':u)
            fieldName = entry(2, fieldName, '.':u)
            no-error.


        if Util:IsEmpty(fieldName) or Util:IsEmpty(tableName) then
            return.

        RemoveFilter(tableName, fieldName).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.

    method public void RemoveSort (fieldName as character):

        define variable tableName  as  character no-undo.


        if not valid-handle(datasetHandle) or not BeforeRemoveSort (fieldName) then
            return.

        assign
            fieldName = DataAccess:getFullFieldName(fieldName)
            tableName = entry(1, fieldName, '.':u)
            fieldName = entry(2, fieldName, '.':u)
            no-error.

        if Util:IsEmpty(fieldName) or Util:IsEmpty(tableName) then
            return.

        RemoveSort(tableName,fieldName).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.

    method public logical SetSort( input tableName as character, input fieldName as character ):
		return SetSort(tableName,fieldName, false).
	end method.

	method public character GetFilterValue (fieldname as character, operName as character):
        define variable tableName as character no-undo.

        if not valid-handle(datasetHandle) then return ?.

        assign
            fieldName = DataAccess:getFullFieldName(fieldName)
            tableName = entry(1, fieldName, '.':u)
            fieldName = entry(2, fieldName, '.':u)
            no-error.

        if Util:IsEmpty(fieldName) or Util:IsEmpty(tableName) then
            return ?.

        return GetFilterValue(tableName, fieldname, operName).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method public logical SetFilter (fieldName as character, operName as character, fieldValue as character):

        define variable tableName as character no-undo.


        if not valid-handle(datasetHandle) or not BeforeSetFilter (fieldName, operName, fieldValue) then
            return false.

        assign
            fieldName = DataAccess:getFullFieldName(fieldName)
            tableName = entry(1, fieldName, '.':u)
            fieldName = entry(2, fieldName, '.':u)
            no-error.

        if Util:IsEmpty(fieldName) or Util:IsEmpty(tableName) then
            return false.

        return SetFilter (tableName,fieldName,operName,fieldValue).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.


    method public override logical SetFilter (tableName as character, fieldName as character, operName as character, fieldValue as character):

        define variable dbFieldName as character no-undo.

        if Util:IsEmpty(fieldName) or Util:IsEmpty(tableName) then
            return false.

        dbFieldName = DataAccess:GetPhysicalFieldName (substitute('&1.&2':u, tableName, fieldName)).

        return DataContext:SetFilter (tableName,fieldName,operName,fieldValue,dbFieldName).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.


    method public logical    SetSort (fieldName as character):
        return SetSort(fieldName, false).
    end method.


    method public logical    SetSort (fieldName as character, descendingSort as logical):

        define variable tableName  as  character no-undo.


        if not valid-handle(datasetHandle) or not BeforeSetSort (fieldName, descendingSort) then
            return false.

        assign
            fieldName = DataAccess:getFullFieldName(fieldName)
            tableName = entry(1, fieldName, '.':u)
            fieldName = entry(2, fieldName, '.':u)
            no-error.

        if Util:IsEmpty(fieldName) or Util:IsEmpty(tableName) then
            return false.

        return SetSort(tableName, fieldName, descendingSort).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.

    method override public void SetDataContext (input dtContext as DataContext):

        if not valid-object(dtContext) then return.

        DataContext = dtContext.
        DataAccess:DataContext = dtContext.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.

    method public void setUseIndex (tableName as character, useIndex as character):

    	DataContext:setUseIndex(tableName, useIndex).

    end method.

    method public override final logical IsFirstBatch():
        define variable hTT    as handle  no-undo.
        define variable lFirst as logical no-undo init true.

        if valid-handle(datasetHandle) then do:
            htt = datasetHandle:get-buffer-handle(1):table-handle.
            lFirst = DataContext:getFirstBatch(htt:name).
        end.

        return lFirst.
    end method.

    method public override final logical IsLastBatch():
        define variable hTT   as handle  no-undo.
        define variable lLast as logical no-undo init true.

        if valid-handle(datasetHandle) then do:
            htt = datasetHandle:get-buffer-handle(1):table-handle.
            llast = DataContext:getLastBatch(htt:name).
        end.

        return lLast.
    end method.

end class.
