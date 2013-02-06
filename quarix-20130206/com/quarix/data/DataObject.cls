 /*------------------------------------------------------------------------
    File        : DataObject
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Thu May 05 11:35:01 EEST 2011
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


class com.quarix.data.DataObject
    inherits BaseObject
    implements com.quarix.base.iDisposable:

	define public property ACTION_PAINT				as character							no-undo initial 'paint':u
        get.

    define public property ACTION_DATA				as character							no-undo initial 'data':u
        get.

    define public property ACTION_LOOKUP			as character							no-undo initial 'lookup':u
        get.

    define public property ACTION_AUTOCOMPLETE		as character							no-undo initial 'autocomplete':u
        get.

    define public property ACTION_REPORT			as character							no-undo initial 'birtreport':u
        get.

    define public property ACTION_DATA_FILL			as character							no-undo initial 'sendRows':u
        get.

    define public property ACTION_DATA_SAVE			as character							no-undo initial 'submitCommit':u
        get.

    define public property ID						as character							no-undo
      get.
      protected set.

    define public property ReadOnly					as logical								no-undo
      get.
      protected set.

	define public property OpenOnInit				as logical								no-undo
      get.
      set.

    define public property AutoSync					as logical								no-undo   initial true
      get.
      set.

    define public property SendChangesOnly			as logical								no-undo   initial true
      get.
      set.

    define public property SendFilterEveryTime		as logical								no-undo   initial true
      get.
      set.

    define public property UpdateMode				as logical								no-undo
      get.
      set.

    define public property RemoteChildFilter		as logical								no-undo
      get.
      set.

    define public property BatchSize				as integer								no-undo
      get.
      set (numRecords as integer):
         BatchSize = numRecords.
      end set.

    define public property BatchMargin				as integer								no-undo   initial 5
      get.
      set.

    define public property ThreadTimeOut			as integer								no-undo   initial 30000
      get.
      set.

    define private property defaultDatasetHandle	as handle								no-undo
	  private get.
	  private set.

    define public property datasetHandle			as handle								no-undo
      get.
      set (dsHandle as handle):
		if not valid-handle(dsHandle) or
			dsHandle:type ne 'dataset':u
		then return.

		datasetHandle = dsHandle.

		localizeDataset().

		if defaultDatasetHandle = ?
		then defaultDatasetHandle = datasetHandle.

      end set.

    define protected property JsonWriter			as com.quarix.data.parser.JsonWriter	no-undo
      get:
         if not valid-object(JsonWriter) then
            JsonWriter = cast(GetInstance('com.quarix.data.parser.JsonWriter':u), 'com.quarix.data.parser.JsonWriter':u).
         return JsonWriter.
      end get.
      set.

    define public property DataRequest				as DataRequest							no-undo
      get.
      set.

    define protected property Request				as Request								no-undo
      get.
      set.

    define protected property Response				as Response								no-undo
      get.
      set.

    define public property DataContext				as com.quarix.data.DataContext			no-undo
      get:
		if not valid-object(DataContext) then
      		DataContext = cast(GetInstance('com.quarix.data.DataContext':u), com.quarix.data.DataContext).

        return DataContext.

      end get.
      protected set.

	define public property LogResponse				as logical								no-undo initial false
		get.
		set.

	constructor public DataObject (  ):

		super ().

	end constructor.

	destructor public DataObject ( ):

		define variable iNumBuf	as integer	no-undo.
		define variable hBuf	as handle	no-undo.
		define variable htt		as handle	no-undo.

		UnloadInstance(DataContext).

		if valid-handle(datasetHandle)
		then do:
			Util:TrackChanges(datasetHandle, false).

			datasetHandle:empty-dataset ().

			do iNumBuf = 1 to datasetHandle:num-buffers:

				hBuf = datasetHandle:get-buffer-handle (iNumBuf).

				htt = hBuf:table-handle.

				delete object hBuf no-error.

				delete object htt no-error.
			end.

			delete object datasetHandle no-error.

			if valid-handle(datasetHandle)
			then datasetHandle = ?.

		end. /* if valid-handle(datasetHandle) */

	end destructor.

	method public final memptr GetDescription (responseFormat as character, applicationPath as character):
      define variable saxWriter		as handle								no-undo.
      define variable saxAttr		as handle								no-undo.
      define variable mpResponse	as memptr								no-undo.
      define variable relFields		as character							no-undo.
      define variable className		as character							no-undo.
      define variable numBuf		as integer								no-undo.
      define variable numRel		as integer								no-undo.
      define variable numFld		as integer								no-undo.
      define variable hBuf			as handle								no-undo.
      define variable hRel			as handle								no-undo.
      define variable thisClass		as Progress.Lang.Class					no-undo.
      define variable saxLogger		as com.quarix.service.logging.SaxLogger	no-undo.
      define variable cLogDirectory	as character							no-undo.
      define variable cFileName		as character							no-undo.

      set-size(mpResponse) = 0.

      if not valid-handle(datasetHandle) then
      do:
         ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).
         return ?.
      end.

      if OpenOnInit eq true then
      do:
         if not dataFetch()
         then return ?.

         if not BeforeDataOutput()
         then return ?.
      end.

      if OpenOnInit and valid-object(ErrorManager) and ErrorManager:GetLogLevel() ge ErrorManager:DebugLevel()
      then do:
         cLogDirectory = Util:GetLogDirectory().

         cFileName = cLogDirectory + '/data_paint.xml':u.

         datasetHandle:write-xml('file', cFileName, true, 'utf-8', ?, false, true, true).
      end.

      if responseFormat eq 'xml':u then
      do:
         if OpenOnInit eq true then
            datasetHandle:write-xml('memptr':u, mpResponse, false, 'utf-8':u, ?, true, true, true).
         else
            datasetHandle:write-xmlschema('memptr':u, mpResponse, false, 'utf-8':u, true).

         return mpResponse.
      end.

      create sax-writer     saxWriter.
      create sax-attributes saxAttr.

      assign
         thisClass = GetClass()
         className = replace(thisClass:TypeName, '.':u, '~/').

      if className begins Application:Name then
         className = substring(className, length(Application:Name, 'character':u) + 2, -1, 'character':u).

      assign
         saxWriter:strict   = false
         saxWriter:fragment = true
         saxWriter:encoding = 'utf-8':u.

      saxWriter:set-output-destination('memptr':u, mpResponse).
      saxWriter:start-document().
      saxWriter:start-element('dataSet':u).
      saxWriter:insert-attribute('id':u, ID).

      saxWriter:start-element('comm':u).
      saxWriter:insert-attribute('method':u, 'post':u).
      saxWriter:insert-attribute('relURL':u, substitute('"&1/&2/data"':u, applicationPath, className)).
      saxWriter:insert-attribute('requestFormat':u, 'xml':u).
      saxWriter:insert-attribute('responseFormat':u, 'json':u).
      saxWriter:write-characters('').
      saxWriter:end-element('comm':u).

      saxWriter:start-element('properties':u).
      saxAttr:insert-attribute('name':u, 'autoSync':u).
      saxWriter:write-data-element('prop':u, string(AutoSync, 'true/false':u), ?, saxAttr).
      saxAttr:update-attribute('name':u, 'filteredChildren':u).
      saxWriter:write-data-element('prop':u, string(not RemoteChildFilter, 'true/false':u), ?, saxAttr).
      saxAttr:update-attribute('name':u, 'batchSize':u).
      saxWriter:write-data-element('prop':u, string(BatchSize), ?, saxAttr).
      saxAttr:update-attribute('name':u, 'margin':u).
      saxWriter:write-data-element('prop':u, string(BatchMargin), ?, saxAttr).
      saxAttr:update-attribute('name':u, 'readOnly':u).
      saxWriter:write-data-element('prop':u, string(ReadOnly, 'true/false':u), ?, saxAttr).
      saxAttr:update-attribute('name':u, 'updateMode':u).
      saxWriter:write-data-element('prop':u, string(UpdateMode, 'true/false':u), ?, saxAttr).
      saxAttr:update-attribute('name':u, 'sendFilterEveryTime':u).
      saxWriter:write-data-element('prop':u, string(SendFilterEveryTime, 'true/false':u), ?, saxAttr).
      saxAttr:update-attribute('name':u, 'threadTimeout':u).
      saxWriter:write-data-element('prop':u, string(ThreadTimeout), ?, saxAttr).
      saxAttr:update-attribute('name':u, 'openOnInit':u).
      saxWriter:write-data-element('prop':u, string(OpenOnInit, 'true/false':u), ?, saxAttr).
      saxAttr:update-attribute('name':u, 'sendChangesOnly':u).
      saxWriter:write-data-element('prop':u, string(SendChangesOnly, 'true/false':u), ?, saxAttr).
      saxWriter:end-element('properties':u).

      saxAttr:insert-attribute('format':u, '':u).
      saxAttr:insert-attribute('type':u, '':u).
      saxAttr:insert-attribute('sortable':u, '':u).
      saxAttr:insert-attribute('read-only':u, '':u).
      saxAttr:insert-attribute('defaultValue':u, '':u).
      saxAttr:insert-attribute('required':u, '':u).
      saxAttr:insert-attribute('view-as':u, '':u).
      saxAttr:insert-attribute('label':u, '':u).
      saxAttr:insert-attribute('tooltip':u, '':u).

      saxWriter:start-element('tables':u).
      do numBuf = 1 to datasetHandle:num-top-buffers:
         paintTable(saxWriter, saxAttr, datasetHandle:get-top-buffer(numBuf)).
      end.
      saxWriter:end-element('tables':u).

      saxWriter:start-element('relations':u).
      do numRel = 1 to datasetHandle:num-relations:
         assign
            relFields = ''
            hRel      = datasetHandle:get-relation(numRel).
         do numFld = 1 to num-entries(hRel:relation-fields) by 2:
            relFields = substitute('&1,&2:&3':u, relFields,
               entry(numFld, hRel:relation-fields),
               entry(numFld + 1, hRel:relation-fields)).
         end.
         saxWriter:start-element('relation':u).
         saxWriter:write-data-element('parent':u, substitute('"&1"':u, lower(hRel:parent-buffer:name))).
         saxWriter:write-data-element('child':u, substitute('"&1"':u, lower(hRel:child-buffer:name))).
         saxWriter:write-data-element('mapping':u, substitute('"&1"':u, lower(left-trim(relFields, ',':u)))).
         saxWriter:end-element('relation':u).
      end.
      saxWriter:end-element('relations':u).

      if valid-object(ErrorManager) and (ErrorManager:GetNumMessages() gt 0 or ErrorManager:GetNumDebugMessages() gt 0) then
      do:
         /* log all errors - including debug messages - in log manager */
         ErrorManager:LogMessages(cast(Application:GetService('logging':u), 'com.quarix.service.logging.iLogger':u)).

         if ErrorManager:GetNumMessages() gt 0 then
         do:
            saxLogger = cast(GetInstance('com.quarix.service.logging.SaxLogger':u), 'com.quarix.service.logging.SaxLogger':u).
            if valid-object(saxLogger) then
            do:
               saxLogger:SaxWriter = saxWriter.
               ErrorManager:LogInfoMessages(saxLogger).
               UnloadInstance(saxLogger).
            end.
         end.
         ErrorManager:Purge ().
      end.


      saxWriter:end-element('dataSet':u).
      saxWriter:end-document().

      return mpResponse.

      catch appError as Progress.Lang.Error :

      	 set-size(mpResponse) = 0.

         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
      finally:
         if valid-handle(saxAttr) then
            delete object saxAttr.
         if valid-handle(saxWriter) then
            delete object saxWriter.
      end finally.

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
            if lookup(hFld:data-type, 'blob') gt 0 then next.

            fieldFormat  = if hFld:data-type eq 'logical'
                                then hFld:format
                                else Localization:GetFormat(hFld:data-type, hFld:format) no-error.

            /* use substitute for default value to trap null value '?' */
            if lookup(hFld:data-type, 'character,clob':u) gt 0 then
               defaultVal = substitute('&1':u, if hFld:extent gt 0 then hFld:buffer-value[1] else hFld:buffer-value) no-error.
            else
               defaultVal = trim(substitute('&1':u, string(if hFld:extent gt 0 then hFld:buffer-value[1] else hFld:buffer-value, hFld:format))) no-error.

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

            if hFld:extent gt 0 then
            do iExt = 1 to hFld:extent:
               saxAttr:update-attribute('name', substitute('&1_&2', lower(hFld:name), iExt)).
               saxWriter:write-empty-element('col', ?, saxAttr).
            end.
            else
            do:
               saxWriter:write-empty-element('col', ?, saxAttr).
            end.
         end.
         hBuf:buffer-delete().
      end.
      saxWriter:end-element('columnNames':u).
      if OpenOnInit and valid-object(JsonWriter) then
      do:
         JsonWriter:SetDestination(mpData).
         JsonWriter:OpenStream().
         JsonWriter:StartElement('').

         serializeBufferToJson(table-handle hTable by-reference).

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
            delete object hTable.
      end finally.
   end method.

   method protected logical serializeBufferToJson (table-handle tableHandle):
   end method.

   method protected void serializeErrors (bufferName as character):
      define variable JsonLogger   as com.quarix.service.logging.JsonLogger no-undo.
      define variable numErr       as integer no-undo.

      if valid-object(ErrorManager) and ErrorManager:GetNumClientErrors() gt 0 then
      do:
         JsonLogger = cast(GetInstance('com.quarix.service.logging.JsonLogger':u),
            'com.quarix.service.logging.JsonLogger':u).
         if valid-object(JsonLogger) then
         do:
            if bufferName eq ? or ErrorManager:GetNumClientErrors(bufferName) eq 0 then
               JsonWriter:Out(',':u).

            JsonLogger:SetJsonWriter(JsonWriter).
            ErrorManager:LogClientErrors(JsonLogger, bufferName).
            UnloadInstance(JsonLogger).
         end.
         ErrorManager:Purge(bufferName).
      end.
   end method.

   method public logical dataFetch ():

      if not valid-handle(datasetHandle)
      then do:
         ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).

         return false.
      end.

      if not DataContext:getAllRecords() and
         not DataContext:hasFilters()
      then do:
          ThrowClientError(1000, 'No filter defined for fetch':u).

         return true.
      end.

      datasetHandle:empty-dataset().

      if not dataFetch(output dataset-handle datasetHandle by-reference)
      then do:
         datasetHandle:empty-dataset().

         return false.
      end.

      return true.

   end method.

   method public logical dataFetch (output dataset-handle dsHandle):

      ThrowError(1000, 'msg_err_no_handler':u, ?, ?).

      return false.

      finally:
      	delete object dsHandle no-error.
      end finally.

   end method.

   method public logical dataUpdate ():

      if not valid-handle(datasetHandle)
      then do:
         ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).

         return false.
      end.

      return dataUpdate(input-output dataset-handle datasetHandle by-reference).

   end method.

   method public logical dataUpdate(input-output dataset-handle dsHandle):

      ThrowError(1000, 'msg_err_no_handler':u, ?, ?).

      return false.

      finally:
      	delete object dsHandle no-error.
      end finally.

   end method.

   method public logical BeforeDataOutput ():
      return true.
   end method.

   method protected final void localizeDataset ():
      localizeDataset (input dataset-handle datasetHandle by-reference).
   end method.

   method protected void localizeDataset (input dataset-handle dsHandle):
	finally:
		delete object dsHandle no-error.
	end finally.
   end method.

	method public logical SetBatchSizeFromContext ():

        define variable numItem	as integer	no-undo.
        define variable hBuf	as handle	no-undo.
        define variable hTT     as handle	no-undo.

        if not valid-handle(datasetHandle)
        then return false.

        if not valid-object(DataContext)
        then return false.

		do numItem = 1 to datasetHandle:num-buffers:

			assign
				hBuf	= datasetHandle:get-buffer-handle(numItem)
				hTT		= hBuf:table-handle.

			hBuf:batch-size = 0.

			if Util:IsEmpty(DataContext:getBatchSize(hTT:name))
			then do:
				if not Util:IsEmpty(BatchSize)
            	then do:
            		hBuf:batch-size = BatchSize.

            		DataContext:setBatchSize(hTT:name, BatchSize).
            	end.
			end.
			else hBuf:batch-size = DataContext:getBatchSize(hTT:name).

		end. /* do numItem = 1 to datasetHandle:num-buffers */

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

	method private void DumpResponseToFile(input mp as memptr):

		Util:DumpMemPtrToFile(mp, 'Response':U).

	end method.

   method protected logical dataOutput():

        define variable numBuf			as integer		no-undo.
        define variable firstBuffer		as logical		no-undo initial true.
        define variable hBuf			as handle		no-undo.
        define variable mpOut			as memptr		no-undo.
        define variable lRetValue		as logical		no-undo.
        define variable cLogDirectory	as character	no-undo.
        define variable cFileName		as character	no-undo.
        define variable iStartTime		as integer		no-undo.
        define variable iCallDuration	as integer		no-undo.

        iStartTime = etime.

        if not valid-object(DataRequest)
        then return false.

        if not valid-handle(datasetHandle)
        then return false.

        /* make sure we use the right localization */
        if valid-object(Localization) then
            assign
                session:date-format    = Localization:GetDateFormat()
                session:numeric-format = Localization:GetNumericFormat().

        if valid-object(ErrorManager) and ErrorManager:GetLogLevel() ge ErrorManager:DebugLevel()
        then do:
           cLogDirectory = Util:GetLogDirectory().

		   cFileName = cLogDirectory + '/data_response.xml':u.

           datasetHandle:write-xml('file', cFileName, true, 'utf-8', ?, false, true, true).
       	end.

        set-size(mpOut) = 0.

        case DataRequest:OutputFormat:

            when 'json':u
            then do:
            	if not valid-object(JsonWriter)
            	then return false.

                JsonWriter:SetDestination(mpOut).
                JsonWriter:OpenStream().
                JsonWriter:StartElement('').
                JsonWriter:Out('"tables": [':u).

                lRetValue = true.

                do numBuf = 1 to datasetHandle:num-top-buffers:

                	hBuf = datasetHandle:get-top-buffer(numBuf).

                   	if not dataOutput(input-output firstBuffer, hBuf)
                   	then do:
                   		lRetValue = false.

                   		leave.
                   	end.
               	end. /* do numBuf = 1 to datasetHandle:num-top-buffers */

                JsonWriter:Out(']':u).

                /* dump the rest of dataset generic errors */
                serializeErrors(?).

                JsonWriter:EndElement('').
                JsonWriter:CloseStream().

                lRetValue = Response:Out(mpOut) and lRetValue.

				if LogResponse
				then DumpResponseToFile(mpOut).

                set-size(mpOut) = 0.

                if not lRetValue
                then return false.

            end. /* when 'json':u */

            when 'xml':u
            then do:
            	datasetHandle:write-xml('memptr':u, mpOut, false, 'utf-8':u, ?, true, true, true).

                if not Response:Out(mpOut)
                then do:
                	set-size(mpOut) = 0.

                	return false.
               	end.

                set-size(mpOut) = 0.

            end. /* when 'xml':u */

        end case. /* case DataRequest:OutputFormat */

        return true.

        catch appError as Progress.Lang.Error :

        	set-size(mpOut) = 0.

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
        finally:
        	iCallDuration = etime - iStartTime.

        	message 'Serialization: ' iCallDuration skip.

        end finally.

    end method.

    method private logical setBatchInfo(input hBuf as handle):

    	define variable lFirstBatch	as logical	no-undo.
    	define variable lLastBatch	as logical	no-undo.

    	define variable hTT as handle no-undo.

    	if not valid-handle(hBuf)
    	then return false.

    	if not valid-object(DataContext)
    	then return false.

    	hTT = hBuf:table-handle.

    	lFirstBatch	= DataContext:getFirstBatch(hTT:name).
    	lLastBatch	= DataContext:getLastBatch(hTT:name).

    	DataContext:setBatchInfo(hTT:name, lFirstBatch, lLastBatch).

    	hBuf:find-first ('where true':U, no-lock) no-error.

    	if not hBuf:available
    	then DataContext:setBatchInfo(hTT:name, true, true).

    	return true.

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private logical dataOutput(input-output firstBuffer as logical, hBuf as handle):

        define variable numRel as integer no-undo.
        define variable hTT    as handle  no-undo.

        if  not valid-handle(hBuf) or
            not valid-object(JsonWriter) or
            not valid-object(DataRequest)
        then
            return false.

        if not setBatchInfo(hBuf)
        then return false.

        hTT = hBuf:table-handle.

        /* send data only if requested... */
        if DataRequest:HasRequest(hTT:name)
       	then do:
            JsonWriter:Out(substitute('&1~{"id": "&2",':u, if firstBuffer then '' else ',', lower(hTT:name))).

            if not serializeBufferToJson(table-handle hTT by-reference)
            then do:
            	JsonWriter:Out('~}').

            	return false.
            end.

            JsonWriter:Out('~}').

            firstBuffer = false.
        end.

        do numRel = 1 to hBuf:num-child-relations:
            if not dataOutput(input-output firstBuffer, hBuf:get-child-relation(numRel):child-buffer)
            then return false.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public void SetDataContext (input dtContext as DataContext):
        if not valid-object(dtContext) then return.

        DataContext = dtContext.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.

    method public logical SetFiltersFromRequest():

    	define variable numBuf  as integer no-undo.
        define variable hTT     as handle  no-undo.

        if not valid-handle(datasetHandle)
        then return false.

		if valid-object(DataContext)
		then do:
        	DataContext:ClearFilters().
        	DataContext:ClearProperties().
        end.

        do numBuf = 1 to datasetHandle:num-buffers:

            assign hTT = datasetHandle:get-buffer-handle(numBuf):table-handle.

            if not SetRequestFilters(hTT:name, true)
            then return false.

        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical SetSortFromRequest():

    	define variable numBuf  as integer no-undo.
        define variable hTT     as handle  no-undo.

        if not valid-handle(datasetHandle)
        then return false.

		if valid-object(DataContext)
		then DataContext:ClearSort().

        do numBuf = 1 to datasetHandle:num-buffers:

            assign hTT = datasetHandle:get-buffer-handle(numBuf):table-handle.

            if not SetRequestSort(hTT:name)
            then return false.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical SetBatchSizeFromRequest():

    	define variable numBuf  as integer	no-undo.
        define variable hTT     as handle	no-undo.
        define variable hBuf	as handle	no-undo.

        if not valid-handle(datasetHandle)
        then return false.

        if not valid-object(DataRequest)
        then return false.

        do numBuf = 1 to datasetHandle:num-buffers:

            assign
            	hBuf	= datasetHandle:get-buffer-handle(numBuf)
            	hTT		= hBuf:table-handle.

            if not Util:IsEmpty(DataRequest:GetBatchSize(hTT:name))
            then DataContext:setBatchSize(hTT:name, DataRequest:GetBatchSize(hTT:name)).
            else DataContext:setBatchSize(hTT:name, 0).

           	DataContext:setIsSearchRequest(hTT:name, DataRequest:IsSearchRequest(hTT:name)).

            DataContext:setStartRowid(hTT:name, Util:Nvl(DataRequest:GetStartRowId(hTT:name), 'first':u)).

            DataContext:setStartRow(hTT:name, DataRequest:GetStartRow(hTT:name)).

            DataContext:setSkipRow(hTT:name, DataRequest:GetSkipRow(hTT:name)).

        end. /* do numBuf = 1 to datasetHandle:num-buffers */

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical LoadRequestData ():

        define variable numBuf  as integer no-undo.
        define variable hTT     as handle  no-undo.

        if not valid-handle(datasetHandle)
        then return false.

        if not valid-object(DataRequest)
        then return false.

        if not valid-object(DataContext)
        then return false.

        do numBuf = 1 to datasetHandle:num-buffers:

            assign hTT = datasetHandle:get-buffer-handle(numBuf):table-handle.

            hTT:private-data = DataContext:GetPrivateData().

            if not DataRequest:LoadData(table-handle hTT by-reference)
            then return false.

        end. /* do numBuf = 1 to datasetHandle:num-buffers */

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

        finally:
            if valid-handle(hTT) then
                delete object hTT.
        end finally.

    end method.

    method public final void SetParentBatchSize (parentBatchSize as integer):
        define variable numBuf as integer no-undo.
        define variable hTT    as handle  no-undo.

        do numBuf = 1 to datasetHandle:num-buffers:
            hTT = datasetHandle:get-buffer-handle(numBuf):table-handle.
            DataContext:setBatchSize(hTT:name,parentBatchSize).
        end.
    end method.

    method public void SetProperty(input PropertyName as character, input PropertyValue as character):

        DataContext:SetProperty(input PropertyName, input PropertyValue).

    end method.


    method public final logical SetRequestFilters (tableName as character, considerSearchFilter as logical):

        define variable numFilter    as integer   no-undo initial 1.
        define variable filterCol    as character no-undo.
        define variable filterOp     as character no-undo.
        define variable filterVal    as character no-undo.
        define variable reverseSort  as logical   no-undo.

        if not valid-object(DataRequest)
        then return false.

        do while DataRequest:GetSearch(tableName, numFilter, output filterCol, output filterVal)
            on error undo, throw:

            if considerSearchFilter and filterVal ne ? then do:
                reverseSort = DataRequest:IsSortDescending(tableName, filterCol).

                SetFilter (tableName, filterCol, if reverseSort eq ? or reverseSort eq false then 'ge':u else 'le':u, filterVal).
            end.
            else
                RemoveFilter(tableName, filterCol).

            numFilter = numFilter + 1.
        end.

        numFilter = 1.

        do while DataRequest:GetFilter(tableName, numFilter, output filterCol, output filterOp, output filterVal)
            on error undo, throw:
            if filterVal ne ? then do:
                if filterCol begins '_' then
                    SetProperty(left-trim(filterCol,'_'),filterVal).
                else
                    SetFilter (tableName, filterCol, filterOp, filterVal).
            end.
            numFilter = numFilter + 1.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical SetFilter (tableName as character, fieldName as character, operName as character, fieldValue as character):

        if Util:IsEmpty(fieldName) or Util:IsEmpty(tableName) then
            return false.

        return DataContext:SetFilter (tableName,fieldName,operName,fieldValue,?).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public character GetFilterValue (tableName as character, fieldname as character, operName as character):

    	return DataContext:GetFilterValue(tableName, fieldname, operName).

    	catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return ?.
        end catch.

    end method.

    method public void RemoveFilter (tableName as character, fieldName as character):
        if not DataContext:RemoveFilter (tableName,fieldName) then
            return.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.

    method private logical SetRequestSort (tableName as character):

        define variable numSort   as integer   no-undo initial 1.
        define variable sortCol   as character no-undo.
        define variable sortDesc  as logical   no-undo.

        if not valid-object(DataRequest)
        then return false.

        do while DataRequest:GetSort(tableName, numSort, output sortCol, output sortDesc)
            on error undo, throw:
            SetSort (tableName, sortCol, sortDesc).
            numSort = numSort + 1.
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public logical SetSort( input tableName as character, input fieldName as character, input descendingSort as logical ):
        return DataContext:SetSort (tableName, fieldName, descendingSort).
    end method.

    method public void ClearFilters ():
        DataContext:ClearFilters().
    end method.

    method public void ClearSort ():
        DataContext:ClearSort().
    end method.

    method public void RemoveTableFilters (tableName as character):
        if Util:IsEmpty(tableName) then
            return.

        if not DataContext:RemoveTableFilters (tableName) then
            return.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.

    method public void RemoveSort( input tableName as character, input fieldName as character ):
        if not DataContext:RemoveSort (tableName, fieldName) then
            return.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.

    method public void RemoveTableSort( input tableName as character ):
        if Util:IsEmpty(tableName) then
            return.

        if not DataContext:RemoveTableSort (tableName) then
            return.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
        end catch.
    end method.

    method public void ClearProperties():

        DataContext:ClearProperties().

    end method.

    method public void ClearProperty(input PropertyName as character):

    	DataContext:ClearProperty(PropertyName).

    end method.

    method public character GetProperty(input PropertyName as character):

        return DataContext:GetProperty(input PropertyName).

    end method.

    method public logical PropertyAvailable(input PropertyName as character):

        return DataContext:PropertyAvailable(input PropertyName).

    end method.

    method public logical PropertyAvailable():

        return DataContext:PropertyAvailable().

    end method.

    method public void SetQueryName(input PropertyValue as character):

        DataContext:SetQueryName(input PropertyValue).

    end method.

    method public void SetIdListTmpHandle(input PropertyValue as handle):

        DataContext:SetIdListTmpHandle(input PropertyValue).

    end method.

    method public character GetQueryName():

        return DataContext:GetQueryName().

    end method.

    method public handle GetIdListTmpHandle():

        return DataContext:GetIdListTmpHandle().

    end method.

    method public void ClearContext():

        DataContext:ClearContext().

    end method.

    method public void ClearQueryData():

        DataContext:ClearQueryData ().

    end method.

    method public logical dataFetchIdList(input idlist as com.quarix.data.IdList, output dataset-handle hDsDataset, input QueryName as character):

        ClearProperties().

        if not valid-object(IdList)         or
            not valid-handle(hDsDataset)    or
            Util:IsEmpty(QueryName)         or
            not valid-handle(idlist:getTableHandle())
        then return false.

		SetQueryName(QueryName).

		SetIdListTmpHandle(idlist:getTableHandle()).

		hDsDataset:empty-dataset ().

		if idlist:IdListHasData()
		then return dataFetch(output dataset-handle hDsDataset by-reference).

		return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
        finally:
        	delete object hDsDataset no-error.
        end finally.

	end method.

	method public logical dataFetchIdList(input idlist as com.quarix.data.IdList, output dataset-handle hDsDataset):

		return dataFetchIdList(input idlist, output dataset-handle hDsDataset by-reference, input 'FetchDataByIdList':U).

		finally:
        	delete object hDsDataset no-error.
        end finally.

	end method.

	method public logical IsFirstBatch():
	    return true.
    end method.

    method public logical IsLastBatch():
        return true.
    end method.

    method public void EnableLogResponse():
    	LogResponse = true.
    end method.

    method public void DisableLogResponse():
    	LogResponse = false.
    end method.

    method public void DumpToXml():

        if valid-handle(datasetHandle)
        then Util:LogDataset(input dataset-handle datasetHandle by-reference).

	end method.

	method public void DumpToXml(input pcName as character):

        if valid-handle(datasetHandle)
        then Util:LogDataset(input dataset-handle datasetHandle by-reference, pcName).

	end method.

	method public final handle getDefaultDatasetHandle():

		return defaultDatasetHandle.

	end method.

	method public final void restoreDatasetHandle():

		datasetHandle = getDefaultDatasetHandle().

	end method.

	method public logical  SaveData ():

		ThrowError(1000, 'msg_err_no_handler':u, ?, ?).

        return false.

	end method.

	method public logical HandleRequest( input actionName as character, input webRequest as com.quarix.web.Request, input webResponse as com.quarix.web.Response ):

	    ThrowError(1000, 'msg_err_no_handler':u, ?, ?).

	    return false.

	end method.

	method public logical BeforeRequest (actionName as character):
        return true.
    end method.

    method public logical AfterRequest  (actionName as character):
        return true.
    end method.

    method public void ClearSessionParameters():

    	if valid-object(DataContext)
    	then do:
    		DataContext:ClearProperty('FilterSessionID':u).
    		DataContext:ClearProperty('FilterLanguageID':u).
    		DataContext:ClearProperty('FilterUsrID':u).
    		DataContext:ClearProperty('Company':u).
    		DataContext:ClearProperty('CustomerID':u).
    		DataContext:ClearProperty('EntityType':u).
    	end.

    end method.

    method public void ClearContextManagerSessionParameters():

    	if valid-object(ContextManager)
    	then do:
    		ContextManager:SetValue(input 'SessionID':u, input '':u).
	    	ContextManager:SetValue(input 'LanguageID':u, input '':u).
    		ContextManager:SetValue(input 'UsrID':u, input '':u).
    		ContextManager:SetValue(input 'Company':u, input '':u).
    		ContextManager:SetValue(input 'CustomerID':u, input '':u).
    		ContextManager:SetValue(input 'EntityType':u, input '':u).
    	end.

    end method.

    method public void SetSessionSessionID(input cFilterSessionID as character):

    	DataContext:SetProperty('FilterSessionID':u, cFilterSessionID).

    end method.

    method public void SetSessionLanguageID(input cFilterLanguageID as character):

    	DataContext:SetProperty('FilterLanguageID':u, cFilterLanguageID).

    end method.

    method public void SetSessionUsrID(input cFilterUsrID as character):

    	DataContext:SetProperty('FilterUsrID':u, cFilterUsrID).

    end method.

    method public void SetSessionCompany(input cFilterCompany as character):

    	DataContext:SetProperty('Company':u, cFilterCompany).

    end method.

    method public void SetSessionCustomer(input cFilterCustomer as character):

    	DataContext:SetProperty('CustomerID':u, cFilterCustomer).

    end method.

    method public void SetSessionEntityType(input cFilterEntityType as character):

    	DataContext:SetProperty('EntityType':u, cFilterEntityType).

    end method.

    method public void SetSessionParameters():

    	define variable cFilterSessionID	as character	no-undo.
    	define variable cFilterLanguageID	as character	no-undo.
    	define variable cFilterUsrID		as character	no-undo.
    	define variable cCompany			as character	no-undo.
    	define variable cCustomerID			as character	no-undo.
    	define variable cEntityType			as character	no-undo.

    	ClearContextManagerSessionParameters().

    	if not valid-object(DataContext) or
    		not valid-object(ContextManager)
    	then return.

    	assign
    		cFilterSessionID	= DataContext:GetProperty('FilterSessionID':u)
    		cFilterLanguageID	= DataContext:GetProperty('FilterLanguageID':u)
    		cFilterUsrID		= DataContext:GetProperty('FilterUsrID':u)
    		cCompany			= DataContext:GetProperty('Company':u)
    		cCustomerID			= DataContext:GetProperty('CustomerID':u)
    		cEntityType			= DataContext:GetProperty('EntityType':u).

		ClearSessionParameters().

    	ContextManager:SetValue(input 'SessionID':u, input cFilterSessionID).
    	ContextManager:SetValue(input 'LanguageID':u, input cFilterLanguageID).
    	ContextManager:SetValue(input 'UsrID':u, input cFilterUsrID).
    	ContextManager:SetValue(input 'Company':u, input cCompany).
    	ContextManager:SetValue(input 'CustomerID':u, input cCustomerID).
    	ContextManager:SetValue(input 'EntityType':u, input cEntityType).

        return.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return.
        end catch.

    end method.

    method public void DisplaySessionParameters():

    	define variable cFilterSessionID	as character	no-undo.
    	define variable cFilterLanguageID	as character	no-undo.
    	define variable cFilterUsrID		as character	no-undo.
    	define variable cCompany			as character	no-undo.
    	define variable cCustomerID			as character	no-undo.
    	define variable cEntityType			as character	no-undo.

    	ContextManager:GetValue(input 'SessionID':u, output cFilterSessionID).
    	ContextManager:GetValue(input 'LanguageID':u, output cFilterLanguageID).
    	ContextManager:GetValue(input 'UsrID':u, output cFilterUsrID).
    	ContextManager:GetValue(input 'Company':u, output cCompany).
    	ContextManager:GetValue(input 'CustomerID':u, output cCustomerID).
    	ContextManager:GetValue(input 'EntityType':u, output cEntityType).

    	message
    		'------------------------------------------------------------------------------------------------------------------------------':U skip
    		'cFilterSessionID' cFilterSessionID skip
    		'cFilterLanguageID' cFilterLanguageID skip
    		'cFilterUsrID' cFilterUsrID skip
    		'cCompany' cCompany skip
    		'cCustomerID' cCustomerID skip
    		'cEntityType' cEntityType skip
    		'------------------------------------------------------------------------------------------------------------------------------':U skip
		view-as alert-box.

    end method.

    method public logical CheckMandatory(input hBuf as handle, input pcField as character):

    	define variable hField as handle no-undo.

    	if not valid-handle(hBuf)
    	then return false.

    	if Util:IsEmpty(pcField)
    	then return false.

    	if not hBuf:available
    	then return false.

    	hField = hBuf:buffer-field (pcField) no-error.

    	if not valid-handle(hField)
    	then return false.

    	if Util:IsEmpty(hField:buffer-value ())
    	then do:
    		ThrowClientError(100, substitute('Field "&1" is required.':U, pcField), hBuf:name, pcField).

			return false.
    	end.

    	return true.

    	catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

    end method.

    method public logical InitializeDataObject():

    	define variable numBuf	as integer	no-undo.
    	define variable hBuf	as handle	no-undo.
    	define variable hTT		as handle	no-undo.

    	if not valid-object(DataContext)
    	then return false.

    	DataContext:ClearContext().

    	if not valid-handle(datasetHandle)
        then return false.

        do numBuf = 1 to datasetHandle:num-buffers:

            assign
            	hBuf	= datasetHandle:get-buffer-handle(numBuf)
            	hTT		= hBuf:table-handle.

            hBuf:batch-size = 0.

            DataContext:setBatchSize(hTT:name, 0).

            DataContext:setIsSearchRequest(hTT:name, false).

            DataContext:setStartRowid(hTT:name, 'first':u).

            DataContext:setSkipRow(hTT:name, true).

        end. /* do numBuf = 1 to datasetHandle:num-buffers */

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

end class.
