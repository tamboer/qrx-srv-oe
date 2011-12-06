/*------------------------------------------------------------------------
    File        : DataCollection
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marian
    Created     : Mon Jan 19 16:12:17 EET 2009
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

using com.quarix.data.DataRequest.
using com.quarix.web.Request.
using com.quarix.web.Response.

&scoped-define req-var-xml-data         'xml':u
&scoped-define req-var-lookup-table     'table':u
&scoped-define req-var-lookup-col       'col':u
&scoped-define req-var-lookup-val       'val':u
&scoped-define req-var-lookup-desc      'desc':u
&scoped-define req-var-lookup-filter    'filter':u
&scoped-define req-var-lookup-max-rec   10

class com.quarix.data.DataCollection
   inherits com.quarix.base.BaseObject
   implements com.quarix.data.iDataCollection, com.quarix.web.iWebObject use-widget-pool:

   define protected temp-table ttFilter   no-undo
      field tableName     as character
      field fieldName     as character
      field dbFieldName   as character
      field operName      as character
      field fieldValue    as character
      index xpk is primary tableName fieldName operName.

   define protected temp-table ttSort    no-undo
      field tableName     as character
      field fieldName     as character
      field descOrder     as logical
      field sortOrder     as integer
      index xpk is primary unique tableName sortOrder
      index xfk is unique tableName fieldName.

   define protected temp-table ttInfo  no-undo
      field ttName     as character
      field ttRowPos   as character
      field lastBatch  as logical
      field firstBatch as logical
      index xpk is primary unique ttName.

   define private temp-table ttRowId   no-undo
      field ttName  as character
      field ttRowId as character
      field dbRowId as character
      index xpk is primary unique ttName ttRowId.

   define public property ACTION_PAINT          as character     no-undo initial 'paint':u
      get.

   define public property ACTION_DATA           as character     no-undo initial 'data':u
      get.

   define public property ACTION_LOOKUP         as character     no-undo initial 'lookup':u
      get.

   define public property ACTION_AUTOCOMPLETE   as character     no-undo initial 'autocomplete':u
      get.

   define public property ACTION_REPORT         as character     no-undo initial 'birtreport':u
      get.

   define public property ACTION_DATA_FILL      as character     no-undo initial 'sendRows':u
      get.

   define public property ACTION_DATA_SAVE      as character     no-undo initial 'submitCommit':u
      get.

   define public property ID                    as character     no-undo
      get.
      protected set.

   define public property ReadOnly              as logical       no-undo
      get.
      protected set.

   define public property ReadOnlyFields        as character     no-undo
      get.
      protected set (roFields as character):
         if setReadOnlyFields (input-output roFields) then
            ReadOnlyFields = roFields.
      end set.

   define public property OpenOnInit            as logical       no-undo
      get.
      set.

   define public property AutoSync              as logical       no-undo   initial true
      get.
      set.

   define public property SendChangesOnly       as logical       no-undo   initial true
      get.
      set.

   define public property SendFilterEveryTime   as logical       no-undo   initial true
      get.
      set.

   define public property UpdateMode            as logical       no-undo
      get.
      set.

   define public property RemoteChildFilter     as logical       no-undo
      get.
      set.

   define public property BatchSize             as integer       no-undo   initial 100
      get.
      set (numRecords as integer):
         BatchSize = numRecords.
         SetBatchSize(BatchSize).
      end set.

   define public property BatchMargin           as integer       no-undo   initial 5
      get.
      set.

   define public property ThreadTimeOut         as integer       no-undo   initial 30000
      get.
      set.

   define protected property EnableCache        as logical       no-undo
      get.
      set.

   define public property datasetHandle         as handle        no-undo
      get.
      protected set (dsHandle as handle):
         if not valid-handle(dsHandle) or dsHandle:type ne 'dataset':u then
            return.
         datasetHandle = dsHandle.
         localizeDataset().
      end set.

   define protected property Request            as Request       no-undo
      get.
      private set.

   define protected property Response           as Response      no-undo
      get.
      private set.

   define protected property DataRequest        as DataRequest   no-undo
      get.
      private set.

   define protected property ActionName         as character     no-undo
      get.
      private set.

   define private property JsonWriter           as com.quarix.data.parser.JsonWriter
      get:
         if not valid-object(JsonWriter) then
            JsonWriter = cast(GetInstance('com.quarix.data.parser.JsonWriter':u), 'com.quarix.data.parser.JsonWriter':u).
         return JsonWriter.
      end get.
      set.

   define public property exceptTables          as character     no-undo
      get.
      set.

   define private variable lastSortOrder       as integer      no-undo.

   constructor protected DataCollection ():

   end constructor.

   destructor public DataCollection ():
      DetachDataSource().
      UnloadInstance(DataRequest).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end destructor.

   method public  logical    AddWhereClause (tableName as character, bufferName as character, whereClause as character):
      return AddWhereClause (tableName, bufferName, whereClause, false).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public  logical    AddJoinClause (tableName as character, bufferName as character, joinClause as character):
      return AddWhereClause (tableName, bufferName, joinClause, true).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method private logical    addWhereClause (tableName as character, bufferName as character, whereClause as character, isJoin as logical):
   define buffer   bufFilter  for ttFilter.

      define variable tableHandle    as handle  no-undo.
      define variable bufferHandle   as handle  no-undo.
      define variable numBuf         as integer no-undo.

      assign
         whereClause = trim(whereClause)
         tableHandle = datasetHandle:get-buffer-handle(tableName).

      if Util:IsEmpty(whereClause) or
         not valid-handle(tableHandle) or
         not valid-handle(tableHandle:data-source) then
         return false.

      do numBuf = 1 to tableHandle:data-source:num-source-buffers:
         bufferHandle = tableHandle:data-source:get-source-buffer (numBuf).
         if bufferHandle:name eq bufferName then leave.
      end.

      if not valid-handle(bufferHandle) or bufferHandle:name ne bufferName then
         return false.

      if entry(1, whereClause, ' ':u) eq 'where':u then
         whereClause = trim(substring(whereClause, 6, -1)).

      create bufFilter.
      assign
         bufFilter.tableName    = tableName
         bufFilter.fieldName    = ?
         bufFilter.dbFieldName  = substitute('&1.':u, bufferName)
         bufFilter.fieldValue   = whereClause
         bufFilter.operName     = string(isJoin).

      return true.

   end method.


   method public logical    SetFilter (fieldName as character, operName as character, fieldValue as character):
      define variable tableName   as  character no-undo.
      define variable dbFieldName as  character no-undo.
      define buffer   bufFilter  for ttFilter.


      if not valid-handle(datasetHandle)
         or Util:IsEmpty(fieldName)
         or not BeforeSetFilter (fieldName, operName, fieldValue) then
         return false.

      assign
         fieldName = getFullFieldName(fieldName)
         tableName = entry(1, fieldName, '.':u)
         fieldName = entry(2, fieldName, '.':u) .

      if Util:IsEmpty(fieldName) or Util:IsEmpty(tableName) then
         return false.

      dbFieldName = GetPhysicalFieldName (substitute('&1.&2':u, tableName, fieldName)).

      for each bufFilter
         where bufFilter.tableName = tableName
           and bufFilter.fieldName = fieldName
           and bufFilter.operName  = operName
              on error undo, throw:
         assign
             bufFilter.dbFieldName = dbFieldName
             bufFilter.fieldValue  = fieldValue.
         return true.
      end.

      create bufFilter.
      assign
         bufFilter.dbFieldName = dbFieldName
         bufFilter.tableName   = tableName
         bufFilter.fieldName   = fieldName
         bufFilter.operName    = operName
         bufFilter.fieldValue  = fieldValue.

      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method protected logical    BeforeSetFilter (fieldName as character, operName as character, fieldValue as character):
      return true.
   end method.

   method protected logical    BeforeRemoveFilter (fieldName as character):
      return true.
   end method.

   method public void RemoveFilter (fieldName as character):
      define variable tableName  as  character no-undo.
      define buffer   bufFilter  for ttFilter.

      if not valid-handle(datasetHandle)
         or Util:IsEmpty(fieldName)
         or not BeforeRemoveFilter (fieldName) then
         return.

      assign
         fieldName = getFullFieldName(fieldName)
         tableName = entry(1, fieldName, '.':u)
         fieldName = entry(2, fieldName, '.':u) .

      for each bufFilter
         where bufFilter.tableName eq tableName
         and bufFilter.fieldName eq fieldName
            on error undo, throw:
         delete bufFilter.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public void RemoveFilter (fieldName as character, fieldValue as character, output operName as character):
      define variable tableName  as  character no-undo.
      define buffer   bufFilter  for ttFilter.

      if not valid-handle(datasetHandle)
         or Util:IsEmpty(fieldName)
         or not BeforeRemoveFilter (fieldName) then
         return.

      assign
         fieldName = getFullFieldName(fieldName)
         tableName = entry(1, fieldName, '.':u)
         fieldName = entry(2, fieldName, '.':u) .

      for each bufFilter
         where bufFilter.tableName eq tableName
         and bufFilter.fieldName eq fieldName
         and bufFilter.fieldValue eq fieldValue
            on error undo, throw:
         operName = bufFilter.operName.
         delete bufFilter.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public void ClearFilters ():
      empty temp-table ttFilter.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method protected logical    BeforeSetSort (fieldName as character, descendingSort as logical):
      return true.
   end method.

   method protected logical    BeforeRemoveSort (fieldName as character):
      return true.
   end method.

   method public logical    SetSort (fieldName as character):
      return SetSort(fieldName, false).
   end method.

   method public logical    SetSort (fieldName as character, descendingSort as logical):
      define variable tableName  as  character no-undo.
      define buffer   bufSort    for ttSort.

      if not valid-handle(datasetHandle)
         or Util:IsEmpty(fieldName)
         or not BeforeSetSort (fieldName, descendingSort) then
         return false.

      assign
         fieldName = getFullFieldName(fieldName)
         tableName = entry(1, fieldName, '.':u)
         fieldName = entry(2, fieldName, '.':u) .


      for each bufSort
         where bufSort.tableName eq tableName
           and bufSort.fieldName eq fieldName
              on error undo, throw:
         assign
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

   method public void RemoveSort (fieldName as character):
      define variable tableName  as  character no-undo.
      define buffer   bufSort    for ttSort.

      if not valid-handle(datasetHandle)
         or Util:IsEmpty(fieldName)
         or not BeforeRemoveSort (fieldName) then
         return.

      assign
         fieldName = getFullFieldName(fieldName)
         tableName = entry(1, fieldName, '.':u)
         fieldName = entry(2, fieldName, '.':u) .

      for each bufSort
         where bufSort.tableName eq tableName
         and bufSort.fieldName eq fieldName
            on error undo, throw:
         delete bufSort.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public void ClearSort ():
      empty temp-table ttSort.
      lastSortOrder = 0.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public void ClearSort (input tableName as character):
   define buffer ttSort for ttSort.

      for each ttSort
         where ttSort.tableName eq tableName
            on error undo, throw:
         delete ttSort.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public logical    SetBatchSize (batchSize as integer):
      define variable numItem   as integer no-undo.

      if valid-handle(datasetHandle) then
      do:
         do numItem = 1 to datasetHandle:num-buffers:
            datasetHandle:get-buffer-handle(numItem):batch-size = batchSize.
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

   method public logical    SetBatchSize (bufferName as character, batchSize as integer):
      define variable bufferHdl as handle no-undo.

      if valid-handle(datasetHandle) then
      do:
         bufferHdl = datasetHandle:get-buffer-handle(bufferName) .
         if valid-handle(bufferHdl) then
         do:
            bufferHdl:batch-size = batchSize.
            return true.
         end.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public final integer GetBatchSize (bufferName as character):
      define variable bufferHdl as handle no-undo.

      if valid-handle(datasetHandle) then
      do:
         bufferHdl = datasetHandle:get-buffer-handle(bufferName).
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

   method protected logical    RepositionToRow (bufferName as character, repositionRow as integer):
      return RepositionToRow (bufferName, repositionRow, false, false).
   end method.

   method protected logical    RepositionToRow (bufferName as character, repositionRow as integer, goBackward as logical):
      return RepositionToRow (bufferName, repositionRow, goBackward, false).
   end method.

   method protected logical    RepositionToRow
      (bufferName as character, repositionRow   as integer,
      goBackward as logical,   skipRecord      as logical):
      define variable hDatasource    as handle  no-undo.
      define variable hBuf           as handle  no-undo.
      define variable hQuery         as handle  no-undo.
      define variable iBuf           as integer no-undo.


      if repositionRow eq 0 then
         return true.

      if not valid-handle(datasetHandle) then
         return false.

      assign
         hBuf        = datasetHandle:get-buffer-handle(bufferName)
         hDatasource = hBuf:data-source
         hQuery      = hDatasource:query .

      if valid-handle(hDatasource) then
      do:
         if not hQuery:is-open then
            hQuery:query-open().
         /* -1 - last; 0 - first */
         if repositionRow eq -1 then
         do:
            hQuery:get-last(no-lock) .
            if skipRecord then
               hQuery:reposition-backward(hBuf:batch-size + 1) .
            else
               hQuery:reposition-backward(hBuf:batch-size) .
         end.
         else
         do:
            if goBackward then
               repositionRow = repositionRow - hBuf:batch-size.
            if skipRecord then
               repositionRow = repositionRow + (if goBackward then -1 else 1).
            hQuery:reposition-to-row(repositionRow) .
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

   method protected final logical    RepositionToRow (bufferName as character, repositionRowid as character):
      return RepositionToRow (bufferName, repositionRowid, false, false).
   end method.

   method protected final logical    RepositionToRow (bufferName as character, repositionRowid as character, goBackward as logical):
      return RepositionToRow (bufferName, repositionRowid, goBackward, false).
   end method.

   method protected logical    RepositionToRow
      (bufferName as character, repositionRowid as character,
      goBackward as logical,   skipRecord      as logical):
      define variable hDatasource    as handle  no-undo.
      define variable hQuery         as handle  no-undo.
      define variable hBuf           as handle  no-undo.
      define variable iBuf           as integer no-undo.
      define variable posRowid       as rowid   no-undo extent 18.
      define variable cQuery         as character no-undo.

      if not valid-handle(datasetHandle) then
         return false.

      if repositionRowid eq ? or repositionRowid eq 'first':u then
         return true.
      if repositionRowid eq 'last':u then
         return RepositionToRow(bufferName, -1, true, false).

      assign
         hBuf        = datasetHandle:get-buffer-handle(bufferName)
         hDatasource = hBuf:data-source
         hQuery      = hDatasource:query .

      if valid-handle(hDatasource) then
      do:
         do iBuf = 1 to num-entries(repositionRowid):
            posRowid[iBuf] = to-rowid(entry(iBuf, repositionRowid)) .
         end.
         cQuery = trim(hQuery:prepare-string, ' .':u).
         if
            index (cQuery, 'indexed-reposition':u) = 0 then
         do:
            if hQuery:is-open then
               hQuery:query-close () .
            hQuery:forward-only = false.
            hQuery:query-prepare(cQuery + ' indexed-reposition':u) .
         end.
         if not hQuery:is-open then
            hQuery:query-open().
         hQuery:reposition-to-rowid(posRowid) .
         if goBackward eq true then
         do:
            if skipRecord then
               hQuery:reposition-backward(hBuf:batch-size) .
            else
               hQuery:reposition-backward(hBuf:batch-size - 1) .
         end.
         else
         do:
            if skipRecord then hQuery:reposition-forward(1) .
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

   method public logical LoadData ():
      return fillDataSet().

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method private logical fillDataSet():
      define variable numBuf       as integer no-undo.

      do numBuf = 1 to datasetHandle:num-top-buffers:
         if not fillBuffer (datasetHandle:get-top-buffer(numBuf)) then
            return false.
      end.
      return true.
   end method.

   method private logical needFill (hBuf as handle):
      if not valid-object(DataRequest) or DataRequest:HasRequest(hBuf:table) then
         return true.
      if valid-handle(hBuf:parent-relation) then
         return needFill(hBuf:parent-relation:parent-buffer).
      return false.
   end method.

   method private logical fillBuffer(hBuf as handle):
      return fillBuffer(hBuf, true).
   end method.


   method private logical IsFirstRecord(hBuf as handle, lastBatch as logical, numRecords as integer):
       define variable hQry as handle no-undo.
       define variable qryString as character no-undo.
       define variable dsQuery     as character no-undo.
       define variable oldRowid    as rowid     no-undo.
       define variable newRowid    as rowid     no-undo.
       define variable hTT         as handle    no-undo.
       define variable fieldName    as character no-undo.
       define variable fieldValue   as character no-undo.
       define variable operName     as character no-undo.

       hTT  = hBuf:table-handle.
       hQry = hBuf:data-source:query .
       dsQuery = hQry:prepare-string.
       qryString = updateDataSourceQuery(table-handle hTT by-reference) .
       if Util:IsEmpty(qryString) then
          return false.

       /*get the rowid of the first record with the current query*/
       hQry:query-prepare(qryString).
       hQry:query-open().

       if  lastBatch
       and hBuf:batch-size lt numRecords then do:
           if hQry:get-last() then
           do:
               if hQry:reposition-backward (hBuf:batch-size) then
               oldRowid = hQry:get-buffer-handle:rowid.
           end.
       end.
       else do:
           if hQry:reposition-to-row(1) then
           do:
               hQry:get-first().
               oldRowid = hQry:get-buffer-handle:rowid.
           end.
       end.

       hQry:query-close().

       hQry:query-prepare(dsQuery).

       /*get the rowid of the first record removing the search filter from the query*/
       if valid-object(DataRequest) then
       do:
           /*remove the search filter*/
           DataRequest:GetSearch(hTT:name, 1, output fieldName, output fieldValue).
           RemoveFilter(fieldName,fieldValue, output operName).
           qryString  = updateDataSourceQuery(table-handle hTT by-reference).
           if Util:IsEmpty(qryString) then
              return false.

           hQry:query-prepare(qryString).
           hQry:query-open().
           if hQry:reposition-to-row(1) then
           do:
               hQry:get-first().
               newRowid = hQry:get-buffer-handle:rowid.
           end.
           hQry:query-close().
           SetFilter(fieldName, operName, fieldValue).
       end.
       /*if the rowids are equal than the record is the first from the table*/
       if newRowid = oldRowid then
           return true.
       return false.
       finally:
           if valid-handle(hTT) then
              delete object hTT.
       end finally.
   end method.

   method private logical fillBuffer(hBuf as handle, useSearchFilter as logical):
      define variable numRel       as integer    no-undo.
      define variable numKey       as integer    no-undo.
      define variable numRec       as integer    no-undo.
      define variable keyName      as character  no-undo.
      define variable dsQuery      as character  no-undo.
      define variable qryString    as character  no-undo.
      define variable startRowid   as character  no-undo.
      define variable startRow     as integer    no-undo   initial ?.
      define variable hTT          as handle     no-undo.
      define variable hQry         as handle     no-undo.
      define variable hParentBuf   as handle     no-undo.
      define variable firstBatch   as logical    no-undo.
      define variable lastBatch    as logical    no-undo.
      define variable isSearchReq  as logical    no-undo.

      if not valid-handle(hBuf) then
         return false.

      /* fetch data only if requested... */
      if hBuf:fill-mode eq 'no-fill':u or
         not needFill(hBuf) then
         return true.

      /* no load can't be done without a valid data source */
      if not valid-handle(hBuf:data-source) then
         return false.


      /* if we don't use the search request we reset filters to use only regular ones */
      if valid-object(DataRequest) and DataRequest:IsSearchRequest(hBuf:table) then
      do:
         isSearchReq = true.
         if useSearchFilter eq false then
            SetRequestFilters (hBuf:table, false).
      end.

      /* update query string: filters and sort */
      assign
         hTT                  = hBuf:table-handle
         hTT:tracking-changes = false
         hQry                 = hBuf:data-source:query
         dsQuery              = hQry:prepare-string
         qryString            = updateDataSourceQuery(table-handle hTT by-reference).

       if Util:IsEmpty(qryString) then
          return false.

       if qryString ne ? then
       do:
           if valid-object(DataRequest) then
           do:
               /* use start rowid provided only if not search request */
               if isSearchReq eq false then
               do:
                   assign
                       startRowid = Util:Nvl(DataRequest:GetStartRowId(hTT:name), 'first':u)
                       startRow   = DataRequest:GetStartRow(hTT:name).

               end.
               else
               do:
                   /* if search request didn't found any record we send back the last batch */
                   if useSearchFilter eq false then
                       startRowid = 'last':u.
               end.
           end.
           else
               startRowid = 'first':u.

           if hQry:is-open then
               hQry:query-close().

           if valid-handle(hBuf:parent-relation) and needFill(hBuf:parent-relation:parent-buffer) then
           do:
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
                       hBuf:fill().
                   end.
                   else
                   do:
                       /* server-side child filter, send only records related to the first record from parent */
                       hParentBuf:find-first('':u, no-lock).
                       do numKey = 1 to num-entries(hBuf:parent-relation:relation-fields) by 2:
                           assign
                               keyName   = entry(numKey, hBuf:parent-relation:relation-fields)
                               qryString = replace(qryString,
                                              substitute('&1.&2':u, hParentBuf:table, keyName),
                                              substitute('&1':u, quoter(hParentBuf:buffer-field(keyName):buffer-value))).
                       end.
                       hQry:query-prepare(qryString) no-error.
                       if Util:IsError() then
                       do:
                           ThrowError(100, 'msg_err_data_invalid_query':u, hTT:name, qryString).
                           undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
                       end.

                       if valid-object(ErrorManager) and ErrorManager:GetLogLevel() ge ErrorManager:DebugLevel() then
                       do numKey = 1 to hQry:num-buffers:
                           if
                               index ('whole-index,':u, hQry:index-information(numKey)) eq 1 then
                           do:
                               ThrowDebug(100, 'msg_err_whole_index':u,
                                   substitute('&1 - &2':u, hQry:get-buffer-handle(numKey):name, hQry:index-information(numKey)), '':u).
                           end.
                       end.

                       hQry:query-open().
                       /* reposition to start rowid, for search request don't skip first row */
                       if startRowid ne 'first':u then
                       do:
                           RepositionToRow(hTT:name, startRowid,
                               DataRequest:GetBatchSize(hTT:name) lt 0,
                               isSearchReq eq false and DataRequest:GetSkipRow(hTT:name)).
                       end.
                       else
                       do:
                           if startRow ne ? then
                               RepositionToRow(hTT:name, startRow,
                                   DataRequest:GetBatchSize(hTT:name) lt 0,
                                   isSearchReq eq false and DataRequest:GetSkipRow(hTT:name)).
                       end.
                       numRec = fillBuffer(hBuf, hQry, hBuf:batch-size).
                   end.
               end.
               else assign
                       firstBatch = true
                       lastBatch  = true.
           end.
           else
           do:
               hQry:query-prepare(qryString) no-error.
               if Util:IsError() then
               do:
                   ThrowError(100, 'msg_err_data_invalid_query':u, hTT:name, qryString).
                   undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
               end.
               hQry:query-open() .

               /* reposition to start rowid, for search request don't skip first row */
               if startRowid ne 'first':u then
               do:
                   /* don't reposition if batch-size is 0, all rows need to be loaded */
                   if BatchSize ne 0 then
                       RepositionToRow(hTT:name, startRowid,
                           DataRequest:GetBatchSize(hTT:name) lt 0,
                           isSearchReq eq false and DataRequest:GetSkipRow(hTT:name)).
                   else
                       setNewPosition(hTT:name, startRowid).
               end.
               else
               do:
                   if startRow ne ? then
                       RepositionToRow(hTT:name, startRow,
                           DataRequest:GetBatchSize(hTT:name) lt 0,
                           isSearchReq eq false and DataRequest:GetSkipRow(hTT:name)).
               end.
               numRec = fillBuffer(hBuf, hQry, hBuf:batch-size).
           end.

           /* if no record found using the search criteria, send the last batch - [less than] */
           if numRec eq 0 and isSearchReq eq true and useSearchFilter eq true then
           do:
               hQry:query-close().
               hQry:query-prepare(dsQuery).
               fillBuffer(hBuf, false).
           end.
           else
           do:
               /* see if we have the last batch  */
               if not lastBatch then
               do:

                   if  numRec lt hBuf:batch-size then do:
                       lastBatch = true.
                   end.
                   else do:
                       assign
                           numRec    = numRec + 1
                           lastBatch = not hQry:reposition-forward(1).
                   end.
               end.

               /* see if we have the first batch */
               if not firstBatch then
               do:
                   if startRowid eq 'first':u then
                       firstBatch = true.
                   else
                   do:
                       if isSearchReq eq false then do:
                           firstBatch = not hQry:reposition-backward(numRec + 1).
                       end.
                       else
                       do:
                           hQry:query-prepare(dsQuery).
                           firstBatch = IsFirstRecord(hBuf,lastBatch,numRec).
                       end.
                   end.
               end.
               hQry:query-close().

               if not setBatchInfo(hTT:name, firstBatch , lastBatch) then
               do:
                   ThrowError(700, 'cannot create ttinfo', 'fillbuffer', '').
                   return false.
               end.
           end.
       end.

       /* fill all child tables */
       do numRel = 1 to hBuf:num-child-relations:
           if not fillBuffer(hBuf:get-child-relation(numRel):child-buffer) then
               return false.
       end.

       return true.

       finally:
           if valid-handle(hTT) then
              delete object hTT.
       end finally.
   end method.

   method private integer fillBuffer(hBuf as handle, hQry as handle, batchSize as integer):
      define variable recRowid     as character  no-undo.
      define variable startRowid   as character  no-undo.
      define variable numRec       as integer    no-undo.
      define variable numBuf       as integer    no-undo.
      define variable hSrcBuf      as handle     no-undo.
      define variable fieldMap     as character  no-undo   extent 16.
      define variable isPrefetch   as logical    no-undo.

      if not hQry:is-open then
         hQry:query-open().

      if valid-object(DataRequest) then
         startRowid = DataRequest:GetStartRowId(hBuf:table-handle:name).

      /* get field map for each data source buffers */
      if hBuf:data-source:num-source-buffers eq 1 then
         fieldMap[1] = hBuf:data-source-complete-map.
      else
      do numBuf = 1 to hBuf:data-source:num-source-buffers:
         fieldMap[numBuf] = getBufferFieldMap(hBuf:data-source:get-source-buffer(numBuf):name, hBuf:data-source-complete-map).
      end.

      fillBuff:
      do while hQry:is-open
         and (hBuf:batch-size eq 0 or numRec lt batchSize)
         and hQry:get-next(no-lock)
            on error undo, throw:

         assign
            recRowid = '':u
            numRec   = numRec + 1.
         hBuf:buffer-create().

         do numBuf = 1 to hBuf:data-source:num-source-buffers:
            assign
               hSrcBuf = hBuf:data-source:get-source-buffer(numBuf)
               recRowid = substitute('&1,&2':u, recRowid, if hSrcBuf:available then string(hSrcBuf:rowid) else '?':u).

            /* exclude fields list are set on source's buffer private-data */
            if hSrcBuf:available then
               hBuf:buffer-copy(hSrcBuf, hSrcBuf:private-data, fieldMap[numBuf]).
         end.
         setDbRowid (hBuf, trim(recRowid, ',':u)).

      end.

      return numRec.
   end method.


   method protected final void setDbRowid (hBuf as handle, dbRowid as character):

      for each ttRowid
         where ttRowId.ttName  eq hBuf:table
         and ttRowId.ttRowId eq string(hBuf:rowid)
            on error undo, throw:
         ttRowId.dbRowId = dbRowid.
         return.
      end.

      create ttRowId.
      assign
         ttRowId.dbRowId = dbRowid
         ttRowId.ttName  = hBuf:table
         ttRowId.ttRowId = string(hBuf:rowid).


      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method protected final character getDbRowid (hBuf as handle):

      if valid-handle(hBuf) and hBuf:type eq 'buffer':u then
         return getDbRowid (hBuf:table, string(hBuf:rowid)).
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method protected final character getDbRowid (bufName as character, bufRowId as character):
   define buffer ttRowId for ttRowId.

      for each ttRowid
         where ttRowId.ttName  eq bufName
         and ttRowId.ttRowId eq bufRowId:
         return ttRowId.dbRowId.
      end.
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method protected final character getNewPosition (tableName as character):
   define buffer ttInfo for ttInfo.

      for each ttInfo where ttInfo.ttName eq tableName:
         return ttInfo.ttRowPos.
      end.
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method protected final logical setNewPosition (tableName as character, newRowid as character):
   define buffer ttInfo for ttInfo.

      newRowid = Util:Nvl(getDbRowid(tableName, newRowid), newRowid).

      for each ttInfo where ttInfo.ttName eq tableName
         on error undo, throw:
         ttInfo.ttRowPos = newRowid.
         return true.
      end.
      if valid-handle(datasetHandle:get-buffer-handle(tableName)) then
      do:
         create ttInfo.
         assign
            ttInfo.ttName   = tableName
            ttInfo.ttRowPos = newRowid.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method protected final logical setBatchInfo (tableName as character, firstBatch as logical, lastBatch as logical):
   define buffer ttInfo for ttInfo.

      for each ttInfo where ttInfo.ttName eq tableName
         on error undo, throw:
         assign
            ttInfo.firstBatch = firstBatch
            ttInfo.lastBatch  = lastBatch.
         return true.
      end.
      if valid-handle(datasetHandle:get-buffer-handle(tableName)) then
      do:
         create ttInfo.
         assign
            ttInfo.ttName     = tableName
            ttInfo.firstBatch = firstBatch
            ttInfo.lastBatch  = lastBatch.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical  SaveData ():
      define variable hBuf as handle  no-undo.
      define variable iBuf as integer no-undo.

      do iBuf = 1 to datasetHandle:num-top-buffers:
         hBuf = datasetHandle:get-top-buffer(iBuf).
         if DataRequest:HasRequest(hBuf:table) and not saveTable(hBuf) then
            return false.
      end.

      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method private character getDuplicateKeyField (hBuf as handle, hSrcBuf as handle):
      define variable fldName  as character  no-undo.

      if
         index(error-status:get-message(error-status:num-messages), 'already exists with':u) eq 0 then
         return ?.

      assign
         fldName = error-status:get-message(error-status:num-messages)
         fldName = entry(4, substring(fldName, index(fldName, 'already exists with':u), -1), ' ') .

      if hBuf:attached-pairlist ne ? and lookup(fldName, hBuf:attached-pairlist) gt 0 then
         assign
            fldName = substitute('&1.&2', hSrcBuf:name, fldName)
            fldName = entry(2, entry(lookup(fldName, hBuf:data-source-complete-map) - 1, ',':u), '.':u).
      return fldName.
   end method.

   method private logical saveTable (input hBuf as handle):
      define variable iRel     as integer    no-undo.
      define variable iFld     as integer    no-undo.
      define variable retVal   as logical    no-undo   initial true.
      define variable startRow as character  no-undo.
      define variable fldName  as character  no-undo.
      define variable roFields as character  no-undo.
      define variable fieldMap as character  no-undo.
      define variable saveMap  as character  no-undo.
      define variable hRel     as handle     no-undo.
      define variable hBi      as handle     no-undo.
      define variable hSrcBuf  as handle     no-undo.
      define variable hQry     as handle     no-undo.

      if not valid-handle(hBuf) or
         not valid-handle(hBuf:before-buffer) or
         not hBuf:before-buffer:table-handle:has-records then
         return true.

      if not valid-handle(hBuf:data-source) then do:
         ThrowError(100, 'msg_err_data_no_source':u, hBuf:table, ?).
         return false.
      end.

      /* fields set in exclude list on AddDataSource are considered read-only as well */
      assign
         hBi       = hBuf:before-buffer
         hSrcBuf   = hBuf:data-source:get-source-buffer(1)
         roFields  = Util:Nvl(hSrcBuf:private-data, '':u) .
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
         do iFld = 1 to num-entries(ReadOnlyFields):
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
         hSrcBuf:disable-load-triggers (false) .
         update_block:
         do while hQry:is-open and hQry:get-next(no-lock)
            on error undo, throw:
            hBuf:find-by-rowid(hBi:after-rowid) no-error.
            /*roxanam : watch out if hBuf is available*/
            /* locate the record in the database, nothing to do for new records */
            if hBi:row-state ne row-created then
            do:
               hSrcBuf:find-first(hBuf:data-source:save-where-string(1), exclusive-lock, no-wait) .
               /* record not found in database */
               if not hSrcBuf:available then
               do:
                  if hSrcBuf:locked then
                     ThrowError(101, 'msg_err_data_locked':u, hBuf:name, ?).
                  else
                     ThrowError(102, 'msg_err_data_not_found':u, hBuf:name, ?).

                  retVal = false.
                  leave update_block.
               end.
            end.

            case hBi:row-state:
               /* new record */
               when row-created then
                  do:
                     if not BeforeRowSave(hBuf, ?, ?) then
                     do:
                        retVal = false.
                        leave update_block.
                     end.
                     hSrcBuf:buffer-create()   .
                     hSrcBuf:buffer-copy(hBuf, roFields, saveMap) no-error.
                     if Util:IsError() then
                     do:
                        if error-status:get-number(1) eq 132 then
                           ThrowError(106, 'msg_err_data_dupe':u, hBuf:name, getDuplicateKeyField(hBuf, hSrcBuf)).
                        else
                           ThrowError(106, 'msg_err_data_update':u, hBuf:name, ?).
                        retVal = false.
                        undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
                     end.
                     if not AfterRowSave(hBuf, hSrcBuf) then
                     do:
                        retVal = false.
                        leave update_block.
                     end.
                     hSrcBuf:buffer-validate() no-error.
                     if Util:IsError() then
                     do:
                        if error-status:get-number(1) eq 132 then
                           ThrowError(103, 'msg_err_data_dupe':u, hBuf:name, getDuplicateKeyField(hBuf, hSrcBuf)).
                        else
                           ThrowError(103, 'msg_err_data_create':u, hBuf:name, ?).
                        retVal = false.
                        undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
                     end.
                     startRow = string(hSrcBuf:rowid).
                     /* refresh client record, in case any database trigger updated other fields */
                     hBuf:buffer-copy(hSrcBuf, hSrcBuf:private-data, fieldMap) .
                     /*
                        for new record update the rowid for the client,
                        we only have the rowid of updated buffer now
                     */
                     setDbRowid(hBuf, startRow).
                  end.
               /* delete record */
               when row-deleted then
                  do:
                     if not BeforeRowDelete(hBi, hSrcBuf) then
                     do:
                        retVal = false.
                        leave update_block.
                     end.
                     hSrcBuf:buffer-delete() no-error.
                     if Util:IsError() then
                     do:
                        ThrowError(104, 'msg_err_data_delete':u, hBuf:name, ?).
                        retVal = false.
                        undo, throw new Progress.Lang.AppError(error-statu:get-message(1),error-status:get-number(1)).
                     end.
                     if not AfterRowDelete(hBi) then
                     do:
                        retVal = false.
                        leave update_block.
                     end.
                  end.
               /* update record */
               when row-modified then
                  do:
                     if not BeforeRowSave(hBuf, hBi, hSrcBuf) then
                     do:
                        retVal = false.
                        leave update_block.
                     end.
                     retVal = hSrcBuf:buffer-compare(hBi, 'binary':u, roFields, saveMap, false).
                     /* compare before-image with 'current' data */
                     if retVal eq false then
                     do:
                        ThrowError(105, 'msg_err_data_compare':u, hBuf:name, ?).
                        leave update_block.
                     end.
                     /* save changes */
                     hSrcBuf:buffer-copy(hBuf, roFields, saveMap) no-error.
                     if Util:IsError() then
                     do:
                        if error-status:get-number(1) eq 132 then
                           ThrowError(106, 'msg_err_data_dupe':u, hBuf:name, getDuplicateKeyField(hBuf, hSrcBuf)).
                        else
                           ThrowError(106, 'msg_err_data_update':u, hBuf:name, ?).
                        retVal = false.
                        undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
                     end.
                     if not AfterRowSave(hBuf, hSrcBuf) then
                     do:
                        retVal = false.
                        leave update_block.
                     end.
                     hSrcBuf:buffer-validate() no-error.
                     if Util:IsError() then
                     do:
                        if error-status:get-number(1) eq 132 then
                           ThrowError(106, 'msg_err_data_dupe':u, hBuf:name, getDuplicateKeyField(hBuf, hSrcBuf)).
                        else
                           ThrowError(106, 'msg_err_data_update':u, hBuf:name, ?).
                        retVal = false.
                        undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
                     end.
                     /* get full rowid in case datasource has more than one buffer */
/*                     startRow = getDbRowid(hBuf).*/
                     /* refresh client record, in case any database trigger updated other fields */
                     hBuf:buffer-copy(hSrcBuf, hSrcBuf:private-data, fieldMap) .
                  end.
            end case.
         end.
         if retVal eq false then
            return false.
         /* set new position to latest changed record */
         if startRow ne '':u then
            setNewPosition(hBuf:table, startRow).
      end.
      do iRel = 1 to hBuf:num-child-relations:
         hRel = hBuf:get-child-relation(iRel) .
         if not saveTable(hRel:child-buffer) then
            return false.
      end.
      return true.

      finally:
         if valid-handle(hQry) then do:
            hQry:query-close().
            delete object hQry.
         end.
     end finally.
   end method.

   method protected logical BeforeRequest (actionName as character):
      return true.
   end method.

   method protected void    AfterRequest  (actionName as character):
   end method.

   method public final logical HandleRequest (methodName as character, webRequest as Request, webResponse as Response):

      /* enable cache if defined as such or for paint requests when open on init set to false */
      assign
         Request    = webRequest
         Response   = webResponse
         ActionName = methodName
         Response:EnableCache = EnableCache or
                                  (methodName eq ACTION_PAINT and OpenOnInit eq false).

      if BeforeRequest(methodName) then
      do:
         case actionName:
            when ACTION_PAINT then
               HandleRequestPaint().
            when ACTION_DATA  then
               HandleRequestData().
            when ACTION_LOOKUP then
               HandleRequestLookup().
            when ACTION_AUTOCOMPLETE then
               HandleRequestAutocomplete().
            when ACTION_REPORT then
               HandleRequestReport().
            otherwise
            HandleRequestService().
         end case.

         AfterRequest(methodName).
      end.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method protected logical BeforeSaveData ():
      return true.
   end method.

   method protected logical AfterSaveData ():
      return true.
   end method.

   method protected logical BeforeLoadData ():
      return true.
   end method.

   method protected void AfterLoadData ():
   end method.

   method protected void BeforeDataOutput ():
   end method.

   method protected logical BeforeRowSave (hBuff as handle, hBuffBi as handle, hDbBuff as handle):
      return true.
   end method.

   method protected logical AfterRowSave (hBuff as handle, hDbBuff as handle):
      return true.
   end method.

   method protected logical BeforeRowDelete (hBuffBi as handle, hDbBuff as handle):
      return true.
   end method.

   method protected logical AfterRowDelete  (hBuffBi as handle):
      return true.
   end method.

   method private void dataFetch ():
      SetBatchSize(BatchSize).
      if not BeforeLoadData()   or
         not LoadData() then
         return.
      AfterLoadData().
   end method.

   method private void dataUpdate ():

      if not valid-handle(datasetHandle) then do:
         ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).
         return.
      end.

      /* return error if read-only */
      if ReadOnly then do:
         ThrowError(100, 'msg_err_data_read-only':u, ?, ?).
         return.
      end.

      /* call developer hook, before data save - leave if canceled */
      if not BeforeSaveData() then
         return.
      do transaction
         on error undo, throw:
         if not SaveData() or           /* leave if error ocured in data save */
            not AfterSaveData() then    /* call developer hook, after data save */
            undo, return.
      end.
      /* finally accept the dataset changes */
      datasetHandle:accept-changes().
   end method.

   method private void dataOutput():
      define variable numBuf		as integer		no-undo.
      define variable firstBuffer	as logical		no-undo initial true.
      define variable hBuf			as handle		no-undo.
      define variable mpOut			as memptr		no-undo.
      define variable cLogDirectory	as character	no-undo.
      define variable cFileName		as character	no-undo.

      /* make sure we use the right localization */
      if valid-object(Localization) then
         assign
            session:date-format    = Localization:GetDateFormat()
            session:numeric-format = Localization:GetNumericFormat().

      if valid-object(ErrorManager) and ErrorManager:GetLogLevel() ge ErrorManager:DebugLevel()
      then do:
         cLogDirectory = Util:GetLogDirectory().

         cFileName = cLogDirectory + '/data_response.xml':u.

         datasetHandle:write-xml('file', cFileName, true, 'utf-8', ?, false, true, true) .
      end.

      ThrowDebug(500, 'Serialize - start.':u, 'DataCollection':u, '':u).

      set-size(mpOut) = 0.

      case DataRequest:OutputFormat:
         when 'json':u then
            do:
               if valid-object(JsonWriter) then
               do:
                  JsonWriter:SetDestination(mpOut).
                  JsonWriter:OpenStream().
                  JsonWriter:StartElement('').
                  JsonWriter:Out('"tables": [':u).
                  do numBuf = 1 to datasetHandle:num-top-buffers:
                     assign
                        hBuf = datasetHandle:get-top-buffer(numBuf)
                        hBuf:table-handle:private-data = DataRequest:GetStartRowId(hBuf:name) no-error.
                     if index(exceptTables,hBuf:name) > 0 then next.
                     dataOutput(input-output firstBuffer, hBuf).
                  end.
                  JsonWriter:Out(']':u).

                  /* dump the rest of dataset generic errors */
                  serializeErrors(?).
                  JsonWriter:EndElement('').
                  JsonWriter:CloseStream().
                  Response:Out(mpOut).
                  set-size(mpOut) = 0.
               end.
            end.
         when 'xml':u then
            do:
               datasetHandle:write-xml('memptr':u, mpOut, false, 'utf-8':u, ?, true, true, true).
               Response:Out(mpOut).
               set-size(mpOut) = 0.
            end.
      end case.
      ThrowDebug(500, 'Serialize - end.':u, 'DataCollection':u, '':u).
   end method.

   method private void dataOutput(input-output firstBuffer as logical, hBuf as handle):
      define variable numRel as integer no-undo.
      define variable hTT    as handle  no-undo.

      if not valid-handle(hBuf) or
         not valid-object(JsonWriter) then
         return.

      hTT = hBuf:table-handle.

      /* send data only if requested... */
      if DataRequest:HasRequest(hTT:name) then
      do:
         JsonWriter:Out(substitute('&1~{"id": "&2",':u, if firstBuffer then '' else ',', lower(hTT:name))).
         serializeBufferToJson(table-handle hTT by-reference) .
         JsonWriter:Out('~}').
         firstBuffer = false.
      end.

      do numRel = 1 to hBuf:num-child-relations:
         dataOutput(input-output firstBuffer, hBuf:get-child-relation(numRel):child-buffer).
      end.
      finally:
         if valid-handle(hTT) then
            delete object hTT .
      end finally.
   end method.

   method private void HandleRequestAutocomplete ():
      define variable arrValue   as com.quarix.base.Collection no-undo.
      define variable keyValue   as character                  no-undo.

      if not valid-handle(datasetHandle) then
      do:
         ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).
         return.
      end.

      if not AttachDataSource() then
      do:
         ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
         return.
      end.

      if not BeforeLoadData() then
         return.

      assign
         Response:ResponseType = Response:RESPONSE_DIRECT
         Response:ContentType  = 'application/json':u
         arrValue              = AutocompleteKey (Request:GetVariable({&req-var-lookup-table}),
                                                    Request:GetVariable({&req-var-lookup-col}),
                                                    Request:GetVariable({&req-var-lookup-val})).
      Response:Out('~{"values": [':u).
      if valid-object(arrValue) then
      do:
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
   end method.

   method private void HandleRequestLookup ():

      if not valid-handle(datasetHandle) then
      do:
         ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).
         return.
      end.

      if not AttachDataSource() then
      do:
         ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
         return.
      end.

      if not BeforeLoadData() then
         return.

      assign
         Response:ResponseType = Response:RESPONSE_DIRECT
         Response:ContentType  = 'text/html':u.

      Response:Out(LookupKeyValue(Request:GetVariable({&req-var-lookup-table}),
         Request:GetVariable({&req-var-lookup-col}),
         Request:GetVariable({&req-var-lookup-val}),
         Request:GetVariable({&req-var-lookup-desc}))).
   end method.

   method private void HandleRequestData ():
      define variable mpReq   as memptr      no-undo.

      DataRequest = cast(GetInstance('com.quarix.data.DataRequest':u), 'com.quarix.data.DataRequest':u).
      if not valid-object(DataRequest) then
      do:
         ThrowError(1000, 'msg_err_load_obj':u, ?, ?).
         return.
      end.

      if not AttachDataSource() then
      do:
         ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
         return.
      end.

      assign
         DataRequest:ObjectType = 'DataCollection':u
         mpReq = Request:GetBlobVariable({&req-var-xml-data})
         Response:ResponseType = Response:RESPONSE_DIRECT
         Response:ContentType  = 'application/json':u.

      if get-size(mpReq) gt 0 then
      do:
         DataRequest:Parse(mpReq).
         set-size(mpReq) = 0.
         LoadRequestData().
      end.

      ActionName = DataRequest:ActionName.
      case ActionName:
         when ACTION_DATA_FILL then
            HandleRequestDataFetch().
         when ACTION_DATA_SAVE then
            HandleRequestDataSave().
         otherwise
         HandleRequestDataService().
      end case.
      BeforeDataOutput().

      dataOutput().
      UnloadInstance(DataRequest).

   end method.

   method private void HandleRequestReport():
      define variable mpReq   as memptr      no-undo.
      define variable lcRes   as longchar    no-undo.

      if not AttachDataSource() then
      do:
         ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
         return.
      end.

      mpReq = Request:GetBlobVariable({&req-var-xml-data}).

      if get-size(mpReq) gt 0 then
      do:
         assign
            DataRequest            = cast(GetInstance('com.quarix.data.DataRequest':u), 'com.quarix.data.DataRequest':u)
            DataRequest:ObjectType = 'DataCollection':u.
         if not valid-object(DataRequest) then
         do:
            set-size(mpReq) = 0.
            ThrowError(1000, 'msg_err_load_obj':u, ?, ?).
            return.
         end.
         DataRequest:Parse(mpReq).
         set-size(mpReq) = 0.
         LoadRequestData().
      end.

      /* no batch for reports, send all records */
      assign
         Response:ResponseType = Response:RESPONSE_REPORT
         Response:ContentType  = 'text/xml':u
         BatchSize             = 0.

      HandleRequestDataFetch().

      BeforeDataOutput().

      /* switch to american locale to send data, localization done in birt */
      Localization:SetNumericFormat('american':u).
      Localization:SetDateFormat('mdy':u).

      fix-codepage(lcRes) = 'utf-8':u.

      datasetHandle:write-xml('longchar':u, lcRes, false, 'utf-8':u, ?, false, true).
      Response:Out(lcRes).

      UnloadInstance(DataRequest).
   end method.

   method private void HandleRequestDataService():
      if not BeforeDataService (ActionName) then
         return.
      HandleRequestDataService(ActionName).
      AfterDataService (ActionName).
   end method.

   method private void HandleRequestDataFetch ():
      dataFetch().
   end method.

   method private void HandleRequestDataSave ():
      dataUpdate().
   end method.

   method protected logical BeforeDataService (actionName as character):
      return true.
   end method.

   method protected void AfterDataService (actionName as character):
   end method.

   method protected void HandleRequestDataService (actionName as character):
      ThrowError(1000, 'msg_err_no_handler':u, ?, ?).

      catch appError as Progress.Lang.Error :
      	ThrowError(input appError).
      	delete object appError.
      end catch.
   end method.

   method private void HandleRequestService ():
      if not BeforeService (ActionName) then
         return.
      HandleRequestService (ActionName).
      AfterService (ActionName).
   end method.

   method protected logical BeforeService (actionName as character):
      return true.
   end method.

   method protected void AfterService (actionName as character):
   end method.

   method protected void HandleRequestService (actionName as character):
      ThrowError(1000, 'msg_err_no_handler':u, ?, ?).
   end method.

   method private logical LoadRequestData ():
      define variable numBuf   as integer   no-undo.
      define variable hTT      as handle    no-undo.

      ThrowDebug(500, 'De-serialize - start.':u, 'DataCollection':u, '':u).
      do numBuf = 1 to datasetHandle:num-buffers:
         assign
            hTT = datasetHandle:get-buffer-handle(numBuf):table-handle
            hTT:private-data = string(temp-table ttRowId:handle).

         if not DataRequest:LoadData(table-handle hTT by-reference) then do:
             /* throw error */
             return false.
         end.

         SetRequestFilters (hTT:name).
         SetRequestSort    (hTT:name).
      end.
      ThrowDebug(500, 'De-serialize - end.':u, 'DataCollection':u, '':u).
      return true.

      finally:
         if valid-handle(hTT) then
            delete object hTT.
      end finally.
   end method.

   method private void SetRequestFilters (tableName as character, considerSearchFilter as logical):
      define variable numFilter    as integer   no-undo initial 1.
      define variable filterCol    as character no-undo.
      define variable filterOp     as character no-undo.
      define variable filterVal    as character no-undo.
      define variable reverseSort  as logical   no-undo.

      do while DataRequest:GetSearch(tableName, numFilter, output filterCol, output filterVal)
         on error undo, throw:
         if considerSearchFilter and filterVal ne ? then
         do:
            reverseSort = DataRequest:IsSortDescending(tableName, filterCol).
            SetFilter (substitute('&1.&2':u, tableName, filterCol),
               if reverseSort eq ? or reverseSort eq false then 'ge':u else 'le':u,
               filterVal).
/*            SetSort (substitute('&1.&2':u, tableName, filterCol), reverseSort).*/
         end.
         else
            RemoveFilter(substitute('&1.&2':u, tableName, filterCol)).
         numFilter = numFilter + 1.
      end.

      numFilter = 1.
      do while DataRequest:GetFilter(tableName, numFilter, output filterCol, output filterOp, output filterVal)
         on error undo, throw:
         if filterVal ne ? then
            SetFilter (substitute('&1.&2':u, tableName, filterCol), filterOp, filterVal).
         numFilter = numFilter + 1.
      end.
   end method.

   method private void SetRequestFilters (tableName as character):
      SetRequestFilters (tableName, true).
   end method.

   method private void SetRequestSort (tableName as character):
      define variable numSort   as integer   no-undo initial 1.
      define variable sortCol   as character no-undo.
      define variable sortDesc  as logical   no-undo.

      do while DataRequest:GetSort(tableName, numSort, output sortCol, output sortDesc)
         on error undo, throw:

         SetSort (substitute('&1.&2':u, tableName, sortCol), sortDesc).
         numSort = numSort + 1.
      end.
   end method.

   method protected final character getTableSort (tableName as character):
      define variable sortBy    as character no-undo.
      define buffer   ttSort    for ttSort.

      for each ttSort where ttSort.tableName eq tableName by ttSort.sortOrder
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

      if OpenOnInit eq true then
      do:
         dataFetch().
         BeforeDataOutput().
      end.

      if not valid-handle(datasetHandle) then
      do:
         ThrowError(1000, 'msg_err_no_dataset':u, ?, ?).
         return ?.
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
            datasetHandle:write-xml('memptr':u, mpResponse, false, 'utf-8':u, ?, true, true, true) .
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
            saxLogger = cast(GetInstance('com.quarix.service.logging.SaxLogger':u), 'com.quarix.service.logging.SaxLogger':u) no-error.
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

   method private void HandleRequestPaint ():
      define variable mpResponse     as memptr    no-undo.
      define variable responseFormat as character no-undo.

      if OpenOnInit and not AttachDataSource() then
      do:
         ThrowError(100, 'msg_err_data_source_return':u, ?, ?).
         return.
      end.

      set-size(mpResponse) = 0.

      assign
         ActionName     = ACTION_DATA_FILL
         responseFormat = Request:GetVariable('responseFormat':u)
         mpResponse     = GetDescription(responseFormat, Request:ApplicationPath).

      if mpResponse eq ? or get-size(mpResponse) eq 0 then
         return.

      if responseFormat eq 'xml':u then
         assign
            Response:ResponseType = Response:RESPONSE_DIRECT
            Response:ContentType  = 'text/xml':u.

      Response:Out(mpResponse).

      set-size(mpResponse) = 0.

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


   end method.

   method private logical serializeBufferToJson (table-handle tableHandle):
      define variable hBiTable    as handle     no-undo.
      define variable lLastBatch  as logical    no-undo.
      define variable lFirstBatch as logical    no-undo.
      define variable currentPos  as character  no-undo.
      define variable filterArray as character  no-undo.
      define variable sortArray   as character  no-undo.

      define buffer ttInfo for ttInfo.
      if not valid-object(JsonWriter) or not valid-handle(tableHandle) then
         return false.

      /* if we have errors we don't send any thing back beside those */
      if valid-object(ErrorManager) and ErrorManager:GetNumErrors(tableHandle:name) >  0 then do:
         /* send the buffer specific errors here */
         serializeErrors(tableHandle:name).
         return false.
      end.

      /* for manualy filled tables always send first/last to true */
      if not valid-handle(tableHandle:default-buffer-handle:data-source) then
         assign
            lLastBatch  = true
            lFirstBatch = true.
      for each ttInfo where ttInfo.ttName eq tableHandle:name
         on error undo, throw:
         assign
            lLastBatch  = ttInfo.lastBatch
            lFirstBatch = ttInfo.firstBatch
            currentPos  = ttInfo.ttRowPos.
      end.

      /* current image */
      JsonWriter:Out('"rows":[':u).
      if tableHandle:has-records then do:
         tableHandle:private-data = string(temp-table ttRowId:handle).
         JsonWriter:Serialize(table-handle tableHandle by-reference, false, ?, '*', getTableSort(tableHandle:name), true). /* add sort BY clause as 5 param */
      end.

      JsonWriter:Out(']').

      /* before image, if any */
      hBiTable = tableHandle:before-table no-error.
      if valid-handle(hBiTable) and
         hBiTable:has-records then
      do:
         hBiTable:private-data = string(temp-table ttRowId:handle).
         JsonWriter:Out(',"beforeImage":[':u).
         JsonWriter:Serialize(table-handle hBiTable by-reference, false, ?, '*', ?, true).
         JsonWriter:Out(']').
      end.

      JsonWriter:Out(', "info":~{':u).

      JsonWriter:Out(substitute('"changesOnly": &1':u, string(SendChangesOnly, 'true/false':u))).

      if lFirstBatch then
         JsonWriter:Out(', "hasFirstRow": true':u).

      if lLastBatch then
         JsonWriter:Out(', "hasLastRow": true':u).

      if not Util:IsEmpty(currentPos) then
         JsonWriter:Out(substitute(', "newPosition": "&1"':u, currentPos)).

      filterArray =  getFilterArray(tableHandle:name).
      if not Util:IsEmpty(filterArray) then do:
         JsonWriter:Out(', ').
         JsonWriter:AddArray("filter", filterArray). /*add filter array in Json*/
      end.

      sortArray =  getSortArray(tableHandle:name).
      if not Util:IsEmpty(sortArray) then do:
         JsonWriter:Out(', ').
         JsonWriter:AddArray("sort", sortArray). /*add sort array in Json*/
      end.

      JsonWriter:Out('~}':u).

      /* send the buffer specific messages here (warning, info)*/
      if valid-object(ErrorManager) and ErrorManager:GetNumMessages(tableHandle:name) > 0 then do:
         /* send the buffer specific errors here */
         serializeErrors(tableHandle:name).
      end.

      return true.
   end method.

   method private character getFilterArray(tableName as character) :
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
                        + (if bufFilter.fieldValue eq ? then '"?"' else quoter(bufFilter.fieldValue)) + ', '
            numFilters  = numFilters + 1.

      end.

      if numFilters > 0 then
         filterArray = substr(filterArray, 1, length(filterArray) - 2).

      do i = 1 to numFilters - 1:
         filterArray = quoter("AND") + ", " + filterArray.
      end.

      return filterArray.
   end method.

   method private character getSortArray(tableName as character) :
      define buffer   bufSort  for ttSort.

      define variable sortArray   as character no-undo.

      for each bufSort
         where bufSort.tableName = tableName
            on error undo, throw:

         assign sortArray = sortArray + quoter(substitute('&1&2', if bufSort.descOrder then '-' else '',bufSort.fieldName)) + ', '.

      end.

      return trim(trim(sortArray),',').
   end method.

   method private void localizeDataset ():
      define variable numTbl       as integer    no-undo.
      define variable hTT          as handle     no-undo.
      define variable hTTBefore    as handle     no-undo.

      if valid-object(Localization) then
      do:
         do numTbl = 1 to datasetHandle:num-buffers:
            hTT = datasetHandle:get-buffer-handle(numTbl):table-handle.
            if valid-handle(hTT) then
            do:
               localizeTable (table-handle hTT by-reference).
               hTTBefore = hTT:before-table .
               if valid-handle(hTTBefore) then do:
                  localizeTable (table-handle hTT by-reference).
               end.
            end.
         end.
      end.
   end method.

   method private void localizeTable (table-handle hTT):
      define variable numFld        as integer    no-undo.
      define variable hBuf          as handle     no-undo.

      if not valid-object(Localization) then return.

      hBuf = hTT:default-buffer-handle.
      do numFld = 1 to hBuf:num-fields:
         if hBuf:buffer-field(numFld):data-type ne 'logical':u then
            next.
         hBuf:buffer-field(numFld):format = Localization:GetFormat('logical':u, hBuf:buffer-field(numFld):format).
      end.
   end method.

   method private character updateDataSourceQuery (table-handle hTT):
      define variable hDatasource   as handle    no-undo.
      define variable hSrcBuffer    as handle    no-undo.
      define variable prepareString as character no-undo.
      define variable queryString   as character no-undo.
      define variable bufferQuery   as character no-undo.
      define variable iSrcBuf       as integer   no-undo.
      define variable iIndex        as integer   no-undo.

      define buffer bufFilter  for ttFilter.
      define buffer bufSort    for ttSort.

      assign
         hDataSource   = datasetHandle:get-buffer-handle(hTT:default-buffer-handle:name):data-source
         prepareString = trim(replace(hDataSource:query:prepare-string, 'indexed-reposition':u, '':u), ' .':u) .

      if not valid-handle (hDataSource) then return ?.

      /* pass through all source buffers and update query on each */
      if can-find(first bufFilter where bufFilter.tableName eq hTT:name) then
      do:
         do iSrcBuf = 1 to hDataSource:num-source-buffers:
            assign
               hSrcBuffer  = hDataSource:get-source-buffer(iSrcBuf)
               bufferQuery = getBufferQuery(prepareString, input-output iIndex).

            /* see first if we have an outer-join condition, this have to be taken first */
            for each bufFilter
               where bufFilter.tableName eq hTT:name
               and bufFilter.dbFieldName begins substitute('&1.':u, hSrcBuffer:name)
               and bufFilter.fieldName   eq ?
               and bufFilter.fieldValue  begins 'outer-join':u
                  on error undo, throw:
               bufferQuery = substitute('&1 &2', bufferQuery, bufFilter.fieldValue).
               /* we can only have/use one outer-join... */
               leave.
            end.

            /* all filters set on this source buffer, skip all that have outer-join option */
            for each bufFilter
               where bufFilter.tableName   eq hTT:name
               and bufFilter.dbFieldName begins substitute('&1.':u, hSrcBuffer:name)
               and (bufFilter.fieldName  ne ?
               or not bufFilter.fieldValue begins 'outer-join':u)
                  on error undo, throw:
               bufferQuery = substitute('&1 &2 &3':u, bufferQuery,
                  if lookup('where':u, bufferQuery, ' ':u) eq 0 then
                  'where':u
                  else
                  'and':u,
                  if bufFilter.fieldName eq ? then
                  bufFilter.fieldValue
                  else
                  getFieldFilterCondition(bufFilter.dbFieldName, bufFilter.operName, bufFilter.fieldValue)).
            end.
            queryString = substitute('&1 &2 &3':u, queryString, if queryString eq '':u then '':u else ', ':u, bufferQuery).
         end.
      end.
      else
      do:
         queryString = prepareString.
      end.

      /* all sort options set on buffer */
      for each bufSort
         where bufSort.tableName eq hTT:name
            on error undo, throw:
         queryString = substitute('&1 by &2 &3':u, queryString,
            GetPhysicalFieldName (substitute('&1.&2':u, bufSort.tableName, bufSort.fieldName)),
            if bufSort.descOrder then 'descending':u else '':u).
      end.

      return queryString.
   end method.

   method private void serializeErrors (bufferName as character):
      define variable JsonLogger   as com.quarix.service.logging.JsonLogger no-undo.
      define variable numErr       as integer no-undo.

      if valid-object(ErrorManager) and ErrorManager:GetNumMessages() gt 0 then
      do:
         JsonLogger = cast(GetInstance('com.quarix.service.logging.JsonLogger':u),
            'com.quarix.service.logging.JsonLogger':u).
         if valid-object(JsonLogger) then
         do:
            if bufferName eq ? or ErrorManager:GetNumErrors(bufferName) eq 0 then
               JsonWriter:Out(',':u).

            JsonLogger:SetJsonWriter(JsonWriter).
            ErrorManager:LogInfoMessages(JsonLogger, bufferName).
            UnloadInstance(JsonLogger).
         end.
         ErrorManager:Purge(bufferName).
      end.
   end method.

   method private character getFullFieldName (fieldName as character):
      define variable fieldHandle  as handle no-undo.

      fieldName = trim(fieldName).

      case num-entries(fieldName, '.':u):
         when 1 then
            fieldHandle = datasetHandle:get-top-buffer(1):buffer-field(fieldName).
         when 2 then
            fieldHandle = datasetHandle:get-buffer-handle(entry(1, fieldName, '.':u)):buffer-field(entry(2, fieldName, '.':u)).
         otherwise
         return ?.
      end case.

      if not valid-handle(fieldHandle) then
         return ?.

      return substitute('&1.&2':u, fieldHandle:table, fieldHandle:name).
   end method.

   method private character getBufferQuery (queryString as character, input-output startPos as integer):
      define variable indexPos    as integer   no-undo.
      define variable escapeNext  as logical   no-undo.
      define variable quoteChar   as character no-undo.
      define variable thisChar    as character no-undo   initial ?.

      startPos = maximum(startPos + 1, 1).

      if
         index(queryString, ',', startPos) eq 0 then
      do:
         assign
            indexPos = startPos
            startPos = length(queryString, 'character':u).
         return substring(queryString, indexPos, -1, 'character':u).
      end.
      do indexPos = startPos to length(queryString, 'character':u):
         thisChar = substring(queryString, indexPos, 1, 'character':u).
         if quoteChar eq '':u and thisChar eq ',' then
            leave.
         if not escapeNext and
            index('~'"', thisChar) gt 0 then
         do:
            if quoteChar eq thisChar then
               quoteChar = '':u.
            else if quoteChar eq '':u then
                  quoteChar = thisChar.
            next.
         end.
         escapeNext = thisChar eq '~~':u.
      end.
      if thisChar eq ? then
         return ?.

      assign
         thisChar = trim(replace(substring(queryString, startPos, indexPos - startPos), 'indexed-reposition':u, '':u))
         startPos = indexPos.
      return thisChar.
   end method.

   method public final character GetPhysicalFieldName (logicalName as character):
      define variable hTT        as handle    no-undo.
      define variable fieldName  as character no-undo.
      define variable bufNum     as integer   no-undo.

      case num-entries(logicalName, '.':u):
         when 1 then
            do bufNum = 1 to datasetHandle:num-buffers:
               hTT = datasetHandle:get-buffer-handle(bufNum):table-handle.
               fieldName = getPhysicalFieldName(table-handle hTT by-reference, logicalName).

               return fieldName.
            end.
         when 2 then
            do:
               htt = datasetHandle:get-buffer-handle(entry(1, logicalName, '.':u)):table-handle.
               fieldName = getPhysicalFieldName(table-handle hTT by-reference, logicalName).

               return fieldName.
            end.
      end case.
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
      finally:
         if valid-handle(hTT) then
            delete object hTT.
      end finally.
   end method.

   method private character getPhysicalFieldName (table-handle tableHandle, logicalName as character):
      define variable fieldMap  as character no-undo.
      define variable hBuf      as handle    no-undo.

      if not valid-handle(tableHandle) then
         return ?.

      if
         index(logicalName, '.') eq 0 then
         logicalName = substitute('&1.&2', tableHandle:name, logicalName).

      assign
         hBuf     = datasetHandle:get-buffer-handle(tableHandle:default-buffer-handle:name)
         fieldMap = hBuf:data-source-complete-map
     .

      if fieldMap eq ? or lookup(logicalName, fieldMap) = 0 then return ?.
      return entry(lookup(logicalName, fieldMap) + 1, fieldMap).
   end method.

   method public final void SetDataSource (pcTable as character, pcBuffer as character):
      SetDataSource (pcTable, pcBuffer, ?, ?, ?).
   end method.

   method public final void SetDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character):
      SetDataSource (pcTable, pcBuffer, pcKeyFields, ?, ?).
   end method.

   method public final void SetDataSource
      (pcTable     as character, pcBuffer as character,
      pcKeyFields as character, pcWhere  as character):
      SetDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, ?).
   end method.

   method public final void SetDataSource
      (pcTable     as character, pcBuffer as character,
      pcKeyFields as character, pcWhere  as character,
      pcFieldsMap as character):
      SetDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, ?).
   end method.

   method public final void SetDataSource
      (pcTable     as character, pcBuffer as character,
      pcKeyFields as character, pcWhere  as character,
      pcFieldsMap as character, excludeFields as character):
      SetDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, ?).
   end method.

   method public final void SetDataSource
      (pcTable     as character, pcBuffer as character,
      pcKeyFields as character, pcWhere  as character,
      pcFieldsMap as character, excludeFields as character, includeFields as character):
      SetDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, includeFields, ?).
   end method.

   method public final void SetDataSource
      (pcTable     as character, pcBuffer as character,
      pcKeyFields as character, pcWhere  as character,
      pcFieldsMap as character, excludeFields as character, includeFields as character, joinClause as character):

      define variable hBuffer     as handle no-undo.

      hBuffer = datasetHandle:get-buffer-handle (pcTable).
      if not valid-handle (hBuffer) then
         return.

      /* reset buffer data-source if already set */
      DetachDataSource(hBuffer:table).

      AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, includeFields, joinClause).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public final void AddDataSource (pcTable as character, pcBuffer as character):
      AddDataSource (pcTable, pcBuffer, ?).
   end method.

   method public final void AddDataSource (pcTable as character, pcBuffer as character, pcKeyFields as character):
      AddDataSource (pcTable, pcBuffer, pcKeyFields, ?).
   end method.

   method public final void AddDataSource
      (pcTable     as character, pcBuffer    as character,
      pcKeyFields as character, pcWhere     as character):
      AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, ?).
   end method.

   method public final void AddDataSource
      (pcTable     as character, pcBuffer    as character,
      pcKeyFields as character, pcWhere     as character,
      pcFieldsMap as character):
      AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, ?).
   end method.

   method public final void AddDataSource
      (pcTable     as character, pcBuffer     as character,
      pcKeyFields as character, pcWhere       as character,
      pcFieldsMap as character, excludeFields as character):
      AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, ?).
   end method.

   method public final void AddDataSource
      (pcTable     as character, pcBuffer     as character,
      pcKeyFields as character, pcWhere       as character,
      pcFieldsMap as character, excludeFields as character, includeFields as character):
      AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, includeFields, ?).
   end method.

   method public final void AddDataSource
      (pcTable     as character, pcBuffer     as character,
      pcKeyFields as character, pcWhere       as character,
      pcFieldsMap as character, excludeFields as character, includeFields as character, joinClause as character):

      define variable hBuffer     as handle    no-undo.
      define variable hSrcBuffer  as handle    no-undo.
      define variable hDataSource as handle    no-undo.
      define variable hQuery      as handle    no-undo.
      define variable numBuf      as integer   no-undo.
      define variable cQryString  as character no-undo.


      assign
         pcTable     = trim(pcTable)
         pcBuffer    = trim(pcBuffer)
         pcFieldsMap = trim(Util:Nvl(pcFieldsMap, '':u))
         pcWhere     = trim(Util:Nvl(pcWhere, '':u))
         hBuffer     = datasetHandle:get-buffer-handle (pcTable)
         hDataSource = hBuffer:data-source.

      if not valid-handle (hBuffer) then
         return.

      create buffer hSrcBuffer for table pcBuffer.
      if not valid-handle (hSrcBuffer) then
         return.

      /* exclude fields list are set on source's buffer private-data */
      assign
         cQryString              = substitute('for each &1':u, pcBuffer)
         hSrcBuffer:private-data = getBufferExcludeFields (hBuffer, excludeFields, includeFields).

      if not valid-handle(hDataSource) then
      do:
         create data-source hDataSource.
      end.
      else if hDataSource:num-source-buffers gt 0 then
         do:
            assign
               cQryString = substitute('&1, each &2':u, trim(hDataSource:query:prepare-string, ' .':u), pcBuffer)
               pcFieldsMap = trim(substitute('&1,&2', hBuffer:data-source-complete-map, pcFieldsMap), ',':u).
            hBuffer:detach-data-source().
         end.

      hDataSource:add-source-buffer (hSrcBuffer, pcKeyFields) no-error.
      if Util:IsError() then
      do:
         DetachDataSource(pcTable).
         ThrowError(100, 'msg_err_data_invalid_source':u, pcTable, ?).
         undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
      end.

      if not valid-handle(hBuffer:data-source) then
         hBuffer:attach-data-source(hDataSource, pcFieldsMap, hSrcBuffer:private-data).

      if not valid-handle(hDataSource:query) then
      do:
         create query hQuery.
         do numBuf = 1 to hDataSource:num-source-buffers:
            hQuery:add-buffer(hDataSource:get-source-buffer(numBuf)).
         end.
         hDataSource:query = hQuery.
      end.
      else do:
        /* sometime the add-source-buffer method on data source does not add the new buffer to the query */
        if hDataSource:query:num-buffers lt hDataSource:num-source-buffers then
            hDataSource:query:add-buffer(hSrcBuffer).
      end.

      hDataSource:query:query-prepare (cQryString) no-error.
      if Util:IsError() then
      do:
         DetachDataSource(pcTable).
         ThrowError(100, 'msg_err_data_invalid_query':u, pcTable, cQryString).
         undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
      end.
      AddWhereClause(pcTable, pcBuffer, pcWhere).
      AddJoinClause(pcTable, pcBuffer, joinClause).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method protected logical AttachDataSource ():
      ThrowDebug(100, 'msg_err_data_no_source':u, ?, ?).
      return true.
   end method.

   method protected void DetachDataSource ():
      define variable numBuf as integer   no-undo.

      if valid-handle(datasetHandle) then
      do numBuf = 1 to datasetHandle:num-buffers:
         DetachDataSource(datasetHandle:get-buffer-handle(numBuf):table).
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method protected void DetachDataSource (pcTable as character):
      define variable hTable as handle no-undo.

      if valid-handle(datasetHandle) then
         hTable = datasetHandle:get-buffer-handle(pcTable):table-handle.

      if valid-handle(hTable) then
         DetachDataSource(table-handle hTable by-reference).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
      finally:
         if valid-handle(hTable) then
            delete object hTable.
      end finally.
   end method.

   method private void DetachDataSource (table-handle hTable):
      define variable hDataSource as handle    no-undo.
      define variable hBuffer     as handle    no-undo   extent 16.
      define variable hQuery      as handle    no-undo.
      define variable numBuf      as integer   no-undo.

      if not valid-handle(hTable) or not valid-handle(hTable:default-buffer-handle:data-source) then
         return.

      assign
         hDataSource = hTable:default-buffer-handle:data-source
         hQuery      = hDataSource:query .

      hTable:default-buffer-handle:detach-data-source().

      do numBuf = 1 to hDataSource:num-source-buffers:
         hBuffer[numBuf] = hDataSource:get-source-buffer(numBuf).
      end.

      /* delete attached query if dynamicaly created */
      if valid-handle(hQuery) and
         hQuery:dynamic then
         delete object hQuery.

      /* delete the source buffers if dinamicaly created */
      do numBuf = 1 to 16:
         if valid-handle(hBuffer[numBuf]) and hBuffer[numBuf]:dynamic then
            delete object hBuffer[numBuf].
      end.

      if valid-handle(hDataSource) then
         delete object hDataSource.

      delete object hTable.

      finally:
         if valid-handle(hDataSource) then
            delete object hDataSource .
      end finally.
   end method.

   method public character LookupKeyValue
      (input tableName as character, input keyField    as character,
      input keyValue  as character, input lookupField as character):

      if valid-object(Request) then
         return LookupKeyValue(tableName, keyField, keyValue, lookupField, Request:GetBlobVariable({&req-var-lookup-filter})).
      return LookupKeyValue(tableName, keyField, keyValue, lookupField, ?).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public character LookupKeyValue
      (input tableName as character, input keyField    as character,
      input keyValue  as character, input lookupField as character,
      input filterXml as memptr):
      define variable hBuffer       as handle    no-undo.
      define variable hTT           as handle    no-undo.
      define variable hFldKey       as handle    no-undo.
      define variable hFldDesc      as handle    no-undo.
      define variable fldName       as character no-undo.
      define variable cDescription  as character no-undo.

      if Util:IsEmpty(keyField) or Util:IsEmpty(lookupField) then
         return '':u.

      if Util:IsEmpty(tableName) then
         tableName = datasetHandle:get-buffer-handle(1):table.

      assign
         hTT     = datasetHandle:get-buffer-handle(tableName):table-handle
         hBuffer = datasetHandle:get-buffer-handle(tableName):data-source:get-source-buffer(1)
         hFldKey = hBuffer:buffer-field(keyField) .

      if not valid-handle(hFldKey) then
         assign
            fldName  = getPhysicalFieldName(table-handle hTT by-reference, keyField)
            keyField = entry(2, fldName, '.':u)
            hBuffer  = hTT:default-buffer-handle:data-source:query:get-buffer-handle(entry(1, fldName, '.':u))
            hFldKey  = hBuffer:buffer-field(keyField).

      if not valid-handle (hFldKey) or hFldKey:key eq false or Util:IsEmpty(fldName) then
         return '':u.

      hFldDesc = hBuffer:buffer-field(lookupField) .
      if not valid-handle(hFldKey) then
         assign
            lookupField = entry(2, getPhysicalFieldName(table-handle hTT by-reference, lookupField), '.':u)
            hFldDesc    = hBuffer:buffer-field(lookupField).

      if not valid-handle (hFldDesc) then
         return '':u.

      hBuffer:find-first (substitute ('where &1 = &2 &3',
         keyField, quoter (keyValue),
         getLookupFilter(tableName, hBuffer:name, filterXml)
         ), no-lock).
      if hBuffer:available then
         cDescription = string(hFldDesc:buffer-value).
      hBuffer:buffer-release().

      return cDescription.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
      finally:
         if valid-handle(hTT) then
            delete object hTT.
      end finally.
   end method.

   method public com.quarix.base.Collection AutocompleteKey
      (input tableName   as character, input keyField as character, input keyValue as character):

      if valid-object(Request) then
         return AutocompleteKey(tableName, keyField, keyValue, Request:GetBlobVariable({&req-var-lookup-filter})).
      else
         return AutocompleteKey(tableName, keyField, keyValue, ?).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public com.quarix.base.Collection AutocompleteKey
      (input tableName   as character, input keyField as character, input keyValue as character, input filterXml as memptr):
      define variable hBuffer  as handle                     no-undo.
      define variable hTT      as handle                     no-undo.
      define variable hFld     as handle                     no-undo.
      define variable hQry     as handle                     no-undo.
      define variable fldName  as character                  no-undo.
      define variable numRec   as integer                    no-undo initial {&req-var-lookup-max-rec}.
      define variable arrData  as com.quarix.base.Collection no-undo.

      if Util:IsEmpty(tableName) then
         tableName = datasetHandle:get-buffer-handle(1):table.

      assign
         hTT     = datasetHandle:get-buffer-handle(tableName):table-handle
         hBuffer = datasetHandle:get-buffer-handle(tableName):data-source:get-source-buffer(1)
         hFld    = hBuffer:buffer-field(keyField).

      if not valid-handle(hFld) then
         assign
            fldName  = getPhysicalFieldName(table-handle hTT by-reference, keyField)
            keyField = entry(2, fldName, '.':u)
            hBuffer  = hTT:default-buffer-handle:data-source:query:get-buffer-handle(entry(1, fldName, '.':u))
            hFld     = hBuffer:buffer-field(keyField).

      if not valid-handle (hFld) or hFld:data-type ne 'character':u or hFld:key eq false or Util:IsEmpty(fldName) then
         return ?.

      create query hQry.
      hQry:set-buffers(hBuffer).
      hQry:forward-only = true.

      hQry:query-prepare(substitute('for each &1 where &2 begins &3 &4',
         hBuffer:name, keyField,
         quoter(keyValue),
         getLookupFilter(tableName, hBuffer:name, filterXml))).
      hQry:query-open().
      hQry:get-first(no-lock).
      if hBuffer:available then do:
         arrData = new com.quarix.base.Collection().
         do while not hQry:query-off-end and numRec gt 0
            on error undo, throw:
            arrData:Push(string(hFld:buffer-value)).
            numRec = numRec - 1.
            hQry:get-next(no-lock).
         end.
      end.
      return arrData.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
      finally:
         if valid-handle(hQry) then do:
            hQry:query-close().
            delete object hQry.
         end.
         if valid-handle(hTT) then
            delete object hTT.
      end finally.
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
   end method.

   method protected final logical LoadPersistence
      (input-output dataset-handle datasetHdl, variableName as character, includeData as logical, instanceGuid as character):

      define variable mpXmlData   as memptr    no-undo.
      define variable datasetXml  as handle    no-undo.
      define variable retVal      as logical   no-undo.
      define variable sectionName as character no-undo.

      /* for already existent dataset we only load data */
      if valid-handle(datasetHdl) and includeData eq false then return true.

      if valid-object(ContextManager) then
      do:
         sectionName = substitute('&1&2':u, ID, Util:Nvl(instanceGuid, '':u)).

         if valid-handle(datasetHdl) then
         do:
            if ContextManager:GetValue(sectionName, substitute('&1-data':u, variableName), output mpXmlData) then
            do:
               retVal = datasetHdl:read-xml('memptr':u, mpXmlData, 'empty':u, ?, false).
               if retVal eq false then
                  ThrowDebug(500, 'msg_err_load_persist_xml':u, '':u, '':u).
            end.
         end.
         else
         do:
            if ContextManager:GetValue(sectionName, variableName, output mpXmlData) then
            do:
               create dataset datasetXml.
               retVal = datasetXml:read-xmlschema('memptr':u, mpXmlData, false).
               if retVal eq true then
               do:
                  datasetHdl = datasetXml.
                  set-size(mpXmlData) = 0.
                  if includeData and ContextManager:GetValue(sectionName, substitute('&1-data':u, variableName), output mpXmlData) then
                  do:
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
/* nu e by-reference (err in myretail)*/
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

      if valid-object(ContextManager) then
      do:
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

   method public void DisplayDebugInfo ():
      define variable iBuf as integer no-undo.
      define variable hBuffer as handle no-undo.
      define variable cQuery as character no-undo.
      define variable cFilter as character no-undo.
      define variable cMapped as character no-undo.
      define buffer bufFilter for ttFilter.

      if valid-handle(datasetHandle) then
      do:
         repeat iBuf = 1 to datasetHandle:num-buffers
            on error undo, throw:
            hBuffer = datasetHandle:get-buffer-handle(iBuf).
            if valid-handle (hBuffer:data-source) and valid-handle (hBuffer:data-source:query) then
               cQuery = substitute ('&1 ~n &2':U, cQuery, hBuffer:data-source:query:prepare-string).
            cMapped = substitute ('&1 ~n &2 &3':U, cMapped, hBuffer:name, hBuffer:data-source-complete-map).
         end.
      end.
      for each bufFilter no-lock
         on error undo, throw:
         cFilter = substitute ('&1 ~n &2 &3 &4':U, cFilter,
            bufFilter.fieldName,
            bufFilter.operName,
            bufFilter.fieldValue).
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method private character getLookupFilter (lookupTable as character, bufferName as character, xmlFilter as memptr):
      define variable qryString as character no-undo.
      define variable fldName   as character no-undo.
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

      if get-size(xmlFilter) gt 0 then
      do:
         create temp-table ttFilter.

         ttFilter:read-xml('memptr':u, xmlFilter, 'empty':u, ?, ?).
         do while ttFilter:has-records
            on error undo, throw:
            ttFilter:default-buffer-handle:find-first().
            /* apply filters only on given temp-table */
            fldName = GetPhysicalFieldName(substitute('&1.&2':u, lookupTable, ttFilter::fld)).
            if fldName ne ? and entry(1, fldName, '.':u) eq bufferName then
               qryString = substitute ('&1 and &2', qryString, getFieldFilterCondition(fldName, ttFilter::op, ttFilter::val)).
            ttFilter:default-buffer-handle:buffer-delete().
         end.
         set-size(xmlFilter) = 0.
      end.

      return trim(qryString).

      finally:
         if valid-handle(ttFilter) then
            delete object ttFilter.
      end finally.
   end method.

   method private character getFieldFilterCondition (fieldName as character, fieldOper as character, fieldVal as character):
      if fieldOper eq 'between':u then
         return substitute('&1 ge &2 and &1 lt &3':u, fieldName,
            quoter(entry(1, fieldVal, chr(1))),
            quoter(entry(2, fieldVal, chr(1)))).
      else
         return substitute('&1 &2 &3':u, fieldName, fieldOper, quoter(fieldVal)).
   end method.

   /* build the complete exclude fields list for a buffer given the exclude and include fields list
      if include fields are given the exclude option is not considered anymore, only one option is valid at a time*/
   method private character getBufferExcludeFields (hBuffer as handle, excludeFields as character, includeFields as character):
      define variable numFld    as integer    no-undo.

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
      define variable numEntry as integer    no-undo.
      define variable fieldMap as character  no-undo.

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
            bufField = entry(num-entries(bufField, '.':u), bufField, '.':u).
         if lookup(ttField, excludeFields) eq 0 then
            newMap = substitute('&1,&2,&3', newMap, ttField, bufField).
      end.
      return trim(newMap, ',':u).
   end method.


   method private logical setReadOnlyFields (input-output roFields as character):
      define variable numEntry    as integer   no-undo.
      define variable fieldName   as character no-undo.
      define variable hdlBuffer   as handle    no-undo.
      define variable hdlField    as handle    no-undo.

      /* set read only fields, dataset handle should have been set by now */
      if not valid-handle(datasetHandle) then
         return false.

      /* if not fully qualified the first buffer is assumed, if not valid field name returns false */
      do numEntry = 1 to num-entries(roFields):
         fieldName = entry(numEntry, roFields).
         if num-entries(fieldName, '.') eq 1 then
            assign
               hdlBuffer = datasetHandle:get-buffer-handle(1)
               hdlField  = hdlBuffer:buffer-field(fieldName).
         else
            assign
               hdlBuffer = datasetHandle:get-buffer-handle(entry(1, fieldName, '.':u))
               hdlField  = hdlBuffer:buffer-field(entry(2, fieldName, '.':u)).
         if not valid-handle(hdlField) then
            return false.
         entry(numEntry, roFields) = substitute('&1.&2':u, hdlBuffer:table, hdlField:name).
      end.
      return true.
   end method.
end class.
