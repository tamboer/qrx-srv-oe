 /*------------------------------------------------------------------------
    File        : DataTableRequest
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marian
    Created     : Tue Jan 27 18:38:22 EET 2009
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

class com.quarix.data.DataRequest
   inherits com.quarix.base.BaseObject
   implements com.quarix.base.iSingleton  use-widget-pool final:

   /*&global-define quarix-standard-xml true*/

   &if defined(quarix-standard-xml) gt 0 &then
      &global-define datasetName   reqMsg
      &global-define tableTT       tblMsg
      &global-define datasetTT     dsMsg
   &else
      &global-define datasetName   datasetMsg
      &global-define tableTT       daoMsg
      &global-define datasetTT     action
   &endif

   {com/quarix/data/dsDataRequest.i &scope=private}

   define private temp-table xsd no-undo
      field objectName  as character
      field xsdFile     as character
      index pkxsd       is primary unique objectName.


   define public property   ObjectType as character no-undo
      get.
      set (objectName as character):
         if ObjectType eq objectName then
            return.
         if setObjectType(objectName) then
            ObjectType = objectName.
      end set.

   define public property   ActionName as character no-undo
      get:
         if ActionName ne '':u then
            return ActionName.
         return GetActionName().
      end get.
      protected set.

   define public property   OutputFormat as character no-undo
      get:
         if OutputFormat ne '':u then
            return OutputFormat.
         return GetOutputFormat().
      end get.
      protected set.

   define private variable xsdFile       as character no-undo initial 'com/quarix/data/xsd/DataRequest.xsd':u.

   &if keyword-all('static':u) ne ? &then
   define private static variable dataRequest as DataRequest no-undo.

   method public static DataRequest GetInstance():
      if not valid-object(dataRequest) then
         dataRequest = new DataRequest().
      return dataRequest.
   end method.
   &endif

   method public logical Parse (mpRequest as memptr):

      define variable cLogDirectory	as character no-undo.
      define variable cLogFileName	as character no-undo.

   	  dataset {&datasetName}:empty-dataset().

      if xsdFile eq ? or
         get-size(mpRequest) eq 0 then
         return false.

      dataset {&datasetName}:read-xml('memptr':u, mpRequest, 'empty':u, xsdFile, false, ?, 'ignore':u).

      if valid-object(ErrorManager) and ErrorManager:GetLogLevel() ge ErrorManager:DebugLevel() then do:

         cLogDirectory = Util:GetLogDirectory().

         if not Util:IsEmpty(cLogDirectory)
         then do:
            cLogFileName = cLogDirectory + '/request.xml':u.

            copy-lob mpRequest to file cLogFileName.
         end.
      end.

      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.

   end method.

   method public void Reset():

      assign
         ObjectType = ?
         ActionName = ?.

   end method.

   method public logical HasRequest (tableName as character):
      define buffer daoMsg for {&tableTT}.

      for each daoMsg where daoMsg.id eq tableName:
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public integer GetBatchSize (tableName as character):
      define buffer daoMsg for {&tableTT}.

      for each daoMsg where daoMsg.id eq tableName:
         return daoMsg.batchSize.
      end.
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public integer GetStartRow (tableName as character):
      define buffer daoMsg for {&tableTT}.

      for each daoMsg where daoMsg.id eq tableName:
         return daoMsg.startRow.
      end.
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public character GetStartRowId (tableName as character):
      define buffer daoMsg for {&tableTT}.

      for each daoMsg where daoMsg.id eq tableName:
         return daoMsg.startRowId.
      end.
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public logical GetPrefetch (tableName as character):
      define buffer daoMsg for {&tableTT}.

      for each daoMsg where daoMsg.id eq tableName:
         return daoMsg.Prefetch.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical GetSkipRow (tableName as character):
      define buffer daoMsg for {&tableTT}.

      for each daoMsg where daoMsg.id eq tableName:
         return daoMsg.skipRow.
      end.
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public logical LoadData (dataset-handle hDs):
   &if defined(quarix-standard-xml) eq 0 &then
      return false.
   &else
   &endif
   	finally:
   		delete object hDs no-error.
   	end finally.
   end method.

	method public logical LoadData (table-handle hTT):
		&if defined(quarix-standard-xml) ne 0 &then
		return false.
		&else
		define buffer row for row.

		define variable newRow as rowid no-undo.

		if not valid-handle(hTT)
		then return false.

		for each row
			where row.daoId eq hTT:name
			on error undo, throw:

			if valid-handle(hTT:before-table)
			then hTT:tracking-changes = false.

			case row.state:

				when row-deleted
				then do:
					if not valid-handle(hTT:before-table)
					then next.

					newRow = saveRowBi (table-handle hTT by-reference, row.id,row.state).

					if newRow eq ?
					then return false.

					hTT:tracking-changes = true.

					if deleteRow (table-handle hTT by-reference, row.id, newRow) eq false
					then return false.

				end. /* when row-deleted */

				when row-modified
				then do:
					if not valid-handle(hTT:before-table)
					then do:
						if saveRowAi (table-handle hTT by-reference, row.id, ?, row.state) eq false
						then return false.
					end.
					else do:
						newRow = saveRowBi (table-handle hTT by-reference, row.id,row.state).

						if newRow eq ?
						then return false.

						hTT:tracking-changes = true.

						if saveRowAi (table-handle hTT by-reference, row.id, newRow, row.state) eq false
						then return false.
					end.
				end. /* when row-modified */

				when row-created
				then do:
					if valid-handle(hTT:before-table)
					then hTT:tracking-changes = true.

					if saveRowAi (table-handle hTT by-reference, row.id, ?, row.state) eq ?
					then return false.
				end. /* when row-created */

				when row-unmodified
				then do:
					newRow = saveRowBi (table-handle hTT by-reference, row.id,row.state).

					if newRow eq ?
					then return false.

					hTT:tracking-changes = false.

					if saveRowAi (table-handle hTT by-reference, row.id, newRow, row.state) eq ?
					then return false.
				end.

			end case. /* case row.state */

		end. /* for each row */

		if valid-handle(hTT:before-table)
		then hTT:tracking-changes = false.

		return true.
		&endif

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

   method public logical GetSort (tableName as character, recIndex as integer, output fieldName as character, output sortOrder as logical):
      define buffer sort    for sort.
      define query  qrySort for sort scrolling.

      open query qrySort for
         each sort where sort.daoId eq tableName.

      if recIndex gt 1 then
         reposition qrySort to row recIndex.
      get next qrySort.
      if available(sort) then do:
         assign fieldName = sort.fld
                sortOrder = sort.rev.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical IsSortDescending (tableName as character, input fieldName as character):
      define buffer sort    for sort.

      for each sort
         where sort.daoId eq tableName
           and sort.fld   eq fieldName:
         return sort.rev.
      end.

      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public logical GetFilter (tableName as character, recIndex as integer,
      output fieldName as character, output operName as character, output filterVal as character):
      define buffer filter    for filter.
      define query  qryFilter for filter scrolling.

      open query qryFilter for
         each filter where filter.daoId eq tableName and filter.val ne '?':u.

      if recIndex gt 1 then
         reposition qryFilter to row recIndex.
      get next qryFilter.
      if available(filter) then do:
         assign fieldName = filter.fld
                operName  = filter.op
                filterVal = filter.val.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return  false.
      end catch.
   end method.

   method public logical GetSearch (tableName as character, recIndex as integer,
      output fieldName as character, output filterVal as character):
      define buffer srch    for srch.
      define query  qrySrch for srch scrolling.

      open query qrySrch for
         each srch where srch.daoId eq tableName and srch.val ne '?':u.

      if recIndex gt 1 then
         reposition qrySrch to row recIndex.
      get next qrySrch.
      if available(srch) then do:
         assign fieldName = srch.fld
                filterVal = srch.val.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical IsSearchRequest (tableName as character):
      define buffer srch    for srch.

      return can-find(first srch where srch.daoId eq tableName).
   end method.

   method public character GetActionName ():

      case ObjectType:
         when 'DataCollection':u then do:
            for each {&datasetTT}:
               return {&datasetTT}.command.
            end.
         end.
         when 'DataTable':u then do:
            &if defined(quarix-standard-xml) eq 0 &then
            for each {&tableTT}:
               return {&tableTT}.command.
            end.
            &endif
         end.

      end case.
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.


   method public character GetOutputFormat ():

      case ObjectType:
         when 'DataCollection':u then do:
            for each {&datasetTT}:
               return {&datasetTT}.responseFormat.
            end.
         end.

         when 'DataTable':u then do:
            &if defined(quarix-standard-xml) eq 0 &then
            for each {&tableTT}:
               return {&tableTT}.responseFormat.
            end.
            &endif
         end.
      end case.
      return ?.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method private logical setObjectType (objectName as character):
      define buffer xsd for xsd.

      if Util:IsEmpty(objectName) then
         return true.
      for each xsd where xsd.objectName eq objectName
         on error undo, throw:
         assign
            xsdFile    = xsd.xsdFile
            ActionName = ''.
         return true.
      end.
      file-info:file-name = substitute('com/quarix/data/xsd/&1.xsd':u, objectName).
      if file-info:file-type ne ? then do:
         create xsd.
         assign
            xsd.objectName = objectName
            xsd.xsdFile    = file-info:full-pathname
            xsdFile        = xsd.xsdFile
            ActionName     = ''.
         return true.
      end.
      ThrowError(100, 'Data request XML definition file not found.':u, '':u, '':u).
      return false.

   end method.

   &if defined(quarix-standard-xml) eq 0 &then
   method private rowid saveRowBi (table-handle hTT, id as character, rowState as integer):
      define variable hBuf     as handle    no-undo.
      define variable hFld     as handle    no-undo.
      define variable fldVal   as longchar  no-undo.
      define variable fldExt   as integer   no-undo.
      define variable bufRowid as rowid     no-undo. /*to store buffer rowid*/

      define buffer rowimage for rowimage.
      define buffer fld      for fld.

      #transaction:
      do transaction
         on error undo, throw:

         for each rowimage
            where rowimage.daoId eq hTT:name
              and rowimage.id    eq id
              and (rowimage.type  eq 'bi':u or
                   rowState eq row-unmodified )
                      on error undo, throw:

            hBuf = hTT:default-buffer-handle.

            hBuf:buffer-create().

            for each fld
               where fld.daoId eq rowimage.daoId
                 and fld.id    eq rowimage.id
                 and fld.type  eq rowimage.type
                 and fld.name  ne 'rowid':u
                 and fld.name  ne 'rowstate':u
                    on error undo, throw:
               assign
                  fldExt = 0
                  hFld   = hBuf:buffer-field(fld.name) no-error.
               /* might be extent field */
               if not valid-handle(hFld) and index(fld.name, '_':u) gt 0 then
                  assign
                     hFld   = hBuf:buffer-field(substring(fld.name, 1, r-index(fld.name, '_':u) - 1))
                     fldExt = integer(substring(fld.name, r-index(fld.name, '_':u) + 1, -1)) no-error.
               if not valid-handle(hFld) then next.
               fldVal = fld.val.
               if not setFieldValue (true, hFld, fldExt, fldVal) then do:
                  hBuf:buffer-delete().
                  return ?.
               end.
            end.

            hBuf:buffer-validate() no-error.

            if Util:IsError() then do:
               hBuf:buffer-delete().
               undo, throw new Progress.Lang.AppError(error-status:get-message (1), error-status:get-number(1)).
            end.

            if not setRowId (table-handle hTT by-reference, hBuf:rowid, id)
            then undo #transaction, leave #transaction.

            bufRowid = hBuf:rowid.

            return bufRowid.

         end. /* for each rowimage */

      end. /* do transaction */

      return ?.

   end method.

   method private logical deleteRow (table-handle hTT, id as character, newId as rowid):
      define variable hBuf as handle no-undo.

      hBuf = hTT:default-buffer-handle .

      hBuf:find-by-rowid(newId) .

      if hBuf:available
      then
         do transaction
            on error undo, throw:

            hBuf:buffer-delete() .
           /* update rowid pointer for deleted records to use before-image rowid */
           if not setRowId (table-handle hTT by-reference, hBuf:before-rowid, id)
           then undo, leave.

           return true.
        end.

      return false.

   end method.

   method private logical saveRowAi (table-handle hTT, id as character, newId as rowid, rowState as integer):
      define variable hBuf      as handle    no-undo.
      define variable hFld      as handle    no-undo.
      define variable fldVal    as longchar  no-undo.
      define variable fldExt    as integer   no-undo.

      define buffer rowimage for rowimage.
      define buffer fld      for fld.

      hBuf = hTT:default-buffer-handle.

      do transaction on error undo, throw:
         if rowState ne row-modified and
            rowState ne row-unmodified then
            hBuf:buffer-create().

         if newId ne ? then do:
            hBuf:find-by-rowid(newId) .
            if not hBuf:available then do:
               return false.
            end.
         end.

         for each rowimage
            where rowimage.daoId eq hTT:name
              and rowimage.id    eq id
              and rowimage.type  eq 'ai':u
                 on error undo, throw:
            for each fld
               where fld.daoId eq rowimage.daoId
                 and fld.id    eq rowimage.id
                 and fld.type  eq rowimage.type
                 and fld.name  ne 'rowid':u
                 and fld.name  ne 'rowstate':u
                    on error undo, throw:

               assign
                  fldExt = 0
                  hFld   = hBuf:buffer-field(fld.name) no-error.
               /* might be extent field */
               if not valid-handle(hFld) and index(fld.name, '_':u) gt 0 then
                  assign
                     hFld   = hBuf:buffer-field(substring(fld.name, 1, r-index(fld.name, '_':u) - 1))
                     fldExt = integer(substring(fld.name, r-index(fld.name, '_':u) + 1, -1)) no-error.

               if not valid-handle(hFld) then next.

               fldVal = fld.val.
               if not setFieldValue (false, hFld, fldExt, fldVal) then do:
                  hBuf:buffer-delete() .
                  return false.
               end.
            end.
            hBuf:buffer-validate() no-error.
            if Util:IsError() then do:
               if error-status:get-number(1) eq 132 then
                  ThrowError(103, 'msg_err_data_dupe':u, hBuf:name, entry(1, hBuf:keys)).
               else
                  ThrowError(103, 'msg_err_data_load':u, hBuf:name, ?).
               hBuf:buffer-delete() .
               undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
            end.
         end.
      end.

      return true.

   end method.

   method private logical setFieldValue (beforeImage as logical, hFld as handle, fldExt as integer, fldVal as longchar):
      define variable boolVal as logical no-undo.

      /* empty string value received in xml from client */
      if fldVal eq ? then do:
         fldVal = ''.
         /* for before image we don't need to change the field value if
            initial is already empty string so we skip the assignment */
         if beforeImage and hFld:initial eq fldVal then
            return true.
      end.
      /* if the client send '?' in xml (probably because we've sent that
         from the back-end we change it to null value instead */
      if compare(fldVal, 'eq':u, '?':u, 'raw':u) then do:
         if fldExt eq 0 then
            hFld:buffer-value = ?.
         else
            hFld:buffer-value[fldExt] = ?.
         return true.
      end.
      case hFld:data-type:
         /* character data-type, just assign the input string  */
         when 'character':u or when 'clob':u then do:
            if fldExt eq 0 then
               hFld:buffer-value = fldVal .
            else
               hFld:buffer-value[fldExt] = fldVal .
         end.
         /* date data-type, use session format for conversion  */
         when 'date':u then do:
            if fldExt eq 0 then
               hFld:buffer-value = date(string(fldVal)) .
            else
               hFld:buffer-value[fldExt] = date(string(fldVal)) .
         end.
         /* datetime data-type, use session format for conversion  */
         when 'datetime':u then do:
            if fldExt eq 0 then
               hFld:buffer-value = datetime(string(fldVal)) .
            else
               hFld:buffer-value[fldExt] = datetime(string(fldVal)) .
         end.
         /* datetime data-type, use session format for conversion  */
         when 'datetime-tz':u then do:
            if fldExt eq 0 then
               hFld:buffer-value = datetime-tz(string(fldVal)) .
            else
               hFld:buffer-value[fldExt] = datetime-tz(string(fldVal)) .
         end.
         /* integer data-type, convert the input string to int */
         when 'integer':u or when 'recid':u then do:
            if fldExt eq 0 then
               hFld:buffer-value = integer(string(fldVal)) .
            else
               hFld:buffer-value[fldExt] = integer(string(fldVal)) .
         end.
         /* integer data-type, convert the input string to int */
         when 'rowid':u then do:
            if fldExt eq 0 then
               hFld:buffer-value = to-rowid(string(fldVal)) .
            else
               hFld:buffer-value[fldExt] = to-rowid(string(fldVal)) .
         end.
         /* integer data-type, convert the input string to int */
         when 'int64':u then do:
            if fldExt eq 0 then
               hFld:buffer-value = int64(string(fldVal)) .
            else
               hFld:buffer-value[fldExt] = int64(string(fldVal)) .
         end.
         /* decimal data-type, use session format for conversion  */
         when 'decimal':u then do:
            if fldExt eq 0 then
               hFld:buffer-value = decimal(string(fldVal)) .
            else
               hFld:buffer-value[fldExt] = decimal(string(fldVal)) .
         end.
         /* logical data-type, convert the input string to logical */
         when 'logical':u then do:
            boolVal = string(fldVal) eq entry(1, hFld:format, '~/':u) .
            if fldExt eq 0 then
               hFld:buffer-value = boolVal .
            else
               hFld:buffer-value[fldExt] = boolVal .
         end.
      end case.
      return true.
   end method.

	method private logical setRowid (table-handle hTT, ttRowId as rowid, cliRowId as character):

		define variable hTTRowid	as handle		no-undo.
		define variable hBufRowid	as handle		no-undo.
		define variable cQuery		as character	no-undo.
		define variable hQuery		as handle		no-undo.
		define variable lPrepareOk	as logical		no-undo.
		define variable iNumRec		as integer		no-undo.

		if not valid-handle(hTT)
		then return false.

		assign hTTRowid = handle(hTT:private-data) no-error.

		if not valid-handle(hTTRowid)
		then return false.

		hBufRowid = hTTRowid:default-buffer-handle.

		if Util:IsEmpty(cliRowId) or
			not valid-handle(hBufRowid)
		then return false.

		cQuery = substitute('where ttName eq "&1" and dbRowId eq "&2"':u, hTT:name, cliRowId).

		/* see if client rowid was already set */
		hBufRowid:find-first(cQuery) no-error.

		if hBufRowid:available
		then do:
			hTTRowid::ttRowId = string(ttRowId).
			return true.
		end.

		create query hQuery.

		hQuery:set-buffers(hBufRowid).

		cQuery = substitute('for each &1 no-lock by &1.NumRec descending':U, hBufRowid:name).

		lPrepareOk = hQuery:query-prepare(cQuery) no-error.

		if Util:IsError() or
			not lPrepareOk
		then do:
			delete object hQuery no-error.

			return false.
		end.

		iNumRec = 0.

		hQuery:query-open().

		hQuery:get-first().

		if not hQuery:query-off-end
		then iNumRec = hBufRowid:buffer-field ('NumRec':U):buffer-value.

		hQuery:query-close().

		delete object hQuery no-error.

		iNumRec = iNumRec + 1.

		hBufRowid:buffer-release ().

		hBufRowid:buffer-create().

		assign
			hTTRowid::NumRec	= iNumRec
			hTTRowid::ttName	= hTT:name
			hTTRowid::ttRowId	= string(ttRowId)
			hTTRowid::dbRowId	= cliRowId.

		return true.

		catch appError as Progress.Lang.Error :

			delete object hQuery no-error.

            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

   end method.
   &endif

   method public logical copyRequest (sourceId as character, targetId as character):

      define buffer srcDaoMsg for {&tableTT}.
      define buffer trgDaoMsg for {&tableTT}.
      define buffer srcFilter for Filter.
      define buffer trgFilter for Filter.
      define buffer srcSort for Sort.
      define buffer trgSort for Sort.
      define buffer srcSrch for Srch.
      define buffer trgSrch for Srch.

      if Util:IsEmpty(sourceId) or
         Util:IsEmpty(targetId)
      then return false.

      if sourceId = targetId
      then return true.

      do transaction on error undo, return false:

         for each srcDaoMsg where srcDaoMsg.id eq sourceId:
             create trgDaoMsg.
             assign trgDaoMsg.id = targetId.
             buffer-copy srcDaoMsg except id to trgDaoMsg.
         end.

         for each srcFilter where srcFilter.daoId eq sourceId:
             create trgFilter.
             assign trgFilter.daoId = targetId.
             buffer-copy srcFilter except daoId to trgFilter.
         end.

         for each srcSort where srcSort.daoId eq sourceId:
             create trgSort.
             assign trgSort.daoId = targetId.
             buffer-copy srcSort except daoId to trgSort.
         end.

         for each srcSrch where srcSrch.daoId eq sourceId:
             create trgSrch.
             assign trgSrch.daoId = targetId.
             buffer-copy srcSrch except daoId to trgSrch.
         end.

      end. /* do transaction on error undo, leave */

      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.

   end method.

   method public void DumpToXml():
      Util:LogDataset(dataset {&datasetName} by-reference).
   end method.

   method public void DumpToXml(input pcName as character):
      Util:LogDataset(dataset {&datasetName} by-reference, pcName).
   end method.

end class.
