/*------------------------------------------------------------------------
    File        : JsonWriter
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Tudor
    Created     : Wed Jan 07 09:47:32 EET 2009
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

&scoped-define json_section_info        'info':u
&scoped-define json_section_rows        'rows':u
&scoped-define json_section_values      'values':u
&scoped-define json_section_msgs        'messages':u

&scoped-define json_restricted_chars    '~/,~~",~~t,~~r,~~n,~~f,~~b':u
&scoped-define json_replace_chars       '~/,~",t,r,n,f,b':u
&scoped-define json_control_char        chr(92)

using com.quarix.data.parser.JsonWriter.

class com.quarix.data.parser.JsonWriter
   inherits com.quarix.base.BaseObject
   implements com.quarix.base.iSingleton use-widget-pool final:


   define public property ContentSize   as int64   no-undo
      get.
      private set.

   define public property WriteMetaInfo as logical no-undo
      get.
      set.

   define public property Formatted     as logical no-undo
      get.
      set.

   define public property IsOpen        as logical no-undo
      get.
      protected set.

   define public property Strict        as logical no-undo
      get.
      set.

   define private variable firstEntry   as logical  no-undo.
   define private variable firstElement as logical  no-undo.
   define private variable rawValue     as longchar no-undo.

   define private variable saxWriter_   as handle   no-undo.

   define private property saxWriter    as handle no-undo
      get:
         if not valid-handle(saxWriter_) then
         do:
            create sax-writer saxWriter_.
            assign
               saxWriter_:strict   = false
               saxWriter_:fragment = true
               saxWriter_:encoding = 'utf-8':u.
         end.
         return saxWriter_.
      end get.

   &if keyword-all('static':u) ne ? &then
   define private static variable jsonWriter as JsonWriter no-undo.

   constructor private JsonWriter():
      fix-codepage(rawValue) = 'utf-8':u.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end constructor.

   method public static JsonWriter GetInstance():
      if not valid-object(jsonWriter) then
         jsonWriter = new JsonWriter().
      return jsonWriter.
   end method.

   &else
   constructor public JsonWriter():
      do on error undo, throw:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.
      fix-codepage(rawValue) = 'utf-8':u.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end constructor.
   &endif


   destructor public JsonWriter ( ):
      delete object saxWriter_ .

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end destructor.

   method public void CloseStream(  ):
      if IsOpen eq true and valid-handle(saxWriter_) and
         saxWriter_:write-status ne sax-write-idle and
         saxWriter_:write-status ne sax-write-complete then
         saxWriter:end-document().
      IsOpen = false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public logical EndArray( input elemName as character ):
      return IsOpen and saxWriter_:write-fragment('~]':u).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical EndElement( input elemName as character ):
      return IsOpen and saxWriter_:write-fragment('~}':u).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical OpenStream(  ):
      if IsOpen eq true then
         CloseStream().

      if valid-handle(saxWriter) and saxWriter_:start-document() then
         assign
            IsOpen       = true
            firstElement = true.
      return IsOpen.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetDestination( input fileOut as character ):
      if IsOpen then
         CloseStream().
      return valid-handle(saxWriter) and saxWriter_:set-output-destination('file':u, fileOut).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical SetDestination( input mpOut as memptr ):
      if IsOpen then
         CloseStream().
      return valid-handle(saxWriter) and saxWriter_:set-output-destination('memptr':u, mpOut).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical StartArray( input elemName as character ):

      if not IsOpen or not addSeparator ()then
         return false.

      if ((WriteMetaInfo eq false or Util:IsEmpty(elemName)) and saxWriter_:write-fragment('~[':u)) or
         saxWriter_:write-fragment(substitute('&1: ~[':u, elemName)) then
      do:
         firstEntry = true.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical StartElement( input elemName as character ):
      if not IsOpen or not addSeparator () then
         return false.
      if ((WriteMetaInfo eq false or Util:IsEmpty(elemName)) and saxWriter_:write-fragment('~{':u)) or
         saxWriter_:write-fragment(substitute('&1: ~{':u, elemName)) then
      do:
         firstEntry = true.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical StartElement( input elemName as character, input elemAttr as handle ):
      return StartElement(elemName) and WriteAttributes(elemAttr).
   end method.

   method public logical WriteElement( input elemName as character, input elemValue as character ):
      return WriteValue (elemName, elemValue).
   end method.

   method public logical WriteElement( input elemName as character, input elemValue as character, input elemAttr as handle ):

      if not valid-handle(elemAttr) or elemAttr:type ne 'sax-attributes':u then
         return WriteElement(elemName, elemValue).

      return StartElement(elemName) and
         (Util:IsEmpty(elemValue) or WriteValue ('value', elemValue)) and
         WriteAttributes(elemAttr) and
         EndElement(elemName).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical WriteValue (elemName as character, elemValue as character):
      if not IsOpen or not addSeparator () then
         return false.

      if WriteMetaInfo eq false or Util:IsEmpty(elemName) then
         return saxWriter_:write-fragment(quote(elemValue)).

      return saxWriter_:write-fragment(substitute('&1:&2', quote(elemName), quote(elemValue))).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical WriteValue (elemName as character, elemValue as longchar):
      if not IsOpen or not addSeparator () then
         return false.

      if WriteMetaInfo eq false or Util:IsEmpty(elemName) then
         return saxWriter_:write-fragment(elemValue).
      return saxWriter_:write-fragment(substitute('&1:&2', quote(elemName), elemValue)).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method logical WriteAttributes( input elemAttr as handle ):
      define variable numAttr as integer no-undo.

      if not IsOpen or not valid-handle(elemAttr) or elemAttr:type ne 'sax-attributes':u then
         return false.

      do numAttr = 1 to elemAttr:num-items:
         if not WriteValue(elemAttr:get-localname-by-index(numAttr), elemAttr:get-value-by-index(numAttr)) then
            return false.
      end.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method private logical addSeparator ():
      if not firstElement and not firstEntry and not saxWriter_:write-fragment(',':u) then
         return false.
      assign
         firstEntry   = false
         firstElement = false.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical Serialize (input arrObj  as com.quarix.base.Array):

      define variable stHead as character no-undo.
      define variable stKey  as character no-undo.
      define variable stVal  as character no-undo.
      define variable lVal   as logical   no-undo.

      if not valid-object(arrObj) then
         return false.

      do while arrObj:GetNext()
         on error undo, throw:

         if arrObj:DataType eq 'logical':u then
         do:
            arrObj:GetValue(output lVal).
            stVal = string(lVal, 'true/false':u).
         end.
         else
            arrObj:GetValue(output stVal).

         stKey = arrObj:GetKey().
         Out(substitute('&1&2:&3':u,
            if arrObj:IndexPosition eq 1 then '' else ',':u,
            Quote(stKey),
            if arrObj:DataType eq 'character':u then Quote(stVal) else stVal)).
      end.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   /* Serialize a temp-table data - using column names with override option */
   method public logical Serialize
      (input table-handle   hTT,
      input includeLabels  as logical,
      input fieldMap       as character,
      input fieldList      as character):

      define variable retVal as logical no-undo.


      retVal = Serialize (table-handle hTT by-reference, includeLabels, fieldMap, fieldList, ?, false).
      return retVal.

   end method.

   method public logical Serialize
      (input table-handle   hTT,
      input includeLabels  as logical,
      input fieldMap       as character,
      input fieldList      as character,
      input queryString    as character):

      define variable retVal as logical no-undo.


      retVal = Serialize (table-handle hTT by-reference, includeLabels, fieldMap, fieldList, queryString, false).
      return retVal.
   end method.

   method public logical Serialize
      (input table-handle   hTT,
      input includeLabels  as logical,
      input fieldMap       as character,
      input fieldList      as character,
      input queryString    as character,
      input standardRows   as logical):

      &if keyword-all('write-json') ne ? &then
      define variable lcJson    as longchar no-undo.
      define variable httBuf    as handle   no-undo.
      define variable hQry      as handle   no-undo.
      define variable httRowid  as handle   no-undo.
      define variable hFld      as handle   no-undo extent.
      define variable hRowId    as handle   no-undo.
      define variable hRowState as handle   no-undo.
      define variable hSortOrd  as handle   no-undo.
      define variable iNumFld   as integer  no-undo.

      if not valid-handle(hTT) then
         return false.

      httBuf = hTT:default-buffer-handle.

      if not valid-handle(httBuf) then
        return false.

      hRowId = httBuf:buffer-field ('rowid') no-error.
      hRowState = httBuf:buffer-field ('rowstate') no-error.

      create query hQry.
      hQry:set-buffers(httBuf).
      hQry:forward-only = true.

      if not Util:IsEmpty(queryString) then
         hQry:query-prepare(substitute('for each &1 &2':u, httBuf:name, queryString)) no-error.

      if hQry:prepare-string eq ? then
      do:
        assign hSortOrd = httBuf:buffer-field('SortOrder':U) no-error.

        if valid-handle(hSortOrd)
        then hQry:query-prepare(substitute('for each &1 no-lock by &1.SortOrder':U, httBuf:name)) no-error.

        /* try to see if there is a display index defined, in cases where display order is different from PK */
        if hQry:prepare-string eq ?
        then hQry:query-prepare(substitute('for each &1 use-index _display':u, httBuf:name)) no-error.

         if hQry:prepare-string eq ?
         then hQry:query-prepare(substitute('for each &1':u, httBuf:name)) no-error.

      end. /* if hQry:prepare-string eq ? */

      hQry:query-open() no-error.

      httRowid = handle(hTT:private-data) no-error.

      do while hQry:get-next(no-lock):
             if valid-handle(hRowId) then
             hRowId:buffer-value () = getTableRowid(hTT, httRowid).

             if valid-handle(hRowState) then
             hRowState:buffer-value () = if httBuf:row-state eq ? then 0 else httBuf:row-state.
      end.

      extent(hFld) = httBuf:num-fields.

      do iNumFld = 1 to httBuf:num-fields:
          hFld[iNumFld] = httBuf:buffer-field (iNumFld).
          hFld[iNumFld]:serialize-name = ''.
      end.

      hTT:write-json ('longchar',lcJson,true).

      lcJson = replace(lcJson, '~n    "":', '').
      lcJson = replace(lcJson, '~n  ~{','[').
      lcJson = replace(lcJson, '~n  ~}',']').
      lcJson = substring(lcJson,length(substitute('~{"&1": ~[',httBuf:name)) + 1).
      lcJson = substring(lcJson,1,r-index(lcJson,'~]~}') - 1).

      Out(lcJson).
      &else

      define variable httBuf    as handle    no-undo.
      define variable httRowid  as handle    no-undo.
      define variable hQry      as handle    no-undo.
      define variable hFld      as handle    no-undo.
      define variable hRowId    as handle    no-undo.
      define variable hRowState as handle    no-undo.
      define variable lFld      as integer   no-undo.
      define variable lCnt      as integer   no-undo.
      define variable numFld    as integer   no-undo.
      define variable numRow    as integer   no-undo.
      define variable lExt      as integer   no-undo.
      define variable recDel    as character no-undo extent 2.

      if not valid-handle(hTT) then
         return false.

      httBuf = hTT:default-buffer-handle.

      create query hQry.
      hQry:set-buffers(httBuf).
      hQry:forward-only = true.

      if not Util:IsEmpty(queryString) then
         hQry:query-prepare(substitute('for each &1 &2':u, httBuf:name, queryString)) no-error.

      if hQry:prepare-string eq ? then
      do:
        assign hFld = httBuf:buffer-field('SortOrder':U) no-error.

        if valid-handle(hFld)
        then hQry:query-prepare(substitute('for each &1 no-lock by &1.SortOrder':U, httBuf:name)) no-error.

        /* try to see if there is a display index defined, in cases where display order is different from PK */
        if hQry:prepare-string eq ?
        then hQry:query-prepare(substitute('for each &1 use-index _display':u, httBuf:name)) no-error.

         if hQry:prepare-string eq ?
         then hQry:query-prepare(substitute('for each &1':u, httBuf:name)) no-error.

      end. /* if hQry:prepare-string eq ? */

      hQry:query-open() no-error.

      if includeLabels eq true then assign
            recDel[1] = '~{':u
            recDel[2] = '~}':u.
      else assign
            recDel[1] = '[':u
            recDel[2] = ']':u.

      do lFld = 1 to httBuf:num-fields:
         hfld  = httBuf:buffer-field(lFld).
         if includeLabels eq false then
         do:
            hfld:private-data = hfld:name.
         end.
         if lookup(hFld:data-type, 'blob') gt 0 or (fieldList ne '*' and lookup(hFld:name, fieldList) eq 0) then
         do:
            hFld:private-data = ?.
            next.
         end.
         if lookup(hfld:name, fieldMap) gt 0 then
            hFld:private-data = entry(lookup(hfld:name, fieldMap) + 1, fieldMap) no-error.
         if Util:IsEmpty(hFld:private-data) then
            hFld:private-data = Util:Nvl(hFld:label, hFld:name).
         if hFld:name eq 'rowid':u then
            hRowId = hFld.
         if hFld:name eq 'rowstate':u then
            hRowState = hFld.
         numFld = numFld + 1.
      end.

      if standardRows eq true then
      do:
         httRowid = handle(hTT:private-data) no-error.

         if not valid-handle(hRowId) then
            numFld = numFld + 1.
         if not valid-handle(hRowState) then
            numFld = numFld + 1.
      end.

      do while hQry:get-next(no-lock)
         on error undo, throw:
         /* skip before image for 'new' records, odd enaugh to have something like that */
         if valid-handle(hTT:after-table) and
            httBuf:row-state eq row-created then
            next.
         /* serialize the new retrieved record in json format */
         assign
            lFld   = 0
            numRow = numRow + 1.

         Out(substitute('&1&2', if numRow eq 1 then '' else ',':u, recDel[1])).

         if standardRows eq true then
         do:
            if not valid-handle(hRowId) then
            do:
               lFld = lFld + 1.
               Out(substitute('&1&2"&3"':u,
                  if lFld eq 1 then '' else ',':u,
                  if includeLabels then '"rowid":' else '',
                  string(getTableRowid(hTT, httRowid)))).
            end.
            if not valid-handle(hRowState) then
            do:
               lFld = lFld + 1.
               Out(substitute('&1&2"&3"':u,
                  if lFld eq 1 then '' else ',':u,
                  if includeLabels then '"rowstate":' else '',
                  if httBuf:row-state eq ? then 0 else httBuf:row-state)).
            end.
         end.
         do lCnt = 1 to httBuf:num-fields:
            hfld  = httBuf:buffer-field(lCnt).
            if hFld:private-data eq ? then next.
            lFld = lFld + 1.
            /* check if the field is an 'extent' one */
            if hfld:extent eq 0 then
               Out(substitute('&1&2&3&4':u,
                  if lFld eq 1 then '' else ',':u,
                  if includeLabels then substitute('&1:', Quote(hFld:private-data)) else '',
                  GetFieldValue(hFld),
                  if lFld eq numFld then recDel[2] else '')).
            else
            do lExt = 1 to hfld:extent:
               Out(substitute('&1&2&3&4':u,
                  if lFld eq 1 and lExt eq 1 then '' else ',':u,
                  if includeLabels then substitute('&1:', Quote(substitute('&1_&2', hFld:private-data, lExt))) else '',
                  GetFieldValue(hFld, lExt),
                  if lFld eq numFld and lExt eq hfld:extent then recDel[2] else '')).
            end.
         end.
      end.

      return numRow gt 0.
      &endif

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
      finally:
         if valid-handle(hQry) then
         do:
            hQry:query-close().
            delete object hQry.
         end.
      end finally.
   end method.

   method public logical Serialize
      (input table-handle   hTT,
      input includeLabels  as logical,
      input fieldMap       as character):

      define variable retVal as logical no-undo.


      retVal = Serialize (table-handle hTT by-reference, includeLabels, fieldMap, ?).
      return retVal.
   end method.

   /* Serialize a temp-table data - using column names */
   method public logical Serialize
      (input table-handle   hTT,
      input includeLabels  as logical):

      define variable retVal as logical no-undo.


      retVal = Serialize (table-handle hTT by-reference, includeLabels, ?, ?).
      return retVal.
   end method.

   /* Serialize a temp-table - only data */
   method public logical Serialize
      (input table-handle   hTT):

      define variable retVal as logical no-undo.


      retVal = Serialize (table-handle hTT by-reference, false, ?, ?).
      return retVal.
   end method.

   method public logical AddAttribute
      (input objName as character,
      input objBody as character):
      return AddObject(objName, objBody, 'attribute':u).
   end method.

   method public logical AddObject
      (input objName as character,
      input objBody as character):
      return AddObject(objName, objBody, 'object':u).
   end method.

   method public logical AddObject
      (input objName as character,
      input objBody as memptr):
      return AddObject(objName, objBody, false).
   end method.

   method public logical AddArray
      (input objName as character,
      input objBody as character):
      return AddObject(objName, objBody, 'array':u).
   end method.

   method public logical AddArray
      (input objName as character,
      input objBody as memptr):
      return AddObject(objName, objBody, true).
   end method.

   method private logical AddObject
      (input objName    as character,
      input objBody    as character,
      input objType    as character):

      if not Util:IsEmpty(objName) then
         objName = substitute('&1:', Quote(objName)).
      else
      do:
         if objType eq 'attribute' then
            return false.
         objName = ''.
      end.

      case objType:
         when 'array':u then
            return Out(substitute('&1[&2]':u, objName, objBody)).
         when 'object':u then
            return Out(substitute('&1~{&2~}':u, objName, objBody)).
         when 'attribute':u then
            return Out(substitute('&1&2':u, objName, objBody)).
      end case.

   end method.

   method private logical AddObject
      (input objName    as character,
      input objBody    as memptr,
      input jsonArray  as logical):

      if not Util:IsEmpty(objName) then
         objName = substitute('&1:', Quote(objName)).
      else
         objName = ''.

      if jsonArray then
      do:
         if Out(substitute('&1[':u, objName)) and
            Out(objBody) and
            Out(']':u) then
            return true.
      end.
      else
      do:
         if Out(substitute('&1~{':u, objName)) and
            Out(objBody) and
            Out('~}':u) then
            return true.
      end.
   end method.

   method public character Quote
      (stString as character):
      if stString eq ? then return '~"?~"':u.

      return substitute('~"&1~"':u, EscapeUnsafeChars(stString)).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public character EscapeUnsafeChars
      (stString as character):
      define variable lPos  as integer   no-undo.
      define variable stRep as character no-undo.

      if Util:IsEmpty(stString) then
         return stString.

      stString = replace(stString, {&json_control_char}, {&json_control_char} + {&json_control_char}).
      do lPos = 1 to num-entries({&json_restricted_chars}):
         stString = replace(stString,
            entry(lPos, {&json_restricted_chars}),
            substitute('&1&2':u, {&json_control_char}, entry(lPos, {&json_replace_chars}))) .
      end.
      return stString.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method public logical Out (input  mpOut as memptr ) :
      define variable lcOut as longchar no-undo.

      if not IsOpen then
         return false.

      fix-codepage(lcOut) = 'utf-8':u.
      copy-lob from mpOut to lcOut.
      return Out(lcOut).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical Out (input  stOut as character ) :
      if not IsOpen then
         return false.

      if Util:IsEmpty(stOut) then
         return true.

      if not saxWriter_:write-fragment(stOut) then
         return false.

      ContentSize = ContentSize + length(stOut, 'raw':u).
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public logical Out (input  stOut as longchar) :
      if not IsOpen then
         return false.

      if Util:IsEmpty(stOut) then
         return true.

      if not saxWriter_:write-fragment(stOut) then
         return false.

      ContentSize = ContentSize + length(stOut, 'raw':u).
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public void Reset ():
   end method.

   method public void Purge ():
      ContentSize = 0.
      CloseStream().
      OpenStream().

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method private character GetFieldValue (input hFld as handle):
      return GetFieldValue(hFld, 0).
   end method.

   method private character GetFieldValue (input hFld as handle, numExtent as integer):
      define variable fieldValue as character no-undo.

      if numExtent eq 0 then
      do:
         if hFld:buffer-value eq ? then
            return '"?"':u.
         else
         do:
            case hFld:data-type:
               when 'character':u then
                  fieldValue = hFld:buffer-value.
               when 'clob':u then
                  do:
                     copy-lob hFld:buffer-value to rawValue.
                     fieldValue = string(rawValue) no-error.
                  end.
               otherwise
               do:
                  /*roxanam : get the value no matter what*/
                  fieldValue = hFld:string-value no-error.
                  if Util:IsError() then
                     fieldValue = string(hFld:buffer-value) no-error.

               end.
            end case.
         end.
      end.
      else
      do:
         if hFld:buffer-value[numExtent] eq ? then
            return '"?"':u.
         else
         do:
            case hFld:data-type:
               when 'character':u then
                  fieldValue = hFld:buffer-value[numExtent].
               when 'clob':u then
                  do:
                     copy-lob hFld:buffer-value[numExtent] to rawValue.
                     fieldValue = string(rawValue) no-error.
                  end.
               otherwise
               do:
                  fieldValue = hFld:string-value[numExtent] no-error.
                  if Util:IsError() then
                     fieldValue = string(hFld:buffer-value[numExtent]) no-error.
               end.
            end case.
         end.
      end.
      if hFld:data-type ne 'character':u then
         fieldValue = trim(fieldValue).
      if fieldValue eq '':u  or lookup(hFld:data-type, 'character,clob,date,datetime,datetime-tz':u) eq 0 then
         return quoter(fieldValue).
      return Quote(fieldValue).
   end method.

   method private character getTableRowid (input hTT as handle, input hTTRowid as handle):
      define variable ttRowid as rowid     no-undo.
      define variable hBuf    as handle    no-undo.
      define variable ttName  as character no-undo.


      if valid-handle(hTT:after-table) and hTT:default-buffer-handle:row-state ne row-deleted then assign
            ttName  = hTT:after-table:name
            ttRowid = hTT:default-buffer-handle:after-rowid.
      else assign
            ttName  = hTT:name
            ttRowid = hTT:default-buffer-handle:rowid.

      if valid-handle(hTTRowid) then
      do:
         hBuf = hTTRowid:default-buffer-handle.

         hBuf:find-first(substitute('where ttName eq "&1" and ttRowid eq "&2"':u, ttName, string(ttRowid))) no-error.

         if hBuf:available then
            return hBuf:buffer-field('dbRowid':u):buffer-value.

      end. /* if valid-handle(hTTRowid) */

      return string(ttRowid).

   end method.

end class.
