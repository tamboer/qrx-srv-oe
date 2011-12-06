
 /*------------------------------------------------------------------------
    File        : Collection
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Thu Dec 11 09:02:41 EET 2008
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

class com.quarix.base.Collection use-widget-pool:

   define public property DataType      as character no-undo
   get. protected set.

   define public property IndexPosition as integer   no-undo
   get. protected set.

   define public property NumElements   as integer   no-undo
   get. protected set.

   define protected variable hTT       as handle     no-undo.
   define protected variable hBuf      as handle     no-undo.
   define protected variable hFldIdx   as handle     no-undo.
   define protected variable hFldVal   as handle     no-undo.

   constructor public  Collection ():
     this-object('character':u).
   end constructor.

   constructor public  Collection ( collectionType as character ):

      if collectionType eq 'Object':u then
         DataType = 'Progress.Lang.Object':u.
      else
         DataType = collectionType.

      create temp-table hTT.
      hTT:add-new-field('elementValue':U, replace(DataType, 'longchar':u, 'clob':u)) no-error.
      if error-status:error or error-status:num-messages ne 0 then do:
         delete object hTT no-error.
         return error.
      end.
      hTT:add-new-field('elementIdx':U, 'integer':u).
      hTT:add-new-index('pkidx':u, true, true).
      hTT:add-index-field('pkidx':u, 'elementIdx':u).

      PrepareTempTable().
   end constructor.


   destructor public Collection ( ):

      if valid-handle(hTT) then
         delete object hTT no-error.
   end destructor.

   method public void Empty (  ):

      hTT:empty-temp-table().
   end method.

   method public logical GetFirst (  ):
      return RepositionIndex(1).
   end method.

   method public   logical GetLast (  ):
      return RepositionIndex(NumElements).
   end method.

   method public logical GetNext (  ):
      return RepositionIndex(IndexPosition + 1).
   end method.

   method public logical GetPrev ():
      return RepositionIndex(IndexPosition - 1).
   end method.

   method public logical GetValue (  output fieldValue as longchar ):
      if IndexPosition gt 0 then do:
         case DataType:
            when 'longchar' then
               copy-lob from hFldVal:buffer-value to fieldValue no-convert.
            when 'character' then
               fieldValue = hFldVal:string-value.
            when 'Progress.Lang.Object' then
               fieldValue = cast(hFldVal:buffer-value, 'Progress.Lang.Object':u):ToString().
            otherwise
               fieldValue = string(hFldVal:buffer-value).
         end case.
         return true.
      end.
      return false.
   end method.


   method public logical GetValue (  output fieldValue as character ):

      if IndexPosition gt 0 then do:
         if DataType eq 'Progress.Lang.Object' then
            fieldValue = cast(hFldVal:buffer-value, 'Progress.Lang.Object':u):ToString().
         else
            fieldValue = string(hFldVal:buffer-value).
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as datetime ):

      if IndexPosition gt 0 and DataType eq 'datetime' then do:
         fieldValue = hFldVal:buffer-value.
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as recid ):

      if IndexPosition gt 0 and DataType eq 'recid' then do:
         fieldValue = hFldVal:buffer-value.
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as rowid ):

      if IndexPosition gt 0 and DataType eq 'rowid' then do:
         fieldValue = hFldVal:buffer-value.
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as Progress.Lang.Object ):

      if IndexPosition gt 0 and DataType eq 'Progress.Lang.Object' then do:
         fieldValue = cast(hFldVal:buffer-value, 'Progress.Lang.Object':u).
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as date ):

      if IndexPosition gt 0 and DataType eq 'date' then do:
         fieldValue = hFldVal:buffer-value.
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as logical ):

      if IndexPosition gt 0 and DataType eq 'logical' then do:
         fieldValue = hFldVal:buffer-value.
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as handle ):

      if IndexPosition gt 0 and DataType eq 'handle' then do:
         fieldValue = hFldVal:buffer-value.
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as decimal ):

      if IndexPosition gt 0 and DataType eq 'decimal' then do:
         fieldValue = hFldVal:buffer-value.
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as int64 ):

      if IndexPosition gt 0 and DataType eq 'int64' then do:
         fieldValue = hFldVal:buffer-value.
         return true.
      end.
      return false.
   end method.

   method public logical GetValue (  output fieldValue as integer ):

      if IndexPosition gt 0 and DataType eq 'integer' then do:
         fieldValue = hFldVal:buffer-value.
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as character ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as longchar ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as datetime ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as recid ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as rowid ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as Progress.Lang.Object ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as date ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as logical ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as handle ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as decimal ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as int64 ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (  output fieldValue as integer ):

      if GetLast() and GetValue(output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Push ( newElement as recid ):

      if DataType eq 'recid':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as rowid ):

      if DataType eq 'rowid':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as Progress.Lang.Object ):

      if DataType eq 'Progress.Lang.Object':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as date ):

      if DataType eq 'date':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as logical ):

      if DataType eq 'logical':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as handle ):

      if DataType eq 'handle':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as decimal ):

      if DataType eq 'decimal':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as int64 ):

      if DataType eq 'int64':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as integer ):

      if DataType eq 'integer':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as character ):

      if DataType eq 'character':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as longchar ):

      if lookup(DataType, 'longchar,clob':U) gt 0  then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Push ( newElement as datetime ):

      if DataType eq 'datetime':U then do:
         hBuf:buffer-create().
         assign
            NumElements = NumElements + 1
            hFldVal:buffer-value = newElement
            hFldIdx:buffer-value = NumElements.
      end.
   end method.

   method public logical Split ( elementList as character, listDelimiter as character ):

      define variable i as integer.

      if lookup(DataType, 'handle,recid,Progress.Lang.Object') gt 0 then
         return false.

      do i = 1 to num-entries(elementList, listDelimiter):
         case DataType:
            when 'character':U
            then Push(entry(i,elementList,listDelimiter)) no-error.

            when 'date':U
            then Push(date(entry(i,elementList,listDelimiter))) no-error.

            when 'datetime':U
            then Push(datetime(entry(i,elementList,listDelimiter))) no-error.

            when 'decimal':U
            then Push(decimal(entry(i,elementList,listDelimiter))) no-error.

            when 'int64':U
            then Push(int64(entry(i,elementList,listDelimiter))) no-error.

            when 'integer':U
            then Push(integer(entry(i,elementList,listDelimiter))) no-error.

            when 'logical':U
            then Push(logical(entry(i,elementList,listDelimiter))) no-error.

            when 'rowid':U
            then Push(to-rowid(entry(i,elementList,listDelimiter))) no-error.
         end case.
         if error-status:error or error-status:num-messages gt 0 then
            return false.
      end.

      return true.
   end method.

   method override public character ToString ():
      return Implode(',':u).
   end method.

   method public character Implode ( listDelimiter as character ):
      define variable cString as character no-undo.
      define variable hQuery  as handle    no-undo.

      create query hQuery.
      hQuery:set-buffers(hBuf).
      hQuery:forward-only = true.
      hQuery:query-prepare(substitute('for each &1', hBuf:name)) no-error.
      hQuery:query-open().

      do while hQuery:get-next():
         cString = substitute('&1&2&3', cString, listDelimiter, string(hFldVal:buffer-value)) no-error.
         if error-status:error or error-status:num-messages gt 0 then do:
            cString = ?.
            leave.
         end.
      end.
      hQuery:query-close().
      delete object hQuery.

      return trim(cString, listDelimiter).
   end method.

   method protected void PrepareTempTable (  ):

      if hTT:prepared then return.

      hTT:undo = false.
      hTT:temp-table-prepare('ttCollection':u).

      assign
         hBuf    = hTT:default-buffer-handle
         hFldVal = hBuf:buffer-field('elementValue':u)
         hFldIdx = hBuf:buffer-field('elementIdx':u).
   end method.

   method protected logical RepositionIndex ( newPosition as integer ):

      if NumElements gt 0 and newPosition gt 0 and newPosition le NumElements then do:
         hBuf:find-first(substitute('where elementIdx eq "&1"', newPosition), no-lock) no-error.
         if hBuf:available then do:
            IndexPosition = newPosition.
            return true.
         end.
      end.
      return false.
   end method.


end class.
