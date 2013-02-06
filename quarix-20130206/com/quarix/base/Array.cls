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

class com.quarix.base.Array inherits com.quarix.base.Collection use-widget-pool final:

   define protected variable hFldKey   as handle no-undo.

   constructor public  Array ():
      super().
   end constructor.

   constructor public  Array ( collectionType as character ):
      super(collectionType).
   end constructor.

   method public character GetKey ():
     if IndexPosition gt 0 then
        return hFldKey:buffer-value.
     return ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as character ):

      fieldValue = ?.
      if IndexPosition gt 0 then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as longchar ):

      fieldValue = ?.
      if IndexPosition gt 0 then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue ( input keyName as character, output fieldValue as datetime ):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'datetime':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as recid ):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'recid':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as rowid ):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'rowid':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as Progress.Lang.Object ):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'Progress.Lang.Object':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return valid-object(fieldValue).
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as date ):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'date':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as logical ):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'logical':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as handle ):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'handle':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as decimal ):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'decimal':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as int64 ):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'int64':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical GetValue (  input keyName as character, output fieldValue as integer):

      fieldValue = ?.
      if IndexPosition gt 0 and
         DataType eq 'integer':U then do:
         if repositionToKey(keyName) then GetValue(output fieldValue).
         RepositionIndex(IndexPosition).
      end.
      return fieldValue ne ?.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as character ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as longchar ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as datetime ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as recid ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as rowid ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as Progress.Lang.Object ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as date ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as logical ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as handle ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as decimal ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as int64 ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method public logical Pop (keyName as character,  output fieldValue as integer ):

      if GetValue(keyName, output fieldValue) then do:
         hBuf:buffer-delete().
         NumElements = NumElements - 1.
         RepositionIndex(minimum(IndexPosition, NumElements)).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as recid ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as recid ):

      if DataType eq 'recid':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as rowid ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as rowid ):

      if DataType eq 'rowid':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as Progress.Lang.Object ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as Progress.Lang.Object ):

      if DataType eq 'Progress.Lang.Object':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as date ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as date ):

      if DataType eq 'date':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as logical ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as logical ):

      if DataType eq 'logical':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as handle ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as handle ):

      if DataType eq 'handle':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as decimal ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as decimal ):

      if DataType eq 'decimal':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as int64 ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as int64 ):

      if DataType eq 'int64':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as integer ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as integer ):

      if DataType eq 'integer':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as character ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as character ):

      if DataType eq 'character':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as longchar ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as longchar ):

      if lookup(DataType, 'longchar,clob':U) gt 0 then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Push ( fieldValue as datetime ):
      return Push('', fieldValue).
   end method.

   method public logical Push ( keyName as character, fieldValue as datetime ):

      if DataType eq 'datetime':U then do:
         if repositionToKey(keyName) then
            hFldVal:buffer-value = fieldValue.
         else do:
            hBuf:buffer-create().
            assign
               NumElements = NumElements + 1
               hFldVal:buffer-value = fieldValue
               hFldKey:buffer-value = keyName
               hFldIdx:buffer-value = NumElements no-error.
         end.
         RepositionIndex(IndexPosition).
         return true.
      end.
      return false.
   end method.

   method override public logical Split ( elementList as character, listDelimiter as character ):
      return Split(elementList, listDelimiter, listDelimiter).
   end method.

   method public logical Split ( elementList as character, listDelimiter as character, elementDelimiter as character ):
      define variable i       as integer.
      define variable element as character.

      if lookup(DataType, 'handle,recid,Progress.Lang.Object') gt 0 then
         return false.


      do i = 1 to num-entries(elementList, listDelimiter):
         element = entry(i,elementList,listDelimiter) no-error.
         if num-entries(element, elementDelimiter) ne 2 then
            return false.
         case DataType:
            when 'character':U
            then Push(entry(1,element,elementDelimiter), entry(2,element,elementDelimiter)) no-error.

            when 'date':U
            then Push(entry(1,element,elementDelimiter), date(entry(2,element,elementDelimiter))) no-error.

            when 'datetime':U
            then Push(entry(1,element,elementDelimiter), datetime(entry(2,element,elementDelimiter))) no-error.

            when 'decimal':U
            then Push(entry(1,element,elementDelimiter), decimal(entry(2,element,elementDelimiter))) no-error.

            when 'int64':U
            then Push(entry(1,element,elementDelimiter), int64(entry(2,element,elementDelimiter))) no-error.

            when 'integer':U
            then Push(entry(1,element,elementDelimiter), integer(entry(2,element,elementDelimiter))) no-error.

            when 'logical':U
            then Push(entry(1,element,elementDelimiter), logical(entry(2,element,elementDelimiter))) no-error.

            when 'rowid':U
            then Push(entry(1,element,elementDelimiter), to-rowid(entry(2,element,elementDelimiter))) no-error.
         end case.
      end.

      return true.
   end method.

   method override public character Implode ( listDelimiter as character):
      return Implode(listDelimiter, listDelimiter).
   end method.

   method public character Implode ( listDelimiter as character, elementDelimiter as character ):
      define variable cString as character no-undo.
      define variable hQuery  as handle    no-undo.

      create query hQuery.
      hQuery:set-buffers(hBuf).
      hQuery:forward-only = true.
      hQuery:query-prepare(substitute('for each &1', hBuf:name)).
      hQuery:query-open().

      do while hQuery:get-next():
         cString = substitute('&1&2&3&4&5', cString, listDelimiter, string(hFldKey:buffer-value), elementDelimiter, string(hFldVal:buffer-value)) no-error.
         if error-status:error or error-status:num-messages gt 0 then do:
            cString = ?.
            leave.
         end.
      end.
      hQuery:query-close().
      delete object hQuery.

      return trim(cString, listDelimiter).
   end method.

   method override protected void PrepareTempTable (  ):

      if hTT:prepared then return.

      hTT:add-new-field('elementKey':u, 'character':u).
      hTT:add-new-index('keyidx':u, true).
      hTT:add-index-field('keyidx':u, 'elementKey':u).

      hTT:undo = false.
      hTT:temp-table-prepare('ttArray':u).

      assign
         hBuf    = hTT:default-buffer-handle
         hFldVal = hBuf:buffer-field('elementValue':u)
         hFldKey = hBuf:buffer-field('elementKey':u)
         hFldIdx = hBuf:buffer-field('elementIdx':u).

   end method.

   method private logical repositionToKey (keyName as character):
      hBuf:find-first(substitute('where elementKey eq "&1"', keyName), no-lock) no-error.
      return hBuf:available.
   end method.

end class.
