/*---------------------------------------------------------------------
File        : DBMSLogger.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Dragos
Created     : generated 11/12/08
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
---------------------------------------------------------------------*/
routine-level on error undo, throw.
&scoped-define log-table-fields       'errorLevel,errorCode,errorText,errorParam,errorTime,errorObject':u
&scoped-define log-field-type         'integer,integer,character,character,datetime,character':u
&scoped-define log-config-section     'Service':u
&scoped-define log-config-object      'DBMSLogger':u
&scoped-define log-config-key-table   'TableName':u
&scoped-define log-config-key-map     'FieldMap':u

class com.quarix.service.logging.DBMSLogger
   inherits com.quarix.service.logging.BaseLogger
   implements com.quarix.service.configuration.iConfigurable use-widget-pool:

   define private  variable  hBuffer    as handle    no-undo.
   define private  variable  rgFields   as handle    no-undo extent 6.

   define public property FieldMap      as character no-undo
      get.
      set(newFieldMap as character):
         if newFieldMap ne FieldMap and SetFieldMap(newFieldMap) then
            FieldMap = newFieldMap.
      end set.

   define public property TableName     as character no-undo
      get.
      set (newTable as character):
         if newTable ne TableName and SetTableName(newTable) then
            TableName = newTable.
      end set.

   constructor public DBMSLogger ():
      this-object(?, ?).
   end constructor.

   constructor public DBMSLogger (tblName as character):
      this-object(tblName, ?).
   end constructor.

   constructor public DBMSLogger (tblName as character, fldMap as character):
      assign
         TableName = tblName
         FieldMap  = fldMap.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end constructor.

   destructor public DBMSLogger ():
      if valid-handle(hBuffer) then
         delete object hBuffer.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end destructor.

   method public void LoadConfiguration(input configService as com.quarix.service.configuration.iConfiguration) :
      /* if properties not already set load those from configuration service provided */
      if valid-object(configService) then assign
         TableName = Util:Nvl(TableName, configService:GetKey({&log-config-key-table}, {&log-config-section}, {&log-config-object}))
         FieldMap  = Util:Nvl(FieldMap, configService:GetKey({&log-config-key-map}, {&log-config-section}, {&log-config-object})).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method override public logical LogError(input errorLevel as integer, input  errorCode as integer,
      input errorText   as character, input errorParam as character,
      input errorObject as character, input errorTime  as datetime ):

      define variable hdlField as handle no-undo.

      /* at least the field to hold the text message should be set */
      if IsOpen and valid-handle(hBuffer) and valid-handle(rgFields[3]) then
      do transaction
          on error undo, throw:
         hBuffer:buffer-create.
         rgFields[1]:buffer-value = errorLevel  .
         rgFields[2]:buffer-value = errorCode   .
         rgFields[3]:buffer-value = errorText   .
         rgFields[4]:buffer-value = errorParam  .
         rgFields[5]:buffer-value = errorTime   .
         rgFields[6]:buffer-value = errorObject .
         hBuffer:buffer-validate() no-error.
         if error-status:error then
            undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method override public logical OpenLog():
      if not valid-handle(hBuffer) then
         return false.
      IsOpen = true.
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method private handle getField (fieldName as character, fldMap as character):
      define variable hFld as handle no-undo.

      hFld = hBuffer:buffer-field(fieldName).

      if lookup(fieldName, fldMap) gt 0 then
      do:
         fieldName = entry(lookup(fieldName, fldMap) + 1, fldMap).
         if valid-handle(hBuffer:buffer-field(fieldName)) then
            hFld = hBuffer:buffer-field(fieldName).
      end.
      return hFld.

   end method.

   method private logical SetTableName(newTable as character):
      define variable newBufHdl as handle  no-undo.

      create buffer newBufHdl for table newTable.
      if valid-handle(newBufHdl) then
      do:
         if valid-handle(hBuffer) then
            delete object hBuffer.
         hBuffer = newBufHdl.
         SetFieldMap(FieldMap).
         return true.
      end.
      return false.

   end method.

   method private logical SetFieldMap (newFieldMap as character):
      define variable fieldNum as integer no-undo.
      define variable fieldHdl as handle  no-undo.

      if valid-handle(hBuffer) then
      do:
         do fieldNum = 1 to 6:
            fieldHdl = getField(entry(fieldNum, {&log-table-fields}), newFieldMap).
            if valid-handle(fieldHdl) and fieldHdl:data-type eq entry(fieldNum, {&log-field-type}) then
               rgFields[fieldNum] = fieldHdl.
            else
               rgFields[fieldNum] = ?.
         end.
      end.
      /* at least the field to hold the text message should be set */
      return valid-handle(rgFields[3]).
   end method.
end class.


