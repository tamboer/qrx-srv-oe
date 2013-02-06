/*---------------------------------------------------------------------
File        : JsonLogger.cls
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

using com.quarix.data.parser.JsonWriter.
using com.quarix.service.logging.JsonLogger.

class com.quarix.service.logging.JsonLogger
   inherits com.quarix.service.logging.BaseLogger
   implements com.quarix.base.iSingleton use-widget-pool:


   define private variable numMessages    as integer    no-undo.
   define private variable externalWriter as logical    no-undo.
   define private variable jsonWriter_    as JsonWriter no-undo.

   define public  property JsonWriter     as JsonWriter no-undo
      get:
         if not valid-object(jsonWriter_) then assign
            externalWriter = false
            jsonWriter_    = cast(GetInstance('com.quarix.data.parser.JsonWriter':u),
                                  'com.quarix.data.parser.JsonWriter':u).
         return jsonWriter_.
      end get.

   &if keyword-all('static':u) ne ? &then
    define private static variable jsonLogger as JsonLogger no-undo.

    method public static JsonLogger GetInstance():
        if not valid-object(jsonLogger) then
            jsonLogger = new JsonLogger().
        return jsonLogger.
    end method.
   &endif

   constructor public JsonLogger ():
   end constructor.

   constructor public JsonLogger (extWriter as JsonWriter):
      SetJsonWriter (extWriter).
   end constructor.

   destructor JsonLogger():
      if not externalWriter then
         UnloadInstance(jsonWriter_).
      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end.


   method public override logical LogError(input errorLevel as integer, input  errorCode as integer,
      input errorText   as character, input errorParam as character,
	  input errorObject as character, input errorTime  as datetime,
	  input errorRecord as integer  , input errorTag   as character ):

	  if IsOpen and valid-object(JsonWriter) and
	     JsonWriter:Out(substitute('&1~{"type": &2, "code": &3, "msg": &4&5&6&7~}':u,
                     if numMessages eq 0 then '':u else ',':u,
                     errorLevel,
                     errorCode,
	                 JsonWriter:Quote(GetString(errorText)),
	                 if Util:IsEmpty(errorParam)  then '':u else substitute(', "column": &1', JsonWriter:Quote(errorParam)),
	                 if Util:IsEmpty(errorRecord) then '':u else substitute(', "recordId": &1', errorRecord),
	                 if Util:IsEmpty(errorTag)    then '':u else substitute(', "tag": &1', JsonWriter:Quote(errorTag)))) then do:
         numMessages = numMessages + 1.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method override public logical LogError(input errorLevel as integer, input  errorCode as integer,
      input errorText   as character, input errorParam as character,
      input errorObject as character, input errorTime  as datetime ):

      return LogError( errorLevel, errorCode, errorText, errorParam, errorObject, errorTime, ?, ?).
   end method.

    /*roxanam : write errors  */
    method override public logical LogError(input errorLevel as integer, input  errorCode as integer,
      input errorText   as character, input errorFile as character, input errorMethod as character,
      input errorLine   as integer,   input errorTime as datetime ):

      if IsOpen and valid-object(JsonWriter) and

      JsonWriter:Out(substitute('&1~{"type": &2, "code": &3, "msg": &4&5~}':u,
                     if numMessages eq 0 then '':u else ',':u,
                     errorLevel,
                     errorCode,
                     JsonWriter:Quote(GetString(errorText)),
                     if Util:IsEmpty(errorFile) then ', "location": "unknown"':u else
                        substitute('&1&2&3':u,
                            substitute(', "file": &1':u,   JsonWriter:Quote(errorFile)),
                            substitute(', "method": &1':u, JsonWriter:Quote(errorMethod)),
                            substitute(', "line": &1':u,   JsonWriter:Quote(string(errorLine)))
                        ))) then do:
         numMessages = numMessages + 1.
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
      if not IsOpen and valid-object(JsonWriter)
         and (JsonWriter:IsOpen or JsonWriter:OpenStream()) and
         JsonWriter:Out('"messages": [':u) then
         assign
            numMessages = 0
            IsOpen = true.

      return IsOpen.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical OpenLog (mpOut as memptr):
      if not IsOpen and valid-object(JsonWriter)
         and JsonWriter:SetDestination(mpOut)
         and (JsonWriter:IsOpen or JsonWriter:OpenStream()) and
         JsonWriter:Out('"messages": [':u) then
         assign
            numMessages = 0
            IsOpen = true.

      return IsOpen.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method override public logical CloseLog():
      if not IsOpen then return
         true.

      if valid-object(JsonWriter) then do:
         JsonWriter:Out(']':u).
         if not externalWriter then
            JsonWriter:CloseStream().
      end.
      assign
         IsOpen      = false
         numMessages = 0.
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public void SetJsonWriter (extWriter as JsonWriter):
      if not valid-object(extWriter) then
         return.

      if not externalWriter then
         UnloadInstance(jsonWriter_).

      assign
         externalWriter = true
         jsonWriter_    = extWriter.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public void Reset():
   end method.

end class.


