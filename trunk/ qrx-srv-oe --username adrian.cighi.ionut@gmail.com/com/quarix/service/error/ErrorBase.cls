/*---------------------------------------------------------------------
File        : Error.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Unknown
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

class com.quarix.service.error.ErrorBase
   inherits   com.quarix.base.BaseObject
   implements com.quarix.service.error.iError use-widget-pool:

   &global-define level_error   0
   &global-define level_client  1
   &global-define level_warn    2
   &global-define level_info    3
   &global-define level_debug   4
   &global-define level_app     5

   define public property LogLevel             as integer no-undo
      get.
      protected set.

   define public property NumMessages          as integer no-undo
      get.
      protected set.

   define public property NumErrors            as integer no-undo
      get.
      protected set.

   define public property NumClientErrors            as integer no-undo
      get.
      protected set.

   define public property NumWarnings          as integer no-undo
      get.
      protected set.

   define public property NumInfos             as integer no-undo
      get.
      protected set.

   define public property NumDebugMessages     as integer no-undo
      get.
      protected set.

   define public property NumApplicationErrors as integer no-undo
      get.
      protected set.


   constructor protected ErrorBase ():
   end constructor.

   method public integer GetNumObjectErrors (input errorLevel as integer, errorObject as character):
   end method.

   method public final integer GetNumErrors ():
      return NumErrors.
   end method.

   method public final integer GetNumErrors (errorObject as character):
      return GetNumObjectErrors({&level_error}, errorObject).
   end method.

   method public final integer GetNumClientErrors ():
      return NumClientErrors.
   end method.

   method public final integer GetNumClientErrors (errorObject as character):
      if LogLevel lt {&level_client} then
         return 0.
      return GetNumObjectErrors({&level_client}, errorObject).
   end method.

   method public final integer GetNumMessages ():
      return NumMessages.
   end method.

   method public final integer GetNumMessages (errorObject as character):
      return GetNumObjectErrors(LogLevel, errorObject).
   end method.

   method public final integer GetNumWarnings ():
      return NumWarnings.
   end method.

   method public final integer GetNumWarnings (errorObject as character):
      if LogLevel lt {&level_warn} then
         return 0.
      return GetNumObjectErrors({&level_warn}, errorObject).
   end method.

   method public final integer GetNumInfoMessages ():
      return NumInfos.
   end method.

   method public final integer GetNumInfoMessages (errorObject as character):
      if LogLevel lt {&level_info} then
         return 0.
      return GetNumObjectErrors({&level_info}, errorObject).
   end method.

   method public final integer GetNumDebugMessages ():
      return NumDebugMessages.
   end method.

   method public final integer GetNumDebugMessages (errorObject as character):
      if LogLevel lt {&level_debug} then
         return 0.
      return GetNumObjectErrors({&level_debug}, errorObject).
   end method.

   method public final integer GetNumApplicationErrors ():
      return NumApplicationErrors.
   end method.

   method public final integer GetNumApplicationErrors (errorObject as character):
      return GetNumObjectErrors({&level_app}, errorObject).
   end method.

   method public final integer GetLogLevel ():
      return LogLevel.
   end method.

   method public integer ErrorLevel():
      return {&level_error}.
   end method.

   method public integer ClientLevel():
      return {&level_client}.
   end method.

   method public integer WarningLevel():
      return {&level_warn}.
   end method.

   method public integer InfoLevel():
      return {&level_info}.
   end method.

   method public integer DebugLevel():
      return {&level_debug}.
   end method.

   method public integer ApplicationLevel():
      return {&level_app}.
   end method.

   method public final logical SetLogLevel (input newLogLevel  as integer):
      if newLogLevel ne ? and newLogLevel ge {&level_error} then
      do:
         LogLevel = newLogLevel.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public final void AddApplicationError (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
      if not ErrorRegister({&level_app}, errorCode, errorText, errorParam, errorObject) then
         return.
      assign
         NumMessages          = NumMessages + 1
         NumApplicationErrors = NumApplicationErrors + 1.
      debugProgressError().

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public final void AddApplicationError (input errorCode as integer, input errorText as character, input errorParam as character):
      AddApplicationError(errorCode, errorText, errorParam, ?).
   end method.

   method public final void AddApplicationError (input errorCode as integer, input errorText as character):
      AddApplicationError(errorCode, errorText, ?, ?).
   end method.

   method public final void AddDebug (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
      if not ErrorRegister({&level_debug}, errorCode, errorText, errorParam, errorObject) then
         return.
      NumDebugMessages = NumDebugMessages + 1.
      debugProgressError().

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public final void AddDebug (input errorCode as integer, input errorText as character, input errorParam as character):
      AddDebug(errorCode, errorText, errorParam, ?).
   end method.

   method public final void AddDebug (input errorCode as integer, input errorText as character):
      AddDebug(errorCode, errorText, ?, ?).
   end method.

   method public final void AddError (input errorCode as integer, input errorText as character):
      AddError(errorCode, errorText, ?, ?).
   end method.

   method public final void AddError (input errorCode as integer, input errorText as character, input errorParam as character):
      AddError(errorCode, errorText, errorParam, ?).
   end method.

   method public final void AddError (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
      if not ErrorRegister({&level_error}, errorCode, errorText, errorParam, errorObject) then
         return.
      assign
         NumMessages = NumMessages + 1
         NumErrors   = NumErrors + 1.
      debugProgressError().

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

    method public final void AddError(input errorCode as integer, input errorText as character, input errorFile as character, input errorMethod as character, input errorLine as integer):
       if not ErrorRegister({&level_error}, errorCode, errorText, errorFile, errorMethod, errorLine) then
            return.
       assign
            NumMessages = NumMessages + 1
            NumErrors   = NumErrors + 1.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
       end catch.
   end method.

   method public final void AddClientError (input errorCode as integer, input errorType as integer, input errorText as character, input errorParam as character, input errorObject as character, input errorRecord as integer, input errorTag as character):
      if not ErrorRegister({&level_client}, errorType,errorCode, errorText, errorParam, errorObject, errorRecord,errorTag) then
         return.

      assign
         NumMessages     = NumMessages + 1
         NumClientErrors = NumClientErrors + 1.

      debugProgressError().

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public final void AddClientError (input errorCode as integer, input errorType as integer, input errorText as character, input errorParam as character, input errorObject as character, input errorRecord as integer):
      AddClientError(errorCode, errorType, errorText, errorParam, errorObject,errorRecord,?).
   end method.

   method public final void AddClientError (input errorCode as integer, input errorType as integer, input errorText as character, input errorParam as character, input errorObject as character):
       AddClientError(errorCode, errorType, errorText, errorParam, errorObject,?).
   end method.

   method public final void AddClientError (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
      AddClientError(errorCode, ?, errorText, errorParam, errorObject).
   end method.

   method public final void AddClientError (input errorCode as integer, input errorText as character, input errorParam as character):
      AddClientError(errorCode, errorText, errorParam, ?).
   end method.

   method public final void AddClientError (input errorCode as integer, input errorText as character):
      AddClientError(errorCode, errorText, ?).
   end method.

   method public final void AddInfo (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
      if not ErrorRegister({&level_info}, errorCode, errorText, errorParam, errorObject) then
         return.
      assign
         NumMessages = NumMessages + 1
         NumInfos    = NumInfos + 1.
      debugProgressError().

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public final void AddInfo (input errorCode as integer, input errorText as character, input errorParam as character):
      AddInfo(errorCode, errorText, errorParam, ?).
   end method.

   method public final void AddInfo (input errorCode as integer, input errorText as character):
      AddInfo(errorCode, errorText, ?, ?).
   end method.

   method public final void AddWarning (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
      if not ErrorRegister({&level_warn}, errorCode, errorText, errorParam, errorObject) then
         return.
      assign
         NumMessages = NumMessages + 1
         NumWarnings = NumWarnings + 1.
      debugProgressError().

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public final void AddWarning (input errorCode as integer, input errorText as character, input errorParam as character):
      AddWarning(errorCode, errorText, errorParam, ?).
   end method.

   method public final void AddWarning (input errorCode as integer, input errorText as character):
      AddWarning(errorCode, errorText, ?, ?).
   end method.

   method public final void LogMessages (input logService as com.quarix.service.logging.iLogger):
      log (logService, LogLevel).
   end method.

   method public final void LogMessages (input logService as com.quarix.service.logging.iLogger, input errorObject as character):
      log (logService, LogLevel, errorObject).
   end method.

   method public final void LogErrors (input logService as com.quarix.service.logging.iLogger):
      log (logService, {&level_error}).
   end method.

   method public final void LogErrors (input logService as com.quarix.service.logging.iLogger, input errorObject as character):
      log (logService, {&level_error}, errorObject).
   end method.

   method public final void LogClientErrors (input logService as com.quarix.service.logging.iLogger):
      log (logService, {&level_client}).
   end method.

   method public final void LogClientErrors (input logService as com.quarix.service.logging.iLogger, input errorObject as character):
      log (logService, {&level_client}, errorObject).
   end method.

   method public final void LogWarnings (input logService as com.quarix.service.logging.iLogger):
      log (logService, {&level_warn}).
   end method.

   method public final void LogWarnings (input logService as com.quarix.service.logging.iLogger, input errorObject as character):
      log (logService, {&level_warn}, errorObject).
   end method.

   method public final void LogInfoMessages  (input logService as com.quarix.service.logging.iLogger):
      log (logService, {&level_info}).
   end method.

   method public final void LogInfoMessages  (input logService as com.quarix.service.logging.iLogger, input errorObject as character):
      log (logService, {&level_info}, errorObject).
   end method.

   method public final void LogDebugMessages (input logService as com.quarix.service.logging.iLogger):
      log (logService, {&level_debug}).
   end method.

   method public final void LogDebugMessages (input logService as com.quarix.service.logging.iLogger, input errorObject as character):
      log (logService, {&level_debug}, errorObject).
   end method.

   method public final void LogApplicationErrors (input logService as com.quarix.service.logging.iLogger):
      log (logService, {&level_app}, ?).
   end method.

   method public final void LogApplicationErrors (input logService as com.quarix.service.logging.iLogger, input errorObject as character):
      log (logService, {&level_app}, errorObject).
   end method.

   method public final void Log (input logService as com.quarix.service.logging.iLogger, input logLevel as integer):
      log (logService, logLevel, ?).
   end method.

   method public void Log (input logService as com.quarix.service.logging.iLogger, input logLevel as integer, input errorObject as character):
   end method.

   method public void Purge ():
   end method.

   method public final void Purge (input errorLevel as integer):
      Purge(errorLevel, ?).
   end method.

   method public final void Purge (input errorObject as character):
      Purge(?, errorObject).
   end method.

   method public void Purge (input errorLevel as integer, input errorObject as character):
   end method.

   method public character GetError (input errorIndex as integer, input errorLevel as integer, errorObject as character):
   end method.

   method public final character GetError (input errorIndex as integer, input errorLevel as integer):
      return GetError(errorIndex, errorLevel, ?).
   end method.

   method public final character GetError (input errorIndex as integer):
      return GetError(errorIndex, ?, ?).
   end method.

   method public final character GetApplicationError (input errorIndex as integer, errorObject as character):
      return GetError(errorIndex, {&level_app}, errorObject).
   end method.

   method public final character GetApplicationError (input errorIndex as integer):
      return GetError(errorIndex, {&level_app}).
   end method.

   method protected logical ErrorRegister (input errorLevel as integer, input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
   end method.

   method protected logical ErrorRegister (input errorLevel as integer, input errorType as integer, input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
   end method.

   method protected logical ErrorRegister (input errorLevel as integer, input errorType as integer, input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character, input errorRecord as integer):
   end method.

   method protected logical ErrorRegister (input errorLevel as integer, input errorType as integer, input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character, input errorRecord as integer, input errorTag as character):
   end method.

    /*roxanam*/
   method protected logical ErrorRegister (input errorLevel as integer, input errorCode as integer, input errorText as character, input errorFile as character, input errorMethod as character, input errorLine as integer):
   end method.

   method private void debugProgressError ():
      define variable numErr    as integer no-undo.
      /* log all progress error messages if debug mode */
      if LogLevel eq {&level_debug} and error-status:error then
         do numErr = 1 to error-status:num-messages:
         ErrorRegister({&level_debug}, error-status:get-number(numErr), error-status:get-message(numErr), '':u, ToString()).
      end.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public void DumpToXml():
   end method.

end class.


