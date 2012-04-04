/*---------------------------------------------------------------------
File        : CoreLogger.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Marian
Created     : Fri Mar 19 13:07:24 EET 2010
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
using com.quarix.service.logging.LoggingCore.

class com.quarix.service.logging.LoggingCore
   inherits com.quarix.service.logging.BaseLogger
   implements com.quarix.base.iSingleton use-widget-pool:

   define variable applicationId as character no-undo.
   define variable appUserId     as character no-undo.

   &if keyword-all('static':u) ne ? &then
    define private static variable loggingCore as LoggingCore no-undo.

    method public static LoggingCore GetInstance():
        if not valid-object(loggingCore) then
            loggingCore = new LoggingCore().
        return loggingCore.
    end method.
   &endif

   method override public logical OpenLog():
      if valid-object(Application) then
      do:
         applicationId = Application:ID.
         if valid-object(Application:CurrentUser) then
            appUserId = Application:CurrentUser:ID.
      end.
      return super:OpenLog().

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method override public logical LogError
      (input errorLevel  as integer,   input errorCode  as integer,
      input errorText   as character, input errorParam as character,
      input errorObject as character, input errorTime  as datetime ):

      define buffer core_error for core_error.

      if not IsOpen then
         return false.

      do transaction
         on error undo, throw:
         create core_error .
         assign
            core_error.app_id       = applicationId
            core_error.user_id      = appUserId
            core_error.err_level    = errorLevel
            core_error.err_code     = errorCode
            core_error.err_msg      = errorText
            core_error.err_time     = errorTime
            core_error.program_name = errorObject no-error.
         if error-status:error then
            undo, throw new Progress.Lang.AppError(error-status:get-message(1), error-status:get-number(1)).
      end.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

    /*roxanam : for the new error template*/
   method override public logical LogError
      (input errorLevel as integer, input errorCode as integer, input errorText as character,
      input errorFile as character, input errorMethod as character,
      input errorLine as integer, input errorTime as datetime):

      define buffer core_error for core_error.

      if not IsOpen then
         return false.

      do transaction
         on error undo, throw:
         create core_error.
         assign
            core_error.app_id       = applicationId
            core_error.user_id      = appUserId
            core_error.err_level    = errorLevel
            core_error.err_code     = errorCode
            core_error.err_msg      = errorText
            core_error.err_time     = errorTime
            core_error.program_name = substitute('&1 &2 &3':u, errorFile, errorMethod, string(errorLine)).
      end.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public void Reset():
   end method.

end class.



