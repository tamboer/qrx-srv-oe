/*---------------------------------------------------------------------
File        : BaseLogger.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Marian
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

class com.quarix.service.logging.BaseLogger
   inherits com.quarix.base.BaseObject
   implements com.quarix.service.logging.iLogger use-widget-pool:

   define public property IsOpen       as logical  initial false no-undo
      get.
      protected set.

   constructor protected BaseLogger ():
   end constructor.

   destructor public BaseLogger():
      CloseLog().
   end destructor.

   method final public logical LogError(input errorLevel as integer, input  errorCode as integer,
      input errorText as character):
      return LogError (errorLevel, errorCode, errorText, ?).
   end method.

   method final public logical LogError(input errorLevel as integer, input  errorCode as integer,
      input errorText as character, input errorParam as character):
      return LogError (errorLevel, errorCode, errorText, errorParam, ?).
   end method.

   method final public logical LogError(input errorLevel as integer, input  errorCode as integer,
      input errorText as character, input errorParam as character,
      input errorObject as character):
      define variable evtTime as datetime no-undo.

      evtTime = dtutil:sysNow().
      return LogError (errorLevel, errorCode, errorText, errorParam, errorObject, evtTime).
   end method.

   method public logical LogError (input errorLevel as integer, input  errorCode as integer,
      input errorText as character, input errorParam as character,
      input errorObject as character, input errorTime as datetime ):
   end method.

   method public logical LogError (input errorLevel as integer, input  errorCode as integer,
      input errorText as character, input errorParam as character,
      input errorObject as character, input errorTime as datetime, input errorRecord as integer, input errorTag as character ):
   end method.

    method public logical LogError(input errorLevel as integer, input errorCode as integer,
      input errorText as character, input errorFile as character, input errorMethod as character,
      input errorLine as integer, input errorTime as datetime):
   end method.

   method public logical OpenLog():
      IsOpen = true.
      return true.
   end method.

   method public logical CloseLog():
      IsOpen = false.
      return true.
   end method.

   method public final logical IsOpen():
      return IsOpen.
   end method.

   method public final logical ReOpenLog():
      return CloseLog() and OpenLog().
   end method.
end class.


