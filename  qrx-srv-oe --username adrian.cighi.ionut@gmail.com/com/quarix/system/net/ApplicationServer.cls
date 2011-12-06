/*------------------------------------------------------------------------
   File        : ApplicationServer
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Marian
   Created     : Tue Sep 01 13:17:27 EEST 2009
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

using com.quarix.system.net.ApplicationServer.

class com.quarix.system.net.ApplicationServer
   inherits com.quarix.base.BaseObject
   implements com.quarix.base.iSingleton use-widget-pool final:

   define temp-table ttServer no-undo
      field srvString  as character
      field srvHandle  as character
      index pk is primary unique srvHandle.


	&if keyword-all('static':u) ne ? &then
   define private static variable applicationServer    as ApplicationServer no-undo.

   constructor private ApplicationServer():
   end constructor.

   method public static ApplicationServer GetInstance():
      if not valid-object(applicationServer) then
         applicationServer = new ApplicationServer().
      return applicationServer.
   end method.

   &else

   constructor public ApplicationServer():
      do on error undo, return error:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.
   end constructor.
   &endif

   destructor public ApplicationServer ( ):
      define variable serverHandle as handle no-undo.

      for each ttServer
          on error undo, throw:
         serverHandle = widget-handle(ttServer.srvHandle).
         endConnection(serverHandle).
      end.
   end destructor.

   method public handle GetConnection
      (connectionString as character, userName   as character,
      userPasswd       as character, serverInfo as character):
      define variable serverHandle as handle    no-undo.
      define variable serverString as character no-undo.

      if Util:IsEmpty(connectionString) then
         return ?.

      serverString = substitute('&1|&2|&3':u, connectionString,
         Util:Nvl(userName, ''),
         Util:Nvl(serverInfo, '')).

      for each ttServer where ttServer.srvString eq serverString
          on error undo, throw:
         serverHandle = widget-handle(ttServer.srvHandle).
         if valid-handle(serverHandle) then
            return serverHandle.
         delete ttServer.
      end.

      create server serverHandle.
      serverHandle:connect(connectionString, userName, userPasswd, serverInfo).
      if serverHandle:connected() then
      do:
         create ttServer.
         assign
            ttServer.srvString = serverString
            ttServer.srvHandle = string(serverHandle).
         return serverHandle.
      end.
      delete object serverHandle.
      return ?.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
   end method.

   method public handle GetConnection
      (connectionString as character, userName as character, userPasswd as character):

      return GetConnection(connectionString, userName, userPasswd, ?).
   end method.

   method public handle GetConnection (connectionString as character):

      return GetConnection(connectionString, ?, ?, ?).
   end method.

   method public void CloseConnection (serverHandle as handle):
      for each ttServer
         where ttServer.srvHandle eq string(serverHandle)
            on error undo, throw:
         endConnection(serverHandle).
         delete ttServer.
      end.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method private void endConnection (serverHandle as handle):
      if valid-handle(serverHandle) and serverHandle ne session:handle then
      do:
         if serverHandle:connected() then
            serverHandle:disconnect().
         delete object serverHandle .
      end.
   end method.

   method public void Reset ():
   end method.
end class.
