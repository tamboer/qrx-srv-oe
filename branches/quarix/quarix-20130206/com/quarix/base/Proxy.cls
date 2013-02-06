/*------------------------------------------------------------------------
   File        : proxy
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Marian
   Created     : Mon Sep 07 09:52:34 EEST 2009
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

class com.quarix.base.Proxy
   inherits com.quarix.base.BaseObject
   use-widget-pool:

   define public    property ConnectionString  as character no-undo
      get.
      set (connString as character):
         if SetConnectionString (connString) then
         do:
            ConnectionString = connString.
            setStateFree (connString).
            CloseConnection().
         end.
      end set.

   define public    property UserName          as character no-undo
      get.
      set (usrName as character):
         if SetUserName (usrName) then
         do:
            UserName = usrName.
            CloseConnection().
         end.
      end set.

   define public    property UserPassword      as character no-undo
      get.
      set (usrPasswd as character):
         if SetUserPassword (usrPasswd) then
         do:
            UserPassword = usrPasswd.
            CloseConnection().
         end.
      end set.

   define public    property ServerInfo        as character no-undo
      get.
      set (srvInfo as character):
         if SetServerInfo (srvInfo) then
         do:
            ServerInfo = srvInfo.
            CloseConnection().
         end.
      end set.

   define public    property StateFree         as logical   no-undo
      get.
      private set.

   define public    property ServerHandle      as handle    no-undo
      get.
      private set.


   define private    variable appServerPool  as com.quarix.system.net.ApplicationServer no-undo.

   method public logical SetConnectionString (connString as character):
      return true.
   end method.

   method public logical SetUserName (usrName as character):
      return true.
   end method.

   method public logical SetUserPassword (usrPasswd as character):
      return true.
   end method.

   method public logical SetServerInfo  (srvInfo as character):
      return true.
   end method.

   method public final logical OpenConnection ():
      if IsConnectionOpen () then
         return true.

      if Util:IsEmpty(ConnectionString) or ConnectionString eq 'local':u then
      do:
         ServerHandle = session:handle.
         return true.
      end.

      if not valid-object(appServerPool) then
         appServerPool = cast(GetInstance('com.quarix.system.net.ApplicationServer':u),
            'com.quarix.system.net.ApplicationServer':u).

      if valid-object(appServerPool) then
         ServerHandle = appServerPool:GetConnection(ConnectionString, UserName, UserPassword, ServerInfo).
      return valid-handle(ServerHandle).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public final void CloseConnection ():
      CloseConnection(false).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public final logical IsConnectionOpen ():
      return valid-handle (ServerHandle) and
         (ServerHandle eq session:handle or ServerHandle:connected()).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   destructor public Proxy ( ):
      CloseConnection(true).
      UnloadInstance(appServerPool).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end destructor.

   method private void SetStateFree (connString as character):
      StateFree = lookup('Session-free':u, connString, ' ':u) gt 0.
   end method.

   method private void CloseConnection (disconnectStateFree as logical):
      /* do not disconnect for state-free app srv unless the proxy isn't deleted */
      if StateFree and not disconnectStateFree then
         return.

      if valid-handle(ServerHandle) and ServerHandle ne session:handle then
      do:
         if valid-object(appServerPool) then
            appServerPool:CloseConnection(ServerHandle).
         else
         do:
            if ServerHandle:connected() then
               ServerHandle:disconnect().
            delete object ServerHandle.
         end.
      end.
   end method.

end class.
