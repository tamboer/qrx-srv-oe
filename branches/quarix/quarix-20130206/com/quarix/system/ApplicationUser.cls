/*---------------------------------------------------------------------
File        : ApplicationUser.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Tudor
Created     : generated 19/12/08
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

using com.quarix.service.authentication.iAuthentication.
using com.quarix.base.Collection.

class com.quarix.system.ApplicationUser
    inherits com.quarix.base.BaseObject
    implements com.quarix.service.configuration.iConfigurable use-widget-pool final:

    define public property Login          as character    no-undo
        get.
        set.

    define public property ID             as character    no-undo
        get.
        private set.

    define public property Name           as character    no-undo
        get.
        private set.

    define public property Email          as character    no-undo
        get.
        private set.

    define public property DateFormat     as character    no-undo
        get.
        private set.

    define public property Locale         as character    no-undo
        get.
        private set.

    define public property LogicalFormat  as character    no-undo
        get.
        private set.

    define public property NumericFormat  as character    no-undo
        get.
        private set.

    define public property Theme          as character    no-undo
        get.
        private set.

    define public property Groups         as Collection   no-undo
        get.
        private set.


    method public void LoadConfiguration (input configService as com.quarix.service.configuration.iConfiguration):
    end method.

    method public logical ChangePassword (input oldPasswd as character, input newPasswd as character):
       define variable authService as com.quarix.service.authentication.iAuthentication no-undo.

       if valid-object(Application) then do:
          authService = cast(Application:GetService('authentication':u), 'com.quarix.service.authentication.iAuthentication':u) .
          if valid-object(authService) then
             return authService:ChangePassword(this-object, oldPasswd, newPasswd).
       end.
       return false.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
       end catch.
    end method.

    method public logical ResetPassword ():
       define variable authService as com.quarix.service.authentication.iAuthentication no-undo.
       define variable newPasswd   as character no-undo.

       if valid-object(Application) then do:
          authService = cast(Application:GetService('authentication':u), 'com.quarix.service.authentication.iAuthentication':u).
          if valid-object(authService) and
             authService:ResetPassword(this-object, output newPasswd) then do:
             /* send new passwd by email */
             return true.
          end.
        end.
        return false.

        catch appError as Progress.Lang.Error :
           ThrowError(input appError).
           delete object appError.
           return false.
        end catch.
    end method.

end class.


