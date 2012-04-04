/*------------------------------------------------------------------------
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

using com.quarix.system.ApplicationUser.
using com.quarix.service.authentication.AuthenticationCore.

class com.quarix.service.authentication.AuthenticationCore
    inherits      com.quarix.base.BaseObject
    implements    com.quarix.base.iSingleton, com.quarix.service.authentication.iAuthentication
    use-widget-pool:

    &if keyword-all('static':u) ne ? &then
    define private static variable authenticationCore as AuthenticationCore no-undo.

    method public static AuthenticationCore GetInstance():
        if not valid-object(authenticationCore) then
            authenticationCore = new AuthenticationCore().
        return authenticationCore.
    end method.
   &endif

    /*
       Method: Login
       Authenticate the user for currently selected application.

       Parameters:
           userName   - [in] user login account
         userPasswd - [in] user password

       Returns:
           authentication result, true if user credentials were valid for given application
    */

    method  public logical Login
        (appUser     as ApplicationUser,
        userPasswd  as character):

        define buffer userBuf for core_user.
        define buffer appBuf  for core_app_user.

        if valid-object(appUser) and valid-object(Application) then
            for each userBuf no-lock
                where userBuf.user_login eq appUser:Login
                and userBuf.user_passwd = encode(userPasswd),
                each appBuf no-lock
                where appBuf.app_id  eq Application:ID
                and   appBuf.user_id eq userBuf.user_id:
                return true.
            end.
        return false.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.

    method public void    Logout (input appUser as ApplicationUser):
    end method.

    method  public logical ChangePassword (input appUser as ApplicationUser, input oldPasswd as character, input newPasswd as character):
        define buffer userBuf for core_user.
        define buffer appBuf  for core_app_user.

        if valid-object(appUser) and valid-object(Application) then
            for each userBuf exclusive-lock
                where userBuf.user_login eq appUser:Login
                and userBuf.user_passwd = encode(oldPasswd),
                each appBuf no-lock
                where appBuf.app_id  eq Application:ID
                and   appBuf.user_id eq userBuf.user_id
                on error undo, throw:
                userBuf.user_passwd = encode(newPasswd).
                return true.
            end.
        return false.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.

    method  public logical ResetPassword (input appUser as ApplicationUser, output newPasswd as character):
        define buffer userBuf for core_user.
        define buffer appBuf  for core_app_user.

        if valid-object(appUser) and valid-object(Application) then
            for each userBuf exclusive-lock
                where userBuf.user_login eq appUser:Login,
                each appBuf no-lock
                where appBuf.app_id  eq Application:ID
                and   appBuf.user_id eq userBuf.user_id
                on error undo, throw:
                assign
                    newPasswd           = substring(encode(guid), 2, 12)
                    userBuf.user_passwd = encode(newPasswd).
                return true.
            end.
        return false.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.
    end method.

    method public void Reset():
    end method.

end class.


