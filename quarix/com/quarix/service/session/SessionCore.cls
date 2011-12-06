 /*------------------------------------------------------------------------
    File        : SessionCore
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Mon Dec 22 16:07:31 EET 2008
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

using com.quarix.service.session.SessionCore.

class com.quarix.service.session.SessionCore
   inherits com.quarix.service.session.SessionBase
   implements com.quarix.base.iSingleton use-widget-pool:

   define private variable loopBack as logical no-undo.

   &if keyword-all('static':u) ne ? &then
   define private static variable sessionCore    as SessionCore no-undo.

   constructor private SessionCore():
   end constructor.

   method public static SessionCore GetInstance():
      if not valid-object(sessionCore) then
         sessionCore = new SessionCore().
      return sessionCore.
   end method.
   &else
   constructor public SessionCore():
      do on error undo, return error:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.
   end constructor.
   &endif

   method override public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as memptr):
      define buffer sessBuf  for sess_var.

      set-size(keyValue) = 0.

      if ID = ? then return false.

      for each sessBuf no-lock
         where sessBuf.sess_id    eq ID
         and   sessBuf.obj_id     eq sourceobject
         and   sessBuf.sess_param eq keyName
            on error undo, throw:
         keyValue = sessBuf.sess_value .
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method override public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as memptr):
      define buffer sessBuf  for sess_var.

      if ID = ? then return false.

      find sessBuf
         where sessBuf.sess_id    eq ID
         and   sessBuf.obj_id     eq sourceobject
         and   sessBuf.sess_param eq keyName no-lock no-error.
      /*roxanam : trebuie sa mearga mai departe...reprezinta numai o validare*/
      if not available(sessBuf) then
         do transaction on error undo, throw:

         error-status:error = false.

         if get-size(keyValue) eq 0 then
            return true.

         create sessBuf.
         assign
            sessBuf.sess_id    = ID
            sessBuf.obj_id     = sourceObject
            sessBuf.sess_param = keyName
            sessBuf.sess_value = keyValue .
         release sessBuf.
         return true.
      end.
      else do transaction on error undo, throw:
         find current sessBuf exclusive-lock no-wait.
         if not available(sessBuf) then
            return false.

         if get-size(keyValue) eq 0 then
            delete sessBuf.
         else
            sessBuf.sess_value = keyValue.
         release sessBuf.
         return true.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method override public void Destroy (sessId as character):
      define buffer sessPool for sess_pool.
      define buffer sessBuf  for sess_var.

      for each sessPool exclusive-lock
         where sessPool.sess_id eq sessId
            on error undo, throw:
         for each sessBuf exclusive-lock
            where sessBuf.sess_id eq sessPool.sess_id
               on error undo, throw:
            delete sessBuf.
         end.
         delete sessPool.
      end.

      release sessBuf.
      release sessPool.
      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method override public void Start (sessId as character, sessTimeOut as integer):
      define buffer sessPool for sess_pool.
      define buffer sessBuf  for sess_var.

      define variable recCount as integer  no-undo initial 5.
      define variable dtExpire as datetime no-undo.
      DEFINE VARIABLE dzNow    AS DATETIME-TZ NO-UNDO.

      super:Start(sessId, sessTimeOut).

      /* clean-up some orphan sessions if session time-out set */
      if MaxInactivityInterval gt 0 then do:
         dtExpire = com.quarix.util.dtutil:nowTZ() - MaxInactivityInterval * 1000.
         for each sessPool no-lock
            where sessPool.last_time lt dtExpire
               on error undo, throw:
            Destroy(sessPool.sess_id).
            recCount = recCount - 1.
            if recCount lt 0 then leave.
         end.
      end.

      for each sessPool exclusive-lock
         where sessPool.sess_id eq ID
            on error undo, throw:
         ASSIGN dzNow = com.quarix.util.dtutil:nowTZ().
         if sessPool.app_id eq Application:ID and
            dzNow - sessPool.last_time le MaxInactivityInterval * 1000 then
            assign
               StartTime           = sessPool.start_time
               sessPool.last_time  = LastAccessTime.
         else do:
            /* if session exist for other application or time-out start fresh */
            start(?, sessTimeOut).
         end.
         return.
      end.

      do for sessPool transaction
         on error undo, throw:
         create sessPool.
         assign
            StartTime             = LastAccessTime
            sessPool.app_id       = Application:ID
            sessPool.sess_id      = ID
            sessPool.last_time    = LastAccessTime
            sessPool.start_time   = LastAccessTime .
         if valid-object(Application:CurrentUser) then
            sessPool.user_id      = Application:CurrentUser:ID.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method override public logical LoadContext (input sessContext as com.quarix.service.context.iContext):
      define buffer   sessBuf     for sess_var.
      define variable memptrValue as memptr  no-undo.
      define variable retVal      as logical no-undo.

      if ID = ? or not valid-object(sessContext) then
         return false.

      if not loopBack and sessContext:GetPassThrough() then do:
         loopBack = true.
         retVal = sessContext:LoadContext(this-object).
         loopBack = false.
         return retVal.
      end.
      else do:
         for each sessBuf no-lock
            where sessBuf.sess_id eq ID
               on error undo, throw:
            memptrValue = sessBuf.sess_value.
            sessContext:SetValue(sessBuf.obj_id, sessBuf.sess_param, memptrValue).
            set-size(memptrValue) = 0.
         end.
         loopBack = false.
         return true.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method public void Reset():
   end method.

end class.


