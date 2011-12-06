 /*------------------------------------------------------------------------
    File        : Context
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Tue Dec 16 15:16:03 EET 2008
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

&scoped-define config-section           'Service':u
&scoped-define config-object            'Context':u
&scoped-define config-key-passthrough   'SessionPassThrough':u

using com.quarix.service.context.ContextCore.

class com.quarix.service.context.ContextCore
   inherits   com.quarix.service.context.ContextBase
   implements com.quarix.service.configuration.iConfigurable, com.quarix.base.iSingleton use-widget-pool:

   define private temp-table ttContext no-undo
      field sourceObject    as character
      field keyName         as character
      field keyValue        as blob
      field isDeleted       as logical
      index PK_ttContext    is primary is unique sourceObject keyName.

   &if keyword-all('static':u) ne ? &then
   define private static variable contextCore    as ContextCore no-undo.

   constructor private ContextCore():
   end constructor.

   method public static ContextCore GetInstance():
      if not valid-object(contextCore) then
         contextCore = new ContextCore().
      return contextCore.
   end method.
   &else
   constructor public ContextCore():
      do on error undo, return error:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.
   end constructor.
   &endif

   method override public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as memptr):
   define buffer ttContext for ttContext.

      set-size(keyValue) = 0.
      if SessionPassThrough then
         return super:GetValue(sourceObject, keyName, output keyValue).

      for each ttContext
         where ttContext.sourceObject eq sourceobject
         and   ttContext.keyName      eq keyName
         and   ttContext.isDeleted    eq false
            on error undo, throw:

         keyValue = ttContext.keyValue.
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
   define buffer ttContext for ttContext.

      if SessionPassThrough then
         return super:SetValue(sourceObject, keyName, keyValue).

      for each ttContext
         where ttContext.sourceObject eq sourceobject
         and   ttContext.keyName      eq keyName
            on error undo, throw:

         if get-size(keyValue) eq 0 then
            ttContext.isDeleted   = true.
         else assign
               ttContext.isDeleted   = false
               ttContext.keyValue    = keyValue.
         return true.
      end.
      if get-size(keyValue) eq 0 then
         return true.
      create ttContext.
      assign
         ttContext.sourceObject = sourceObject
         ttContext.keyName      = keyName
         ttContext.keyValue     = keyValue.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method override public logical SaveContext (input sessionManager as com.quarix.service.session.iSession):
      define variable memptrValue as  memptr no-undo.
      define buffer   ttContext   for ttContext.

      if not valid-object(sessionManager) then
         return false.

      if SessionPassThrough and valid-object(contextSession) and sessionManager eq contextSession then
         return true.

      for each ttContext
         on error undo, throw:
         if ttContext.isDeleted then
            sessionManager:RemoveValue(ttContext.sourceObject, ttContext.keyName).
         else
         do:
            copy-lob ttContext.keyValue to memptrValue.
            sessionManager:SetValue(ttContext.sourceObject, ttContext.keyName, memptrValue).
            set-size(memptrValue) = 0.
         end.
      end.
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method override public void Purge ():
      empty temp-table ttContext.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method override public void Purge (input sourceObject as character):
   define buffer ttContext for ttContext.

      for each ttContext where ttContext.sourceObject eq sourceObject
         on error undo, throw:
         delete ttContext.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public void LoadConfiguration(input configService as com.quarix.service.configuration.iConfiguration) :
      if valid-object(configService) then assign
            SessionPassThrough = Util:Nvl(configService:GetKey({&config-key-passthrough}, {&config-section}, {&config-object}), SessionPassThrough).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method public void Reset ():
   end method.
end class.
