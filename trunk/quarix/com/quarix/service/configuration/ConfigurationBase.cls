/*---------------------------------------------------------------------
File        : ConfigurationBase.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Marian
Created     : generated 10/12/08
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

using com.quarix.service.configuration.iConfiguration.

class com.quarix.service.configuration.ConfigurationBase
   inherits com.quarix.base.BaseObject
   implements iConfiguration use-widget-pool:

   {com/quarix/service/configuration/dsConfig.i &scope=private}

   define public     property Environment         as character no-undo   initial ?
      get.
      protected set.

   define public     property SaveOnClose         as logical   no-undo   initial true
      get.
      set.

   define protected  property PassThrough         as logical   no-undo   initial true
      get.
      set.

   define protected  property MergeOnSave         as logical   no-undo   initial true
      get.
      set.

   constructor protected ConfigurationBase ():
   end constructor.

   method final public character GetEnvironment ():
      return Environment.
   end method.

   method public void CloseEnvironment (input environmentName as character):
      define buffer   ttEnvironment  for ttEnvironment.
      define variable oldEnvironment as  character       no-undo.

      if Util:IsEmpty(environmentName) then
         return.

      /* switch environment so we can use regular remove/save key methods */
      if environmentName ne Environment then
         assign
            oldEnvironment = Environment
            Environment    = environmentName.

      for each ttEnvironment
         where ttEnvironment.envName eq environmentName
            on error undo, throw:
         if SaveOnClose then
            SaveEnvironment(environmentName).
         Purge(environmentName).

         delete ttEnvironment.
         if oldEnvironment ne '' then
            Environment = oldEnvironment.
         else
            Environment = ?.
      end.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method final public void CloseEnvironment ():
      CloseEnvironment(Environment).
   end method.

   method final public logical DeleteKey (input keyName as character, input sectionName as character, input objectName as character):
      define buffer ttConfig for ttConfig.

      if Environment eq ? then
         return false.

      /* if pass-through call remove key method here, if not enable
         tracking-changes to be able to save them later on  */
      if PassThrough and not RemoveKey(keyName, sectionName, objectName) then
         return false.
      else
         temp-table ttConfig:tracking-changes = MergeOnSave.

      for each ttConfig
         where ttConfig.envName     eq Environment
         and ttConfig.sectionName   eq sectionName
         and ttConfig.objectName    eq objectName
         and ttConfig.keyName       eq keyName
            on error undo, throw:
         delete ttConfig.
         temp-table ttConfig:tracking-changes = false.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.


   method final public logical DeleteKey (input keyName as character, input sectionName as character):
      return DeleteKey(keyName, sectionName, '':u).
   end method.

   method final public logical DeleteKey (input keyName as character):
      return DeleteKey(keyName, '':u, '':u).
   end method.

   method final public logical DeleteSection (input sectionName as character):
      return Purge(Environment, sectionName, '':u).
   end method.

   method final public logical DeleteSection (input sectionName as character, input objectName as character):

      return Purge(Environment, sectionName, objectName).
   end method.

   method final public character GetKey (input keyName as character, input sectionName as character, input objectName as character):
      define buffer ttConfig for ttConfig.

      define variable keyValue as character no-undo.

      if Environment eq ? then
         return ?.

      for each ttConfig no-lock
         where ttConfig.envName   eq Environment
         and ttConfig.sectionName eq sectionName
         and ttConfig.objectName  eq objectName
         and ttConfig.keyName     eq keyName:
         return ttConfig.keyValue.
      end.

      if not PassThrough then
         return ?.

      keyValue = LoadKey(keyName, sectionName, objectName).
      if keyValue ne ? then do:
         create ttConfig.
         assign
            ttConfig.envName       = Environment
            ttConfig.sectionName   = sectionName
            ttConfig.objectName    = objectName
            ttConfig.keyName       = keyName
            ttConfig.keyValue      = keyValue.
      end.
      return keyValue.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method final public character GetKey (input keyName as character, input sectionName as character):
      return GetKey(keyName, sectionName, '':u).
   end method.

   method final public character GetKey (input keyName as character):
      return GetKey(keyName, '':u, '':u).
   end method.

   method final public logical OpenEnvironment (input environmentName as character):

      define buffer ttEnvironment for ttEnvironment.

      if Util:IsEmpty(environmentName) then
         return false.

      for each ttEnvironment
         where ttEnvironment.envName eq environmentName
            on error undo, throw:
         Environment = environmentName.
         return true.
      end.
      /* check for valid environment, load external env identifier */
      if not LoadEnvironment(environmentName) then
         return false.

      create ttEnvironment.
      assign
         Environment               = environmentName
         ttEnvironment.envName     = environmentName
         ttEnvironment.externalEnv = externalEnv.

      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.

   end method.


   method public logical SaveEnvironment (input environmentName as character):
      /* for pass-through all updates are committed right away */
      define buffer   ttConfig       for ttConfig.
      define variable bttHandle      as  handle    no-undo.
      define variable oldEnvironment as  character no-undo.
      define variable retValue       as  logical   no-undo   initial true.

      if Util:IsEmpty(environmentName) then
         return false.

      if PassThrough or
         (MergeOnSave and not temp-table bttConfig:has-records) then
         return true.

      /* switch environment so we can use regular remove/save key methods */
      if environmentName ne Environment then
         assign
            oldEnvironment = Environment
            Environment    = environmentName.

      /* if merge are not supported save all keys */
      if not MergeOnSave then do:
         saveBlock:
         do transaction
            on error undo, throw:
            for each ttConfig
               where ttConfig.envName eq Environment
                  on error undo, throw:
               if not SaveKey(ttConfig.keyName, ttConfig.keyValue, ttConfig.sectionName, ttConfig.objectName) then do:
                  retValue = false.
                  undo, leave saveBlock.
               end.
            end.
         end.
      end.
      else do:
         /* save changes only if merge is possible */
         bttHandle = temp-table ttConfig:before-table:default-buffer-handle.

         mergeBlock:
         do transaction
            on error undo, throw:
            for each bttConfig
               on error undo, throw:
               /* for deleted keys call remove key method, rollback if fails */
               if row-state(bttConfig) eq row-deleted then do:
                  if bttConfig.envName eq Environment and not RemoveKey(bttConfig.keyName, bttConfig.sectionName, bttConfig.objectName) then do:
                     retValue = false.
                     undo, leave mergeBlock.
                  end.
                  next.
               end.
               /* updated or new */
               for each ttConfig where rowid(ttConfig) eq bttHandle:after-rowid
                  on error undo, throw:
                  if ttConfig.envName eq Environment and not SaveKey(ttConfig.keyName, ttConfig.keyValue, ttConfig.sectionName, ttConfig.objectName) then do:
                     retValue = false.
                     undo, leave mergeBlock.
                  end.
                  next.
               end.
            end.
         end.
      end.

      /* restore previous environment if needed */
      if oldEnvironment ne '' then
         Environment = oldEnvironment.

      if not retValue then
         return false.

      temp-table ttConfig:default-buffer-handle:accept-changes().
      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method final public logical SaveEnvironment ():
      return SaveEnvironment(Environment).
   end method.

   method final public logical Purge ():
      return Purge(Environment).
   end method.

   method final public logical Purge (input environmentName as character):
      return Purge (environmentName, '':u, '':u).
   end method.

   method private logical Purge (input environmentName as character,
      input sectionName as character,
      input objectName as character):

      define buffer   ttConfig       for ttConfig.
      define variable oldEnvironment as  character no-undo.
      define variable retValue       as  logical   no-undo   initial true.

      if Util:IsEmpty(environmentName) then
         return false.

      /* switch environment so we can use regular remove/save key methods */
      if environmentName ne Environment then
         assign
            oldEnvironment = Environment
            Environment    = environmentName.

      saveBlock:
      do transaction
         on error undo, throw:
         for each ttConfig
            where ttConfig.envName eq environmentName
            and (sectionName eq '':u  or ttConfig.sectionName eq sectionName)
            and (objectName  eq '':u  or ttConfig.objectName  eq objectName)
               on error undo, throw:
            if not DeleteKey (ttConfig.keyName, ttConfig.sectionName, ttConfig.objectName) then do:
               retValue = false.
               undo, leave saveBlock.
            end.
         end.
      end.
      /* reset back the environment if it was changed */
      if oldEnvironment ne '' then
         Environment = oldEnvironment.
      return retValue.

   end method.

   method final public logical SetKey (input keyName as character, input keyValue as character, input sectionName as character, input objectName as character):
      define buffer   ttConfig for ttConfig.

      if Environment eq ? then
         return false.

      if Util:IsEmpty(keyValue) then
         return DeleteKey(keyName, sectionName, objectName).

      if PassThrough and not SaveKey(keyName, keyValue, sectionName, objectName) then
         return false.
      else
         temp-table ttConfig:tracking-changes = MergeOnSave.

      for each ttConfig
         where ttConfig.envName   eq Environment
         and ttConfig.sectionName eq sectionName
         and ttConfig.objectName  eq objectName
         and ttConfig.keyName     eq keyName
            on error undo, throw:
         assign
            ttConfig.keyValue = keyValue
            temp-table ttConfig:tracking-changes = false.
         return true.
      end.

      create ttConfig.

      assign
         ttConfig.envName     = Environment
         ttConfig.sectionName = sectionName
         ttConfig.objectName  = objectName
         ttConfig.keyName     = keyName
         ttConfig.keyValue    = keyValue
         temp-table ttConfig:tracking-changes = false.

      return true.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.


   method final public logical SetKey (input keyName as character, input keyValue as character, input sectionName as character):
      return SetKey(keyName, keyValue, sectionName, '':u).
   end method.

   method final public logical SetKey (input keyName as character, input keyValue as character):
      return SetKey(keyName, keyValue, '':u, '':u).
   end method.

   method protected logical LoadEnvironment (input environmentName as character):
      return true.
   end method.

   method protected character LoadKey (input keyName as character, input sectionName as character, input objectName as character):
      return ?.
   end method.

   method protected logical RemoveKey (input keyName as character, input sectionName as character, input objectName as character):
      return true.
   end method.

   method protected logical SaveKey (input keyName as character, input keyValue as character, input sectionName as character, input objectName as character):
      return true.
   end method.
end class.


