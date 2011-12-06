/*------------------------------------------------------------------------
   File        : Factory
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Tudor
   Created     : Tue Dec 09 10:17:50 EET 2008
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

using com.quarix.base.Factory.

class com.quarix.base.Factory
   implements com.quarix.base.iSingleton
   use-widget-pool final:

   define public property   ErrorStatus       as logical        no-undo
      get.
      private set.

   define public property   ErrorMessage      as character      no-undo
      get.
      private set.

   define private temp-table ttInstance
      field className					as character
      field isSingleton					as logical
      field instanceHandle				as Progress.Lang.Object
      field instantiatingObjectHandle	as Progress.Lang.Object
      index ttInstance_className is primary className.

   define private temp-table ttHelper
      field packageName    as character
      field helperHandle   as handle
      index PK_ttHelper    is primary unique packageName.


   &if keyword-all('static':u) ne ? &then
   define private static variable ablFactory   as Factory no-undo.

   constructor private Factory():
   end constructor.

   method public static Factory GetInstance():
      if not valid-object(ablFactory) then
         ablFactory = new Factory().
      return ablFactory.
   end method.

   &else

   constructor public Factory():
      do on error undo, return error:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.
   end constructor.
   &endif

   destructor public Factory():
      Purge().
   end destructor.

   method  public Progress.Lang.Object GetInstance
      (className  as character, input instantiatingObjectHandle as Progress.Lang.Object):

      define variable objInstance   as Progress.Lang.Object no-undo.
      define variable factoryHelper as handle               no-undo.

      define buffer   ttInstance    for ttInstance.

      assign
         ErrorStatus  = false
         ErrorMessage = '':u.

      /* check for singleton's instances */
      for each ttInstance no-lock
         where ttInstance.className eq className
         and   ttInstance.isSingleton:

         if valid-object(ttInstance.instanceHandle) then
         do:
            cast(ttInstance.instanceHandle, 'com.quarix.base.iSingleton':u):Reset().

            return ttInstance.instanceHandle.
         end.

         /* if singleton was purged clean up the records as well */
         delete ttInstance.
      end.

      /* use the 'old' factory helper way if dynamic-new isn't available or default
         constructor isn't available (either does not exist or is not public) */

      factoryHelper = getFactoryHelper(className).

      if valid-handle(factoryHelper) then
         run getInstance in factoryHelper (substring(className, r-index(className, '.') + 1, -1, 'character':u),
            output objInstance) no-error.
      else
         assign
            ErrorStatus  = true
            ErrorMessage = 'Factory helper can not be loaded.':u.

      /* try first the dynamic-new statement if available */
      &if keyword-all('dynamic-new':u) ne ? &then
      if not valid-object(objInstance) then
         objInstance = dynamic-new className () no-error.
      &endif

      if valid-object(objInstance) then
      do:
         create ttInstance.

         assign
            ErrorStatus								= false
            ErrorMessage							= '':u
            ttInstance.className					= className
            ttInstance.isSingleton					= type-of(objInstance, 'com.quarix.base.iSingleton':u)
            ttInstance.instanceHandle				= objInstance
            ttInstance.instantiatingObjectHandle	= instantiatingObjectHandle.

         if ttInstance.isSingleton then
            cast(objInstance, 'com.quarix.base.iSingleton':u):Reset().

         return objInstance.
      end.
      else do:
         assign
            ErrorStatus  = true
            ErrorMessage = substitute ('Unable to instantiate class &1.':u, className).
      end.

      return ?.

      catch appError as Progress.Lang.Error :
         delete object appError.
         return ?.
      end catch.

   end method.

   method public void Unload (instanceObj as Progress.Lang.Object):

      define buffer ttInstance for ttInstance.

      if not valid-object(instanceObj) then
      return.

      for each ttInstance
         where ttInstance.isSingleton eq false:

         if valid-object(ttInstance.instanceHandle) and instanceObj eq ttInstance.instanceHandle then
         do
            on error undo, return
             on stop  undo, return:

            delete object ttInstance.instanceHandle no-error.
            delete ttInstance no-error.

            return.
         end.
      end.

      catch appError as Progress.Lang.Error :
         delete object appError.
         return.
      end catch.

   end method.

	method public void UnloadInstances (instantiatingObjectHandle as Progress.Lang.Object):

   		define buffer ttInstance for ttInstance.

		for each ttInstance
			where ttInstance.instantiatingObjectHandle = instantiatingObjectHandle
			no-lock:

			delete object ttInstance.instanceHandle no-error.

			delete ttInstance no-error.
		end.

		catch appError as Progress.Lang.Error :
        	delete object appError.
         	return.
		end catch.

   end method.

   method public void Unload ():

      define buffer ttInstance for ttInstance.

      for each ttInstance:
         if ttInstance.isSingleton and valid-object(ttInstance.instanceHandle) then
         do:
            cast(ttInstance.instanceHandle, com.quarix.base.iSingleton):Reset().
            next.
         end.

         if valid-object(ttInstance.instanceHandle) then
            delete object ttInstance.instanceHandle no-error.

         delete ttInstance.
      end.

      catch appError as Progress.Lang.Error :
         delete object appError.
         return.
      end catch.

   end method.

   method public void Purge ():
      define buffer   ttInstance    for ttInstance.
      define buffer   ttHepler      for ttHelper.

      for each ttInstance:
         if valid-object(ttInstance.instanceHandle) then
            delete object ttInstance.instanceHandle no-error.
      end.

      for each ttHelper:
         if valid-handle(ttHelper.helperHandle) then
            delete object ttHelper.helperHandle no-error.
      end.

      empty temp-table ttInstance.
      empty temp-table ttHelper.

      catch appError as Progress.Lang.Error :
         delete object appError.
         return.
      end catch.

   end method.


   method private handle getFactoryHelper
      (className     as character):

      define variable packageName   as character no-undo.
      define variable factoryHdl    as handle    no-undo.
      define buffer   ttHelper      for ttHelper.

      assign
         packageName = substring(className, 1, r-index(className, '.') - 1, 'character':u)
         className   = replace(substitute('&1.FactoryHelper':u, packageName), '.', '/').

      for each ttHelper where ttHelper.packageName eq packageName:
         if valid-handle(ttHelper.helperHandle) then return ttHelper.helperHandle.
         delete ttHelper.
      end.

      if search(substitute('&1.p':u, className)) ne ?
         or search(substitute('&1.r':u, className)) ne ? then
      do
         on error undo, leave
         on quit  undo, leave
         on stop  undo, leave:

         run value(substitute('&1.p':u, className)) persistent set factoryHdl no-error.

         if valid-handle(factoryHdl) then
         do:
            create ttHelper.
            assign
               ttHelper.packageName		= packageName
               ttHelper.helperHandle	= factoryHdl.
         end.

      end.

      return factoryHdl.

      catch appError as Progress.Lang.Error :
         delete object appError.
         return ?.
      end catch.

   end method.

   method public void Reset():
   end method.

end class.
