/*------------------------------------------------------------------------
   File        : BaseObject
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Marian
   Created     : Wed Dec 10 16:00:37 EET 2008
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

using Progress.Lang.Object.

class com.quarix.base.BaseObject use-widget-pool:

   define private variable currentApp  as character no-undo.

   define public property Factory as com.quarix.base.Factory
      get:
         if valid-object(Factory) then
            return Factory.
         &if keyword-all('static':u) ne ? &then
         Factory = com.quarix.base.Factory:GetInstance().
         &else
         define variable obj as Progress.Lang.Object no-undo.

         obj = session:first-object.
         do while valid-object(obj)
             on error undo, throw:
            if type-of(obj, 'com.quarix.base.Factory':u) then
            do:
               Factory = cast(obj, 'com.quarix.base.Factory':u).
               leave.
            end.
            obj = obj:next-sibling.
         end.
         if not valid-object(Factory) then
            Factory = new com.quarix.base.Factory().
         &endif
         return Factory.
      end get.
      private set.

   define public property Util as com.quarix.base.Util
      get:
         if not valid-object(Util) then
            Util = cast(GetInstance('com.quarix.base.Util':u), 'com.quarix.base.Util':u).
         return Util.
      end get.
      private set.

   define public property Application as com.quarix.system.Application
      get:
         if not valid-object(Application) then
            Application = cast(GetInstance('com.quarix.system.Application':u), 'com.quarix.system.Application':u).
         return Application.
      end get.
      private set.

   define public property QuarixVersion as com.quarix.system.Version
      get:
         if not valid-object(QuarixVersion) then
            QuarixVersion = cast(GetInstance('com.quarix.system.Version':u), 'com.quarix.system.Version':u).
         return QuarixVersion.
      end get.
      private set.

   define public property Localization as com.quarix.service.localization.iLocalization
      get:
         if not valid-object(Application) then
            return ?.

         if not valid-object(Localization) or Application:Name ne currentApp then
         do:
            assign
               currentApp = Application:Name
               Localization = cast(Application:GetService('localization':u), 'com.quarix.service.localization.iLocalization':u).
         end.

         return Localization.

      end get.
      private set.

   define public property ErrorManager as com.quarix.service.error.iError
      get:
         if not valid-object(Application) then
            return ?.

         if not valid-object(ErrorManager) or Application:Name ne currentApp then
         do:
            assign
               currentApp   = Application:Name
               ErrorManager = cast(Application:GetService('error':u), 'com.quarix.service.error.iError':u) .
         end.
         return ErrorManager.
      end get.
      private set.

   define public property ContextManager as com.quarix.service.context.iContext
      get:
         if not valid-object(Application) then
            return ?.

         if not valid-object(ContextManager) or Application:Name ne currentApp then
         do:
            assign
               currentApp     = Application:Name
               ContextManager = cast(Application:GetService('context':u), 'com.quarix.service.context.iContext':u) .
         end.

         return ContextManager.

      end get.
      private set.

	constructor BaseObject():

		super().

	end constructor.

   method final public com.quarix.base.Factory GetFactory ():
      return Factory.
   end method.

   method final public Progress.Lang.Object GetInstance (className  as character):

      define variable instanceObj as Progress.Lang.Object no-undo.

      instanceObj = GetInstance(className, true).

      return instanceObj.

   end method.

   method final public Progress.Lang.Object GetInstance (className  as character, logError as logical):
      define variable instanceObj as Progress.Lang.Object no-undo.

      if not valid-object(Factory) then
         return ?.

      instanceObj = Factory:GetInstance(className, this-object).

      if valid-object(instanceObj) then
         return instanceObj.

      if logError then
      do:
         ThrowError(1002, Factory:ErrorMessage, '', '').
         ThrowDebug(1002, substitute('Unable to instantiate class [&1].':u, className), '', '').
      end.

      return ?.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.

   end method.

   method final public void UnloadInstance (instanceObj as Progress.Lang.Object):
      if not valid-object(instanceObj) then
         return.

      if valid-object(Factory) then
         Factory:Unload(instanceObj).
      else
         delete object instanceObj.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.

   end method.

   method final public com.quarix.base.Util GetUtil ():
      return Util.
   end method.

   method final public com.quarix.system.Application GetApplication ():
      return Application.
   end method.

   method final public com.quarix.system.Version GetQuarixVersion ():
      return QuarixVersion.
   end method.

   method final public com.quarix.service.localization.iLocalization GetLocalization ():
      return Localization.
   end method.

   method final public com.quarix.service.context.iContext GetContextManager ():
      return ContextManager.
   end method.

   method final public com.quarix.service.error.iError GetErrorManager ():
      return ErrorManager.
   end method.

   method final public character GetString (input keyName as character):
      if valid-object(Localization) then
         return Localization:LocaleValue(keyName).
      return keyName.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.

   end method.

   method final public void ThrowError
      (input errorCode   as integer,
      input errorText   as character):

      ThrowError (errorCode, errorText, '', '').

   end method.

   method final public void ThrowError
      (input errorCode   as integer,
      input errorText   as character,
      input errorBuffer as character,
      input errorField  as character):

      if valid-object(ErrorManager)
      then ErrorManager:AddError(errorCode, errorText, errorField, errorBuffer).

      return.

      catch appError as Progress.Lang.Error:
         delete object appError.
         return.
      end catch.

   end method.

   method final public void ThrowClientError
      (input errorCode   as integer,
      input errorText   as character):

      ThrowClientError (errorCode, errorText, '', '').

   end method.

   method final public void ThrowClientError
      (input errorCode   as integer,
      input errorText   as character,
      input errorBuffer as character,
      input errorField  as character):

      ThrowClientError (errorCode, ?, errorText, errorBuffer, errorField).

   end method.

   method final public void ThrowClientError
      (input errorCode   as integer,
      input errorType   as integer,
      input errorText   as character,
      input errorBuffer as character,
      input errorField  as character):

      ThrowClientError (errorCode, errorType, errorText, errorBuffer, errorField, ?).

   end method.

   method final public void ThrowClientError
      (input errorCode   as integer,
      input errorType   as integer,
      input errorText   as character,
      input errorBuffer as character,
      input errorField  as character,
      input errorRecord as integer ):

      ThrowClientError (errorCode, errorType, errorText, errorBuffer, errorField, errorRecord, ?).

   end method.

   method final public void ThrowClientError
      (input errorCode   as integer,
       input errorType   as integer,
       input errorText   as character,
       input errorBuffer as character,
       input errorField  as character,
       input errorRecord as integer,
       input errorTag    as character ):

      if valid-object(ErrorManager)
      then ErrorManager:AddClientError(errorCode, errorType, errorText, errorField, errorBuffer, errorRecord,errorTag).

      return.

      catch appError as Progress.Lang.Error:
         delete object appError.
         return.
      end catch.

   end method.

   /*roxanam : add new ThrowError method for the new error handling system*/
   method public final void ThrowError
        (input errorCode as integer,
        input errorText as character,
        input errorCallStack as character):

    define variable errorTrace	as character	no-undo.
    define variable errorMethod	as character	no-undo.
    define variable errorFile	as character	no-undo.
    define variable errorLine	as integer		no-undo.
    define variable cVal		as character	no-undo.

    /*format the call stack to show only the line where the error raised
    chr(10) = NL - can be defined as a property*/
    if not Util:IsEmpty(errorCallStack)
    then do on error undo, throw:

        assign
            errorText	= replace(errorText, "~"", "'")
            errorTrace	= trim(entry(num-entries(errorCallStack, chr(10)), errorCallStack, chr(10)))
            errorMethod	= trim(entry(1, errorTrace, " "))
            errorFile	= trim(entry(2, errorTrace, " "))
            cVal		= trim(entry(3, errorTrace, " "))
            errorLine	= integer(entry(num-entries(cVal, ":"), cVal, ":"))
            no-error.

		if Util:IsError()
		then leave.

		if valid-object(ErrorManager)
		then ErrorManager:AddError(errorCode, errorText, errorFile, errorMethod, errorLine).
    end.
    else
        ThrowDebug(errorCode, errorText, '', '').

    return.

	catch appError as Progress.Lang.Error :
		delete object appError.
		return.
	end catch.

   end method.

   method public final void ThrowError(input appError as Progress.Lang.Error):

   	if not valid-object(appError)
   	then return.

   	ThrowError(
   		input appError:GetMessageNum(1),
   		input appError:GetMessage(1),
   		appError:CallStack
   		).

   	return.

   	catch appError as Progress.Lang.Error :
		delete object appError.
		return.
	end catch.

   end method.

   method final public void ThrowWarning
      (input errorCode   as integer,
      input errorText   as character):

      ThrowWarning (errorCode, errorText, '', '').

   end method.

   method final public void ThrowWarning
      (input errorCode   as integer,
      input errorText   as character,
      input errorBuffer as character,
      input errorField  as character):

      if valid-object(ErrorManager)
      then ErrorManager:AddWarning(errorCode, errorText, errorField, errorBuffer).

      return.

      catch appError as Progress.Lang.Error :
		delete object appError.
		return.
      end catch.

   end method.

   method final public void ThrowInfo
      (input errorCode   as integer,
      input errorText   as character):

      ThrowInfo (errorCode, errorText, '', '').

   end method.

   method final public void ThrowInfo
      (input errorCode   as integer,
      input errorText   as character,
      input errorBuffer as character,
      input errorField  as character):

      if valid-object(ErrorManager)
      then ErrorManager:AddInfo(errorCode, errorText, errorField, errorBuffer).

      return.

      catch appError as Progress.Lang.Error :
		delete object appError.
		return.
      end catch.

   end method.

   method final public void ThrowDebug
      (input errorCode   as integer,
      input errorText   as character):

      ThrowDebug (errorCode, errorText, '', '').

   end method.

   method final public void ThrowDebug
      (input errorCode   as integer,
      input errorText   as character,
      input errorBuffer as character,
      input errorField  as character):

      define variable thisClass    as Progress.Lang.Class no-undo.

      if Util:IsEmpty(errorBuffer) then
         assign
            thisClass = GetClass()
            errorBuffer = thisClass:TypeName.

      if valid-object(ErrorManager) then
         ErrorManager:AddDebug(errorCode, errorText, errorField, errorBuffer).

      return.

      catch appError as Progress.Lang.Error :
		delete object appError.
		return.
      end catch.

   end method.

   method final public void UnloadAll (collection as com.quarix.base.Collection):
      define variable objInstance as Object no-undo.

      if not valid-object(collection) or collection:NumElements eq 0 then
         return.

      do while collection:Pop(output objInstance)
          on error undo, throw:
         UnloadInstance(objInstance).
      end.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.

   end method.

   destructor BaseObject():

		if valid-object(Factory)
		then Factory:UnloadInstances(this-object).

   end destructor.

end class.
