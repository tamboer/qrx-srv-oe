 /*------------------------------------------------------------------------
    File        : Controller
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marian
    Created     : Mon Jan 05 13:41:01 EET 2009
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

using com.quarix.web.*.
using com.quarix.service.authentication.*.
using com.quarix.service.session.*.
using com.quarix.service.context.*.
using com.quarix.service.logging.*.
using com.quarix.system.*.
using com.quarix.data.parser.*.

&global-define res-sess-id             'session_id':u
&global-define res-sess-app-name       'app_name':u
&global-define res-sess-app-login      'full_login_path':u
&global-define res-sess-app-user       'user_name':u
&global-define res-sess-app-theme      'user_theme':u

&global-define res-sess-loc-code       'user_locale':u
&global-define res-sess-loc-lang       'locale_lang':u
&global-define res-sess-loc-country    'locale_country':u
&global-define res-sess-loc-date-fmt   'locale_date_format':u
&global-define res-sess-loc-bool-fmt   'locale_logic_format':u
&global-define res-sess-loc-num-fmt    'locale_num_format':u
&global-define res-sess-loc-num-dec    'locale_num_dec':u
&global-define res-sess-loc-num-sep    'locale_num_sep':u

&global-define res-sess-start          'session_creation_time':u
&global-define res-sess-access         'session_last_accessed_time':u
&global-define res-sess-max-inactivity 'session_max_inactive_interval':u
&global-define res-sess-authenticated  'session_is_authenticated':u

&global-define res-srv-error-msg       'X-Quarix-Server-Error-Msg':u
&global-define res-var-user-name       'User':u
&global-define res-var-user-pass       'Password':u

using com.quarix.system.Controller.

class com.quarix.system.Controller
   inherits com.quarix.base.BaseObject
   implements com.quarix.base.iSingleton use-widget-pool final:

    define private temp-table ttDateFormat no-undo
        field dDate         as date
        field dDateTime     as datetime
        field dDateTimeTz   as datetime-tz
        .

   define private property Request         as Request     no-undo
      get:
         if not valid-object(Request) then
            Request = cast(GetInstance('com.quarix.web.Request':u), 'Request':u).
         return Request.
      end get.
      set.

   define private property Response        as Response     no-undo
      get:
         if not valid-object(Response) then
            Response = cast(GetInstance('com.quarix.web.Response':u), 'Response':u).
         return Response.
      end get.
      set.


   &if keyword-all('static':u) ne ? &then
   define private static variable ablController   as Controller no-undo.

   constructor private Controller():

   end constructor.

   method public static Controller GetInstance():
      if not valid-object(ablController) then
         ablController = new Controller().
      return ablController.
   end method.

   &else

   constructor public Controller():
      do on error undo, return error:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.

   end constructor.
   &endif

   destructor public Controller ():
      if valid-object(Factory) then
         delete object Factory.
   end destructor.

   method public void HandleRequest
      (input table-handle    httRequest,
       input pstrRequest     as memptr,
       input table-handle    httResponse,
       input pstrResponse    as memptr):

      define variable appAuthentication    as iAuthentication      no-undo.
      define variable appUser              as ApplicationUser      no-undo.

      define variable sessionId            as character            no-undo.
      define variable oldSessionId         as character            no-undo.
      define variable prevAppl             as character            no-undo.
      define variable retValue             as character            no-undo.
      define variable reqModule            as character            no-undo.
      define variable reqMethod            as character            no-undo.

      Reset().

      if not valid-object(Request) or not valid-object(Response) then do:
         ThrowError(100, 'core_err_no_web_handle':u, '', '').
         return.
      end.


      Request:Initialize(table-handle httRequest by-reference, pstrRequest).
      Response:Initialize(table-handle httResponse by-reference, pstrResponse).

/*      LogData(httRequest,pstrRequest).*/

      assign
         prevAppl  = Application:Name
         reqModule = Request:ModuleName
         reqMethod = Request:ModuleMethod.

      if Util:IsEmpty(Request:GetCgi({&res-sess-id}))
      then sessionId = guid.
      else sessionId = Request:GetCgi({&res-sess-id}) + '.' + guid.

      if prevAppl ne Request:ApplicationName then do:
         ThrowDebug (100, substitute('Application switch: [&1]->[&2].', prevAppl, Request:ApplicationName)).
         Application:Name = Request:ApplicationName.
      end.

      /* if new session started clear the old one, expired session */
      if Util:IsEmpty(Request:GetCgi({&res-sess-access})) then do:
         /* clean-up old session, expired */
         retValue = Request:GetCookie({&res-sess-id}).
         if not Util:IsEmpty(retValue) then
            Application:EndSession(retValue).
         Response:SetCookie({&res-sess-id}, sessionId).
      end.

      /* start the session and load it into the context */
      Application:StartSession(sessionId, Util:Nvl(Request:GetCgi({&res-sess-max-inactivity}), 1800)).
      ThrowDebug (100, substitute('Request [&1].[&2] made on [&3] - Process ID [&4].', reqModule, reqMethod, Application:Name,string(Util:GetPid()))).

      /* authenticate the user if required and not already authenticated */
      if Application:LoginRequired and valid-object(ContextManager) and
         not ContextManager:GetValue({&res-sess-authenticated}, output retValue) then do:
         if Request:AjaxRequest then do:
            if Request:ModuleName ne Application:LoginPage then
               Response:RedirectURL = substitute('&1~/&2~/':u, Request:ApplicationPath, Application:LoginPage).
            else do:
               /* try to authenticate with given credentials */
               appUser = new ApplicationUser().
               if valid-object(appUser) then do:
                  appUser:Login = Request:GetVariable({&res-var-user-name}).
                  if Application:Login(appUser, Request:GetVariable({&res-var-user-pass})) then do:
                     /* load user settings and save them into the context */
                     appUser:LoadConfiguration(Application:Configuration).
                     ContextManager:SetValue({&res-var-user-name}, appUser:Login).
                     ContextManager:SetValue({&res-sess-authenticated}, 'true':u).
                     ContextManager:SetValue({&res-sess-app-theme}, lower(appUser:Theme)).
                     ContextManager:SetValue({&res-sess-loc-code},  lower(appUser:Locale)).
                     ContextManager:SetValue({&res-sess-loc-date-fmt}, appUser:DateFormat).
                     ContextManager:SetValue({&res-sess-loc-bool-fmt}, appUser:LogicalFormat).
                     ContextManager:SetValue({&res-sess-loc-num-fmt},  appUser:NumericFormat).

                     /* redirect the user to the application main page */
                     Response:RedirectURL = substitute('&1~/&2~/':u, Request:ApplicationPath, Application:DefaultPage).
                     ThrowDebug (100, substitute('Login success, redirect to main page [&1].', Response:RedirectURL)).
                  end.
                  else do:
                     ThrowError(100, 'core_err_auth_invalid':u, '', {&res-var-user-name}).
                  end.
                  delete object appUser.
               end.
               else
                  ThrowError(100, 'core_err_auth_invalid':u, '', {&res-var-user-name}).
            end.
         end.
         else do:
            ThrowDebug (100, substitute('Redirect to login page: [&1]->[&2].', reqModule, Application:LoginPage)).
            assign
               reqModule = Application:LoginPage
               reqMethod = ?.
         end.
      end.

      /* application switch, set info for JAVA side */
      if prevAppl ne Application:Name then do:
         ContextManager:SetValue({&res-sess-app-name}, Application:Name).
         if Application:LoginRequired then
            ContextManager:SetValue({&res-sess-app-login}, substitute('&1~/&2':u, Request:ApplicationPath, Application:LoginPage)).
         else
            ContextManager:RemoveValue({&res-sess-app-login}).
         /* if theme not set at user level */
         if not ContextManager:GetValue({&res-sess-app-theme}, output retValue) then
            ContextManager:SetValue({&res-sess-app-theme}, Application:Theme).
      end.

      /* set localization values                                  */
      if valid-object(Localization) then do:
         ThrowDebug (100, 'Set locale settings.').
         ContextManager:SetValue({&res-sess-loc-code},     Localization:GetLocale()).
         ContextManager:SetValue({&res-sess-loc-lang},     Localization:GetLanguage()).
         ContextManager:SetValue({&res-sess-loc-country},  Localization:GetCountry()).
         ContextManager:SetValue({&res-sess-loc-date-fmt}, Localization:GetDateFormat()).
         ContextManager:SetValue({&res-sess-loc-bool-fmt}, Localization:GetLogicalFormat()).
         ContextManager:SetValue({&res-sess-loc-num-fmt},  Localization:GetNumericFormat()).
      end.

      /* if don't have already a redirect request (login) */
      if ErrorManager:GetNumErrors() eq 0 and Response:RedirectURL eq ? then do:
        /* localization request */
        if reqModule eq 'Localization' then
            callLocalizationMethod().
        else
        /* else run requested module method */
            callWebObjectMethod(reqModule, reqMethod).
      end.

      /* set info each time for now, to be solved by java layer */
      setJavaContext({&res-sess-app-name}).
      setJavaContext({&res-sess-app-login}).
      setJavaContext({&res-sess-app-theme}).
      setJavaContext({&res-sess-loc-code}).
      setJavaContext({&res-sess-loc-lang}).
      setJavaContext({&res-sess-loc-country}).
      setJavaContext({&res-sess-loc-date-fmt}).
      setJavaContext({&res-sess-loc-bool-fmt}).
      setJavaContext({&res-sess-loc-num-fmt}).
      setJavaContext({&res-var-user-name}, {&res-sess-app-user}).

      /* java needs the the numeric-decimal-point and numeric-separator*/
      Response:SetJavaHeader({&res-sess-loc-num-dec},  session:numeric-decimal-point).
      Response:SetJavaHeader({&res-sess-loc-num-sep},  session:numeric-separator).

      /* save context in user session */
      Application:SaveSession().
      /* send back errors if any */
      logErrors().

/*      LogData(httResponse,pstrResponse).*/

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
      finally:
      	if valid-object(Response)
      	then do:
      		Response:Close().

      		if get-size(pstrResponse) eq 0 then
         	Response:ResponseType = Response:RESPONSE_DIRECT.
      	end.

      	if valid-object(Factory) then
        	Factory:Unload().

      end.

   end method.

    method private void LogData (hTT as handle, mOut as memptr):

      define variable hBuf      as handle    no-undo.
      define variable hQry      as handle    no-undo.
      define variable hFldType  as handle    no-undo.
      define variable hFldName  as handle    no-undo.
      define variable hFldValue as handle    no-undo.
      define variable cMsg      as character no-undo.
      define variable lcValue   as longchar  no-undo.

      hBuf = hTT:default-buffer-handle.
      hFldType  = hBuf:buffer-field ('fieldType').
      hFldName  = hBuf:buffer-field ('fieldName').
      hFldValue = hBuf:buffer-field ('fieldValue').

      ThrowDebug(100,hBuf:name).

      create query hQry.
      hQry:set-buffers (hBuf).
      hQry:query-prepare (substitute('for each &1',hBuf:name)).

      hQry:query-open ().
      do while hQry:get-next (no-lock):
          if hFldValue:data-type eq 'blob' then
            copy-lob hFldValue:buffer-value () to lcValue.
          else
            lcValue = hFldValue:buffer-value ().

          cMsg = substitute ('&1 &2 &3',hFldType:buffer-value (),hFldName:buffer-value (),lcValue).

          ThrowDebug(100,cMsg).
      end.

      hQry:query-close ().

      delete object hBuf.
      delete object hQry.
      delete object hFldType.
      delete object hFldName.
      delete object hFldValue.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.

    end method.


	method private Progress.Lang.Object InstantiateWebObject(input objectName as character):

		define variable cObjectName		as character			no-undo.
		define variable reqObject		as Progress.Lang.Object	no-undo.
		define variable cErrorMessage	as character			no-undo.

		if not valid-object(Factory)
		then return ?.

		if not Util:IsEmpty(Application:Name)
		then do:
      		cObjectName = substitute('&1.&2':u, Application:Name, replace(objectName, '~/':u, '.':u)).

      		reqObject = Factory:GetInstance(cObjectName, this-object).

      		if not valid-object(reqObject)
      		then cErrorMessage = Factory:ErrorMessage.
		end.

		if not valid-object(reqObject)
		then do:
			cObjectName = replace(objectName, '~/':u, '.':u).

			reqObject = Factory:GetInstance(cObjectName, this-object).
		end.

		if not valid-object(reqObject)
		then do:
			if not Util:IsEmpty(Application:Name)
			then do:
				ThrowError(1002, cErrorMessage, '', '').

				cObjectName = substitute('&1.&2':u, Application:Name, replace(objectName, '~/':u, '.':u)).

         		ThrowDebug(1002, substitute('Unable to instantiate class [&1].':u, cObjectName), '', '').
         	end.

         	ThrowError(1002, Factory:ErrorMessage, '', '').

         	cObjectName = replace(objectName, '~/':u, '.':u).

         	ThrowDebug(1002, substitute('Unable to instantiate class [&1].':u, cObjectName), '', '').

			return ?.

		end. /* if not valid-object(reqObject) */

		return reqObject.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return ?.
		end catch.

	end method.


   method private logical callWebObjectMethod (objectName as character, methodName as character):

      define variable reqObject		as Progress.Lang.Object			no-undo.
      define variable webObject		as com.quarix.web.iWebObject	no-undo.
      define variable retValue		as logical						no-undo.

      ThrowDebug (100, substitute('Call web object: [&1].[&2].', objectName, methodName)).

      reqObject = InstantiateWebObject(input objectName).

      if not valid-object(reqObject)
      then do:
         ThrowError (100, substitute('Unable to load web object: [&1].', objectName)).

         return false.
      end.

      message
      	'Request for: ' reqObject:ToString() skip
      	'Method name: ' methodName skip.

      webObject = cast(reqObject, 'com.quarix.web.iWebObject':u).

      if valid-object(webObject)
      then do
         on error undo, retry
         on quit  undo, retry
         on stop  undo, retry:

         if retry then do:
            retValue = false.
            leave.
         end.

         retValue = webObject:HandleRequest(methodName, Request, Response).

         if retValue = false and
         	(ErrorManager:GetNumErrors() > 0 or
         	(ErrorManager:GetNumErrors() = 0 and
         	ErrorManager:GetNumClientErrors() = 0))
         then ThrowError (100, substitute('Error occured during request for object: [&1].', objectName)).

         ThrowDebug (100, substitute('Web object return code: [&1].', retValue)).
      end.
      else do:
         ThrowError (100, substitute('Object is not an web object: [&1].', objectName)).

         retValue = false.
      end.

      UnloadInstance(reqObject).

      return retValue.

      catch appError as Progress.Lang.Error :

         UnloadInstance(reqObject).

         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.

   end method.

    method private logical callLocalizationMethod(  ):
        define variable jsonWriter as JsonWriter no-undo.
        define variable mpOut      as memptr     no-undo.
        define variable dateFormat as character  no-undo.
        define variable timeFormat as character  no-undo.
        define variable tzFormat   as character  no-undo.
        define variable bufferHdl  as handle     no-undo.

        bufferHdl = temp-table ttDateFormat:default-buffer-handle.
        if valid-handle(bufferHdl) then
        assign
            dateFormat = lc(bufferHdl:buffer-field ('dDate'):format)
            timeFormat = lc(bufferHdl:buffer-field ('dDateTime'):format)
            tzFormat   = lc(bufferHdl:buffer-field ('dDateTimeTz'):format)
            .

        jsonWriter = cast(GetInstance('com.quarix.data.parser.JsonWriter'),'com.quarix.data.parser.JsonWriter').

        JsonWriter:SetDestination(mpOut).
        JsonWriter:OpenStream().
        JsonWriter:StartElement('').

        /* serialize localization info */
        JsonWriter:Out(substitute ('"dateFormat": "&1"':u, Localization:GetDateFormat())).
        JsonWriter:Out(substitute (', "logicalFormat": "&1"':u, Localization:GetLogicalFormat())).
        JsonWriter:Out(substitute (', "numericalFormat": "&1"':u, Localization:GetNumericFormat())).
        JsonWriter:Out(substitute (', "numericalDecimalPoint": "&1"':u, session:numeric-decimal-point)).
        JsonWriter:Out(substitute (', "numericalSeparator": "&1"':u, session:numeric-separator)).
        JsonWriter:Out(substitute (', "formatDate": "&1"':u, dateFormat)).
        JsonWriter:Out(substitute (', "formatDateTime": "&1"':u, timeFormat)).
        JsonWriter:Out(substitute (', "formatDateTimeTz": "&1"':u, tzFormat)).

        JsonWriter:EndElement('').
        JsonWriter:CloseStream().
        Response:Out(mpOut).
        set-size(mpOut) = 0.

		return true.

		catch appError as Progress.Lang.Error:
            ThrowError(input appError).
            delete object appError.
            return false.
		end catch.
		finally:
        	UnloadInstance(jsonWriter).
        end finally.
	end method.

   method private void logErrors ():
      define variable mpOut      as memptr     no-undo.
      define variable errMsg     as character  no-undo.
      define variable AppLogger as iLogger no-undo.

      if not valid-object(ErrorManager) or
         (ErrorManager:GetNumMessages() eq 0 and ErrorManager:GetNumDebugMessages() eq 0) then
         return.

      AppLogger = cast(Application:GetService('logging':u), 'com.quarix.service.logging.iLogger':u).
      if valid-object(AppLogger) then do:
         AppLogger:OpenLog().
         ErrorManager:Log(AppLogger,Application:LogLevel).
         AppLogger:CloseLog().
      end.

      /* we don't send back the debug messages, if no other messages we leave here */
      if ErrorManager:GetNumErrors() gt 0 then do:

         /* clear all output if errors */
         Response:Empty().

         Response:ResponseType = Response:RESPONSE_DIRECT.
         /* if really errors send http error status code */
         if Request:AjaxRequest eq false then do:
            errMsg = entry(2, ErrorManager:GetError(1)).
            Response:SetHttpHeader({&res-srv-error-msg}, errMsg).
            Response:SetHttpStatusCode(500, errMsg).
         end.
      end.

      ErrorManager:Purge().

      return.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return.
      end catch.

   end method.

   method private void setJavaContext (contextVar as character):
      setJavaContext(contextVar, contextVar).
   end method.

   method private void setJavaContext (contextVar as character, javaVar as character):
      define variable retValue as character no-undo.
      if valid-object(ContextManager) and
         ContextManager:GetValue(contextVar, output retValue) then do:
         Response:SetJavaHeader(javaVar,  retValue).
      end.
   end method.


   method public void Reset():
      /* clean up previous context and error managers */
      if valid-object(ContextManager) then
         ContextManager:Purge().
      if valid-object(ErrorManager) then
         ErrorManager:Purge().

      /* clean up */
      if valid-object(Request) then
         Request:Empty().

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.

   end method.
end class.
