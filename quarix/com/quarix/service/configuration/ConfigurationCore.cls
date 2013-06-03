/*---------------------------------------------------------------------
File        : ConfigurationCore.cls
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

using com.quarix.service.configuration.ConfigurationXml.
using com.quarix.service.configuration.ConfigurationCore.

class com.quarix.service.configuration.ConfigurationCore
   inherits com.quarix.service.configuration.ConfigurationBase
   implements com.quarix.base.iSingleton use-widget-pool:

   &scoped-define section_app		    'Application':u
   &scoped-define section_user		    'User':u
   &scoped-define section_service	    'Service':u

   &scoped-define framework-conf-dir    'com~/quarix~/config':u
   &scoped-define application-conf-file 'config~/config.xml':u

   define private variable xmlConfigImp as ConfigurationXml no-undo.

   define private property xmlConfig    as ConfigurationXml no-undo
      get:
         if not valid-object(xmlConfigImp) then
            xmlConfigImp = cast(GetInstance('com.quarix.service.configuration.ConfigurationXml':u), 'com.quarix.service.configuration.ConfigurationXml':u).
         return xmlConfigImp.
      end get.

   &if keyword-all('static':u) ne ? &then
    define private static variable configurationCore as ConfigurationCore no-undo.

    method public static ConfigurationCore GetInstance():
        if not valid-object(configurationCore) then
            configurationCore = new ConfigurationCore().
        return configurationCore.
    end method.
   &endif

   constructor public ConfigurationCore ():
   end constructor.

   destructor public ConfigurationCore ():
      UnloadInstance(xmlConfigImp).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end destructor.

   method override protected logical LoadEnvironment (input environmentName as character):

      define buffer appBuf		for core_app.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = environmentName
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = environmentName
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = environmentName.

      for each appBuf no-lock
         where appBuf.app_name eq cAppName
            on error undo, throw:

         OpenXmlEnvironment(environmentName).

         return true.

      end.

      return OpenXmlEnvironment(environmentName).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method override public logical SaveEnvironment (input environmentName as character):

      define buffer appBuf		for core_app.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = environmentName
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = environmentName
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = environmentName.

      for each appBuf no-lock
         where appBuf.app_name eq cAppName
            on error undo, throw:

         return super:SaveEnvironment(environmentName).
      end.

      if OpenXmlEnvironment(environmentName) then
         return xmlConfig:SaveEnvironment().

      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method override public void CloseEnvironment (input environmentName as character):
      super:CloseEnvironment(environmentName).
      if not OpenXmlEnvironment(environmentName) then
         return.

      /* already saved by super closeenvironment */
      xmlConfig:SaveOnClose = false.
      xmlConfig:CloseEnvironment().

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.

   method protected override character LoadKey (input keyName as character, input sectionName as character, input objectName as character):
      define variable keyValue   as character   no-undo initial ?.

      if OpenXmlEnvironment(Environment) then
      keyValue = xmlConfig:GetKey(keyName, sectionName, objectName).

      if keyValue eq ? then do:
          case sectionName:
             when {&section_app} then
                keyValue = LoadApplicationKey(keyName, objectName).
             when {&section_user} then
                keyValue = LoadUserKey(keyName, objectName).
             otherwise
                keyValue = LoadConfigKey(keyName, sectionName, objectName).
          end case.
      end.

      return keyValue.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return ?.
      end catch.
   end method.

   method protected override logical SaveKey (input keyName as character, input keyValue as character, input sectionName as character, input objectName as character):

      define buffer appBuf		for core_app.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = Environment
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = Environment
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = Environment.

      for each appBuf no-lock
         where appBuf.app_name eq cAppName
            on error undo, throw:

         case sectionName:
            when {&section_app} then
               return SaveApplicationKey(keyName, keyValue, objectName).
            when {&section_user} then
               return SaveUserKey(keyName, keyValue, objectName).
            otherwise
               return SaveConfigKey(keyName, keyValue, sectionName, objectName).
         end case.
      end.

      if OpenXmlEnvironment(Environment) then
         return xmlConfig:SetKey(keyName, keyValue, sectionName, objectName).

      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method protected override logical RemoveKey (input keyName as character, input sectionName as character, input objectName as character):

      define buffer appBuf		for core_app.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = Environment
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = Environment
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = Environment.

      for each appBuf no-lock
         where appBuf.app_name eq cAppName
            on error undo, throw:

         case sectionName:
            when {&section_app} then
               return RemoveApplicationKey(keyName, objectName).
            when {&section_user} then
               return RemoveUserKey(keyName, objectName).
            otherwise
               return RemoveConfigKey(keyName, sectionName, objectName).
         end case.
      end.

      if OpenXmlEnvironment(Environment) then
         return xmlConfig:DeleteKey(keyName, sectionName, objectName).

      return false.

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
         return false.
      end catch.
   end method.

   method private logical RemoveApplicationKey (input keyName as character, input objectName as character):

      define buffer appBuf		for core_app.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = Environment
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = Environment
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = Environment.

      for each appBuf no-lock
         where appBuf.app_name eq cAppName
            on error undo, throw:

         case keyName:
            when 'Description':u then
               appBuf.app_desc = ?.
            when 'Version':u then
               appBuf.app_version = ?.
            when 'LoginRequired':u then
               appBuf.req_authentication = ?.
            when 'AuthorizationEnabled':u then
               appBuf.req_authorization = ?.
            when 'DefaultPage':u then
               appBuf.page_default = ?.
            when 'LoginPage':u then
               appBuf.page_login = ?.
            when 'LogLevel':u then
               appBuf.log_level = ?.
            otherwise
               return RemoveConfigKey(keyName, {&section_app}, objectName).
         end case.
         return true.
      end.

      return false.
   end method.

   method private logical RemoveConfigKey (input keyName as character, input sectionName as character, input objectName as character):

      define buffer appBuf		for core_app.
      define buffer confBuf		for core_config.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = Environment
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = Environment
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = Environment.

      for each appBuf no-lock
         where appBuf.app_name eq cAppName,
         each confBuf
         exclusive-lock
         where confBuf.app_id       eq appBuf.app_id
           and confBuf.section_name eq sectionName
           and confBuf.object_name  eq objectName
           and confBuf.var_name     eq keyName
              on error undo, throw:
           delete confBuf.
           return true.
      end.

      return false.

   end method.

   method private character LoadApplicationKey (input keyName as character, input objectName as character):

      define buffer appBuf		for core_app.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = Environment
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = Environment
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = Environment.

      for each appBuf no-lock
         where appBuf.app_name eq cAppName
            on error undo, throw:

         case keyName:
           when 'Id':u                   then return appBuf.app_id.
           when 'Description':u          then return appBuf.app_desc.
           when 'Version':u              then return appBuf.app_version.
           when 'LoginRequired':u        then return string(appBuf.req_authentication).
           when 'AuthorizationEnabled':u then return string(appBuf.req_authorization).
           when 'DefaultPage':u          then return appBuf.page_default.
           when 'LoginPage':u            then return appBuf.page_login.
           when 'LogLevel':u             then return string(appBuf.log_level).
           when 'ThemeCode':u            then return appBuf.theme_code.
           otherwise return LoadConfigKey(keyName, {&section_app}, objectName).
         end case.
      end.

      return ?.

   end method.

   method private logical SaveApplicationKey (input keyName as character, keyValue as character, input objectName as character):

      define buffer appBuf		for core_app.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = Environment
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = Environment
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = Environment.

      for each appBuf exclusive-lock
         where appBuf.app_name eq cAppName
            on error undo, throw:

         case keyName:
           when 'Description':u           then appBuf.app_desc = keyValue.
           when 'Version':u               then appBuf.app_version = keyValue.
           when 'LoginRequired':u         then Util:Nvl(keyValue, false).
           when 'AuthorizationEnabled':u  then Util:Nvl(keyValue, false).
           when 'DefaultPage':u           then appBuf.page_default = keyValue.
           when 'LoginPage':u             then appBuf.page_login = keyValue.
           when 'LogLevel':u              then Util:Nvl(keyValue, 0).
           when 'ThemeCode':u             then appBuf.theme_code = keyValue.
           otherwise return SaveConfigKey(keyName, keyValue, {&section_app}, objectName).
         end case.

         return true.

      end.

      return false.

   end method.

   method private character LoadConfigKey
      (keyName as character, sectionName as character, objectName as character):

      define buffer appBuf		for core_app.
      define buffer confBuf		for core_config.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = Environment
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = Environment
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = Environment.

      for each appBuf no-lock
         where appBuf.app_name eq cAppName,
         each confBuf no-lock
         where confBuf.app_id       eq appBuf.app_id
           and confBuf.section_name eq sectionName
           and confBuf.object_name  eq objectName
           and confBuf.var_name     eq keyName:
           return confBuf.var_value.
      end.

      return ?.

   end method.

   method private logical SaveConfigKey
      (keyName as character, keyValue as character,
       sectionName as character, objectName as character):

      define buffer appBuf		for core_app.
      define buffer confBuf		for core_config.
      define buffer appAliasBuf	for core_app_alias.

      define variable cAppName as character no-undo.

      find first appBuf
         where appBuf.app_name = Environment
         no-lock no-error.

      if not available(appBuf)
      then do:
         find first appAliasBuf
            where appAliasBuf.app_name_alias = Environment
            no-lock no-error.

         if available(appAliasBuf)
         then cAppName = appAliasBuf.app_name.
      end.
      else
         cAppName = Environment.

      for each appBuf no-lock
         where appBuf.app_name eq cAppName
            on error undo, throw:

         do transaction
            on error undo, throw:

            for each confBuf exclusive-lock
               where confBuf.app_id       eq appBuf.app_id
                 and confBuf.section_name eq sectionName
                 and confBuf.object_name  eq objectName
                 and confBuf.var_name     eq keyName
                    on error undo, throw:
                 confBuf.var_value = keyValue.

                 return true.
            end.

            create confBuf.

            assign confBuf.app_id       = appBuf.app_id
                   confBuf.section_name = sectionName
                   confBuf.object_name  = objectName
                   confBuf.var_name     = keyName
                   confBuf.var_value    = keyValue.

            return true.
         end.
      end.

      return false.

   end method.

   method private character LoadUserKey (input keyName as character, input objectName as character):
      return ?.
   end method.

   method private logical   RemoveUserKey (input keyName as character, input objectName as character):
      return false.
   end method.

   method private logical   SaveUserKey (input keyName as character, keyValue as character, input objectName as character):
      return false.
   end method.

   method private logical OpenXmlEnvironment (input environmentName as character):

      if valid-object(xmlConfig) then
         return
            xmlConfig:OpenEnvironment(substitute('&1~/&2.xml':u, {&framework-conf-dir}, lower(environmentName))) or
            xmlConfig:OpenEnvironment(substitute('&1~/&2':u, lower(environmentName), {&application-conf-file})).

      return false.

   end method.

   method public void Reset():
      if valid-object(Application) then
         OpenEnvironment(Application:Name).

      catch appError as Progress.Lang.Error :
         ThrowError(input appError).
         delete object appError.
      end catch.
   end method.
end class.


