/*---------------------------------------------------------------------
File        : Application.cls
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

using com.quarix.base.*.
using com.quarix.service.configuration.*.
using com.quarix.service.session.*.
using com.quarix.system.*.
using Progress.Lang.Object.

&scoped-define config-section-app     'Application':u
&scoped-define config-section-service 'Service':u

class com.quarix.system.Application
   inherits BaseObject
   implements iSingleton, iConfigurable use-widget-pool:

    define public property ID                   as character  no-undo
       get.
       private set.

    define public property AuthorizationEnabled as logical    no-undo
       get.
       private set.

    define public property Configuration        as iConfiguration
       get:
          return GetConfiguration().
       end get.

    define public property AppSession           as iSession
       get:
          return GetSession().
       end get.

    define public property CurrentUser          as ApplicationUser
       get.
       private set.

    define public property Locale               as character  no-undo
       get.
       private set.

    define public property DateFormat           as character  no-undo
       get.
       private set.

    define public property LogicalFormat        as character  no-undo
       get.
       private set.

    define public property NumericFormat        as character  no-undo
       get.
       private set.


    define public property LoginPage            as character  no-undo
       get.
       private set.

    define public property DefaultPage          as character  no-undo
       get.
       private set.

    define public property LoginRequired        as logical    no-undo
       get.
       private set.

    define public property LogLevel             as integer    no-undo
       get.
       private set.

    define public property Name                 as character  no-undo
       get.
       set (appName as character):
          if appName eq Name then
             return.
          purgeServices().
          if loadConfiguration(defaultConfigurationService(), appName) then
             Name = appName.
          else
             Name = ?.
       end set.

    define public property Theme                as character  no-undo
       get.
       private set.

    define public property Version              as character  no-undo
       get.
       private set.

    define private temp-table ttApp no-undo
       field appName          as character
       field appId            as character
       field appVer           as character
       field appTheme         as character
       field appLocale        as character
       field appFormatLogic   as character
       field appFormatNumeric as character
       field appFormatDate    as character
       field appDefaultPage   as character
       field appLoginPage     as character
       field appLoginEnable   as logical
       field appAuthEnable    as logical
       field appLogLevel      as integer
       index PK_ttApp         is primary unique appName.


    define private temp-table ttService no-undo
       field appId          as character
       field serviceName    as character
       field className      as character
       field classObj       as Object
       field isConfigurable as logical
       field isConfigured   as logical
       index PK_ttService is primary unique appId serviceName.

    define private stream streamSrv.

    constructor public Application ():
    end constructor.

    destructor public Application ():
       define buffer ttService for ttService.

       if valid-object(CurrentUser) then
          delete object CurrentUser.

       for each ttService
           on error undo, throw:
          UnloadInstance(ttService.classObj).
       end.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
       end catch.
    end destructor.

    method public final iConfiguration GetConfiguration():
       define variable configManager as iConfiguration no-undo.
       configManager = cast(GetService('configuration':u), 'com.quarix.service.configuration.iConfiguration':u).
       if valid-object(configManager) then
          configManager:OpenEnvironment(Name).
       return configManager.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
       end catch.
    end method.

    method public final iSession GetSession():
       define variable sessionManager as iSession no-undo.
       sessionManager = cast(GetService('session':u), 'com.quarix.service.session.iSession':u).
       return sessionManager.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
       end catch.
    end method.

    method public Object GetService (input serviceName as character):
       define variable configObj    as iConfigurable no-undo.
       define variable serviceClass as character     no-undo.
       define variable serviceObj   as Object        no-undo.

       define buffer ttService   for ttService.

       if not can-find(first ttService where ttService.appId eq ID
                                         and ttService.serviceName eq serviceName) then do:
          if serviceName ne 'configuration':u and valid-object(Configuration) then
             serviceClass = Configuration:GetKey(serviceName, {&config-section-app}, {&config-section-service}).

          create ttService.
          assign
             ttService.className   = Util:Nvl(serviceClass, substitute('com.quarix.service.&1.&2Core':u, serviceName, Util:FirstUpper(serviceName)))
             ttService.appId       = ID
             ttService.serviceName = serviceName.
       end.

       for each ttService
          where ttService.appId eq ID
            and ttService.serviceName eq serviceName
             on error undo, throw:
          if not valid-object(ttService.classObj) then do:
             ttService.classObj = GetInstance(ttService.className, lookup(serviceName, 'configuration,error':u) eq 0).
             if valid-object(ttService.classObj) then do:
                /* if core configuration service and external configuration manager used, make the switch here */
                if serviceName eq 'configuration':u and
                   type-of(ttService.classObj, 'com.quarix.service.configuration.iConfiguration':u) and
                   cast(ttService.classObj, 'com.quarix.service.configuration.iConfiguration':u):OpenEnvironment(Name) then do:

                   serviceClass = cast(ttService.classObj, 'com.quarix.service.configuration.iConfiguration':u):GetKey(serviceName, {&config-section-app}, {&config-section-service}).
                   /* if invalid or core configuration manager used leave the default in place */
                   if Util:Nvl(serviceClass, substitute('com.quarix.service.&1.&2Core':u, serviceName, Util:FirstUpper(serviceName))) ne ttService.className then do:
                      serviceObj = GetInstance(serviceClass, false).
                      if valid-object(serviceObj) then do:
                         UnloadInstance(ttService.classObj).
                         ttService.classObj = serviceObj.
                      end.
                   end.
                end.

                if type-of(ttService.classObj, 'com.quarix.service.configuration.iConfigurable':u) then
                   assign
                      ttService.isConfigurable = true
                      ttService.isConfigured   = false.
             end.
          end.

          /* load configuration for services which are configurable */
          if valid-object(ttService.classObj) then do:
             if ttService.isConfigurable and
                not ttService.isConfigured and
                valid-object(Configuration) then do:
                configObj = cast(ttService.classObj, 'com.quarix.service.configuration.iConfigurable':u).
                configObj:LoadConfiguration(Configuration).
                ttService.isConfigured = true.
             end.
             if type-of(ttService.classObj, 'com.quarix.base.iSingleton':u) then
                cast(ttService.classObj, 'com.quarix.base.iSingleton':u):Reset().
          end.

          return ttService.classObj.
       end.

       ThrowError(300, 'Application service not available':u, '':u, serviceName).
       return ?.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
       end catch.
    end method.

    method public void LoadConfiguration (input configService as com.quarix.service.configuration.iConfiguration):
       loadConfiguration (configService, Name).
    end method.

    method private logical loadConfiguration (input configService as com.quarix.service.configuration.iConfiguration, appName as character):
       define buffer ttApp           for ttApp.
       define buffer ttService       for ttService.
       define buffer bufService      for ttService.
       define variable serviceName   as character no-undo.

       if Util:IsEmpty(appName) then
          return false.


       if appName eq Name then return true.

       ThrowDebug(300, substitute('Application switch from [&1] to [&2]':u, Name, appName), '', '').

       for each ttApp where ttApp.appName eq appName
           on error undo, throw:
          assign
             ID                   = ttApp.appId
             Version              = ttApp.appVer
             Theme                = ttApp.appTheme
             Locale               = ttApp.appLocale
             DateFormat           = ttApp.appFormatDate
             LogicalFormat        = ttApp.appFormatLogic
             NumericFormat        = ttApp.appFormatNumeric
             LoginPage            = ttApp.appLoginPage
             DefaultPage          = ttApp.appDefaultPage
             AuthorizationEnabled = ttApp.appAuthEnable
             LoginRequired        = ttApp.appLoginEnable
             LogLevel             = ttApp.appLogLevel.

          /* reset services configuration if application switch */
          for each ttService
             where ttService.appId eq ID
               and ttService.isConfigurable
                on error undo, throw:
                ttService.isConfigured = false.
          end.
          return true.
       end.

       if valid-object(configService) and configService:OpenEnvironment(appName) then do:
          create ttApp.
          assign
             ttApp.appVer           = Util:Nvl(configService:GetKey('Version':u, {&config-section-app}), '1.0':u)
             ttApp.appTheme         = lower(configService:GetKey('Theme':u, {&config-section-app}))
             ttApp.appLocale        = lower(configService:GetKey('Locale':u, {&config-section-app}))
             ttApp.appFormatDate    = lower(configService:GetKey('DateFormat':u, {&config-section-app}))
             ttApp.appFormatLogic   = configService:GetKey('LogicalFormat':u, {&config-section-app})
             ttApp.appFormatNumeric = lower(configService:GetKey('NumericFormat':u, {&config-section-app}))
             ttApp.appLoginEnable   = Util:Nvl(configService:GetKey('LoginRequired':u, {&config-section-app}), false)
             ttApp.appAuthEnable    = Util:Nvl(configService:GetKey('AuthorizationEnabled':u, {&config-section-app}), false)
             ttApp.appLoginPage     = configService:GetKey('LoginPage':u, {&config-section-app})
             ttApp.appDefaultPage   = configService:GetKey('DefaultPage':u, {&config-section-app})
             ttApp.appLogLevel      = Util:Nvl(configService:GetKey('LogLevel':u, {&config-section-app}), 0)
             ttApp.appId            = configService:GetKey('ID':u, {&config-section-app})
             ttApp.appName          = appName.

          return loadConfiguration(configService, appName).
       end.

       return false.
    end method.

    method public logical Login (input newUser as com.quarix.system.ApplicationUser, userPasswd as character):
       define variable authService as com.quarix.service.authentication.iAuthentication no-undo.

       if not valid-object(newUser) then return false.

       authService = cast(GetService('authentication':u), 'com.quarix.service.authentication.iAuthentication':u).
       if valid-object(authService) and authService:Login(newUser, userPasswd) then do:
          CurrentUser = newUser.
          return true.
       end.
       return false.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
       end catch.
    end method.

    method public void Logout ():
       EndSession().

       if valid-object(CurrentUser) then
          delete object CurrentUser.
       if valid-object(ContextManager) then
          ContextManager:Purge().

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
       end catch.
    end method.

    method public void Reset():
    end method.

    method public character GetConfigurationKey (keyName as character, sectionName as character, objectName as character):

       if valid-object(Configuration) and Configuration:OpenEnvironment(Name) then
          return Configuration:GetKey(keyName, sectionName, objectName).
       return ?.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
       end catch.
    end method.

    method public character GetConfigurationKey (keyName as character, objectName as character):
       return GetConfigurationKey(keyName, {&config-section-app}, objectName).
    end method.

    method public character GetConfigurationKey (keyName as character):
       return GetConfigurationKey(keyName, {&config-section-app}, '':u).
    end method.

    method public void StartSession(sessionId as character, sessionTimeOut as integer):
      if Util:IsEmpty(sessionId) then
         return.
      if valid-object(AppSession) then do
         on error undo, throw:
         ThrowDebug (100, substitute('Session start [&1].', sessionId)).
         AppSession:Start(sessionId, sessionTimeOut).
         ThrowDebug (100, substitute('Session load context [&1].', valid-object(ContextManager))).
         ContextManager:LoadContext(AppSession).
      end.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
    end method.

    method public void SaveSession():
       if valid-object (ContextManager) then
          ContextManager:SaveContext(AppSession).

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
       end catch.
    end method.

    method public void EndSession():
       EndSession(?).
    end method.

   method public void EndSession(sessionId as character):
      if valid-object(AppSession) then
      do:
         if sessionId eq ? then
            AppSession:Destroy(AppSession:GetID()).
         else
            AppSession:Destroy(sessionId).
      end.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method private void purgeServices ():
      for each ttService
         where ttService.appId eq ID
            on error undo, throw:
         UnloadInstance(ttService.classObj).
         delete ttService.
      end.
   end method.

   method private iConfiguration defaultConfigurationService ():
      return cast(GetInstance('com.quarix.service.configuration.ConfigurationCore':u, false),
                  'com.quarix.service.configuration.iConfiguration':u).
   end method.
end class.


