/*---------------------------------------------------------------------
File        : FileLogger.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Dragos
Created     : generated 11/12/08
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
&scoped-define log-config-section     'Service':u
&scoped-define log-config-object      'FileLogger':u
&scoped-define log-config-key-file    'LogFile':u
&scoped-define log-config-key-format  'LogFormat':u
&scoped-define log-config-key-append  'LogAppend':u

using com.quarix.system.FileSystem.

class com.quarix.service.logging.FileLogger
   inherits com.quarix.service.logging.BaseLogger
   implements com.quarix.service.configuration.iConfigurable use-widget-pool:

   define public property FileName   as character  no-undo
      get.
      set (newValue as character):
         if newValue ne FileName then do:
            FileName = newValue.
            if IsOpen() then
               ReOpenLog().
         end.
      end set.

   define public property LogFormat  as character  no-undo   initial '[&1] [&2] [&3] &4 [&5] - [&6 ~/ &7]':u
      get.
      set.

   define public property ExtendedLogFormat as character no-undo initial '[&1] [&2] [&3] &4 [&5] - [&6 ~/ &7 ~/ &8]':u
      get.
      set.

   define public property AppendTo   as logical    no-undo   initial true
      get.
      set (newValue as logical):
         if newValue ne AppendTo then do:
            AppendTo = newValue.
            if IsOpen() then
               ReOpenLog().
         end.
      end set.

   define private stream   strLog.

   define private variable appName        as character  no-undo.
   define private variable fileSystem_    as FileSystem no-undo.
   define private property FileSystem     as FileSystem no-undo
      get:
         if not valid-object(fileSystem_) then
            fileSystem_    = cast(GetInstance('com.quarix.system.FileSystem':u),
                                  'com.quarix.system.FileSystem':u).
         return fileSystem_.
      end get.

   constructor public FileLogger ():
   end constructor.

   constructor public FileLogger (logFile as character):
      this-object(logFile, ?).
   end constructor.

   constructor public FileLogger (logFile as character, formatString as character):
      this-object(logFile, logFormat, true).
   end constructor.

   constructor public FileLogger (logFile as character, formatString as character, addTo as logical):
      assign
         FileName   = logFile
         LogFormat  = formatString
         AppendTo   = addTo.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end constructor.


   destructor FileLogger():
      UnloadInstance(fileSystem_).

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end.

   method public void LoadConfiguration(input configService as com.quarix.service.configuration.iConfiguration) :
      /* if not already configured load settings from configuration service provided */
      if Util:IsEmpty(FileName) and valid-object(configService) then do:
         assign
            FileName  = Util:Nvl(configService:GetKey({&log-config-key-file}, {&log-config-section}, {&log-config-object}), FileName)
            LogFormat = Util:Nvl(configService:GetKey({&log-config-key-format}, {&log-config-section}, {&log-config-object}), LogFormat)
            AppendTo  = Util:Nvl(configService:GetKey({&log-config-key-append}, {&log-config-section}, {&log-config-object}), AppendTo).
         if IsOpen() then
            ReOpenLog().
      end.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method override public logical LogError(input errorLevel as integer, input  errorCode as integer,
      input errorText   as character, input errorParam as character,
      input errorObject as character, input errorTime  as datetime ):

      if not IsOpen then
         return false.

      put stream strLog unformatted
         trim(substitute(LogFormat, errorTime, appName, errorLevel, errorText, errorCode,
         Util:Nvl(errorObject, '':u), Util:Nvl(errorParam, '':u))).
      put stream strLog skip.
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   /*roxanam : for the new error template*/
   method override public logical LogError(input errorLevel as integer, input errorCode as integer,
      input errorText as character, input errorFile as character, input errorMethod as character,
      input errorLine as integer, input errorTime as datetime):

      if not IsOpen then
         return false.

      put stream strLog unformatted
         substitute(ExtendedLogFormat, errorTime, appName, errorLevel, errorText, errorCode,
         Util:Nvl(errorFile, '':u), Util:Nvl(errorMethod, '':u), Util:Nvl(string(errorLine), '':u)).

      put stream strLog control null.
      put stream strLog skip.
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method override public logical OpenLog():
      if not valid-object(FileSystem) then
      return false.

      if Util:IsEmpty(FileName) then
         FileName = Util:GetLogDirectory() + '/quarix.log'.
      else do:
         if num-entries(FileName,'/') eq 1 and
            num-entries(FileName,chr(92)) eq 1 then
         FileName = Util:GetLogDirectory() + '/' + FileName.
      end.

      if not FileSystem:IsFile(FileName) then do:
         if not FileSystem:FileCreate(FileName)
         then return false.
      end.

      if not IsOpen and
         FileSystem:CanWrite(FileName) then
      do:
         if AppendTo then do:
            FileName = FileSystem:GetFullPath(FileName).
            output stream strLog to value(FileName) append.
         end.
         else do:
            FileName = substitute('&1~/&2':u, FileSystem:GetFullPath(FileSystem:GetPath(FileName)), FileSystem:GetName(FileName)).
            output stream strLog to value(FileName).
         end.
         IsOpen = true.
         if valid-object(Application) then
            appName = Application:Name.
      end.
      return IsOpen.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method override public logical CloseLog():

      if IsOpen then
         output stream strLog close.

      IsOpen = false.
      return true.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

end class.


