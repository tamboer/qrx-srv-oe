/*---------------------------------------------------------------------
File        : Error.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Unknown
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

&scoped-define err-config-section  'ErrorService':u
using com.quarix.service.error.ErrorCore.

class com.quarix.service.error.ErrorCore
   inherits com.quarix.service.error.ErrorBase
   implements com.quarix.base.iSingleton use-widget-pool:

    {com/quarix/service/error/ttError.i &scope=private}

    /*roxanam */
   define private query qryErr for ttError scrolling.

   &if keyword-all('static':u) ne ? &then
   define private static variable errorCore    as ErrorCore no-undo.

   constructor public ErrorCore():
   end constructor.

   method public static ErrorCore GetInstance():
      if not valid-object(errorCore) then
         errorCore = new ErrorCore().
      return errorCore.
   end method.
   &else
   constructor public ErrorCore():
      do on error undo, return error:
         run com/quarix/base/enforceSingleton.p (this-object).
      end.
      LogLevel = InfoLevel().
   end constructor.
   &endif

   method public override void Purge ():
      define buffer ttError for ttError.

      empty temp-table ttError.
      assign
         NumMessages          = 0
         NumErrors            = 0
         NumWarnings          = 0
         NumInfos             = 0
         NumDebugMessages     = 0
         NumApplicationErrors = 0.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public override void Purge (input errorLevel as integer, input errorObject as character):
   define buffer ttError for ttError.

      if errorObject = '' then
         errorObject = ?.

      for each ttError
         where (errorLevel  eq ? or ttError.errorLevel  ge errorLevel)
         and (errorObject eq ? or ttError.errorObject eq errorObject)
             on error undo, throw:
         case ttError.errorLevel:
            when ErrorLevel() then
               NumErrors   = NumErrors - 1.
            when WarningLevel()  then
               NumWarnings = NumWarnings - 1.
            when InfoLevel()  then
               NumInfos    = NumInfos - 1.
            when ApplicationLevel()   then
               NumApplicationErrors = NumApplicationErrors - 1.
         end case.
         if ttError.errorLevel ne DebugLevel() then
            NumMessages = NumMessages - 1.
         delete ttError.
      end.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method public override void Log
      (input logService  as com.quarix.service.logging.iLogger,
      input errorLevel  as integer,
      input errorObject as character):
      define buffer ttError for ttError.

      define variable lCustom as logical no-undo.

      if errorObject = '' then
         errorObject = ?.

      if valid-object(logService) and logService:OpenLog() then
      do:
         lCustom = type-of(logService,'com.quarix.service.logging.JsonLogger').

         if errorLevel eq ApplicationLevel() then
            for each ttError
               where ttError.errorLevel  eq errorLevel
                and (errorObject eq ? or ttError.errorObject eq errorObject)
                 on error undo, throw:
               if not lCustom then
                   logService:LogError(if Util:IsEmpty(ttError.errorType) then ttError.errorLevel else ttError.errorType,   ttError.errorCode,
                      ttError.errorMessage, ttError.errorParam,
                      ttError.errorObject,  ttError.logTime).
               else
                   logService:LogError(if Util:IsEmpty(ttError.errorType) then ttError.errorLevel else ttError.errorType,   ttError.errorCode,
                      ttError.errorMessage, ttError.errorParam,
                      ttError.errorObject,  ttError.logTime, ttError.errorRecord, ttError.errorTag).
            end.
         else
            for each ttError
               where (errorLevel  eq ? or ttError.errorLevel  le errorLevel)
                 and (errorObject eq ? or ttError.errorObject eq errorObject)
                  on error undo, throw:
                /*roxanam : log error with the new and old template*/
               if (errorObject eq ? and (not Util:IsEmpty(ttError.errorMethod))) then
                   logService:LogError(ttError.errorLevel, ttError.errorCode, ttError.errorMessage,
                        ttError.errorFile, ttError.errorMethod, ttError.errorLine, ttError.logTime).
               else do:
                   if not lCustom then
                       logService:LogError(if Util:IsEmpty(ttError.errorType) then ttError.errorLevel else ttError.errorType,   ttError.errorCode,
                          ttError.errorMessage, ttError.errorParam,
                          ttError.errorObject,  ttError.logTime).
                   else
                       logService:LogError(if Util:IsEmpty(ttError.errorType) then ttError.errorLevel else ttError.errorType,   ttError.errorCode,
                          ttError.errorMessage, ttError.errorParam,
                          ttError.errorObject,  ttError.logTime, ttError.errorRecord, ttError.errorTag).
               end.
            end.
         logService:CloseLog().
      end.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
      end catch.
   end method.

   method protected override logical ErrorRegister (input errorLevel as integer, input errorType as integer, input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character, input errorRecord as integer, input errorTag as character):
      define buffer ttError for ttError.

      if errorLevel ne ?
         and errorCode ne ?
         and (LogLevel = 0 or errorLevel le LogLevel or errorLevel eq ApplicationLevel()) then do:
         create ttError.
         assign
            ttError.logTime        = dtUtil:sysNow()
            ttError.errorLevel     = errorLevel
            ttError.errorType      = errorType
            ttError.errorCode      = errorCode
            ttError.errorMessage   = errorText
            ttError.errorParam     = errorParam
            ttError.errorObject    = errorObject
            ttError.errorRecord    = errorRecord
            ttError.errorTag       = errorTag
            .
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method protected override logical ErrorRegister (input errorLevel as integer, input errorType as integer, input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character, input errorRecord as integer):
        return ErrorRegister (errorLevel, errorType, errorCode, errorText, errorParam, errorObject, errorRecord, ?).
   end method.

   method protected override logical ErrorRegister (input errorLevel as integer, input errorType as integer, input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
        return ErrorRegister (errorLevel, errorType, errorCode, errorText, errorParam, errorObject, ?).
   end method.

   method protected override logical ErrorRegister (input errorLevel as integer, input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character):
        return ErrorRegister (errorLevel, ?, errorCode, errorText, errorParam, errorObject).
   end method.

    /*roxanam: add error information to temp-table (get information from callstack can be proccessed here)*/
   method protected override logical ErrorRegister (input errorLevel as integer, input errorCode as integer, input errorText as character, input errorFile as character, input errorMethod as character, input errorLine as integer):

   define buffer ttError for ttError.

      if errorLevel ne ?
         and errorCode ne ?
         and (LogLevel = 0 or errorLevel le LogLevel or errorLevel eq ApplicationLevel()) then do:
         create ttError.
         assign
            ttError.logTime        = dtutil:sysNow()
            ttError.errorLevel     = errorLevel
            ttError.errorCode      = errorCode
            ttError.errorMessage   = errorText
            ttError.errorFile      = errorFile
            ttError.errorMethod    = errorMethod
            ttError.errorLine      = errorLine
            .
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method override public character GetError (input errorIndex as integer, input errorLevel as integer, errorObject as character):
      define variable errMsg as character no-undo   initial ?.

      if openQuery(errorLevel, errorObject) then do:
         if errorIndex gt 1 then
            query qryErr:reposition-to-row(errorIndex).
         query qryErr:get-next().
         if available(ttError) then do:
            /*roxanam - get error in the old and new fashion*/
            if (errorObject eq ? and (not Util:IsEmpty(ttError.errorMethod))) then
                errMsg = substitute('&1,&2,&3,&4,&5':u, ttError.errorCode, ttError.errorMessage, ttError.errorFile, ttError.errorMethod, ttError.errorLine).
            else
                errMsg = substitute('&1,&2,&3,&4':u, ttError.errorCode, ttError.errorMessage, ttError.errorObject, ttError.errorParam).
         end.
         query qryErr:query-close().
      end.
      return errMsg.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
   end method.

   method override public integer GetNumObjectErrors (input errorLevel as integer, errorObject as character):
      define variable numErr as integer no-undo.

      if openQuery(errorLevel, errorObject) then do:
         numErr = query qryErr:num-results.
         query qryErr:query-close().
      end.
      return numErr.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
   end method.

   method public void Reset ():
      if valid-object(Application) then
         LogLevel = Application:LogLevel.
   end method.

   method private logical openQuery (input errorLevel as integer, errorObject as character):
      if errorLevel eq ? then do:
         if errorObject eq ? then
             query qryErr:query-prepare('preselect each ttError':u).
          else
             query qryErr:query-prepare(substitute('preselect each ttError where errorObject eq &1':u, quoter(errorObject))).
      end.
      else do:
         if errorObject eq ? then
             query qryErr:query-prepare(substitute('preselect each ttError where errorLevel eq &1':u, quoter(errorLevel))).
          else
             query qryErr:query-prepare(substitute('preselect each ttError where errorLevel eq &1 and errorObject eq &2':u,
                                                    quoter(errorLevel), quoter(errorObject))).
      end.
      query qryErr:query-open().
      return true.
   end method.

   method public override handle GetTmpErrorHandle():

	return buffer ttError:table-handle.

   end method.

   method public override void DumpToXml():

		define variable cLogDirectory		as character	no-undo.
    	define variable cLogFileName		as character	no-undo.
    	define variable cFullLogFileName	as character	no-undo.
    	define variable iNum				as integer		no-undo.
    	define variable cFileName			as character	no-undo.
    	define variable httHandle			as handle		no-undo.

		httHandle = buffer ttError:handle.

    	assign
			cLogDirectory	= Util:GetLogDirectory()
			cFileName		= httHandle:name.

		if cFileName = '' or
			cFileName = ? or
			cLogDirectory = '' or
			cLogDirectory = ?
    	then return.

    	assign
			cLogFileName		= cFileName + '.xml':U
    		cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	do while search(cFullLogFileName) <> ?:

    		iNum = iNum + 1.

    		assign
				cLogFileName		= cFileName + '_':U + string(iNum) + '.xml':U
    			cFullLogFileName	= cLogDirectory + '~/':U + cLogFileName.

    	end. /* do while search(cFullLogFileName) = ? */

    	httHandle:write-xml('file':U, cFullLogFileName, yes).

    	return.

    	catch appError as Progress.Lang.Error :
         	delete object appError.
		end catch.

	end method.

end class.


