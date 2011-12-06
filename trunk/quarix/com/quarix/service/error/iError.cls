/*---------------------------------------------------------------------
File        : iError.cls
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

interface  com.quarix.service.error.iError:

    method public integer GetNumErrors            ().
    method public integer GetNumErrors            (errorObject as character).

    method public integer GetNumMessages          ().
    method public integer GetNumMessages          (errorObject as character).

    method public integer GetNumClientErrors      ().
    method public integer GetNumClientErrors      (errorObject as character).

    method public integer GetNumWarnings          ().
    method public integer GetNumWarnings          (errorObject as character).

    method public integer GetNumInfoMessages	  ().
    method public integer GetNumInfoMessages      (errorObject as character).

    method public integer GetNumDebugMessages	  ().
    method public integer GetNumDebugMessages     (errorObject as character).

    method public integer GetNumApplicationErrors ().
    method public integer GetNumApplicationErrors (errorObject as character).

    method public integer GetNumObjectErrors      (input errorLevel as integer, errorObject as character).

    method public integer GetLogLevel             ().
    method public logical SetLogLevel             (input logLevel  as integer).

    method public integer ErrorLevel              ().
    method public integer ClientLevel             ().
    method public integer WarningLevel            ().
    method public integer InfoLevel               ().
    method public integer DebugLevel              ().
    method public integer ApplicationLevel        ().

    method public void AddApplicationError (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character).


    method public void AddApplicationError (input errorCode as integer, input errorText as character, input errorParam as character).


    method public void AddApplicationError (input errorCode as integer, input errorText as character).


    method public void AddDebug (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character).


    method public void AddDebug (input errorCode as integer, input errorText as character, input errorParam as character).


    method public void AddDebug (input errorCode as integer, input errorText as character).


    method public void AddError (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character).


    method public void AddError (input errorCode as integer, input errorText as character, input errorParam as character).


    method public void AddError (input errorCode as integer, input errorText as character).

    /*roxanam*/
    method public void AddError(input errorCode as integer, input errorText as character, input errorFile as character, input errorMethod as character, input errorLine as integer).

    method public void AddClientError (input errorCode as integer, input errorType as integer, input errorText as character, input errorParam as character, input errorObject as character, input errorRecord as integer, input errorTag as character).

    method public void AddClientError (input errorCode as integer, input errorType as integer, input errorText as character, input errorParam as character, input errorObject as character, input errorRecord as integer).

    method public void AddClientError (input errorCode as integer, input errorType as integer, input errorText as character, input errorParam as character, input errorObject as character).

    method public void AddClientError (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character).

    method public void AddClientError (input errorCode as integer, input errorText as character, input errorParam as character).

    method public void AddClientError (input errorCode as integer, input errorText as character).


    method public void AddInfo (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character).


    method public void AddInfo (input errorCode as integer, input errorText as character, input errorParam as character).


    method public void AddInfo (input errorCode as integer, input errorText as character).


    method public void AddWarning (input errorCode as integer, input errorText as character, input errorParam as character).


    method public void AddWarning (input errorCode as integer, input errorText as character, input errorParam as character, input errorObject as character).


    method public void AddWarning (input errorCode as integer, input errorText as character).


    method public void Purge ().

    method public void Purge (input errorLevel as integer).

    method public void Purge (input errorObject as character).

    method public void Purge (input errorLevel as integer, input errorObject as character).

    method public void LogMessages (input logService as com.quarix.service.logging.iLogger).

    method public void LogMessages (input logService as com.quarix.service.logging.iLogger, input errorObject as character).

    method public void LogErrors   (input logService as com.quarix.service.logging.iLogger).

    method public void LogErrors   (input logService as com.quarix.service.logging.iLogger, input errorObject as character).

    method public void LogClientErrors (input logService as com.quarix.service.logging.iLogger).

    method public void LogClientErrors (input logService as com.quarix.service.logging.iLogger, input errorObject as character).

    method public void LogWarnings (input logService as com.quarix.service.logging.iLogger).

    method public void LogWarnings (input logService as com.quarix.service.logging.iLogger, input errorObject as character).

    method public void LogDebugMessages (input logService as com.quarix.service.logging.iLogger).

    method public void LogDebugMessages (input logService as com.quarix.service.logging.iLogger, input errorObject as character).

    method public void LogInfoMessages  (input logService as com.quarix.service.logging.iLogger).

    method public void LogInfoMessages  (input logService as com.quarix.service.logging.iLogger, input errorObject as character).

    method public void LogApplicationErrors (input logService as com.quarix.service.logging.iLogger).

    method public void LogApplicationErrors (input logService as com.quarix.service.logging.iLogger, input errorObject as character).

    method public void Log (input logService as com.quarix.service.logging.iLogger, input logLevel as integer).

    method public void Log (input logService as com.quarix.service.logging.iLogger, input logLevel as integer, input errorObject as character).

    method public character GetError (input errorIndex as integer, input errorLevel as integer, errorObject as character).

    method public character GetError (input errorIndex as integer, input errorLevel as integer).

    method public character GetError (input errorIndex as integer).

    method public character GetApplicationError (input errorIndex as integer, errorObject as character).

    method public character GetApplicationError (input errorIndex as integer).

    method public void DumpToXml().

end interface.


