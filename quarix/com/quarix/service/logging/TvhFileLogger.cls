
 /*------------------------------------------------------------------------
    File        : TvhFileLogger
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Fri Oct 14 10:52:26 EEST 2011
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

using Progress.Lang.*.
using com.quarix.service.logging.FileLogger.
&scoped-define log-config-section       'Service':u
&scoped-define log-config-object        'FileLogger':u
&scoped-define log-config-key-file-win  'LogFile_Win':u
&scoped-define log-config-key-file-unix 'LogFile_Unix':u
&scoped-define log-config-key-format    'LogFormat':u
&scoped-define log-config-key-append    'LogAppend':u


class com.quarix.service.logging.TvhFileLogger inherits FileLogger:

	method override public void LoadConfiguration( input configService as com.quarix.service.configuration.iConfiguration ):

	   if Util:IsEmpty(FileName) and valid-object(configService) then do:
	     if opsys eq 'WIN32' then
	       FileName  = Util:Nvl(configService:GetKey({&log-config-key-file-win}, {&log-config-section}, {&log-config-object}), FileName).

	     if opsys eq 'UNIX' then
           FileName  = Util:Nvl(configService:GetKey({&log-config-key-file-unix}, {&log-config-section}, {&log-config-object}), FileName).

         assign
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

end class.
