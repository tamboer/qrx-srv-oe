 /*------------------------------------------------------------------------
    File        : Version
    Purpose     :
    Syntax      :
    Description : Quarix Framework Version
    Author(s)   : Marian
    Created     : Wed Oct 28 09:25:23 EET 2009
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

&global-define number.minor   'version.minor':u
&global-define number.major   'version.major':u
&global-define build.date     'version.lastbuild':u

using com.quarix.system.Version.

class com.quarix.system.Version implements com.quarix.base.iSingleton use-widget-pool final :

	/*------------------------------------------------------------------------------
			Purpose:
			Notes:
	------------------------------------------------------------------------------*/
	define public property Major         as decimal   no-undo
	   get.
	   private set.

	define public property Minor         as integer   no-undo
	   get.
	   private set.

	define public property BuildDate     as date      no-undo
	   get.
	   private set.

	define public property Version       as character no-undo
	   get:
	      return replace(substitute('&1.&2':u, Major, Minor), session:numeric-decimal-point, '.':u).
	   end get.

	define public property FullVersion   as character no-undo
	   get:
	      return substitute('&1 &2':u, Version, iso-date(BuildDate)).
	   end get.

	define private stream _stIn.

	&if keyword-all('static':u) ne ? &then
    define private static variable ver as Version no-undo.

    method public static Version GetInstance():
        if not valid-object(ver) then
            ver = new Version().
        return ver.
    end method.
   &endif

	constructor public  Version (  ):
	   define variable varName    as character no-undo.
	   define variable varValue   as character no-undo.

	   file-information:file-name = 'com/quarix/version.properties':u.
       if file-information:file-type ne ? and
          index(file-information:file-type, 'F':u) gt 0 then do:
          input stream _stIn from value(file-information:full-pathname).
          repeat:
             import stream _stIn delimiter '=':u varName varValue no-error.
             case varName:
                when {&number.major} then
                   Major = decimal(replace(varValue, '.':u, session:numeric-decimal-point)) no-error.
                when {&number.minor} then
                   Minor = integer(varValue) no-error.
                /* use iso date format yyyy_mm_dd */
                when {&build.date} then
                   BuildDate = date(integer(entry(2, varValue, '_':u)),
                                    integer(entry(3, varValue, '_':u)),
                                    integer(entry(1, varValue, '_':u)))  no-error.
             end case.
          end.
          input stream _stIn close.
       end.
	end constructor.


    method public void Reset ():
    end method.

end class.
