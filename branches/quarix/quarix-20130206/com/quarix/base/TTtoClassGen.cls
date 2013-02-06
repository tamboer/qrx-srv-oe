

/*------------------------------------------------------------------------
   File        : TTtoClassGen
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : Andriuhan
   Created     : Fri Nov 18 14:46:23 EET 2011
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



class com.quarix.base.TTtoClassGen
    inherits   com.quarix.base.BaseObject use-widget-pool final:


    method public logical GenerateClass (input pcPath as character, input phTT as handle):
        define variable hBuffer    as handle    no-undo.
        define variable hField     as handle    no-undo.
        define variable iNumFields as integer   no-undo.
        define variable cClassPath as character no-undo.
        define variable cClassName as character no-undo.


        if Util:IsEmpty(search(pcPath)) then
            return false.


        cClassPath = substitute ('&1.cls',entry(1,pcPath,'.')).
        cClassName = replace (entry(1,pcPath,'.'),'/','.').


        if not valid-handle(phTT) then
            return false.


        hBuffer = phTT:default-buffer-handle.


        if not valid-handle(hBuffer) then
            return false.


        output to value(cClassPath).
        put unformatted 'using com.quarix.data.meta.*.' skip(1).
        put unformatted substitute('class &1 inherits FilterTable:',cClassName) skip(1).
        put unformatted substitute('&1&2 &3Scope="private static"&4',chr(123),pcPath,chr(38),chr(125)) skip(1).


        put unformatted substitute('define public static property Instance as &1 no-undo',cClassName) skip.
        put unformatted 'public get:' skip.
        put unformatted substitute('if Instance = ? then Instance = new &1().',cClassName) skip.
        put unformatted 'return Instance.' skip.
        put unformatted 'end get.' skip.
        put unformatted 'private set.' skip(1).


        do iNumFields = 1 to hBuffer:num-fields:
            hField = hBuffer:buffer-field (iNumFields).
            if not valid-handle(hField) then
                return false.


            GenerateFieldProperties(hField:name,cClassName,hBuffer:name).
        end.


        put unformatted 'method public override char ToString():' skip.
        put unformatted substitute('return "&1".',hBuffer:name) skip.
        put unformatted 'end method.' skip(1).


        put unformatted 'end class.'.
        return true.


        catch appError as Progress.Lang.Error :
            delete object appError.
        end catch.
        finally:
            output close.
        end finally.
    end method.


    method private logical GenerateFieldProperties (input pcField as character, input pcClass as character, input pcTable as character):


        put unformatted substitute('define public static property &1 as FilterField no-undo',pcField) skip.
        put unformatted 'public get:' skip.
        put unformatted substitute('if &1 = ? then',pcField) skip.
        put unformatted substitute('assign &1 = new FilterField(&2:Instance, temp-table &3:default-buffer-handle:buffer-field("&1"):name).',pcField,pcClass,pcTable) skip.
        put unformatted substitute('return &1.',pcField) skip.
        put unformatted 'end get.' skip.
        put unformatted 'private set.' skip(1).


        return true.


    end method.

end class.
