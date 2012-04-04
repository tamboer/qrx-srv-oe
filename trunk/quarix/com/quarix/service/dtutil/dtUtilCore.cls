
/*------------------------------------------------------------------------
    File        : dtUtilCore
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Wed Jan 25 11:34:19 EET 2012
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
using com.quarix.service.dtutil.*.
using com.quarix.base.*.



class com.quarix.service.dtutil.dtUtilCore
    inherits dtUtilBase
    implements iSingleton use-widget-pool:

    &if keyword-all('static':u) ne ? &then
    define private static variable dtUtilCore as dtUtilCore no-undo.

    constructor private dtUtilCore():
        message 'dtUtilCore'.
    end constructor.

/*    method public static dtUtilCore GetInstance(): */
/*        message 'before ' valid-object(dtUtilCore).*/
/*        if not valid-object(dtUtilCore) then       */
/*            dtUtilCore = new dtUtilCore().         */
/*        message 'after ' valid-object(dtUtilCore). */
/*        return dtUtilCore.                         */
/*    end method.                                    */
   &else
    constructor public dtUtilCore():
        do on error undo, return error:
            run com/quarix/base/enforceSingleton.p (this-object).
        end.
    end constructor.
   &endif

/*    method public void Reset(  ):*/
/*        message 'reset'.         */
/*    end method.                  */

    method override public datetime-tz sysNow(  ):

        return super:sysNow().

    end method.

    method override public date sysDate(  ):

        return super:sysDate().

    end method.

    method override public datetime-tz nowTZ(  ):

        return super:nowTZ().

    end method.

    method override public date todayTZ(  ):

        return super:todayTZ().

    end method.

    method override public integer sysTime(  ):

        return super:sysTime().

    end method.

end class.