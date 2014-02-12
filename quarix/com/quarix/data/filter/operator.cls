
 /*------------------------------------------------------------------------
    File        : operator
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Adam
    Created     : Thu Jun 30 14:58:01 EEST 2011
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

using Progress.Lang.*.

class com.quarix.data.filter.operator final:

    define private static variable cEqual               as character initial 'eq'.
    define private static variable cNotEqual            as character initial 'ne'.
    define private static variable cLessThan            as character initial 'lt'.
    define private static variable cLessThanOrEqual     as character initial 'le'.
    define private static variable cGreaterThan         as character initial 'gt'.
    define private static variable cGreaterThanOrEqual  as character initial 'ge'.
    define private static variable cMatches             as character initial 'matches'.
    define private static variable cBegins              as character initial 'begins'.
	define private static variable cAnd					as character initial 'and'.
	define private static variable cOr					as character initial 'or'.

    constructor static operator():
    end constructor.

    method public static character isEq():
        return cEqual.
    end method.

    method public static character isNe():
        return cNotEqual.
    end method.

    method public static character isLt():
        return cLessThan.
    end method.

    method public static character isLe():
        return cLessThanOrEqual.
    end method.

    method public static character isGt():
        return cGreaterThan.
    end method.

    method public static character isGe():
        return cGreaterThanOrEqual.
    end method.

    method public static character isMatches():
        return cMatches.
    end method.

    method public static character isBegins():
        return cBegins.
    end method.

	method public static character isAnd():
		return cAnd.
	end method.

	method public static character isOr():
		return cOr.
	end method.

end class.
