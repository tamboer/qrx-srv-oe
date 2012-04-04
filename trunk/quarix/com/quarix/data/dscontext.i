
/*------------------------------------------------------------------------
    File        : dscontext.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Adam
    Created     : Thu Jul 28 17:12:31 EEST 2011
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

  	{com/quarix/data/queryTables.i  {&*}}

    define {&scope} temp-table ttInfo no-undo {&reference-only}
        field ttName		as character
        field ttRowPos		as character
        field lastBatch		as logical		initial true
        field firstBatch	as logical		initial true
        field useIndex		as character
        field batchSize		as integer
        field startRowid	as character	initial 'first':u
        field startRow		as integer		initial ?
        field skipRow		as logical		initial true
        field isSearchReq	as logical		initial false
        index xpk is primary unique ttName.

    define {&scope} temp-table ttRowId no-undo {&reference-only}
        field ttName	as character
        field ttRowId	as character
        field dbRowId	as character
        field NumRec	as integer
        index xpk is primary unique ttName ttRowId
        index idxNumRec is unique NumRec.

    define {&scope} temp-table ttProperty no-undo {&reference-only}
    	field propertyName	as character
    	field propertyValue	as character
    	index xpk is primary unique propertyName.

    define dataset dsContext {&reference-only} for ttFilter, ttSort, ttInfo, ttRowId, ttProperty.


