/*------------------------------------------------------------------------
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

define {&scope} temp-table ttError no-undo {&reference-only}
    field errorLevel    as integer
    field errorCode     as integer
    field errorType     as integer
    field errorMessage  as character
    field errorParam    as character
    field errorObject   as character
    field errorRecord   as integer
    field errorTag      as character
    field logTime       as datetime
/*roxanam : new fields added to know where the error raised */
    field errorFile     as character
    field errorMethod   as character
    field errorLine     as integer
    index pk is primary errorLevel errorObject logTime.



