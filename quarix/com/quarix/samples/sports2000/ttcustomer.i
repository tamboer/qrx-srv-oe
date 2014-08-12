/*------------------------------------------------------------------------
File : dacustomer
Purpose : Customer Temp-table for sports2000 sample
Syntax :
Description :
Author(s) : tomd
Created : Thu Aug 12 13:55:00 CET 2014
Notes :
License :
This file is part of the QRX-SRV-OE software framework.

The QRX-SRV-OE software framework is free software; you can redistribute
it and/or modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either version 2.1
of the License, or (at your option) any later version.

The QRX-SRV-OE software framework is distributed in the hope that it will
be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the QRX-SRV-OE software framework; if not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA or on the internet at the following address:
http://www.gnu.org/licenses/lgpl-2.1.txt
----------------------------------------------------------------------*/

define {&scope} temp-table {&prefix}ttCustomer no-undo {&REFERENCE-ONLY}
    before-table {&prefix}btCustomer
	{com/quarix/data/sortorder.i &fields="
  field custNum      as int  format '>>>>9'
	field country      as char format 'x(20)'
	field name         as char format 'x(30)'
	field address      as char format 'x(35)'
	field address2     as char format 'x(35)'
	field city         as char format 'x(25)'
	field state        as char format 'x(20)'
	field postalCode   as char format 'x(10)'
	field contact      as char format 'x(30)'
	field phone        as char format 'x(20)'
	field salesRep     as char format 'x(4)'
	field creditLimit  as dec  format '->,>>>,>>9'
	field balance      as dec  format '->,>>>,>>9.99'
	field terms        as char format 'x(20)'
	field discount     as int  format '>>9%'
	field comments     as char format 'x(80)'
	"}
    /*field TableRowId       as rowid*/
    index IDX_ttCustomer is unique custNum.