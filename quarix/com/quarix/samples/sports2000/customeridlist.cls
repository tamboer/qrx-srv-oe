/*------------------------------------------------------------------------
File : dacustomer
Purpose : Custoemr IdList container for sports2000 sample
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

routine-level on error undo, throw.

using Progress.Lang.*.

class com.quarix.samples.sports2000.customeridlist inherits com.quarix.data.IdList:

	{com/quarix/samples/sports2000/ttcustomeridlist.i}

	constructor customeridlist():
		hbttHandle = buffer ttcustomeridlist:handle.
	end constructor.

	method public void addCustNum(input iCustNum as int):

		AddRecord('custNum':U, string(iCustNum)).

	end method.

end class.