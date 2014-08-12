/*------------------------------------------------------------------------
File : dacustomer
Purpose : Cutomer Data Model for sports2000 sample
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
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dscustomer:handle

class com.quarix.samples.sports2000.dmcustomer inherits DataModel:

	{com/quarix/samples/sports2000/dscustomer.i}

	constructor public dmcustomer (  ):

        DataAccess      = cast(GetInstance('com.quarix.samples.sports2000.dacustomer':u), 'com.quarix.samples.sports2000.dacustomer':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daocustomer'.
        OpenOnInit      = false.
        SendChangesOnly = false.

        DataContext:setAllRecords(true).

        if not valid-object(DataAccess) or
        	not valid-handle(datasetHandle)
        then return error.

    end constructor.

	method public logical fetchByCustomerIdList(input custidlist as com.quarix.samples.sports2000.customeridlist,
		                                     output dataset dsCustomer):

		return dataFetchIdList(input custidlist, output dataset dsCustomer by-reference).

	end method.

	

	method public logical GetByCustNum (input iCustNum as int,
		                                output dataset dsCustomer):

		dataset dsCustomer:empty-dataset().

		ClearContext().

		SetFilter('ttCustomer.custNum':U, '=':U, if iCustNum = ? then ? else string(iCustNum)).

		return dataFetch(output dataset dsCustomer by-reference).

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

end class.
