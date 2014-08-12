/*------------------------------------------------------------------------
File : dacustomer
Purpose : Customer Data Access for sports2000 sample
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
using com.quarix.data.DataAccess.
using com.quarix.samples.sports2000.tableobjects.TTCustomer.
using com.quarix.samples.sports2000.tableobjects.DBCustomer.


&global-define tbl_customer Customer

class com.quarix.samples.sports2000.dacustomer inherits DataAccess:

	{com/quarix/samples/sports2000/dscustomer.i}

	define buffer {&tbl_customer} for {&tbl_customer}.

	constructor public dacustomer (  ):
        ID = 'dacustomer':u.
	end constructor.

	method private character SetFieldMappings():
   
    define variable cFieldMapping as character no-undo.

    cFieldMapping = AutoFieldMap(DBCustomer:Instance, TTCustomer:Instance).
    
		/*
		assign
			cFieldMapping	=	
			          '{&tbl_customer}.custNum'     + ',ttCustomer.custNum'			+ ',' +
								'{&tbl_customer}.country'     + ',ttCustomer.country'		  + ',' +
								'{&tbl_customer}.name'        + ',ttCustomer.name'		    + ',' +
								'{&tbl_customer}.address'     + ',ttCustomer.address'	    + ',' +
								'{&tbl_customer}.address2'    + ',ttCustomer.address2'	  + ',' +
								'{&tbl_customer}.city'        + ',ttCustomer.city'	      + ',' +
								'{&tbl_customer}.state'       + ',ttCustomer.state'				+ ',' +
								'{&tbl_customer}.postalCode'  + ',ttCustomer.postalCode'	+ ',' +
								'{&tbl_customer}.contact'     + ',ttCustomer.contact'			+ ',' +
								'{&tbl_customer}.phone'       + ',ttCustomer.phone'				+ ',' +
								'{&tbl_customer}.salesRep'    + ',ttCustomer.salesRep'		+ ',' +
								'{&tbl_customer}.creditLimit' + ',ttCustomer.creditLimit'	+ ',' +
								'{&tbl_customer}.balance'     + ',ttCustomer.balance'			+ ',' +
								'{&tbl_customer}.terms'       + ',ttCustomer.terms'			  + ',' +
								'{&tbl_customer}.discount'    + ',ttCustomer.discount'		+ ',' +
								'{&tbl_customer}.comments'    + ',ttCustomer.comments'.
    */
		return cFieldMapping.

	end method.

	method private logical AttachDataSourceDefault():

		define variable cFieldMapping	as character	no-undo.
		define variable cDbTables     as character	no-undo.
		define variable cKeys         as character	no-undo.

		cFieldMapping = SetFieldMappings().

		assign
			cDbTables	= "{&tbl_customer}"
			cKeys     	= 'custNum':U.

		return AddDataSource ('ttCustomer':u, cDbTables, cKeys, ?, cFieldMapping).

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	method private logical AttachDataSourceIdList():

		define variable hTmpList       as handle    no-undo.
		define variable cJoinClause    as character	no-undo.
		define variable cKeys          as character	no-undo.
		define variable cFieldMapping  as character	no-undo.
		define variable cDbTables      as character	no-undo.
		define variable cExcludeFields as character	no-undo.

		hTmpList = GetIdListTmpHandle().

		if not valid-handle(hTmpList)
			then return false.

		assign
			cKeys			= 'custNum':U
			cExcludeFields	= 'ttCustomer.custNum':U.

		if not AddDataSource ('ttCustomer':u, hTmpList:default-buffer-handle, cKeys, ?, ?, cExcludeFields, ?, ?)
		then return false.

		cFieldMapping = SetFieldMappings().

		assign
			cDbTables	= "{&tbl_customer}"
			cKeys		= 'custNum':U.

		cJoinClause	= 'ttCustomer.custNum = ttCustomerIdList.custNum':U.

		if not AddDataSource ('ttCustomer':u, cDbTables, cKeys, ?, cFieldMapping, cExcludeFields, ?, cJoinClause, true)
		then return false.

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	method public override logical AttachDataSource():

		define variable cQueryName as character no-undo.

		cQueryName = GetQueryName().

		case cQueryName:

			when 'FetchDataByIdList':U
			then return AttachDataSourceIdList().

			otherwise
				return AttachDataSourceDefault().

		end case. /* case cQueryName */

		return false.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

end class.
