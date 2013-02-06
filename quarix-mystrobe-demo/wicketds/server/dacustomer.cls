
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataAccess.

&global-define tbl_customer Customer

class wicketds.server.dacustomer inherits DataAccess:

	{wicketds/server/dscustomer.i}

	define buffer {&tbl_customer} for {&tbl_customer}.

	constructor public dacustomer (  ):
        ID = 'dacustomer':u.
	end constructor.

	method private character SetFieldMappings():

		define variable cFieldMapping as character no-undo.

		assign
			cFieldMapping	=
				'{&tbl_customer}.Address'		+ ',ttcustomer.Address'			+ ',' +
				'{&tbl_customer}.Address2'		+ ',ttcustomer.Address2'		+ ',' +
				'{&tbl_customer}.Balance'		+ ',ttcustomer.Balance'			+ ',' +
				'{&tbl_customer}.City'			+ ',ttcustomer.City'			+ ',' +
				'{&tbl_customer}.Comments'		+ ',ttcustomer.Comments'		+ ',' +
				'{&tbl_customer}.Contact'		+ ',ttcustomer.Contact'			+ ',' +
				'{&tbl_customer}.Country'		+ ',ttcustomer.Country'			+ ',' +
				'{&tbl_customer}.CreditLimit'	+ ',ttcustomer.CreditLimit'		+ ',' +
				'{&tbl_customer}.CustNum'		+ ',ttcustomer.CustNum'			+ ',' +
				'{&tbl_customer}.Discount'		+ ',ttcustomer.Discount'		+ ',' +
				'{&tbl_customer}.EmailAddress'	+ ',ttcustomer.EmailAddress'	+ ',' +
				'{&tbl_customer}.Fax'			+ ',ttcustomer.Fax'				+ ',' +
				'{&tbl_customer}.Name'			+ ',ttcustomer.Name'			+ ',' +
				'{&tbl_customer}.Phone'			+ ',ttcustomer.Phone'			+ ',' +
				'{&tbl_customer}.PostalCode'	+ ',ttcustomer.PostalCode'		+ ',' +
				'{&tbl_customer}.SalesRep'		+ ',ttcustomer.SalesRep'		+ ',' +
				'{&tbl_customer}.State'			+ ',ttcustomer.State'			+ ',' +
				'{&tbl_customer}.Terms'			+ ',ttcustomer.Terms'
				.

		return cFieldMapping.

	end method.

	method private logical AttachDataSourceDefault():

		define variable cFieldMapping	as character	no-undo.
		define variable cDbTables		as character	no-undo.
		define variable cKeys			as character	no-undo.

		cFieldMapping = SetFieldMappings().

		assign
			cDbTables	= "{&tbl_customer}"
			cKeys		= 'CustNum':U.

		return AddDataSource ('ttcustomer':u, cDbTables, cKeys, ?, cFieldMapping).

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
			cKeys			= 'CustNum':U
			cExcludeFields	= 'ttcustomer.CustNum':U.

		if not AddDataSource ('ttcustomer':u, hTmpList:default-buffer-handle, cKeys, ?, ?, cExcludeFields, ?, ?)
		then return false.

		cFieldMapping = SetFieldMappings().

		assign
			cDbTables	= "{&tbl_customer}"
			cKeys		= 'CustNum':U.

		cJoinClause	= 'ttcustomer.CustNum = ttcustomeridlist.CustNum':U.

		if not AddDataSource ('ttcustomer':u, cDbTables, cKeys, ?, cFieldMapping, cExcludeFields, ?, cJoinClause)
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

		end case.  /* case cQueryName */

		return false.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	method private logical BeforeSaveData(input-output dataset dscustomer):

		for each btCustomer
			where row-state(btCustomer) = row-deleted
			no-lock:

			find first BillTo
				where BillTo.CustNum = btCustomer.CustNum
				no-lock no-error.

			find first Invoice
				where Invoice.CustNum = btCustomer.CustNum
				no-lock no-error.

			find first Order
				where Order.CustNum = btCustomer.CustNum
				no-lock no-error.

			find first RefCall
				where RefCall.CustNum = btCustomer.CustNum
				no-lock no-error.

			find first ShipTo
				where ShipTo.CustNum = btCustomer.CustNum
				no-lock no-error.

			if available(BillTo)	or
				available(Invoice)	or
				available(Order)	or
				available(RefCall)	or
				available(ShipTo)
			then do:
				ThrowClientError(1000, 'Customer cannot be deleted, it is used in other tables!').

				return false.
			end.

		end. /* for each btCustomer */


        for each ttcustomer
            where row-state(ttcustomer) = row-created
            no-lock:

            ttcustomer.CustNum = next-value(NextCustNum).
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public override logical BeforeSaveData():
        return BeforeSaveData(input-output dataset-handle datasetHandle by-reference).
    end method.

end class.
