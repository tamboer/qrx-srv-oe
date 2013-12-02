
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataAccess.
using wicketds.server.tableobjects.*.
using com.quarix.data.*.

class wicketds.server.dacustomer inherits DataAccess:

	{wicketds/server/dscustomer.i}

	constructor public dacustomer (  ):
        ID = 'dacustomer':u.
	end constructor.

	method private character SetFieldMappings():

		define variable cFieldMapping as character no-undo.

		ClearFieldMap().

		AddFieldMap(DBCustomer:Address, TTCustomer:Address).
		AddFieldMap(DBCustomer:Address2, TTCustomer:Address2).
		AddFieldMap(DBCustomer:Balance, TTCustomer:Balance).
		AddFieldMap(DBCustomer:City, TTCustomer:City).
		AddFieldMap(DBCustomer:Comments, TTCustomer:Comments).
		AddFieldMap(DBCustomer:Contact, TTCustomer:Contact).
		AddFieldMap(DBCustomer:Country, TTCustomer:Country).
		AddFieldMap(DBCustomer:CreditLimit, TTCustomer:CreditLimit).
		AddFieldMap(DBCustomer:CustNum, TTCustomer:CustNum).
		AddFieldMap(DBCustomer:Discount, TTCustomer:Discount).
		AddFieldMap(DBCustomer:EmailAddress, TTCustomer:EmailAddress).
		AddFieldMap(DBCustomer:Fax, TTCustomer:Fax).
		AddFieldMap(DBCustomer:Name, TTCustomer:Name).
		AddFieldMap(DBCustomer:Phone, TTCustomer:Phone).
		AddFieldMap(DBCustomer:PostalCode, TTCustomer:PostalCode).
		AddFieldMap(DBCustomer:SalesRep, TTCustomer:SalesRep).
		AddFieldMap(DBCustomer:State, TTCustomer:State).
		AddFieldMap(DBCustomer:Terms, TTCustomer:Terms).

		cFieldMapping = GetFieldMapping().

		return cFieldMapping.

	end method.

	method private logical AttachDataSourceDefault():

		define variable cFieldMapping	as character	no-undo.
		define variable cDbTables		as character	no-undo.
		define variable cKeys			as character	no-undo.

		cFieldMapping = SetFieldMappings().

		assign
			cDbTables	= DBCustomer:TableName
			cKeys		= DBCustomer:CustNum:getName().

		return AddDataSource (TTCustomer:TableName, cDbTables, cKeys, ?, cFieldMapping).

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
			cKeys			= TTCustomer:CustNum:getName()
			cExcludeFields	= TTCustomer:CustNum:getName().

		if not AddDataSource (TTCustomer:TableName, hTmpList:default-buffer-handle, cKeys, ?, ?, cExcludeFields, ?, ?)
		then return false.

		cFieldMapping = SetFieldMappings().

		assign
			cDbTables	= DBCustomer:TableName
			cKeys		= DBCustomer:CustNum:getName().

		cJoinClause	= substitute('&1 = &2', TTCustomer:CustNum:getFqName(), TTCustomeridlist:CustNum:getFqName()).

		if not AddDataSource (ttcustomer:TableName, cDbTables, cKeys, ?, cFieldMapping, cExcludeFields, ?, cJoinClause)
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
