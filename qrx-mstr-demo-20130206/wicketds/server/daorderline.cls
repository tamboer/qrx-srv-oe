
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataAccess.

&global-define tbl_orderline OrderLine

class wicketds.server.daorderline inherits DataAccess:

    {wicketds/server/dsorderline.i}

    define buffer {&tbl_orderline} for {&tbl_orderline}.

    constructor public daorderline (  ):
        ID = 'daorderline':u.
    end constructor.

    method private character SetFieldMappings():

        define variable cFieldMapping as character no-undo.

        assign
            cFieldMapping   =
                '{&tbl_orderline}.Discount'         + ',ttorderline.Discount'         + ',' +
                '{&tbl_orderline}.ExtendedPrice'    + ',ttorderline.ExtendedPrice'   + ',' +
                '{&tbl_orderline}.Itemnum'          + ',ttorderline.Itemnum'   + ',' +
                '{&tbl_orderline}.Linenum'          + ',ttorderline.Linenum'      + ',' +
                '{&tbl_orderline}.OrderLineStatus'  + ',ttorderline.OrderLineStatus'      + ',' +
                '{&tbl_orderline}.Ordernum'         + ',ttorderline.Ordernum'      + ',' +
                '{&tbl_orderline}.Price'            + ',ttorderline.Price'      + ',' +
                '{&tbl_orderline}.Qty'              + ',ttorderline.Qty'
                .
        return cFieldMapping.

    end method.

    method private logical AttachDataSourceDefault():

		define variable cFieldMapping	as character	no-undo.
		define variable cDbTables		as character	no-undo.
		define variable cKeys			as character	no-undo.

		cFieldMapping = SetFieldMappings().

        assign
            cDbTables   = "{&tbl_orderline}"
            cKeys       = 'Ordernum,Linenum':U.

        return AddDataSource ('ttorderline':u, cDbTables, cKeys, ?, cFieldMapping).

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	method private logical AttachDataSourceItemIdList():

		define variable hTmpList		as handle		no-undo.
		define variable cJoinClause		as character	no-undo.
		define variable cKeys			as character	no-undo.
		define variable cFieldMapping	as character	no-undo.
		define variable cDbTables		as character	no-undo.
		define variable cExcludeFields	as character	no-undo.

		hTmpList = GetIdListTmpHandle().

		if not valid-handle(hTmpList)
		then return false.

		assign
			cKeys			= 'Itemnum':U
			cExcludeFields	= 'ttorderline.Itemnum':U.

		if not AddDataSource ('ttorderline':u, hTmpList:default-buffer-handle, cKeys, ?, ?, cExcludeFields, ?, ?)
		then return false.

		cFieldMapping = SetFieldMappings().

        assign
            cDbTables   = "{&tbl_orderline}"
            cKeys       = 'Ordernum,Linenum':U.

		cJoinClause	= 'ttorderline.Itemnum = ttitemidlist.Itemnum':U.

		if not AddDataSource ('ttorderline':u, cDbTables, cKeys, ?, cFieldMapping, cExcludeFields, ?, cJoinClause, true)
		then return false.

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	method private logical AttachDataSourceOrderIdList():

		define variable hTmpList		as handle		no-undo.
		define variable cJoinClause		as character	no-undo.
		define variable cKeys			as character	no-undo.
		define variable cFieldMapping	as character	no-undo.
		define variable cDbTables		as character	no-undo.
		define variable cExcludeFields	as character	no-undo.

		hTmpList = GetIdListTmpHandle().

		if not valid-handle(hTmpList)
		then return false.

		assign
			cKeys			= 'Ordernum':U
			cExcludeFields	= 'ttorderline.Ordernum':U.

		if not AddDataSource ('ttorderline':u, hTmpList:default-buffer-handle, cKeys, ?, ?, cExcludeFields, ?, ?)
		then return false.

		cFieldMapping = SetFieldMappings().

        assign
            cDbTables   = "{&tbl_orderline}"
            cKeys       = 'Ordernum,Linenum':U.

		cJoinClause	= 'ttorderline.Ordernum = ttorderidlist.Ordernum':U.

		if not AddDataSource ('ttorderline':u, cDbTables, cKeys, ?, cFieldMapping, cExcludeFields, ?, cJoinClause)
		then return false.

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

    method public override logical AttachDataSource():

        define variable cFieldMapping   as character    no-undo.
        define variable cDbTables       as character    no-undo.
        define variable cKeys           as character    no-undo.
        define variable cQueryName		as character	no-undo.

		cQueryName = GetQueryName().

		case cQueryName:

			when 'FetchDataByItemIdList':U
			then return AttachDataSourceItemIdList().

			when 'FetchDataByOrderIdList':U
			then return AttachDataSourceOrderIdList().

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