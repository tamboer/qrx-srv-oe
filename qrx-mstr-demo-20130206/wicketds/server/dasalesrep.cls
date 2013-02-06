routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataAccess.

&global-define tbl_salesrep Salesrep

class wicketds.server.dasalesrep inherits DataAccess:

    {wicketds/server/dssalesrep.i}

    define buffer {&tbl_salesrep} for {&tbl_salesrep}.

    constructor public dasalesrep (  ):
        ID = 'dasalesrep':u.
    end constructor.

    method private character SetFieldMappings():

        define variable cFieldMapping as character no-undo.

        assign
            cFieldMapping   =
                '{&tbl_salesrep}.Region'    + ',ttsalesrep.Region'   + ',' +
                '{&tbl_salesrep}.SalesRep'  + ',ttsalesrep.SalesRep' + ',' +
                '{&tbl_salesrep}.RepName'   + ',ttsalesrep.RepName'
                .
        return cFieldMapping.

    end method.

    method private logical AttachDataSourceDefault():

        define variable cFieldMapping   as character    no-undo.
        define variable cDbTables       as character    no-undo.
        define variable cKeys           as character    no-undo.

        cFieldMapping = SetFieldMappings().

        assign
            cDbTables   = "{&tbl_salesrep}"
            cKeys       = 'SalesRep':U.

        return AddDataSource ('ttsalesrep':u, cDbTables, cKeys, ?, cFieldMapping).

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
			cKeys			= 'SalesRep':U
			cExcludeFields	= 'ttsalesrep.SalesRep':U.

		if not AddDataSource ('ttsalesrep':u, hTmpList:default-buffer-handle, cKeys, ?, ?, cExcludeFields, ?, ?)
		then return false.

		cFieldMapping = SetFieldMappings().

        assign
            cDbTables   = "{&tbl_salesrep}"
            cKeys       = 'SalesRep':U.

		cJoinClause	= 'ttsalesrep.SalesRep = ttsalesrepidlist.SalesRep':U.

		if not AddDataSource ('ttsalesrep':u, cDbTables, cKeys, ?, cFieldMapping, cExcludeFields, ?, cJoinClause)
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

end class.