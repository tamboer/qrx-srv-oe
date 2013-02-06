
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataAccess.

&global-define tbl_department Department

class wicketds.server.dadepartment inherits DataAccess:

    {wicketds/server/dsdepartment.i}

    define buffer {&tbl_department} for {&tbl_department}.

    constructor public dadepartment (  ):
        ID = 'dadepartment':u.
    end constructor.

    method private character SetFieldMappings():

        define variable cFieldMapping as character no-undo.

        assign
            cFieldMapping   =
                '{&tbl_department}.DeptCode'    + ',ttdepartment.DeptCode'         + ',' +
                '{&tbl_department}.DeptName'    + ',ttdepartment.DeptName'
                .
        return cFieldMapping.

    end method.

    method private logical AttachDataSourceDefault():

        define variable cFieldMapping   as character    no-undo.
        define variable cDbTables       as character    no-undo.
        define variable cKeys           as character    no-undo.

        cFieldMapping = SetFieldMappings().

        assign
            cDbTables   = "{&tbl_department}"
            cKeys       = 'DeptCode':U.

        return AddDataSource ('ttdepartment':u, cDbTables, cKeys, ?, cFieldMapping).

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
			cKeys			= 'DeptCode':U
			cExcludeFields	= 'ttdepartment.DeptCode':U.

		if not AddDataSource ('ttdepartment':u, hTmpList:default-buffer-handle, cKeys, ?, ?, cExcludeFields, ?, ?)
		then return false.

		cFieldMapping = SetFieldMappings().

		assign
            cDbTables   = "{&tbl_department}"
            cKeys       = 'DeptCode':U.

		cJoinClause	= 'ttdepartment.DeptCode = ttdepartmentidlist.DeptCode':U.

		if not AddDataSource ('ttdepartment':u, cDbTables, cKeys, ?, cFieldMapping, cExcludeFields, ?, cJoinClause)
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