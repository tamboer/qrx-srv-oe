routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dsemployee:handle

class wicketds.server.dmemployee inherits DataModel:

    {wicketds/server/dsemployee.i}

    {wicketds/server/dsdepartment.i}

    {wicketds/server/dsstate.i}

    def private property dmdepartment as wicketds.server.dmdepartment no-undo
    get:
        if not valid-object(dmdepartment) then dmdepartment = cast (GetInstance ('wicketds.server.dmdepartment':u), wicketds.server.dmdepartment).
        return dmdepartment.
    end.
    private set.

    def private property dmstate as wicketds.server.dmstate no-undo
    get:
        if not valid-object(dmstate) then dmstate = cast (GetInstance ('wicketds.server.dmstate':u), wicketds.server.dmstate).
        return dmstate.
    end.
    private set.

    constructor public dmemployee (  ):

        DataAccess      = cast(GetInstance('wicketds.server.daemployee':u), 'wicketds.server.daemployee':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daoemployee'.
        OpenOnInit      = false.
        SendChangesOnly = false.

        if not valid-object(DataAccess) or
            not valid-handle(datasetHandle)
        then return error.

    end constructor.

    method public override logical dataFetch ():

        DataContext:setAllRecords(true).

        return super:dataFetch().

    end method.

    method private logical AfterLoadData (input-output dataset dsemployee):

    	define variable departmentidlist	as wicketds.server.departmentidlist	no-undo.
    	define variable stateidlist			as wicketds.server.stateidlist		no-undo.

    	departmentidlist = cast(GetInstance('wicketds.server.departmentidlist':u), 'wicketds.server.departmentidlist':u).

    	stateidlist = cast(GetInstance('wicketds.server.stateidlist':u), 'wicketds.server.stateidlist':u).

		for each ttEmployee
			no-lock:

			departmentidlist:addDeptCode(ttEmployee.DeptCode).

			stateidlist:addStateCode(ttEmployee.State).

		end. /* for each ttEmployee */

		if departmentidlist:IdListHasData()
		then do:
			if not dmdepartment:getbydepartmentidlist(departmentidlist, output dataset dsdepartment by-reference)
			then return false.

			for each ttEmployee
				no-lock:

				find first ttDepartment
					where ttDepartment.DeptCode = ttEmployee.DeptCode
					no-lock no-error.

				if available(ttDepartment)
				then ttEmployee.DeptName = ttDepartment.DeptName.

			end.

		end. /* if departmentidlist:IdListHasData() */

		if stateidlist:IdListHasData()
		then do:
			if not dmstate:getbystateidlist(stateidlist, output dataset dsstate by-reference)
			then return false.

			for each ttEmployee
				no-lock:

				find first ttstate
					where ttstate.StateCode = ttEmployee.State
					no-lock no-error.

				if available(ttstate)
				then ttEmployee.StateName = ttstate.StateName.
			end.

		end. /* if stateidlist:IdListHasData() */

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.
		finally:
			UnloadInstance(departmentidlist).
			UnloadInstance(stateidlist).
		end finally.

	end method.

	method public override logical AfterLoadData():

		return AfterLoadData(input-output dataset-handle datasetHandle by-reference).

	end method.

end class.