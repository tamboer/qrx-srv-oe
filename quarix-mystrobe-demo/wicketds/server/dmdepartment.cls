routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dsdepartment:handle

class wicketds.server.dmdepartment inherits DataModel:

    {wicketds/server/dsdepartment.i}

    constructor public dmdepartment (  ):

        DataAccess      = cast(GetInstance('wicketds.server.dadepartment':u), 'wicketds.server.dadepartment':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daodepartment'.
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

    method public logical getbydepartmentidlist(input departmentidlist as wicketds.server.departmentidlist,
                                                output dataset dsdepartment):

		return dataFetchIdList(input departmentidlist, output dataset dsdepartment by-reference).

    end method.

end class.