routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dssalesrep:handle

class wicketds.server.dmsalesrep inherits DataModel:

    {wicketds/server/dssalesrep.i}

    constructor public dmsalesrep (  ):

        DataAccess      = cast(GetInstance('wicketds.server.dasalesrep':u), 'wicketds.server.dasalesrep':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daosalesrep'.
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

    method public logical getbysalesrepidlist(input salesrepidlist as wicketds.server.salesrepidlist,
                                                output dataset dssalesrep):

		return dataFetchIdList(input salesrepidlist, output dataset dssalesrep by-reference).

    end method.

end class.