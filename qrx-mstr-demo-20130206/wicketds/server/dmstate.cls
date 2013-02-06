routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dsstate:handle

class wicketds.server.dmstate inherits DataModel:

    {wicketds/server/dsstate.i}

    constructor public dmstate (  ):

        DataAccess      = cast(GetInstance('wicketds.server.dastate':u), 'wicketds.server.dastate':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daostate'.
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

    method public logical getbystateidlist(input stateidlist as wicketds.server.stateidlist,
                                                output dataset dsstate):

		return dataFetchIdList(input stateidlist, output dataset dsstate by-reference).

    end method.

end class.