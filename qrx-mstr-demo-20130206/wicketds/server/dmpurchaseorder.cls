
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dspurchaseorder:handle

class wicketds.server.dmpurchaseorder inherits DataModel:

    {wicketds/server/dspurchaseorder.i}

    constructor public dmpurchaseorder (  ):

        DataAccess      = cast(GetInstance('wicketds.server.dapurchaseorder':u), 'wicketds.server.dapurchaseorder':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daopurchaseorder'.
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

end class.
