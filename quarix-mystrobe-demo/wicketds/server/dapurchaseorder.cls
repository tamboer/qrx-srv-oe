
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataAccess.

&global-define tbl_purchaseorder PurchaseOrder

class wicketds.server.dapurchaseorder inherits DataAccess:

    {wicketds/server/dspurchaseorder.i}

    define buffer {&tbl_purchaseorder} for {&tbl_purchaseorder}.

    constructor public dapurchaseorder (  ):
        ID = 'dapurchaseorder':u.
    end constructor.

    method private character SetFieldMappings():

        define variable cFieldMapping as character no-undo.

        assign
            cFieldMapping   =
                '{&tbl_purchaseorder}.PONum'            + ',ttPurchaseOrder.PONum'         + ',' +
                '{&tbl_purchaseorder}.DateEntered'      + ',ttPurchaseOrder.DateEntered'   + ',' +
                '{&tbl_purchaseorder}.ReceiveDate'      + ',ttPurchaseOrder.ReceiveDate'   + ',' +
                '{&tbl_purchaseorder}.POStatus'         + ',ttPurchaseOrder.POStatus'      + ',' +
                '{&tbl_purchaseorder}.SupplierIDNum'    + ',ttPurchaseOrder.SupplierIDNum'
                .

        return cFieldMapping.

    end method.

    method public override logical AttachDataSource():

        define variable cFieldMapping   as character    no-undo.
        define variable cDbTables       as character    no-undo.
        define variable cKeys           as character    no-undo.

        cFieldMapping = SetFieldMappings().

        assign
            cDbTables   = "{&tbl_purchaseorder}"
            cKeys       = 'PONum':U.

        return AddDataSource ('ttpurchaseorder':u, cDbTables, cKeys, ?, cFieldMapping).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private logical BeforeSaveData(input-output dataset dspurchaseorder):

        for each ttpurchaseorder
            where row-state(ttpurchaseorder) = row-created
            no-lock:

            ttpurchaseorder.PONum = next-value(NextPONum).
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
