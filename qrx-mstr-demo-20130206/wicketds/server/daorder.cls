
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataAccess.

&global-define tbl_order Order

class wicketds.server.daorder inherits DataAccess:

    {wicketds/server/dsorder.i}

    define buffer {&tbl_order} for {&tbl_order}.

    constructor public daorder (  ):
        ID = 'daorder':u.
    end constructor.

    method private character SetFieldMappings():

        define variable cFieldMapping as character no-undo.

        assign
            cFieldMapping   =
                '{&tbl_order}.BillToID'        + ',ttOrder.BillToID'         + ',' +
                '{&tbl_order}.Carrier'         + ',ttOrder.Carrier'   + ',' +
                '{&tbl_order}.CreditCard'      + ',ttOrder.CreditCard'   + ',' +
                '{&tbl_order}.CustNum'         + ',ttOrder.CustNum'      + ',' +
                '{&tbl_order}.Instructions'    + ',ttOrder.Instructions'      + ',' +
                '{&tbl_order}.OrderDate'       + ',ttOrder.OrderDate'      + ',' +
                '{&tbl_order}.Ordernum'        + ',ttOrder.Ordernum'      + ',' +
                '{&tbl_order}.OrderStatus'     + ',ttOrder.OrderStatus'      + ',' +
                '{&tbl_order}.PO'              + ',ttOrder.PO'      + ',' +
                '{&tbl_order}.PromiseDate'     + ',ttOrder.PromiseDate'      + ',' +
                '{&tbl_order}.SalesRep'        + ',ttOrder.SalesRep'      + ',' +
                '{&tbl_order}.ShipDate'        + ',ttOrder.ShipDate'      + ',' +
                '{&tbl_order}.ShipToID'        + ',ttOrder.ShipToID'      + ',' +
                '{&tbl_order}.Terms'           + ',ttOrder.Terms'      + ',' +
                '{&tbl_order}.WarehouseNum'    + ',ttOrder.WarehouseNum'
                .
        return cFieldMapping.

    end method.

    method public override logical AttachDataSource():

        define variable cFieldMapping   as character    no-undo.
        define variable cDbTables       as character    no-undo.
        define variable cKeys           as character    no-undo.

        cFieldMapping = SetFieldMappings().

        assign
            cDbTables   = "{&tbl_order}"
            cKeys       = 'Ordernum':U.

        return AddDataSource ('ttorder':u, cDbTables, cKeys, ?, cFieldMapping).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

     method private logical BeforeSaveData(input-output dataset dsorder):

        for each ttorder
            where row-state(ttorder) = row-created
            no-lock:

            ttorder.Ordernum = next-value(NextOrdNum).
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