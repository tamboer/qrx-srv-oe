class wicketds.server.tableobjects.ttpurchaseorder inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttPurchaseOrder"
        get.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowid) then rowid = new com.quarix.data.ttfield("rowid", "ttPurchaseOrder").

            return rowid.
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowstate) then rowstate = new com.quarix.data.ttfield("rowstate", "ttPurchaseOrder").

            return rowstate.
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SortOrder) then SortOrder = new com.quarix.data.ttfield("SortOrder", "ttPurchaseOrder").

            return SortOrder.
        end.
        private set.

    define public static property PONum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(PONum) then PONum = new com.quarix.data.ttfield("PONum", "ttPurchaseOrder").

            return PONum.
        end.
        private set.

    define public static property DateEntered as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(DateEntered) then DateEntered = new com.quarix.data.ttfield("DateEntered", "ttPurchaseOrder").

            return DateEntered.
        end.
        private set.

    define public static property ReceiveDate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(ReceiveDate) then ReceiveDate = new com.quarix.data.ttfield("ReceiveDate", "ttPurchaseOrder").

            return ReceiveDate.
        end.
        private set.

    define public static property POStatus as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(POStatus) then POStatus = new com.quarix.data.ttfield("POStatus", "ttPurchaseOrder").

            return POStatus.
        end.
        private set.

    define public static property SupplierIDNum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SupplierIDNum) then SupplierIDNum = new com.quarix.data.ttfield("SupplierIDNum", "ttPurchaseOrder").

            return SupplierIDNum.
        end.
        private set.

    constructor static ttpurchaseorder():
    end constructor.

    destructor public ttpurchaseorder():
        delete object rowid no-error.
        delete object rowstate no-error.
        delete object SortOrder no-error.
        delete object PONum no-error.
        delete object DateEntered no-error.
        delete object ReceiveDate no-error.
        delete object POStatus no-error.
        delete object SupplierIDNum no-error.
    end destructor.

end class.
