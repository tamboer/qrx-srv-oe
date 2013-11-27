class wicketds.server.tableobjects.ttorder inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttOrder"
        get.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowid) then rowid = new com.quarix.data.ttfield("rowid", "ttOrder").

            return rowid.
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowstate) then rowstate = new com.quarix.data.ttfield("rowstate", "ttOrder").

            return rowstate.
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SortOrder) then SortOrder = new com.quarix.data.ttfield("SortOrder", "ttOrder").

            return SortOrder.
        end.
        private set.

    define public static property BillToID as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(BillToID) then BillToID = new com.quarix.data.ttfield("BillToID", "ttOrder").

            return BillToID.
        end.
        private set.

    define public static property Carrier as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Carrier) then Carrier = new com.quarix.data.ttfield("Carrier", "ttOrder").

            return Carrier.
        end.
        private set.

    define public static property CreditCard as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(CreditCard) then CreditCard = new com.quarix.data.ttfield("CreditCard", "ttOrder").

            return CreditCard.
        end.
        private set.

    define public static property CustNum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(CustNum) then CustNum = new com.quarix.data.ttfield("CustNum", "ttOrder").

            return CustNum.
        end.
        private set.

    define public static property Instructions as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Instructions) then Instructions = new com.quarix.data.ttfield("Instructions", "ttOrder").

            return Instructions.
        end.
        private set.

    define public static property OrderDate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(OrderDate) then OrderDate = new com.quarix.data.ttfield("OrderDate", "ttOrder").

            return OrderDate.
        end.
        private set.

    define public static property Ordernum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Ordernum) then Ordernum = new com.quarix.data.ttfield("Ordernum", "ttOrder").

            return Ordernum.
        end.
        private set.

    define public static property OrderStatus as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(OrderStatus) then OrderStatus = new com.quarix.data.ttfield("OrderStatus", "ttOrder").

            return OrderStatus.
        end.
        private set.

    define public static property PO as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(PO) then PO = new com.quarix.data.ttfield("PO", "ttOrder").

            return PO.
        end.
        private set.

    define public static property PromiseDate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(PromiseDate) then PromiseDate = new com.quarix.data.ttfield("PromiseDate", "ttOrder").

            return PromiseDate.
        end.
        private set.

    define public static property SalesRep as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SalesRep) then SalesRep = new com.quarix.data.ttfield("SalesRep", "ttOrder").

            return SalesRep.
        end.
        private set.

    define public static property ShipDate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(ShipDate) then ShipDate = new com.quarix.data.ttfield("ShipDate", "ttOrder").

            return ShipDate.
        end.
        private set.

    define public static property ShipToID as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(ShipToID) then ShipToID = new com.quarix.data.ttfield("ShipToID", "ttOrder").

            return ShipToID.
        end.
        private set.

    define public static property Terms as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Terms) then Terms = new com.quarix.data.ttfield("Terms", "ttOrder").

            return Terms.
        end.
        private set.

    define public static property WarehouseNum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(WarehouseNum) then WarehouseNum = new com.quarix.data.ttfield("WarehouseNum", "ttOrder").

            return WarehouseNum.
        end.
        private set.

    define public static property CustName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(CustName) then CustName = new com.quarix.data.ttfield("CustName", "ttOrder").

            return CustName.
        end.
        private set.

    define public static property SalesRepName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SalesRepName) then SalesRepName = new com.quarix.data.ttfield("SalesRepName", "ttOrder").

            return SalesRepName.
        end.
        private set.

    constructor static ttorder():
    end constructor.

    destructor public ttorder():
        delete object rowid no-error.
        delete object rowstate no-error.
        delete object SortOrder no-error.
        delete object BillToID no-error.
        delete object Carrier no-error.
        delete object CreditCard no-error.
        delete object CustNum no-error.
        delete object Instructions no-error.
        delete object OrderDate no-error.
        delete object Ordernum no-error.
        delete object OrderStatus no-error.
        delete object PO no-error.
        delete object PromiseDate no-error.
        delete object SalesRep no-error.
        delete object ShipDate no-error.
        delete object ShipToID no-error.
        delete object Terms no-error.
        delete object WarehouseNum no-error.
        delete object CustName no-error.
        delete object SalesRepName no-error.
    end destructor.

end class.
