class wicketds.server.tableobjects.ttorderline inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttOrderline"
        get.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowid) then rowid = new com.quarix.data.ttfield("rowid", "ttOrderline").

            return rowid.
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowstate) then rowstate = new com.quarix.data.ttfield("rowstate", "ttOrderline").

            return rowstate.
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SortOrder) then SortOrder = new com.quarix.data.ttfield("SortOrder", "ttOrderline").

            return SortOrder.
        end.
        private set.

    define public static property Discount as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Discount) then Discount = new com.quarix.data.ttfield("Discount", "ttOrderline").

            return Discount.
        end.
        private set.

    define public static property ExtendedPrice as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(ExtendedPrice) then ExtendedPrice = new com.quarix.data.ttfield("ExtendedPrice", "ttOrderline").

            return ExtendedPrice.
        end.
        private set.

    define public static property Itemnum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Itemnum) then Itemnum = new com.quarix.data.ttfield("Itemnum", "ttOrderline").

            return Itemnum.
        end.
        private set.

    define public static property Linenum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Linenum) then Linenum = new com.quarix.data.ttfield("Linenum", "ttOrderline").

            return Linenum.
        end.
        private set.

    define public static property OrderLineStatus as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(OrderLineStatus) then OrderLineStatus = new com.quarix.data.ttfield("OrderLineStatus", "ttOrderline").

            return OrderLineStatus.
        end.
        private set.

    define public static property Ordernum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Ordernum) then Ordernum = new com.quarix.data.ttfield("Ordernum", "ttOrderline").

            return Ordernum.
        end.
        private set.

    define public static property Price as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Price) then Price = new com.quarix.data.ttfield("Price", "ttOrderline").

            return Price.
        end.
        private set.

    define public static property Qty as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Qty) then Qty = new com.quarix.data.ttfield("Qty", "ttOrderline").

            return Qty.
        end.
        private set.

    define public static property ItemName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(ItemName) then ItemName = new com.quarix.data.ttfield("ItemName", "ttOrderline").

            return ItemName.
        end.
        private set.

    constructor static ttorderline():
    end constructor.

    destructor public ttorderline():
        delete object rowid no-error.
        delete object rowstate no-error.
        delete object SortOrder no-error.
        delete object Discount no-error.
        delete object ExtendedPrice no-error.
        delete object Itemnum no-error.
        delete object Linenum no-error.
        delete object OrderLineStatus no-error.
        delete object Ordernum no-error.
        delete object Price no-error.
        delete object Qty no-error.
        delete object ItemName no-error.
    end destructor.

end class.
