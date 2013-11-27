class wicketds.server.tableobjects.ttitem inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttItem"
        get.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowid) then rowid = new com.quarix.data.ttfield("rowid", "ttItem").

            return rowid.
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowstate) then rowstate = new com.quarix.data.ttfield("rowstate", "ttItem").

            return rowstate.
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SortOrder) then SortOrder = new com.quarix.data.ttfield("SortOrder", "ttItem").

            return SortOrder.
        end.
        private set.

    define public static property CatDescription as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(CatDescription) then CatDescription = new com.quarix.data.ttfield("CatDescription", "ttItem").

            return CatDescription.
        end.
        private set.

    define public static property ItemName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(ItemName) then ItemName = new com.quarix.data.ttfield("ItemName", "ttItem").

            return ItemName.
        end.
        private set.

    define public static property Itemnum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Itemnum) then Itemnum = new com.quarix.data.ttfield("Itemnum", "ttItem").

            return Itemnum.
        end.
        private set.

    define public static property Price as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Price) then Price = new com.quarix.data.ttfield("Price", "ttItem").

            return Price.
        end.
        private set.

    define public static property Weight as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Weight) then Weight = new com.quarix.data.ttfield("Weight", "ttItem").

            return Weight.
        end.
        private set.

    define public static property Category1 as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Category1) then Category1 = new com.quarix.data.ttfield("Category1", "ttItem").

            return Category1.
        end.
        private set.

    define public static property Category2 as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Category2) then Category2 = new com.quarix.data.ttfield("Category2", "ttItem").

            return Category2.
        end.
        private set.

    constructor static ttitem():
    end constructor.

    destructor public ttitem():
        delete object rowid no-error.
        delete object rowstate no-error.
        delete object SortOrder no-error.
        delete object CatDescription no-error.
        delete object ItemName no-error.
        delete object Itemnum no-error.
        delete object Price no-error.
        delete object Weight no-error.
        delete object Category1 no-error.
        delete object Category2 no-error.
    end destructor.

end class.
