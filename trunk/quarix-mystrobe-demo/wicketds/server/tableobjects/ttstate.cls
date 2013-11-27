class wicketds.server.tableobjects.ttstate inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttState"
        get.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowid) then rowid = new com.quarix.data.ttfield("rowid", "ttState").

            return rowid.
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowstate) then rowstate = new com.quarix.data.ttfield("rowstate", "ttState").

            return rowstate.
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SortOrder) then SortOrder = new com.quarix.data.ttfield("SortOrder", "ttState").

            return SortOrder.
        end.
        private set.

    define public static property Region as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Region) then Region = new com.quarix.data.ttfield("Region", "ttState").

            return Region.
        end.
        private set.

    define public static property StateCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(StateCode) then StateCode = new com.quarix.data.ttfield("StateCode", "ttState").

            return StateCode.
        end.
        private set.

    define public static property StateName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(StateName) then StateName = new com.quarix.data.ttfield("StateName", "ttState").

            return StateName.
        end.
        private set.

    constructor static ttstate():
    end constructor.

    destructor public ttstate():
        delete object rowid no-error.
        delete object rowstate no-error.
        delete object SortOrder no-error.
        delete object Region no-error.
        delete object StateCode no-error.
        delete object StateName no-error.
    end destructor.

end class.
