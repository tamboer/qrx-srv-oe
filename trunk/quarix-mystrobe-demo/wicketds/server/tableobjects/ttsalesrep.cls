class wicketds.server.tableobjects.ttsalesrep inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttSalesrep"
        get.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowid) then rowid = new com.quarix.data.ttfield("rowid", "ttSalesrep").

            return rowid.
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowstate) then rowstate = new com.quarix.data.ttfield("rowstate", "ttSalesrep").

            return rowstate.
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SortOrder) then SortOrder = new com.quarix.data.ttfield("SortOrder", "ttSalesrep").

            return SortOrder.
        end.
        private set.

    define public static property SalesRep as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SalesRep) then SalesRep = new com.quarix.data.ttfield("SalesRep", "ttSalesrep").

            return SalesRep.
        end.
        private set.

    define public static property RepName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(RepName) then RepName = new com.quarix.data.ttfield("RepName", "ttSalesrep").

            return RepName.
        end.
        private set.

    define public static property Region as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Region) then Region = new com.quarix.data.ttfield("Region", "ttSalesrep").

            return Region.
        end.
        private set.

    constructor static ttsalesrep():
    end constructor.

    destructor public ttsalesrep():
        delete object rowid no-error.
        delete object rowstate no-error.
        delete object SortOrder no-error.
        delete object SalesRep no-error.
        delete object RepName no-error.
        delete object Region no-error.
    end destructor.

end class.
