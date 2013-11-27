class wicketds.server.tableobjects.ttsalesrepidlist inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttsalesrepidlist"
        get.
        private set.

    define public static property IdListOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(IdListOrder) then IdListOrder = new com.quarix.data.ttfield("IdListOrder", "ttsalesrepidlist").

            return IdListOrder.
        end.
        private set.

    define public static property HashCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(HashCode) then HashCode = new com.quarix.data.ttfield("HashCode", "ttsalesrepidlist").

            return HashCode.
        end.
        private set.

    define public static property SalesRep as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SalesRep) then SalesRep = new com.quarix.data.ttfield("SalesRep", "ttsalesrepidlist").

            return SalesRep.
        end.
        private set.

    constructor static ttsalesrepidlist():
    end constructor.

    destructor public ttsalesrepidlist():
        delete object IdListOrder no-error.
        delete object HashCode no-error.
        delete object SalesRep no-error.
    end destructor.

end class.
