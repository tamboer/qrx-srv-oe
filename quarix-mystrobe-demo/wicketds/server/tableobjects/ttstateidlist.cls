class wicketds.server.tableobjects.ttstateidlist inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttstateidlist"
        get.
        private set.

    define public static property IdListOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(IdListOrder) then IdListOrder = new com.quarix.data.ttfield("IdListOrder", "ttstateidlist").

            return IdListOrder.
        end.
        private set.

    define public static property HashCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(HashCode) then HashCode = new com.quarix.data.ttfield("HashCode", "ttstateidlist").

            return HashCode.
        end.
        private set.

    define public static property StateCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(StateCode) then StateCode = new com.quarix.data.ttfield("StateCode", "ttstateidlist").

            return StateCode.
        end.
        private set.

    constructor static ttstateidlist():
    end constructor.

    destructor public ttstateidlist():
        delete object IdListOrder no-error.
        delete object HashCode no-error.
        delete object StateCode no-error.
    end destructor.

end class.
