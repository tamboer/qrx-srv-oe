class wicketds.server.tableobjects.ttorderidlist inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttorderidlist"
        get.
        private set.

    define public static property IdListOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(IdListOrder) then IdListOrder = new com.quarix.data.ttfield("IdListOrder", "ttorderidlist").

            return IdListOrder.
        end.
        private set.

    define public static property HashCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(HashCode) then HashCode = new com.quarix.data.ttfield("HashCode", "ttorderidlist").

            return HashCode.
        end.
        private set.

    define public static property Ordernum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Ordernum) then Ordernum = new com.quarix.data.ttfield("Ordernum", "ttorderidlist").

            return Ordernum.
        end.
        private set.

    constructor static ttorderidlist():
    end constructor.

    destructor public ttorderidlist():
        delete object IdListOrder no-error.
        delete object HashCode no-error.
        delete object Ordernum no-error.
    end destructor.

end class.
