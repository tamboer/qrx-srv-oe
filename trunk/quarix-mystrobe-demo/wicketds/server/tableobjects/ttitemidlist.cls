class wicketds.server.tableobjects.ttitemidlist inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttitemidlist"
        get.
        private set.

    define public static property IdListOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(IdListOrder) then IdListOrder = new com.quarix.data.ttfield("IdListOrder", "ttitemidlist").

            return IdListOrder.
        end.
        private set.

    define public static property HashCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(HashCode) then HashCode = new com.quarix.data.ttfield("HashCode", "ttitemidlist").

            return HashCode.
        end.
        private set.

    define public static property Itemnum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Itemnum) then Itemnum = new com.quarix.data.ttfield("Itemnum", "ttitemidlist").

            return Itemnum.
        end.
        private set.

    constructor static ttitemidlist():
    end constructor.

    destructor public ttitemidlist():
        delete object IdListOrder no-error.
        delete object HashCode no-error.
        delete object Itemnum no-error.
    end destructor.

end class.
