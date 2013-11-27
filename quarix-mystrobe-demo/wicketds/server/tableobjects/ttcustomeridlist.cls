class wicketds.server.tableobjects.ttcustomeridlist inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttcustomeridlist"
        get.
        private set.

    define public static property IdListOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(IdListOrder) then IdListOrder = new com.quarix.data.ttfield("IdListOrder", "ttcustomeridlist").

            return IdListOrder.
        end.
        private set.

    define public static property HashCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(HashCode) then HashCode = new com.quarix.data.ttfield("HashCode", "ttcustomeridlist").

            return HashCode.
        end.
        private set.

    define public static property CustNum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(CustNum) then CustNum = new com.quarix.data.ttfield("CustNum", "ttcustomeridlist").

            return CustNum.
        end.
        private set.

    constructor static ttcustomeridlist():
    end constructor.

    destructor public ttcustomeridlist():
        delete object IdListOrder no-error.
        delete object HashCode no-error.
        delete object CustNum no-error.
    end destructor.

end class.
