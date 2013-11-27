class wicketds.server.tableobjects.ttdepartmentidlist inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttdepartmentidlist"
        get.
        private set.

    define public static property IdListOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(IdListOrder) then IdListOrder = new com.quarix.data.ttfield("IdListOrder", "ttdepartmentidlist").

            return IdListOrder.
        end.
        private set.

    define public static property HashCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(HashCode) then HashCode = new com.quarix.data.ttfield("HashCode", "ttdepartmentidlist").

            return HashCode.
        end.
        private set.

    define public static property DeptCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(DeptCode) then DeptCode = new com.quarix.data.ttfield("DeptCode", "ttdepartmentidlist").

            return DeptCode.
        end.
        private set.

    constructor static ttdepartmentidlist():
    end constructor.

    destructor public ttdepartmentidlist():
        delete object IdListOrder no-error.
        delete object HashCode no-error.
        delete object DeptCode no-error.
    end destructor.

end class.
