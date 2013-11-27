class wicketds.server.tableobjects.ttdepartment inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttDepartment"
        get.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowid) then rowid = new com.quarix.data.ttfield("rowid", "ttDepartment").

            return rowid.
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowstate) then rowstate = new com.quarix.data.ttfield("rowstate", "ttDepartment").

            return rowstate.
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SortOrder) then SortOrder = new com.quarix.data.ttfield("SortOrder", "ttDepartment").

            return SortOrder.
        end.
        private set.

    define public static property DeptCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(DeptCode) then DeptCode = new com.quarix.data.ttfield("DeptCode", "ttDepartment").

            return DeptCode.
        end.
        private set.

    define public static property DeptName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(DeptName) then DeptName = new com.quarix.data.ttfield("DeptName", "ttDepartment").

            return DeptName.
        end.
        private set.

    constructor static ttdepartment():
    end constructor.

    destructor public ttdepartment():
        delete object rowid no-error.
        delete object rowstate no-error.
        delete object SortOrder no-error.
        delete object DeptCode no-error.
        delete object DeptName no-error.
    end destructor.

end class.
