class wicketds.server.tableobjects.TTDepartmentidlist inherits com.quarix.data.tttableobject final:

    &scoped-define idxIdListOrder 1

    &scoped-define idxHashCode 2

    &scoped-define idxDeptCode 3

    &scoped-define idxNumFields 3

    define public static property Instance as wicketds.server.tableobjects.TTDepartmentidlist no-undo
        public get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTDepartmentidlist().

            return Instance.
        end.
        private set.

    define public static property TableName as character no-undo
        get:
            return Instance:TableObjectName.
        end.
        private set.

    define public static property IdListOrder as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxIdListOrder}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property HashCode as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxHashCode}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property DeptCode as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxDeptCode}], com.quarix.data.ttfield).
        end.
        private set.

    constructor private TTDepartmentidlist():

        TableObjectName = "ttdepartmentidlist".

        extent(FieldInstanceList#) = {&idxNumFields}.

        FieldInstanceList#[{&idxIdListOrder}] = com.quarix.data.ttfield:newField(input "IdListOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxIdListOrder}]) then return error.

        FieldInstanceList#[{&idxHashCode}] = com.quarix.data.ttfield:newField(input "HashCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxHashCode}]) then return error.

        FieldInstanceList#[{&idxDeptCode}] = com.quarix.data.ttfield:newField(input "DeptCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxDeptCode}]) then return error.

    end constructor.

    method public static integer NumberOfFields():
        return Instance:FieldCount().
    end method.

end class.
