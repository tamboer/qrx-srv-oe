class wicketds.server.tableobjects.TTDepartment inherits com.quarix.data.tttableobject final:

    &scoped-define idxrowid 1

    &scoped-define idxrowstate 2

    &scoped-define idxSortOrder 3

    &scoped-define idxDeptCode 4

    &scoped-define idxDeptName 5

    define public variable FieldInstanceList# as com.quarix.data.ttfield extent 5 no-undo.

    define private static property Instance as wicketds.server.tableobjects.TTDepartment no-undo
        private get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTDepartment().

            return Instance.
        end.
        private set.

    define public static property TableName as character no-undo
        get:
            return Instance:TableObjectName.
        end.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxrowid}].
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxrowstate}].
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxSortOrder}].
        end.
        private set.

    define public static property DeptCode as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxDeptCode}].
        end.
        private set.

    define public static property DeptName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxDeptName}].
        end.
        private set.

    constructor private TTDepartment():

        TableObjectName = "ttDepartment".

        FieldInstanceList#[{&idxrowid}] = com.quarix.data.ttfield:newField(input "rowid", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowid}]) then return error.

        FieldInstanceList#[{&idxrowstate}] = com.quarix.data.ttfield:newField(input "rowstate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowstate}]) then return error.

        FieldInstanceList#[{&idxSortOrder}] = com.quarix.data.ttfield:newField(input "SortOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSortOrder}]) then return error.

        FieldInstanceList#[{&idxDeptCode}] = com.quarix.data.ttfield:newField(input "DeptCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxDeptCode}]) then return error.

        FieldInstanceList#[{&idxDeptName}] = com.quarix.data.ttfield:newField(input "DeptName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxDeptName}]) then return error.

    end constructor.

    method public static override integer NumberOfFields():
        return extent(Instance:FieldInstanceList#).
    end method.

    destructor public TTDepartment():
        delete object FieldInstanceList#[{&idxrowid}] no-error.
        delete object FieldInstanceList#[{&idxrowstate}] no-error.
        delete object FieldInstanceList#[{&idxSortOrder}] no-error.
        delete object FieldInstanceList#[{&idxDeptCode}] no-error.
        delete object FieldInstanceList#[{&idxDeptName}] no-error.
    end destructor.

end class.
