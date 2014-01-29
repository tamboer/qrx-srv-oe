class wicketds.server.tableobjects.TTState inherits com.quarix.data.tttableobject final:

    &scoped-define idxrowid 1

    &scoped-define idxrowstate 2

    &scoped-define idxSortOrder 3

    &scoped-define idxRegion 4

    &scoped-define idxStateCode 5

    &scoped-define idxStateName 6

    define public variable FieldInstanceList# as com.quarix.data.ttfield extent 6 no-undo.

    define private static property Instance as wicketds.server.tableobjects.TTState no-undo
        private get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTState().

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

    define public static property Region as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxRegion}].
        end.
        private set.

    define public static property StateCode as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxStateCode}].
        end.
        private set.

    define public static property StateName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxStateName}].
        end.
        private set.

    constructor private TTState():

        TableObjectName = "ttState".

        FieldInstanceList#[{&idxrowid}] = com.quarix.data.ttfield:newField(input "rowid", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowid}]) then return error.

        FieldInstanceList#[{&idxrowstate}] = com.quarix.data.ttfield:newField(input "rowstate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowstate}]) then return error.

        FieldInstanceList#[{&idxSortOrder}] = com.quarix.data.ttfield:newField(input "SortOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSortOrder}]) then return error.

        FieldInstanceList#[{&idxRegion}] = com.quarix.data.ttfield:newField(input "Region", input this-object).
        if not valid-object(FieldInstanceList#[{&idxRegion}]) then return error.

        FieldInstanceList#[{&idxStateCode}] = com.quarix.data.ttfield:newField(input "StateCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxStateCode}]) then return error.

        FieldInstanceList#[{&idxStateName}] = com.quarix.data.ttfield:newField(input "StateName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxStateName}]) then return error.

    end constructor.

    method public static override integer NumberOfFields():
        return extent(Instance:FieldInstanceList#).
    end method.

    destructor public TTState():
        delete object FieldInstanceList#[{&idxrowid}] no-error.
        delete object FieldInstanceList#[{&idxrowstate}] no-error.
        delete object FieldInstanceList#[{&idxSortOrder}] no-error.
        delete object FieldInstanceList#[{&idxRegion}] no-error.
        delete object FieldInstanceList#[{&idxStateCode}] no-error.
        delete object FieldInstanceList#[{&idxStateName}] no-error.
    end destructor.

end class.
