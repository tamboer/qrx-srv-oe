class wicketds.server.tableobjects.TTStateidlist inherits com.quarix.data.tttableobject final:

    &scoped-define idxIdListOrder 1

    &scoped-define idxHashCode 2

    &scoped-define idxStateCode 3

    define public variable FieldInstanceList# as com.quarix.data.ttfield extent 3 no-undo.

    define private static property Instance as wicketds.server.tableobjects.TTStateidlist no-undo
        private get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTStateidlist().

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
            return Instance:FieldInstanceList#[{&idxIdListOrder}].
        end.
        private set.

    define public static property HashCode as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxHashCode}].
        end.
        private set.

    define public static property StateCode as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxStateCode}].
        end.
        private set.

    constructor private TTStateidlist():

        TableObjectName = "ttstateidlist".

        FieldInstanceList#[{&idxIdListOrder}] = com.quarix.data.ttfield:newField(input "IdListOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxIdListOrder}]) then return error.

        FieldInstanceList#[{&idxHashCode}] = com.quarix.data.ttfield:newField(input "HashCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxHashCode}]) then return error.

        FieldInstanceList#[{&idxStateCode}] = com.quarix.data.ttfield:newField(input "StateCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxStateCode}]) then return error.

    end constructor.

    method public static override integer NumberOfFields():
        return extent(Instance:FieldInstanceList#).
    end method.

    destructor public TTStateidlist():
        delete object FieldInstanceList#[{&idxIdListOrder}] no-error.
        delete object FieldInstanceList#[{&idxHashCode}] no-error.
        delete object FieldInstanceList#[{&idxStateCode}] no-error.
    end destructor.

end class.
