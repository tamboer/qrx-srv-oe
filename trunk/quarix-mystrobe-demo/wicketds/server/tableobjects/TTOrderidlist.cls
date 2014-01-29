class wicketds.server.tableobjects.TTOrderidlist inherits com.quarix.data.tttableobject final:

    &scoped-define idxIdListOrder 1

    &scoped-define idxHashCode 2

    &scoped-define idxOrdernum 3

    define public variable FieldInstanceList# as com.quarix.data.ttfield extent 3 no-undo.

    define private static property Instance as wicketds.server.tableobjects.TTOrderidlist no-undo
        private get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTOrderidlist().

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

    define public static property Ordernum as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxOrdernum}].
        end.
        private set.

    constructor private TTOrderidlist():

        TableObjectName = "ttorderidlist".

        FieldInstanceList#[{&idxIdListOrder}] = com.quarix.data.ttfield:newField(input "IdListOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxIdListOrder}]) then return error.

        FieldInstanceList#[{&idxHashCode}] = com.quarix.data.ttfield:newField(input "HashCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxHashCode}]) then return error.

        FieldInstanceList#[{&idxOrdernum}] = com.quarix.data.ttfield:newField(input "Ordernum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxOrdernum}]) then return error.

    end constructor.

    method public static override integer NumberOfFields():
        return extent(Instance:FieldInstanceList#).
    end method.

    destructor public TTOrderidlist():
        delete object FieldInstanceList#[{&idxIdListOrder}] no-error.
        delete object FieldInstanceList#[{&idxHashCode}] no-error.
        delete object FieldInstanceList#[{&idxOrdernum}] no-error.
    end destructor.

end class.
