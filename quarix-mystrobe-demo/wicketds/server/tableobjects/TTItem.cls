class wicketds.server.tableobjects.TTItem inherits com.quarix.data.tttableobject final:

    &scoped-define idxrowid 1

    &scoped-define idxrowstate 2

    &scoped-define idxSortOrder 3

    &scoped-define idxCatDescription 4

    &scoped-define idxItemName 5

    &scoped-define idxItemnum 6

    &scoped-define idxPrice 7

    &scoped-define idxWeight 8

    &scoped-define idxCategory1 9

    &scoped-define idxCategory2 10

    define public variable FieldInstanceList# as com.quarix.data.ttfield extent 10 no-undo.

    define private static property Instance as wicketds.server.tableobjects.TTItem no-undo
        private get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTItem().

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

    define public static property CatDescription as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCatDescription}].
        end.
        private set.

    define public static property ItemName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxItemName}].
        end.
        private set.

    define public static property Itemnum as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxItemnum}].
        end.
        private set.

    define public static property Price as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxPrice}].
        end.
        private set.

    define public static property Weight as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxWeight}].
        end.
        private set.

    define public static property Category1 as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCategory1}].
        end.
        private set.

    define public static property Category2 as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCategory2}].
        end.
        private set.

    constructor private TTItem():

        TableObjectName = "ttItem".

        FieldInstanceList#[{&idxrowid}] = com.quarix.data.ttfield:newField(input "rowid", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowid}]) then return error.

        FieldInstanceList#[{&idxrowstate}] = com.quarix.data.ttfield:newField(input "rowstate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowstate}]) then return error.

        FieldInstanceList#[{&idxSortOrder}] = com.quarix.data.ttfield:newField(input "SortOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSortOrder}]) then return error.

        FieldInstanceList#[{&idxCatDescription}] = com.quarix.data.ttfield:newField(input "CatDescription", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCatDescription}]) then return error.

        FieldInstanceList#[{&idxItemName}] = com.quarix.data.ttfield:newField(input "ItemName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxItemName}]) then return error.

        FieldInstanceList#[{&idxItemnum}] = com.quarix.data.ttfield:newField(input "Itemnum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxItemnum}]) then return error.

        FieldInstanceList#[{&idxPrice}] = com.quarix.data.ttfield:newField(input "Price", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPrice}]) then return error.

        FieldInstanceList#[{&idxWeight}] = com.quarix.data.ttfield:newField(input "Weight", input this-object).
        if not valid-object(FieldInstanceList#[{&idxWeight}]) then return error.

        FieldInstanceList#[{&idxCategory1}] = com.quarix.data.ttfield:newField(input "Category1", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCategory1}]) then return error.

        FieldInstanceList#[{&idxCategory2}] = com.quarix.data.ttfield:newField(input "Category2", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCategory2}]) then return error.

    end constructor.

    method public static override integer NumberOfFields():
        return extent(Instance:FieldInstanceList#).
    end method.

    destructor public TTItem():
        delete object FieldInstanceList#[{&idxrowid}] no-error.
        delete object FieldInstanceList#[{&idxrowstate}] no-error.
        delete object FieldInstanceList#[{&idxSortOrder}] no-error.
        delete object FieldInstanceList#[{&idxCatDescription}] no-error.
        delete object FieldInstanceList#[{&idxItemName}] no-error.
        delete object FieldInstanceList#[{&idxItemnum}] no-error.
        delete object FieldInstanceList#[{&idxPrice}] no-error.
        delete object FieldInstanceList#[{&idxWeight}] no-error.
        delete object FieldInstanceList#[{&idxCategory1}] no-error.
        delete object FieldInstanceList#[{&idxCategory2}] no-error.
    end destructor.

end class.
