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

    &scoped-define idxNumFields 10

    define public static property Instance as wicketds.server.tableobjects.TTItem no-undo
        public get:
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
            return cast(Instance:FieldInstanceList#[{&idxrowid}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxrowstate}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxSortOrder}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property CatDescription as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCatDescription}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property ItemName as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxItemName}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Itemnum as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxItemnum}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Price as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxPrice}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Weight as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxWeight}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Category1 as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCategory1}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Category2 as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCategory2}], com.quarix.data.ttfield).
        end.
        private set.

    constructor private TTItem():

        TableObjectName = "ttItem".

        extent(FieldInstanceList#) = {&idxNumFields}.

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

    method public static integer NumberOfFields():
        return Instance:FieldCount().
    end method.

end class.
