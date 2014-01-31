class wicketds.server.tableobjects.TTOrderline inherits com.quarix.data.tttableobject final:

    &scoped-define idxrowid 1

    &scoped-define idxrowstate 2

    &scoped-define idxSortOrder 3

    &scoped-define idxDiscount 4

    &scoped-define idxExtendedPrice 5

    &scoped-define idxItemnum 6

    &scoped-define idxLinenum 7

    &scoped-define idxOrderLineStatus 8

    &scoped-define idxOrdernum 9

    &scoped-define idxPrice 10

    &scoped-define idxQty 11

    &scoped-define idxItemName 12

    &scoped-define idxNumFields 12

    define public static property Instance as wicketds.server.tableobjects.TTOrderline no-undo
        public get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTOrderline().

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

    define public static property Discount as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxDiscount}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property ExtendedPrice as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxExtendedPrice}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Itemnum as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxItemnum}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Linenum as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxLinenum}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property OrderLineStatus as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxOrderLineStatus}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Ordernum as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxOrdernum}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Price as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxPrice}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Qty as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxQty}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property ItemName as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxItemName}], com.quarix.data.ttfield).
        end.
        private set.

    constructor private TTOrderline():

        TableObjectName = "ttOrderline".

        extent(FieldInstanceList#) = {&idxNumFields}.

        FieldInstanceList#[{&idxrowid}] = com.quarix.data.ttfield:newField(input "rowid", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowid}]) then return error.

        FieldInstanceList#[{&idxrowstate}] = com.quarix.data.ttfield:newField(input "rowstate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowstate}]) then return error.

        FieldInstanceList#[{&idxSortOrder}] = com.quarix.data.ttfield:newField(input "SortOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSortOrder}]) then return error.

        FieldInstanceList#[{&idxDiscount}] = com.quarix.data.ttfield:newField(input "Discount", input this-object).
        if not valid-object(FieldInstanceList#[{&idxDiscount}]) then return error.

        FieldInstanceList#[{&idxExtendedPrice}] = com.quarix.data.ttfield:newField(input "ExtendedPrice", input this-object).
        if not valid-object(FieldInstanceList#[{&idxExtendedPrice}]) then return error.

        FieldInstanceList#[{&idxItemnum}] = com.quarix.data.ttfield:newField(input "Itemnum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxItemnum}]) then return error.

        FieldInstanceList#[{&idxLinenum}] = com.quarix.data.ttfield:newField(input "Linenum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxLinenum}]) then return error.

        FieldInstanceList#[{&idxOrderLineStatus}] = com.quarix.data.ttfield:newField(input "OrderLineStatus", input this-object).
        if not valid-object(FieldInstanceList#[{&idxOrderLineStatus}]) then return error.

        FieldInstanceList#[{&idxOrdernum}] = com.quarix.data.ttfield:newField(input "Ordernum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxOrdernum}]) then return error.

        FieldInstanceList#[{&idxPrice}] = com.quarix.data.ttfield:newField(input "Price", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPrice}]) then return error.

        FieldInstanceList#[{&idxQty}] = com.quarix.data.ttfield:newField(input "Qty", input this-object).
        if not valid-object(FieldInstanceList#[{&idxQty}]) then return error.

        FieldInstanceList#[{&idxItemName}] = com.quarix.data.ttfield:newField(input "ItemName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxItemName}]) then return error.

    end constructor.

    method public static integer NumberOfFields():
        return Instance:FieldCount().
    end method.

end class.
