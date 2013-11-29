class wicketds.server.tableobjects.TTPurchaseOrder inherits com.quarix.data.tableobject final:

    &scoped-define idxrowid 1

    &scoped-define idxrowstate 2

    &scoped-define idxSortOrder 3

    &scoped-define idxPONum 4

    &scoped-define idxDateEntered 5

    &scoped-define idxReceiveDate 6

    &scoped-define idxPOStatus 7

    &scoped-define idxSupplierIDNum 8

    define public variable FieldInstanceList# as com.quarix.data.ttfield extent 8 no-undo.

    define private static property Instance as wicketds.server.tableobjects.TTPurchaseOrder no-undo
        private get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTPurchaseOrder().

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

    define public static property PONum as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxPONum}].
        end.
        private set.

    define public static property DateEntered as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxDateEntered}].
        end.
        private set.

    define public static property ReceiveDate as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxReceiveDate}].
        end.
        private set.

    define public static property POStatus as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxPOStatus}].
        end.
        private set.

    define public static property SupplierIDNum as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxSupplierIDNum}].
        end.
        private set.

    constructor private TTPurchaseOrder():

        TableObjectName = "ttPurchaseOrder".

        FieldInstanceList#[{&idxrowid}] = com.quarix.data.ttfield:newField(input "rowid", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowid}]) then return error.

        FieldInstanceList#[{&idxrowstate}] = com.quarix.data.ttfield:newField(input "rowstate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowstate}]) then return error.

        FieldInstanceList#[{&idxSortOrder}] = com.quarix.data.ttfield:newField(input "SortOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSortOrder}]) then return error.

        FieldInstanceList#[{&idxPONum}] = com.quarix.data.ttfield:newField(input "PONum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPONum}]) then return error.

        FieldInstanceList#[{&idxDateEntered}] = com.quarix.data.ttfield:newField(input "DateEntered", input this-object).
        if not valid-object(FieldInstanceList#[{&idxDateEntered}]) then return error.

        FieldInstanceList#[{&idxReceiveDate}] = com.quarix.data.ttfield:newField(input "ReceiveDate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxReceiveDate}]) then return error.

        FieldInstanceList#[{&idxPOStatus}] = com.quarix.data.ttfield:newField(input "POStatus", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPOStatus}]) then return error.

        FieldInstanceList#[{&idxSupplierIDNum}] = com.quarix.data.ttfield:newField(input "SupplierIDNum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSupplierIDNum}]) then return error.

    end constructor.

    destructor public TTPurchaseOrder():
        delete object FieldInstanceList#[{&idxrowid}] no-error.
        delete object FieldInstanceList#[{&idxrowstate}] no-error.
        delete object FieldInstanceList#[{&idxSortOrder}] no-error.
        delete object FieldInstanceList#[{&idxPONum}] no-error.
        delete object FieldInstanceList#[{&idxDateEntered}] no-error.
        delete object FieldInstanceList#[{&idxReceiveDate}] no-error.
        delete object FieldInstanceList#[{&idxPOStatus}] no-error.
        delete object FieldInstanceList#[{&idxSupplierIDNum}] no-error.
    end destructor.

end class.
