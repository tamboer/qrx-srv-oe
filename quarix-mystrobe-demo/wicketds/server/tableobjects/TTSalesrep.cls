class wicketds.server.tableobjects.TTSalesrep inherits com.quarix.data.tableobject final:

    &scoped-define idxrowid 1

    &scoped-define idxrowstate 2

    &scoped-define idxSortOrder 3

    &scoped-define idxSalesRep 4

    &scoped-define idxRepName 5

    &scoped-define idxRegion 6

    define public variable FieldInstanceList# as com.quarix.data.ttfield extent 6 no-undo.

    define private static property Instance as wicketds.server.tableobjects.TTSalesrep no-undo
        private get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTSalesrep().

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

    define public static property SalesRep as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxSalesRep}].
        end.
        private set.

    define public static property RepName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxRepName}].
        end.
        private set.

    define public static property Region as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxRegion}].
        end.
        private set.

    constructor private TTSalesrep():

        TableObjectName = "ttSalesrep".

        FieldInstanceList#[{&idxrowid}] = com.quarix.data.ttfield:newField(input "rowid", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowid}]) then return error.

        FieldInstanceList#[{&idxrowstate}] = com.quarix.data.ttfield:newField(input "rowstate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowstate}]) then return error.

        FieldInstanceList#[{&idxSortOrder}] = com.quarix.data.ttfield:newField(input "SortOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSortOrder}]) then return error.

        FieldInstanceList#[{&idxSalesRep}] = com.quarix.data.ttfield:newField(input "SalesRep", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSalesRep}]) then return error.

        FieldInstanceList#[{&idxRepName}] = com.quarix.data.ttfield:newField(input "RepName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxRepName}]) then return error.

        FieldInstanceList#[{&idxRegion}] = com.quarix.data.ttfield:newField(input "Region", input this-object).
        if not valid-object(FieldInstanceList#[{&idxRegion}]) then return error.

    end constructor.

    destructor public TTSalesrep():
        delete object FieldInstanceList#[{&idxrowid}] no-error.
        delete object FieldInstanceList#[{&idxrowstate}] no-error.
        delete object FieldInstanceList#[{&idxSortOrder}] no-error.
        delete object FieldInstanceList#[{&idxSalesRep}] no-error.
        delete object FieldInstanceList#[{&idxRepName}] no-error.
        delete object FieldInstanceList#[{&idxRegion}] no-error.
    end destructor.

end class.
