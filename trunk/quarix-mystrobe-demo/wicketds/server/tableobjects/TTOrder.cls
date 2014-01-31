class wicketds.server.tableobjects.TTOrder inherits com.quarix.data.tttableobject final:

    &scoped-define idxrowid 1

    &scoped-define idxrowstate 2

    &scoped-define idxSortOrder 3

    &scoped-define idxBillToID 4

    &scoped-define idxCarrier 5

    &scoped-define idxCreditCard 6

    &scoped-define idxCustNum 7

    &scoped-define idxInstructions 8

    &scoped-define idxOrderDate 9

    &scoped-define idxOrdernum 10

    &scoped-define idxOrderStatus 11

    &scoped-define idxPO 12

    &scoped-define idxPromiseDate 13

    &scoped-define idxSalesRep 14

    &scoped-define idxShipDate 15

    &scoped-define idxShipToID 16

    &scoped-define idxTerms 17

    &scoped-define idxWarehouseNum 18

    &scoped-define idxCustName 19

    &scoped-define idxSalesRepName 20

    &scoped-define idxNumFields 20

    define public static property Instance as wicketds.server.tableobjects.TTOrder no-undo
        public get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTOrder().

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

    define public static property BillToID as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxBillToID}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Carrier as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCarrier}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property CreditCard as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCreditCard}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property CustNum as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCustNum}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Instructions as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxInstructions}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property OrderDate as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxOrderDate}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Ordernum as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxOrdernum}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property OrderStatus as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxOrderStatus}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property PO as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxPO}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property PromiseDate as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxPromiseDate}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property SalesRep as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxSalesRep}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property ShipDate as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxShipDate}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property ShipToID as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxShipToID}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Terms as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxTerms}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property WarehouseNum as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxWarehouseNum}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property CustName as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCustName}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property SalesRepName as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxSalesRepName}], com.quarix.data.ttfield).
        end.
        private set.

    constructor private TTOrder():

        TableObjectName = "ttOrder".

        extent(FieldInstanceList#) = {&idxNumFields}.

        FieldInstanceList#[{&idxrowid}] = com.quarix.data.ttfield:newField(input "rowid", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowid}]) then return error.

        FieldInstanceList#[{&idxrowstate}] = com.quarix.data.ttfield:newField(input "rowstate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowstate}]) then return error.

        FieldInstanceList#[{&idxSortOrder}] = com.quarix.data.ttfield:newField(input "SortOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSortOrder}]) then return error.

        FieldInstanceList#[{&idxBillToID}] = com.quarix.data.ttfield:newField(input "BillToID", input this-object).
        if not valid-object(FieldInstanceList#[{&idxBillToID}]) then return error.

        FieldInstanceList#[{&idxCarrier}] = com.quarix.data.ttfield:newField(input "Carrier", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCarrier}]) then return error.

        FieldInstanceList#[{&idxCreditCard}] = com.quarix.data.ttfield:newField(input "CreditCard", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCreditCard}]) then return error.

        FieldInstanceList#[{&idxCustNum}] = com.quarix.data.ttfield:newField(input "CustNum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCustNum}]) then return error.

        FieldInstanceList#[{&idxInstructions}] = com.quarix.data.ttfield:newField(input "Instructions", input this-object).
        if not valid-object(FieldInstanceList#[{&idxInstructions}]) then return error.

        FieldInstanceList#[{&idxOrderDate}] = com.quarix.data.ttfield:newField(input "OrderDate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxOrderDate}]) then return error.

        FieldInstanceList#[{&idxOrdernum}] = com.quarix.data.ttfield:newField(input "Ordernum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxOrdernum}]) then return error.

        FieldInstanceList#[{&idxOrderStatus}] = com.quarix.data.ttfield:newField(input "OrderStatus", input this-object).
        if not valid-object(FieldInstanceList#[{&idxOrderStatus}]) then return error.

        FieldInstanceList#[{&idxPO}] = com.quarix.data.ttfield:newField(input "PO", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPO}]) then return error.

        FieldInstanceList#[{&idxPromiseDate}] = com.quarix.data.ttfield:newField(input "PromiseDate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPromiseDate}]) then return error.

        FieldInstanceList#[{&idxSalesRep}] = com.quarix.data.ttfield:newField(input "SalesRep", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSalesRep}]) then return error.

        FieldInstanceList#[{&idxShipDate}] = com.quarix.data.ttfield:newField(input "ShipDate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxShipDate}]) then return error.

        FieldInstanceList#[{&idxShipToID}] = com.quarix.data.ttfield:newField(input "ShipToID", input this-object).
        if not valid-object(FieldInstanceList#[{&idxShipToID}]) then return error.

        FieldInstanceList#[{&idxTerms}] = com.quarix.data.ttfield:newField(input "Terms", input this-object).
        if not valid-object(FieldInstanceList#[{&idxTerms}]) then return error.

        FieldInstanceList#[{&idxWarehouseNum}] = com.quarix.data.ttfield:newField(input "WarehouseNum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxWarehouseNum}]) then return error.

        FieldInstanceList#[{&idxCustName}] = com.quarix.data.ttfield:newField(input "CustName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCustName}]) then return error.

        FieldInstanceList#[{&idxSalesRepName}] = com.quarix.data.ttfield:newField(input "SalesRepName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSalesRepName}]) then return error.

    end constructor.

    method public static integer NumberOfFields():
        return Instance:FieldCount().
    end method.

end class.
