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

    define private static property Instance as wicketds.server.tableobjects.TTOrder no-undo
        private get:
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

    define public static property BillToID as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxBillToID}].
        end.
        private set.

    define public static property Carrier as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCarrier}].
        end.
        private set.

    define public static property CreditCard as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCreditCard}].
        end.
        private set.

    define public static property CustNum as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCustNum}].
        end.
        private set.

    define public static property Instructions as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxInstructions}].
        end.
        private set.

    define public static property OrderDate as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxOrderDate}].
        end.
        private set.

    define public static property Ordernum as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxOrdernum}].
        end.
        private set.

    define public static property OrderStatus as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxOrderStatus}].
        end.
        private set.

    define public static property PO as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxPO}].
        end.
        private set.

    define public static property PromiseDate as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxPromiseDate}].
        end.
        private set.

    define public static property SalesRep as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxSalesRep}].
        end.
        private set.

    define public static property ShipDate as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxShipDate}].
        end.
        private set.

    define public static property ShipToID as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxShipToID}].
        end.
        private set.

    define public static property Terms as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxTerms}].
        end.
        private set.

    define public static property WarehouseNum as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxWarehouseNum}].
        end.
        private set.

    define public static property CustName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCustName}].
        end.
        private set.

    define public static property SalesRepName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxSalesRepName}].
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

    destructor public TTOrder():
        delete object FieldInstanceList#[{&idxrowid}] no-error.
        delete object FieldInstanceList#[{&idxrowstate}] no-error.
        delete object FieldInstanceList#[{&idxSortOrder}] no-error.
        delete object FieldInstanceList#[{&idxBillToID}] no-error.
        delete object FieldInstanceList#[{&idxCarrier}] no-error.
        delete object FieldInstanceList#[{&idxCreditCard}] no-error.
        delete object FieldInstanceList#[{&idxCustNum}] no-error.
        delete object FieldInstanceList#[{&idxInstructions}] no-error.
        delete object FieldInstanceList#[{&idxOrderDate}] no-error.
        delete object FieldInstanceList#[{&idxOrdernum}] no-error.
        delete object FieldInstanceList#[{&idxOrderStatus}] no-error.
        delete object FieldInstanceList#[{&idxPO}] no-error.
        delete object FieldInstanceList#[{&idxPromiseDate}] no-error.
        delete object FieldInstanceList#[{&idxSalesRep}] no-error.
        delete object FieldInstanceList#[{&idxShipDate}] no-error.
        delete object FieldInstanceList#[{&idxShipToID}] no-error.
        delete object FieldInstanceList#[{&idxTerms}] no-error.
        delete object FieldInstanceList#[{&idxWarehouseNum}] no-error.
        delete object FieldInstanceList#[{&idxCustName}] no-error.
        delete object FieldInstanceList#[{&idxSalesRepName}] no-error.
    end destructor.

end class.
