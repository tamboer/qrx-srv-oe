define  temp-table ttOrder no-undo
    before-table btOrder
    {com/quarix/data/sortorder.i &fields="
    field BillToID  as integer
    field Carrier   as character
    field CreditCard   as character
    field CustNum  as integer
    field Instructions   as character
    field OrderDate   as date
    field Ordernum  as integer
    field OrderStatus  as character
    field PO as character
    field PromiseDate as date
    field SalesRep as character
    field ShipDate as date
    field ShipToID as integer
    field Terms as character
    field WarehouseNum as integer
    field CustName as character
    field SalesRepName  as character
    "}
    index PK_Order is unique Ordernum.