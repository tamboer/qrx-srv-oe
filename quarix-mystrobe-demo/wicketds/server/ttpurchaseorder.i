define  temp-table ttPurchaseOrder no-undo
    before-table btPurchaseOrder
    {com/quarix/data/sortorder.i &fields="
    field PONum         as integer
    field DateEntered   as date
    field ReceiveDate   as date
    field POStatus      as character
    field SupplierIDNum as integer
    "}
    index PK_PurchaseOrder is unique PONum.