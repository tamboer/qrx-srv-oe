define  temp-table ttOrderline no-undo
    before-table btOrderline
    {com/quarix/data/sortorder.i &fields="
    field Discount  as integer
    field ExtendedPrice   as decimal
    field Itemnum   as integer
    field Linenum  as integer
    field OrderLineStatus as character
    field Ordernum  as integer
    field Price as decimal
    field Qty as integer
    field ItemName as character
    "}
    index PK_OrderLine is unique Ordernum Linenum.