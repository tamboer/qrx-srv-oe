define  temp-table ttItem no-undo
    before-table btItem
    {com/quarix/data/sortorder.i &fields="
    field CatDescription as character
    field ItemName  as character
    field Itemnum as integer
    field Price as decimal
    field Weight as decimal
    field Category1 as character
    field Category2 as character
    "}
    index PK_Item is unique Itemnum.