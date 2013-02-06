define  temp-table ttState no-undo
    before-table btState
    {com/quarix/data/sortorder.i &fields="
    field Region  as character
    field StateCode  as character
    field StateName   as character
    "}
    index PK_State is unique StateCode.