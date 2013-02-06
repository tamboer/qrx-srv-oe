define  temp-table ttSalesrep no-undo
    before-table btSalesrep
    {com/quarix/data/sortorder.i &fields="
    field SalesRep as character
    field RepName  as character
    field Region as character
    "}
    index PK_Salesrep is unique SalesRep.