define  temp-table ttEmployee no-undo
    before-table btEmployee
    {com/quarix/data/sortorder.i &fields="
    field EmpNum  as integer
    field Address   as character
    field Address2   as character
    field Birthdate  as date
    field City as character
    field DeptCode  as character
    field FirstName  as character
    field LastName  as character
    field HomePhone as character
    field WorkPhone as character
    field Position as character
    field StartDate as date
    field PostalCode as character
    field State as character
    field DeptName as character
    field StateName as character
    "}
    index PK_Employee is unique EmpNum.