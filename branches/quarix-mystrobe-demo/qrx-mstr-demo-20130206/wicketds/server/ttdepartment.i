define  temp-table ttDepartment no-undo
    before-table btDepartment
    {com/quarix/data/sortorder.i &fields="
    field DeptCode  as character
    field DeptName   as character
    "}
    index PK_Department is unique DeptCode.