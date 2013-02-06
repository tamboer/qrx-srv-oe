define {&scope} temp-table {&prefix}ttdepartmentidlist no-undo {&REFERENCE-ONLY}
	{com/quarix/data/idlistorder.i &fields="
	field DeptCode as character ~
	"}
	index idxDeptCode is unique DeptCode
.
