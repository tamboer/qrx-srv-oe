define {&scope} temp-table {&prefix}ttcustomeridlist no-undo {&REFERENCE-ONLY}
	{com/quarix/data/idlistorder.i &fields="
	field CustNum as integer ~
	"}
	index idxCustNum is unique CustNum
.
