define {&scope} temp-table {&prefix}ttCustomerIdList no-undo {&REFERENCE-ONLY}
    {com/quarix/data/idlistorder.i &fields="
	field custNum as int ~
	"}
    index idxUQ is unique custNum.
