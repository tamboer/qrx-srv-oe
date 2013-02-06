define {&scope} temp-table {&prefix}ttsalesrepidlist no-undo {&REFERENCE-ONLY}
	{com/quarix/data/idlistorder.i &fields="
	field SalesRep as character ~
	"}
	index idxSalesRep is unique SalesRep
.
