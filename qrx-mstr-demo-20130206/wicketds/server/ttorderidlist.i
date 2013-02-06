define {&scope} temp-table {&prefix}ttorderidlist no-undo {&REFERENCE-ONLY}
	{com/quarix/data/idlistorder.i &fields="
	field Ordernum as integer ~
	"}
	index idxOrdernum is unique Ordernum
.
