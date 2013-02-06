define {&scope} temp-table {&prefix}ttitemidlist no-undo {&REFERENCE-ONLY}
	{com/quarix/data/idlistorder.i &fields="
	field Itemnum as integer ~
	"}
	index idxItemnum is unique Itemnum
.
