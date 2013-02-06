define {&scope} temp-table {&prefix}ttstateidlist no-undo {&REFERENCE-ONLY}
	{com/quarix/data/idlistorder.i &fields="
	field StateCode as character ~
	"}
	index idxStateCode is unique StateCode
.
