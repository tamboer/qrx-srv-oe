	field IdListOrder	as integer
	field HashCode		as character
{&fields}
	index PK_IdListOrder is primary is unique IdListOrder
	index idxHashCode is unique HashCode
	index idxIdListOrderHashCode IdListOrder HashCode
