	field rowid		as character
	field rowstate	as character
	field SortOrder	as integer
{&fields}
	index PK_sortOrder is primary is unique SortOrder
	index idxRowid rowid
	index idxRowstate rowstate
