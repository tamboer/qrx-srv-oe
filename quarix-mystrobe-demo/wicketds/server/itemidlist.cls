
routine-level on error undo, throw.

using Progress.Lang.*.

class wicketds.server.itemidlist inherits com.quarix.data.IdList:

	{wicketds/server/ttitemidlist.i}

	constructor itemidlist():
		hbttHandle = buffer ttitemidlist:handle.
	end constructor.

	method public void addItemNum(input iItemNum as integer):

		AddRecord('Itemnum':U, string(iItemNum)).

	end method.

end class.