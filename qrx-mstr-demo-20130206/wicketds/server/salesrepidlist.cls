
routine-level on error undo, throw.

using Progress.Lang.*.

class wicketds.server.salesrepidlist inherits com.quarix.data.IdList:

	{wicketds/server/ttsalesrepidlist.i}

	constructor salesrepidlist():
		hbttHandle = buffer ttsalesrepidlist:handle.
	end constructor.

	method public void addSalesRep(input cSalesRep as character):

		AddRecord('SalesRep':U, string(cSalesRep)).

	end method.

end class.
