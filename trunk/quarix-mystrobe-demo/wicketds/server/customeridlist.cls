
routine-level on error undo, throw.

using Progress.Lang.*.

class wicketds.server.customeridlist inherits com.quarix.data.IdList:

	{wicketds/server/ttcustomeridlist.i}

	constructor customeridlist():
		hbttHandle = buffer ttcustomeridlist:handle.
	end constructor.

	method public void addCustNum(input iCustNum as integer):

		AddRecord('CustNum':U, string(iCustNum)).

	end method.

end class.
