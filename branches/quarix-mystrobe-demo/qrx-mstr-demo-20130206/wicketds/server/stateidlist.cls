
routine-level on error undo, throw.

using Progress.Lang.*.

class wicketds.server.stateidlist inherits com.quarix.data.IdList:

	{wicketds/server/ttstateidlist.i}

	constructor stateidlist():
		hbttHandle = buffer ttstateidlist:handle.
	end constructor.

	method public void addStateCode(input cStateCode as character):

		AddRecord('StateCode':U, cStateCode).

	end method.

end class.
