
routine-level on error undo, throw.

using Progress.Lang.*.

class wicketds.server.orderidlist inherits com.quarix.data.IdList:

	{wicketds/server/ttorderidlist.i}

	constructor orderidlist():
		hbttHandle = buffer ttorderidlist:handle.
	end constructor.

	method public void addOrderNum(input iOrdernum as integer):

		AddRecord('Ordernum':U, string(iOrdernum)).

	end method.

end class.
