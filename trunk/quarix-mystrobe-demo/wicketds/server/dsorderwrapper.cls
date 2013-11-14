
routine-level on error undo, throw.

using Progress.Lang.*.

class wicketds.server.dsorderwrapper inherits com.quarix.data.dswrapper:

	{wicketds/server/dsorder.i}

	constructor	dsorderwrapper():
		dsHandle = dataset dsorder:handle.
	end constructor.

    method public override handle getDsHandle():
        return dataset dsorder:handle.
    end method.

end class.
