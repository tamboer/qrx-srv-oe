
routine-level on error undo, throw.

using Progress.Lang.*.

class wicketds.server.dsorderlinewrapper inherits com.quarix.data.dswrapper:

	{wicketds/server/dsorderline.i}

	constructor	dsorderlinewrapper():
		dsHandle = dataset dsorderline:handle.
	end constructor.

    method public override handle getDsHandle():
        return dataset dsorderline:handle.
    end method.

end class.
