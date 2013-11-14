
routine-level on error undo, throw.

using Progress.Lang.*.

class wicketds.server.dsordordlinewrapper inherits com.quarix.data.dswrapper:

	{wicketds/server/dsordordline.i}

	constructor	dsordordlinewrapper():
		dsHandle = dataset dsordordline:handle.
	end constructor.

	method public static logical createOrder(input dataset-handle hdsOrdOrdLine, input cSourceObject as character, output dataset-handle hdsOrder):

		return createDataset(input dataset-handle hdsOrdOrdLine by-reference, input 'ttorder':U, input cSourceObject, output dataset-handle hdsOrder by-reference).

		finally:
			delete object hdsOrdOrdLine	no-error.
			delete object hdsOrder		no-error.
		end finally.

	end method.

	method public static logical createOrderLine(input dataset-handle hdsOrdOrdLine, input cSourceObject as character, output dataset-handle hdsOrderLine):

		return createDataset(input dataset-handle hdsOrdOrdLine by-reference, input 'ttorderline':U, input cSourceObject, output dataset-handle hdsOrderLine by-reference).

		finally:
			delete object hdsOrdOrdLine	no-error.
			delete object hdsOrderLine	no-error.
		end finally.

	end method.

    method public override handle getDsHandle():
        return dataset dsordordline:handle.
    end method.

end class.
