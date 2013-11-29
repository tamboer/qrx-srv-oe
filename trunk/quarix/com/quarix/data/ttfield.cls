
 /*------------------------------------------------------------------------
    File        : ttfield
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Adam
    Created     : Thu Jun 30 14:58:01 EEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

routine-level on error undo, throw.

using Progress.Lang.*.

class com.quarix.data.ttfield final:

	define private property FieldName as character no-undo
		private get.
		private set.

	define private property TableObject as com.quarix.data.tableobject no-undo
		private get.
		private set.

	constructor	private ttfield():
	end constructor.

	constructor	private ttfield(input cFieldName as character, input oTableObject as com.quarix.data.tableobject):

		if cFieldName = ? or
			cFieldName = '' or
			not valid-object(oTableObject)
		then return error.

		assign
			FieldName	= cFieldName
			TableObject	= oTableObject.

	end constructor.

	method public static com.quarix.data.ttfield newField(input cFieldName as character, input oTableObject as com.quarix.data.tableobject):
		return new com.quarix.data.ttfield(input cFieldName, input oTableObject).
	end method.

	method public character getName():
		return FieldName.
	end method.

	method public character getFqName():
		return substitute('&1.&2', TableObject:TableObjectName, FieldName).
	end method.

end class.
