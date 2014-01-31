
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

class com.quarix.data.ttfield inherits com.quarix.data.tablefield final:

	constructor	private ttfield():
	end constructor.

	constructor	private ttfield(input cFieldName as character, input ottTableObject as com.quarix.data.tttableobject):

		if cFieldName = ? or
			cFieldName = '' or
			not valid-object(ottTableObject)
		then return error.

		assign
			FieldName		= cFieldName
			oTableObject	= ottTableObject.

	end constructor.

	method public static com.quarix.data.ttfield newField(input cFieldName as character, input ottTableObject as com.quarix.data.tttableobject):
		return new com.quarix.data.ttfield(input cFieldName, input ottTableObject).
	end method.

end class.
