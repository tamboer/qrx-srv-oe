
 /*------------------------------------------------------------------------
    File        : dbfield
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Adam
    Created     : Thu Jun 30 14:58:01 EEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

routine-level on error undo, throw.

using Progress.Lang.*.

class com.quarix.data.dbfield final:

	define private property FieldName as character no-undo
		private get.
		private set.

	define private property FieldDbPos as integer no-undo
		private get.
		private set.

	define private property dbTableObject as com.quarix.data.dbtableobject no-undo
		private get.
		private set.

	constructor	private dbfield():
	end constructor.

	constructor	private dbfield(input cFieldName as character, input iFieldDbPos as integer, input odbTableObject as com.quarix.data.dbtableobject):

		if cFieldName = ?	or
			cFieldName = ''	or
			iFieldDbPos = ?	or
			iFieldDbPos = 0	or
			not valid-object(odbTableObject)
		then return error.

		assign
			FieldName   	= cFieldName
			dbTableObject	= odbTableObject
			FieldDbPos		= iFieldDbPos.

	end constructor.

	method public static com.quarix.data.dbfield newField(input cFieldName as character, input iFieldDbPos as integer, input odbTableObject as com.quarix.data.dbtableobject):
		return new com.quarix.data.dbfield(input cFieldName, input iFieldDbPos, input odbTableObject).
	end method.

	method public character getName():
		return FieldName.
	end method.

	method public integer getFieldDbPos():
		return FieldDbPos.
	end method.

	method public character getFqName():
		return substitute('&1.&2', dbTableObject:TableObjectName, FieldName).
	end method.

end class.
