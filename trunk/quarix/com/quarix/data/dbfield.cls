
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

	define private property ObjectName as character no-undo
		private get.
		private set.

	constructor	dbfield():
	end constructor.

	constructor	dbfield(input cFieldName as character):
		FieldName = cFieldName.
	end constructor.

	constructor	dbfield(input cFieldName as character, input cObjectName as character):
		assign
			FieldName	= cFieldName
			ObjectName	= cObjectName.
	end constructor.

	method public character getName():
		return FieldName.
	end method.

	method public character getFqName():
		return substitute('&1.&2', ObjectName, FieldName).
	end method.

end class.
