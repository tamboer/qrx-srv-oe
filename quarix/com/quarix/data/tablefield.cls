
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

class com.quarix.data.tablefield:

	define protected property FieldName as character no-undo
		private get.
		protected set.

	define protected property oTableObject as com.quarix.data.tableobject no-undo
		private get.
		protected set.

	method public character getName():
		return FieldName.
	end method.

	method public character getFqName():
		return substitute('&1.&2', oTableObject:TableObjectName, FieldName).
	end method.

end class.
