
 /*------------------------------------------------------------------------
    File        : tableobject
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Adam
    Created     : Thu Jun 30 14:58:01 EEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

routine-level on error undo, throw.

using Progress.Lang.*.

class com.quarix.data.tableobject:

	define public property TableObjectName as character no-undo
		get.
		protected set.

	define public variable FieldInstanceList# as com.quarix.data.tablefield extent no-undo.

	method public integer FieldCount():
		return extent(FieldInstanceList#).
	end method.

	destructor public tableobject():

		define variable iNumFld		as integer	no-undo.
		define variable iFldCount	as integer	no-undo.

		iFldCount = extent(FieldInstanceList#) no-error.

		do iNumFld = 1 to extent(FieldInstanceList#):
			delete object FieldInstanceList#[iNumFld] no-error.
		end.

	end destructor.

end class.
