
 /*------------------------------------------------------------------------
    File        : tttableobject
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Adam
    Created     : Thu Jun 30 14:58:01 EEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

routine-level on error undo, throw.

using Progress.Lang.*.

class com.quarix.data.tttableobject inherits com.quarix.data.tableobject:

	define public variable FieldInstanceList# as com.quarix.data.ttfield extent no-undo.

	method public integer FieldCount():
		return extent(FieldInstanceList#).
	end method.

end class.
