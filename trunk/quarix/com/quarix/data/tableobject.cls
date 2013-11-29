
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

end class.
