
 /*------------------------------------------------------------------------
    File        : fieldmapping
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Adam
    Created     : Thu Jun 30 14:58:01 EEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.*.

class com.quarix.data.fieldmapping inherits com.quarix.base.BaseObject implements com.quarix.base.iDisposable:

	define temp-table ttFieldMapping no-undo
		field dbFieldName	as character
		field ttFieldName	as character
		field FieldDbPos	as integer
		index PK_ttFieldMapping is primary is unique dbFieldName ttFieldName.

	constructor	fieldmapping():
		super().
	end constructor.

	method public void ClearFieldMap():
		empty temp-table ttFieldMapping.
	end method.

	method public logical AddFieldMap(input dbfield as dbfield, input ttfield as ttfield):

		define variable cDbFieldName as character no-undo.
		define variable cTtFieldName as character no-undo.

		assign
			cDbFieldName	= dbfield:getFqName()
			cTtFieldName	= ttfield:getFqName().

		if Util:IsEmpty(cDbFieldName) or
			Util:IsEmpty(cTtFieldName)
		then return false.

		find first ttFieldMapping
			where ttFieldMapping.dbFieldName = cDbFieldName
			  and ttFieldMapping.ttFieldName = cTtFieldName
			no-lock no-error.

		if not available(ttFieldMapping)
		then do:
			create ttFieldMapping.

			assign
				ttFieldMapping.dbFieldName	= cDbFieldName
				ttFieldMapping.ttFieldName	= cTtFieldName.

		end. /* if not available(ttFieldMapping)*/

		ttFieldMapping.FieldDbPos = dbfield:getFieldDbPos().

		release ttFieldMapping.

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	method public character GetFieldMapping():

		define variable cValue			as character	no-undo.
		define variable cFieldMapping	as character	no-undo.

		for each ttFieldMapping
			no-lock:

			cValue = substitute('&1,&2', ttFieldMapping.dbFieldName, ttFieldMapping.ttFieldName).

			if Util:IsEmpty(cFieldMapping)
			then cFieldMapping = cValue.
			else cFieldMapping = substitute('&1,&2', cFieldMapping, cValue).

		end. /* for each ttFieldMapping */

		return cFieldMapping.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return ''.
		end catch.

	end method.

	method public character AutoFieldMap()

	destructor public fieldmapping():
		empty temp-table ttFieldMapping.
	end destructor.

end class.
