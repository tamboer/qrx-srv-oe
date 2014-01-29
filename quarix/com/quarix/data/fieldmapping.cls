
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

		if not valid-object(dbfield) or
			not valid-object(ttfield)
		then return false.

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
			return ?.
		end catch.

	end method.

	method public character AutoFieldMap(input oDbTableObject as com.quarix.data.dbtableobject, input oTtTableObject as com.quarix.data.tttableobject):

		define variable iTtFieldCount	as integer					no-undo.
		define variable iTtNumField		as integer					no-undo.
		define variable iDbFieldCount	as integer					no-undo.
		define variable iDbNumField   	as integer					no-undo.
		define variable cDbFieldName	as character				no-undo.
		define variable cTtFieldName	as character				no-undo.
		define variable cFieldMapping	as character				no-undo.
		define variable oTtField		as com.quarix.data.ttfield	no-undo.
		define variable oDbField		as com.quarix.data.dbfield	no-undo.

		ClearFieldMap().

		if not valid-object(oDbTableObject) or
			not valid-object(oTtTableObject)
		then return ?.

		assign
			iTtFieldCount = oTtTableObject:FieldCount()
			iDbFieldCount = oDbTableObject:FieldCount().

		do iTtNumField = 1 to iTtFieldCount:

			oTtField = oTtTableObject:FieldInstanceList#[iTtNumField].

			cTtFieldName= oTtField:getName().

			do iDbNumField = 1 to iDbFieldCount:

				oDbField = oDbTableObject:FieldInstanceList#[iDbNumField].

				cDbFieldName = oDbField:getName().

				if cTtFieldName = cDbFieldName
				then do:
					if not AddFieldMap(input oDbField, input oTtField)
					then return ?.

					leave.
				end.

			end. /* do iDbNumField = 1 to iDbFieldCount */

		end. /* do iTtNumField = 1 to iTtFieldCount */

		cFieldMapping = GetFieldMapping().

		return cFieldMapping.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return ?.
		end catch.

	end method.

	method public logical RemoveFieldMapByTTField(input ttfield as ttfield):

		define variable cFieldName as character no-undo.

		if not valid-object(ttfield)
		then return false.

		cFieldName = ttfield:getFqName().

		for each ttFieldMapping
			where ttFieldMapping.ttFieldName = cFieldName
			no-lock:

			delete ttFieldMapping.
		end.

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	method public logical RemoveFieldMapByDBField(input dbfield as dbfield):

		define variable cFieldName as character no-undo.

		if not valid-object(dbfield)
		then return false.

		cFieldName = dbfield:getFqName().

		for each ttFieldMapping
			where ttFieldMapping.dbFieldName = cFieldName
			no-lock:

			delete ttFieldMapping.
		end.

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	method public logical RemoveFieldMapsAfterPos(input dbfield as dbfield):

		define variable iPos as integer no-undo.

		if not valid-object(dbfield)
		then return false.

		iPos = dbfield:getFieldDbPos().

		for each ttFieldMapping
			where ttFieldMapping.FieldDbPos > iPos
			no-lock:

			delete ttFieldMapping.
		end.

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.

	end method.

	destructor public fieldmapping():
		empty temp-table ttFieldMapping.
	end destructor.

end class.
