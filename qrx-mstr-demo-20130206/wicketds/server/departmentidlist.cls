
routine-level on error undo, throw.

using Progress.Lang.*.

class wicketds.server.departmentidlist inherits com.quarix.data.IdList:

	{wicketds/server/ttdepartmentidlist.i}

	constructor departmentidlist():
		hbttHandle = buffer ttdepartmentidlist:handle.
	end constructor.

	method public void addDeptCode(input cDeptCode as character):

		AddRecord('DeptCode':U, cDeptCode).

	end method.

end class.
