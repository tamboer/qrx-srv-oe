
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataAccess.

&global-define tbl_employee Employee

class wicketds.server.daemployee inherits DataAccess:

    {wicketds/server/dsemployee.i}

    define buffer {&tbl_employee} for {&tbl_employee}.

    constructor public daemployee (  ):
        ID = 'daemployee':u.
    end constructor.

    method private character SetFieldMappings():

        define variable cFieldMapping as character no-undo.

        assign
            cFieldMapping   =
                '{&tbl_employee}.EmpNum'     + ',ttEmployee.EmpNum'         + ',' +
                '{&tbl_employee}.Address'    + ',ttEmployee.Address'   + ',' +
                '{&tbl_employee}.Address2'   + ',ttEmployee.Address2'   + ',' +
                '{&tbl_employee}.Birthdate'  + ',ttEmployee.Birthdate'      + ',' +
                '{&tbl_employee}.City'       + ',ttEmployee.City'      + ',' +
                '{&tbl_employee}.DeptCode'   + ',ttEmployee.DeptCode'      + ',' +
                '{&tbl_employee}.FirstName'  + ',ttEmployee.FirstName'      + ',' +
                '{&tbl_employee}.LastName'   + ',ttEmployee.LastName'      + ',' +
                '{&tbl_employee}.HomePhone'  + ',ttEmployee.HomePhone'      + ',' +
                '{&tbl_employee}.WorkPhone'  + ',ttEmployee.WorkPhone'      + ',' +
                '{&tbl_employee}.Position'   + ',ttEmployee.Position'      + ',' +
                '{&tbl_employee}.StartDate'  + ',ttEmployee.StartDate'      + ',' +
                '{&tbl_employee}.PostalCode' + ',ttEmployee.PostalCode'     + ',' +
                '{&tbl_employee}.State'      + ',ttEmployee.State'
                .
        return cFieldMapping.

    end method.

    method public override logical AttachDataSource():

        define variable cFieldMapping   as character    no-undo.
        define variable cDbTables       as character    no-undo.
        define variable cKeys           as character    no-undo.

        cFieldMapping = SetFieldMappings().

        assign
            cDbTables   = "{&tbl_employee}"
            cKeys       = 'EmpNum':U.

        return AddDataSource ('ttemployee':u, cDbTables, cKeys, ?, cFieldMapping).

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method private logical BeforeSaveData(input-output dataset dsemployee):

        for each ttEmployee
            where row-state(ttEmployee) = row-created
            no-lock:

            ttEmployee.EmpNum = next-value(NextEmpNum).
        end.

        return true.

        catch appError as Progress.Lang.Error :
            ThrowError(input appError).
            delete object appError.
            return false.
        end catch.

    end method.

    method public override logical BeforeSaveData():
        return BeforeSaveData(input-output dataset-handle datasetHandle by-reference).
    end method.

end class.