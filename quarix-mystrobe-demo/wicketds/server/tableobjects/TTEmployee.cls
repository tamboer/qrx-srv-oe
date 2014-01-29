class wicketds.server.tableobjects.TTEmployee inherits com.quarix.data.tttableobject final:

    &scoped-define idxrowid 1

    &scoped-define idxrowstate 2

    &scoped-define idxSortOrder 3

    &scoped-define idxEmpNum 4

    &scoped-define idxAddress 5

    &scoped-define idxAddress2 6

    &scoped-define idxBirthdate 7

    &scoped-define idxCity 8

    &scoped-define idxDeptCode 9

    &scoped-define idxFirstName 10

    &scoped-define idxLastName 11

    &scoped-define idxHomePhone 12

    &scoped-define idxWorkPhone 13

    &scoped-define idxPosition 14

    &scoped-define idxStartDate 15

    &scoped-define idxPostalCode 16

    &scoped-define idxState 17

    &scoped-define idxDeptName 18

    &scoped-define idxStateName 19

    define public variable FieldInstanceList# as com.quarix.data.ttfield extent 19 no-undo.

    define private static property Instance as wicketds.server.tableobjects.TTEmployee no-undo
        private get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTEmployee().

            return Instance.
        end.
        private set.

    define public static property TableName as character no-undo
        get:
            return Instance:TableObjectName.
        end.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxrowid}].
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxrowstate}].
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxSortOrder}].
        end.
        private set.

    define public static property EmpNum as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxEmpNum}].
        end.
        private set.

    define public static property Address as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxAddress}].
        end.
        private set.

    define public static property Address2 as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxAddress2}].
        end.
        private set.

    define public static property Birthdate as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxBirthdate}].
        end.
        private set.

    define public static property City as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCity}].
        end.
        private set.

    define public static property DeptCode as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxDeptCode}].
        end.
        private set.

    define public static property FirstName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxFirstName}].
        end.
        private set.

    define public static property LastName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxLastName}].
        end.
        private set.

    define public static property HomePhone as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxHomePhone}].
        end.
        private set.

    define public static property WorkPhone as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxWorkPhone}].
        end.
        private set.

    define public static property Position as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxPosition}].
        end.
        private set.

    define public static property StartDate as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxStartDate}].
        end.
        private set.

    define public static property PostalCode as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxPostalCode}].
        end.
        private set.

    define public static property State as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxState}].
        end.
        private set.

    define public static property DeptName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxDeptName}].
        end.
        private set.

    define public static property StateName as com.quarix.data.ttfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxStateName}].
        end.
        private set.

    constructor private TTEmployee():

        TableObjectName = "ttEmployee".

        FieldInstanceList#[{&idxrowid}] = com.quarix.data.ttfield:newField(input "rowid", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowid}]) then return error.

        FieldInstanceList#[{&idxrowstate}] = com.quarix.data.ttfield:newField(input "rowstate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowstate}]) then return error.

        FieldInstanceList#[{&idxSortOrder}] = com.quarix.data.ttfield:newField(input "SortOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSortOrder}]) then return error.

        FieldInstanceList#[{&idxEmpNum}] = com.quarix.data.ttfield:newField(input "EmpNum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxEmpNum}]) then return error.

        FieldInstanceList#[{&idxAddress}] = com.quarix.data.ttfield:newField(input "Address", input this-object).
        if not valid-object(FieldInstanceList#[{&idxAddress}]) then return error.

        FieldInstanceList#[{&idxAddress2}] = com.quarix.data.ttfield:newField(input "Address2", input this-object).
        if not valid-object(FieldInstanceList#[{&idxAddress2}]) then return error.

        FieldInstanceList#[{&idxBirthdate}] = com.quarix.data.ttfield:newField(input "Birthdate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxBirthdate}]) then return error.

        FieldInstanceList#[{&idxCity}] = com.quarix.data.ttfield:newField(input "City", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCity}]) then return error.

        FieldInstanceList#[{&idxDeptCode}] = com.quarix.data.ttfield:newField(input "DeptCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxDeptCode}]) then return error.

        FieldInstanceList#[{&idxFirstName}] = com.quarix.data.ttfield:newField(input "FirstName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxFirstName}]) then return error.

        FieldInstanceList#[{&idxLastName}] = com.quarix.data.ttfield:newField(input "LastName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxLastName}]) then return error.

        FieldInstanceList#[{&idxHomePhone}] = com.quarix.data.ttfield:newField(input "HomePhone", input this-object).
        if not valid-object(FieldInstanceList#[{&idxHomePhone}]) then return error.

        FieldInstanceList#[{&idxWorkPhone}] = com.quarix.data.ttfield:newField(input "WorkPhone", input this-object).
        if not valid-object(FieldInstanceList#[{&idxWorkPhone}]) then return error.

        FieldInstanceList#[{&idxPosition}] = com.quarix.data.ttfield:newField(input "Position", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPosition}]) then return error.

        FieldInstanceList#[{&idxStartDate}] = com.quarix.data.ttfield:newField(input "StartDate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxStartDate}]) then return error.

        FieldInstanceList#[{&idxPostalCode}] = com.quarix.data.ttfield:newField(input "PostalCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPostalCode}]) then return error.

        FieldInstanceList#[{&idxState}] = com.quarix.data.ttfield:newField(input "State", input this-object).
        if not valid-object(FieldInstanceList#[{&idxState}]) then return error.

        FieldInstanceList#[{&idxDeptName}] = com.quarix.data.ttfield:newField(input "DeptName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxDeptName}]) then return error.

        FieldInstanceList#[{&idxStateName}] = com.quarix.data.ttfield:newField(input "StateName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxStateName}]) then return error.

    end constructor.

    destructor public TTEmployee():
        delete object FieldInstanceList#[{&idxrowid}] no-error.
        delete object FieldInstanceList#[{&idxrowstate}] no-error.
        delete object FieldInstanceList#[{&idxSortOrder}] no-error.
        delete object FieldInstanceList#[{&idxEmpNum}] no-error.
        delete object FieldInstanceList#[{&idxAddress}] no-error.
        delete object FieldInstanceList#[{&idxAddress2}] no-error.
        delete object FieldInstanceList#[{&idxBirthdate}] no-error.
        delete object FieldInstanceList#[{&idxCity}] no-error.
        delete object FieldInstanceList#[{&idxDeptCode}] no-error.
        delete object FieldInstanceList#[{&idxFirstName}] no-error.
        delete object FieldInstanceList#[{&idxLastName}] no-error.
        delete object FieldInstanceList#[{&idxHomePhone}] no-error.
        delete object FieldInstanceList#[{&idxWorkPhone}] no-error.
        delete object FieldInstanceList#[{&idxPosition}] no-error.
        delete object FieldInstanceList#[{&idxStartDate}] no-error.
        delete object FieldInstanceList#[{&idxPostalCode}] no-error.
        delete object FieldInstanceList#[{&idxState}] no-error.
        delete object FieldInstanceList#[{&idxDeptName}] no-error.
        delete object FieldInstanceList#[{&idxStateName}] no-error.
    end destructor.

end class.
