class wicketds.server.tableobjects.ttemployee inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttEmployee"
        get.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowid) then rowid = new com.quarix.data.ttfield("rowid", "ttEmployee").

            return rowid.
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowstate) then rowstate = new com.quarix.data.ttfield("rowstate", "ttEmployee").

            return rowstate.
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SortOrder) then SortOrder = new com.quarix.data.ttfield("SortOrder", "ttEmployee").

            return SortOrder.
        end.
        private set.

    define public static property EmpNum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(EmpNum) then EmpNum = new com.quarix.data.ttfield("EmpNum", "ttEmployee").

            return EmpNum.
        end.
        private set.

    define public static property Address as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Address) then Address = new com.quarix.data.ttfield("Address", "ttEmployee").

            return Address.
        end.
        private set.

    define public static property Address2 as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Address2) then Address2 = new com.quarix.data.ttfield("Address2", "ttEmployee").

            return Address2.
        end.
        private set.

    define public static property Birthdate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Birthdate) then Birthdate = new com.quarix.data.ttfield("Birthdate", "ttEmployee").

            return Birthdate.
        end.
        private set.

    define public static property City as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(City) then City = new com.quarix.data.ttfield("City", "ttEmployee").

            return City.
        end.
        private set.

    define public static property DeptCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(DeptCode) then DeptCode = new com.quarix.data.ttfield("DeptCode", "ttEmployee").

            return DeptCode.
        end.
        private set.

    define public static property FirstName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(FirstName) then FirstName = new com.quarix.data.ttfield("FirstName", "ttEmployee").

            return FirstName.
        end.
        private set.

    define public static property LastName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(LastName) then LastName = new com.quarix.data.ttfield("LastName", "ttEmployee").

            return LastName.
        end.
        private set.

    define public static property HomePhone as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(HomePhone) then HomePhone = new com.quarix.data.ttfield("HomePhone", "ttEmployee").

            return HomePhone.
        end.
        private set.

    define public static property WorkPhone as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(WorkPhone) then WorkPhone = new com.quarix.data.ttfield("WorkPhone", "ttEmployee").

            return WorkPhone.
        end.
        private set.

    define public static property Position as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Position) then Position = new com.quarix.data.ttfield("Position", "ttEmployee").

            return Position.
        end.
        private set.

    define public static property StartDate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(StartDate) then StartDate = new com.quarix.data.ttfield("StartDate", "ttEmployee").

            return StartDate.
        end.
        private set.

    define public static property PostalCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(PostalCode) then PostalCode = new com.quarix.data.ttfield("PostalCode", "ttEmployee").

            return PostalCode.
        end.
        private set.

    define public static property State as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(State) then State = new com.quarix.data.ttfield("State", "ttEmployee").

            return State.
        end.
        private set.

    define public static property DeptName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(DeptName) then DeptName = new com.quarix.data.ttfield("DeptName", "ttEmployee").

            return DeptName.
        end.
        private set.

    define public static property StateName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(StateName) then StateName = new com.quarix.data.ttfield("StateName", "ttEmployee").

            return StateName.
        end.
        private set.

    constructor static ttemployee():
    end constructor.

    destructor public ttemployee():
        delete object rowid no-error.
        delete object rowstate no-error.
        delete object SortOrder no-error.
        delete object EmpNum no-error.
        delete object Address no-error.
        delete object Address2 no-error.
        delete object Birthdate no-error.
        delete object City no-error.
        delete object DeptCode no-error.
        delete object FirstName no-error.
        delete object LastName no-error.
        delete object HomePhone no-error.
        delete object WorkPhone no-error.
        delete object Position no-error.
        delete object StartDate no-error.
        delete object PostalCode no-error.
        delete object State no-error.
        delete object DeptName no-error.
        delete object StateName no-error.
    end destructor.

end class.
