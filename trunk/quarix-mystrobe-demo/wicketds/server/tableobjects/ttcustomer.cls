class wicketds.server.tableobjects.ttcustomer inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "ttCustomer"
        get.
        private set.

    define public static property rowid as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowid) then rowid = new com.quarix.data.ttfield("rowid", "ttCustomer").

            return rowid.
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(rowstate) then rowstate = new com.quarix.data.ttfield("rowstate", "ttCustomer").

            return rowstate.
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SortOrder) then SortOrder = new com.quarix.data.ttfield("SortOrder", "ttCustomer").

            return SortOrder.
        end.
        private set.

    define public static property Address as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Address) then Address = new com.quarix.data.ttfield("Address", "ttCustomer").

            return Address.
        end.
        private set.

    define public static property Address2 as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Address2) then Address2 = new com.quarix.data.ttfield("Address2", "ttCustomer").

            return Address2.
        end.
        private set.

    define public static property Balance as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Balance) then Balance = new com.quarix.data.ttfield("Balance", "ttCustomer").

            return Balance.
        end.
        private set.

    define public static property City as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(City) then City = new com.quarix.data.ttfield("City", "ttCustomer").

            return City.
        end.
        private set.

    define public static property Comments as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Comments) then Comments = new com.quarix.data.ttfield("Comments", "ttCustomer").

            return Comments.
        end.
        private set.

    define public static property Contact as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Contact) then Contact = new com.quarix.data.ttfield("Contact", "ttCustomer").

            return Contact.
        end.
        private set.

    define public static property Country as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Country) then Country = new com.quarix.data.ttfield("Country", "ttCustomer").

            return Country.
        end.
        private set.

    define public static property CreditLimit as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(CreditLimit) then CreditLimit = new com.quarix.data.ttfield("CreditLimit", "ttCustomer").

            return CreditLimit.
        end.
        private set.

    define public static property CustNum as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(CustNum) then CustNum = new com.quarix.data.ttfield("CustNum", "ttCustomer").

            return CustNum.
        end.
        private set.

    define public static property Discount as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Discount) then Discount = new com.quarix.data.ttfield("Discount", "ttCustomer").

            return Discount.
        end.
        private set.

    define public static property EmailAddress as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(EmailAddress) then EmailAddress = new com.quarix.data.ttfield("EmailAddress", "ttCustomer").

            return EmailAddress.
        end.
        private set.

    define public static property Fax as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Fax) then Fax = new com.quarix.data.ttfield("Fax", "ttCustomer").

            return Fax.
        end.
        private set.

    define public static property Name as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Name) then Name = new com.quarix.data.ttfield("Name", "ttCustomer").

            return Name.
        end.
        private set.

    define public static property Phone as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Phone) then Phone = new com.quarix.data.ttfield("Phone", "ttCustomer").

            return Phone.
        end.
        private set.

    define public static property PostalCode as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(PostalCode) then PostalCode = new com.quarix.data.ttfield("PostalCode", "ttCustomer").

            return PostalCode.
        end.
        private set.

    define public static property SalesRep as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SalesRep) then SalesRep = new com.quarix.data.ttfield("SalesRep", "ttCustomer").

            return SalesRep.
        end.
        private set.

    define public static property State as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(State) then State = new com.quarix.data.ttfield("State", "ttCustomer").

            return State.
        end.
        private set.

    define public static property Terms as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(Terms) then Terms = new com.quarix.data.ttfield("Terms", "ttCustomer").

            return Terms.
        end.
        private set.

    define public static property StateName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(StateName) then StateName = new com.quarix.data.ttfield("StateName", "ttCustomer").

            return StateName.
        end.
        private set.

    define public static property SalesRepName as com.quarix.data.ttfield no-undo
        get:
            if not valid-object(SalesRepName) then SalesRepName = new com.quarix.data.ttfield("SalesRepName", "ttCustomer").

            return SalesRepName.
        end.
        private set.

    constructor static ttcustomer():
    end constructor.

    destructor public ttcustomer():
        delete object rowid no-error.
        delete object rowstate no-error.
        delete object SortOrder no-error.
        delete object Address no-error.
        delete object Address2 no-error.
        delete object Balance no-error.
        delete object City no-error.
        delete object Comments no-error.
        delete object Contact no-error.
        delete object Country no-error.
        delete object CreditLimit no-error.
        delete object CustNum no-error.
        delete object Discount no-error.
        delete object EmailAddress no-error.
        delete object Fax no-error.
        delete object Name no-error.
        delete object Phone no-error.
        delete object PostalCode no-error.
        delete object SalesRep no-error.
        delete object State no-error.
        delete object Terms no-error.
        delete object StateName no-error.
        delete object SalesRepName no-error.
    end destructor.

end class.
