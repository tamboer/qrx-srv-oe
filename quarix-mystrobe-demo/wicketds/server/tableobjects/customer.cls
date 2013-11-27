class wicketds.server.tableobjects.customer inherits com.quarix.data.tableobject final:

    define public static property ObjectName as character no-undo initial "Customer"
        get.
        private set.

    define public static property CustNum as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(CustNum) then CustNum = new com.quarix.data.dbfield("CustNum", "Customer").

            return CustNum.
        end.
        private set.

    define public static property Country as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Country) then Country = new com.quarix.data.dbfield("Country", "Customer").

            return Country.
        end.
        private set.

    define public static property Name as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Name) then Name = new com.quarix.data.dbfield("Name", "Customer").

            return Name.
        end.
        private set.

    define public static property Address as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Address) then Address = new com.quarix.data.dbfield("Address", "Customer").

            return Address.
        end.
        private set.

    define public static property Address2 as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Address2) then Address2 = new com.quarix.data.dbfield("Address2", "Customer").

            return Address2.
        end.
        private set.

    define public static property City as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(City) then City = new com.quarix.data.dbfield("City", "Customer").

            return City.
        end.
        private set.

    define public static property State as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(State) then State = new com.quarix.data.dbfield("State", "Customer").

            return State.
        end.
        private set.

    define public static property PostalCode as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(PostalCode) then PostalCode = new com.quarix.data.dbfield("PostalCode", "Customer").

            return PostalCode.
        end.
        private set.

    define public static property Contact as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Contact) then Contact = new com.quarix.data.dbfield("Contact", "Customer").

            return Contact.
        end.
        private set.

    define public static property Phone as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Phone) then Phone = new com.quarix.data.dbfield("Phone", "Customer").

            return Phone.
        end.
        private set.

    define public static property SalesRep as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(SalesRep) then SalesRep = new com.quarix.data.dbfield("SalesRep", "Customer").

            return SalesRep.
        end.
        private set.

    define public static property CreditLimit as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(CreditLimit) then CreditLimit = new com.quarix.data.dbfield("CreditLimit", "Customer").

            return CreditLimit.
        end.
        private set.

    define public static property Balance as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Balance) then Balance = new com.quarix.data.dbfield("Balance", "Customer").

            return Balance.
        end.
        private set.

    define public static property Terms as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Terms) then Terms = new com.quarix.data.dbfield("Terms", "Customer").

            return Terms.
        end.
        private set.

    define public static property Discount as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Discount) then Discount = new com.quarix.data.dbfield("Discount", "Customer").

            return Discount.
        end.
        private set.

    define public static property Comments as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Comments) then Comments = new com.quarix.data.dbfield("Comments", "Customer").

            return Comments.
        end.
        private set.

    define public static property Fax as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(Fax) then Fax = new com.quarix.data.dbfield("Fax", "Customer").

            return Fax.
        end.
        private set.

    define public static property EmailAddress as com.quarix.data.dbfield no-undo
        get:
            if not valid-object(EmailAddress) then EmailAddress = new com.quarix.data.dbfield("EmailAddress", "Customer").

            return EmailAddress.
        end.
        private set.

    constructor static customer():
    end constructor.

    destructor public customer():
        delete object CustNum no-error.
        delete object Country no-error.
        delete object Name no-error.
        delete object Address no-error.
        delete object Address2 no-error.
        delete object City no-error.
        delete object State no-error.
        delete object PostalCode no-error.
        delete object Contact no-error.
        delete object Phone no-error.
        delete object SalesRep no-error.
        delete object CreditLimit no-error.
        delete object Balance no-error.
        delete object Terms no-error.
        delete object Discount no-error.
        delete object Comments no-error.
        delete object Fax no-error.
        delete object EmailAddress no-error.
    end destructor.

end class.
