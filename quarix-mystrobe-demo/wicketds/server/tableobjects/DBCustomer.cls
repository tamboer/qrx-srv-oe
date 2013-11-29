class wicketds.server.tableobjects.DBCustomer inherits com.quarix.data.tableobject final:

    &scoped-define idxCustNum 1

    &scoped-define idxCountry 2

    &scoped-define idxName 3

    &scoped-define idxAddress 4

    &scoped-define idxAddress2 5

    &scoped-define idxCity 6

    &scoped-define idxState 7

    &scoped-define idxPostalCode 8

    &scoped-define idxContact 9

    &scoped-define idxPhone 10

    &scoped-define idxSalesRep 11

    &scoped-define idxCreditLimit 12

    &scoped-define idxBalance 13

    &scoped-define idxTerms 14

    &scoped-define idxDiscount 15

    &scoped-define idxComments 16

    &scoped-define idxFax 17

    &scoped-define idxEmailAddress 18

    define public variable FieldInstanceList# as com.quarix.data.dbfield extent 18 no-undo.

    define private static property Instance as wicketds.server.tableobjects.DBCustomer no-undo
        private get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.DBCustomer().

            return Instance.
        end.
        private set.

    define public static property TableName as character no-undo
        get:
            return Instance:TableObjectName.
        end.
        private set.

    define public static property CustNum as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCustNum}].
        end.
        private set.

    define public static property Country as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCountry}].
        end.
        private set.

    define public static property Name as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxName}].
        end.
        private set.

    define public static property Address as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxAddress}].
        end.
        private set.

    define public static property Address2 as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxAddress2}].
        end.
        private set.

    define public static property City as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCity}].
        end.
        private set.

    define public static property State as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxState}].
        end.
        private set.

    define public static property PostalCode as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxPostalCode}].
        end.
        private set.

    define public static property Contact as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxContact}].
        end.
        private set.

    define public static property Phone as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxPhone}].
        end.
        private set.

    define public static property SalesRep as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxSalesRep}].
        end.
        private set.

    define public static property CreditLimit as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxCreditLimit}].
        end.
        private set.

    define public static property Balance as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxBalance}].
        end.
        private set.

    define public static property Terms as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxTerms}].
        end.
        private set.

    define public static property Discount as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxDiscount}].
        end.
        private set.

    define public static property Comments as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxComments}].
        end.
        private set.

    define public static property Fax as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxFax}].
        end.
        private set.

    define public static property EmailAddress as com.quarix.data.dbfield no-undo
        get:
            return Instance:FieldInstanceList#[{&idxEmailAddress}].
        end.
        private set.

    constructor private DBCustomer():

        TableObjectName = "Customer".

        FieldInstanceList#[{&idxCustNum}] = com.quarix.data.dbfield:newField(input "CustNum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCustNum}]) then return error.

        FieldInstanceList#[{&idxCountry}] = com.quarix.data.dbfield:newField(input "Country", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCountry}]) then return error.

        FieldInstanceList#[{&idxName}] = com.quarix.data.dbfield:newField(input "Name", input this-object).
        if not valid-object(FieldInstanceList#[{&idxName}]) then return error.

        FieldInstanceList#[{&idxAddress}] = com.quarix.data.dbfield:newField(input "Address", input this-object).
        if not valid-object(FieldInstanceList#[{&idxAddress}]) then return error.

        FieldInstanceList#[{&idxAddress2}] = com.quarix.data.dbfield:newField(input "Address2", input this-object).
        if not valid-object(FieldInstanceList#[{&idxAddress2}]) then return error.

        FieldInstanceList#[{&idxCity}] = com.quarix.data.dbfield:newField(input "City", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCity}]) then return error.

        FieldInstanceList#[{&idxState}] = com.quarix.data.dbfield:newField(input "State", input this-object).
        if not valid-object(FieldInstanceList#[{&idxState}]) then return error.

        FieldInstanceList#[{&idxPostalCode}] = com.quarix.data.dbfield:newField(input "PostalCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPostalCode}]) then return error.

        FieldInstanceList#[{&idxContact}] = com.quarix.data.dbfield:newField(input "Contact", input this-object).
        if not valid-object(FieldInstanceList#[{&idxContact}]) then return error.

        FieldInstanceList#[{&idxPhone}] = com.quarix.data.dbfield:newField(input "Phone", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPhone}]) then return error.

        FieldInstanceList#[{&idxSalesRep}] = com.quarix.data.dbfield:newField(input "SalesRep", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSalesRep}]) then return error.

        FieldInstanceList#[{&idxCreditLimit}] = com.quarix.data.dbfield:newField(input "CreditLimit", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCreditLimit}]) then return error.

        FieldInstanceList#[{&idxBalance}] = com.quarix.data.dbfield:newField(input "Balance", input this-object).
        if not valid-object(FieldInstanceList#[{&idxBalance}]) then return error.

        FieldInstanceList#[{&idxTerms}] = com.quarix.data.dbfield:newField(input "Terms", input this-object).
        if not valid-object(FieldInstanceList#[{&idxTerms}]) then return error.

        FieldInstanceList#[{&idxDiscount}] = com.quarix.data.dbfield:newField(input "Discount", input this-object).
        if not valid-object(FieldInstanceList#[{&idxDiscount}]) then return error.

        FieldInstanceList#[{&idxComments}] = com.quarix.data.dbfield:newField(input "Comments", input this-object).
        if not valid-object(FieldInstanceList#[{&idxComments}]) then return error.

        FieldInstanceList#[{&idxFax}] = com.quarix.data.dbfield:newField(input "Fax", input this-object).
        if not valid-object(FieldInstanceList#[{&idxFax}]) then return error.

        FieldInstanceList#[{&idxEmailAddress}] = com.quarix.data.dbfield:newField(input "EmailAddress", input this-object).
        if not valid-object(FieldInstanceList#[{&idxEmailAddress}]) then return error.

    end constructor.

    destructor public DBCustomer():
        delete object FieldInstanceList#[{&idxCustNum}] no-error.
        delete object FieldInstanceList#[{&idxCountry}] no-error.
        delete object FieldInstanceList#[{&idxName}] no-error.
        delete object FieldInstanceList#[{&idxAddress}] no-error.
        delete object FieldInstanceList#[{&idxAddress2}] no-error.
        delete object FieldInstanceList#[{&idxCity}] no-error.
        delete object FieldInstanceList#[{&idxState}] no-error.
        delete object FieldInstanceList#[{&idxPostalCode}] no-error.
        delete object FieldInstanceList#[{&idxContact}] no-error.
        delete object FieldInstanceList#[{&idxPhone}] no-error.
        delete object FieldInstanceList#[{&idxSalesRep}] no-error.
        delete object FieldInstanceList#[{&idxCreditLimit}] no-error.
        delete object FieldInstanceList#[{&idxBalance}] no-error.
        delete object FieldInstanceList#[{&idxTerms}] no-error.
        delete object FieldInstanceList#[{&idxDiscount}] no-error.
        delete object FieldInstanceList#[{&idxComments}] no-error.
        delete object FieldInstanceList#[{&idxFax}] no-error.
        delete object FieldInstanceList#[{&idxEmailAddress}] no-error.
    end destructor.

end class.
