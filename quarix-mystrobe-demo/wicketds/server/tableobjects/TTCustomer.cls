class wicketds.server.tableobjects.TTCustomer inherits com.quarix.data.tttableobject final:

    &scoped-define idxrowid 1

    &scoped-define idxrowstate 2

    &scoped-define idxSortOrder 3

    &scoped-define idxAddress 4

    &scoped-define idxAddress2 5

    &scoped-define idxBalance 6

    &scoped-define idxCity 7

    &scoped-define idxComments 8

    &scoped-define idxContact 9

    &scoped-define idxCountry 10

    &scoped-define idxCreditLimit 11

    &scoped-define idxCustNum 12

    &scoped-define idxDiscount 13

    &scoped-define idxEmailAddress 14

    &scoped-define idxFax 15

    &scoped-define idxName 16

    &scoped-define idxPhone 17

    &scoped-define idxPostalCode 18

    &scoped-define idxSalesRep 19

    &scoped-define idxState 20

    &scoped-define idxTerms 21

    &scoped-define idxStateName 22

    &scoped-define idxSalesRepName 23

    &scoped-define idxNumFields 23

    define public static property Instance as wicketds.server.tableobjects.TTCustomer no-undo
        public get:
            if not valid-object(Instance) then Instance = new wicketds.server.tableobjects.TTCustomer().

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
            return cast(Instance:FieldInstanceList#[{&idxrowid}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property rowstate as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxrowstate}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property SortOrder as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxSortOrder}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Address as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxAddress}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Address2 as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxAddress2}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Balance as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxBalance}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property City as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCity}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Comments as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxComments}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Contact as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxContact}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Country as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCountry}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property CreditLimit as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCreditLimit}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property CustNum as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxCustNum}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Discount as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxDiscount}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property EmailAddress as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxEmailAddress}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Fax as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxFax}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Name as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxName}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Phone as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxPhone}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property PostalCode as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxPostalCode}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property SalesRep as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxSalesRep}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property State as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxState}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property Terms as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxTerms}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property StateName as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxStateName}], com.quarix.data.ttfield).
        end.
        private set.

    define public static property SalesRepName as com.quarix.data.ttfield no-undo
        get:
            return cast(Instance:FieldInstanceList#[{&idxSalesRepName}], com.quarix.data.ttfield).
        end.
        private set.

    constructor private TTCustomer():

        TableObjectName = "ttCustomer".

        extent(FieldInstanceList#) = {&idxNumFields}.

        FieldInstanceList#[{&idxrowid}] = com.quarix.data.ttfield:newField(input "rowid", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowid}]) then return error.

        FieldInstanceList#[{&idxrowstate}] = com.quarix.data.ttfield:newField(input "rowstate", input this-object).
        if not valid-object(FieldInstanceList#[{&idxrowstate}]) then return error.

        FieldInstanceList#[{&idxSortOrder}] = com.quarix.data.ttfield:newField(input "SortOrder", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSortOrder}]) then return error.

        FieldInstanceList#[{&idxAddress}] = com.quarix.data.ttfield:newField(input "Address", input this-object).
        if not valid-object(FieldInstanceList#[{&idxAddress}]) then return error.

        FieldInstanceList#[{&idxAddress2}] = com.quarix.data.ttfield:newField(input "Address2", input this-object).
        if not valid-object(FieldInstanceList#[{&idxAddress2}]) then return error.

        FieldInstanceList#[{&idxBalance}] = com.quarix.data.ttfield:newField(input "Balance", input this-object).
        if not valid-object(FieldInstanceList#[{&idxBalance}]) then return error.

        FieldInstanceList#[{&idxCity}] = com.quarix.data.ttfield:newField(input "City", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCity}]) then return error.

        FieldInstanceList#[{&idxComments}] = com.quarix.data.ttfield:newField(input "Comments", input this-object).
        if not valid-object(FieldInstanceList#[{&idxComments}]) then return error.

        FieldInstanceList#[{&idxContact}] = com.quarix.data.ttfield:newField(input "Contact", input this-object).
        if not valid-object(FieldInstanceList#[{&idxContact}]) then return error.

        FieldInstanceList#[{&idxCountry}] = com.quarix.data.ttfield:newField(input "Country", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCountry}]) then return error.

        FieldInstanceList#[{&idxCreditLimit}] = com.quarix.data.ttfield:newField(input "CreditLimit", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCreditLimit}]) then return error.

        FieldInstanceList#[{&idxCustNum}] = com.quarix.data.ttfield:newField(input "CustNum", input this-object).
        if not valid-object(FieldInstanceList#[{&idxCustNum}]) then return error.

        FieldInstanceList#[{&idxDiscount}] = com.quarix.data.ttfield:newField(input "Discount", input this-object).
        if not valid-object(FieldInstanceList#[{&idxDiscount}]) then return error.

        FieldInstanceList#[{&idxEmailAddress}] = com.quarix.data.ttfield:newField(input "EmailAddress", input this-object).
        if not valid-object(FieldInstanceList#[{&idxEmailAddress}]) then return error.

        FieldInstanceList#[{&idxFax}] = com.quarix.data.ttfield:newField(input "Fax", input this-object).
        if not valid-object(FieldInstanceList#[{&idxFax}]) then return error.

        FieldInstanceList#[{&idxName}] = com.quarix.data.ttfield:newField(input "Name", input this-object).
        if not valid-object(FieldInstanceList#[{&idxName}]) then return error.

        FieldInstanceList#[{&idxPhone}] = com.quarix.data.ttfield:newField(input "Phone", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPhone}]) then return error.

        FieldInstanceList#[{&idxPostalCode}] = com.quarix.data.ttfield:newField(input "PostalCode", input this-object).
        if not valid-object(FieldInstanceList#[{&idxPostalCode}]) then return error.

        FieldInstanceList#[{&idxSalesRep}] = com.quarix.data.ttfield:newField(input "SalesRep", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSalesRep}]) then return error.

        FieldInstanceList#[{&idxState}] = com.quarix.data.ttfield:newField(input "State", input this-object).
        if not valid-object(FieldInstanceList#[{&idxState}]) then return error.

        FieldInstanceList#[{&idxTerms}] = com.quarix.data.ttfield:newField(input "Terms", input this-object).
        if not valid-object(FieldInstanceList#[{&idxTerms}]) then return error.

        FieldInstanceList#[{&idxStateName}] = com.quarix.data.ttfield:newField(input "StateName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxStateName}]) then return error.

        FieldInstanceList#[{&idxSalesRepName}] = com.quarix.data.ttfield:newField(input "SalesRepName", input this-object).
        if not valid-object(FieldInstanceList#[{&idxSalesRepName}]) then return error.

    end constructor.

    method public static integer NumberOfFields():
        return Instance:FieldCount().
    end method.

end class.
