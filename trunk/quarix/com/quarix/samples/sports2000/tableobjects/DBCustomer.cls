class com.quarix.samples.sports2000.tableobjects.DBCustomer inherits com.quarix.data.dbtableobject final:

&scoped-define idxCustNum 1
&scoped-define idxCustNumDbPos 2

&scoped-define idxCountry 2
&scoped-define idxCountryDbPos 8

&scoped-define idxName 3
&scoped-define idxNameDbPos 3

&scoped-define idxAddress 4
&scoped-define idxAddressDbPos 4

&scoped-define idxAddress2 5
&scoped-define idxAddress2DbPos 5

&scoped-define idxCity 6
&scoped-define idxCityDbPos 6

&scoped-define idxState 7
&scoped-define idxStateDbPos 7

&scoped-define idxPostalCode 8
&scoped-define idxPostalCodeDbPos 17

&scoped-define idxContact 9
&scoped-define idxContactDbPos 10

&scoped-define idxPhone 10
&scoped-define idxPhoneDbPos 9

&scoped-define idxSalesRep 11
&scoped-define idxSalesRepDbPos 11

&scoped-define idxCreditLimit 12
&scoped-define idxCreditLimitDbPos 13

&scoped-define idxBalance 13
&scoped-define idxBalanceDbPos 14

&scoped-define idxTerms 14
&scoped-define idxTermsDbPos 15

&scoped-define idxDiscount 15
&scoped-define idxDiscountDbPos 16

&scoped-define idxComments 16
&scoped-define idxCommentsDbPos 12

&scoped-define idxFax 17
&scoped-define idxFaxDbPos 18

&scoped-define idxEmailAddress 18
&scoped-define idxEmailAddressDbPos 19

&scoped-define idxNumFields 18

define public static property Instance as com.quarix.samples.sports2000.tableobjects.DBCustomer no-undo
public get:
if not valid-object(Instance) then Instance = new com.quarix.samples.sports2000.tableobjects.DBCustomer().

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
return cast(Instance:FieldInstanceList#[{&idxCustNum}], com.quarix.data.dbfield).
end.
private set.

define public static property Country as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxCountry}], com.quarix.data.dbfield).
end.
private set.

define public static property Name as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxName}], com.quarix.data.dbfield).
end.
private set.

define public static property Address as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxAddress}], com.quarix.data.dbfield).
end.
private set.

define public static property Address2 as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxAddress2}], com.quarix.data.dbfield).
end.
private set.

define public static property City as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxCity}], com.quarix.data.dbfield).
end.
private set.

define public static property State as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxState}], com.quarix.data.dbfield).
end.
private set.

define public static property PostalCode as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxPostalCode}], com.quarix.data.dbfield).
end.
private set.

define public static property Contact as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxContact}], com.quarix.data.dbfield).
end.
private set.

define public static property Phone as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxPhone}], com.quarix.data.dbfield).
end.
private set.

define public static property SalesRep as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxSalesRep}], com.quarix.data.dbfield).
end.
private set.

define public static property CreditLimit as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxCreditLimit}], com.quarix.data.dbfield).
end.
private set.

define public static property Balance as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxBalance}], com.quarix.data.dbfield).
end.
private set.

define public static property Terms as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxTerms}], com.quarix.data.dbfield).
end.
private set.

define public static property Discount as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxDiscount}], com.quarix.data.dbfield).
end.
private set.

define public static property Comments as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxComments}], com.quarix.data.dbfield).
end.
private set.

define public static property Fax as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxFax}], com.quarix.data.dbfield).
end.
private set.

define public static property EmailAddress as com.quarix.data.dbfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxEmailAddress}], com.quarix.data.dbfield).
end.
private set.

constructor private DBCustomer():

TableObjectName = "Customer".

extent(FieldInstanceList#) = {&idxNumFields}.

FieldInstanceList#[{&idxCustNum}] = com.quarix.data.dbfield:newField(input "CustNum", input {&idxCustNumDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxCustNum}]) then return error.

FieldInstanceList#[{&idxCountry}] = com.quarix.data.dbfield:newField(input "Country", input {&idxCountryDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxCountry}]) then return error.

FieldInstanceList#[{&idxName}] = com.quarix.data.dbfield:newField(input "Name", input {&idxNameDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxName}]) then return error.

FieldInstanceList#[{&idxAddress}] = com.quarix.data.dbfield:newField(input "Address", input {&idxAddressDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxAddress}]) then return error.

FieldInstanceList#[{&idxAddress2}] = com.quarix.data.dbfield:newField(input "Address2", input {&idxAddress2DbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxAddress2}]) then return error.

FieldInstanceList#[{&idxCity}] = com.quarix.data.dbfield:newField(input "City", input {&idxCityDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxCity}]) then return error.

FieldInstanceList#[{&idxState}] = com.quarix.data.dbfield:newField(input "State", input {&idxStateDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxState}]) then return error.

FieldInstanceList#[{&idxPostalCode}] = com.quarix.data.dbfield:newField(input "PostalCode", input {&idxPostalCodeDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxPostalCode}]) then return error.

FieldInstanceList#[{&idxContact}] = com.quarix.data.dbfield:newField(input "Contact", input {&idxContactDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxContact}]) then return error.

FieldInstanceList#[{&idxPhone}] = com.quarix.data.dbfield:newField(input "Phone", input {&idxPhoneDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxPhone}]) then return error.

FieldInstanceList#[{&idxSalesRep}] = com.quarix.data.dbfield:newField(input "SalesRep", input {&idxSalesRepDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxSalesRep}]) then return error.

FieldInstanceList#[{&idxCreditLimit}] = com.quarix.data.dbfield:newField(input "CreditLimit", input {&idxCreditLimitDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxCreditLimit}]) then return error.

FieldInstanceList#[{&idxBalance}] = com.quarix.data.dbfield:newField(input "Balance", input {&idxBalanceDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxBalance}]) then return error.

FieldInstanceList#[{&idxTerms}] = com.quarix.data.dbfield:newField(input "Terms", input {&idxTermsDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxTerms}]) then return error.

FieldInstanceList#[{&idxDiscount}] = com.quarix.data.dbfield:newField(input "Discount", input {&idxDiscountDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxDiscount}]) then return error.

FieldInstanceList#[{&idxComments}] = com.quarix.data.dbfield:newField(input "Comments", input {&idxCommentsDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxComments}]) then return error.

FieldInstanceList#[{&idxFax}] = com.quarix.data.dbfield:newField(input "Fax", input {&idxFaxDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxFax}]) then return error.

FieldInstanceList#[{&idxEmailAddress}] = com.quarix.data.dbfield:newField(input "EmailAddress", input {&idxEmailAddressDbPos}, input this-object).
if not valid-object(FieldInstanceList#[{&idxEmailAddress}]) then return error.

end constructor.

method public static integer NumberOfFields():
return Instance:FieldCount().
end method.

end class.
