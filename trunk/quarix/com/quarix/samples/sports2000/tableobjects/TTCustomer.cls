class com.quarix.samples.sports2000.tableobjects.TTCustomer inherits com.quarix.data.tttableobject final:

&scoped-define idxrowid 1

&scoped-define idxrowstate 2

&scoped-define idxSortOrder 3

&scoped-define idxcustNum 4

&scoped-define idxcountry 5

&scoped-define idxname 6

&scoped-define idxaddress 7

&scoped-define idxaddress2 8

&scoped-define idxcity 9

&scoped-define idxstate 10

&scoped-define idxpostalCode 11

&scoped-define idxcontact 12

&scoped-define idxphone 13

&scoped-define idxsalesRep 14

&scoped-define idxcreditLimit 15

&scoped-define idxbalance 16

&scoped-define idxterms 17

&scoped-define idxdiscount 18

&scoped-define idxcomments 19

&scoped-define idxNumFields 19

define public static property Instance as com.quarix.samples.sports2000.tableobjects.TTCustomer no-undo
public get:
if not valid-object(Instance) then Instance = new com.quarix.samples.sports2000.tableobjects.TTCustomer().

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

define public static property custNum as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxcustNum}], com.quarix.data.ttfield).
end.
private set.

define public static property country as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxcountry}], com.quarix.data.ttfield).
end.
private set.

define public static property name as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxname}], com.quarix.data.ttfield).
end.
private set.

define public static property address as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxaddress}], com.quarix.data.ttfield).
end.
private set.

define public static property address2 as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxaddress2}], com.quarix.data.ttfield).
end.
private set.

define public static property city as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxcity}], com.quarix.data.ttfield).
end.
private set.

define public static property state as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxstate}], com.quarix.data.ttfield).
end.
private set.

define public static property postalCode as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxpostalCode}], com.quarix.data.ttfield).
end.
private set.

define public static property contact as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxcontact}], com.quarix.data.ttfield).
end.
private set.

define public static property phone as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxphone}], com.quarix.data.ttfield).
end.
private set.

define public static property salesRep as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxsalesRep}], com.quarix.data.ttfield).
end.
private set.

define public static property creditLimit as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxcreditLimit}], com.quarix.data.ttfield).
end.
private set.

define public static property balance as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxbalance}], com.quarix.data.ttfield).
end.
private set.

define public static property terms as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxterms}], com.quarix.data.ttfield).
end.
private set.

define public static property discount as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxdiscount}], com.quarix.data.ttfield).
end.
private set.

define public static property comments as com.quarix.data.ttfield no-undo
get:
return cast(Instance:FieldInstanceList#[{&idxcomments}], com.quarix.data.ttfield).
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

FieldInstanceList#[{&idxcustNum}] = com.quarix.data.ttfield:newField(input "custNum", input this-object).
if not valid-object(FieldInstanceList#[{&idxcustNum}]) then return error.

FieldInstanceList#[{&idxcountry}] = com.quarix.data.ttfield:newField(input "country", input this-object).
if not valid-object(FieldInstanceList#[{&idxcountry}]) then return error.

FieldInstanceList#[{&idxname}] = com.quarix.data.ttfield:newField(input "name", input this-object).
if not valid-object(FieldInstanceList#[{&idxname}]) then return error.

FieldInstanceList#[{&idxaddress}] = com.quarix.data.ttfield:newField(input "address", input this-object).
if not valid-object(FieldInstanceList#[{&idxaddress}]) then return error.

FieldInstanceList#[{&idxaddress2}] = com.quarix.data.ttfield:newField(input "address2", input this-object).
if not valid-object(FieldInstanceList#[{&idxaddress2}]) then return error.

FieldInstanceList#[{&idxcity}] = com.quarix.data.ttfield:newField(input "city", input this-object).
if not valid-object(FieldInstanceList#[{&idxcity}]) then return error.

FieldInstanceList#[{&idxstate}] = com.quarix.data.ttfield:newField(input "state", input this-object).
if not valid-object(FieldInstanceList#[{&idxstate}]) then return error.

FieldInstanceList#[{&idxpostalCode}] = com.quarix.data.ttfield:newField(input "postalCode", input this-object).
if not valid-object(FieldInstanceList#[{&idxpostalCode}]) then return error.

FieldInstanceList#[{&idxcontact}] = com.quarix.data.ttfield:newField(input "contact", input this-object).
if not valid-object(FieldInstanceList#[{&idxcontact}]) then return error.

FieldInstanceList#[{&idxphone}] = com.quarix.data.ttfield:newField(input "phone", input this-object).
if not valid-object(FieldInstanceList#[{&idxphone}]) then return error.

FieldInstanceList#[{&idxsalesRep}] = com.quarix.data.ttfield:newField(input "salesRep", input this-object).
if not valid-object(FieldInstanceList#[{&idxsalesRep}]) then return error.

FieldInstanceList#[{&idxcreditLimit}] = com.quarix.data.ttfield:newField(input "creditLimit", input this-object).
if not valid-object(FieldInstanceList#[{&idxcreditLimit}]) then return error.

FieldInstanceList#[{&idxbalance}] = com.quarix.data.ttfield:newField(input "balance", input this-object).
if not valid-object(FieldInstanceList#[{&idxbalance}]) then return error.

FieldInstanceList#[{&idxterms}] = com.quarix.data.ttfield:newField(input "terms", input this-object).
if not valid-object(FieldInstanceList#[{&idxterms}]) then return error.

FieldInstanceList#[{&idxdiscount}] = com.quarix.data.ttfield:newField(input "discount", input this-object).
if not valid-object(FieldInstanceList#[{&idxdiscount}]) then return error.

FieldInstanceList#[{&idxcomments}] = com.quarix.data.ttfield:newField(input "comments", input this-object).
if not valid-object(FieldInstanceList#[{&idxcomments}]) then return error.

end constructor.

method public static integer NumberOfFields():
return Instance:FieldCount().
end method.

end class.
