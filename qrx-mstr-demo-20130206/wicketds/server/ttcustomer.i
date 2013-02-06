define  temp-table ttCustomer no-undo
    before-table btCustomer
    {com/quarix/data/sortorder.i &fields="
    field Address		as character
	field Address2		as character
	field Balance		as decimal
	field City			as character
	field Comments		as character
	field Contact		as character
	field Country		as character
	field CreditLimit	as decimal
	field CustNum		as integer
	field Discount		as integer
	field EmailAddress	as character
	field Fax			as character
	field Name			as character
	field Phone			as character
	field PostalCode	as character
	field SalesRep		as character
	field State			as character
	field Terms			as character
	field StateName		as character
	field SalesRepName  as character
    "}
    index PK_Customer is unique CustNum.
.
