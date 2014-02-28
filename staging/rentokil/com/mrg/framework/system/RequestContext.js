/*
 * RequestContext.js defines the RequestContext object
 * Sample usage:
 * 		var requestContext = new RequestContext ();
 *		requestContext.SetFilter ('ttProduct', 'ProductId', '=', '12345');
 *  	requestContext.SetSort ('ttProduct', 'Name', true);
 *		requestContext.SetBatchSize ('ttProduct', 100);
 *		requestContext.SetNoFill ('ttProductAttribute');
 *		requestContext.SetNoFill ('ttProductAttributeValue');
 *		requestContext.SetNoFill ('ttProductHierarchy');
 *		requestContext.SetProperty ('QueryName', 'ParentOnly');
 *		document.write ('<p>' + requestContext.Serialize () + '</p>');
 */

function RequestContext ()
{
	this.Filter = new Array ();
	this.Sort = new Array ();
	this.BatchSize = new Array ();
	this.NoFill = new Array ();
	this.Property = new Array ();
	this.SetFilter = function (tableName, fieldName, fieldOperator, fieldValue)
	{
		var Filter = new Object ();
		Filter.Table = tableName;
		Filter.Field = fieldName;
		Filter.Operator = fieldOperator;
		Filter.Value = fieldValue;
		this.Filter[this.Filter.length] = Filter;
	};
	this.SetSort = function (tableName, fieldName, Direction)
	{
		var Sort = new Object ();
		Sort.Table = tableName;
		Sort.Field = fieldName;
		Sort.Ascending = Direction;			
		this.Sort[this.Sort.length] = Sort;
	};
	this.SetBatchSize = function (tableName, batchSize)
	{
		var BatchSize = new Object ();
		BatchSize.Table = tableName;
		BatchSize.BatchSize = batchSize;
		this.BatchSize[this.BatchSize.length] = BatchSize;
	};
	this.SetNoFill = function (tableName)
	{
		var NoFill = new Object ();
		NoFill.Table = tableName;
		this.NoFill[this.NoFill.length] = NoFill;
	};
	this.SetProperty = function (propName, propValue)
	{
		var Property = new Object ();
		Property.Name = propName;
		Property.Value = propValue;
		this.Property[this.Property.length] = Property;
	};		
	this.Serialize = function ()
	{
		return JSON.stringify (this);
	};
}

