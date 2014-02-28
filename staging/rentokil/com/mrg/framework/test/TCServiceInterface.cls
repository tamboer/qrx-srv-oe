 
 /*------------------------------------------------------------------------
    File        : TCServiceInterface
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : andrei.tarba
    Created     : Thu Oct 24 19:37:20 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.unit.Assert.

CLASS com.mrg.framework.test.TCServiceInterface: 

    DEFINE PRIVATE VARIABLE jsonString AS CHARACTER NO-UNDO INIT '~{"Filter":[~{"Table":"ttProduct","Field":"ProductId","Operator":"=","Value":"12345"~},~{"Table":"ttArticle","Field":"ArticleNo","Operator":"=","Value":"ABC"~}],"Sort":[~{"Table":"ttProduct","Field":"Name","Ascending":true~}],"BatchSize":[~{"Table":"ttProduct","BatchSize":100~}],"NoFill":[~{"Table":"ttProductAttribute"~},~{"Table":"ttProductAttributeValue"~},~{"Table":"ttProductHierarchy"~}],"Property":[~{"Name":"QueryName","Value":"ParentOnly"~}]~}'.
    DEFINE PRIVATE VARIABLE serviceInterface AS com.mrg.framework.system.rest.ServiceInterface. 
     
    @BeforeClass.
    METHOD PUBLIC VOID PrepareTests ():
        serviceInterface = NEW com.mrg.framework.system.rest.ServiceInterface ().
    END METHOD.        

    @AfterClass.
    METHOD PUBLIC VOID EndTests ():
        DELETE OBJECT serviceInterface NO-ERROR.
    END METHOD.        
    
    /* tests converting jsonString to RequestContext object */
    @Test.
    METHOD PUBLIC VOID TestConvertRequest ():
        
        DEFINE VARIABLE requestContext AS com.mrg.framework.system.RequestContext NO-UNDO.
          
        requestContext = serviceInterface:ConvertRequest(jsonString).

        /* test filter */
        Assert:AreEqual (
            requestContext:GetFilterValue ("ttProduct", "ProductId", "="),
            "12345",
            SUBSTITUTE ("Filter on ttProduct.ProductID: expected 12345 but received &1", requestContext:GetFilterValue ("ttProduct", "ProductId", "="))).
            
        /* test sorting */
        Assert:AreEqual (
            requestContext:GetTableSort ("ttProduct"),
            " by Name",
            SUBSTITUTE ("Sorting on ttProduct: expected 'by ProductId' but received &1", requestContext:GetTableSort ("ttProduct"))). 
            
        /* test batch size */                   
        Assert:AreEqual (
            requestContext:GetBatchSize ("ttProduct"),
            100,
            SUBSTITUTE ("Batch size: expected 100 received &1", requestContext:GetBatchSize ('ttProduct'))).

        /* test no-fill */                   
        Assert:AreEqual (
            requestContext:GetNoFill ("ttProductAttribute"),
            TRUE,
            SUBSTITUTE ("No-fill: expected TRUE received &1", requestContext:GetNoFill ("ttProductAttribute"))).
            
        /* test property */            
        Assert:AreEqual (
            requestContext:GetProperty ("QueryName"),
            "ParentOnly",
            SUBSTITUTE ("Value for property QueryName: expected ParentOnly but received &1", requestContext:GetProperty ("QueryName"))).
        
    END METHOD.
    
                           
END CLASS.