 
/*------------------------------------------------------------------------
   File        : RequestContext
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Fri Apr 26 16:29:01 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.system.RequestContext INHERITS com.quarix.data.DataContext:
    
    /* TO DO: Quarix was changed to make the dataset protected instead private; to move this into Quarix source code */
        
    /* Serialize the DataContext object into a human readable string; mainly for logging purposes */            
    METHOD PUBLIC CHARACTER Request2String ():
        
        DEFINE VARIABLE requestString AS CHARACTER NO-UNDO.
        
        /* add filter related information */
        FOR EACH ttFilter:
            requestString = SUBSTITUTE ("&1FILTER: &2.&3 &4 &5~n", requestString, ttFilter.tableName, ttFilter.dbFieldName, ttFilter.operName, ttFilter.fieldValue).
        END.
        
        /* add sorting related information */
        requestString = SUBSTITUTE ("&1SORT:", requestString).
        FOR EACH ttSort:
            requestString = SUBSTITUTE ("&1 by &2.&3&4", requestString, ttSort.tableName, ttSort.fieldName, (IF ttSort.descOrder THEN " descending" ELSE "")).
        END.
        
        /* add properties information */
        requestString = requestString + "~n":U.
        FOR EACH ttProperty:
            requestString = SUBSTITUTE ("&1PROPERTY: &2 = &3~n", requestString, ttProperty.propertyName, ttProperty.propertyValue).            
        END.
        
        RETURN requestString.
        
    END METHOD.
    
    /* apply filter on the specific temp-table field, temp-table name si specified separately */                                
    METHOD PUBLIC LOGICAL SetFilter (tableName AS CHARACTER, fieldName AS CHARACTER, operName AS CHARACTER, fieldValue AS CHARACTER):
        SetFilter (tableName, fieldName, operName, fieldValue, ?).
    END METHOD.
    
    /* apply filter on the specific temp-table field; field specified as <temp-table>.<field-name> */                            
    METHOD PUBLIC LOGICAL SetFilter (fieldName AS CHARACTER, operName AS CHARACTER, fieldValue AS CHARACTER):
        IF NUM-ENTRIES (fieldName, ".") = 2 THEN
            RETURN SetFilter (ENTRY (1, fieldName, "."), ENTRY (2, fieldName, "."), operName, fieldValue, ?).        
        RETURN FALSE.                    
    END METHOD.

    /* set ascending sorting of fieldName; field name is in format <temp-table-name>.<field-name> */
    METHOD PUBLIC LOGICAL SetSort (fieldName AS CHARACTER):        
        RETURN SetSort (fieldName, NO).        
    END METHOD.

    /* set sorting of fieldName; field name is in format <temp-table-name>.<field-name> */
    METHOD PUBLIC LOGICAL SetSort (fieldName AS CHARACTER, descendingSort AS LOGICAL):
        IF NUM-ENTRIES (fieldName, ".") = 2 THEN
            RETURN SetSort (ENTRY (1, fieldName, "."), ENTRY (2, fieldName, "."), descendingSort).
        RETURN FALSE.
    END METHOD.

    /* set no-fill for the tableName */
    METHOD PUBLIC VOID SetNoFill (tableName AS CHARACTER):
        SetNoFill (tableName, TRUE).
    END METHOD.

    /* set teh fill mode for the tableName */
    METHOD PUBLIC VOID SetNoFill (tableName AS CHARACTER, noFill AS LOGICAL):
        SetProperty (SUBSTITUTE ("&1.NoFill", tableName), STRING (noFill, "yes/no")).
    END METHOD.

    /* get noFill flag */
    METHOD PUBLIC LOGICAL GetNoFill (tableName AS CHARACTER):
        RETURN GetProperty (SUBSTITUTE ("&1.NoFill", tableName)) = "yes".
    END METHOD.
                               
END CLASS.