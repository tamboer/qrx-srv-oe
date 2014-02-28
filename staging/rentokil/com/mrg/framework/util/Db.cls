 
 /*------------------------------------------------------------------------
    File        : Db
    Purpose     : DB utility methods
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Thu May 09 18:24:14 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.util.Util.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.util.Db:
    
    /**
     * Empties the content of pcTableName table
     */   
    /**
     * Empties the content of pcTableName table
     */   
    METHOD PUBLIC STATIC VOID EmptyTable(INPUT pcTableName AS CHARACTER):
    
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        DEFINE VARIABLE hQry    AS HANDLE NO-UNDO.
    
        CREATE BUFFER hBuffer FOR TABLE pcTableName.
        CREATE QUERY hQry.
    
        hQry:SET-BUFFERS (hBuffer).
        hQry:QUERY-PREPARE (SUBSTITUTE ("FOR EACH &1 EXCLUSIVE-LOCK", pcTableName)).    
        hQry:QUERY-OPEN ().
    
        DO TRANSACTION:
            DO WHILE hQry:GET-NEXT (EXCLUSIVE-LOCK):
                hBuffer:BUFFER-DELETE (). 
            END.
        END.
    
        FINALLY:
            DELETE OBJECT hBuffer NO-ERROR.
            DELETE OBJECT hQry    NO-ERROR.               
        END FINALLY.
    
    END METHOD.
       
    /**
     * Empties the content of pcTableName table
     */   
    METHOD PUBLIC STATIC VOID EmptyDb(INPUT databaseName AS CHARACTER):
           
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        DEFINE VARIABLE hQry    AS HANDLE NO-UNDO.
        DEFINE VARIABLE hField  AS HANDLE NO-UNDO.
        
        CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE ("&1._file", databaseName).
        CREATE QUERY hQry.
    
        hQry:SET-BUFFERS (hBuffer).
        hQry:QUERY-PREPARE (SUBSTITUTE ("FOR EACH &1._file NO-LOCK WHERE &1._file._hidden = FALSE", databaseName)).    
        hQry:QUERY-OPEN ().
        hField = hBuffer:BUFFER-FIELD ("_file-name").
        
        DO WHILE hQry:GET-NEXT (NO-LOCK):
            EmptyTable (SUBSTITUTE ("&1.&2", databaseName, hField:BUFFER-VALUE)).             
        END.
        
        FINALLY:
            DELETE OBJECT hBuffer NO-ERROR.
            DELETE OBJECT hQry NO-ERROR.               
        END FINALLY.
    
    END METHOD.  
      
END CLASS.