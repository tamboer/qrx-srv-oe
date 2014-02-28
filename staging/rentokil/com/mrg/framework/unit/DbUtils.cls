 
 /*------------------------------------------------------------------------
    File        : DbUtils
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : andrei.tarba
    Created     : Mon Oct 07 21:58:21 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OEUnit.Assertion.*.

CLASS com.mrg.framework.unit.DbUtils:
     
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
     * Loads a database table with test data, emptying it first.
     *
     * pcTableName  - name of the database table to empty and then load.
     * dataFilePath - directory containing data to load. Should contain the file named in the DUMP-NAME meta-schema
     *                field of the table having data loaded into it.
     */   
    METHOD PUBLIC STATIC VOID LoadData(INPUT pcTableName AS CHARACTER, INPUT dataDirPath AS CHARACTER):
    
        DEFINE VARIABLE cDataFileFullPath AS CHARACTER NO-UNDO.
            
        EmptyTable(pcTableName).
    
        FILE-INFO:FILE-NAME = dataDirPath.
    
        cDataFileFullPath = FILE-INFO:FULL-PATHNAME.

        AssertString:IsNotNullOrEmpty(cDataFileFullPath, SUBSTITUTE("Unable to find data file &1", dataDirPath)).

        Assert:AreEqual(SUBSTRING(FILE-INFO:FILE-TYPE, 1, 1), "D", SUBSTITUTE("'&1' is not a directory. File type is: &2", cDataFileFullPath, FILE-INFO:FILE-TYPE)).
    
        RUN prodict/load_d.p(INPUT pcTableName, INPUT cDataFileFullPath).
    
    END METHOD.   
END CLASS.