 
/*------------------------------------------------------------------------
   File        : Assert
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : andrei.tarba
   Created     : Tue Sep 17 21:13:48 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OEUnit.Assertion.Assert.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.unit.Assert INHERITS Assert: 
  
    /**
     * Checks that the values in a column in the temp-table phTempTable match 
     * the expected ones.
     *
     * phTempTable       - temp-table containing the data to verify
     * pcSortOrder       - valid sort clause, excluding the initial BY
     * pcColumnName      - name of the column to check values for
     * pcExpectedValues  - a delimited list of the values to check, in the order they should appear
     * pcColumnDelimiter - column delimiter used in pcExpectedValues
     * pcMsgPrefix       - text to be prepended to the message output following an assertion failure
     */
    METHOD PUBLIC STATIC VOID HasColumnValues(
        INPUT phTempTable AS HANDLE, 
        INPUT pcSortOrder AS CHARACTER,  
        INPUT pcColumnName AS CHARACTER, 
        INPUT pcExpectedValues AS CHARACTER,
        INPUT pcColumnDelimiter AS CHARACTER,
        INPUT pcMsgPrefix AS CHARACTER):
        
        com.mrg.framework.unit.Assert:HasColumnValues(
            INPUT phTempTable, 
            INPUT "TRUE",  
            INPUT pcSortOrder,  
            INPUT pcColumnName, 
            INPUT pcExpectedValues,
            INPUT pcColumnDelimiter,
            INPUT pcMsgPrefix).
    END METHOD.       
         
    /**
     * Checks that the values in a column in the temp-table phTempTable match 
     * the expected ones.
     *
     * phTempTable       - temp-table containing the data to verify
     * pcWhereClause     - valid where clause, excluding the initial WHERE
     * pcSortOrder       - valid sort clause, excluding the initial BY
     * pcColumnName      - name of the column to check values for
     * pcExpectedValues  - a delimited list of the values to check, in the order they should appear
     * pcColumnDelimiter - column delimiter used in pcExpectedValues
     * pcMsgPrefix       - text to be prepended to the message output following an assertion failure
     */
    METHOD PUBLIC STATIC VOID HasColumnValues(
        INPUT phTempTable AS HANDLE, 
        INPUT pcWhereClause AS CHARACTER,  
        INPUT pcSortOrder AS CHARACTER,  
        INPUT pcColumnName AS CHARACTER, 
        INPUT pcExpectedValues AS CHARACTER,
        INPUT pcColumnDelimiter AS CHARACTER,
        INPUT pcMsgPrefix AS CHARACTER):
    
        DEFINE VARIABLE cColValues AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hCol       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hQry       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lOK        AS LOGICAL   NO-UNDO.

        Assert:IsTrue(VALID-HANDLE(phTempTable), SUBSTITUTE("&1. Temp-Table handle passed to assertNumRows is not valid.", pcMsgPrefix)).
        Assert:IsTrue(phTempTable:TYPE = "TEMP-TABLE", SUBSTITUTE("&1. Handle passed to assertNumRows is of type '&2', not a temp-table.", phTempTable:TYPE, pcMsgPrefix)).
    
        hCol = phTempTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (pcColumnName) NO-ERROR.
    
        Assert:IsTrue(VALID-HANDLE(hCol), SUBSTITUTE("&1 is not a valid column name for temp-table &2", pcColumnName, phTempTable:NAME)).
        
        CREATE QUERY hQry.
        hQry:ADD-BUFFER(phTempTable:DEFAULT-BUFFER-HANDLE).
        
        IF com.mrg.framework.util.Util:IsEmpty(pcWhereClause) THEN
            pcWhereClause = "TRUE".
          
        lOK = hQry:QUERY-PREPARE(SUBST("PRESELECT EACH &1 WHERE &2 BY &3", phTempTable:DEFAULT-BUFFER-HANDLE:NAME, pcWhereClause, pcSortOrder)) NO-ERROR.
    
        Assert:IsTrue(lOK, SUBSTITUTE("Query failed to prepare. Error is: &1", ERROR-STATUS:GET-MESSAGE (1))).
    
        hQry:QUERY-OPEN().    
    
        DO WHILE(hQry:GET-NEXT()):      
            cColValues = cColValues + pcColumnDelimiter + hCol:BUFFER-VALUE().      
        END.
    
        cColValues = SUBSTRING(cColValues, 2).
    
        Assert:IsTrue(pcExpectedValues = cColValues, SUBSTITUTE("&1. Expected columns '&2', but got '&3'", pcMsgPrefix, pcExpectedValues, cColValues)).    
    
        FINALLY: 
            IF VALID-HANDLE(hQry) THEN 
                DELETE OBJECT hQry.
        END.
        
    END METHOD.
    
    /**
     * Checks that the values in a column in the temp-table phTempTable match 
     * the expected ones.
     *
     * phTempTable1       - first temp-table containing the data to verify
     * phTempTable2       - second temp-table containing the data to verify
     * pcWhereClause1     - valid where clause, excluding the initial WHERE
     * pcSortOrder1       - valid sort clause, excluding the initial BY
     * pcColumnName1      - name of the column to check values for
     * pcExpectedValues1  - a delimited list of the values to check, in the order they should appear
     * pcMsgPrefix1       - text to be prepended to the message output following an assertion failure
     * pcWhereClause2     - valid where clause, excluding the initial WHERE
     * pcSortOrder2       - valid sort clause, excluding the initial BY
     * pcColumnName2      - name of the column to check values for
     * pcExpectedValues2  - a delimited list of the values to check, in the order they should appear
     * pcMsgPrefix2       - text to be prepended to the message output following an assertion failure
     * pcColumnDelimiter - column delimiter used in pcExpectedValues
     */
    METHOD PUBLIC STATIC VOID HasColumnValues(
        INPUT phTempTable1 AS HANDLE, 
        INPUT phTempTable2 AS HANDLE, 
        INPUT pcWhereClause1 AS CHARACTER,  
        INPUT pcSortOrder1 AS CHARACTER,  
        INPUT pcColumnName1 AS CHARACTER, 
        INPUT pcExpectedValues1 AS CHARACTER,
        INPUT pcMsgPrefix1 AS CHARACTER,
        INPUT pcWhereClause2 AS CHARACTER,  
        INPUT pcSortOrder2 AS CHARACTER,  
        INPUT pcColumnName2 AS CHARACTER, 
        INPUT pcExpectedValues2 AS CHARACTER,
        INPUT pcMsgPrefix2 AS CHARACTER):
            
        HasColumnValues(phTempTable1, phTempTable2, 
            pcWhereClause1, pcSortOrder1, pcColumnName1, pcExpectedValues1, pcMsgPrefix1,
            pcWhereClause2, pcSortOrder2, pcColumnName2, pcExpectedValues2, pcMsgPrefix2,
            ",").
            
    END METHOD.            
        
    /**
     * Checks that the values in a column in the temp-table phTempTable match 
     * the expected ones.
     *
     * phTempTable1       - first temp-table containing the data to verify
     * phTempTable2       - second temp-table containing the data to verify
     * pcWhereClause1     - valid where clause, excluding the initial WHERE
     * pcSortOrder1       - valid sort clause, excluding the initial BY
     * pcColumnName1      - name of the column to check values for
     * pcExpectedValues1  - a delimited list of the values to check, in the order they should appear
     * pcMsgPrefix1       - text to be prepended to the message output following an assertion failure
     * pcWhereClause2     - valid where clause, excluding the initial WHERE
     * pcSortOrder2       - valid sort clause, excluding the initial BY
     * pcColumnName2      - name of the column to check values for
     * pcExpectedValues2  - a delimited list of the values to check, in the order they should appear
     * pcMsgPrefix2       - text to be prepended to the message output following an assertion failure
     * pcColumnDelimiter - column delimiter used in pcExpectedValues
     */
    METHOD PUBLIC STATIC VOID HasColumnValues(
        INPUT phTempTable1 AS HANDLE, 
        INPUT phTempTable2 AS HANDLE, 
        INPUT pcWhereClause1 AS CHARACTER,  
        INPUT pcSortOrder1 AS CHARACTER,  
        INPUT pcColumnName1 AS CHARACTER, 
        INPUT pcExpectedValues1 AS CHARACTER,
        INPUT pcMsgPrefix1 AS CHARACTER,
        INPUT pcWhereClause2 AS CHARACTER,  
        INPUT pcSortOrder2 AS CHARACTER,  
        INPUT pcColumnName2 AS CHARACTER, 
        INPUT pcExpectedValues2 AS CHARACTER,
        INPUT pcMsgPrefix2 AS CHARACTER,
        INPUT pcColumnDelimiter AS CHARACTER):
    
        DEFINE VARIABLE cColValues1 AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cColValues2 AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hCol1       AS HANDLE NO-UNDO.
        DEFINE VARIABLE hCol2       AS HANDLE NO-UNDO.
        DEFINE VARIABLE hQry        AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lOK         AS LOGICAL   NO-UNDO.

        Assert:IsTrue(VALID-HANDLE(phTempTable1), SUBSTITUTE("&1. Temp-Table handle 1 passed to assertNumRows is not valid.", pcMsgPrefix1)).
        Assert:IsTrue(phTempTable1:TYPE = "TEMP-TABLE", SUBSTITUTE("&1. Handle 1 passed to assertNumRows is of type '&2', not a temp-table.", phTempTable1:TYPE, pcMsgPrefix1)).
        Assert:IsTrue(VALID-HANDLE(phTempTable2), SUBSTITUTE("&1. Temp-Table handle 2 passed to assertNumRows is not valid.", pcMsgPrefix2)).
        Assert:IsTrue(phTempTable2:TYPE = "TEMP-TABLE", SUBSTITUTE("&1. Handle 2 passed to assertNumRows is of type '&2', not a temp-table.", phTempTable2:TYPE, pcMsgPrefix2)).
    
        hCol1 = phTempTable1:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (pcColumnName1) NO-ERROR.
        hCol2 = phTempTable2:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (pcColumnName2) NO-ERROR.
    
        Assert:IsTrue(VALID-HANDLE(hCol1), SUBSTITUTE("&1 is not a valid column name for temp-table &2", pcColumnName1, phTempTable1:NAME)).
        Assert:IsTrue(VALID-HANDLE(hCol2), SUBSTITUTE("&1 is not a valid column name for temp-table &2", pcColumnName2, phTempTable2:NAME)).
        
        CREATE QUERY hQry.
        hQry:ADD-BUFFER(phTempTable1:DEFAULT-BUFFER-HANDLE).
        hQry:ADD-BUFFER(phTempTable2:DEFAULT-BUFFER-HANDLE).
        
        IF com.mrg.framework.util.Util:IsEmpty(pcWhereClause1) THEN
            pcWhereClause1 = "TRUE".
        IF com.mrg.framework.util.Util:IsEmpty(pcWhereClause2) THEN
            pcWhereClause2 = "TRUE".

        lOK = hQry:QUERY-PREPARE(SUBST("PRESELECT EACH &1 WHERE &2, EACH &4 WHERE &5 BY &1.&3 BY &4.&6 ", 
                    phTempTable1:DEFAULT-BUFFER-HANDLE:NAME, pcWhereClause1, pcSortOrder1,
                    phTempTable2:DEFAULT-BUFFER-HANDLE:NAME, pcWhereClause2, pcSortOrder2)) NO-ERROR.
        Assert:IsTrue(lOK, SUBSTITUTE("Query failed to prepare. Error is: &1", ERROR-STATUS:GET-MESSAGE (1))).
    
        hQry:QUERY-OPEN().

        DO WHILE(hQry:GET-NEXT()):
            cColValues1 = cColValues1 + pcColumnDelimiter + hCol1:BUFFER-VALUE().      
            cColValues2 = cColValues2 + pcColumnDelimiter + hCol2:BUFFER-VALUE().      
        END.
    
        cColValues1 = SUBSTRING(cColValues1, 2).
        cColValues2 = SUBSTRING(cColValues2, 2).
    
        Assert:IsTrue(pcExpectedValues1 = cColValues1, SUBSTITUTE("&1. Expected columns '&2', but got '&3'", pcMsgPrefix1, pcExpectedValues1, cColValues1)).    
        Assert:IsTrue(pcExpectedValues2 = cColValues2, SUBSTITUTE("&1. Expected columns '&2', but got '&3'", pcMsgPrefix2, pcExpectedValues2, cColValues2)).    
    
        FINALLY: 
            IF VALID-HANDLE(hQry) THEN 
                DELETE OBJECT hQry.
        END.
        
    END METHOD.
      
    /**
     * Verifies that the number of rows in the temp table pointed to by phTempTable
     * matches piExpectedRows.
     *
     * phTempTable    - handle to the temp-table that is to have its rows counted
     * piExpectedRows - number of rows expected in table referenced in phTempTable
     * pcMsgPrefix    - text to be prepended to the message output following an assertion failure
     */
    METHOD PUBLIC STATIC VOID HasNumRows(INPUT phTempTable AS HANDLE, INPUT piExpectedRows AS INTEGER, INPUT pcMsgPrefix AS CHARACTER) :
    
        HasNumRows(INPUT phTempTable, "", INPUT piExpectedRows, INPUT pcMsgPrefix).
        
    END METHOD.
    
    /**
     * Verifies that the number of rows in the temp table pointed to by phTempTable
     * matches piExpectedRows.
     *
     * phTempTable    - handle to the temp-table that is to have its rows counted
     * pcWhereClause  - valid where clause, excluding the initial WHERE
     * piExpectedRows - number of rows expected in table referenced in phTempTable
     * pcMsgPrefix    - text to be prepended to the message output following an assertion failure
     */
    METHOD PUBLIC STATIC VOID HasNumRows(INPUT phTempTable AS HANDLE, INPUT pcWhereClause AS CHARACTER, INPUT piExpectedRows AS INTEGER, INPUT pcMsgPrefix AS CHARACTER) :
    
        DEFINE VARIABLE hQry AS HANDLE  NO-UNDO.
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE lOK  AS LOGICAL NO-UNDO.

        Assert:IsTrue(VALID-HANDLE(phTempTable), SUBSTITUTE("&1. Temp-Table handle passed to assertNumRows is not valid.", pcMsgPrefix)).    
        Assert:IsTrue(phTempTable:TYPE = "TEMP-TABLE", SUBSTITUTE("&1. Handle passed to assertNumRows is of type '&2', not a temp-table.", phTempTable:TYPE, pcMsgPrefix)).
        
        IF com.mrg.framework.util.Util:IsEmpty(pcWhereClause) THEN 
            pcWhereClause= "TRUE".
            
        CREATE QUERY hQry.
        hQry:ADD-BUFFER(phTempTable:DEFAULT-BUFFER-HANDLE).
        lOK = hQry:QUERY-PREPARE(SUBSTITUTE("PRESELECT EACH &1 WHERE &2", phTempTable:DEFAULT-BUFFER-HANDLE:NAME, pcWhereClause)) NO-ERROR.

        Assert:IsTrue(lOK, SUBSTITUTE("&1. Unable to prepare query. Error: &2", pcMsgPrefix, ERROR-STATUS:GET-MESSAGE (1))).
    
        hQry:QUERY-OPEN().

        Assert:IsTrue(hQry:NUM-RESULTS = piExpectedRows, SUBSTITUTE("&1. Found &2, expected &3 rows", pcMsgPrefix, hQry:NUM-RESULTS, piExpectedRows)).

        FINALLY: 
            IF VALID-HANDLE(hQry) THEN 
                DELETE OBJECT hQry.
        END.
        
    END METHOD.

END CLASS.