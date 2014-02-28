 
/*------------------------------------------------------------------------
   File        : Buffer
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Wed May 15 19:29:19 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.util.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.util.Buffer: 
    
    /* returns a comma separated list with field names in the primary index of the bufferHandle */
    METHOD PUBLIC STATIC CHARACTER GetPrimaryKey (INPUT bufferHandle AS HANDLE):
        DEFINE VARIABLE primaryKey    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE indexInfo     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE indexInfoIdx  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE indexFieldIdx AS INTEGER   NO-UNDO.
        
        IF NOT VALID-HANDLE (bufferHandle) THEN
            RETURN "":U.
            
        REPEAT:
            indexInfoIdx = indexInfoIdx + 1.
            indexInfo = bufferHandle:INDEX-INFORMATION (indexInfoIdx).
            IF indexInfo = ? THEN
                RETURN '':U.
            IF ENTRY (3, indexInfo) = '0':U THEN
                NEXT.
            DO indexFieldIdx = 5 TO NUM-ENTRIES (indexInfo) - 1 BY 2:
                primaryKey = primaryKey
                    + (IF primaryKey = '':U THEN '':U ELSE ',':U)
                    + entry (indexFieldIdx, indexInfo).
            END.
            RETURN primaryKey.
        END.
    END METHOD.
    
    /* returns a chr(1) separated list with primary key values or ? if there is no record selected */
    METHOD PUBLIC STATIC CHARACTER GetPrimaryKeyValues (INPUT bufferHandle AS HANDLE):
        DEFINE VARIABLE fieldList AS CHARACTER NO-UNDO.                
        IF NOT bufferHandle:AVAILABLE THEN
            RETURN ?.            
        fieldList = GetPrimaryKey (bufferHandle).
        IF Util:IsEmpty (fieldList) THEN
            RETURN ?.
        RETURN BufferValueList (bufferHandle, fieldList, CHR (1)).
    END METHOD.
    
    /* Returns a comma-delimited list with field-values. */
    METHOD PUBLIC STATIC CHARACTER BufferValueList (INPUT bufferHandle AS HANDLE, INPUT bufferFields AS CHARACTER):
        RETURN BufferValueList (bufferHandle, bufferFields, ',':U).
    END METHOD.
   
    /* Returns a delimited list with field-values. */
    METHOD PUBLIC STATIC CHARACTER BufferValueList (INPUT bufferHandle AS HANDLE, INPUT bufferFields AS CHARACTER, INPUT listDelimiter AS CHARACTER):
        DEFINE VARIABLE fieldIdx  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE valueList AS CHARACTER NO-UNDO.
        
        IF NOT bufferHandle:AVAILABLE THEN
            RETURN "". 
        
        DO fieldIdx = 1 TO NUM-ENTRIES(bufferFields):
            valueList = SUBSTITUTE('&1&2&3',
                valueList,
                listDelimiter,
                bufferHandle:BUFFER-FIELD(ENTRY(fieldIdx, bufferFields)):BUFFER-VALUE).
        END.
        RETURN SUBSTRING (valueList, 2).
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL IsFieldEmpty (fieldHandle AS HANDLE):
        CASE fieldHandle:DATA-TYPE:
            WHEN "CHARACTER" THEN
                RETURN Util:IsEmpty (fieldHandle:BUFFER-VALUE).
            WHEN "INTEGER" THEN
                RETURN Util:IsEmpty (INTEGER (fieldHandle:BUFFER-VALUE)).
            WHEN "INT64" THEN
                RETURN Util:IsEmpty (INT64 (fieldHandle:BUFFER-VALUE)).                
            WHEN "DECIMAL" THEN
                RETURN Util:IsEmpty (DECIMAL (fieldHandle:BUFFER-VALUE)).
            WHEN "LOGICAL" THEN
                RETURN Util:IsEmpty (LOGICAL (fieldHandle:BUFFER-VALUE)).
            WHEN "DATE" THEN
                RETURN Util:IsEmpty (DATE (fieldHandle:BUFFER-VALUE)).
            WHEN "DATETIME" THEN
                RETURN Util:IsEmpty (DATETIME (fieldHandle:BUFFER-VALUE)).
            WHEN "DATETIME-TZ" THEN
                RETURN Util:IsEmpty (DATETIME-TZ (fieldHandle:BUFFER-VALUE)).                
            OTHERWISE
                RETURN FALSE.                
        END CASE.
    END METHOD.        

END CLASS.