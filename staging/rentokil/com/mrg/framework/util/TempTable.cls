 
/*------------------------------------------------------------------------
   File        : TempTable
   Purpose     : Temp-table related utility methods
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Thu May 09 12:59:34 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.util.TempTable:
     
    /* Translate the temp-table field name in a database field name based on the map */
    METHOD PUBLIC STATIC CHARACTER MappedDbField (INPUT fieldName AS CHARACTER, INPUT fieldsMap AS CHARACTER):
        DEF VAR cEntry AS CHARACTER NO-UNDO.
        DEF VAR iEntry AS INTEGER   NO-UNDO.

        DO iEntry = NUM-ENTRIES (fieldsMap) - 1 TO 1 BY -2:
            IF ENTRY (iEntry, fieldsMap) = fieldName THEN
                RETURN ENTRY (iEntry + 1, fieldsMap).
        END.
        RETURN ?.
    END METHOD.

    /* Translate the db field name in a temp-table field name based on the map */
    METHOD PUBLIC STATIC CHARACTER MappedTtField (INPUT fieldName AS CHARACTER, INPUT fieldsMap AS CHARACTER):
        DEF VAR cEntry AS CHARACTER NO-UNDO.
        DEF VAR iEntry AS INTEGER   NO-UNDO.

        DO iEntry = NUM-ENTRIES (fieldsMap) - 1 TO 1 BY -2:
            IF ENTRY (iEntry + 1, fieldsMap) = fieldName THEN
                RETURN ENTRY (iEntry, fieldsMap).
        END.
        RETURN ?.
    END METHOD.
    
END CLASS.