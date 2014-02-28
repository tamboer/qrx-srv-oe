 
/*------------------------------------------------------------------------
   File        : Dataset
   Purpose     : Dataset related utility methods
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Thu May 09 12:59:16 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.util.Dataset FINAL: 
    
    /* Returns the first buffer that contains a field with the specified name */
    METHOD PUBLIC STATIC CHARACTER GetFieldBufferName (INPUT DsDataset AS HANDLE, INPUT bufferFieldName AS CHARACTER):
        DEFINE VARIABLE bufferNum    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE bufferHandle AS HANDLE    NO-UNDO.
        DEFINE VARIABLE fieldHandle  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE tableName    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE fieldName    AS CHARACTER NO-UNDO.

        IF NOT VALID-HANDLE (DsDataset) THEN
            RETURN ?.

        IF NUM-ENTRIES (bufferFieldName, '.':U) >= 2 THEN
            ASSIGN
                tableName = ENTRY (1, bufferFieldName, '.':U)
                fieldName = ENTRY (2, bufferFieldName, '.':U).
        ELSE
            fieldName = bufferFieldName.

        IF tableName <> ? AND tableName <> '':U THEN
            RETURN tableName.

        DO bufferNum = 1 TO DsDataset:NUM-BUFFERS:
            bufferHandle = DsDataset:GET-BUFFER-HANDLE (bufferNum).
            fieldHandle = bufferHandle:BUFFER-FIELD (bufferFieldName) NO-ERROR.
            IF VALID-HANDLE (fieldHandle) THEN
                RETURN bufferHandle:NAME.
        END.

        RETURN ?.
        
    END METHOD.
    
    /* Returns the handle of a temp-table field */    
    METHOD PUBLIC STATIC HANDLE GetFieldHandle (INPUT DsDataset AS HANDLE, INPUT bufferFieldName AS CHARACTER):
        DEFINE VARIABLE bufferNum    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE bufferHandle AS HANDLE    NO-UNDO.
        DEFINE VARIABLE fieldHandle  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE tableName    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE fieldName    AS CHARACTER NO-UNDO.

        IF bufferFieldName = '':U OR bufferFieldName = ? THEN
            RETURN ?.

        IF NOT VALID-HANDLE (DsDataset) THEN
            RETURN ?.

        IF NUM-ENTRIES (bufferFieldName, '.':U) >= 2 THEN
            ASSIGN
                tableName = ENTRY (1, bufferFieldName, '.':U)
                fieldName = ENTRY (2, bufferFieldName, '.':U).
        ELSE
            fieldName = bufferFieldName.
        IF tableName > '':U THEN 
        DO:
            bufferHandle = DsDataset:GET-BUFFER-HANDLE (tableName).
            fieldHandle = bufferHandle:BUFFER-FIELD (fieldName) NO-ERROR.
            IF VALID-HANDLE (fieldHandle) THEN
                RETURN fieldHandle.
        END.
        DO bufferNum = 1 TO DsDataset:NUM-BUFFERS:
            bufferHandle = DsDataset:GET-BUFFER-HANDLE (bufferNum).
            fieldHandle = bufferHandle:BUFFER-FIELD (fieldName) NO-ERROR.
            IF VALID-HANDLE (fieldHandle) THEN
                RETURN fieldHandle.
        END.
        RETURN ?.
    END METHOD.
    
    /* Gets the data type of a field */
    METHOD PUBLIC STATIC CHARACTER GetFieldDataType (INPUT DsDataset AS HANDLE, INPUT bufferFieldName AS CHARACTER):
        DEFINE VARIABLE fieldHandle AS HANDLE NO-UNDO.
        fieldHandle = GetFieldHandle (DsDataset, bufferFieldName).
        IF VALID-HANDLE(fieldHandle) THEN
            RETURN fieldHandle:DATA-TYPE.
        RETURN ?.
    END METHOD.
    
    /* Returns the main table name */
    METHOD PUBLIC STATIC CHARACTER GetMainTable (INPUT DsDataset AS HANDLE):
        DEFINE VARIABLE hDataSource AS HANDLE NO-UNDO.
        IF NOT VALID-HANDLE (DsDataset) THEN
            RETURN '':U.
        hDataSource = DsDataset:GET-TOP-BUFFER (1):data-source.
        IF NOT VALID-HANDLE (hDataSource) THEN
            RETURN '':U.
        RETURN hDataSource:GET-SOURCE-BUFFER (1):name.
    END METHOD.
    
    /* Gets the first top buffer name */
    METHOD PUBLIC STATIC CHARACTER GetDefaultTable (INPUT DsDataset AS HANDLE):
        IF NOT VALID-HANDLE (DsDataset) THEN
            RETURN '':U.
        RETURN DsDataset:GET-TOP-BUFFER(1):table.
    END METHOD.
        
    /* returns the default query for a specific temp-table based on dataset relations */
    METHOD PUBLIC STATIC HANDLE GetDefaultQueryHandle (INPUT DsDataset AS HANDLE, INPUT tableName AS CHARACTER):
        DEFINE VARIABLE bufferHandle AS HANDLE NO-UNDO.
        DEFINE VARIABLE hQuery       AS HANDLE NO-UNDO.
        hQuery = DsDataset:TOP-NAV-QUERY (tableName) NO-ERROR.
        IF VALID-HANDLE (hQuery) THEN
            RETURN hQuery.
        bufferHandle = DsDataset:GET-BUFFER-HANDLE (tableName) NO-ERROR.
        IF NOT VALID-HANDLE (bufferHandle) THEN
            RETURN ?.
        RETURN bufferHandle:PARENT-RELATION:query.
    END METHOD.

    /* Returns the buffer with a specific name, if no name is given returns the buffer with the default name */
    METHOD PUBLIC STATIC HANDLE GetBufferHandle (INPUT dsDataset AS HANDLE, INPUT tableName AS CHARACTER, INPUT defaultTable AS CHARACTER):
        IF NOT VALID-HANDLE (dsDataset) THEN
            RETURN ?.
        IF tableName = '':U OR tableName = ? THEN 
        DO:
            IF defaultTable <> '':U AND defaultTable <> ? THEN
                RETURN dsDataset:GET-BUFFER-HANDLE (defaultTable).
            ELSE
                RETURN dsDataset:GET-TOP-BUFFER(1).
        END.
        ELSE
            RETURN dsDataset:GET-BUFFER-HANDLE (tableName).
    END METHOD.

    /* Returns the buffer with a specific name */
    METHOD PUBLIC STATIC HANDLE GetBufferHandle (INPUT dsDataset AS HANDLE, INPUT tableName AS CHARACTER):
        RETURN GetBufferHandle(dsDataset, tableName, '').
    END METHOD.
    
    /* Set the FILL-MODE attribute of all buffers in the dsDataset to value fillMode */
    METHOD PUBLIC STATIC VOID SetFillMode (INPUT dsDataset AS HANDLE, INPUT fillMode AS CHARACTER):
        DEFINE VARIABLE bufferIdx AS INTEGER NO-UNDO.
        DO bufferIdx = 1 TO DsDataset:NUM-BUFFERS:
            dsDataset:GET-BUFFER-HANDLE (bufferIdx):FILL-MODE = fillMode. 
        END.
    END METHOD.
    
    /* Set the no-fill attribute of all buffers in the dataset */
    METHOD PUBLIC STATIC VOID SetNoFill (INPUT dsDataset AS HANDLE):
        SetFillMode (dsDataset, "no-fill").
    END METHOD.
    
    @Imo.method.
    
    /* Turn on/off tracking changes for every table in a dataset. */
    METHOD PUBLIC STATIC VOID TrackChanges (INPUT DsDataset AS HANDLE, INPUT trackChanges AS LOGICAL):
        DEFINE VARIABLE iBuffer AS INTEGER NO-UNDO.
        DO iBuffer = 1 TO DsDataset:NUM-BUFFERS:
            DsDataset:GET-BUFFER-HANDLE(iBuffer):table-handle:tracking-changes = trackChanges.
        END.
    END METHOD.
    
        
END CLASS.