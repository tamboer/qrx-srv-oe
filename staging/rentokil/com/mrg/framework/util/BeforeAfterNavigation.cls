 
/*------------------------------------------------------------------------
   File        : BeforeAfterNavigation
   Purpose     : Object to simplify the usage of a query to walk through before or after image buffers.
                 When walking through before image buffers, the after image buffers are synchronized
                 When walking through after image buffers, the before image buffers are synchronized
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Tue May 21 09:09:24 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.util.BeforeAfterNavigation.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.util.BeforeAfterNavigation: 

    /* Buffer object handle of the current before buffer. Contains the before image of the current record. */
    DEFINE PUBLIC PROPERTY Before AS HANDLE NO-UNDO
        GET.
        PROTECTED SET.

    /* Buffer object handle of the current after buffer. Contains the after image of the current record. */
    DEFINE PUBLIC PROPERTY Buffer AS HANDLE NO-UNDO
        GET.
        PROTECTED SET.

    /* Returns the number of records in the record set */
    DEFINE PUBLIC PROPERTY Count AS INTEGER NO-UNDO
        GET:
            /* Since we use a preselect, we can use the num-results as a quick way to count the records */
            DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
            IF NOT THIS-OBJECT:Query:IS-OPEN THEN 
            DO:
                THIS-OBJECT:Query:QUERY-OPEN().
                iCount = THIS-OBJECT:Query:NUM-RESULTS.
                THIS-OBJECT:Query:QUERY-CLOSE().
            END.
            ELSE
                iCount = THIS-OBJECT:Query:NUM-RESULTS.
            RETURN iCount.
        END.

    /* Is the current record marked as NEW or not */
    DEFINE PUBLIC PROPERTY IsCreated AS LOGICAL NO-UNDO
        GET:
            RETURN VALID-HANDLE(THIS-OBJECT:Buffer) AND valid-handle(THIS-OBJECT:Buffer:BEFORE-BUFFER)
                AND THIS-OBJECT:Buffer:BEFORE-BUFFER:available AND THIS-OBJECT:buffer:BEFORE-BUFFER:row-state = ROW-CREATED.
        END. 

    /* Is the current before image record marked as deleted or not (there is no after image record) */
    DEFINE PUBLIC PROPERTY IsDeleted AS LOGICAL NO-UNDO
        GET:
            RETURN VALID-HANDLE(THIS-OBJECT:Buffer) AND valid-handle(THIS-OBJECT:Buffer:BEFORE-BUFFER)
                AND THIS-OBJECT:Buffer:BEFORE-BUFFER:available AND THIS-OBJECT:buffer:BEFORE-BUFFER:row-state = ROW-DELETED.
        END. 

    /* Is the current before/after image record marked as modified */
    DEFINE PUBLIC PROPERTY IsModified AS LOGICAL NO-UNDO
        GET:
            RETURN VALID-HANDLE(THIS-OBJECT:Buffer) AND valid-handle(THIS-OBJECT:Buffer:BEFORE-BUFFER)
                AND THIS-OBJECT:Buffer:BEFORE-BUFFER:available AND THIS-OBJECT:buffer:BEFORE-BUFFER:row-state = ROW-MODIFIED.
        END. 

    /* Which fields are modified. For new fields this are the fields which are different from the initial values. */
    DEFINE PUBLIC PROPERTY ModifiedFields AS CHARACTER NO-UNDO
        GET:
            DEFINE VARIABLE cChanges     AS CHARACTER NO-UNDO.
            DEFINE VARIABLE iField       AS INTEGER   NO-UNDO.
            DEFINE VARIABLE hBeforeField AS HANDLE    NO-UNDO.
            DEFINE VARIABLE hAfterField  AS HANDLE    NO-UNDO.
            DEFINE VARIABLE lChanged     AS LOGICAL   NO-UNDO.

            IF NOT THIS-OBJECT:buffer:AVAILABLE 
                OR NOT valid-handle(THIS-OBJECT:Buffer:BEFORE-BUFFER) 
                OR NOT THIS-OBJECT:buffer:BEFORE-BUFFER:AVAILABLE 
                OR THIS-OBJECT:Buffer:ROW-STATE = ROW-UNMODIFIED THEN
                RETURN ''.

            DO iField = 1 TO THIS-OBJECT:Buffer:NUM-FIELDS:
                hAfterField = THIS-OBJECT:Buffer:BUFFER-FIELD(iField).
                hBeforeField = THIS-OBJECT:Buffer:BEFORE-BUFFER:buffer-field(iField).
                IF hAfterField:DATA-TYPE = 'character':U THEN
                    lChanged = COMPARE(
                        hAfterField:BUFFER-VALUE,
                        '<>':U,
                        hBeforeField:BUFFER-VALUE,
                        'RAW':U).
                ELSE
                    lChanged = hAfterField:BUFFER-VALUE <> hBeforeField:BUFFER-VALUE.
                IF lChanged THEN
                    cChanges = cChanges + ',':U + hBeforeField:NAME.
            END.
            RETURN SUBSTRING(cChanges, 2).
        END.

    /* Row-state Modified or Created (ROW-MODIFIED + ROW-CREATED) */
    DEFINE PUBLIC STATIC PROPERTY RowChanged AS INTEGER NO-UNDO INITIAL 5
        GET.

    /* A record set is defined by a query. This is the query object which is used */
    DEFINE PUBLIC PROPERTY Query AS HANDLE NO-UNDO
        GET.
        PROTECTED SET.

    /* The query buffer is the buffer which is used to define the record set. This is either the before or the after buffer. */
    DEFINE PUBLIC PROPERTY QueryBuffer AS HANDLE NO-UNDO
        GET.
        PROTECTED SET.

    CONSTRUCTOR PUBLIC BeforeAfterNavigation (INPUT phBuffer AS HANDLE):
        this-object(phBuffer, '':U).
    END CONSTRUCTOR.

    /* The constructor accepts the following parameters:
    * Buffer, required:
    *      Handle of the before image buffer or
    *      Handle of the after image buffer or
    *      Handle of the dataset (the first top-buffer of the dataset will be used)
    * Where, optional:
    *      Where condition to limit or sort the records in the recordset or
    *      ROW-STATE to limit the records in the recordset (ROW-UNMODIFIED, ROW-MODIFIED, ROW-DELETED, ROW-CREATED) */
    CONSTRUCTOR PUBLIC BeforeAfterNavigation (INPUT phBuffer AS HANDLE, INPUT pcWhere AS CHARACTER):
        /* "Overloaded" constructor, support for a dataset handle */
        IF phBuffer:TYPE = 'dataset':U THEN
            phBuffer = phBuffer:GET-BUFFER-HANDLE(1).

        /* "Overloaded" constructor, supports both a query on the before buffer as well as a query on the after buffer as parameters */  
        IF VALID-HANDLE(phBuffer:AFTER-BUFFER) THEN 
        DO:
            THIS-OBJECT:Buffer = phBuffer:AFTER-BUFFER.
            THIS-OBJECT:Before = phBuffer.
        END.
        ELSE 
        DO:
            THIS-OBJECT:Buffer = phBuffer.  
            THIS-OBJECT:Before = phBuffer:BEFORE-BUFFER.
        END.   

        CREATE QUERY THIS-OBJECT:Query.
        THIS-OBJECT:Query:SET-BUFFERS(phBuffer).
        THIS-OBJECT:Query:QUERY-PREPARE(SUBSTITUTE('preselect each &1 where &2':U, phBuffer:NAME, pcWhere)).
        THIS-OBJECT:QueryBuffer = phBuffer.
    END CONSTRUCTOR.

    CONSTRUCTOR PUBLIC BeforeAfterNavigation (INPUT phBuffer AS HANDLE, INPUT piState AS INTEGER):
        THIS-OBJECT (phBuffer, 
            IF piState = BeforeAfterNavigation:RowChanged
            THEN SUBSTITUTE('ROW-STATE(&1) = ROW-MODIFIED OR ROW-STATE(&1) = ROW-CREATED':U, phBuffer:NAME)
            ELSE SUBSTITUTE('ROW-STATE(&1) = &2':U, phBuffer:NAME, piState)).
    END CONSTRUCTOR.

    DESTRUCTOR PUBLIC BeforeAfterNavigation ():
        DELETE OBJECT THIS-OBJECT:Query NO-ERROR.
    END DESTRUCTOR.

    /* Position the cursor at a row (like an index) */
    METHOD PUBLIC LOGICAL GetAt (INPUT piAt AS INTEGER):
        IF NOT THIS-OBJECT:Query:IS-OPEN THEN
            THIS-OBJECT:Query:QUERY-OPEN().
        THIS-OBJECT:Query:REPOSITION-TO-ROW(piAt).
        THIS-OBJECT:Query:GET-NEXT().
        THIS-OBJECT:SynchronizeQuery().
        RETURN NOT THIS-OBJECT:Query:QUERY-OFF-END.
    END METHOD.

    /* Position the cursor at a key position (like a dictionary) */
    METHOD PUBLIC LOGICAL FindByKey (INPUT pcKey AS CHARACTER, INPUT pcValue AS CHARACTER):
        THIS-OBJECT:QueryBuffer:FIND-FIRST(SUBSTITUTE('where &1 = &2':U, pcKey, QUOTER(pcValue))) NO-ERROR.   
        RETURN FindByRowid(THIS-OBJECT:QueryBuffer:ROWID).
    END METHOD.

    METHOD PUBLIC LOGICAL FindByKey (INPUT pcKey AS CHARACTER, INPUT piValue AS INTEGER):
        RETURN THIS-OBJECT:FindByKey(pcKey, STRING(piValue)).
    END METHOD.

    METHOD PUBLIC LOGICAL FindByKey (INPUT pcKey AS CHARACTER, INPUT piValue AS INT64):
        RETURN THIS-OBJECT:FindByKey(pcKey, STRING(piValue)).
    END METHOD.

    METHOD PUBLIC LOGICAL FindByKey (INPUT pcKey AS CHARACTER, INPUT pdValue AS DECIMAL):
        RETURN THIS-OBJECT:FindByKey(pcKey, STRING(pdValue)).
    END METHOD.

    METHOD PUBLIC LOGICAL FindByRowid (INPUT prRowid AS ROWID):
        /* The query should be closed when the record is not in the set */
        IF prRowid <> ? THEN 
        DO:    
            IF NOT THIS-OBJECT:Query:IS-OPEN THEN
                THIS-OBJECT:Query:QUERY-OPEN().

            THIS-OBJECT:Query:REPOSITION-TO-ROWID (prRowid) NO-ERROR.
            /* The error-status:error will not be set when the rowid cannot be found! */
            IF ERROR-STATUS:NUM-MESSAGES = 0 THEN
                THIS-OBJECT:Query:GET-NEXT().
            ELSE
                THIS-OBJECT:Query:QUERY-CLOSE().
        END.
        ELSE 
        DO:
            IF THIS-OBJECT:Query:IS-OPEN THEN
                THIS-OBJECT:Query:QUERY-CLOSE().

        END.
        THIS-OBJECT:SynchronizeQuery().
        RETURN THIS-OBJECT:QueryBuffer:AVAILABLE.
    END METHOD.

    /* Position the cursor at the first row */
    METHOD PUBLIC LOGICAL GetFirst ():
        IF NOT THIS-OBJECT:Query:IS-OPEN THEN
            THIS-OBJECT:Query:QUERY-OPEN().
        THIS-OBJECT:Query:GET-FIRST().
        THIS-OBJECT:SynchronizeQuery().
        RETURN NOT THIS-OBJECT:Query:QUERY-OFF-END.
    END METHOD.

    /* Position the cursor at the next row, it there is no 'current position' than the cursor will be positioned at the first row */
    METHOD PUBLIC LOGICAL GetNext ():
        IF NOT THIS-OBJECT:Query:IS-OPEN THEN 
            RETURN GetFirst().
        ELSE 
        DO:
            THIS-OBJECT:Query:GET-NEXT().
            THIS-OBJECT:SynchronizeQuery().
            RETURN NOT THIS-OBJECT:Query:QUERY-OFF-END.
        END.
    END METHOD.

    /* Determine if the given field in the current row is changed */
    METHOD PUBLIC LOGICAL IsChanged (INPUT phField AS HANDLE):
        IF NOT THIS-OBJECT:buffer:AVAILABLE 
            OR NOT valid-handle(THIS-OBJECT:Buffer:BEFORE-BUFFER) 
            OR NOT THIS-OBJECT:buffer:BEFORE-BUFFER:available 
            OR THIS-OBJECT:Buffer:ROW-STATE = ROW-UNMODIFIED THEN
            RETURN NO.

        IF phField:DATA-TYPE = 'character':U THEN
            RETURN COMPARE(
                phField:BUFFER-VALUE,
                '<>':U,
                THIS-OBJECT:Buffer:BEFORE-BUFFER:buffer-field(phField:NAME):buffer-value,
                'RAW':U).
        ELSE
            RETURN phField:BUFFER-VALUE <> THIS-OBJECT:Buffer:BEFORE-BUFFER:buffer-field(phField:NAME):buffer-value.
    END METHOD.

    /* Determine if the named field in the current row is changed */
    METHOD PUBLIC LOGICAL IsChanged (INPUT pcField AS CHARACTER):
        DEFINE VARIABLE hField AS HANDLE NO-UNDO.

        IF NOT THIS-OBJECT:buffer:AVAILABLE 
            OR NOT valid-handle(THIS-OBJECT:Buffer:BEFORE-BUFFER) 
            OR NOT THIS-OBJECT:buffer:BEFORE-BUFFER:available 
            OR THIS-OBJECT:Buffer:ROW-STATE = ROW-UNMODIFIED THEN
            RETURN NO.

        hField = THIS-OBJECT:Buffer:BUFFER-FIELD(pcField).
        IF hField:DATA-TYPE = 'character':U THEN
            RETURN COMPARE(
                hField:BUFFER-VALUE,
                '<>':U,
                THIS-OBJECT:Buffer:BEFORE-BUFFER:buffer-field(pcField):buffer-value,
                'RAW':U).
        ELSE
            RETURN hField:BUFFER-VALUE <> THIS-OBJECT:Buffer:BEFORE-BUFFER:buffer-field(pcField):buffer-value.
    END METHOD.

    /* Set the cursor before the first row */
    METHOD PUBLIC LOGICAL Rewind ():
        IF THIS-OBJECT:Query:IS-OPEN THEN
            THIS-OBJECT:Query:QUERY-CLOSE().
        IF THIS-OBJECT:QueryBuffer:AVAILABLE THEN  
            THIS-OBJECT:QueryBuffer:BUFFER-RELEASE().
        THIS-OBJECT:SynchronizeQuery().
    END METHOD.

    /* Internal method to position the other buffer (after or before depends on the buffer not used in the query) */
    METHOD PROTECTED VOID SynchronizeQuery ():
        IF THIS-OBJECT:QueryBuffer = THIS-OBJECT:Buffer THEN 
        DO:
            IF THIS-OBJECT:Buffer:BEFORE-ROWID <> ? AND valid-handle(THIS-OBJECT:Before) THEN
                THIS-OBJECT:Before:FIND-BY-ROWID(THIS-OBJECT:Buffer:BEFORE-ROWID).
            ELSE
                THIS-OBJECT:Before:BUFFER-RELEASE().
        END.
        ELSE 
        DO:
            IF THIS-OBJECT:Before:AFTER-ROWID <> ? AND valid-handle(THIS-OBJECT:Buffer) THEN
                THIS-OBJECT:Buffer:FIND-BY-ROWID(THIS-OBJECT:Before:AFTER-ROWID).
            ELSE
                THIS-OBJECT:Buffer:BUFFER-RELEASE().
        END.
    END METHOD.

END CLASS.