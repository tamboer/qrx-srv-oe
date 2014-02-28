 
/*------------------------------------------------------------------------
   File        : ErrorService
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Wed May 15 18:02:45 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.util.Buffer.
USING com.mrg.framework.service.error.ErrorService.
USING com.quarix.service.error.ErrorCore.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.error.ErrorService INHERITS com.quarix.service.error.ErrorCore:
      
    {com/mrg/framework/service/error/ttError.i &scope=protected}
    
    DEFINE PRIVATE STATIC VARIABLE errorService AS ErrorService NO-UNDO.
    
    DEFINE PUBLIC VARIABLE NumAppErrors AS INTEGER NO-UNDO.
    
    CONSTRUCTOR PRIVATE ErrorService ():
    END CONSTRUCTOR.        
    
    METHOD PUBLIC STATIC OVERRIDE ErrorCore GetInstance():
        IF NOT VALID-OBJECT(errorService) THEN
            errorService = NEW ErrorService ().
        RETURN errorService.
    END METHOD.
    
    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER):
        AddError (errorCode, ?, ?, ?, ?, ?).
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER):
        AddError (errorCode, errorParam1, ?, ?, ?, ?).
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER):
        AddError (errorCode, errorParam1, errorParam2, ?, ?, ?).
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER):
        AddError (errorCode, errorParam1, errorParam2, errorParam3, ?, ?).        
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER):
        AddError (errorCode, errorParam1, errorParam2, errorParam3, errorParam4, ?).        
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER, errorParam5 AS CHARACTER):
        CREATE ttError.
        ASSIGN
            numAppErrors        = numAppErrors + 1
            ttError.ErrorNum    = NumAppErrors
            ttError.ErrorCode   = errorCode                 
            ttError.ErrorParam1 = errorParam1   
            ttError.ErrorParam2 = errorParam2  
            ttError.ErrorParam3 = errorParam3  
            ttError.ErrorParam4 = errorParam4  
            ttError.ErrorParam5 = errorParam5
            ttError.ErrorText   = SUBSTITUTE (GetString (ttError.ErrorCode), ttError.ErrorParam1, ttError.ErrorParam2, ttError.ErrorParam3, ttError.ErrorParam4, ttError.ErrorParam5).
                                  
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER):
        AddValidationError (datasetHandle, bufferField, errorCode, ?, ?, ?, ?, ?).
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER):
        AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, ?, ?, ?, ?).
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER):
        AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, ?, ?, ?).        
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER):
        AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3, ?, ?).        
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER):
        AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3, errorParam4, ?).
    END METHOD.        
    
    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER, errorParam5 AS CHARACTER):
        DEFINE VARIABLE fieldName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE tableName AS CHARACTER NO-UNDO.
        
        IF NUM-ENTRIES (bufferField, ".":U) = 2 THEN
            ASSIGN
                tableName = ENTRY (1, bufferField, ".")
                fieldName = ENTRY (2, bufferField, ".").
        ELSE
            tableName = bufferField.
                            
        CREATE ttError.
        ASSIGN
            numAppErrors        = numAppErrors + 1
            ttError.ErrorNum    = NumAppErrors       
            ttError.ErrorCode   = errorCode      
            ttError.ErrorParam1 = errorParam1   
            ttError.ErrorParam2 = errorParam2  
            ttError.ErrorParam3 = errorParam3  
            ttError.ErrorParam4 = errorParam4  
            ttError.ErrorParam5 = errorParam5
            ttError.ErrorBuffer = tableName
            ttError.ErrorField  = fieldName
            ttError.ErrorKey    = com.mrg.framework.util.Buffer:GetPrimaryKeyValues(datasetHandle:GET-BUFFER-HANDLE (tableName))        
            ttError.ErrorText   = SUBSTITUTE (GetString (ttError.ErrorCode), ttError.ErrorParam1, ttError.ErrorParam2, ttError.ErrorParam3, ttError.ErrorParam4, ttError.ErrorParam5).
                                        
    END METHOD.
    
    /* returns true if any error */
    METHOD PUBLIC LOGICAL AnyError ():
        RETURN NumAppErrors > 0 OR NumMessages > 0 OR NumErrors > 0 OR NumClientErrors > 0 OR NumClientErrors > 0 OR NumApplicationErrors > 0.
    END METHOD.                
    
    /* Reset (delete) errors in the ErrorManager */
    METHOD PUBLIC OVERRIDE VOID Purge ():
        EMPTY TEMP-TABLE ttError.
        NumAppErrors = 0.
        SUPER:Purge ().        
    END METHOD.   
    
    METHOD PUBLIC CHARACTER GetMessage (messageNum AS INTEGER):
        FOR FIRST ttError NO-LOCK 
                WHERE ttError.ErrorNum = messageNum:
            RETURN ttError.ErrorText.
        END.
        RETURN ?.                                    
    END METHOD.        
END CLASS.