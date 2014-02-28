 
 /*------------------------------------------------------------------------
    File        : Error
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Wed May 15 19:51:44 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.service.error.ErrorService.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.Error:
    
    {com/mrg/framework/service/error/ttError.i &static=static}
    
    /* private property used to make sure the error manager is running */
    DEFINE STATIC VARIABLE errorMgr_ AS ErrorService NO-UNDO.
    DEFINE STATIC PROPERTY errorMgr AS ErrorService
        GET:
            IF NOT VALID-OBJECT (errorMgr_) THEN
                errorMgr_ = CAST (ErrorService:GetInstance (), ErrorService).
            RETURN errorMgr_.
        END GET.
        SET. 
        
    DEFINE PUBLIC STATIC PROPERTY NumAppErrors AS INTEGER
        GET ():
            RETURN errorMgr:NumAppErrors.
        END GET.        
        
    METHOD PUBLIC STATIC VOID AddError (errorCode AS CHARACTER):
        errorMgr:AddError (errorCode).
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER):
        errorMgr:AddError (errorCode, errorParam1).
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER):
        errorMgr:AddError (errorCode, errorParam1, errorParam2).
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER):
        errorMgr:AddError (errorCode, errorParam1, errorParam2, errorParam3).        
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER):
        errorMgr:AddError (errorCode, errorParam1, errorParam2, errorParam3, errorParam4).        
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER, errorParam5 AS CHARACTER):
        errorMgr:AddError (errorCode, errorParam1, errorParam2, errorParam3, errorParam4, errorParam5).                 
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER):
        errorMgr:AddValidationError (datasetHandle, bufferField, errorCode).
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER):
        errorMgr:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1).
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER):
        errorMgr:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2).        
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER):
        errorMgr:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3).        
    END METHOD.        

    METHOD PUBLIC STATIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER):
        errorMgr:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3, errorParam4).
    END METHOD.        
    
    METHOD PUBLIC STATIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER, errorParam5 AS CHARACTER):
        errorMgr:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3, errorParam4, errorParam5).                         
    END METHOD.
            
    /* returns true if any error */
    METHOD PUBLIC STATIC LOGICAL AnyError ():
        RETURN errorMgr:AnyError ().
    END METHOD.  
                  
    /* cleanup errros*/    
    METHOD PUBLIC STATIC VOID Purge ():
        errorMgr:Purge ().
    END METHOD.          

    /* cleanup errros*/    
    METHOD PUBLIC STATIC CHARACTER GetMessage (numMessage AS INTEGER):
        RETURN errorMgr:GetMessage (numMessage).
    END METHOD.          

    /* cleanup errros*/    
    METHOD PUBLIC STATIC CHARACTER GetError (numMessage AS INTEGER):
        RETURN errorMgr:GetError (numMessage).
    END METHOD.          

    
END CLASS.