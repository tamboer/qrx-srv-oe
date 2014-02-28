 
 /*------------------------------------------------------------------------
    File        : BusinessTask
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Thu Apr 25 18:31:21 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.quarix.base.iSingleton.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.data.BusinessTask ABSTRACT INHERITS com.quarix.base.BaseObject IMPLEMENTS iSingleton:
    
    METHOD PUBLIC VOID Reset ():
    END METHOD.
         
    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER):
        com.mrg.framework.service.Error:AddError (errorCode).
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER):
        com.mrg.framework.service.Error:AddError (errorCode, errorParam1).
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER):
        com.mrg.framework.service.Error:AddError (errorCode, errorParam1, errorParam2).
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER):
        com.mrg.framework.service.Error:AddError (errorCode, errorParam1, errorParam2, errorParam3).        
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER):
        com.mrg.framework.service.Error:AddError (errorCode, errorParam1, errorParam2, errorParam3, errorParam4).        
    END METHOD.        

    METHOD PUBLIC VOID AddError (errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER, errorParam5 AS CHARACTER):
        com.mrg.framework.service.Error:AddError (errorCode, errorParam1, errorParam2, errorParam3, errorParam4, errorParam5).                 
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode).
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1).
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2).        
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3).        
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3, errorParam4).
    END METHOD.        
    
    METHOD PUBLIC VOID AddValidationError (datasetHandle AS HANDLE, bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER, errorParam5 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3, errorParam4, errorParam5).                         
    END METHOD.
    
END CLASS.