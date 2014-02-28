 
/*------------------------------------------------------------------------
   File        : BusinessEntity
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Thu Apr 25 18:30:33 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.quarix.data.DataModel.
USING com.mrg.framework.system.RequestContext.
USING com.mrg.framework.util.BeforeAfterNavigation.
USING com.mrg.framework.util.Buffer.
USING com.mrg.framework.util.Dataset.
USING com.quarix.base.iSingleton.
USING com.mrg.framework.data.iBusinessEntity.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.data.BusinessEntity ABSTRACT INHERITS DataModel IMPLEMENTS iSingleton, iBusinessEntity:
    
    DEFINE PRIVATE TEMP-TABLE ttMandatoryFields
        FIELD TableName AS CHARACTER
        FIELD FieldList AS CHARACTER
        FIELD ValidationErrorString AS CHARACTER
        INDEX i1 TableName FieldList.
         
    METHOD PUBLIC VOID Reset ():
    END METHOD.
        
    DEFINE PROTECTED PROPERTY DatasetClassName AS CHARACTER NO-UNDO
        PROTECTED GET.
         PROTECTED SET.
             
    CONSTRUCTOR PUBLIC BusinessEntity ():
        DatasetClassName = REPLACE (GetClass ():TypeName, ".BE", ".ds").
    END CONSTRUCTOR.        
    
    /* returns data in the dsDataset based on filter criteria found int the dataContext object */
    METHOD PUBLIC VOID FetchData (requestContext AS RequestContext, OUTPUT DATASET-HANDLE dsDataset):
        SetDataContext (requestContext).
        dataFetch (OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE).
    END METHOD.

    /* stores data in the dsDataset */
    METHOD PUBLIC LOGICAL StoreData (INPUT-OUTPUT DATASET-HANDLE dsDataset):
        DEFINE VARIABLE dataContext AS com.quarix.data.DataContext.
        define variable da as com.mrg.framework.data.DataAccess no-undo.
        
        dataContext = NEW com.quarix.data.DataContext ().
        SetDataContext (dataContext).
        da = cast (DataAccess, com.mrg.framework.data.DataAccess).
        da:SavingData = yes.        
        RETURN dataUpdate (INPUT-OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE).
        FINALLY:
            DELETE OBJECT dataContext.            
            da:SavingData = no.
        END FINALLY.
    END METHOD.
    
    /* override to make generic validations, like mandatory fields and FK */
    METHOD PUBLIC OVERRIDE LOGICAL BeforeSaveData ():
        IF NOT ValidateData () THEN
            RETURN FALSE.
        RETURN SUPER:BeforeSaveData ().                       
    END METHOD.        
    
    /* valdiates mandatory fields and foreign keys */
    METHOD PUBLIC LOGICAL ValidateData ():
        DEFINE VARIABLE objNavigation AS BeforeAfterNavigation NO-UNDO.
        DEFINE VARIABLE fieldIdx      AS INTEGER               NO-UNDO.
        DEFINE VARIABLE changedFields AS CHARACTER             NO-UNDO.
        DEFINE VARIABLE fieldName     AS CHARACTER             NO-UNDO.
        DEFINE VARIABLE fieldHandle   AS HANDLE                NO-UNDO.
        DEFINE VARIABLE anyError      AS LOGICAL               NO-UNDO.        
        FOR EACH ttMandatoryFields ON ERROR UNDO, THROW:
            IF Util:IsEmpty(ttMandatoryFields.TableName) THEN
                ttMandatoryFields.TableName = datasetHandle:GET-BUFFER-HANDLE (1):NAME.                
            objNavigation = NEW com.mrg.framework.util.BeforeAfterNavigation (datasetHandle:GET-BUFFER-HANDLE (ttMandatoryFields.TableName), BeforeAfterNavigation:RowChanged).
            DO WHILE objNavigation:GetNext():                
                changedFields = objNavigation:ModifiedFields.
                DO fieldIdx = 1 TO NUM-ENTRIES (ttMandatoryFields.fieldList):
                    fieldName = ENTRY (fieldIdx, ttMandatoryFields.fieldList).
                    IF CAN-DO (changedFields, fieldName) THEN
                    DO:
                        fieldHandle = objNavigation:Buffer:BUFFER-FIELD (fieldName) NO-ERROR.
                        IF VALID-HANDLE (fieldHandle) AND com.mrg.framework.util.Buffer:IsFieldEmpty(fieldHandle) THEN
                        DO:
                            AddValidationError (objNavigation:Buffer:Name + "." + fieldName, ttMandatoryFields.ValidationErrorString, GetString (fieldHandle:LABEL)).
                            anyError = YES.
                        END.
                    END.                                        
                END.                    
            END.                    
        END.
        
        RETURN NOT anyError.
        
        FINALLY:
            DELETE OBJECT objNavigation NO-ERROR.
        END FINALLY.
        
    END METHOD.
    
    /* return true if at least one record is found to match the criteira in the requestContext */ 
    METHOD PUBLIC LOGICAL CanFind (queryString AS CHARACTER):
        DEFINE VARIABLE datasetObj AS com.mrg.framework.data.Dataset NO-UNDO.
        DEFINE VARIABLE dsHandle AS HANDLE NO-UNDO.        
        datasetObj = CAST (GetInstance (DatasetClassName), com.mrg.framework.data.Dataset).
        dsHandle = datasetObj:Handle.        
        RETURN CAST (DataAccess, com.mrg.framework.data.DataAccess):CanFind (DATASET-HANDLE dsHandle BY-REFERENCE, queryString).
        FINALLY:
            UnloadInstance (datasetObj).
        END FINALLY.                    
    END METHOD.
            
    /* the fields added with this method are validated at save data to be mandatory */     
    METHOD PROTECTED VOID AddMandatoryFields (fieldList AS CHARACTER):
        AddMandatoryFields ("", fieldList).
    END METHOD.                    

    /* the fields added with this method are validated at save data to be mandatory */     
    METHOD PROTECTED VOID AddMandatoryFields (tableName AS CHARACTER, fieldList AS CHARACTER):
        AddMandatoryFields (tableName, fieldList, "MRG00001").
    END METHOD.
    
    /* the fields added with this method are validated at save data to be mandatory */     
    METHOD PROTECTED VOID AddMandatoryFields (tableName AS CHARACTER, fieldList AS CHARACTER, errorCode AS CHARACTER):
        IF Util:IsEmpty (fieldList) THEN
            RETURN.
        FIND FIRST ttMandatoryFields NO-LOCK
            WHERE ttMandatoryFields.TableName = tableName
              AND ttMandatoryFields.FieldList = fieldList
            NO-ERROR.
        IF NOT AVAILABLE (ttMandatoryFields) THEN 
        DO:
            CREATE ttMandatoryFields.
            ASSIGN
                ttMandatoryFields.TableName = tableName
                ttMandatoryFields.FieldList = fieldList.
        END.
        ttMandatoryFields.ValidationErrorString = errorCode.
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

    METHOD PUBLIC VOID AddValidationError (bufferField AS CHARACTER, errorCode AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode).
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1).
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2).        
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3).        
    END METHOD.        

    METHOD PUBLIC VOID AddValidationError (bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3, errorParam4).
    END METHOD.        
    
    METHOD PUBLIC VOID AddValidationError (bufferField AS CHARACTER, errorCode AS CHARACTER, errorParam1 AS CHARACTER, errorParam2 AS CHARACTER, errorParam3 AS CHARACTER, errorParam4 AS CHARACTER, errorParam5 AS CHARACTER):
        com.mrg.framework.service.Error:AddValidationError (datasetHandle, bufferField, errorCode, errorParam1, errorParam2, errorParam3, errorParam4, errorParam5).                         
    END METHOD.
            
END CLASS.