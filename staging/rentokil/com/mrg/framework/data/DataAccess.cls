 
/*------------------------------------------------------------------------
   File        : DataAccess
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Thu Apr 25 18:29:59 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.quarix.data.DataAccess.
USING com.mrg.framework.util.Dataset.
USING com.mrg.framework.util.Db.
USING com.mrg.framework.util.TempTable.
USING com.mrg.framework.system.RequestContext.
USING com.mrg.framework.util.BeforeAfterNavigation.
USING com.quarix.base.iSingleton.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.data.DataAccess ABSTRACT INHERITS com.quarix.data.DataAccess IMPLEMENTS iSingleton:
    
    DEFINE PROTECTED VARIABLE Translatable AS LOGICAL NO-UNDO.
    /* list of tabels and corresponding translation tables and fields */
    /* format: db-table1,trasnlation-db-table-1,translatable-fields-1.... */     
    DEFINE PRIVATE VARIABLE translationTables AS CHARACTER NO-UNDO.
    DEFINE PUBLIC VARIABLE SavingData AS LOGICAL NO-UNDO. 
    
    CONSTRUCTOR PUBLIC DataAccess ():
        SUPER ().
        /* Quarix only fills the children of the first parerent; we need al the children to be filled */
        FillAllChildren = TRUE.
    END CONSTRUCTOR.  
     
    METHOD PUBLIC VOID Reset ():
    END METHOD.            
    
    /* create/update/delete the translation related table if any */       
    METHOD PUBLIC OVERRIDE  LOGICAL BeforeRowSave (ttBuffer AS HANDLE, ttBIBuffer AS HANDLE, srcBuffer AS HANDLE):
        
        IF Translatable THEN
            SaveTranslatableData (ttBuffer, ttBIBuffer, srcBuffer).
        
        RETURN TRUE.
        
        CATCH appError AS Progress.Lang.Error :    
            ThrowError(appError).
            DELETE OBJECT appError.
            RETURN FALSE.
        END CATCH.
                        
    END METHOD.
    
    /* saves data in the translation table */
    METHOD PROTECTED VOID SaveTranslatableData (ttBuffer AS HANDLE, ttBIBuffer AS HANDLE, srcBuffer AS HANDLE):
        
        DEFINE VARIABLE translationBuffer AS HANDLE NO-UNDO.
        DEFINE VARIABLE translationTable AS CHARACTER NO-UNDO.

        translationTable = getTranslationTable (ttBuffer:NAME).                        
        IF NOT Util:IsEmpty (translationTable) THEN DO:
            CREATE BUFFER translationBuffer FOR TABLE TranslationTable.
            translationBuffer:FIND-FIRST (
                SUBSTITUTE (
                    "WHERE &1.&2Id = &3 AND &1.LanguageCode = &4",
                    TranslationTable,
                    srcBuffer:NAME,
                    QUOTER (ttBuffer:BUFFER-FIELD (srcBuffer:NAME + "Id"):BUFFER-VALUE),
                    QUOTER (ttBuffer:BUFFER-FIELD ("LanguageCode"):BUFFER-VALUE)),
                EXCLUSIVE-LOCK) NO-ERROR.            
            CASE ttBuffer:BEFORE-BUFFER:ROW-STATE:                
                WHEN ROW-CREATED OR WHEN ROW-MODIFIED THEN DO:
                    IF NOT translationBuffer:AVAILABLE THEN DO:
                        translationBuffer:BUFFER-CREATE ().
                        translationBuffer:BUFFER-FIELD (TranslationTable + "Id"):BUFFER-VALUE = GUID.
                    END.                                                        
                    translationBuffer:BUFFER-COPY (ttBuffer).
                END.
                WHEN ROW-DELETED THEN DO:
                    IF translationBuffer:AVAILABLE THEN
                        translationBuffer:BUFFER-DELETE ().                     
                END.
            END.
        END.
        
        FINALLY:
            IF VALID-HANDLE (translationBuffer) THEN 
                DELETE OBJECT translationBuffer NO-ERROR.   
        END FINALLY.
         
    END METHOD.        
    
    /* override to deal with translatable entities */
    METHOD PUBLIC OVERRIDE LOGICAL AddDataSource(
        pcTable  AS CHARACTER, pcBuffer AS CHARACTER, pcKeyFields  AS CHARACTER, pcWhere AS CHARACTER,  pcFieldsMap AS CHARACTER, 
        excludeFields AS CHARACTER, includeFields  AS CHARACTER, joinClause AS CHARACTER, joinType  AS LOGICAL):
            
        IF Translatable AND NOT SavingData THEN
            AttachTranslatableDataSource (INPUT-OUTPUT pcTable, INPUT-OUTPUT pcBuffer, INPUT-OUTPUT pcKeyFields, INPUT-OUTPUT pcWhere, pcFieldsMap, excludeFields, includeFields, joinClause, joinType).
                     
        RETURN SUPER:AddDataSource (pcTable, pcBuffer, pcKeyFields, pcWhere, pcFieldsMap, excludeFields, includeFields, joinClause, joinType).
                 
    END METHOD.                 
    
    METHOD PROTECTED VOID AttachTranslatableDataSource (
        INPUT-OUTPUT pcTable  AS CHARACTER, INPUT-OUTPUT pcBuffer AS CHARACTER, INPUT-OUTPUT pcKeyFields  AS CHARACTER, INPUT-OUTPUT pcWhere AS CHARACTER,  pcFieldsMap AS CHARACTER, 
        excludeFields AS CHARACTER, includeFields  AS CHARACTER, joinClause AS CHARACTER, joinType  AS LOGICAL):
        
        DEFINE VARIABLE translatableFields AS CHARACTER NO-UNDO.        
        DEFINE VARIABLE translationTable AS CHARACTER NO-UNDO.
        DEFINE VARIABLE fieldIndex AS INTEGER NO-UNDO.
        DEFINE VARIABLE fieldName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE languageCode AS CHARACTER NO-UNDO. 
        DEFINE VARIABLE filterTranslatableFields AS CHARACTER NO-UNDO.
        
        translatable-block: 
        DO: 
            translatableFields = GetTranslatableFields (pcTable).
            translationTable = GetTranslationTable (pcTable).
            IF Util:IsEmpty (translationTable) OR Util:IsEmpty (translatableFields) THEN
                LEAVE translatable-block.
            languageCode = DataContext:GetFilterValue (pcTable, "LanguageCode", "=").
            
            /* check if there is any filtering on translatable fields */                
            IF DataContext:HasFilters () THEN
                filterTranslatableFields = getFilterTranslationFields (pcTable, translatableFields).
                
            /* if no filtering on translatable fields then first buffer in query is the "normal" table and the next one is the translation table */   
            IF Util:IsEmpty (filterTranslatableFields) THEN DO:              
                IF Util:IsEmpty (pcWhere) THEN
                    pcWhere = SUBSTITUTE ("for each &1", pcBuffer).
                pcWhere = SUBSTITUTE ("&1, each &2 of &3", pcWhere, translationTable, pcBuffer).
                IF NOT Util:IsEmpty (LanguageCode) THEN 
                    pcWhere = SUBSTITUTE ("&1 where &2.LanguageCode = &3", pcWhere, translationTable, QUOTER (languageCode)).
                ELSE
                    pcWhere = pcWhere + " outer-join ".                                       
                pcBuffer = pcBuffer + "," + translationTable.                 
            END. 
            ELSE DO:                                
                pcWhere = SUBSTITUTE ("for each &1", translationTable). 
                DO fieldIndex = 1 TO NUM-ENTRIES (filterTranslatableFields):
                    fieldName = ENTRY (fieldIndex, filterTranslatableFields).
                    pcWhere = pcWhere + (IF fieldIndex = 1 THEN " where " ELSE " and ") + SUBSTITUTE ("&1.&2 = &3", translationTable, fieldName, DataContext:GetFilterValue (pcTable, fieldName, "=")). 
                END.
                IF NOT Util:IsEmpty (LanguageCode) THEN  
                    pcWhere = SUBSTITUTE ("&1 and &2.LanguageCode = &3", pcWhere, translationTable, QUOTER (languageCode)). 
                pcWhere = pcWhere + SUBSTITUTE (", each &1 of &2", pcBuffer, translationTable).
                pcBuffer = translationTable + ",".                  
            END.                
        END.        
        
    END METHOD.        
    
    /* returns a comma separated list of translation fields that are used in filtering in current call */
    METHOD PRIVATE CHARACTER getFilterTranslationFields (tableName AS CHARACTER, translatableFields AS CHARACTER):
        
        DEFINE VARIABLE fieldList AS CHARACTER NO-UNDO.
        DEFINE VARIABLE fieldIndex AS INTEGER NO-UNDO.
        DEFINE VARIABLE fieldName AS CHARACTER NO-UNDO.
        
        DO fieldIndex = 1 TO NUM-ENTRIES (translatableFields):
            fieldName = ENTRY (fieldIndex, translatableFields).
            IF DataContext:AnyFilter (tableName, fieldName) THEN  
                fieldList = fieldList + (IF FieldList = "" THEN "" ELSE ",") + fieldName. 
        END.
        
        RETURN fieldList.
        
    END METHOD.  
    
    /* returns the translation table coresponding to a certain table in the data source */
    METHOD PROTECTED VOID SetTranslationTable (tableName AS CHARACTER, translationTable AS CHARACTER, translatableFields AS CHARACTER):
        IF Util:IsEmpty (tableName) OR Util:IsEmpty (translationTable) THEN 
            RETURN.
        translationTables = translationTables + (IF translationTables = "" THEN "" ELSE "|") + tableName + "|" + translationTable + "|" + translatableFields. 
    END METHOD.        

    /* returns the translation table coresponding to a certain table in the data source */
    METHOD PROTECTED CHARACTER GetTranslationTable (tableName AS CHARACTER):
        DEFINE VARIABLE tableIndex AS INTEGER NO-UNDO.
        IF Util:IsEmpty (translationTables) OR Util:IsEmpty (tableName) THEN 
            RETURN "".
        tableIndex = LOOKUP (tableName, translationTables, "|").
        IF tableIndex > 0 THEN 
            RETURN ENTRY (tableIndex + 1, translationTables, "|").
        RETURN "".
    END METHOD.

    /* returns the translation fields coresponding to a certain table in the data source */
    METHOD PROTECTED CHARACTER GetTranslatableFields (tableName AS CHARACTER):
        DEFINE VARIABLE tableIndex AS INTEGER NO-UNDO.
        IF Util:IsEmpty (translationTables) OR Util:IsEmpty (tableName) THEN 
            RETURN "".
        tableIndex = LOOKUP (tableName, translationTables, "|").
        IF tableIndex > 0 THEN 
            RETURN ENTRY (tableIndex + 2, translationTables, "|").
        RETURN "".
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
    
    /* return true if at least one record is found to match the criteira in the requestContext */ 
    METHOD PUBLIC LOGICAL CanFind (requestContext AS RequestContext):
    END METHOD.   
    
    /* override to set the request no-fill option */ 
    METHOD PUBLIC OVERRIDE LOGICAL AttachDataSource ():

        DEFINE VARIABLE bufferIdx    AS INTEGER NO-UNDO.
        DEFINE VARIABLE bufferHandle AS HANDLE  NO-UNDO.
        DEFINE VARIABLE noFill       AS LOGICAL NO-UNDO.
                
        IF NOT VALID-HANDLE (datasetHandle) THEN
            RETURN FALSE.

        IF VALID-OBJECT (dataContext) AND TYPE-OF (dataContext, com.mrg.framework.system.RequestContext) THEN
            DO bufferIdx = 1 TO datasetHandle:NUM-BUFFERS:
                bufferHandle = datasetHandle:GET-BUFFER-HANDLE (bufferIdx).
                noFill = CAST (dataContext, com.mrg.framework.system.RequestContext):GetNoFill (bufferHandle:NAME).   
                IF noFill = TRUE THEN
                    bufferHandle:FILL-MODE = "NO-FILL".
                ELSE   
                    bufferHandle:FILL-MODE = "MERGE".
            END.        

        RETURN TRUE.
            
    END METHOD.
    
    /* return true if there is at least one record found for the queryString */     
    METHOD PUBLIC LOGICAL CanFind (DATASET-HANDLE dsHandle, queryString AS CHARACTER):
        DEFINE VARIABLE bufferHandle AS HANDLE NO-UNDO.
        DEFINE VARIABLE queryHandle AS HANDLE NO-UNDO.
        
        datasetHandle = dsHandle.
        
        IF NOT AttachDataSource () THEN
            RETURN FALSE.
            
        /* calculate the query string */    
        IF NOT TRIM (queryString) BEGINS "where " THEN
            queryString =  "where " + queryString.
        bufferHandle = datasetHandle:GET-TOP-BUFFER (1).            
        queryString = bufferHandle:DATA-SOURCE:QUERY:PREPARE-STRING + " " + REPLACE (queryString, bufferHandle:NAME + ".", "").
        
        /* set the new query for the data source */
        queryHandle = bufferHandle:DATA-SOURCE:QUERY.
        IF queryHandle:IS-OPEN THEN
            queryHandle:QUERY-CLOSE ().
        IF NOT queryHandle:QUERY-PREPARE (queryString) THEN
            RETURN FALSE.
        bufferHandle:BATCH-SIZE = 1.
        IF datasetHandle:NUM-BUFFERS > 1 THEN DO:
            com.mrg.framework.util.Dataset:SetNoFill (datasetHandle).
            bufferHandle:FILL-MODE = "no-fill".
        END.             
        bufferHandle:FILL ().
        RETURN bufferHandle:TABLE-HANDLE:HAS-RECORDS.
        
        FINALLY:
            DetachDataSource ().
        END FINALLY.            
        
    END METHOD. 
          
    /* gets the db fields associated with the tempTableFieldName field name */
    METHOD PROTECTED CHARACTER MappedDBField (tempTableFieldName AS CHARACTER):
        
        DEFINE VARIABLE tableName AS CHARACTER NO-UNDO.     
        DEFINE VARIABLE fieldMap  AS CHARACTER NO-UNDO.
           
        IF Util:IsEmpty (TempTableFieldName) THEN
            RETURN ?.
                    
        IF NUM-ENTRIES (tempTableFieldName, '.':U) = 2 THEN
            tableName = ENTRY (1, tempTableFieldName, '.':U).
        ELSE 
            ASSIGN 
                tableName          = com.mrg.framework.util.Dataset:GetFieldBufferName (datasetHandle, tempTableFieldName)
                tempTableFieldName = SUBSTITUTE ("&1.&2", tableName, tempTableFieldName).
                             
        RETURN com.mrg.framework.util.TempTable:MappedDbField(tempTableFieldName, com.mrg.framework.util.Dataset:GetBufferHandle (datasetHandle, tableName):DATA-SOURCE-COMPLETE-MAP).  
                                     
    END METHOD.            
           
END CLASS.