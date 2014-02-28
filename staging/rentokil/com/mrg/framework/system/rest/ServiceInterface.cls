 
/*------------------------------------------------------------------------
   File        : ServiceInterface
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Jul 28 10:57:06 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING com.mrg.framework.service.*.
USING com.mrg.framework.system.RequestContext.
USING com.quarix.system.Application.
USING com.quarix.system.ApplicationUser.
USING Progress.Json.ObjectModel.*.

&global-define res-var-user-name       'User':u
&global-define res-sess-authenticated  'session_is_authenticated':u
&global-define res-sess-app-name       'app_name':u
&global-define res-sess-loc-code       'user_locale':u
&global-define res-sess-loc-lang       'locale_lang':u
&global-define res-sess-loc-country    'locale_country':u
&global-define res-sess-loc-date-fmt   'locale_date_format':u
&global-define res-sess-loc-bool-fmt   'locale_logic_format':u
&global-define res-sess-loc-num-fmt    'locale_num_format':u
&global-define res-sess-loc-num-dec    'locale_num_dec':u
&global-define res-sess-loc-num-sep    'locale_num_sep':u

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.system.rest.ServiceInterface INHERITS com.quarix.base.BaseObject: 
    
    DEFINE PRIVATE VARIABLE startTime AS INTEGER NO-UNDO.
    DEFINE PRIVATE VARIABLE startDate AS DATE    NO-UNDO.
    DEFINE PRIVATE VARIABLE authToken AS HANDLE  NO-UNDO.
    
    /* converts the json string containing service parameter and put;s them in a requestContext object */
    /* TO DO: modify the structure of the JSON to have the temp-table name at first level!!! */
    METHOD PUBLIC com.mrg.framework.system.RequestContext ConvertRequest (jsonString AS CHARACTER):

        DEFINE VARIABLE requestContext     AS com.mrg.framework.system.RequestContext NO-UNDO.
        DEFINE VARIABLE jsonParser         AS ObjectModelParser                      NO-UNDO.
        DEFINE VARIABLE jsonParams         AS JsonObject                             NO-UNDO.
        DEFINE VARIABLE jsonFilterArray    AS JsonArray                              NO-UNDO.      
        DEFINE VARIABLE jsonFilter         AS JsonObject                             NO-UNDO.
        DEFINE VARIABLE jsonNoFillArray    AS JsonArray                              NO-UNDO.      
        DEFINE VARIABLE jsonNoFill         AS JsonObject                             NO-UNDO.        
        DEFINE VARIABLE jsonSortArray      AS JsonArray                              NO-UNDO.      
        DEFINE VARIABLE jsonSort           AS JsonObject                             NO-UNDO.
        DEFINE VARIABLE jsonBatchSizeArray AS JsonArray                              NO-UNDO.      
        DEFINE VARIABLE jsonBatchSize      AS JsonObject                             NO-UNDO.
        DEFINE VARIABLE jsonStartRowArray  AS JsonArray                              NO-UNDO.      
        DEFINE VARIABLE jsonStartRow       AS JsonObject                             NO-UNDO.
        DEFINE VARIABLE jsonPropertyArray  AS JsonArray                              NO-UNDO.      
        DEFINE VARIABLE jsonProperty       AS JsonObject                             NO-UNDO.                       
        DEFINE VARIABLE numArray           AS INTEGER                                NO-UNDO.
        DEFINE VARIABLE tableName          AS CHARACTER                              NO-UNDO.
               
        requestContext = CAST (GetInstance ("com.mrg.framework.system.RequestContext":U), com.mrg.framework.system.RequestContext). 
        
        ASSIGN 
            jsonParser = NEW ObjectModelParser()
            jsonParams = CAST(jsonParser:Parse(jsonString), JsonObject).            

                 
        /* filters */  
        DO ON ERROR UNDO, THROW:                  
            jsonFilterArray = jsonParams:GetJsonArray("Filter").
            DO numArray = 1 TO jsonFilterArray:Length:
                jsonFilter = jsonFilterArray:GetJsonObject(numArray).
                tableName = (IF jsonFilter:Has("Table") THEN jsonFilter:GetCharacter ("Table") ELSE ?).
                IF NOT Util:IsEmpty (tableName) THEN 
                    requestContext:SetFilter (
                        tableName,   
                        jsonFilter:GetCharacter ("Field"),
                        jsonFilter:GetCharacter ("Operator"),
                        jsonFilter:GetCharacter ("Value")).
                ELSE
                    requestContext:SetFilter (
                        jsonFilter:GetCharacter ("Field"),
                        jsonFilter:GetCharacter ("Operator"),
                        jsonFilter:GetCharacter ("Value")).                                
                
            END.
            /* do not do anything in case there is no filter */
            CATCH jsonError AS Progress.Lang.Error :
                DELETE OBJECT jsonError.
            END CATCH.   
        END.

        /* sorting */ 
        DO ON ERROR UNDO, THROW:                   
            jsonSortArray = jsonParams:GetJsonArray("Sort").
            DO numArray = 1 TO jsonSortArray:Length:
                jsonSort = jsonSortArray:GetJsonObject(numArray).
                tableName = (IF jsonSort:Has("Table") THEN jsonSort:GetCharacter ("Table") ELSE ?).
                IF NOT Util:IsEmpty (tableName) THEN            
                    requestContext:SetSort (
                        tableName,   
                        jsonSort:GetCharacter ("Field"),
                        (IF jsonSort:Has("Ascending") THEN NOT jsonSort:GetLogical ("Ascending") ELSE TRUE)).
                ELSE
                    requestContext:SetSort (   
                        jsonSort:GetCharacter ("Field"),
                        (IF jsonSort:Has("Ascending") THEN NOT jsonSort:GetLogical ("Ascending") ELSE TRUE)).            
            END.
            /* do not do anything in case there is no sort */
            CATCH jsonError AS Progress.Lang.Error :
                DELETE OBJECT jsonError.
            END CATCH.                 
        END.
        
        /* batch size (s) */                    
        DO ON ERROR UNDO, THROW:
            jsonBatchSizeArray = jsonParams:GetJsonArray("BatchSize").
            DO numArray = 1 TO jsonBatchSizeArray:Length:
                jsonBatchSize = jsonBatchSizeArray:GetJsonObject(numArray).
                requestContext:SetBatchSize (
                    jsonBatchSize:GetCharacter ("Table"),   
                    jsonBatchSize:GetInteger ("BatchSize")).            
            END.
            /* do not do anything in case there is no batch size */
            CATCH jsonError AS Progress.Lang.Error :                
                DELETE OBJECT jsonError.
            END CATCH.                 
        END.

        /* no-fill */  
        DO ON ERROR UNDO, THROW:                  
            jsonNoFillArray = jsonParams:GetJsonArray("NoFill").
            DO numArray = 1 TO jsonNoFillArray:Length:
                jsonNoFill = jsonNoFillArray:GetJsonObject(numArray).
                tableName = (IF jsonNoFill:Has("Table") THEN jsonNoFill:GetCharacter ("Table") ELSE ?).
                IF NOT Util:IsEmpty (tableName) THEN DO:
                    IF jsonNoFill:Has("NoFill") THEN
                        requestContext:SetNoFill (
                            tableName,   
                            jsonNoFill:GetLogical ("NoFill")).
                    ELSE
                        requestContext:SetNoFill (tableName).                                               
                END.                                        
            END.
            /* do not do anything in case there is no filter */
            CATCH jsonError AS Progress.Lang.Error :
                DELETE OBJECT jsonError.
            END CATCH.   
        END.
                
        /* start row(s) */
        DO ON ERROR UNDO, THROW:                    
            jsonStartRowArray = jsonParams:GetJsonArray("StartRow").
            DO numArray = 1 TO jsonStartRowArray:Length:
                jsonStartRow = jsonStartRowArray:GetJsonObject(numArray).
                requestContext:SetStartRow (
                    jsonStartRow:GetCharacter ("Table"),   
                    jsonStartRow:GetInteger ("Row")).            
            END.   
            /* do not do anything in case there is no start row */
            CATCH jsonError AS Progress.Lang.Error :
                DELETE OBJECT jsonError.
            END CATCH.                 
        END.

        /* start row(s) */
        DO ON ERROR UNDO, THROW:                    
            jsonPropertyArray = jsonParams:GetJsonArray("Property").
            DO numArray = 1 TO jsonPropertyArray:Length:
                jsonProperty = jsonPropertyArray:GetJsonObject(numArray).
                requestContext:SetProperty (
                    jsonProperty:GetCharacter ("Name"),   
                    jsonProperty:GetCharacter ("Value")).            
            END.   
            /* do not do anything in case there is no start row */
            CATCH jsonError AS Progress.Lang.Error :
                DELETE OBJECT jsonError.
            END CATCH.                 
        END.
                                        
        RETURN requestContext.
        
        FINALLY:
            DELETE OBJECT jsonParser NO-ERROR.
            DELETE OBJECT jsonParams NO-ERROR.
            DELETE OBJECT jsonFilterArray NO-ERROR.
            DELETE OBJECT jsonFilter NO-ERROR.
            DELETE OBJECT jsonNoFillArray NO-ERROR.
            DELETE OBJECT jsonNoFill NO-ERROR.            
            DELETE OBJECT jsonSortArray NO-ERROR.
            DELETE OBJECT jsonSort NO-ERROR.
            DELETE OBJECT jsonBatchSizeArray NO-ERROR.
            DELETE OBJECT jsonBatchSize NO-ERROR.
            DELETE OBJECT jsonStartRowArray NO-ERROR.
            DELETE OBJECT jsonStartRow NO-ERROR.
            DELETE OBJECT jsonPropertyArray NO-ERROR.
            DELETE OBJECT jsonProperty NO-ERROR.                                                         
        END FINALLY.
        
    END METHOD.
    
    /* runs before the service is running and is doing authentication, session management and context management */    
    METHOD PROTECTED LOGICAL ActivateService (serviceName AS CHARACTER, requestContext AS RequestContext):

        DEFINE VARIABLE application     AS Application     NO-UNDO.
        DEFINE VARIABLE appUser         AS ApplicationUser NO-UNDO.
        DEFINE VARIABLE clientPrincipal AS HANDLE          NO-UNDO.
        DEFINE VARIABLE retValue        AS CHARACTER       NO-UNDO.
        
        /* store the start time for performance logging */
        ASSIGN
            startTime = MTIME
            startDate = TODAY. 
        
        /* get external authentication token */
        clientPrincipal = getRequestAuthenticationToken ().
        
        /* start/setup the application/session */
        application = com.quarix.system.Application:GetInstance (). 
        application:Name = "MDM".        
        application:StartSession(clientPrincipal:SESSION-ID, 0). /* TO DO: check if we do sesion timeout on the appserver */
        
        /* if current user session is not authenticated then authenticate the client-principal */
        IF VALID-OBJECT (ContextManager) AND NOT ContextManager:GetValue({&res-sess-authenticated}, OUTPUT retValue) THEN
        DO:
            IF NOT Authentication:Login (clientPrincipal) THEN
                RETURN FALSE.
            ContextManager:SetValue({&res-sess-authenticated}, 'true':u).                 
        END.                
             
        /* load user settings and save them into the context */        
        appUser = getApplicationUser (clientPrincipal:USER-ID, application).
         
        /* set locale settings from the user; if empty get it from the application level */
        setUserLocalization (appUser, clientPrincipal:GET-PROPERTY ("Locale")).
        
        /* set user, application and localization data in context */
        setContext (appUser, application).

        RETURN TRUE.
                
    END METHOD.
    
    /* runs after service has been finished */
    METHOD PROTECTED VOID DeactivateService (serviceName AS CHARACTER, requestContext AS RequestContext):
        logPerformance (serviceName, IF VALID-OBJECT(requestContext) THEN requestContext:Request2String () ELSE ""). 
        DELETE OBJECT requestContext NO-ERROR.
    END METHOD.                        
    
    /* this is to be called from the application service interface class */
    METHOD PRIVATE INTEGER logPerformance (serviceName AS CHARACTER, serviceDefinition AS CHARACTER):
        DEFINE VARIABLE serviceDuration AS INTEGER NO-UNDO.             
        serviceDuration = getServiceDuration (). 
        IF serviceDuration > getMaxServiceDuration () THEN
            Logging:LogMessage("PERFORMANCE":U, serviceName, SUBSTITUTE ("&1 miliseconds", serviceDuration), (IF Util:IsEmpty (serviceDefinition) THEN "" ELSE SUBSTITUTE ("~n&1", serviceDefinition))).           
    END METHOD.
    
    METHOD PRIVATE VOID setUserLocalization (appUser AS ApplicationUser, requestLocale AS CHARACTER):
        /* override the locale with the request locale if any */
        IF NOT Util:IsEmpty (requestLocale) THEN
            Localization:SetLocale (requestLocale).                                                 
        ELSE IF NOT Util:IsEmpty (appUser:Locale) THEN
            Localization:SetLocale (appUser:Locale).         
        IF NOT Util:IsEmpty (appUser:DateFormat) THEN
            Localization:SetDateFormat (appUser:DateFormat).
        IF NOT Util:IsEmpty (appUser:LogicalFormat) THEN
            Localization:SetLogicalFormat (appUser:LogicalFormat).
        IF NOT Util:IsEmpty (appUser:NumericFormat) THEN
            Localization:SetNumericFormat (appUser:NumericFormat).
        application:Locale = Localization:GetLocale().
        application:DateFormat = Localization:GetDateFormat().
        application:LogicalFormat = Localization:GetLogicalFormat().              
        application:NumericFormat = Localization:GetNumericFormat().
    END METHOD.                
    
    METHOD PRIVATE ApplicationUser getApplicationUser (userCode AS CHARACTER, application AS Application):
        DEFINE VARIABLE appUser AS ApplicationUser NO-UNDO.
        appUser = NEW ApplicationUser ().
        appUser:Login = userCode.            
        appUser:LoadConfiguration(application:configuration).
        application:CurrentUser = appUser.
        RETURN appUser.
    END METHOD.        
    
    METHOD PRIVATE VOID setContext (appUser AS ApplicationUser, application AS Application):
        ContextManager:SetValue({&res-var-user-name}, appUser:Login).               
        ContextManager:SetValue({&res-sess-app-name}, application:Name).        
        ContextManager:SetValue({&res-sess-loc-code}, Localization:GetLocale()).
        ContextManager:SetValue({&res-sess-loc-lang}, Localization:GetLanguage()).
        ContextManager:SetValue({&res-sess-loc-country}, Localization:GetCountry()).
        ContextManager:SetValue({&res-sess-loc-date-fmt}, Localization:GetDateFormat()).
        ContextManager:SetValue({&res-sess-loc-bool-fmt}, Localization:GetLogicalFormat()).
        ContextManager:SetValue({&res-sess-loc-num-fmt}, Localization:GetNumericFormat()).        
    END METHOD.
        
    /* returns the externally authenticated/sealed client principal */                
    METHOD PUBLIC HANDLE setRequestAuthenticationToken (externalAuthToken AS HANDLE):
        authToken = externalAuthToken.
    END METHOD. 

    /* returns the externally authenticated/sealed client principal */                
    METHOD PUBLIC HANDLE getRequestAuthenticationToken ():
        IF NOT VALID-HANDLE (authToken) THEN
            authToken = SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal(). 
        RETURN  authToken.
    END METHOD. 

    
    /* returns the duration of the service in miliseconds */
    METHOD PRIVATE INTEGER getMaxServiceDuration ():
        RETURN  10. /* TO DO: get this from a Configuration service */       
    END METHOD.

    /* returns the duration of the service in miliseconds */
    METHOD PRIVATE INTEGER getServiceDuration ():
        IF startDate = TODAY THEN
            RETURN MTIME - startTime.
        RETURN  86400000 - startTime + MTIME.       
    END METHOD.

END CLASS.