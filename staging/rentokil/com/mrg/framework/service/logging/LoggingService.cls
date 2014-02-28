 
 /*------------------------------------------------------------------------
    File        : LoggingService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Fri Apr 26 12:44:43 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.quarix.base.Factory.
USING com.mrg.framework.service.*.
USING com.mrg.framework.service.logging.LoggingService.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.logging.LoggingService INHERITS com.quarix.base.BaseObject IMPLEMENTS com.quarix.base.iSingleton, com.quarix.base.iDisposable:
    
    {com/mrg/framework/service/logging/ttLogAdapter.i &scope=protected}

    DEFINE PRIVATE STATIC VARIABLE loggingService AS LoggingService NO-UNDO.
    DEFINE PRIVATE VARIABLE adaptersInited AS LOGICAL NO-UNDO.
         
    CONSTRUCTOR PRIVATE LoggingService ():
    END CONSTRUCTOR.        
    
    METHOD PUBLIC STATIC LoggingService GetInstance():
        IF NOT VALID-OBJECT(loggingService) THEN
            loggingService = NEW LoggingService ().
        RETURN loggingService.
    END METHOD.
    
    /* required by the iSingleton interface */
    METHOD PUBLIC VOID Reset ():
        EMPTY TEMP-TABLE ttLogCategoryAdapter.
        adaptersInited = NO.
    END METHOD.      

    METHOD PROTECTED VOID GetAdapters ():
        IF NOT adaptersInited THEN DO:
            FOR EACH LogCategoryAdapter NO-LOCK:
                CREATE ttLogCategoryAdapter.
                BUFFER-COPY LogCategoryAdapter TO ttLogCategoryAdapter.
            END.            
            adaptersInited = TRUE.
        END.            
    END METHOD.        
        
    METHOD PUBLIC VOID LogMessage (logCategory AS CHARACTER, logCode AS CHARACTER, logMessage AS CHARACTER):
        GetAdapters ().
        LogMessage (logCategory, logCode, logMessage, ?).
    END METHOD.
    
    METHOD PUBLIC VOID LogMessage (logCategory AS CHARACTER, logCode AS CHARACTER, logMessage AS CHARACTER, logDetail AS LONGCHAR):
        DEFINE VARIABLE loggingAdapter AS com.mrg.framework.service.logging.iLoggingAdapter.
        DEFINE VARIABLE obj AS Progress.Lang.Object NO-UNDO.
        
        GetAdapters ().
        
        FOR EACH ttLogCategoryAdapter
                WHERE ttLogCategoryAdapter.LogCategoryCode = LogCategory:
            obj = DYNAMIC-NEW (ttLogCategoryAdapter.AdapterClass)().
            loggingAdapter = CAST (obj, com.mrg.framework.service.logging.iLoggingAdapter).
            loggingAdapter:WriteLog(ttLogCategoryAdapter.Configuration, logCategory, logCode, logMessage, logDetail).
            DELETE OBJECT loggingAdapter NO-ERROR.                  
        END.                            
    END METHOD.
           
END CLASS.