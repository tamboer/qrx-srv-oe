 
 /*------------------------------------------------------------------------
    File        : Logging
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Fri Apr 26 12:44:21 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.service.logging.LoggingService.
ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.Logging: 
    
    /* private property used to make sure the l is running */
    DEFINE STATIC VARIABLE logMgr_ AS LoggingService NO-UNDO.
    DEFINE STATIC PROPERTY logMgr AS LoggingService
        GET:
            IF NOT VALID-OBJECT (logMgr_) THEN
                logMgr_ = LoggingService:GetInstance ().
            RETURN logMgr_.
        END GET.
        SET. 

    METHOD PUBLIC STATIC VOID LogMessage (logCategory AS CHARACTER, logCode AS CHARACTER, logMessage AS CHARACTER):
        logMgr:LogMessage (logCategory, logCode, logMessage).
    END METHOD.
    
    METHOD PUBLIC STATIC VOID LogMessage (logCategory AS CHARACTER, logCode AS CHARACTER, logMessage AS CHARACTER, logDetail AS LONGCHAR):
        logMgr:LogMessage (logCategory, logCode, logMessage, logDetail).
    END METHOD.                
        
END CLASS.