 
/*------------------------------------------------------------------------
   File        : FileLogger
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Mon May 13 09:55:46 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.service.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.logging.DBLogger INHERITS com.quarix.base.BaseObject IMPLEMENTS com.mrg.framework.service.logging.iLoggingAdapter, com.quarix.base.iSingleton: 
    
    /* Log a message to the database */
    METHOD PUBLIC VOID WriteLog (INPUT logConfiguration AS CHARACTER, INPUT logCategory AS CHARACTER, INPUT logCode AS CHARACTER, INPUT logText AS CHARACTER, INPUT plcDetail AS LONGCHAR):
        
        DEFINE BUFFER bufDbLog FOR DbLog.
        
        DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.

        cUser = Authentication:UserCode.
        
        CREATE bufDbLog.
        ASSIGN
            bufDbLog.LogCategoryCode = logCategory
            bufDbLog.LogDateTime     = NOW
            bufDbLog.LogCode         = logCode
            bufDbLog.LogText         = logText
            bufDbLog.LogDetail       = plcDetail
            bufDbLog.LogGuid         = GUID
            bufDbLog.LogUserCode     = cUser
            bufDbLog.LogOrder        = NEXT-VALUE (seqDbLog).
        
    END METHOD.
    
    METHOD PUBLIC VOID Reset ():
    END METHOD.
    
END CLASS.