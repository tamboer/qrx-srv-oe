 
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
USING com.mrg.framework.util.OESession.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.logging.FileLogger INHERITS com.quarix.base.BaseObject IMPLEMENTS com.mrg.framework.service.logging.iLoggingAdapter, com.quarix.base.iSingleton: 
    
    DEFINE PRIVATE STREAM sOutput.
    
    /* Log a message to an os-file. The configuration parameter is used to know the location of the file. */
    METHOD PUBLIC VOID WriteLog (INPUT logFileName AS CHARACTER, INPUT logCategory AS CHARACTER, INPUT logCode AS CHARACTER, INPUT logText AS CHARACTER, INPUT plcDetail AS LONGCHAR):

        IF Util:IsEmpty (logFileName) THEN
            RETURN.
            
        OUTPUT STREAM sOutput TO VALUE (logFileName) APPEND.
        PUT STREAM sOutput UNFORMATTED
            "[":U + string (DtUtilManager:sysNow()) + "] ":U
            OESession:GetPID () " "
            Authentication:UserCode " "
            logCategory " "
            logCode " "
            logText " " SKIP.
        OUTPUT STREAM sOutput CLOSE.
        IF plcDetail <> ? THEN
            COPY-LOB FROM OBJECT plcDetail TO FILE logFileName APPEND.

    END METHOD.
    
    METHOD PUBLIC VOID Reset ():
    END METHOD.
    
END CLASS.