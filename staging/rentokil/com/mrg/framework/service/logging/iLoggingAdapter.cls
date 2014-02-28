 
 /*------------------------------------------------------------------------
    File        : iLoggingAdapter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Mon May 13 09:21:23 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

INTERFACE com.mrg.framework.service.logging.iLoggingAdapter: 

    /* Log a message in the logging service. The action that will be performed depends on the category that is supplied. */
    METHOD PUBLIC VOID WriteLog (INPUT pcConfiguration AS CHARACTER, INPUT pcCategory AS CHARACTER, INPUT pcCode AS CHARACTER, INPUT pcText AS CHARACTER, INPUT plcDetail AS LONGCHAR).

END INTERFACE.