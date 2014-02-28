 
 /*------------------------------------------------------------------------
    File        : OESession
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Mon May 13 10:07:00 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.util.OESession:
    
    /* returns the PID of the current OE session */
    METHOD PUBLIC STATIC CHARACTER GetPID ():
        DEFINE VARIABLE bufferHandle AS HANDLE NO-UNDO.        
        CREATE BUFFER bufferHandle FOR TABLE LDBNAME (1) + "._myconnection".
        bufferHandle:FIND-FIRST ("", NO-LOCK).
        RETURN (IF NOT bufferHAndle:AVAILABLE THEN "" ELSE bufferHandle:BUFFER-FIELD ("_myconn-pid"):buffer-value ()).
    END METHOD.            

END CLASS.