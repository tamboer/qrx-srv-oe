 
/*------------------------------------------------------------------------
   File        : Util
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Thu May 09 18:31:37 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.util.Util FINAL: 
    
    METHOD PUBLIC STATIC LOGICAL IsEmpty (INPUT pcValue AS CHARACTER):
        RETURN pcValue = ? OR pcValue = '':U.
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL IsEmpty (INPUT pcValue AS LONGCHAR):
        RETURN pcValue = ? OR pcValue = '':U.
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL IsEmpty (INPUT piValue AS INTEGER):
        RETURN piValue = ? OR piValue = 0.
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL IsEmpty (INPUT pdValue AS DATE):
        RETURN pdValue = ?.
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL IsEmpty (INPUT pdValue AS DATETIME):
        RETURN pdValue = ?.
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL IsEmpty (INPUT pdValue AS DATETIME-TZ):
        RETURN pdValue = ?.
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL IsEmpty (INPUT pdeValue AS DECIMAL):
        RETURN pdeValue = ? OR pdeValue = 0.
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL IsEmpty (INPUT plValue AS LOGICAL):
        RETURN plValue = ?.
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL IsEmpty (INPUT phValue AS HANDLE):
        RETURN phValue = ? OR NOT valid-handle(phValue).
    END METHOD.

END CLASS.