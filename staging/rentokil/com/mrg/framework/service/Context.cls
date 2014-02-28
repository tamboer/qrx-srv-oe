 
 /*------------------------------------------------------------------------
    File        : Context
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Fri Apr 26 12:27:17 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.quarix.service.context.ContextCore.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.Context: 

    /* private property used to make sure the ContextManager is running */
    DEFINE STATIC VARIABLE contextMgr_ AS ContextCore NO-UNDO.
    DEFINE STATIC PROPERTY contextMgr AS ContextCore
        GET:
            IF NOT VALID-OBJECT (contextMgr_) THEN
                contextMgr_ = ContextCore:GetInstance ().
            RETURN contextMgr_.
        END GET.
        SET.   

    METHOD PUBLIC STATIC LOGICAL SetValue (INPUT keyName AS CHARACTER , INPUT keyValue AS CHARACTER):
        RETURN contextMgr:SetValue (keyName, keyValue).
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL SetValue (INPUT keyName AS CHARACTER , INPUT keyValue AS INTEGER):
        RETURN contextMgr:SetValue (keyName, keyValue).
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL SetValue (INPUT keyName AS CHARACTER , INPUT keyValue AS DECIMAL):
        RETURN contextMgr:SetValue (keyName, keyValue).
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL SetValue (INPUT keyName AS CHARACTER , INPUT keyValue AS DATE):
        RETURN contextMgr:SetValue (keyName, keyValue).
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL SetValue (INPUT keyName AS CHARACTER , INPUT keyValue AS DATETIME):
        RETURN contextMgr:SetValue (keyName, keyValue).
    END METHOD.

    METHOD PUBLIC STATIC LOGICAL SetValue (INPUT keyName AS CHARACTER , INPUT keyValue AS LOGICAL):
        RETURN contextMgr:SetValue (keyName, keyValue).
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL SetValue (INPUT keyName AS CHARACTER , INPUT keyValue AS LONGCHAR):
        RETURN contextMgr:SetValue (keyName, keyValue).
    END METHOD.

    METHOD PUBLIC LOGICAL GetValue (INPUT keyName AS CHARACTER , OUTPUT keyValue AS CHARACTER):
        RETURN contextMgr:GetValue (keyName, OUTPUT keyValue).
    END METHOD.

    METHOD PUBLIC LOGICAL GetValue (INPUT keyName AS CHARACTER , OUTPUT keyValue AS INTEGER):
        RETURN contextMgr:GetValue (keyName, OUTPUT keyValue).
    END METHOD.

    METHOD PUBLIC LOGICAL GetValue (INPUT keyName AS CHARACTER , OUTPUT keyValue AS DECIMAL):
        RETURN contextMgr:GetValue (keyName, OUTPUT keyValue).
    END METHOD.
   
    METHOD PUBLIC LOGICAL GetValue (INPUT keyName AS CHARACTER , OUTPUT keyValue AS DATE):
        RETURN contextMgr:GetValue (keyName, OUTPUT keyValue).
    END METHOD.
   
    METHOD PUBLIC LOGICAL GetValue (INPUT keyName AS CHARACTER , OUTPUT keyValue AS DATETIME):
        RETURN contextMgr:GetValue (keyName, OUTPUT keyValue).
    END METHOD.

    METHOD PUBLIC LOGICAL GetValue (INPUT keyName AS CHARACTER , OUTPUT keyValue AS LOGICAL):
        RETURN contextMgr:GetValue (keyName, OUTPUT keyValue).
    END METHOD.

    METHOD PUBLIC LOGICAL GetValue (INPUT keyName AS CHARACTER , OUTPUT keyValue AS LONGCHAR):
        RETURN contextMgr:GetValue (keyName, OUTPUT keyValue).
    END METHOD.

END CLASS.