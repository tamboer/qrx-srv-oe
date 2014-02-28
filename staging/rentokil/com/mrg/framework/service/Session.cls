 
 /*------------------------------------------------------------------------
    File        : Session
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Fri Apr 26 12:11:00 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.quarix.service.session.SessionCore.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.Session: 
    
    /* private property used to make sure the AuthenticationManager is running */
    DEFINE STATIC VARIABLE sessMgr_ AS SessionCore NO-UNDO.
    DEFINE STATIC PROPERTY sessMgr AS SessionCore
        GET:
            IF NOT VALID-OBJECT (sessMgr_) THEN
                sessMgr_ = SessionCore:GetInstance ().
            RETURN sessMgr_.
        END GET.
        SET.        
    
    METHOD PUBLIC STATIC VOID Start (sessionId AS CHARACTER):
        sessMgr:Start (sessionId).
    END METHOD.
    
END CLASS.