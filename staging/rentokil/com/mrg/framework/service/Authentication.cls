 
 /*------------------------------------------------------------------------
    File        : Authentication
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Fri Apr 26 10:58:51 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.service.authentication.AuthenticationService.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.Authentication: 
    
    /* private property used to make sure the AuthenticationManager is running */
    DEFINE PRIVATE STATIC VARIABLE authMgr_ AS AuthenticationService NO-UNDO.
    DEFINE PRIVATE STATIC PROPERTY authMgr AS AuthenticationService
        GET:
            IF NOT VALID-OBJECT (authMgr_) THEN
                authMgr_ = CAST (AuthenticationService:GetInstance (), AuthenticationService).
            RETURN authMgr_.
        END GET.
        SET.        
    
    DEFINE PUBLIC STATIC PROPERTY UserCode AS CHARACTER
        GET:
            RETURN authMgr:UserCode.
        END GET.
        PRIVATE SET.
                
    DEFINE PUBLIC PROPERTY QualifiedUserID AS CHARACTER
        GET:
            RETURN authMgr:QualifiedUserID.
        END GET.
        PRIVATE SET.
        
    DEFINE PUBLIC PROPERTY UserName AS CHARACTER
        GET:
            RETURN authMgr:UserName.
        END GET.
        PRIVATE SET.

    DEFINE PUBLIC PROPERTY UserRoles AS CHARACTER
        GET:
            RETURN authMgr:UserRoles.
        END GET.
        PRIVATE SET.    
    
    /* authenticates and externally received client principal */
    METHOD PUBLIC STATIC LOGICAL Login (clientPrincipal AS HANDLE):
        RETURN authMgr:Login (clientPrincipal).
    END METHOD.
    
END CLASS.