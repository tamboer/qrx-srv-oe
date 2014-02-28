 
 /*------------------------------------------------------------------------
    File        : AuthenticationManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Fri Apr 26 11:00:32 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.service.authentication.AuthenticationService.
USING com.quarix.service.authentication.AuthenticationCore.
USING com.mrg.framework.service.Error.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.service.authentication.AuthenticationService INHERITS AuthenticationCore:

    DEFINE PUBLIC PROPERTY UserCode AS CHARACTER
        GET.
        PRIVATE SET.
                
    DEFINE PUBLIC PROPERTY QualifiedUserID AS CHARACTER
        GET.
        PRIVATE SET.
        
    DEFINE PUBLIC PROPERTY UserName AS CHARACTER
        GET.
        PRIVATE SET.

    DEFINE PUBLIC PROPERTY UserRoles AS CHARACTER
        GET.
        PRIVATE SET.

    DEFINE PRIVATE STATIC VARIABLE authMgr AS AuthenticationService NO-UNDO.
     
    CONSTRUCTOR PRIVATE AuthenticationService ():
    END CONSTRUCTOR.
    
    METHOD PUBLIC STATIC OVERRIDE AuthenticationCore GetInstance():
        IF NOT VALID-OBJECT(authMgr) THEN
            authMgr = NEW AuthenticationService ().
        RETURN authMgr.
    END METHOD.
                    
    /* validates an externally authenticated  client principal */ 
    METHOD PUBLIC LOGICAL Login (clientPrincipal AS HANDLE):
        
        DEFINE VARIABLE dbIndex AS INTEGER NO-UNDO.
        
        IF NOT VALID-HANDLE (clientPrincipal) THEN
        DO:
            com.mrg.framework.service.Error:AddError ("Invalid client principal").
            RETURN FALSE.
        END. 
        IF Util:IsEmpty (clientPrincipal:USER-ID) OR NOT ValidApplicationUser (clientPrincipal:USER-ID) THEN
        DO:
            com.mrg.framework.service.Error:AddError ("Invalid user id").
            RETURN FALSE.
        END.   
        IF clientPrincipal:LOGIN-STATE = "EXPIRED":U THEN
        DO:
            com.mrg.framework.service.Error:AddError ("Session has expired").
            RETURN FALSE.
        END.
        ELSE IF clientPrincipal:LOGIN-STATE <> "LOGIN":U AND clientPrincipal:LOGIN-STATE <> "SSO":U THEN 
        DO: 
            com.mrg.framework.service.Error:AddError ("Invalid login token").
            RETURN FALSE.
        END.
        
        /* TO DO: uncomment this when db authentication is necessary
        /* db authetication */
        DO dbIndex = 1 TO NUM-DBS:
            IF NOT SET-DB-CLIENT(clientPrincipal, dbIndex) THEN DO:    
                com.mrg.framework.service.Error:AddError ("Userid/password are incorrect. (DB-AUTHENTICATION)").
                RETURN FALSE.
            END.                
        END.            
        
        /* appserver session authentication */
        IF NOT SECURITY-POLICY:SET-CLIENT (clientPrincipal) THEN DO:
            com.mrg.framework.service.Error:AddError ("Invalid login token").
            RETURN FALSE.
        END.  
        */
        
        ASSIGN
            UserCode        = clientPrincipal:USER-ID
            UserName        = GetApplicationUserName (UserCode)
            QualifiedUserId = clientPrincipal:QUALIFIED-USER-ID 
            UserRoles       = clientPrincipal:ROLES.
            
        RETURN TRUE.
        
        CATCH appError AS Progress.Lang.Error:            
            ThrowError(appError).
            DELETE OBJECT appError.
            RETURN FALSE.
        END CATCH.
                                         
    END METHOD.            
    
    /* validates the authenticated user against the MDM user table */
    METHOD PUBLIC LOGICAL ValidApplicationUser (userCode AS CHARACTER):
        
    END METHOD.        
    
    /* get the MDM user name */
    METHOD PUBLIC CHARACTER GetApplicationUserName (userCode AS CHARACTER):
    END METHOD.        
    
END CLASS.