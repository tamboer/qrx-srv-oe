
/*------------------------------------------------------------------------
    File        : iBaseObject
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Fri Mar 02 14:01:25 EET 2012
    Notes       :
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using com.quarix.system.*.
using com.quarix.service.context.*.
using com.quarix.service.error.*.
using com.quarix.base.*.
using com.quarix.service.localization.*.

interface com.quarix.base.iBaseObject:

    method public Application   GetApplication ().

    method public iContext      GetContextManager ().

    method public iError        GetErrorManager ().

    method public Factory       GetFactory ().

    method public Object        GetInstance (className  as character).

    method public Object        GetInstance (className  as character, logError as logical).

    method public iLocalization GetLocalization ().

    method public Version       GetQuarixVersion ().

    method public character     GetString (keyName as character).

    method public Util          GetUtil ().

    method public void ThrowClientError (errorCode as integer, errorText as character).

    method public void ThrowClientError (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowClientError (errorCode as integer, errorType as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowClientError (errorCode as integer, errorType as integer, errorText as character, errorBuffer as character, errorField as character, errorRecord as integer ).

    method public void ThrowClientError (errorCode as integer, errorType as integer, errorText as character, errorBuffer as character, errorField as character, errorRecord as integer, errorTag as character ).

    method public void ThrowDebug       (errorCode as integer, errorText as character).

    method public void ThrowDebug       (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowError       (errorCode as integer, errorText as character).

    method public void ThrowError       (errorCode as integer, errorText as character, errorCallStack as character).

    method public void ThrowError       (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowError       (appError as Progress.Lang.Error).

    method public void ThrowInfo        (errorCode as integer, errorText as character).

    method public void ThrowInfo        (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowWarning     (errorCode as integer, errorText as character).

    method public void ThrowWarning     (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void UnloadAll        (collection as Collection).

    method public void UnloadInstance   (instanceObj as Object).

end interface.