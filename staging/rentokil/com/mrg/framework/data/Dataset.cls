 
 /*------------------------------------------------------------------------
    File        : Dataset
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Andrei
    Created     : Thu May 23 17:34:35 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.data.Dataset INHERITS com.quarix.base.BaseObject:
    
    DEFINE PUBLIC PROPERTY Handle AS HANDLE
        GET.
        PROTECTED SET. 
        
END CLASS.