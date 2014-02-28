
/*------------------------------------------------------------------------
    File        : ttError.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Andrei 
    Created     : Wed May 15 18:03:50 EEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

define {&scope} {&static} temp-table ttError no-undo {&reference-only}
    field ErrorNum      as integer
    field ErrorCode     as character /* the code / text of the error */
    field ErrorText     as character /* the translated value for the error */
    field ErrorBuffer   as character /* the name of the buffer which generated the error */
    field ErrorKey      as character /* chr(1) separated string with PK values */
    field ErrorField    as character /* the name of the field which generated the error */    
    /* fields to support message parametrization */
    field ErrorParam1   as character
    field ErrorParam2   as character
    field ErrorParam3   as character
    field ErrorParam4   as character
    field ErrorParam5   as character    
    index pk is primary ErrorNum.



  