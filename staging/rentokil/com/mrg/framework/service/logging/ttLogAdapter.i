
/*------------------------------------------------------------------------
    File        : ttLogAdapter.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Andrei
    Created     : Mon May 13 10:35:34 EEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE tt{&prefix}LogCategoryAdapter NO-UNDO {&REFERENCE-ONLY} BEFORE-TABLE bt{&prefix}LogCategoryAdapter
    FIELD LogCategoryCode AS CHARACTER FORMAT "x(16)":U
    FIELD AdapterClass    AS CHARACTER FORMAT "x(64)":U
    FIELD Configuration   AS CHARACTER FORMAT "x(256)":U   
    INDEX PK_ttCategoryAdapter IS PRIMARY IS UNIQUE LogCategoryCode AdapterClass.