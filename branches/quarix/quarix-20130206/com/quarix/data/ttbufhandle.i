
/*------------------------------------------------------------------------
    File        : ttbufhandle.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Adam
    Created     : Fri Jul 01 09:58:37 EEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

define {&scope} temp-table {&prefix}ttbufhandle no-undo {&REFERENCE-ONLY}

    field hBufHandle as handle

    index idxPk is primary unique hBufHandle.
