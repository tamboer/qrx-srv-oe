
 /*------------------------------------------------------------------------
    File        : operator
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Adam
    Created     : Thu Jun 30 14:58:01 EEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

routine-level on error undo, throw.

using Progress.Lang.*.

class com.quarix.data.operator final:

    define private static variable cEqual               as character initial 'eq'.
    define private static variable cNotEqual            as character initial 'ne'.
    define private static variable cLessThan            as character initial 'lt'.
    define private static variable cLessThanOrEqual     as character initial 'le'.
    define private static variable cGreaterThan         as character initial 'gt'.
    define private static variable cGreaterThanOrEqual  as character initial 'ge'.
    define private static variable cMatches             as character initial 'matches'.
    define private static variable cBegins              as character initial 'begins'.

    constructor private operator():
    end constructor.

    method public static character Eq():
        return cEqual.
    end method.

    method public static character Ne():
        return cNotEqual.
    end method.

    method public static character Lt():
        return cLessThan.
    end method.

    method public static character Le():
        return cLessThanOrEqual.
    end method.

    method public static character Gt():
        return cGreaterThan.
    end method.

    method public static character Ge():
        return cGreaterThanOrEqual.
    end method.

    method public static character Matches():
        return cMatches.
    end method.

    method public static character Begins():
        return cBegins.
    end method.

end class.
