 
/*------------------------------------------------------------------------
   File        : TSFramework
   Purpose     : Run all tests 
   Syntax      : 
   Description : 
   Author(s)   : andrei.tarba
   Created     : Thu Oct 24 22:12:05 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING com.mrg.framework.test.*.

CLASS com.mrg.framework.test.TSFramework INHERITS OEUnit.Runner.TestSuite:
    
    CONSTRUCTOR TSFramework():
        AddTest(NEW TCServiceInterface()). 
    END CONSTRUCTOR.

END CLASS.