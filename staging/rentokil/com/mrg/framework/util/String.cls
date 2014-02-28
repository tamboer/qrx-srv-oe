 
/*------------------------------------------------------------------------
   File        : String
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : andrei.tarba
   Created     : Mon Oct 07 11:29:42 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.util.String: 
    
    /* it receives a tokeSeparator delimited string and it returns the string in pascal case */
    METHOD PUBLIC STATIC CHARACTER String2PascalCase (aString AS CHARACTER, tokenSeparator AS CHARACTER):
        DEFINE VARIABLE tokenNum AS INT NO-UNDO.
        DEFINE VARIABLE resultString AS CHAR NO-UNDO.                
        DO tokenNum = 1 TO NUM-ENTRIES (aString, tokenSeparator):
            resultString = resultString + StartWithCapital (ENTRY (tokenNum, aString, tokenSeparator)). 
        END.                
        RETURN resultString.        
    END METHOD.            

    METHOD PUBLIC STATIC LOGICAL IsFirstLetterCapital (INPUT aString AS CHARACTER):
        DEFINE VARIABLE firstLetter AS CHARACTER.
        firstLetter = SUBSTRING (aString, 1, 1). 
        RETURN ASC (firstLetter) = ASC (CAPS (firstLetter)).
    END METHOD.         
        
    /* it makes the aString to start with a capital, the rest of the string is unchanged */    
    METHOD PUBLIC STATIC CHARACTER StartWithCapital (INPUT aString AS CHARACTER):
        RETURN CAPS (SUBSTRING (aString, 1, 1)) + LC (SUBSTRING (aString, 2)).
    END METHOD.
    
    /* 
     * Return the value of a parameter from a list of parameter-value.
     * The format of the list is: ParamName1=value1[|paramName2=value2[|...]]
     * 
     * <param name="parameterString"> The parameter-value list</param>
     * <param name="parameterName">Parameter vane of which value is requested</param>
     * <returns> value of the token
     */
    METHOD PUBLIC STATIC CHARACTER GetTokenValueFromString(parameterString AS CHARACTER, parameterName AS CHARACTER):
      
        RETURN GetTokenValueFromString(parameterString, parameterName, '|').
        
    END METHOD.
     
    /* 
     * Return the value of a parameter from a list of parameter-value.
     * The format of the list is: ParamName1=value1[SEPparamName2=value2[SEL...]]
     * where SEP is used to separate the tokep=value list
     *
     * <param name="parameterString"> The parameter-value list</param>
     * <param name="parameterName">Parameter vane of which value is requested</param>
     * <param name="separator">List separator</param>
     * <returns> value of the token
     */
    METHOD PUBLIC STATIC CHARACTER GetTokenValueFromString(parameterString AS CHARACTER, parameterName AS CHARACTER, separator AS CHARACTER):
        
        DEFINE VARIABLE iIndex      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE parVlaue    AS CHARACTER NO-UNDO.

        IF parameterString = ? OR parameterString = "" THEN 
            RETURN "".

        DO iIndex= 1 TO NUM-ENTRIES(parameterString, separator):
            parVlaue = ENTRY(iIndex, parameterString, separator).
            IF parVlaue = "" THEN NEXT.
            
            IF ENTRY(1, parVlaue, "=") = parameterName THEN
                RETURN  ENTRY(2, parVlaue, "=").
        END.
        RETURN "".
    END METHOD.

    /* 
     * Set a parameter value parameter into a list of parameter-value.
     * The format of the list is: ParamName1=value1[SEPparamName2=value2[SEL...]]
     * where SEP is used to separate the tokep=value list
     *
     * <param name="parameterString"> The parameter-value list</param>
     * <param name="parameterName"> Parameter nane of which value is requested</param>
     * <param name="separator">List separator</param>
     * <return> new list with parameter value updated 
     */
    METHOD PUBLIC STATIC CHARACTER SetTokenInString(parameterString AS CHARACTER, parameterName AS CHARACTER , parameterValue AS CHARACTER):
    
        RETURN SetTokenInString(parameterString, parameterName, parameterValue, '|').
    END METHOD.

    /* 
     * Set a parameter value parameter into a list of parameter-value.
     * The format of the list is: ParamName1=value1[SEPparamName2=value2[SEL...]]
     * where SEP is used to separate the tokep=value list
     *
     * <param name="parameterString"> The parameter-value list</param>
     * <param name="parameterName"> Parameter nane of which value is requested</param>
     * <param name="parameterValue> Parameter value to set 
     * <param name="separator">List separator</param>
     * <return> new list with parameter value updated 
     */
    METHOD PUBLIC STATIC CHARACTER SetTokenInString(parameterString AS CHARACTER, parameterName AS CHARACTER , parameterValue AS CHARACTER, separator AS CHARACTER):
    
        DEFINE VARIABLE iIndex          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE tokenAndValue   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lFound          AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE newList         AS CHARACTER NO-UNDO.
        
        IF parameterName = ? OR parameterName = "" OR parameterValue = ? OR parameterValue = "" THEN
                RETURN "".

        IF parameterString = ? OR parameterString = "" THEN
                RETURN parameterName + "=" + parameterValue.

        lFound = FALSE.
        DO iIndex= 1 TO NUM-ENTRIES(parameterString, separator):
            tokenAndValue = ENTRY(iIndex, parameterString, separator).
            IF tokenAndValue = "" THEN NEXT.
            
            IF ENTRY(1, tokenAndValue, "=") = parameterName THEN DO:
                ENTRY(2, tokenAndValue, "=") = parameterValue.
                lFound = TRUE.
            END.
            
            IF newList = "" THEN 
                newList = tokenAndValue.
            ELSE 
                newList= newList + "|" + tokenAndValue.
        END.
        IF lFound = FALSE THEN
            RETURN parameterString + "|" + parameterName + "=" + parameterValue.
        RETURN newList.
                            
    END METHOD.

    METHOD PUBLIC STATIC CHARACTER RemoveTokenFromString(parameterString AS CHARACTER,parameterToken AS CHARACTER):

            RETURN RemoveTokenFromString(parameterString, parameterToken, '|').
    END METHOD.

    METHOD PUBLIC STATIC CHARACTER RemoveTokenFromString(parameterString AS CHARACTER, parameterToken AS CHARACTER, separator AS CHARACTER):
    
        DEFINE VARIABLE iIndex          AS INTEGER NO-UNDO.
        DEFINE VARIABLE tokenAndValue   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE newList         AS CHARACTER NO-UNDO INITIAL "".
        
        IF parameterToken = ? OR parameterToken= "" THEN 
                RETURN "".
        IF parameterString = ? OR parameterString= "" THEN 
            RETURN "".

        DO iIndex = 1 TO NUM-ENTRIES(parameterString, separator):
            tokenAndValue = ENTRY(iIndex, parameterString, separator).
            IF tokenAndValue = "" THEN NEXT.
            
            IF ENTRY(1, tokenAndValue, "=") = parameterToken THEN
                NEXT.
                
            IF newList = "" THEN
                newList = tokenAndValue.
            ELSE
                newList = newList + "|" + tokenAndValue.
        END.
        RETURN newList.
    END METHOD.
    
    METHOD PUBLIC STATIC CHARACTER RemoveSymbols (INPUT ipcValue AS CHARACTER):
        RETURN RemoveSymbols (INPUT ipcValue, INPUT FALSE).
    END.
    
    METHOD PUBLIC STATIC CHARACTER RemoveSymbols (INPUT ipcValue AS CHARACTER, INPUT iplRemoveSpace AS LOGICAL):
        DEFINE VARIABLE iOrder   AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cReturn  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iCharAsc AS INTEGER   NO-UNDO.
        
        DO iOrder = 1 TO LENGTH (ipcValue):
            iCharAsc = ASC (SUBSTRING(ipcValue,iOrder,1)).
            
            IF (iCharAsc >= ASC("A") AND iCharAsc <= ASC("Z"))
            OR (iCharAsc >= ASC("a") AND iCharAsc <= ASC("z"))
            OR (iCharAsc >= ASC("0") AND iCharAsc <= ASC("9")) 
                THEN  
                cReturn = cReturn + SUBSTRING(ipcValue, iOrder, 1).
                
            IF NOT iplRemoveSpace AND (iCharAsc = 32 OR iCharAsc = 9) THEN                            
                cReturn = cReturn + SUBSTRING(ipcValue, iOrder, 1).
        END.
        RETURN cReturn.
    END METHOD.            
END CLASS.