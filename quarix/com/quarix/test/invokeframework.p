&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
File : invokeframework
Purpose : invoke a call to the framework (for testing purposes)
Syntax :
Description :
Author(s) : tomd
Created : Thu Aug 12 13:55:00 CET 2014
Notes :
License :
This file is part of the QRX-SRV-OE software framework.

The QRX-SRV-OE software framework is free software; you can redistribute
it and/or modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either version 2.1
of the License, or (at your option) any later version.

The QRX-SRV-OE software framework is distributed in the hope that it will
be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the QRX-SRV-OE software framework; if not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA or on the internet at the following address:
http://www.gnu.org/licenses/lgpl-2.1.txt
----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* {locals.i} */

{com/quarix/web/ttRequest.i}
{com/quarix/web/ttResponse.i}

DEFINE TEMP-TABLE ttDaoMsgParams NO-UNDO
  FIELD filterGroup   AS INT
  FIELD tempTableName AS CHAR
  FIELD batchSize     AS INT.

DEFINE TEMP-TABLE ttFilters NO-UNDO
  FIELD filterGroup AS INT
  FIELD filterName  AS CHAR
  FIELD filterOp    AS CHAR
  FIELD filterValue AS CHAR
  INDEX idxPK IS PRIMARY UNIQUE 
    filterGroup 
    filterName 
    filterOp.

DEFINE TEMP-TABLE ttRecordChange NO-UNDO
  FIELD recordRowId     AS CHAR /* rowid of the db record */
  FIELD RowState        AS INT  /* row-state of the bi record */
  INDEX PK_ttRecordChange IS PRIMARY UNIQUE
    recordRowId.

DEFINE TEMP-TABLE ttFieldChange NO-UNDO
  FIELD recordRowId     AS CHAR /* rowid of the db record */
  FIELD isBeforeImage   AS LOG  /* before image or after image */
  FIELD fieldSortOrder  AS INT  /* sortorder of the temp-table fields */
  FIELD fieldName       AS CHAR /* Name of the temp-table field */
  FIELD fieldValue      AS CHAR /* Value of the temp-table field */
  INDEX PK_ttRecordChange IS PRIMARY UNIQUE 
    recordRowId 
    isBeforeImage
    fieldSortOrder.

&GLOBAL-DEFINE FILTER_OP_EQUAL "eq"
&GLOBAL-DEFINE FILTER_OP_GREATER_OR_EQUAL "ge"
&GLOBAL-DEFINE FILTER_OP_GREATER_THAN "gt"
&GLOBAL-DEFINE FILTER_OP_LESS_OR_EQUAL "le"
&GLOBAL-DEFINE FILTER_OP_LESS_THAN "lt"
&GLOBAL-DEFINE FILTER_OP_NOT_EQUAL "ne"

&GLOBAL-DEFINE REQ_TYPE_INPUT    1
&GLOBAL-DEFINE REQ_TYPE_HEADERS  2
&GLOBAL-DEFINE REQ_TYPE_CGI      3
&GLOBAL-DEFINE REQ_TYPE_SESSION  4

&global-define xml_elem_datasetmsg   "datasetMsg"
&global-define xml_elem_action       "action"
&global-define xml_elem_daomsg       "daoMsg"
&global-define xml_elem_filter       "filter"
&global-define xml_elem_row          "row"
&global-define xml_elem_rowimage     "rowImage"
&global-define xml_elem_fld          "fld"
                                    
&global-define xml_attr_command          "command"
&global-define xml_attr_val_sendrows     "sendRows"
&global-define xml_attr_val_submitCommit "submitCommit"

DEF VAR iFilterGroup AS INT NO-UNDO.
DEF VAR cAppName as char no-undo init "ecommerce".

/* Connectie naar de broker port van de appserver */
DEF VAR cAppSrvURL AS CHAR NO-UNDO. 

ASSIGN cAppSrvURL = "-URL 'AppServerDC://localhost:44050'". /* Locale Quarix appserver */
/*ASSIGN cAppSrvURL = "-URL 'AppServerDC://be-plw-sde-0001.tvh.com:11130'". /* Yonder appserver */*/

DEFINE VARIABLE lDebug  AS LOGICAL    NO-UNDO INIT TRUE.
DEFINE VARIABLE cLogDir AS CHARACTER  NO-UNDO INIT "C:~\Temp~\framework_test~\response~\".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-buildRequestXML) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildRequestXML Procedure 
FUNCTION buildRequestXML RETURNS LONGCHAR
  ( INPUT icCommand       AS CHAR,
    INPUT icStartRowId    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-generateSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD generateSessionId Procedure 
FUNCTION generateSessionId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 20.43
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/*
IF THIS-PROCEDURE:PERSISTENT THEN
  SOURCE-PROCEDURE:ADD-SUPER-PROCEDURE(THIS-PROCEDURE).
  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addCGIParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addCGIParam Procedure 
PROCEDURE addCGIParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM fieldName  AS CHAR NO-UNDO.
  DEF INPUT PARAM fieldValue AS LONGCHAR NO-UNDO.

  RUN addParam({&REQ_TYPE_CGI}, fieldName, fieldValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addDaoMsgParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addDaoMsgParams Procedure 
PROCEDURE addDaoMsgParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER icTempTableName AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER iiBatchSize     AS INTEGER    NO-UNDO.

  CREATE ttDaoMsgParams.
  ASSIGN ttDaoMsgParams.filterGroup   = iFilterGroup
         ttDaoMsgParams.tempTableName = icTempTableName
         ttDaoMsgParams.batchSize     = iiBatchSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFilter Procedure 
PROCEDURE addFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM cFilterName  AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilterOp    AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilterValue AS CHAR NO-UNDO.

  CREATE ttFilters.
  ASSIGN ttFilters.filterGroup = iFilterGroup
         ttFilters.filterName  = cFilterName
         ttFilters.filterOp    = cFilterOp
         ttFilters.filterValue = cFilterValue.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addFilterEqual) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFilterEqual Procedure 
PROCEDURE addFilterEqual :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM cFilterName  AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilterValue AS CHAR NO-UNDO.

  RUN addFilter(cFilterName, {&FILTER_OP_EQUAL}, cFilterValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addFilterGreaterOrEqual) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFilterGreaterOrEqual Procedure 
PROCEDURE addFilterGreaterOrEqual :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM cFilterName  AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilterValue AS CHAR NO-UNDO.

  RUN addFilter(cFilterName, {&FILTER_OP_GREATER_OR_EQUAL}, cFilterValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addFilterGreaterThan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFilterGreaterThan Procedure 
PROCEDURE addFilterGreaterThan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM cFilterName  AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilterValue AS CHAR NO-UNDO.

  RUN addFilter(cFilterName, {&FILTER_OP_GREATER_THAN}, cFilterValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addFilterLessOrEqual) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFilterLessOrEqual Procedure 
PROCEDURE addFilterLessOrEqual :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM cFilterName  AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilterValue AS CHAR NO-UNDO.

  RUN addFilter(cFilterName, {&FILTER_OP_LESS_OR_EQUAL}, cFilterValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addFilterLessThan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFilterLessThan Procedure 
PROCEDURE addFilterLessThan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM cFilterName  AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilterValue AS CHAR NO-UNDO.

  RUN addFilter(cFilterName, {&FILTER_OP_LESS_THAN}, cFilterValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addFilterNotEqual) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFilterNotEqual Procedure 
PROCEDURE addFilterNotEqual :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM cFilterName  AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilterValue AS CHAR NO-UNDO.

  RUN addFilter(cFilterName, {&FILTER_OP_NOT_EQUAL}, cFilterValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addInputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addInputParam Procedure 
PROCEDURE addInputParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM fieldName  AS CHAR NO-UNDO.
  DEF INPUT PARAM fieldValue AS LONGCHAR NO-UNDO.

  RUN addParam({&REQ_TYPE_INPUT}, fieldName, fieldValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addInputXmlParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addInputXmlParam Procedure 
PROCEDURE addInputXmlParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM fieldValue AS LONGCHAR NO-UNDO.

  RUN addInputParam("xml", fieldValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addLessThanFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addLessThanFilter Procedure 
PROCEDURE addLessThanFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addParam Procedure 
PROCEDURE addParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM fieldType  AS INT  NO-UNDO.
  DEF INPUT PARAM fieldName  AS CHAR NO-UNDO.
  DEF INPUT PARAM fieldValue AS LONGCHAR NO-UNDO.

  CREATE ttRequest.
  ASSIGN ttRequest.fieldType  = fieldType
         ttRequest.fieldName  = fieldName.

  COPY-LOB FROM fieldValue TO ttRequest.fieldValue NO-CONVERT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addProperty Procedure 
PROCEDURE addProperty :
/*------------------------------------------------------------------------------
  Purpose:     Add a property to the filter-table
  Parameters:  <none>
  Notes:       If the filterName begins with an "_", its a property
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER icPropertyName  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER icPropertyValue AS CHARACTER  NO-UNDO.

  RUN addFilter(INPUT "_" + icPropertyName,
                INPUT "begins", /* Operator is not used for properties, but cannot be empty */
                INPUT icPropertyValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addRecordChange) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecordChange Procedure 
PROCEDURE addRecordChange :
/*------------------------------------------------------------------------------
  Purpose:     Create temp-table record with before and after image
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER icRowId     AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER ihBeforeBuf AS HANDLE      NO-UNDO.
  DEFINE INPUT  PARAMETER ihAfterBuf  AS HANDLE      NO-UNDO.

  DEFINE VARIABLE iField AS INTEGER    NO-UNDO.

  CREATE ttRecordChange.
  ASSIGN ttRecordChange.recordRowId = icRowId
         ttRecordChange.RowState    = ihBeforeBuf:ROW-STATE.

  DO iField = 1 TO ihBeforeBuf:NUM-FIELDS:
    CREATE ttFieldChange.
    ASSIGN ttFieldChange.recordRowId    = icRowId
           ttFieldChange.isBeforeImage  = TRUE
           ttFieldChange.fieldSortOrder = iField
           ttFieldChange.fieldName      = ihBeforeBuf:BUFFER-FIELD(iField):NAME
           ttFieldChange.fieldValue     = ihBeforeBuf:BUFFER-FIELD(iField):BUFFER-VALUE.
  END.

  IF ihAfterBuf:AVAILABLE THEN
  DO iField = 1 TO ihAfterBuf:NUM-FIELDS:
    CREATE ttFieldChange.
    ASSIGN ttFieldChange.recordRowId    = icRowId
           ttFieldChange.isBeforeImage  = FALSE
           ttFieldChange.fieldSortOrder = iField
           ttFieldChange.fieldName      = ihAfterBuf:BUFFER-FIELD(iField):NAME
           ttFieldChange.fieldValue     = ihAfterBuf:BUFFER-FIELD(iField):BUFFER-VALUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addSessionParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addSessionParam Procedure 
PROCEDURE addSessionParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM fieldName  AS CHAR NO-UNDO.
  DEF INPUT PARAM fieldValue AS LONGCHAR NO-UNDO.

  RUN addParam({&REQ_TYPE_SESSION}, fieldName, fieldValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clearDaoMsgParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearDaoMsgParams Procedure 
PROCEDURE clearDaoMsgParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  EMPTY TEMP-TABLE ttDaoMsgParams.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clearFilters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearFilters Procedure 
PROCEDURE clearFilters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  EMPTY TEMP-TABLE ttFilters.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clearRecordChange) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearRecordChange Procedure 
PROCEDURE clearRecordChange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  EMPTY TEMP-TABLE ttRecordChange.
  EMPTY TEMP-TABLE ttFieldChange.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-invokeServerCall) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE invokeServerCall Procedure 
PROCEDURE invokeServerCall :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM cSessionId     AS CHAR NO-UNDO.
  DEF INPUT PARAM cPathInfo      AS CHAR NO-UNDO.
  DEF INPUT PARAM icCommand      AS CHAR NO-UNDO.
  DEF INPUT PARAM icStartRowId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM TABLE FOR ttResponse.
  DEF OUTPUT PARAM pstrResponse  AS MEMPTR NO-UNDO.


  DEF VAR hSvr AS HANDLE NO-UNDO.

  DEF VAR pstrRequest  AS MEMPTR NO-UNDO.

  DEFINE VARIABLE deTime AS DECIMAL    NO-UNDO.

  CREATE SERVER hSvr.
  IF hSvr:CONNECT(cAppSrvURL,?,?,?) THEN DO ON ERROR UNDO, THROW:
      MESSAGE "connected" VIEW-AS ALERT-BOX.

      RUN serverRequest(cSessionId, 
                        cPathInfo, 
                        buildRequestXml(INPUT icCommand,
                                        INPUT icStartRowId)).

      EMPTY TEMP-TABLE ttResponse.

      IF lDebug THEN DO:
        OUTPUT TO VALUE(cLogDir + cSessionId + "-ttRequest.d").
        FOR EACH ttRequest NO-LOCK:
          EXPORT ttRequest.
        END.
        OUTPUT CLOSE.

        OUTPUT TO VALUE(cLogDir + cSessionId + "-strRequest.txt") BINARY NO-CONVERT.
        EXPORT pstrRequest.
        OUTPUT CLOSE.
      END.

      ASSIGN deTime = ETIME.

      RUN com/quarix/bin/dispatcher.p ON SERVER hSvr
        (INPUT TABLE ttRequest, 
         INPUT pstrRequest,
         OUTPUT TABLE ttResponse,
         OUTPUT pstrResponse).

      ASSIGN deTime = ETIME - deTime.

      IF lDebug THEN DO:
        OUTPUT TO VALUE(cLogDir + cSessionId + "-ttResponse.d").
        FOR EACH ttResponse NO-LOCK:
          EXPORT ttResponse.
        END.
        OUTPUT CLOSE.

        OUTPUT TO VALUE(cLogDir + cSessionId + "-strResponse.txt") BINARY NO-CONVERT.
        EXPORT pstrResponse.
        OUTPUT CLOSE.

        MESSAGE "Time required to fetch data" STRING(INT(TRUNCATE(deTime / 1000,0)),"hh:mm:ss") + ":" + STRING(deTime MOD 1000,"999")
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.

  END.

  FINALLY:
    IF VALID-HANDLE(hSvr) THEN DO:
      IF hSvr:CONNECTED() THEN
        hSvr:DISCONNECT() NO-ERROR.
      DELETE OBJECT hSvr NO-ERROR.
    END.
  END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-newDaoMsgParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newDaoMsgParams Procedure 
PROCEDURE newDaoMsgParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER icTempTableName AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER iiBatchSize     AS INTEGER    NO-UNDO.

  ASSIGN iFilterGroup = 0.

  RUN clearDaoMsgParams.
  RUN clearRecordChange.
  RUN clearFilters.

  RUN addDaoMsgParams(INPUT icTempTableName,
                      INPUT iiBatchSize).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-newFilterGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newFilterGroup Procedure 
PROCEDURE newFilterGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN iFilterGroup = iFilterGroup + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendRows Procedure 
PROCEDURE sendRows :
/*------------------------------------------------------------------------------
  Purpose:     Fetch records from the database using the quarix framework
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ihTT         AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER icModule     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER icStartRowId AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER icSessionId  AS CHAR       NO-UNDO.

  DEF VAR pstrResponse AS MEMPTR NO-UNDO.

  RUN invokeServerCall
    (INPUT icSessionId, 
     INPUT cAppName + "/" + /* application name */
           icModule + "/" + /* module name */
           "data", /* module method  
                        ACTION_DATA = data (SendRows + SubmitCommit), 
                        ACTION_PAINT = paint (?) */
     INPUT {&xml_attr_val_sendrows}, /* Execute sendRows */
     INPUT icStartRowId,
     OUTPUT TABLE ttResponse BY-REFERENCE,
     OUTPUT pstrResponse).
  
  /*
  OUTPUT TO c:/qrx_srv_oe/LOG/res.json BINARY NO-CONVERT.
  EXPORT pstrResponse.
  OUTPUT CLOSE.
  */ 
  
  RUN com/quarix/test/jsonreader.p(INPUT-output pstrResponse,
                                   INPUT ihTT).
  
/*  RUN populateTTfromJSON(INPUT pstrResponse, INPUT ihTT BY-REFERENCE).  */
/*
  RUN com/quarix/test/p-json-reader.p(INPUT pstrResponse,
                                      INPUT ihTT).
*/
  SET-SIZE(pstrResponse) = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-serverRequest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE serverRequest Procedure 
PROCEDURE serverRequest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM cSessionId AS CHAR NO-UNDO.
  DEF INPUT PARAM cPathInfo  AS CHAR NO-UNDO.
  DEF INPUT PARAM lcInputXml AS LONGCHAR NO-UNDO.

  MESSAGE STRING(lcInputXml) VIEW-AS ALERT-BOX.

  EMPTY TEMP-TABLE ttRequest.

  RUN addInputXmlParam(lcInputXml).

  RUN addCGIParam("CHECK_ATTACHMENTS", string(false)).
  RUN addCGIParam("CHECK_BODY_DATA", string(false)).
  RUN addCGIParam("COUNTRY", "BE").
  RUN addCGIParam("LANG-PREF-LIST", "BE").
  RUN addCGIParam("PATH_INFO", cPathInfo).
  RUN addCGIParam("SESSION_ID", cSessionId).
  RUN addCGIParam("SESSION_MAX_INACTIVE_INTERVAL", "300000").
  RUN addCGIParam("UTC_OFFSET", "2").
  
  RUN addSessionParam("APP_NAME", cAppName).
  RUN addSessionParam("LOCALE_LANG", "en").
  RUN addSessionParam("USER_THEME", "default").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAppName Procedure 
PROCEDURE setAppName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM icAppName AS CHAR NO-UNDO.
  ASSIGN cAppName = icAppName.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppSrvURL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAppSrvURL Procedure 
PROCEDURE setAppSrvURL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM icAppSrvURL AS CHAR NO-UNDO.
  ASSIGN cAppSrvURL = icAppSrvURL.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDebug) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDebug Procedure 
PROCEDURE setDebug :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ilDebug AS LOG NO-UNDO.
  ASSIGN lDebug = ilDebug.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLogDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLogDir Procedure 
PROCEDURE setLogDir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM icLogDir AS CHAR NO-UNDO.
  ASSIGN cLogDir = icLogDir.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-submitCommit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE submitCommit Procedure 
PROCEDURE submitCommit :
/*------------------------------------------------------------------------------
  Purpose:     Update db-records using the quarix framework
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ihTT        AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER icModule    AS CHARACTER  NO-UNDO.

  DEF VAR pstrResponse   AS MEMPTR NO-UNDO.
  DEF VAR cSessionId     AS CHAR   NO-UNDO.

  ASSIGN cSessionId = generateSessionId().

  RUN invokeServerCall
    (INPUT cSessionId, 
     INPUT cAppName + "/" + /* application name */
           icModule + "/" + /* module name */
           "data", /* module method  
                        ACTION_DATA = data (SendRows + SubmitCommit), 
                        ACTION_PAINT = paint (?) */
     INPUT {&xml_attr_val_submitCommit}, /* Execute submitCommit */
     INPUT "commit", /* StartRowId */
     OUTPUT TABLE ttResponse BY-REFERENCE,
     OUTPUT pstrResponse).
     
  message "test" view-as alert-box.

  run com/quarix/test/jsonreader.p(input-output pstrResponse, input ihTT).
  /*
  RUN com/quarix/test/p-json-reader.p(INPUT pstrResponse,
                                      INPUT ihTT).
  */
  SET-SIZE(pstrResponse) = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-buildRequestXML) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildRequestXML Procedure 
FUNCTION buildRequestXML RETURNS LONGCHAR
  ( INPUT icCommand       AS CHAR,
    INPUT icStartRowId    AS CHAR ) :
/* ----------------------------------------------------------------------------
PURPOSE:  
NOTES:  
HISTORY:
2011/09/13 tomde edp  : Creation.
---------------------------------------------------------------------------- */
  DEF VAR lcXml      AS LONGCHAR NO-UNDO.
  DEF VAR hSaxWriter AS HANDLE NO-UNDO.

  CREATE SAX-WRITER hSaxWriter.

  hSaxWriter:SET-OUTPUT-DESTINATION ( "longchar", lcXml).
  hSaxWriter:ENCODING = 'utf-8'.
  hSaxWriter:START-DOCUMENT().
  hSaxWriter:START-ELEMENT({&xml_elem_datasetmsg}).

  hSaxWriter:START-ELEMENT({&xml_elem_action}).
  hSaxWriter:INSERT-ATTRIBUTE({&xml_attr_command}, icCommand).
  hSaxWriter:INSERT-ATTRIBUTE("responseFormat", "json").
  hSaxWriter:END-ELEMENT({&xml_elem_action}).

  FOR EACH ttDaoMsgParams NO-LOCK:
    hSaxWriter:START-ELEMENT({&xml_elem_daomsg}).
    hSaxWriter:INSERT-ATTRIBUTE("batchSize", STRING(ttDaoMsgParams.batchSize)).
    hSaxWriter:INSERT-ATTRIBUTE({&xml_attr_command}, icCommand).
    hSaxWriter:INSERT-ATTRIBUTE("id", ttDaoMsgParams.TempTableName).
    hSaxWriter:INSERT-ATTRIBUTE("prefetch", "false"). /*** Van belang voor submitCommit? ***/
    hSaxWriter:INSERT-ATTRIBUTE("skipRow", "true").   /*** Van belang voor submitCommit? ***/
    hSaxWriter:INSERT-ATTRIBUTE("startRowId", icStartRowId).

    /* Add filter data */
    FOR EACH ttFilters WHERE ttFilters.filterGroup = ttDaoMsgParams.FilterGroup NO-LOCK:
        hSaxWriter:START-ELEMENT({&xml_elem_filter}).
        hSaxWriter:INSERT-ATTRIBUTE("fld", ttFilters.filterName).
        hSaxWriter:INSERT-ATTRIBUTE("op",  ttFilters.filterOp).
        hSaxWriter:INSERT-ATTRIBUTE("val", ttFilters.filterValue).
        hSaxWriter:END-ELEMENT({&xml_elem_filter}).
    END.

    /* Add before and after image for submitCommit */
    FOR EACH ttRecordChange NO-LOCK:
      hSaxWriter:START-ELEMENT({&xml_elem_row}).
      hSaxWriter:INSERT-ATTRIBUTE("id", ttRecordChange.recordRowId).
      hSaxWriter:INSERT-ATTRIBUTE("state", STRING(ttRecordChange.RowState)).

      /* Before image */
      FIND FIRST ttFieldChange WHERE 
        ttFieldChange.recordRowId = ttRecordChange.recordRowId AND
        ttFieldChange.isBeforeImage = TRUE
        NO-LOCK NO-ERROR.
      IF AVAIL ttFieldChange THEN DO:
        hSaxWriter:START-ELEMENT({&xml_elem_rowimage}).
        hSaxWriter:INSERT-ATTRIBUTE("type", "bi").

        hSaxWriter:START-ELEMENT({&xml_elem_fld}).
        hSaxWriter:INSERT-ATTRIBUTE("name", "rowid").
        hSaxWriter:WRITE-CHARACTERS(ttRecordChange.recordRowId).
        hSaxWriter:END-ELEMENT({&xml_elem_fld}).

        hSaxWriter:START-ELEMENT({&xml_elem_fld}).
        hSaxWriter:INSERT-ATTRIBUTE("name", "rowstate").
        hSaxWriter:WRITE-CHARACTERS(STRING(ttRecordChange.RowState)).
        hSaxWriter:END-ELEMENT({&xml_elem_fld}).

        FOR EACH ttFieldChange WHERE 
          ttFieldChange.recordRowId = ttRecordChange.recordRowId AND
          ttFieldChange.isBeforeImage = TRUE
          NO-LOCK:
          hSaxWriter:START-ELEMENT({&xml_elem_fld}).
          hSaxWriter:INSERT-ATTRIBUTE("name", ttFieldChange.fieldName).
          hSaxWriter:WRITE-CHARACTERS(ttFieldChange.fieldValue).
          hSaxWriter:END-ELEMENT({&xml_elem_fld}).
        END.

        hSaxWriter:END-ELEMENT({&xml_elem_rowimage}).
      END.

      /* After image */
      FIND FIRST ttFieldChange WHERE 
        ttFieldChange.recordRowId = ttRecordChange.recordRowId AND
        ttFieldChange.isBeforeImage = FALSE
        NO-LOCK NO-ERROR.
      IF AVAIL ttFieldChange THEN DO:
        hSaxWriter:START-ELEMENT({&xml_elem_rowimage}).
        hSaxWriter:INSERT-ATTRIBUTE("type", "ai").

        hSaxWriter:START-ELEMENT({&xml_elem_fld}).
        hSaxWriter:INSERT-ATTRIBUTE("name", "rowid").
        hSaxWriter:WRITE-CHARACTERS(ttRecordChange.recordRowId).
        hSaxWriter:END-ELEMENT({&xml_elem_fld}).

        hSaxWriter:START-ELEMENT({&xml_elem_fld}).
        hSaxWriter:INSERT-ATTRIBUTE("name", "rowstate").
        hSaxWriter:WRITE-CHARACTERS(STRING(ttRecordChange.RowState)).
        hSaxWriter:END-ELEMENT({&xml_elem_fld}).

        FOR EACH ttFieldChange WHERE 
          ttFieldChange.recordRowId = ttRecordChange.recordRowId AND
          ttFieldChange.isBeforeImage = FALSE
          NO-LOCK:
          hSaxWriter:START-ELEMENT({&xml_elem_fld}).
          hSaxWriter:INSERT-ATTRIBUTE("name", ttFieldChange.fieldName).
          hSaxWriter:WRITE-CHARACTERS(ttFieldChange.fieldValue).
          hSaxWriter:END-ELEMENT({&xml_elem_fld}).
        END.

        hSaxWriter:END-ELEMENT({&xml_elem_rowimage}).
      END.

      hSaxWriter:END-ELEMENT({&xml_elem_row}).
    END.

    hSaxWriter:END-ELEMENT({&xml_elem_daomsg}).
  END.

  hSaxWriter:END-ELEMENT({&xml_elem_datasetmsg}).
  hSaxWriter:END-DOCUMENT().

  RETURN lcXml.
 
  FINALLY:
    IF VALID-HANDLE(hSaxWriter) THEN
      DELETE OBJECT hSaxWriter.
  END FINALLY.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-generateSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION generateSessionId Procedure 
FUNCTION generateSessionId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/* ----------------------------------------------------------------------------
PURPOSE:  
NOTES:  
HISTORY:
2011/09/13 tomde edp  : Creation.
---------------------------------------------------------------------------- */

  RETURN STRING(ETIME(FALSE)).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

