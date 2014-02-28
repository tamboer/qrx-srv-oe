&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*
*/

USING com.mrg.framework.util.*.
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE STREAM sOutput.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmbDbs fillRoot fillRelative selectTables ~
togSI togBE togDA togDS edLog btnGenerate btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cmbDbs fillRoot fillRelative selectTables ~
togSI togBE togDA togDS edLog 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetMandatoryFieldList C-Win 
FUNCTION GetMandatoryFieldList RETURNS CHARACTER
  ( databaseName AS CHARACTER, tableName AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnGenerate 
     LABEL "Generate" 
     SIZE 15 BY 1.14 TOOLTIP "Generate source code".

DEFINE VARIABLE cmbDbs AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE edLog AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 92 BY 10 NO-UNDO.

DEFINE VARIABLE fillRelative AS CHARACTER FORMAT "X(256)":U 
     LABEL "Relative path" 
     VIEW-AS FILL-IN 
     SIZE 77.6 BY 1 NO-UNDO.

DEFINE VARIABLE fillRoot AS CHARACTER FORMAT "X(256)":U 
     LABEL "Root folder" 
     VIEW-AS FILL-IN 
     SIZE 77.6 BY 1 NO-UNDO.

DEFINE VARIABLE selectTables AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 42 BY 15.48 NO-UNDO.

DEFINE VARIABLE togBE AS LOGICAL INITIAL yes 
     LABEL "Generate Business Entities" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE togDA AS LOGICAL INITIAL yes 
     LABEL "Generate Data Access" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE togDS AS LOGICAL INITIAL yes 
     LABEL "Generate Dataset and Temp-Table" 
     VIEW-AS TOGGLE-BOX
     SIZE 52.6 BY .81 NO-UNDO.

DEFINE VARIABLE togSI AS LOGICAL INITIAL yes 
     LABEL "Generate Service Interface" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbDbs AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 30
     fillRoot AT ROW 1.24 COL 57.4 COLON-ALIGNED WIDGET-ID 20
     fillRelative AT ROW 2.43 COL 57.4 COLON-ALIGNED WIDGET-ID 36
     selectTables AT ROW 2.67 COL 2 NO-LABEL WIDGET-ID 2
     togSI AT ROW 3.67 COL 59.4 WIDGET-ID 12
     togBE AT ROW 4.52 COL 59.4 WIDGET-ID 8
     togDA AT ROW 5.43 COL 59.4 WIDGET-ID 10
     togDS AT ROW 6.33 COL 59.4 WIDGET-ID 18
     edLog AT ROW 8.14 COL 45 NO-LABEL WIDGET-ID 26
     btnGenerate AT ROW 18.43 COL 106.4 WIDGET-ID 32
     btnCancel AT ROW 18.43 COL 122 WIDGET-ID 34
     "Logging" VIEW-AS TEXT
          SIZE 92 BY .62 AT ROW 7.43 COL 45.2 WIDGET-ID 28
          BGCOLOR 7 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136.4 BY 18.91
         CANCEL-BUTTON btnCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Generate objects"
         HEIGHT             = 18.76
         WIDTH              = 137
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 193
         VIRTUAL-HEIGHT     = 29
         VIRTUAL-WIDTH      = 193
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX cmbDbs IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       edLog:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generate objects */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generate objects */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGenerate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGenerate C-Win
ON CHOOSE OF btnGenerate IN FRAME DEFAULT-FRAME /* Generate */
DO:
    DEFINE VARIABLE tableIdx AS INTEGER NO-UNDO.    
    ASSIGN FRAME {&frame-name}
        cmbDbs
        togSI
        togBE
        togDA
        togDS
        fillRoot
        fillRelative.
        
    edLog:screen-value = "Start generating source file at ":U + STRING (NOW) + "...~n".    
    DO tableIdx = 1 TO NUM-ENTRIES (selectTables:screen-value):  
        IF togSI THEN
            RUN GenerateSI (ENTRY (tableIdx, selectTables:screen-value), fillRoot, fillRelative).
        IF togBE THEN
            RUN GenerateBE (ENTRY (tableIdx, selectTables:screen-value), fillRoot, fillRelative).
        IF togDA THEN
            RUN GenerateDA (ENTRY (tableIdx, selectTables:screen-value), fillRoot, fillRelative).
        IF togDS THEN
            RUN GenerateDS (cmbDbs, ENTRY (tableIdx, selectTables:screen-value), fillRoot, fillRelative).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbDbs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbDbs C-Win
ON VALUE-CHANGED OF cmbDbs IN FRAME DEFAULT-FRAME
DO:
    
    ASSIGN FRAME {&FRAME-NAME} cmbDbs.
    
    DEFINE VARIABLE queryHandle AS HANDLE NO-UNDO.
    DEFINE VARIABLE bufferHandle AS HANDLE NO-UNDO.
    DEFINE VARIABLE tableName AS CHARACTER NO-UNDO.
    
    tableName = SUBSTITUTE ("&1._file", cmbDbs).
    
    CREATE BUFFER bufferHandle FOR TABLE tableName.
    CREATE QUERY queryHandle.
    queryHandle:SET-BUFFERS (bufferHandle).
    queryHandle:QUERY-PREPARE (SUBST ("for each &1 where &1._hidden = no", tableName)).
    queryHandle:QUERY-OPEN ().
    queryHandle:GET-FIRST (NO-LOCK).
    selectTables:LIST-ITEMS = ?.
    DO WHILE bufferHandle:AVAILABLE:
        selectTables:ADD-LAST (bufferHandle::_file-name).
        queryHandle:GET-NEXT (NO-LOCK).
    END.
    queryHandle:QUERY-CLOSE ().
    
    FINALLY:
        DELETE OBJECT bufferHandle NO-ERROR.
        DELETE OBJECT queryHandle NO-ERROR.
   END FINALLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitObject.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cmbDbs fillRoot fillRelative selectTables togSI togBE togDA togDS 
          edLog 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cmbDbs fillRoot fillRelative selectTables togSI togBE togDA togDS 
         edLog btnGenerate btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateBE C-Win 
PROCEDURE GenerateBE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER tableName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER rootFolder AS CHARACTER.
    DEFINE INPUT PARAMETER relativeFolder AS CHARACTER.
    
    DEFINE VARIABLE fullFileName AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE fileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE className AS CHARACTER NO-UNDO.
    DEFINE VARIABLE daClassName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE datasetName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE mandatoryFieldList AS CHARACTER No-UNDO.
    ASSIGN
        className = RIGHT-TRIM (REPLACE (com.mrg.framework.util.FileSystem:UnixName (relativeFolder), "~/", "."), ".") + ".bl.BE" + tableName
        daClassName = REPLACE (className, ".bl.BE", ".data.DA")
        datasetName = REPLACE (REPLACE (className, ".bl.BE", ".bl.ds"), ".", "~/") + ".i"
        fileName = SUBSTITUTE ("BE&1.cls", tableName)
        fullFileName = com.mrg.framework.util.FileSystem:BuildFolderName(rootFolder, relativeFolder, "bl").
    
    IF NOT com.mrg.framework.util.FileSystem:FolderExist(fullFileName) THEN
        com.mrg.framework.util.FileSystem:CreateFolder (fullFileName).
    
    fullFileName = com.mrg.framework.util.FileSystem:BuildFolderName (fullFileName, fileName).        
    
    DO WITH FRAME {&frame-name}:
        edLog:MOVE-TO-EOF ().
        edLog:INSERT-STRING ("Generating " + fullFileName + "...").
        mandatoryFieldList = GetMandatoryFieldList (cmbDbs:screen-value, tableName).            
        OUTPUT STREAM sOutput TO VALUE (fullFileName).
        PUT STREAM sOutput UNFORMATTED
            "/**************************************************************************" SKIP
            " * File        : " fileName SKIP
            " * Purpose     : " SKIP
            " * Syntax      : " SKIP
            " * Description : " SKIP
            " * Author(s)   : Generated by the ObjectGenerator" SKIP
            " * Created     : " STRING (NOW) SKIP
            " * Notes       : " SKIP
            " **************************************************************************/" SKIP (1)                      
            "USING Progress.Lang.*." SKIP(1)            
            "ROUTINE-LEVEL ON ERROR UNDO, THROW." SKIP(1)            
            "CLASS " className " INHERITS com.mrg.framework.data.BusinessEntity:" SKIP (1)                 
            "    ~{" datasetName " ~&REFERENCE-ONLY=REFERENCE-ONLY~}" SKIP (1)
            "    CONSTRUCTOR PUBLIC BE" tableName "():" SKIP
            "        DataAccess = CAST (GetInstance (~"" daClassName "~"), " daClassName ")." SKIP.
        IF NOT com.mrg.framework.util.Util:IsEmpty (mandatoryFieldList) THEN
            PUT STREAM sOutput UNFORMATTED 
                "        AddMandatoryFields (~"" mandatoryFieldList "~")." SKIP.
        PUT STREAM sOutput UNFORMATTED            
            "    END CONSTRUCTOR." SKIP (1)    
            "    DESTRUCTOR PUBLIC BE" tableName "():" SKIP
            "        UnloadInstance (DataAccess)." SKIP
            "    END DESTRUCTOR." SKIP (1)                
            "END CLASS." SKIP.    
        
        OUTPUT STREAM sOutput CLOSE.
        
        edLog:insert-string ("Done.~n").
        
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateDA C-Win 
PROCEDURE GenerateDA :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER tableName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER rootFolder AS CHARACTER.
    DEFINE INPUT PARAMETER relativeFolder AS CHARACTER.
    
    DEFINE VARIABLE fullFileName AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE fileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE className AS CHARACTER NO-UNDO.
    DEFINE VARIABLE datasetName AS CHARACTER NO-UNDO.
    
    ASSIGN
        className = RIGHT-TRIM (REPLACE (com.mrg.framework.util.FileSystem:UnixName (relativeFolder), "~/", "."), ".") + ".data.DA" + tableName
        datasetName = REPLACE (REPLACE (className, ".data.DA", ".bl.ds"), ".", "~/") + ".i"
        fileName = SUBSTITUTE ("DA&1.cls", tableName)
        fullFileName = com.mrg.framework.util.FileSystem:BuildFolderName(rootFolder, relativeFolder, "data").
    
    IF NOT com.mrg.framework.util.FileSystem:FolderExist(fullFileName) THEN
        com.mrg.framework.util.FileSystem:CreateFolder (fullFileName).
    
    fullFileName = com.mrg.framework.util.FileSystem:BuildFolderName (fullFileName, fileName).        
    
    DO WITH FRAME {&frame-name}:
        edLog:MOVE-TO-EOF ().
        edLog:INSERT-STRING ("Generating " + fullFileName + "...").
                
        OUTPUT STREAM sOutput To VALUE (fullFileName).
        PUT STREAM sOutput UNFORMATTED
            "/**************************************************************************" SKIP
            " * File        : " fileName SKIP
            " * Purpose     : " SKIP
            " * Syntax      : " SKIP
            " * Description : " SKIP
            " * Author(s)   : Generated by the ObjectGenerator" SKIP
            " * Created     : " STRING (NOW) SKIP
            " * Notes       : " SKIP
            " **************************************************************************/" SKIP (1)                              
            "USING Progress.Lang.*." SKIP(1)            
            "ROUTINE-LEVEL ON ERROR UNDO, THROW." SKIP(1)            
            "CLASS " className " INHERITS com.mrg.framework.data.DataAccess:" SKIP (1)                 
            "    ~{" datasetName " ~&REFERENCE-ONLY=REFERENCE-ONLY~}" SKIP (1)
            "    METHOD PUBLIC OVERRIDE LOGICAL AttachDataSource():"  SKIP   
            "        IF NOT AddDataSource(~"tt" tableName "~", ~"" tableNAme "~") THEN" SKIP
            "            RETURN FALSE." SKIP
            "        RETURN SUPER:AttachDataSource()." SKIP    
            "    END METHOD." SKIP (1)                            
            "END CLASS." SKIP.    
        
        OUTPUT STREAM sOutput CLOSE.
        
        edLog:INSERT-STRING ("Done.~n").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateDS C-Win 
PROCEDURE GenerateDS :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER databaseName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER tableName        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER rootFolder       AS CHARACTER.
    DEFINE INPUT PARAMETER relativeFolder   AS CHARACTER.
    
    DEFINE VARIABLE fullFileName            AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE fileName                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE tableFile               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fileBuffer              AS HANDLE NO-UNDO.
    DEFINE VARIABLE fieldBuffer             AS HANDLE NO-UNDO.
    DEFINE VARIABLE fieldQuery              AS HANDLE NO-UNDO.
    DEFINE VARIABLE indexFieldBuffer        AS HANDLE NO-UNDO.
    DEFINE VARIABLE indexFieldQuery         AS HANDLE NO-UNDO.
 
    ASSIGN
        fileName        = SUBSTITUTE ("ds&1.i", tableName)
        fullFileName    = com.mrg.framework.util.FileSystem:BuildFolderName(rootFolder, RIGHT-TRIM (com.mrg.framework.util.FileSystem:UnixName (relativeFolder), "/") + "/bl")
        tableFile       = RIGHT-TRIM (com.mrg.framework.util.FileSystem:UnixName (relativeFolder), "/") + "/bl/tt" + tableName + ".i". 
            
    IF NOT com.mrg.framework.util.FileSystem:FolderExist(fullFileName) THEN
        com.mrg.framework.util.FileSystem:CreateFolder (fullFileName).
    
    fullFileName = com.mrg.framework.util.FileSystem:BuildFolderName (fullFileName, fileName).        
    
    
    DO WITH FRAME {&frame-name} ON ERROR UNDO, THROW:
        
        /* generate dataset */
        edLog:MOVE-TO-EOF ().
        edLog:INSERT-STRING ("Generating " + fullFileName + "...").
                
        OUTPUT STREAM sOutput To VALUE (fullFileName).
        PUT STREAM sOutput UNFORMATTED
            "/**************************************************************************" SKIP
            " * File        : " fileName SKIP
            " * Purpose     : " SKIP
            " * Syntax      : " SKIP
            " * Description : " SKIP
            " * Author(s)   : Generated by the ObjectGenerator" SKIP
            " * Created     : " STRING (NOW) SKIP
            " * Notes       : " SKIP
            " **************************************************************************/" SKIP (1)                      
            "~{" tableFile " ~{~&*~}~}" SKIP (1)
            "DEFINE ~{~&Scope~} DATASET ds~{~&Prefix~}" tableName " ~{~&REFERENCE-ONLY~} FOR tt~{~&Prefix~}" tableName "." SKIP.            
        OUTPUT STREAM sOutput CLOSE.
        
        edLog:INSERT-STRING ("Done.~n").
        
        /* generate temp-table */
        assign
            fullFileName = replace (fullFileName, "bl/ds", "bl/tt")
            fullFileName = replace (fullFileName, "bl\ds", "bl/tt")
            fileName = SUBSTITUTE ("tt&1.i", tableName).
            
        edLog:MOVE-TO-EOF ().
        edLog:INSERT-STRING ("Generating " + fullFileName + "...").
        
        OUTPUT STREAM sOutput To VALUE (fullFileName).
        PUT STREAM sOutput UNFORMATTED
            "/**************************************************************************" SKIP
            " * File        : " fileName SKIP
            " * Purpose     : " SKIP
            " * Syntax      : " SKIP
            " * Description : " SKIP
            " * Author(s)   : Generated by the ObjectGenerator" SKIP
            " * Created     : " STRING (NOW) SKIP
            " * Notes       : " SKIP
            " **************************************************************************/" SKIP (1)                      
            "~&IF '~{~&BufferName~}' = '' OR '~{~&BufferName~}' = 'tt ~{~&Prefix~}" tableName "' ~&THEN" SKIP
            "    ~&SCOP BufferName tt~{~&Prefix~}" tableName SKIP
            "    ~&SCOP BeforeBuffer bt~{~&Prefix~}" tableName SKIP
            "~&ENDIF" SKIP
            "&IF '~{~&BeforeBuffer~}' = '' &THEN" SKIP
            "    &SCOP BeforeBuffer b{&BufferName}" SKIP
            "~&ENDIF" SKIP (1)            
            "DEFINE ~{~&Scope~} TEMP-TABLE ~{~&BufferName~} NO-UNDO ~{~&reference-only~} BEFORE-TABLE ~{~&BeforeBuffer~}" SKIP.
                        
        CREATE BUFFER fileBuffer FOR TABLE SUBSTITUTE ("&1._File", databaseName).
        fileBuffer:FIND-FIRST (SUBSTITUTE ("where &1._File._File-Name = &2", databaseName, QUOTER (tableName))).
        CREATE BUFFER fieldBuffer FOR TABLE SUBSTITUTE ("&1._Field", databaseName, tableName).
        CREATE QUERY fieldQuery.
        fieldQuery:SET-BUFFERS (fieldBuffer).
        fieldQuery:QUERY-PREPARE (SUBSTITUTE ("for each &1._field where &1._Field._File-Recid = &2 by &1._Field._Order", databaseName, fileBuffer:RECID)).
        fieldQuery:QUERY-OPEN ().
        fieldQuery:GET-FIRST (NO-LOCK).
        REPEAT WHILE fieldBuffer:AVAILABLE ON ERROR UNDO, THROW:
            PUT STREAM sOutput UNFORMATTED "    FIELD " fieldBuffer::_field-name " AS " CAPS (fieldBuffer::_data-type) SKIP.
            fieldQuery:GET-NEXT (NO-LOCK).                
        END.                
        fieldQuery:QUERY-CLOSE().
        
        CREATE BUFFER indexFieldBuffer FOR TABLE SUBSTITUTE ("&1._Index-Field", databaseName).
        CREATE QUERY indexFieldQuery.
        indexFieldQuery:SET-BUFFERS (indexFieldBuffer).
        indexFieldQuery:QUERY-PREPARE (SUBSTITUTE ("for each &1._Index-Field where &1._Index-Field._Index-Recid = &2", databaseName, fileBuffer::_Prime-Index)).
        indexFieldQuery:QUERY-OPEN ().
        indexFieldQuery:GET-FIRST (NO-LOCK).
        PUT STREAM sOutput UNFORMATTED
            "    INDEX XPK_Primary IS PRIMARY IS UNIQUE".
        REPEAT WHILE indexFieldBuffer:AVAILABLE ON ERROR UNDO, THROW:
            fieldBuffer:FIND-FIRST (SUBSTITUTE ("where recid (&1._Field) = &2", databaseName, indexFieldBuffer::_Field-Recid)).
            PUT STREAM sOutput UNFORMATTED " "  fieldBuffer::_field-name.
            indexFieldQuery:GET-NEXT (NO-LOCK).                
        END.                
        indexFieldQuery:QUERY-CLOSE ().
        PUT STREAM sOutput UNFORMATTED "." SKIP.
              
        OUTPUT STREAM sOutput CLOSE.

        edLog:INSERT-STRING ("Done.~n").
        
    END.
    
    FINALLY:
        DELETE OBJECT fileBuffer NO-ERROR.
        DELETE OBJECT fieldBuffer NO-ERROR.
        DELETE OBJECT fieldQuery NO-ERROR.
        DELETE OBJECT indexFieldBuffer NO-ERROR.
        DELETE OBJECT indexFieldQuery NO-ERROR.
    END FINALLY.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateSA C-Win 
PROCEDURE GenerateSA :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER tableName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER rootFolder AS CHARACTER.
    DEFINE INPUT PARAMETER relativeFolder AS CHARACTER.
    
    DEFINE VARIABLE fullFileName AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE fileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE className AS CHARACTER NO-UNDO.
    DEFINE VARIABLE siClassName AS CHARACTER NO-UNDO.
    
    ASSIGN
        className = RIGHT-TRIM (REPLACE (com.mrg.framework.util.FileSystem:UnixName (relativeFolder), "~/", "."), ".") + ".rest.SA" + tableName
        siClassName = REPLACE (className, ".rest.SA", ".si.SI")
        fileName = SUBSTITUTE ("SA&1.cls", tableName)
        fullFileName = com.mrg.framework.util.FileSystem:BuildFolderName(rootFolder, relativeFolder, "rest").
    
    IF NOT com.mrg.framework.util.FileSystem:FolderExist(fullFileName) THEN
        com.mrg.framework.util.FileSystem:CreateFolder (fullFileName).
    
    fullFileName = com.mrg.framework.util.FileSystem:BuildFolderName (fullFileName, fileName).        
    
    DO WITH FRAME {&frame-name}:
        edLog:MOVE-TO-EOF ().
        edLog:INSERT-STRING ("Generating " + fullFileName + "...").
                
        OUTPUT STREAM sOutput To VALUE (fullFileName).
        PUT STREAM sOutput UNFORMATTED
            "/**************************************************************************" SKIP
            " * File        : " fileName SKIP
            " * Purpose     : " SKIP
            " * Syntax      : " SKIP
            " * Description : " SKIP
            " * Author(s)   : Generated by the ObjectGenerator" SKIP
            " * Created     : " STRING (NOW) SKIP
            " * Notes       : " SKIP
            " **************************************************************************/" SKIP (1)                                 
            "USING Progress.Lang.*." SKIP(1)            
            "ROUTINE-LEVEL ON ERROR UNDO, THROW." SKIP(1)            
            "CLASS " className " INHERITS com.mrg.framework.system.rest.ServiceInterface:" SKIP (1)                 
            "    DEFINE PROTECTED VARIABLE si" tableNAme " AS " siClassName " NO-UNDO." SKIP (1)
            "    CONSTRUCTOR PUBLIC SA" tableName "():" SKIP
            "        si" tableName ' = CAST (GetInstance ("' siClassNAme '"), ' siClassName ")." SKIP
            "    END CONSTRUCTOR." SKIP (1)    
            "    DESTRUCTOR PUBLIC SA" tableName "():" SKIP
            "        UnloadInstance (si" tableName ")." SKIP
            "    END DESTRUCTOR." SKIP (1)                
            "    METHOD PUBLIC VOID FetchData (jsonFilter AS CHARACTER, OUTPUT DATASET-HANDLE dsDataset):" SKIP
            "        si" tableName ":FetchData (ConvertRequest (jsonFilter), OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE)." SKIP (1)
            "        /* catch any error thrown during the service request */ " SKIP 
            "        CATCH appError AS Progress.Lang.Error: " SKIP
            "           /* TO DO: add code here to return errors on the client */" SKIP
            "           MESSAGE appError:GetMessage(1)." SKIP
            "           DELETE OBJECT appError." SKIP
            "        END CATCH." SKIP (1)
            "    END METHOD." SKIP (1)
            "    METHOD PUBLIC VOID StoreData (INPUT-OUTPUT DATASET-HANDLE dsDataset):" SKIP
            "        si" tableName ":StoreData (INPUT-OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE)." SKIP (1)
            "        /* catch any error thrown during the service request */ " SKIP 
            "        CATCH appError AS Progress.Lang.Error: " SKIP
            "           /* TO DO: add code here to return errors on the client */" SKIP
            "           MESSAGE appError:GetMessage(1)." SKIP
            "           DELETE OBJECT appError." SKIP
            "        END CATCH." SKIP (1)
            "    END METHOD." SKIP (1)            
            "END CLASS." SKIP.    
        
        OUTPUT STREAM sOutput CLOSE.
        
        edLog:INSERT-STRING ("Done.~n").
    END.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateSI C-Win 
PROCEDURE GenerateSI :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER tableName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER rootFolder AS CHARACTER.
    DEFINE INPUT PARAMETER relativeFolder AS CHARACTER.
    
    DEFINE VARIABLE fullFileName AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE fileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE className AS CHARACTER NO-UNDO.
    DEFINE VARIABLE beClassName AS CHARACTER NO-UNDO.
    
    ASSIGN
        className = RIGHT-TRIM (REPLACE (com.mrg.framework.util.FileSystem:UnixName (relativeFolder), "~/", "."), ".") + ".si.SI" + tableName
        beClassName = REPLACE (className, ".si.SI", ".bl.BE")
        fileName = SUBSTITUTE ("SI&1.cls", tableName)
        fullFileName = com.mrg.framework.util.FileSystem:BuildFolderName(rootFolder, relativeFolder, "si").
    
    IF NOT com.mrg.framework.util.FileSystem:FolderExist(fullFileName) THEN
        com.mrg.framework.util.FileSystem:CreateFolder (fullFileName).
    
    fullFileName = com.mrg.framework.util.FileSystem:BuildFolderName (fullFileName, fileName).        
    
    DO WITH FRAME {&frame-name}:
        edLog:MOVE-TO-EOF ().
        edLog:INSERT-STRING ("Generating " + fullFileName + "...").
                
        OUTPUT STREAM sOutput To VALUE (fullFileName).
        PUT STREAM sOutput UNFORMATTED
            "/**************************************************************************" SKIP
            " * File        : " fileName SKIP
            " * Purpose     : " SKIP
            " * Syntax      : " SKIP
            " * Description : " SKIP
            " * Author(s)   : Generated by the ObjectGenerator" SKIP
            " * Created     : " STRING (NOW) SKIP
            " * Notes       : " SKIP
            " *************************************************************************/" SKIP (1)                               
            "USING Progress.Lang.*." SKIP
            "USING com.mrg.framework.system.RequestContext." SKIP (1)            
            "ROUTINE-LEVEL ON ERROR UNDO, THROW." SKIP(1)            
            "CLASS " className " INHERITS com.mrg.framework.system.rest.ServiceInterface:" SKIP (1)                 
            "    DEFINE PROTECTED VARIABLE be" tableNAme " AS " beClassName " NO-UNDO." SKIP (1)
            "    CONSTRUCTOR PUBLIC SI" tableName "():" SKIP
            "        be" tableName ' = CAST (GetInstance ("' beClassName '"), ' beClassName ")." SKIP
            "    END CONSTRUCTOR." SKIP (1)    
            "    DESTRUCTOR PUBLIC SI" tableName "():" SKIP
            "        UnloadInstance (be" tableName ")." SKIP
            "    END DESTRUCTOR." SKIP (1)                
            "    METHOD PUBLIC VOID FetchData (jsonFilter AS CHARACTER, OUTPUT DATASET-HANDLE dsDataset):" SKIP     
            "        IF ActivateService (~"be" tableName ":FetchData~":U, ?) THEN" SKIP
            "            Be" tableName ":FetchData (ConvertRequest (jsonFilter), OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE)." SKIP
            "        DeactivateService (~"be" tableName ":FetchData~":U, requestContext)." SKIP   
            "        /* catch any error thrown during the service request */ " SKIP (1)
            "        CATCH appError AS Progress.Lang.Error: " SKIP
            "           /* TO DO: add code here to return errors on the client */" SKIP
            "           MESSAGE appError:GetMessage(1)." SKIP
            "           DELETE OBJECT appError." SKIP
            "        END CATCH." SKIP (1)                            
            "    END METHOD." SKIP(1)                
            "    METHOD PUBLIC LOGICAL StoreData (INPUT-OUTPUT DATASET-HANDLE dsDataset):" SKIP
            "        DEFINE VARIABLE lOk AS LOGICAL NO-UNDO." SKIP
            "        IF ActivateService (~"be" tableName ":FetchData~":U, ?) THEN" SKIP 
            "            lOk = be" tableName ":StoreData (INPUT-OUTPUT DATASET-HANDLE dsDataset BY-REFERENCE)." SKIP
            "        DeactivateService (~"be" tableName ":FetchData~":U, ?)." SKIP     
            "        RETURN lOk." SKIP (1)
            "        /* catch any error thrown during the service request */ " SKIP 
            "        CATCH appError AS Progress.Lang.Error: " SKIP
            "           /* TO DO: add code here to return errors on the client */" SKIP
            "           MESSAGE appError:GetMessage(1)." SKIP
            "           DELETE OBJECT appError." SKIP
            "        END CATCH." SKIP (1)
            "    END METHOD." SKIP           
            "END CLASS." SKIP.    
        
        OUTPUT STREAM sOutput CLOSE.
        
        edLog:INSERT-STRING ("Done.~n").
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitObject C-Win 
PROCEDURE InitObject :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dbIndex AS INTEGER NO-UNDO.
    DEFINE VARIABL MDMConnected AS LOGICAL NO-UNDO. 
    DO WITH FRAME {&FRAME-NAME}:
        DO dbIndex = 1 TO NUM-DBS:
            cmbDbs:ADD-LAST (LDBNAME (dbIndex)).
            IF LDBNAME (dbIndex) = "mdm" THEN
                 MDMConnected = YES.
        END.  
    END.
    
    IF MDMConnected THEN
    DO:
        cmbDbs:SCREEN-VALUE = "mdm".
        APPLY "VALUE-CHANGED" TO cmbDbs.
    END.        
        
    fillRoot:SCREEN-VALUE = ENTRY (1, PROPATH).
    fillRelative:SCREEN-VALUE = "com\mrg\mdm".
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetMandatoryFieldList C-Win 
FUNCTION GetMandatoryFieldList RETURNS CHARACTER
  ( databaseName AS CHARACTER, tableName AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE fieldList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fileBuffer AS HANDLE NO-UNDO.
    DEFINE VARIABLE fieldBuffer AS HANDLE NO-UNDO.
    DEFINE VARIABLE fieldQuery AS HANDLE NO-UNDO. 
    
    CREATE BUFFER fileBuffer FOR TABLE SUBSTITUTE ("&1._File", databaseName).
    fileBuffer:FIND-FIRST (SUBSTITUTE ("where &1._File._File-Name = &2", databaseName, QUOTER (tableName))).
    CREATE BUFFER fieldBuffer FOR TABLE SUBSTITUTE ("&1._Field", databaseName, tableName).
    CREATE QUERY fieldQuery.
    fieldQuery:SET-BUFFERS (fieldBuffer).
    fieldQuery:QUERY-PREPARE (SUBSTITUTE ("for each &1._field where &1._Field._File-Recid = &2 and &1._Field._Mandatory = yes by &1._Field._Order", databaseName, fileBuffer:RECID)).
    fieldQuery:QUERY-OPEN ().
    fieldQuery:GET-FIRST (NO-LOCK).
    REPEAT WHILE fieldBuffer:AVAILABLE ON ERROR UNDO, THROW:
        fieldList = fieldList + (IF fieldList = "" THEN "" ELSE ",") + fieldBuffer::_field-name. 
        fieldQuery:GET-NEXT (NO-LOCK).                
    END.                
    fieldQuery:QUERY-CLOSE().
        
        RETURN fieldList.
    
    FINALLY:
        DELETE OBJECT fileBuffer NO-ERROR.
        DELETE OBJECT fieldQuery NO-ERROR. 
        DELETE OBJECT fieldBuffer NO-ERROR.
    END FINALLY.
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

