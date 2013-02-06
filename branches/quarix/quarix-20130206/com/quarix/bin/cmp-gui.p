/*------------------------------------------------------------------------
    File        : cmp-gui.p
    Purpose     : Compile generated ProJapi XML templates.

    Syntax      :

    Description : QuariX Template Compiler

    Author(s)   : Marian Edu
    Created     : 10.11.2006
    Notes       : QuariX project (contributed from ganimede open-source project)
    License     :
    This file is part of the QRX-SRV-OE software framework.
    Copyright (C) 2011, SC Yonder SRL (http://www.tss-yonder.com)

    The QRX-SRV-OE software framework is free software; you can redistribute
    it and/or modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either version 2.1
    of the License, or (at your option) any later version.

    The QRX-SRV-OE software framework is distributed in the hope that it will
    be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
    General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with the QRX-SRV-OE software framework; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA or on the internet at the following address:
    http://www.gnu.org/licenses/lgpl-2.1.txt
  ----------------------------------------------------------------------*/
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&GLOBAL-DEFINE SOURCE_FILES     'xml,htm,html':U

&GLOBAL-DEFINE SS_PREPROCESSOR com/quarix/bin/cmphtml.p

DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStr       AS CHARACTER  NO-UNDO   EXTENT 2.

DEFINE STREAM _sFileIn.


DEFINE TEMP-TABLE tt_files NO-UNDO
    FIELD f_name        AS CHARACTER
    FIELD f_dir         AS CHARACTER
    FIELD f_embedded    AS LOGICAL
    FIELD f_include     AS LOGICAL
    FIELD f_wsoptions   AS CHARACTER
    INDEX idx_file      f_include DESCENDING
                        f_dir f_name.

DEFINE TEMP-TABLE tt_cmpmsg NO-UNDO
    FIELD f_file        AS CHARACTER
    FIELD f_error       AS CHARACTER
    INDEX idx_file      f_file.

&SCOPED-DEFINE WINDOW-NAME CmpWin
&SCOPED-DEFINE FRAME-NAME DEFAULT-FRAME

/* ************************  Function Prototypes ********************** */
FUNCTION GetFileFormat RETURNS LOGICAL
  ( INPUT   cFileName       AS CHARACTER,
    OUTPUT  cFileBaseName   AS CHARACTER,
    OUTPUT  cFileExtension  AS CHARACTER)  FORWARD.

/* Define the widget handle for the window                              */
DEFINE VAR CmpWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_Browse
     LABEL "Browse"
     SIZE 15 BY 1.

DEFINE BUTTON btn_Close
     LABEL "Close"
     SIZE 15 BY 1.

DEFINE BUTTON btn_Compile
     LABEL "Compile"
     SIZE 15 BY 1.

DEFINE BUTTON btn_Path
     LABEL "Propath"
     SIZE 15 BY 1.

DEFINE BUTTON btn_Connect
     LABEL "Databases"
     SIZE 15 BY 1.

DEFINE VARIABLE edt_err AS CHARACTER
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 70 BY &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN 9 &ELSE 5 &ENDIF NO-UNDO.

DEFINE VARIABLE fill_Dir AS CHARACTER FORMAT "X(256)":U
     LABEL "Directory"
     VIEW-AS FILL-IN
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE fill_Base AS CHARACTER FORMAT "X(256)":U
     LABEL "Class root"
     VIEW-AS FILL-IN
     SIZE 41 BY 1 NO-UNDO.


DEFINE VARIABLE fill_Mask AS CHARACTER FORMAT "X(256)":U
     LABEL "File mask"
     VIEW-AS FILL-IN
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE tgl_Recursive AS LOGICAL INITIAL no
     LABEL "Recursive"
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fill_Dir HELP "Enter directory..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 5
          &ELSE AT ROW 2 COL 6 &ENDIF
     btn_Browse HELP "Browse for directory..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 58
          &ELSE AT ROW 1.95 COL 58 &ENDIF

     fill_Base HELP "Set application class root..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 5
          &ELSE AT ROW 3.38 COL 5.2 &ENDIF

     fill_Mask HELP "Enter file mask..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 5
          &ELSE AT ROW 5.38 COL 5.6 &ENDIF
     tgl_Recursive
          HELP "Compile directory recursive..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 44
          &ELSE AT ROW 6.81 COL 42 &ENDIF
     btn_Path
          HELP "Edit Propath..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 58
          &ELSE AT ROW 5.38 COL 58 &ENDIF
     btn_Connect
          HELP "Connect database..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 58
          &ELSE AT ROW 6.81 COL 58 &ENDIF
     btn_Compile
          HELP "Start compile..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 21 COL 58
          &ELSE AT ROW 13.5 COL 58 &ENDIF
     btn_Close
          HELP "Quit..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 21 COL 42
          &ELSE AT ROW 13.5 COL 42 &ENDIF
     edt_err
          HELP "Compiler results..."
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 5
          &ELSE AT ROW 8.3 COL 3 &ENDIF NO-LABEL
   WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN
         TITLE "quarix - xmlCompiler"
         CENTERED
         AT COL 1 ROW 1
         SIZE 80 BY 30.
         &ELSE
         NO-BOX
         AT COL 1 ROW 1
         SIZE 80 BY 30.
         &ENDIF


IF NOT SESSION:BATCH-MODE THEN DO:
  &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN
    ASSIGN
        {&WINDOW-NAME} = CURRENT-WINDOW
        FRAME DEFAULT-FRAME:VIRTUAL-HEIGHT = 21
        FRAME DEFAULT-FRAME:VIRTUAL-WIDTH  = 80.
  &ELSE
    CREATE WINDOW CmpWin ASSIGN
           HIDDEN             = YES
           TITLE              = "quarix - xmlCompiler"
           HEIGHT             = 13.67
           WIDTH              = 74.2
           MAX-HEIGHT         = 16
           MAX-WIDTH          = 80
           VIRTUAL-HEIGHT     = 16
           VIRTUAL-WIDTH      = 80
           RESIZE             = YES
           SCROLL-BARS        = NO
           STATUS-AREA        = YES
           BGCOLOR            = ?
           FGCOLOR            = ?
           KEEP-FRAME-Z-ORDER = YES
           THREE-D            = YES
           MESSAGE-AREA       = NO
           SENSITIVE          = YES.

    IF VALID-HANDLE(CmpWin) THEN CmpWin:HIDDEN = NO.
  &ENDIF

  ASSIGN
    edt_err:READ-ONLY IN FRAME DEFAULT-FRAME = TRUE.
END.

/* ************************  Control Triggers  ************************ */

IF NOT SESSION:BATCH-MODE THEN DO:
  ON END-ERROR OF CmpWin
  OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  END.

  ON WINDOW-CLOSE OF CmpWin
  DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

  ON CHOOSE OF btn_Close IN FRAME DEFAULT-FRAME
  DO:
      &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
          APPLY "WINDOW-CLOSE":U TO CURRENT-WINDOW.
      &ELSE
          QUIT.
      &ENDIF
  END.

  ON CHOOSE OF btn_Compile IN FRAME DEFAULT-FRAME
  DO:
      SESSION:SET-WAIT-STATE('GENERAL':U).
      RUN CompileDirectory(fill_Dir:SCREEN-VALUE,
                           fill_Base:SCREEN-VALUE,
                           fill_Mask:SCREEN-VALUE,
                           tgl_Recursive:CHECKED,
                           'template').
      SESSION:SET-WAIT-STATE('':U).
  END.

  ON CHOOSE OF btn_Path IN FRAME DEFAULT-FRAME
  DO:
      /* hope this works, if not i'll have to write my own :((  */
      cStr[1] = REPLACE(PROPATH,';':U,',':U).
      RUN adecomm/_modpath.w (cStr[1],TRUE,
                              INPUT-OUTPUT cStr[2],
                              OUTPUT cStr[1]).
      IF cStr[2] NE '':U THEN PROPATH = cStr[1].
  END.

  ON CHOOSE OF btn_Browse IN FRAME DEFAULT-FRAME
  DO:
      /* to be implemented... lack of time right now            */
    DEFINE VARIABLE oServer  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE oFolder  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE oParent  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE sTitle   AS CHARACTER  NO-UNDO.

    CREATE 'Shell.Application' oServer.

    oFolder = oServer:BrowseForFolder(CURRENT-WINDOW:HWND,'Select Folder',0).
    IF VALID-HANDLE(oFolder) = TRUE THEN DO:

      ASSIGN
        oParent = oFolder:ParentFolder
        sTitle  = oFolder:Title
        iCount  = oParent:Items:Count NO-ERROR.

      IF iCount GT 0 THEN DO:
        CASE sTitle:
          WHEN "Recycle Bin" OR
            WHEN "My Documents" OR
            WHEN "My Computer" OR
            WHEN "My Network Places" THEN DO:
            MESSAGE 'Invalid selection!' VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
          END.
          OTHERWISE
            sTitle = oFolder:ParentFolder:ParseName(sTitle):Path.
        END CASE.
        fill_Dir:SCREEN-VALUE = sTitle.
        apply 'value-changed':u to fill_Dir.
      END.
    END.

    RELEASE OBJECT oParent  NO-ERROR.
    RELEASE OBJECT oFolder  NO-ERROR.
    RELEASE OBJECT oServer  NO-ERROR.
  END.

  ON CHOOSE OF btn_Connect IN FRAME DEFAULT-FRAME
  DO:
      DEFINE VARIABLE lg  AS LOGICAL NO-UNDO.
      RUN protools/_dblist.p.
  END.

  ON VALUE-CHANGED OF fill_Dir IN FRAME DEFAULT-FRAME
  DO:
     fill_Base:SCREEN-VALUE = fill_Dir:SCREEN-VALUE.
  END.
END.

/* ***************************  Main Block  *************************** */

SUBSCRIBE TO 'CompilerMsg':U ANYWHERE RUN-PROCEDURE 'TrapCompilerMsg':U.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
   RUN disable_UI.

IF NOT SESSION:BATCH-MODE THEN DO:
  /* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.      */
  ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
         THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

  /* Best default for GUI applications is...                            */
  PAUSE 0 BEFORE-HIDE.

  /* Now enable the interface and wait for the exit condition.          */
  /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.  */
  MAIN-BLOCK:
  DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
     ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.
END.


/* **********************  Internal Procedures  *********************** */

PROCEDURE BuildFileList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cDir         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cFileMask    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER lRecursive   AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cFile       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cType       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFullPath   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMaskName   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMaskExt    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFileName   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFileExt    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDirList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iNum        AS INTEGER    NO-UNDO.

    FILE-INFO:FILE-NAME = cDir.
    IF FILE-INFO:FILE-TYPE = ? OR
        INDEX(FILE-INFO:FILE-TYPE,'D':U) = 0 OR
        INDEX(FILE-INFO:FILE-TYPE,'R':U) = 0 THEN RETURN.

    GetFileFormat(cFileMask,OUTPUT cMaskName, OUTPUT cMaskExt).

    INPUT STREAM _sFileIn FROM OS-DIR(cDir).
    REPEAT:
        IMPORT STREAM _sFileIn cFile cFullPath cType.
        IF INDEX(cType,'D') > 0 THEN DO:
            IF lRecursive AND TRIM(cFile,'.':U) NE '':U THEN
                cDirList = cDirList + CHR(1) + cFullPath.
            NEXT.
        END.

        /* check file mask          */
        GetFileFormat(cFile,OUTPUT cFileName, OUTPUT cFileExt).
        IF CAN-DO({&SOURCE_FILES},cFileExt) AND
            cFileName MATCHES cMaskName AND
            cFileExt MATCHES cMaskExt THEN DO:
            CREATE tt_files.
            ASSIGN
                tt_files.f_name     = cFullPath
                tt_files.f_dir      = cDir
                tt_files.f_embedded = CAN-DO('xml,htm,html':U,cFileExt).
            /* check to see if it's include or not  */
            IF tt_files.f_embedded THEN DO:
                cType = 'get-options':U.
                RUN {&SS_PREPROCESSOR} (cFullPath,
                                        INPUT-OUTPUT cFile,
                                        INPUT-OUTPUT cType) NO-ERROR.
                ASSIGN
                    tt_files.f_include   = CAN-DO(cType,'include')
                    tt_files.f_wsoptions = cType NO-ERROR.
            END.
        END.
    END.
    INPUT STREAM _sFileIn CLOSE.

    cDirList = TRIM(cDirList,CHR(1)).
    IF lRecursive THEN DO iNum = 1 TO NUM-ENTRIES(cDirList,CHR(1)):
        RUN BuildFileList(ENTRY(iNum,cDirList,CHR(1)),cFileMask,lRecursive).
    END.

END PROCEDURE.

PROCEDURE CompileDirectory :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cDir         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cBase        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cFileMask    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER lRecursive   AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER cWsOptions   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFile       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cWsOpt      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iNum        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cSaveDir    AS CHARACTER  NO-UNDO.

    /* build the list of files to be compiled   */
    EMPTY TEMP-TABLE tt_files NO-ERROR.
    RUN BuildFileList(cDir,cFileMask,lRecursive).

    IF NOT SESSION:BATCH-MODE THEN
      edt_err:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '':U.

    IF cWsOptions = 'default':U THEN cWsOptions = '':U.

    if cBase eq ? or index(cDir, cBase) eq 0 then
       cBase = cDir.

    FOR EACH tt_files:
        /* embedded speed script                */
        IF tt_files.f_embedded THEN DO:
            ASSIGN
                cFile   = tt_files.f_name
                cWsOpt  = cWsOptions.
            RUN {&SS_PREPROCESSOR} (tt_files.f_name,
                                    cBase,
                                    INPUT-OUTPUT cFile,
                                    INPUT-OUTPUT cWsOpt).
        END.
        /* progress source files                */
        ELSE DO:
            IF cSaveDir = '':U THEN
                COMPILE VALUE(tt_files.f_name) SAVE = TRUE NO-ERROR.
            ELSE
                COMPILE VALUE(tt_files.f_name) SAVE
                    INTO VALUE(cSaveDir) NO-ERROR.

            IF COMPILER:ERROR THEN DO:
                DO iNum = 1 TO ERROR-STATUS:NUM-MESSAGES:
                    CREATE tt_cmpmsg.
                    ASSIGN
                        tt_cmpmsg.f_file  = tt_files.f_name
                        tt_cmpmsg.f_error = ERROR-STATUS:GET-MESSAGE(iNum)
                        NO-ERROR.
                END.
            END.
            ELSE DO:
                CREATE tt_cmpmsg.
                tt_cmpmsg.f_file  = tt_files.f_name + ' - OK.':U.
            END.
        END.
    END.
    EMPTY TEMP-TABLE tt_files NO-ERROR.

    iNum = 0.
    FOR EACH tt_cmpmsg GROUP BY tt_cmpmsg.f_file:
        IF FIRST-OF(tt_cmpmsg.f_file) THEN DO:
            iNum = iNum + 1.
            IF SESSION:BATCH-MODE THEN
              PUT UNFORMATTED STRING(iNum) '.~t':U tt_cmpmsg.f_file SKIP.
            ELSE DO:
              edt_err:MOVE-TO-EOF() NO-ERROR.
              edt_err:INSERT-STRING(STRING(iNum) + '.~t':U
                                    + tt_cmpmsg.f_file + '~n':U).
            END.
        END.
        IF INDEX(tt_cmpmsg.f_file,' - OK.':U) > 0 THEN NEXT.
        IF SESSION:BATCH-MODE THEN
          PUT UNFORMATTED '~t':U tt_cmpmsg.f_error SKIP.
        ELSE DO:
          edt_err:MOVE-TO-EOF() NO-ERROR.
          edt_err:INSERT-STRING('~t':U + tt_cmpmsg.f_error + '~n':U).
        END.
    END.
    IF SESSION:BATCH-MODE THEN PUT UNFORMATTED SKIP(2).
    EMPTY TEMP-TABLE tt_cmpmsg NO-ERROR.
END PROCEDURE.

PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CmpWin)
  THEN DELETE WIDGET CmpWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fill_Dir fill_Mask tgl_Recursive edt_err
      WITH FRAME DEFAULT-FRAME IN WINDOW CmpWin.
  ENABLE btn_Browse fill_Dir fill_Base fill_Mask btn_Compile tgl_Recursive
         btn_Close btn_Path btn_Connect edt_err
      WITH FRAME DEFAULT-FRAME IN WINDOW CmpWin.
  VIEW CmpWin.
END PROCEDURE.

PROCEDURE TrapCompilerMsg :
/*------------------------------------------------------------------------------
  Purpose:     Trap compiler errors from embedded script preprocessor
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cMessageLine AS CHARACTER.

    DEFINE VARIABLE cFile   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iNum    AS INTEGER    NO-UNDO.

    ASSIGN
        cFile = ENTRY(1,cMessageLine,CHR(1))
        iNum  = R-INDEX(cFile,'-':U)
        iNum  = IF iNum > 0 THEN iNum - 1 ELSE 0
        cFile = SUBSTRING(cFile,1,iNum,'CHARACTER':U) NO-ERROR.
    IF NUM-ENTRIES(cMessageLine,CHR(1)) > 1 THEN
        DO iNum = 2 TO NUM-ENTRIES(cMessageLine,CHR(1)):
        CREATE tt_cmpmsg.
        ASSIGN
            tt_cmpmsg.f_file  = cFile
            tt_cmpmsg.f_error = ENTRY(iNum,cMessageLine,CHR(1)) NO-ERROR.
    END.
    ELSE DO:
        CREATE tt_cmpmsg.
        tt_cmpmsg.f_file  = cFile + ' - OK.':U.
    END.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION GetFileFormat RETURNS LOGICAL
  ( INPUT   cFileName       AS CHARACTER,
    OUTPUT  cFileBaseName   AS CHARACTER,
    OUTPUT  cFileExtension  AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iPos    AS INTEGER    NO-UNDO.

    ASSIGN
        iPos            = R-INDEX(cFileName,'.':U)
        cFileExtension  = SUBSTRING(cFileName,iPos + 1,-1,'CHARACTER':U)
        iPos            = IF iPos > 0 THEN iPos - 1 ELSE 0
        cFileBaseName   = SUBSTRING(cFileName,1,iPos,'CHARACTER':U)
        NO-ERROR.

    RETURN TRUE.

END FUNCTION.


