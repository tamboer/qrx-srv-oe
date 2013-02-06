/*------------------------------------------------------------------------
    File        : cmphtml.p
    Purpose     : Ganimede Studio - embedded html file compiler

    Syntax      : cmphtml(FileName, DemoMode).

    Description : This is used to compile html embedded files
                  For now only '<!--WSS' tag are supported,
                  you can change the tags to suit your needs

    Parameters  : cInFile    - file name with html embedded source code
                  cOutFile   - file name with resulting progress source
                  cWsOptions - embedded script options

    Author(s)   : Marian EDU <medu@users.sourceforge.net>
			      M. Totham  <mjtweb@users.sourceforge.net>

    Created     : 29.11.2002
    Updated     : 02.02.2003 medu - support for other webSpeed tags
                             <SCRIPT, <SERVER>, <!--WSE, <!--WS4GL, <?WS>
                             <%=, <%, "`", {=
                  27.03.2003 medu - support for embedded META tags
			      			10.06.2003 mjtweb - SpeedScript fix for "<SCRIPT" meta-tag
                  04.10.2003 medu - fixed new-line striping from html code
                  27.03.2004 medu - removed new-line from generated code
                                    when expresion

    Notes       : Ganimede project
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
/* ***************************  Definitions  ************************** */
define input        parameter cInFile     as character no-undo.
define input        parameter cClsRoot    as character no-undo.
define input-output parameter cOutFile    as character no-undo.
define input-output parameter cWsOptions  as character no-undo.

define variable cCmpFile      as character  no-undo.
define variable cTmpFile      as character  no-undo.
define variable cLine         as character  no-undo.
define variable lOutput       as logical    no-undo.
define variable lScriptBlock  as logical    no-undo.
define variable iLine         as integer    no-undo.
define variable iCount        as integer    no-undo.
define variable jCount        as integer    no-undo.
define variable iExprTag      as integer    no-undo.
define variable cEndTag       as character  no-undo.
define variable cContentType  as character  no-undo.
define variable lHeadSection  as logical    no-undo initial true.

define variable hXmlDoc       as handle    no-undo.
define variable hRootNode     as handle    no-undo.
define variable hNode         as handle    no-undo.
define variable hNodeReqPaint as handle    no-undo.
define variable cAttrVal      as character no-undo.

define stream _sCmpIn.
define stream _sCmpOut.
define stream _sProxy.

/* ************************* TAGS DEFINITION ************************** */
&GLOBAL-DEFIne SPD_BLK_BEGIN '<jscript><![CDATA[,<script,<SERVER>,<!--WSS,<!--WSE,<!--WS4GL,<?WS>,<%=,<%,`,':U + CHR(123) + '=':U
&GLOBAL-DEFIne SPD_BLK_END   ']]></jscript>,</script>,</SERVER>,-->,-->,-->,</?WS>,%>,%>,`,=':U + CHR(125)
&GLOBAL-DEFIne SPD_EXPR_TAG  '<!--WSE,<%=,<%,`,':U + CHR(123) + '=':U
&GLOBAL-DEFIne WSS_WSOPTION_TAG     'METANAME="wsoptions"CONTENT="':U
&GLOBAL-DEFIne WSS_HTTP_CONTENT_TAG '<METAHTTP-EQUIV="Content-Type"CONTENT="':U
&GLOBAL-DEFIne WSS_OUT_CODE    'Response:out( string(':U
&GLOBAL-DEFIne WSS_OUT_TEXT    'Response:out(  ~"':U

/* function's declarations                                              */
function GetFileBaseName  returns character
  (input cFile as character) forward.
function GetClassName     returns character
  (input cFile    as character,
   input cClsBase as character) forward.
function html-decode returns character
  (input pcString as character) forward.
function escape-output returns character
  (input pcString as character) forward.
function CheckWsMetaTags  returns logical
  (input        cString       as character,
   input-output cWsoption     as character,
   input-output cContentType  as character) forward.
function OutputScript     returns logical
  (input-output cString       as character,
   input-output cWsoption     as character,
   input-output cContentType  as character,
   input-output lHeadSection  as logical,
   lScriptBlock               as logical) forward.
function TranslateLine    returns logical
  (cLine                      as character,
   input-output cWsoption     as character,
   input-output cContentType  as character,
   input-output lHeadSection  as logical,
   lScriptBlock               as logical) forward.
function GetIanaEncode  returns character
  (input cAblEncode as character) forward.


/* **************** MAIN-BLOCK *********************** */
/* check for input file, if not found exit             */
assign
   cClsRoot = right-trim(replace(cClsRoot,chr(92),chr(47)), chr(47))
   file-info:file-name = cInFile.
if file-info:file-type = ? or
  index(file-info:file-type,'F':U) = 0 or
  index(file-info:file-type,'R':U) = 0 then do:
   publish 'CompilerMsg':U ( cInFile + ' - &2':U).
   return.
end.

/* Generate a temporary filename to write the file to. */
if not can-do(cWsOptions,'get-options':U) then do:
  run adecomm/_tmpfile.p ( '':U, '.cmp':U, output cCmpFile).
  output stream _sCmpOut to value(cCmpFile) unbuffered no-map no-convert.
end.

create x-document hXmlDoc.
hXmlDoc:encoding = GetIanaEncode(session:cpinternal).

hXmlDoc:load("file", cInFile, false) no-error.
if error-status:error or
   error-status:num-messages gt 0
   then do:
      publish 'CompilerMsg':U ( error-status:get-message(error-status:num-messages) + chr(1)).
   return.
end.
create x-noderef hRootNode.
hXmlDoc:get-document-element(hRootNode).


do iCount = 1 to hRootNode:num-children:
   create x-noderef hNode.
   if hRootNode:get-child(hNode, iCount) and
      hNode:name eq 'jscript':u and
      num-entries(hNode:attribute-names) ge 1 then do:
         cAttrVal = hNode:get-attribute('methodABL':u) no-error.
         hNode:get-child(hNode, 1).
         case cAttrVal:
            when 'class':u then do:
               put stream _sCmpOut unformatted hNode:node-value skip.
               hNode:get-parent(hNode).
               hRootNode:remove-child(hNode).
            end.
            when 'BeforePaint':u then do:
               put stream _scmpOut unformatted 'method overridee protected logical BeforePaint():' skip.
               put stream _sCmpOut unformatted hNode:node-value skip.
               put stream _scmpOut unformatted 'end method.' skip.
               hNode:get-parent(hNode).
               hRootNode:remove-child(hNode).
            end.
            when 'AfterPaint':u then do:
               put stream _scmpOut unformatted 'method override protected void AfterPaint():' skip.
               put stream _sCmpOut unformatted hNode:node-value skip.
               put stream _scmpOut unformatted 'end method.' skip.
               hNode:get-parent(hNode).
               hRootNode:remove-child(hNode).
            end.
            when 'HandleRequestPaint':u then do:
               create x-noderef hNodeReqPaint.
               hNode:clone-node(hNodeReqPaint, true).
               hNode:get-parent(hNode).
               hRootNode:remove-child(hNode).
           end.
         end case.
      end.
end.

run adecomm/_tmpfile.p ( '':U, '.tmp':U, output cTmpFile).
hXmlDoc:save('file', cTmpFile).

output to value(cTmpFile) append.
put unformatted "~n" skip(1).
output close.


input stream _sCmpIn from value(cTmpFile) unbuffered no-map no-convert.

put stream _sCmpOut unformatted 'method override public logical HandleRequestPaint ():' skip(2).

if valid-handle(hNodeReqPaint) then put stream _sCmpOut unformatted hNodeReqPaint:node-value skip.
/* translate each line                                */
repeat:
  import stream _sCmpIn unformatted cLine.
  if trim(cLine) = '':U then
    next.
  lScriptBlock = TranslateLine(cLine,
                               input-output cWsOptions,
                               input-output cContentType,
                               input-output lHeadSection,
                               lScriptBlock).
  if can-do(cWsOptions,'get-options':U) then do:
      if not lHeadSection then leave.
      next.
  end.

  if lScriptBlock then
    put stream _sCmpOut unformatted skip.
  else
    put stream _sCmpOut unformatted {&WSS_OUT_TEXT} + '~~n~").':U skip.
end.

input stream _sCmpIn close.

if can-do (cWsOptions, 'template':u) then do:
  put stream _sCmpOut unformatted
      'return true.' skip
      'end method.' skip
      'end class.' skip.
end.

if cWsOptions = '':U then
  cWsOptions = 'web-object':U.
else do:
  if not can-do(cWsOptions,'include':U) then
    cWsOptions = 'web-object,':U + cWsOptions.
  cWsOptions = cWsOptions + ',wsoptions-found':U.
end.

if can-do(cWsOptions,'get-options':U) then
  return.
else
  output stream _sCmpOut close.

if cContentType = '':U or
  not can-do(cWsOptions,'keep-meta-content-type':U) then
  cContentType = 'text/html':U.

if cOutFile = ? or cOutFile = '':U then
  cOutFile = cInFile.
if can-do(cWsOptions,'include':U) then
    cOutFile = substitute('&1.i':U, cOutFile).
else if can-do(cWsOptions,'template':U) then
    cOutFile = substitute('&1.cls':U, GetFileBaseName(cOutFile)).
else
    cOutFile = substitute('&1.w':U, GetFileBaseName(cOutFile)).

output stream _sCmpOut to value(cOutFile) unbuffered no-convert.
put stream _sCmpOut unformatted
  '/******************************************************************':U skip
  '**** File: ':U cOutFile skip
  '**** Generated on: ' string(today,'99/99/9999':U)
  ' ':U string(time,'HH:MM:SS':U) skip
  '**** Source file: ':U cInFile skip
  '**** Options: ':U cWsOptions skip
  '****':U skip
  '**** WARNING: do not EDIT THIS FILE. ':U
  'Make changes to the original':U skip
  '**** template file and regenerate this file from it.':U skip
  '******************************************************************/':U skip(1).

if not can-do(cWsOptions,'include':U) and not can-do(cWsOptions,'template':U) then do:
  put stream _sCmpOut unformatted
      '/*********** Ganimede embedded speed script includes *************/':U skip
      '~{src/web2/wrap-cgi.i~}':U skip.
  if not can-do(cWsOptions,'no-content-type':U) then
    put stream _sCmpOut unformatted
      '~{src/web2/e4gl.i~}':U skip
      'output-content-type("':U cContentType '").' skip.
  put stream _sCmpOut unformatted skip
    '/************** Embedded speed script start here *****************/':U skip(1).
end.

/* if can-do (cWsOptions, 'template':u) then do:                  */
/*   put stream _sCmpOut unformatted                              */
/*     '~{vendor~/nethrom~/quarix~/inc~/qrxscript.i~}'  skip (2). */



/*put stream _sCmpOut unformatted substitute('class &1 inherits com.quarix.web.WebObject:', GetClassName(cInFile, cClsRoot)) skip.*/
cAttrVal = ''.
if valid-handle(hRootNode) and
   num-entries(hRootNode:attribute-names) ge 1
   then cAttrVal = hRootNode:get-attribute('classABL':u) no-error.

put stream _sCmpOut unformatted substitute('class &1 inherits &2:', GetClassName(cInFile, cClsRoot), if cAttrVal gt '' then cAttrVal else 'com.quarix.web.WebObject':u) skip.

output stream _sCmpOut close.
os-append value(cCmpFile) value(cOutFile).
os-delete value(cCmpFile).

delete object hXmlDoc no-error.
delete object hNode no-error.
delete object hRootNode no-error.
delete object hNodeReqPaint no-error.

if can-do(cWsOptions,'include':U) or
  can-do(cWsOptions,'no-compile':U) or
  can-do(cWsOptions,'template':U) then return.

compile value(cOutFile) save = true no-error.
/* get compiler errors if any                                     */
if compiler:error then do:
  do iLine = 1 to error-status:num-messages:
    cLine = cLine + chr(1) + error-status:get-message(iLine).
  end.
  entry(1,cLine,chr(1)) = cInFile + ' - &2':U.
  publish 'CompilerMsg':U (cLine).
end.
else publish 'CompilerMsg':U (cInFile + ' - &1':U).

/* ******************* end Main-Block *************************** */


/* function's implementation                                      */
function GetFileBaseName return character (cFile as character):
/*------------------------------------------------------------------------------
  Purpose:  Get file name including full-path, excluding only extension
    Notes:
------------------------------------------------------------------------------*/
  assign
    cFile = replace(cFile,chr(92),chr(47))
    cFile = substring(cFile,1,
                      length(cFile,'character':U) - 1 -
                      length(entry(num-entries(cFile,'.':U),cFile,'.':U),'character':U),
                      'character':U) no-error.
  return cFile.
end function.

function html-decode returns character
  (input pcString as character /* string to be html decoded */):
/*------------------------------------------------------------------------------
  Purpose:  Encode the html tags from string
    Notes:
------------------------------------------------------------------------------*/

    assign
        pcString = replace(pcString, "&amp~;":U, chr(38))   /* "@" */
        pcString = replace(pcString, "&quot~;":U, chr(34))  /* <"> */
        pcString = replace(pcString, "&lt~;":U, chr(60))    /* "<" */
        pcString = replace(pcString, "&gt~;":U, chr(62)).   /* ">" */
    return pcString.
end function. /* html-encode */

function escape-output returns character
  (input pcString as character):
/*------------------------------------------------------------------------------
  Purpose:  Escape Progress special characters for output
    Notes:
------------------------------------------------------------------------------*/

    assign
        pcString = replace(pcString,chr(126),chr(126) + chr(126))
        pcString = replace(pcString,chr(34),chr(126) + chr(34))
        pcString = replace(pcString,chr(92),chr(126) + chr(92))
        pcString = replace(pcString,chr(123),chr(126) + chr(123)).
    return pcString.
end function.

function CheckWsMetaTags  returns logical
  (input        cString       as character,
   input-output cWsoption     as character,
   input-output cContentType  as character):
/*------------------------------------------------------------------------------
  Purpose:  Check for embedded speed script META TAGS
    Notes:
------------------------------------------------------------------------------*/
   define variable iPos as integer extent 3 no-undo.
   define variable cStr as character  no-undo.

   /* check for content-type in code line                  */
   assign
     cString = replace(cString,' ':U,'':U)
     cString = replace(cString,'~t':U,'':U)
     cString = replace(cString,'~'':U,'~"':U)
     cString = trim(cString)
     iPos[1] = index(cString,{&WSS_WSOPTION_TAG})
     iPos[2] = index(cString,{&WSS_HTTP_CONTENT_TAG})
     iPos[3] = index(cString,'</HEAD>':U).
   /* META Tags count only before HTML content              */
   if iPos[3] = 0 then iPos[3] = index(cString,'<BODY>':U).

   /* WebSpeed MetaTags                                     */
   if iPos[1] > 0 and
     (iPos[3] = 0 or iPos[1] < iPos[3]) then assign
     cStr = substring(cString,
                      iPos[1] + length({&WSS_WSOPTION_TAG},
                                       'character':U),
                      -1,'character':U)
     cWsOption = trim(cWsOption + ',':U +
                      entry(1,cStr,'~"':U),',':U).
    /* HTTP MetaTags                                        */
    if iPos[2] > 0 and
      (iPos[3] = 0 or iPos[2] < iPos[3]) then do:
      if cContentType = '':U then assign
        cStr = substring(cString,
                         iPos[2] + length({&WSS_HTTP_CONTENT_TAG},
                                          'character':U),
                         -1,'character':U)
        cContentType = chr(1) + replace(entry(1,cStr,'~"':U),';':U,'; ':U).
      else cContentType = chr(1) + cContentType.
    end.

    /* end of HTML HEAD section... stop check here          */
    return iPos[3] = 0.
end function.

function TranslateLine    returns logical
  (cLine                      as character,
   input-output cWsoption     as character,
   input-output cContentType  as character,
   input-output lHeadSection  as logical,
   lScriptBlock               as logical):
/*------------------------------------------------------------------------------
  Purpose:  Translate each line form input embedded speed script file
    Notes:
------------------------------------------------------------------------------*/

  define variable cString as character  no-undo.

  assign cString = trim(cLine).
  repeat while cString ne '':U:
    lScriptBlock = OutputScript(input-output cString,
                                input-output cWsoption,
                                input-output cContentType,
                                input-output lHeadSection,
                                lScriptBlock).
  end.
  return lScriptBlock.
end function.

function OutputScript     returns logical
  (input-output cString       as character,
   input-output cWsoption     as character,
   input-output cContentType  as character,
   input-output lHeadSection  as logical,
   lScriptBlock               as logical):
/*------------------------------------------------------------------------------
  Purpose:  Output preprocessed text, handle html or script block
    Notes:
------------------------------------------------------------------------------*/

  define variable cCodeLine as character  no-undo.
  define variable iIdxPos   as integer    no-undo.
  define variable iCount    as integer    no-undo.
  define variable cTag      as character  no-undo.
  define variable cStr      as character  no-undo.
  define variable cStr1     as character  no-undo.
  define variable lOutput   as logical    no-undo.

  lOutput = not can-do(cWsoption,'get-options':U).

  if lScriptBlock then
    assign cTag    = cEndTag
           iIdxPos = index(cString, cTag).
  else
  do iCount = 1 to num-entries({&SPD_BLK_BEGIN}):
    assign cTag    = entry(iCount,{&SPD_BLK_BEGIN})
           iIdxPos = index(cString, cTag).

    /* skip fake script tag, only speedscript considered  */
    if iCount = 2 and (num-entries(cString,'=':U) = 1
                      or
                      not can-do('PROGRESS,WebSpeed4GL,SpeedScript':U,trim(entry(2,cString,'=':U),' ~t">':U))) then
    do:
      assign iIdxPos = 0.
      next.
    end.

    if iIdxPos > 0 then
    do:
      assign cEndTag   = entry(iCount,{&SPD_BLK_END})
             iExprTag  = lookup(cTag,{&SPD_EXPR_TAG}).
      leave.
    end.
  end.

  /* we stay in the same block                                    */
  if iIdxPos = 0 then
  do:
    if lScriptBlock then
      cStr = cString.
    else
    do:
      if lHeadSection then
        assign lHeadSection = CheckWsMetaTags(cString,
                                       input-output cWsoption,
                                       input-output cContentType).
      if num-entries(cContentType,chr(1)) = 2 then
      do:
        if can-do(cWsoption,'keep-meta-content-type':U) then
          assign cStr = cString.
        else
          assign iCount  = index(cString,'<meta':U) - 1
                 iCount  = if iCount < 0 then 0 else iCount
                 cStr    = substring(cString,1,iCount,'character':U)
                 cStr1   = substring(cString,iCount + 1,-1,'character':U)
                 iCount  = index(cStr1,'>':U) + 1
                 cStr1   = substring(cStr1,iCount,-1,'character':U)
                 cStr    = cStr + cStr1.
        cContentType = entry(2,cContentType,chr(1)).
      end.
      else cStr = cString.
      if cStr ne '':U then
        cStr = {&WSS_OUT_TEXT} + escape-output(cStr) + '~").':U.
    end.
    if cStr ne '':U and lOutput then
      put stream _sCmpOut unformatted cStr skip.
    cString = '':U.
    return lScriptBlock.
  end.

  /* we have an escape block tag                                  */
  if iIdxPos > 0 then
  do:
    if lScriptBlock then
    do:
      if iExprTag = 0 then
        assign cStr = substring(cString,1,iIdxPos - 1,'character':U).
      else
        assign cStr = {&WSS_OUT_CODE} +
                      html-decode(substring(cString,1,iIdxPos - 1,'character':U)) + ')).'.

    end.
    else
    do:
      cCodeLine = substring(cString,1,iIdxPos - 1,'character':U).
      if lHeadSection then
        lHeadSection = CheckWsMetaTags(cCodeLine,
                                       input-output cWsoption,
                                       input-output cContentType).
      if num-entries(cContentType,chr(1)) = 2 then
      do:
        if can-do(cWsoption,'keep-meta-content-type':U) then
          cStr = cCodeLine.
        else
          assign
          iCount  = index(cCodeLine,'<meta':U) - 1
          iCount  = if iCount < 0 then 0 else iCount
          cStr    = substring(cCodeLine,1,iCount,'character':U)
          cStr1   = substring(cCodeLine,iCount + 1,-1,'character':U)
          iCount  = index(cStr1,'>':U) + 1
          cStr1   = substring(cStr1,iCount,-1,'character':U)
          cStr    = cStr + cStr1.
        cContentType = entry(2,cContentType,chr(1)).
      end.
      else
        cStr = cCodeLine.
      if cStr ne '':U then
        cStr = {&WSS_OUT_TEXT} + escape-output(cStr) + '~").'.
    end.
    if cStr ne '':U and lOutput then
      put stream _sCmpOut unformatted cStr skip.
    /* The SCRIPTing tag is selected by BEGINS, but need full tag removal */
    if cTag = "<SCRIPT" then
    do:
      assign cString = trim(substring(cString,
                             iIdxPos + length(cTag,'character':U),
                             -1,'character':U))
             iIdxPos = index(cString,">":U)  /* Find end position of start tag */
             cString = trim(substring(cString,iIdxPos + 1)).
    end.
    else
      assign cString = trim(substring(cString,
                             iIdxPos + length(cTag,'character':U),
                             -1,'character':U)).
    return not lScriptBlock.
  end.
end function.

function GetClassName     returns character
  (input cFile    as character,
   input cClsBase as character):
   cFile = GetFileBaseName(cFile).
   if index(cFile, cClsBase) eq 1 then
      cFile = substring(cFile, length(cClsBase) + 2, -1) no-error.
   return replace(cFile, chr(47), '.').
end function.

function GetIanaEncode  returns character
  (input cAblEncode as character):
  &scoped-define ablEncode 'BIG-5,EUCJIS,GB2312,CP936,ROMAN-8,IBM858,KSC5601,SHIFT-JIS,620-2533':u
  &scoped-define ianaEncode 'Big5,EUC-JP,GB_2312-80,GBK,hp-roman8,IBM00858,KS_C_5601-1987,Shift_JIS,TIS-620':u

  define variable numEntry as integer no-undo.

  numEntry = lookup(cAblEncode, {&ablEncode}).
  if numEntry ne 0 then
     return entry(numEntry, {&ianaEncode}).

  /* iso encoding */
  if entry(1, cAblEncode, '-':u) eq 'ISO8859':u then
     return substitute('ISO-8859-&1':u, entry(2, cAblEncode, '-':u)).
  /* windows encoding */
  numEntry = integer(cAblEncode) no-error.
  if numEntry ne ? and numEntry gt 1250 and numEntry lt 1258 then
     return substitute('windows-&1', numEntry).
  /* default to same codepage */
  return cAblEncode.
end function.
