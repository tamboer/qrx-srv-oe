/*------------------------------------------------------------------------
File : jsonreader
Purpose : decode json responses from framework
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

USING Progress.Json.ObjectModel.ObjectModelParser.
using Progress.Json.ObjectModel.JsonConstruct.
using Progress.Json.ObjectModel.JsonObject.
using Progress.Json.ObjectModel.JsonArray.

def input-output param mJson as memptr no-undo.
def input        param ihTT  as handle no-undo.

{com/quarix/samples/sports2000/ttcustomer.i}

/* Definitions for fieldtype conversion between character & integer */

&global-define FIELDTYPE_CHARACTER 1
&GLOBAL-DEFINE FIELDTYPE_DATE 2
&GLOBAL-DEFINE FIELDTYPE_LOGICAL 3
&GLOBAL-DEFINE FIELDTYPE_INTEGER 4
&GLOBAL-DEFINE FIELDTYPE_DECIMAL 5
&GLOBAL-DEFINE FIELDTYPE_RECID 7
&GLOBAL-DEFINE FIELDTYPE_RAW 8
&GLOBAL-DEFINE FIELDTYPE_HANDLE 10
&GLOBAL-DEFINE FIELDTYPE_ROWID 13
&GLOBAL-DEFINE FIELDTYPE_COM-HANDLE 14
&GLOBAL-DEFINE FIELDTYPE_BLOB 18
&GLOBAL-DEFINE FIELDTYPE_CLOB 19
&GLOBAL-DEFINE FIELDTYPE_DATETIME 34
&GLOBAL-DEFINE FIELDTYPE_DATETIME-TZ 40
&GLOBAL-DEFINE FIELDTYPE_INT64 41
&GLOBAL-DEFINE FIELDTYPE_PROGRESS_LANG_OBJECT 42

PROCEDURE fillTableRows:
  def input param oTableRowsJsonArr as JsonArray no-undo. /* The array with record data */
  def input param hBuf              as handle    no-undo. /* The buffer where the data has to go into */

  def var iTableRowsLen    as int       no-undo.
  def var iTableRowLen     as int       no-undo.
  def var hFldArr          as handle    no-undo extent. /* We use an array to hold the field handles */
  def var iFldTypeArr      as int       no-undo extent. /* We use an array to hold the field types */
  def var oTableRowJsonArr as JsonArray no-undo.
  def var iRowIdx          as int       no-undo.
  def var iFldIdx          as int       no-undo.
  def var hFld             as handle    no-undo.
  def var cDataType        as char      no-undo.
  def var iFldType         as int       no-undo.

  hBuf:empty-temp-table.

  assign iTableRowsLen = oTableRowsJsonArr:Length.

  /* message "We got a table of " iTableRowsLen " rows" view-as alert-box. */

  /* We actually have data so lets get started */
  IF iTableRowsLen > 0 THEN do:

    assign oTableRowJsonArr = oTableRowsJsonArr:GetJsonArray(1)
           iTableRowLen     = oTableRowJsonArr:Length.

    extent(hFldArr)     = iTableRowLen.
    extent(iFldTypeArr) = iTableRowLen.

    /* message "Each row has " iTableRowLen " fields" view-as alert-box. */

    /* Map the buffer handles and the data types to the 2 allocated progress arrays */
    DO iFldIdx = 1 to iTableRowLen:
      assign hFld             = hBuf:buffer-field(iFldIdx)
             hFldArr[iFldIdx] = hFld
             cDataType        = hFld:data-type
             iFldType         = 
                          (IF cDataType = "INTEGER"     THEN {&FIELDTYPE_INTEGER}
                      else IF cDataType = "CHARACTER"   THEN {&FIELDTYPE_CHARACTER}
                      else IF cDataType = "DECIMAL"     THEN {&FIELDTYPE_DECIMAL}
                      else IF cDataType = "LOGICAL"     THEN {&FIELDTYPE_LOGICAL}
                      else IF cDataType = "DATE"        THEN {&FIELDTYPE_DATE}
                      else IF cDataType = "DATETIME"    THEN {&FIELDTYPE_DATETIME}
                      else IF cDataType = "DATETIME-TZ" THEN {&FIELDTYPE_DATETIME-TZ}
                      else IF cDataType = "ROWID"       THEN {&FIELDTYPE_ROWID}
                      else IF cDataType = "INT64"       THEN {&FIELDTYPE_INT64}
                      else IF cDataType = "HANDLE"      THEN {&FIELDTYPE_HANDLE}
                      else IF cDataType = "RECID"       THEN {&FIELDTYPE_RECID}
                      else IF cDataType = "RAW"         THEN {&FIELDTYPE_RAW}
                      else ?)
             iFldTypeArr[iFldIdx] = iFldType.
    END.

    /* Iterate the rows array, and for each row array, transfer the data to the buffer */

    DO iRowIdx = 1 to iTableRowsLen:
      oTableRowJsonArr = oTableRowsJsonArr:GetJsonArray(iRowIdx).
      hBuf:buffer-create.
      /*  message "ici" iTableRowsLen view-as alert-box. */
      DO iFldIdx = 1 to iTableRowLen:

         assign iFldType = iFldTypeArr[iFldIdx]
                hFld     = hFldArr[iFldIdx].

         IF iFldType = {&FIELDTYPE_INTEGER}          THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetInteger(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_CHARACTER}   THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetCharacter(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_DECIMAL}     THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetDecimal(iFldIdx). 
         ELSE IF iFldType = {&FIELDTYPE_LOGICAL}     THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetLogical(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_DATE}        THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetDate(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_DATETIME}    THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetDateTime(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_DATETIME-TZ} THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetDateTimeTZ(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_ROWID}       THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetRowid(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_INT64}       THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetInt64(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_HANDLE}      THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetHandle(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_RECID}       THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetRecid(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_RAW}         THEN
           assign hFld:buffer-value = oTableRowJsonArr:GetRaw(iFldIdx).
        
         /*
         ELSE IF iFldType = {&FIELDTYPE_LONGCHAR} THEN
           assign hFldArr[iFldIdx]:buffer-value = oTableRowJsonArr:GetLongchar(iFldIdx).
         ELSE IF iFldType = {&FIELDTYPE_MEMPTR} THEN
           assign hFldArr[iFldIdx]:buffer-value = oTableRowJsonArr:GetMemptr(iFldIdx).
         */

      END. /* Done iterating the fields of one row */
      delete object oTableRowJsonArr.    
    END.   /* Done iterating the complete rows array */
  END.

  finally:
    IF valid-object(oTableRowJsonArr) THEN delete object oTableRowJsonArr no-error.
  END finally.


END procedure.






DEF VAR oJsonParser       AS ObjectModelParser no-undo.
def var oJsonConstruct    as JsonConstruct     no-undo.
def var oTablesJsonObj    as JsonObject        no-undo.
def var oTablesJsonArr    as JsonArray         no-undo.
def var oTableJsonObj     as JsonObject        no-undo.
def var oTableRowsJsonArr as JsonArray         no-undo.
def var oTableInfoJsonObj as JsonObject        no-undo.

def var iTableRowsLen     as int               no-undo.
def var iTblIdx    as int no-undo.
def var iTblRowIdx as int no-undo.
def var cTableId   as char no-undo.
def var lChangesOnly as log no-undo.
def var lHasFirstRow as log no-undo.
def var lHasLastRow  as log no-undo.


def var lcTest as longchar no-undo.
def var cTest as char no-undo.


DO on error undo, throw:
  oJsonParser = NEW ObjectModelParser().
  oJsonConstruct = oJsonParser:Parse(mJson).
  IF valid-object(oJsonConstruct) then do:
                                     
    if type-of(oJsonConstruct, Progress.Json.ObjectModel.JsonObject) THEN do:
      oTablesJsonObj = cast(oJsonConstruct, Progress.Json.ObjectModel.JsonObject).
 
      oTablesJsonArr = oTablesJsonObj:GetJsonArray('tables').
      DO iTblIdx = 1 to oTablesJsonArr:Length:
        assign oTableJsonObj     = oTablesJsonArr:GetJsonObject(iTblIdx)
               cTableId          = oTableJsonObj:GetCharacter('id')
               oTableRowsJsonArr = oTableJsonObj:GetJsonArray('rows')
               oTableInfoJsonObj = oTableJsonObj:GetJsonObject('info').
 

        RUN fillTableRows(oTableRowsJsonArr, ihTT:default-buffer-handle).

        assign lChangesOnly = oTableInfoJsonObj:GetLogical('changesOnly')
               lHasFirstRow = oTableInfoJsonObj:GetLogical('hasFirstRow')
               lHasLastRow  = oTableInfoJsonObj:GetLogical('hasLastRow').

        /* Dispose the objects allocated for the processed table */
        delete object oTableRowsJsonArr.
        delete object oTableInfoJsonObj.
        delete object oTableJsonObj.
      END.

    END.
    delete object oJsonConstruct.
  END.
  

END.
/*
CATCH err AS Progress.lang.ProError:
  IF type-of(err, Progress.json.JsonParserError) THEN
    MESSAGE err:getclass():typename err:GetMessage(1) VIEW-AS ALERT-BOX.
END CATCH.
*/
FINALLY:
  IF valid-object(oTableInfoJsonObj) THEN delete object oTableInfoJsonObj no-error.
  if valid-object(oTableRowsJsonArr) then delete object oTableRowsJsonArr no-error.
  if valid-object(oTableJsonObj)     then delete object oTableJsonObj     no-error.
  if valid-object(oTablesJsonArr)    then delete object oTablesJsonArr    no-error.
  if valid-object(oJsonConstruct)    then delete object oJsonConstruct    no-error.
  if valid-object(oJsonParser)       then DELETE OBJECT oJsonParser       NO-ERROR.
END FINALLY.
