/*-----------------------------------------------------------------------
File: dispatcher.p

Purpose:
	Handle all calls from the interface layer.

Description:
	This is the entry point for every call made from the interface layer;
	it servers mainly as a dispatcher to back-end business logic.
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
------------------------------------------------------------------------*/
/*-----------------------------------------------------------------------
Location	: com/quarix/bin
Author(s)   : medu
Created     : 28/11/2006
Notes       : If input-output parameters get changed the Java layer should
			  be adapted with newly generated class (proxygen).

------------------------------------------------------------------------*/
using com.quarix.system.Controller.

/*  context temp-table                                              	*/
{com/quarix/web/ttRequest.i}
{com/quarix/web/ttResponse.i}

define input  parameter table         for ttRequest.
define input  parameter pstrRequest   as memptr    no-undo.
define output parameter table         for ttResponse.
define output parameter pstrResponse  as memptr    no-undo.

define variable mainController as Controller no-undo.
define variable startUpHdl     as handle     no-undo.
define variable superProc      as integer    no-undo.

/* try to avoid memory leak issues when calling external code
   all dinamicaly created objects that are not explicitely created
   in a named widget pool will be deleted when the dispacther ends   */
create widget-pool.

session:error-stack-trace = true.


set-size(pstrResponse) = 0.

file-info:file-name = '.~/log~/4gl.log':u.
if file-info:file-type ne ? then
   output to '.~/log~/4gl.log':u append.

/* try to get the reference to the controller from super */
do superProc = 1 to num-entries(session:super-procedures):
   startUpHdl = widget-handle(entry(superProc, session:super-procedures)).
   if startUpHdl:name eq 'com/quarix/bin/startup.p':u then do:
      run getQuarixController in startUpHdl (output mainController) no-error.
      leave.
   end.
end.

/* if startup super not already started do it now and set it as session super */
if not valid-object(mainController) then do
   on error undo, leave
   on stop undo, leave:
   run com/quarix/bin/startup.p persistent set startUpHdl no-error.
   if valid-handle(startUpHdl) then do:
      run getQuarixController in startUpHdl (output mainController) no-error.
      session:add-super-procedure(startUpHdl) no-error.
   end.
end.

/* we should have a valid controller by now so run the request on it */
if valid-object(mainController) then do
   on error undo, retry
   on quit  undo, retry
   on stop  undo, retry:

   if retry then do:
      set-size(pstrRequest) = 0.
      create ttResponse.
      assign
         ttResponse.fieldType  = 4
         ttResponse.fieldName  = 'ERROR-STATUS':u
         ttResponse.fieldValue = 'TRUE':u.
      leave.
   end.

   message '------------------------------------------------------------------------------------------------------------------------------':U skip.

   message 'START HandleRequest: ' skip.

   mainController:HandleRequest(table ttRequest by-reference, pstrRequest, table ttResponse by-reference, pstrResponse).

   message 'END HandleRequest: ' skip.

   message '------------------------------------------------------------------------------------------------------------------------------':U skip.

   set-size(pstrRequest) = 0.

end. /* if valid-object(mainController) then do */

output close.

