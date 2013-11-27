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

define variable lcFieldValue as longchar no-undo.
define variable cFieldValue as character no-undo.

for each ttRequest
    no-lock:

    copy-lob ttRequest.fieldValue to lcFieldValue.

    cFieldValue = lcFieldValue.


    message 'ttRequest.fieldType' ttRequest.fieldType skip
        'ttRequest.fieldName' ttRequest.fieldName skip
    cFieldValue
    view-as alert-box.

end.






define variable mainController	as Controller	no-undo.
define variable iStartTime		as integer		no-undo.
define variable iCallDuration	as integer		no-undo.

/* Start Enable Profiler */

define variable cProfilerOut	as character	no-undo.
define variable iNum			as integer		no-undo.

cProfilerOut = substitute('&1/&2.&3':u, session:temp-directory, 'profiler':u, 'out':u).

if search('startup_custom.p') <> ?
then run value('startup_custom.p') no-error.

assign
	cProfilerOut = replace(cProfilerOut, '~\':u, '~/':u)
	cProfilerOut = replace(cProfilerOut, '~/~/':u, '~/':u).

do while search(cProfilerOut) <> ?:

	iNum = iNum + 1.

	assign
		cProfilerOut = substitute('&1/&2_&3.&4':u, session:temp-directory, 'profiler':u, string(iNum), 'out':u)
		cProfilerOut = replace(cProfilerOut, '~\':u, '~/':u)
		cProfilerOut = replace(cProfilerOut, '~/~/':u, '~/':u).

end. /* do while search(cProfilerOut) <> ? */

profiler:enabled   = no.
profiler:file-name = cProfilerOut.
profiler:listings  = true.
profiler:directory = session:temp-directory.
profiler:profiling = no.

/* End Enable Profiler */

/* try to avoid memory leak issues when calling external code
   all dinamicaly created objects that are not explicitely created
   in a named widget pool will be deleted when the dispacther ends   */
create widget-pool.

session:error-stack-trace = true.

set-size(pstrResponse) = 0.

file-info:file-name = '.~/log~/4gl.log':u.
if file-info:file-type ne ? then
   output to '.~/log~/4gl.log':u append.

run com/quarix/bin/getcontroller.p(output mainController).

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

   if profiler:enabled and
      cProfilerOut <> '':u and
      cProfilerOut <> ?
   then
      message 'Profiler log file created: ' cProfilerOut skip.

   iStartTime = etime.

   mainController:HandleRequest(table ttRequest by-reference, pstrRequest, table ttResponse by-reference, pstrResponse).

   iCallDuration = etime - iStartTime.

   message 'END HandleRequest: ' iCallDuration skip.

   message '------------------------------------------------------------------------------------------------------------------------------':U skip.

   set-size(pstrRequest) = 0.

end. /* if valid-object(mainController) then do */

output close.
run UnloadInstances.
/* Disable Profiler */

if profiler:enabled
then do:
	profiler:profiling = false.
	profiler:write-data().
end.

procedure UnloadInstances:

    define variable obj		as Progress.Lang.Object	no-undo.
    define variable delObj	as Progress.Lang.Object	no-undo.

    obj = session:first-object.

    do while valid-object(obj):

        if not type-of(obj, 'com.quarix.base.iSingleton':u)  and
               type-of(obj, 'com.quarix.base.iDisposable':u) and
               type-of(obj, 'com.quarix.base.BaseObject':u)
		then do:
            delObj = obj.
        end.

        obj = obj:next-sibling.

        delete object delObj no-error.

    end. /* do while valid-object(obj) */

    run com/quarix/bin/debugInfo.p.

end procedure.
