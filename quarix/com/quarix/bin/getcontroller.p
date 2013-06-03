/*-----------------------------------------------------------------------
File: getcontroller.p

Purpose:
	Instantiate the controller for the requests

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
Author(s)   : Adam
Created     : 19/03/2013
------------------------------------------------------------------------*/

using com.quarix.system.Controller.

define output parameter oController as Controller no-undo.

define variable mainController	as Controller	no-undo.
define variable superProc		as integer		no-undo.
define variable startUpHdl		as handle		no-undo.

#GetController:
do:
    if not valid-object(mainController) then do
       on error undo, leave
       on stop undo, leave:

        /* try to get the reference to the controller from super */
        do superProc = 1 to num-entries(session:super-procedures):
           startUpHdl = widget-handle(entry(superProc, session:super-procedures)).
           if startUpHdl:name eq 'com/quarix/bin/startup.p':u then do:
              run getQuarixController in startUpHdl (output mainController) no-error.
              leave #GetController.
           end.
        end.

        /* if startup super not already started do it now and set it as session super */

       run com/quarix/bin/startup.p persistent set startUpHdl no-error.
       if valid-handle(startUpHdl) then do:
          run getQuarixController in startUpHdl (output mainController) no-error.
          session:add-super-procedure(startUpHdl) no-error.
       end.
    end.
end.

if valid-object(mainController)
then oController = mainController.

return.
