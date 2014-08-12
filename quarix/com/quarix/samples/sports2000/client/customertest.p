/*------------------------------------------------------------------------
File : dacustomer
Purpose : Customer client invocation for sports2000 sample
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

{com/quarix/samples/sports2000/ttcustomer.i}

/*
ASSIGN PROPATH = 'C:\qrx_srv_oe\qrx-srv-oe\trunk\quarix\,' + PROPATH.

FOR FIRST core_app where core_app.app_name = 'sampleapp' NO-LOCK,
 EACH core_config WHERE core_config.app_id = core_app.app_id NO-LOCK:
     DISP core_config.
END.
*/

def var hInvFw as handle no-undo.

RUN com/quarix/test/invokeframework.p PERSISTENT set hInvFw.

def var cSessionId as char no-undo init ?.

assign cSessionId = dynamic-function("generateSessionId" in hInvFw).

run setAppName   in hInvFw("sampleApp").
run setAppSrvURL in hInvFw("-URL 'AppServer://localhost:5162/quarix_samples'").
run setLogDir    in hInvFw("c:/qrx_srv_oe/log/").
run setDebug     in hInvFw(FALSE).

/* TODO: Implement settings */

PROCEDURE ttcustomer_sendrows:

  RUN newDaoMsgParams in hInvFw(TEMP-TABLE ttcustomer:NAME, 0).
                    
  RUN addEqualsFilter in hInvFw("city", "boston").
  /*

  RUN addProperty in hInvFw("FilterLanguageID", "NL").
  RUN addProperty in hInvFw("FilterUsrID",      "382").
  RUN addProperty in hInvFw("FilterEntityType", "EMPLOYEE").
  RUN addProperty in hInvFw("Company",          "TVH").
  RUN addProperty in hInvFw("ZoneId",           "5").
  */


  RUN sendRows in hInvFw(INPUT TEMP-TABLE ttcustomer:HANDLE,
               INPUT "com/quarix/samples/sports2000/dmcustomer",
               INPUT "first",
               input cSessionId).
          
END PROCEDURE.

DEF VAR iStart AS INT NO-UNDO.
ASSIGN istart = ETIME(FALSE).

RUN ttcustomer_sendrows.

MESSAGE (ETIME(FALSE) - iStart) VIEW-AS ALERT-BOX.
output to c:/test/custres.txt.
FOR each ttCustomer no-lock:
    export ttcustomer.
END.
output close.

finally:
  if valid-handle(hInvFw) then delete procedure hInvFw no-error.
end finally.
