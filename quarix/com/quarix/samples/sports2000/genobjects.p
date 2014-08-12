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

using com.quarix.codegen.objectconstructor.

{com/quarix/samples/sports2000/dscustomer.i}

/* connect c:\qrx_srv_oe\sports2000\sports2000.db -RO. */


DO on error undo, throw:
  def var oObjConstruct as objectconstructor no-undo.
  

  def var cDestPacket as char no-undo init "com.quarix.samples.sports2000.tableobjects".
  def var cDestFolder as char no-undo init "C:/qrx_srv_oe/qrx-srv-oe/trunk/quarix".

  assign cDestFolder = cDestFolder + '/' + replace(cDestPacket, '.','/') + '/'.

  def var iTabLength   as int no-undo init -1.

  def var cDbName as char no-undo init 'sports2000'.

  oObjConstruct  = new objectconstructor(cDestPacket, cDestFolder, iTabLength).
  
  oObjConstruct:createTableDefObject(dataset dsCustomer:handle, cDbName).
/*oObjConstruct:createTableDefObject(temp-table ttCustomer:handle, cDbName). */
  oObjConstruct:createTableDefObject(buffer Customer:handle, cDbName).

END.
/*
catch err as progress.lang.ProError:

end catch.
*/
finally:
  if valid-object(oObjConstruct) then delete object oObjConstruct.
end finally.

    
 
