/*------------------------------------------------------------------------
    File        : startup.p
    Purpose     : Start persistent and hold a reference to the global
                  Controller, start one instance of it if needed.

    Syntax      :

    Description :

    Author(s)   : Marian
    Created     : Fri Mar 19 13:07:24 EET 2010
    Notes       :
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
using com.quarix.system.Controller.

define variable mainController as Controller no-undo.

/* ***************************  Main Block  *************************** */

procedure getQuarixController:
   define output parameter qrxController as Controller no-undo.

   if not valid-object(mainController) then do:
&if keyword-all('static':u) ne ? &then
      mainController = com.quarix.system.Controller:GetInstance().
&else
      define variable obj as Progress.Lang.Object no-undo.

      obj = session:first-object.
      do while valid-object(obj):
         if type-of(obj, 'com.quarix.system.Controller':u) then do:
            mainController = cast(obj, 'com.quarix.system.Controller':u).
            leave.
         end.
         obj = obj:next-sibling.
      end.
      if not valid-object(mainController) then
         mainController = new Controller().
&endif
   end.

   qrxController = mainController.

end procedure.


