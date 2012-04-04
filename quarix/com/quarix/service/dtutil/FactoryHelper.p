
/*------------------------------------------------------------------------
    File        : FactoryHelper.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Andriuhan
    Created     : Tue Feb 07 14:47:45 EET 2012
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

procedure getInstance:
   define input  parameter className       as character            no-undo.
   define output parameter classInstance   as Progress.Lang.Object no-undo.

   case className:
      /* add custom localization services */
      when 'dtUtilBase':U then
         classInstance = new com.quarix.service.dtutil.dtUtilBase().

      /* default localization service     */
      otherwise
      &if keyword-all('static':u) ne ? &then
         classInstance = com.quarix.service.dtutil.dtUtilCore:GetInstance().
      &else
         classInstance = new com.quarix.service.dtutil.dtUtilCore().
      &endif
   end case.
end procedure.
