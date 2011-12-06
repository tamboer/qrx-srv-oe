/*------------------------------------------------------------------------
    File        : FactoryHelper.p
    Purpose     :

    Syntax      :

    Description : Factory Helper

    Author(s)   : Marian
    Created     : Mon Dec 22 09:44:07 EET 2008
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
      when 'Application':u then
         classInstance = new com.quarix.system.Application().
      when 'Version':u then
         classInstance = new com.quarix.system.Version().
      when 'ApplicationUser':u then
         classInstance = new com.quarix.system.ApplicationUser().
      when 'FileSystem':u then
         classInstance = new com.quarix.system.FileSystem().
   end case.
end procedure.
