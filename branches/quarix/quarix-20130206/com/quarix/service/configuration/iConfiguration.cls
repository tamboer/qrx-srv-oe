/*---------------------------------------------------------------------
File        : iConfiguration.cls
Purpose     :

Syntax      :

Description :

Author(s)   : Marian
Created     : generated 10/12/08
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
---------------------------------------------------------------------*/


interface  com.quarix.service.configuration.iConfiguration:

    method public character GetEnvironment  ().

    method public logical OpenEnvironment  (input environmentName as character).

    method public void    CloseEnvironment (input environmentName as character).
    method public logical SaveEnvironment  (input environmentName as character).

    method public void    CloseEnvironment ().
    method public logical SaveEnvironment  ().

    method public logical DeleteKey (input keyName as character, input sectionName as character, input objectName as character).
    method public logical DeleteKey (input keyName as character, input sectionName as character).
    method public logical DeleteKey (input keyName as character).

    method public character GetKey (input keyName as character, input sectionName as character, input objectName as character).
    method public character GetKey (input keyName as character, input sectionName as character).
    method public character GetKey (input keyName as character).

    method public logical DeleteSection (input sectionName as character).
    method public logical DeleteSection (input sectionName as character, input objectName as character).

    method public logical Purge ().
    method public logical Purge (input environmentName as character).

    method public logical SetKey (input keyName as character, input keyValue as character, input sectionName as character, input objectName as character).
    method public logical SetKey (input keyName as character, input keyValue as character, input sectionName as character).
    method public logical SetKey (input keyName as character, input keyValue as character).


end interface.


