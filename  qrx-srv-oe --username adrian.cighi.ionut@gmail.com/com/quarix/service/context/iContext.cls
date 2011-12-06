/*------------------------------------------------------------------------
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

interface com.quarix.service.context.iContext:
     method public logical GetPassThrough ().

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as character).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as datetime).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as datetime-tz).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as rowid).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as memptr).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as date).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as logical).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as longchar).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as decimal).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as int64).

     method public logical GetValue (input sourceObject as character, input keyName as character , output keyValue as integer).

     method public logical GetValue (input keyName as character , output keyValue as character).

     method public logical GetValue (input keyName as character , output keyValue as datetime).

     method public logical GetValue (input keyName as character , output keyValue as datetime-tz).

     method public logical GetValue (input keyName as character , output keyValue as rowid).

     method public logical GetValue (input keyName as character , output keyValue as memptr).

     method public logical GetValue (input keyName as character , output keyValue as date).

     method public logical GetValue (input keyName as character , output keyValue as logical).

     method public logical GetValue (input keyName as character , output keyValue as longchar).

     method public logical GetValue (input keyName as character , output keyValue as decimal).

     method public logical GetValue (input keyName as character , output keyValue as int64).

     method public logical GetValue (input keyName as character , output keyValue as integer).

     method public logical SetPassThrough (passThrough as logical).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as character).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as datetime).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as datetime-tz).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as rowid).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as memptr).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as date).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as logical).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as longchar).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as decimal).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as int64).

     method public logical SetValue (input sourceObject as character, input keyName as character , input keyValue as integer).

     method public logical SetValue (input keyName as character , input keyValue as character).

     method public logical SetValue (input keyName as character , input keyValue as datetime).

     method public logical SetValue (input keyName as character , input keyValue as datetime-tz).

     method public logical SetValue (input keyName as character , input keyValue as rowid).

     method public logical SetValue (input keyName as character , input keyValue as memptr).

     method public logical SetValue (input keyName as character , input keyValue as date).

     method public logical SetValue (input keyName as character , input keyValue as logical).

     method public logical SetValue (input keyName as character , input keyValue as longchar).

     method public logical SetValue (input keyName as character , input keyValue as decimal).

     method public logical SetValue (input keyName as character , input keyValue as int64).

     method public logical SetValue (input keyName as character , input keyValue as integer).

     method public logical RemoveValue (input sourceObject as character, input keyName as character).

     method public logical RemoveValue (input keyName as character).

     method public logical SaveContext (input sessionManager as com.quarix.service.session.iSession).

     method public logical LoadContext (input sessionManager as com.quarix.service.session.iSession).

     method public void    Purge ().

     method public void    Purge (input sourceObject as character).
end interface.
