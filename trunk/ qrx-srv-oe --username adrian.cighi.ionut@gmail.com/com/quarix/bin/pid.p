
/*------------------------------------------------------------------------
    File        : pid.p
    Purpose     : Procedure used to get process id.

    Syntax      :

    Description : Procedure used to get process id.

    Author(s)   : Andriuhan
    Created     : Fri Oct 28 12:22:27 EEST 2011
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

procedure GetCurrentProcessId external "KERNEL32.DLL":
    define return parameter intProcessHandle as long.
end procedure.

procedure getpid external "/usr/lib/libc.1" cdecl:
    /* has yet to be implemented */
    define return parameter intProcessHandle as long.
end procedure.


