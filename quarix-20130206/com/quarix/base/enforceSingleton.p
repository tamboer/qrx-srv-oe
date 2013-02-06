/*------------------------------------------------------------------------
    File        : enforceSingleton.p
    Purpose     : Check if there are other instance of the same object
                  already instantiated, returns error if singleton was
                  already instantiated before.

    Syntax      :

    Description : Enforce singleton behaviour, only one instance of a
                  singleton should exists.

    Author(s)   : Marian
    Created     : Fri Sep 17 08:14:09 EEST 2010
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
using com.quarix.base.iSingleton.

/* ***************************  Main Block  *************************** */
define input  parameter singletonObj  as iSingleton no-undo.

define variable obj as Progress.Lang.Object no-undo.
define variable cls as Progress.Lang.Class  no-undo.
define variable ths as character            no-undo.

assign
   cls = singletonObj:GetClass()
   ths = cls:TypeName
   obj = session:first-object.

/* make sure no other factory instance already exists */
do while valid-object(obj):
   if type-of(obj, 'iSingleton':u) then do:
      cls = obj:GetClass().
      if cls:TypeName eq ths and obj ne singletonObj then
         return error.
   end.
   obj = obj:next-sibling.
end.


