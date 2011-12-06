
/*------------------------------------------------------------------------
    File        : iDataModel
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Mon Nov 29 10:23:19 EET 2010
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

   using Progress.Lang.*.

   interface com.quarix.data.iDataModel:
      method public void       ClearFilters ().

      method public void       RemoveFilter (fieldName as character).

      method public void       RemoveFilter (tableName as character, fieldName as character).

      method public void       RemoveTableFilters (tableName as character).

      method public logical    SetFilter (fieldName as character, operName as character, fieldValue as character).

      method public logical    SetFilter (tableName as character, fieldName as character, operName as character, fieldValue as character).

      method public void       ClearSort ().

      method public void       RemoveSort (fieldName as character).

      method public void       RemoveSort (tableName as character, fieldName as character).

      method public void       RemoveTableSort (tableName as character).

      method public logical    SetSort (fieldName as character).

      method public logical    SetSort (tableName as character, fieldName as character).

      method public logical    SetSort (fieldName as character, descendingSort as logical).

      method public logical    SetSort (tableName as character, fieldName as character, descendingSort as logical).

      method public integer    GetBatchSize (bufferName as character).

      method public logical    LoadData ().

      method public logical    SaveData ().

   end interface.
