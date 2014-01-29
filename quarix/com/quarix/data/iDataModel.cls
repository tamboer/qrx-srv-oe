
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
   using com.quarix.web.*.
   using com.quarix.data.*.

   interface com.quarix.data.iDataModel:

      method public void       SetDataContext (input dtContext as DataContext).

      method public character  GetFilterValue (fieldname as character, operName as character).

      method public character  GetKeyValue    (tableName as character, keyField as character, keyValue as character, lookupField as character).

      method public character  GetKeyValue    (tableName as character, keyField as character, keyValue as character, lookupField as character, filterXml as memptr).

      method public logical    BeforeRemoveFilter (fieldName as character).

      method public void       RemoveFilter       (fieldName as character).

      method public logical    BeforeSetFilter (fieldName as character, operName as character, fieldValue as character).

      method public logical    SetFilter       (fieldName as character, operName as character, fieldValue as character).

      method public logical    SetFilter       (tableName as character, fieldName as character, operName as character, fieldValue as character).

      method public logical    BeforeRemoveSort (fieldName as character).

      method public void       RemoveSort       (fieldName as character).

      method public logical    BeforeSetSort (fieldName as character, descendingSort as logical).

      method public logical    SetSort       (fieldName as character).

      method public logical    SetSort       (tableName as character, fieldName as character).

      method public logical    SetSort       (fieldName as character, descendingSort as logical).

      method public void       SetUseIndex (tableName as character, useIndex as character).

      method public integer    GetBatchSize (bufferName as character).

      method public logical    IsFirstBatch ().

      method public logical    IsLastBatch  ().

      method public logical    HandleRequest          (methodName as character, webRequest as Request, webResponse as Response).

      method public logical    dataFetch  (      output dataset-handle dsHandle).

      method public logical    dataUpdate (input-output dataset-handle dsHandle).

      method public logical    LoadData ().

      method public logical    SaveData ().

      method public logical    BeforeDataService (actionName as character).

      method public logical    BeforeService     (actionName as character).

      method public logical    BeforeLoadData    ().

      method public logical    BeforeSaveData    ().

      method public logical    AfterDataService  (actionName as character).

      method public logical    AfterService      (actionName as character).

      method public logical    AfterLoadData     ().

      method public logical    AfterSaveData     ().

   end interface.
