
/*------------------------------------------------------------------------
    File        : iBaseData
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Andriuhan
    Created     : Tue Mar 06 10:01:37 EET 2012
    Notes       :
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using com.quarix.data.*.
using com.quarix.web.*.

interface com.quarix.data.iBaseData:

    define public property DtUtilManager as com.quarix.service.dtutil.idtUtil
        get.

    define public property Util   as com.quarix.base.Util
        get.

    define public property ID                       as character   no-undo
        get.

    define public property ReadOnly                 as logical     no-undo
        get.

    define public property OpenOnInit               as logical     no-undo
        get.

    define public property AutoSync                 as logical     no-undo
        get.

    define public property SendChangesOnly          as logical     no-undo
        get.

    define public property SendFilterEveryTime      as logical     no-undo
        get.

    define public property UpdateMode               as logical     no-undo
        get.

    define public property RemoteChildFilter        as logical     no-undo
        get.

    define public property BatchSize                as integer     no-undo
        get.

    define public property BatchMargin              as integer     no-undo
        get.

    define public property ThreadTimeOut            as integer     no-undo
        get.

    define public property datasetHandle            as handle      no-undo
        get.

    define public property DataRequest              as DataRequest no-undo
        get.

    define public property DataContext              as DataContext no-undo
        get.

    define public property LogResponse              as logical     no-undo
        get.

    /* BaseObject methods */

    method public Object GetInstance (className  as character).

    method public Object GetInstance (className  as character, logError as logical).

    method public void ThrowClientError (errorCode as integer, errorText as character).

    method public void ThrowClientError (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowClientError (errorCode as integer, errorType as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowClientError (errorCode as integer, errorType as integer, errorText as character, errorBuffer as character, errorField as character, errorRecord as integer ).

    method public void ThrowClientError (errorCode as integer, errorType as integer, errorText as character, errorBuffer as character, errorField as character, errorRecord as integer, errorTag as character ).

    method public void ThrowDebug       (errorCode as integer, errorText as character).

    method public void ThrowDebug       (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowError       (errorCode as integer, errorText as character).

    method public void ThrowError       (errorCode as integer, errorText as character, errorCallStack as character).

    method public void ThrowError       (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowError       (appError as Progress.Lang.Error).

    method public void ThrowInfo        (errorCode as integer, errorText as character).

    method public void ThrowInfo        (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void ThrowWarning     (errorCode as integer, errorText as character).

    method public void ThrowWarning     (errorCode as integer, errorText as character, errorBuffer as character, errorField as character).

    method public void UnloadInstance   (instanceObj as Object).

    /* DataObject methods */

    method public logical BeforeRequest (actionName as character).

    method public logical AfterRequest  (actionName as character).

    method public logical BeforeDataOutput ().

    method public logical CheckMandatory(hBuf as handle, pcField as character).

    method public void ClearContext().

    method public void ClearFilters().

    method public void ClearProperties().

    method public void ClearProperty(PropertyName as character).

    method public void ClearQueryData().

    method public void ClearSort().

    method public logical dataFetch().

    method public logical dataFetch(output dataset-handle dsHandle).

    method public logical dataFetchIdList(idlist as IdList, output dataset-handle hDsDataset).

    method public logical dataFetchIdList(idlist as IdList, output dataset-handle hDsDataset, input QueryName as character).

    method public logical dataUpdate().

    method public logical dataUpdate(input-output dataset-handle dsHandle).

    method public void DisableLogResponse().

    method public void EnableLogResponse ().

    method public void DumpToXml().

    method public void DumpToXml(pcName as character).

    method public handle getDefaultDatasetHandle().

    method public memptr    GetDescription(responseFormat as character, applicationPath as character).

    method public character GetFilterValue(tableName as character, fieldname as character, operName as character).

    method public handle    GetIdListTmpHandle().

    method public character GetProperty(PropertyName as character).

    method public character GetQueryName().

    method public logical HandleRequest(actionName as character, webRequest as Request, webResponse as Response).

    method public logical IsFirstBatch().

    method public logical IsLastBatch().

    method public logical LoadRequestData().

    method public logical PropertyAvailable().

    method public logical PropertyAvailable(PropertyName as character).

    method public void RemoveFilter (tableName as character, fieldName as character).

    method public void RemoveSort   (tableName as character, fieldName as character).

    method public void RemoveTableFilters (tableName as character).

    method public void RemoveTableSort    (tableName as character).

    method public void restoreDatasetHandle().

    method public logical SaveData().

    method public logical SetBatchSize().

    method public logical SetBatchSizeFromRequest().

    method public logical SetFilter (tableName as character, fieldName as character, operName as character, fieldValue as character).

    method public logical SetFiltersFromRequest().

    method public logical SetRequestFilters(tableName as character, considerSearchFilter as logical).

    method public logical SetSort(tableName as character, fieldName as character, descendingSort as logical).

    method public logical SetSortFromRequest().

    method public void SetDataContext (dtContext as DataContext).

    method public void SetIdListTmpHandle(PropertyValue as handle).

    method public void SetParentBatchSize(parentBatchSize as integer).

    method public void SetProperty(PropertyName as character, PropertyValue as character).

    method public void SetQueryName(PropertyValue as character).

    method public void SetSessionParameters().

    /* DataModel methods */

    method public character  GetFilterValue (fieldname as character, operName as character).

    method public character  GetKeyValue    (tableName as character, keyField as character, keyValue as character, lookupField as character).

    method public character  GetKeyValue    (tableName as character, keyField as character, keyValue as character, lookupField as character, filterXml as memptr).

    method public logical    BeforeRemoveFilter (fieldName as character).

    method public void       RemoveFilter       (fieldName as character).

    method public logical    BeforeSetFilter (fieldName as character, operName as character, fieldValue as character).

    method public logical    SetFilter       (fieldName as character, operName as character, fieldValue as character).

    method public logical    BeforeRemoveSort (fieldName as character).

    method public void       RemoveSort       (fieldName as character).

    method public logical    BeforeSetSort (fieldName as character, descendingSort as logical).

    method public logical    SetSort       (fieldName as character).

    method public logical    SetSort       (tableName as character, fieldName as character).

    method public logical    SetSort       (fieldName as character, descendingSort as logical).

    method public void       SetUseIndex (tableName as character, useIndex as character).

    method public integer    GetBatchSize (bufferName as character).

    method public logical    HandleRequestDummyData (lReloadDataRequest as logical).

    method public logical    LoadData ().

    method public logical    BeforeDataService (actionName as character).

    method public logical    BeforeService     (actionName as character).

    method public logical    BeforeLoadData    ().

    method public logical    BeforeSaveData    ().

    method public logical    AfterDataService  (actionName as character).

    method public logical    AfterService      (actionName as character).

    method public logical    AfterLoadData     ().

    method public logical    AfterSaveData     ().

end interface.