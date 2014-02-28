
/*------------------------------------------------------------------------
    File        : iBusinessEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : andrei.tarba
    Created     : Mon Oct 21 08:45:02 EEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING com.mrg.framework.system.RequestContext.

INTERFACE com.mrg.framework.data.iBusinessEntity:  
    
    /* returns data in the dsDataset based on filter criteria found int the dataContext object */
    METHOD PUBLIC VOID FetchData (requestContext AS RequestContext, OUTPUT DATASET-HANDLE dsDataset).

    /* stores data in the dsDataset */
    METHOD PUBLIC LOGICAL StoreData (INPUT-OUTPUT DATASET-HANDLE dsDataset).
    
END INTERFACE.