routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dsitem:handle

class wicketds.server.dmitem inherits DataModel:

    {wicketds/server/dsitem.i}

    {wicketds/server/dsorderline.i}

    def private property dmorderline as wicketds.server.dmorderline no-undo
    get:
        if not valid-object(dmorderline) then dmorderline = cast (GetInstance ('wicketds.server.dmorderline':u), wicketds.server.dmorderline).
        return dmorderline.
    end.
    private set.

    constructor public dmitem (  ):

        DataAccess      = cast(GetInstance('wicketds.server.daitem':u), 'wicketds.server.daitem':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daitem'.
        OpenOnInit      = false.
        SendChangesOnly = false.

        if not valid-object(DataAccess) or
            not valid-handle(datasetHandle)
        then return error.

    end constructor.

    method public override logical dataFetch ():

        DataContext:setAllRecords(true).

        return super:dataFetch().

    end method.

    method private logical BeforeSaveData(input-output dataset dsitem):

    	define variable itemidlist as wicketds.server.itemidlist no-undo.

    	itemidlist = cast(GetInstance('wicketds.server.itemidlist':u), 'wicketds.server.itemidlist':u).

    	for each btitem
    		where row-state(btitem) = row-deleted
    		no-lock:

    		itemidlist:addItemNum(btitem.itemnum).
    	end.

    	if itemidlist:IdListHasData()
    	then do:
    		dmorderline:ClearContext().

    		if not dmorderline:getbyitemidlist(itemidlist, output dataset dsorderline by-reference)
    		then return false.

    		find first ttorderline
    			no-lock no-error.

    		if available(ttorderline)
    		then do:
    			ThrowClientError(1000, 'Item cannot be deleted, first delete the order lines!').

    			return false.
    		end.
    	end.

    	return true.

        catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.
		finally:
			UnloadInstance(itemidlist).
		end finally.

    end method.

    method public override logical BeforeSaveData():

    	return BeforeSaveData(input-output dataset-handle datasetHandle by-reference).

    end method.

    method public logical getbyitemidlist(input itemidlist as wicketds.server.itemidlist,
                                                output dataset dsitem):

		return dataFetchIdList(input itemidlist, output dataset dsitem by-reference).

    end method.

end class.