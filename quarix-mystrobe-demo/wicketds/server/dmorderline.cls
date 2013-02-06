routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dsorderline:handle

class wicketds.server.dmorderline inherits DataModel:

    {wicketds/server/dsorderline.i}

    {wicketds/server/dsitem.i}

    def private property dmitem as wicketds.server.dmitem no-undo
    get:
        if not valid-object(dmitem) then dmitem = cast (GetInstance ('wicketds.server.dmitem':u), wicketds.server.dmitem).
        return dmitem.
    end.
    private set.

    constructor public dmorderline (  ):

        DataAccess      = cast(GetInstance('wicketds.server.daorderline':u), 'wicketds.server.daorderline':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daoorderline'.
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

    method public logical getbyitemidlist(input itemidlist as wicketds.server.itemidlist,
                                                output dataset dsorderline):

		return dataFetchIdList(input itemidlist, output dataset dsorderline by-reference, 'FetchDataByItemIdList':U).

    end method.

    method public logical getbyorderidlist(input orderidlist as wicketds.server.orderidlist,
                                                output dataset dsorderline):

		return dataFetchIdList(input orderidlist, output dataset dsorderline by-reference, 'FetchDataByOrderIdList':U).

    end method.

     method private logical AfterLoadData (input-output dataset dsorderline):

    	define variable itemidlist as wicketds.server.itemidlist no-undo.

    	itemidlist = cast(GetInstance('wicketds.server.itemidlist':u), 'wicketds.server.itemidlist':u).

		for each ttorderline
			no-lock:

			itemidlist:addItemNum(ttorderline.Itemnum).
		end.

		if itemidlist:IdListHasData()
		then do:
			dmitem:ClearContext().

			if not dmitem:getbyitemidlist(itemidlist, output dataset dsitem by-reference)
			then return false.

			for each ttorderline
				no-lock:

				find first ttitem
					where ttitem.Itemnum = ttorderline.Itemnum
					no-lock no-error.

				if available(ttitem)
				then ttorderline.ItemName = ttitem.ItemName.
			end.

		end. /* if itemidlist:IdListHasData() */

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

    method public override logical AfterLoadData():

		return AfterLoadData(input-output dataset-handle datasetHandle by-reference).

	end method.

end class.