
routine-level on error undo, throw.

using Progress.Lang.*.
using com.quarix.data.DataModel.

&GLOBAL-DEFINE DefaultDatasetHandle dataset dsorder:handle

class wicketds.server.dmorder inherits DataModel:

    {wicketds/server/dsorder.i}

    {wicketds/server/dscustomer.i}

    {wicketds/server/dssalesrep.i}

    def private property dmcustomer as wicketds.server.dmcustomer no-undo
    get:
        if not valid-object(dmcustomer) then dmcustomer = cast (GetInstance ('wicketds.server.dmcustomer':u), wicketds.server.dmcustomer).
        return dmcustomer.
    end.
    private set.

    def private property dmsalesrep as wicketds.server.dmsalesrep no-undo
    get:
        if not valid-object(dmsalesrep) then dmsalesrep = cast (GetInstance ('wicketds.server.dmsalesrep':u), wicketds.server.dmsalesrep).
        return dmsalesrep.
    end.
    private set.

    constructor public dmorder (  ):

        DataAccess      = cast(GetInstance('wicketds.server.daorder':u), 'wicketds.server.daorder':u).
        datasetHandle   = {&DefaultDatasetHandle}.
        batchSize       = 0.
        id              = 'daoorder'.
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

    method private logical AfterLoadData (input-output dataset dsorder):

    	define variable customeridlist as wicketds.server.customeridlist no-undo.
    	define variable salesrepidlist as wicketds.server.salesrepidlist no-undo.

    	customeridlist = cast(GetInstance('wicketds.server.customeridlist':u), 'wicketds.server.customeridlist':u).

    	salesrepidlist = cast(GetInstance('wicketds.server.salesrepidlist':u), 'wicketds.server.salesrepidlist':u).

		for each ttorder
			no-lock:

			customeridlist:addCustNum(ttorder.CustNum).

			salesrepidlist:addSalesRep(ttorder.SalesRep).
		end.

		if customeridlist:IdListHasData()
		then do:
			dmcustomer:ClearContext().

			if not dmcustomer:getbycustomeridlist(customeridlist, output dataset dscustomer by-reference)
			then return false.

			for each ttorder
				no-lock:

				find first ttcustomer
					where ttcustomer.custnum = ttorder.custnum
					no-lock no-error.

				if available(ttcustomer)
				then ttorder.CustName = ttcustomer.name.
			end.

		end. /* if customeridlist:IdListHasData() */

		if salesrepidlist:IdListHasData()
		then do:
			dmsalesrep:ClearContext().

			if not dmsalesrep:getbysalesrepidlist(salesrepidlist, output dataset dssalesrep by-reference)
			then return false.

			for each ttorder
				no-lock:

				find first ttsalesrep
					where ttsalesrep.SalesRep = ttorder.SalesRep
					no-lock no-error.

				if available(ttsalesrep)
				then ttorder.SalesRepName = ttsalesrep.RepName.
			end.

		end. /* if salesrepidlist:IdListHasData() */

		return true.

		catch appError as Progress.Lang.Error :
			ThrowError(input appError).
			delete object appError.
			return false.
		end catch.
		finally:
			UnloadInstance(customeridlist).
			UnloadInstance(salesrepidlist).
		end finally.

	end method.

    method public override logical AfterLoadData():

		return AfterLoadData(input-output dataset-handle datasetHandle by-reference).

	end method.

end class.