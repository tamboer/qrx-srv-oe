/*------------------------------------------------------------------------
   File        : objcetconstructor
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : adam
   Created     : Fri Nov 14 11:03:02 EEST 2013
   Notes       :
 ----------------------------------------------------------------------*/

class com.quarix.codegen.objectconstructor:

	define public property DestinationPacket as character no-undo
        get.
        set.

    define public property DestinationFolder as character no-undo
        get.
        set.

	define public property TabLength as integer no-undo
        get.
        set.

	define protected variable FileWriter as com.quarix.codegen.filewriter no-undo.

	constructor public objectconstructor():

		FileWriter = new com.quarix.codegen.filewriter().

		if not valid-object(FileWriter)
		then return error.

	end constructor.

	constructor public objectconstructor(input cDestPacket as character, input cDestFolder as character):

        assign
            DestinationPacket   = cDestPacket
            DestinationFolder   = cDestFolder.

		if DestinationPacket	= ''	or
            DestinationPacket	= ?		or
		    DestinationFolder	= ''	or
            DestinationFolder	= ?
        then return error.

		FileWriter = new com.quarix.codegen.filewriter().

		if not valid-object(FileWriter)
        then return error.

	end constructor.

	method private logical addFields(input hBuffer as handle):

		define variable iFieldCount		as integer		no-undo.
		define variable cFieldName		as character	no-undo.

		if not valid-handle(hBuffer) or
			hBuffer:type <> 'BUFFER'
		then return false.

		do iFieldCount = 1 to hBuffer:num-fields:

			cFieldName = hBuffer:buffer-field(iFieldCount):name.

			if not FileWriter:generateProperty(cFieldName)
			then return false.
		end.

		return true.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return false.
		end catch.

	end method.

	method private logical createClassFile(input hBuffer as handle):

	    define variable cClassName			as character	no-undo.
		define variable cAbsoluteFileName	as character	no-undo.
		define variable cFileName			as character	no-undo.

		if not valid-handle(hBuffer) or
			hBuffer:type <> 'BUFFER'
		then return false.

		cFileName = hBuffer:serialize-name.

	    if cFileName            = ''	or
            cFileName           = ?		or
            DestinationPacket   = ''	or
            DestinationPacket   = ?		or
	    	DestinationFolder	= ''	or
	    	DestinationFolder	= ?
        then return false.

		if not valid-object(FileWriter)
		then return false.

		assign
			cFileName			= lc(cFileName)
        	cClassName			= substitute('&1.&2', DestinationPacket, cFileName)
			cClassName			= replace(cClassName, '..', '.')
			DestinationFolder	= replace(DestinationFolder, '~\', '~/')
			cAbsoluteFileName	= substitute('&1~/&2.cls', DestinationFolder, cFileName)
			cAbsoluteFileName	= replace(cAbsoluteFileName, '~/~/', '~/').

		FileWriter:FileName = cAbsoluteFileName.
		FileWriter:TabLength = TabLength.

		FileWriter:DeleteFile().

		if not FileWriter:generateHeader(cClassName)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not addFields(input hBuffer)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not FileWriter:generateClosingStatement()
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

        return true.

        catch appError as Progress.Lang.Error :

            delete object appError.

        	if valid-object(FileWriter)
        	then FileWriter:DeleteFile().

            return false.

        end catch.

    end method.

	method public logical createDefFromBuffer(input hBuffer as handle):

	    if not valid-handle(hBuffer) or
            hBuffer:type <> 'BUFFER'
	    then return false.

        if not createClassFile(input hBuffer)
        then return false.

	    return true.

        catch appError as Progress.Lang.Error :
            delete object appError.
            return false.
        end catch.

	end method.

	method public logical createDefFromTable(input hTable as handle):

        define variable hBuf as handle no-undo.

	    if not valid-handle(hTable) or
            hTable:type <> 'TEMP-TABLE'
        then return false.

        hBuf = hTable:default-buffer-handle.

        if not createDefFromBuffer(input hBuf)
        then return false.

        return true.

        catch appError as Progress.Lang.Error :
            delete object appError.
            return false.
        end catch.

	end method.

	method public logical createDefFromDataset(input hDataset as handle):

	    define variable iNumBuf    as integer  no-undo.
	    define variable hBuf       as handle   no-undo.

	    if not valid-handle(hDataset) or
            hDataset:type <> 'DATASET'
	    then return false.

	    do iNumBuf = 1 to hDataset:num-buffers:

            hBuf = hDataset:get-buffer-handle (iNumBuf).

            if not createDefFromBuffer(input hBuf)
            then return false.

	    end. /* do iNumBuf = 1 to hDataset:num-buffers */

	    return true.

	    catch appError as Progress.Lang.Error :
            delete object appError.
            return false.
        end catch.

	end method.

	method public logical createTableDefObject(input hObject as handle):

	    if not valid-handle(hObject)
	    then return false.

	    case hObject:type:

	        when 'DATASET'
	        then return createDefFromDataset(input hObject).

	        when 'TEMP-TABLE'
	        then return createDefFromTable(input hObject).

	        when 'BUFFER'
	        then return createDefFromBuffer(input hObject).

	        otherwise
	           return false.

	    end case. /* case hObject:type */

        return true.

        catch appError as Progress.Lang.Error :
            delete object appError.
            return false.
        end catch.

	end method.

end class.
