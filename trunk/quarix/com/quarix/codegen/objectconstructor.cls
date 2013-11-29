/*------------------------------------------------------------------------
   File        : objcetconstructor
   Purpose     :
   Syntax      :
   Description :
   Author(s)   : adam
   Created     : Fri Nov 14 11:03:02 EEST 2013
   Notes       :
 ----------------------------------------------------------------------*/

class com.quarix.codegen.objectconstructor final:

	define private variable DestinationPacket	as character						no-undo.
    define private variable DestinationFolder	as character						no-undo.
	define private variable TabLength			as integer							no-undo.
	define private variable FileWriter			as com.quarix.codegen.filewriter	no-undo.

	constructor private objectconstructor():
	end constructor.

	constructor public objectconstructor(input cDestPacket as character, input cDestFolder as character, input cTabLength as integer):

		assign
			DestinationPacket	= cDestPacket
			DestinationFolder	= cDestFolder
			TabLength			= cTabLength.

		if DestinationPacket	= ''	or
			DestinationPacket	= ?		or
			DestinationFolder	= ''	or
			DestinationFolder	= ?		or
			TabLength			= 0		or
			TabLength			= ?
		then return error.

		FileWriter = new com.quarix.codegen.filewriter().

		if not valid-object(FileWriter)
		then return error.

	end constructor.

	method private logical AddScopedDefine(input hBuffer as handle):

		define variable iFieldCount	as integer		no-undo.
		define variable cFieldName	as character	no-undo.

		if not valid-handle(hBuffer) or
			hBuffer:type <> 'BUFFER'
		then return false.

		do iFieldCount = 1 to hBuffer:num-fields:

			cFieldName = hBuffer:buffer-field(iFieldCount):name.

			if not FileWriter:generateScopedDefine(cFieldName, iFieldCount)
			then return false.
		end.

		if hBuffer:dbname <> 'PROGRESST'
		then do:
			if not FileWriter:generateFieldListDefinitionDb(hBuffer:num-fields)
			then return false.
		end.
		else do:
			if not FileWriter:generateFieldListDefinitionTt(hBuffer:num-fields)
			then return false.
		end.

		return true.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return false.
		end catch.

	end method.

	method private logical addFields(input hBuffer as handle):

		define variable iFieldCount	as integer		no-undo.
		define variable cFieldName	as character	no-undo.

		if not valid-handle(hBuffer) or
			hBuffer:type <> 'BUFFER'
		then return false.

		do iFieldCount = 1 to hBuffer:num-fields:

			cFieldName = hBuffer:buffer-field(iFieldCount):name.

			if hBuffer:dbname <> 'PROGRESST'
			then do:
				if not FileWriter:generatePropertyDbField(cFieldName)
				then return false.
			end.
			else do:
				if not FileWriter:generatePropertyTtField(cFieldName)
				then return false.
			end.
		end. /* do iFieldCount = 1 to hBuffer:num-fields */

		return true.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return false.
		end catch.

	end method.

	method private logical createFieldInstance(input hBuffer as handle):

		define variable iFieldCount	as integer  	no-undo.
		define variable cFieldName  as character	no-undo.

		if not valid-handle(hBuffer) or
			hBuffer:type <> 'BUFFER'
		then return false.

		do iFieldCount = 1 to hBuffer:num-fields:

			cFieldName = hBuffer:buffer-field(iFieldCount):name.

			if hBuffer:dbname <> 'PROGRESST'
			then
			do:
				if not FileWriter:generateDbFieldInstance(cFieldName)
				then return false.
			end.
			else
				if not FileWriter:generateTtFieldInstance(cFieldName)
				then return false.
		end.

		return true.

		catch appError as Progress.Lang.Error :
			delete object appError.
			return false.
		end catch.

	end method.

	method private logical deleteFields(input hBuffer as handle):

		define variable iFieldCount	as integer		no-undo.
		define variable cFieldName	as character	no-undo.

		if not valid-handle(hBuffer) or
			hBuffer:type <> 'BUFFER'
		then return false.

		do iFieldCount = 1 to hBuffer:num-fields:

			cFieldName = hBuffer:buffer-field(iFieldCount):name.

			if not FileWriter:generateDeleteProperty(cFieldName)
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
		define variable cTableName			as character	no-undo.

		if not valid-handle(hBuffer) or
			hBuffer:type <> 'BUFFER'
		then return false.

		if not valid-object(FileWriter)
		then return false.

		cTableName = hBuffer:serialize-name.

		if cTableName			= ''	or
			cTableName         	= ?		or
			DestinationPacket   = ''	or
			DestinationPacket   = ?		or
			DestinationFolder	= ''	or
			DestinationFolder	= ?
		then return false.

		if hBuffer:dbname = 'PROGRESST'
		then do:
			if cTableName begins 'tt'
			then cFileName = substitute('&1&2', 'TT', substitute('&1&2', caps(substring(substring(cTableName, 3), 1, 1)), substring(substring(cTableName, 3), 2))).
			else cFileName = substitute('&1&2', 'TT', substitute('&1&2', caps(substring(cTableName, 1, 1)), substring(cTableName, 2))).
		end.
		else
			cFileName= substitute('&1&2', 'DB', substitute('&1&2', caps(substring(cTableName, 1, 1)), substring(cTableName, 2))).

		assign
			cClassName        	= substitute('&1.&2', DestinationPacket, cFileName)
			cClassName        	= replace(cClassName, '..', '.')
			DestinationFolder	= replace(DestinationFolder, '~\', '~/')
			cAbsoluteFileName	= substitute('&1~/&2.cls', DestinationFolder, cFileName)
			cAbsoluteFileName	= replace(cAbsoluteFileName, '~/~/', '~/').

		assign
			FileWriter:FileName		= cAbsoluteFileName
			FileWriter:TabLength	= TabLength.

		FileWriter:DeleteFile().

		if not FileWriter:generateHeader(cClassName)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not AddScopedDefine(input hBuffer)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not FileWriter:generatePropertyField('Instance', cClassName)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not FileWriter:generateProperty('TableName', '')
		then  do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not addFields(input hBuffer)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not FileWriter:generateConstructorHeader(cFileName, cTableName)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not createFieldInstance(input hBuffer)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not FileWriter:generateConstructorClosingStatement()
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not FileWriter:generateDestructorHeader(cFileName)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not deleteFields(input hBuffer)
		then do:
			FileWriter:DeleteFile().
			return false.
		end.

		if not FileWriter:generateDestructorClosingStatement()
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

        return createClassFile(input hBuffer).

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

        return createDefFromBuffer(input hBuf).

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