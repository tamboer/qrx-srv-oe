
define variable cPath as character no-undo.

define stream str_out.

cPath = 'd:\Projects\OpenedgeArchitect\bravura\src\quarix-mystrobe-demo'.

run ParseProject(cPath).

procedure ParseProject:

	define input parameter cStartDir as character no-undo.

	define variable cFileName	as character no-undo.
	define variable cFullPath	as character no-undo.
	define variable cFileType	as character no-undo.
	define variable cCurrDir 	as character no-undo.
	define variable cType		as character no-undo.

	assign
		cStartDir = replace(cStartDir, '~/', '~\')
		cCurrDir  = entry(num-entries(cStartDir, '~\'), cStartDir, '~\').

	if cCurrDir = '.svn'
	then return.

	input from os-dir(cStartDir).

	repeat:
		import cFileName cFullPath cFileType no-error.

		if lookup(cFileName, '.,..') = 0
		then do:
			if index(cFileType, 'D')  > 0
			then run ParseProject(cStartDir + '~\' + cFileName).
			else
				if  index(cFileType, 'F') > 0   						and
					length(cFileName) >= 3								and
					(cFileName begins 'tt' or cFileName begins 'ds')	and
					substring(cFileName, length(cFileName) - 1, 2) = '.i'
				then do:
					if cFileName begins 'tt'
					then cType = 'tt'.
					else cType = 'ds'.

					run GenerateObject(cFullPath, cType, substring(cFileName, 1, length(cFileName) - 2)).
				end.

		end. /* if lookup(cFileName, '.,..') = 0 */

	end.

	input close.

end procedure.

procedure GenerateObject:

	define input parameter pcFullPath	as character no-undo.
	define input parameter pcType		as character no-undo.
	define input parameter pcFileName	as character no-undo.

	define variable cTempProcName as character no-undo.

	cTempProcName = substitute('d:\Projects\OpenedgeArchitect\bravura\src\quarix-mystrobe-demo\gen_obj_def\gen_&1.p', pcFileName).

	output stream str_out to value(cTempProcName).

	put stream str_out unformatted '~{' pcFullPath '~}'skip.

	put stream str_out unformatted 'define variable objectconstructor as com.quarix.codegen.objectconstructor no-undo.' skip.

	put stream str_out unformatted 'define variable hObject as handle no-undo.' skip.

	put stream str_out unformatted 'objectconstructor = new com.quarix.codegen.objectconstructor("wicketds.server.tableobjects", "d:\Projects\OpenedgeArchitect\bravura\src\quarix-mystrobe-demo\wicketds\server\tableobjects\", 4).' skip.

	if pcType = 'tt'
	then
		put stream str_out unformatted 'hObject = temp-table ' pcFileName ':handle.' skip.
	else
		put stream str_out unformatted 'hObject = dataset ' pcFileName ':handle.' skip.

	put stream str_out unformatted 'objectconstructor:createTableDefObject(hObject, "sports2000").' skip.

	put stream str_out unformatted 'finally:' skip.

	put stream str_out unformatted 'delete object objectconstructor no-error.' skip.

	put stream str_out unformatted 'end finally.' skip.

	output stream str_out close.

	run value(cTempProcName).

	os-delete value(cTempProcName).

end procedure.
