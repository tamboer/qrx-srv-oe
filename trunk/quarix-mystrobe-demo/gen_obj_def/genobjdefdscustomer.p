
define variable objectconstructor	as com.quarix.codegen.objectconstructor	no-undo.
define variable hObject				as handle								no-undo.

objectconstructor = new com.quarix.codegen.objectconstructor(
	"wicketds.server.tableobjects",
	"d:\Projects\OpenedgeArchitect\bravura\src\quarix-mystrobe-demo\wicketds\server\tableobjects\",
	4
	).

hObject = buffer Customer:handle.

objectconstructor:createTableDefObject(hObject).

finally:
	delete object objectconstructor no-error.
end finally.
