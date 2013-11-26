{d:\Projects\OpenedgeArchitect\bravura\src\quarix-mystrobe-demo\wicketds\server\dscustomer.i}

define variable objectconstructor	as com.quarix.codegen.objectconstructor	no-undo.
define variable hObject				as handle								no-undo.

objectconstructor = new com.quarix.codegen.objectconstructor().
objectconstructor:DestinationPacket = "wicketds.server.tableobjects".
objectconstructor:DestinationFolder = "d:\Projects\OpenedgeArchitect\bravura\src\quarix-mystrobe-demo\wicketds\server\tableobjects\".

objectconstructor:TabLength = 4.

hObject = dataset dscustomer:handle.

objectconstructor:createTableDefObject(hObject).

delete object objectconstructor no-error.
