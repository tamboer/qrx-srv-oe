
/*------------------------------------------------------------------------
    File        : license.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Andriuhan
    Created     : Thu Aug 25 10:34:58 EEST 2011
    Notes       :
    License     :
    This file is part of the QRX-SRV-OE software framework.
    Copyright (C) 2011, SC Yonder SRL (http://www.tss-yonder.com)

    The QRX-SRV-OE software framework is free software; you can redistribute
    it and/or modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either version 2.1
    of the License, or (at your option) any later version.

    The QRX-SRV-OE software framework is distributed in the hope that it will
    be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
    General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with the QRX-SRV-OE software framework; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA or on the internet at the following address:
    http://www.gnu.org/licenses/lgpl-2.1.txt
  ----------------------------------------------------------------------*/

define variable cWorkSpace as character no-undo.
define variable cProject   as character no-undo.

assign
    cWorkSpace = 'D:\Projects\OpenedgeArchitect\TVH'
    cProject   = 'quarix_TVH'.


define temp-table ttDir no-undo
    field DirName as character
    index ttDir is primary unique DirName.

define temp-table ttDirFile no-undo
    field DirName    as character
    field SourceName as character
    index ttDirFile is primary unique DirName SourceName.

run ParseProject(cWorkSpace,cProject).
run InsertLicense.

message 'done'
    view-as alert-box.

procedure ParseProject:

    define input  parameter pcParent  as character no-undo.
    define input  parameter pcCurrDir as character no-undo.

    define variable cStartDir as character no-undo.
    define variable cFileName as character no-undo.
    define variable cFullPath as character no-undo.
    define variable cFileType as character no-undo.
    define variable cTargName as character no-undo.
    define variable cFileExt  as character no-undo init 'cls,i,p,w'.

    if  pcCurrDir = 'database'   or
        pcCurrDir = 'tools'      or
        pcCurrDir = 'doc'        or
        pcCurrDir = 'uml'        or
        pcCurrDir = '.svn'
        then return.

    assign
        cStartDir = pcParent + '~\' + pcCurrDir
        cStartDir = replace(cStartDir, '~/', '~\').

    create ttDir.
    assign
        ttDir.DirName = cStartDir
        .

    input from os-dir(cStartDir).

    repeat:
        import cFileName cFullPath cFileType no-error.


        if lookup(cFileName, '.,..') = 0
            then
        do:


            if cFileName begins '.'
                then next.


            if index(cFileType, 'D')  > 0
                then run ParseProject(cStartDir,cFileName).
            else
                if  index(cFileType, 'F') > 0 and
                    lookup(entry(2,cFullPath,'.'),cFileExt) gt 0
                    then
                do:


                    create ttDirFile.
                    assign
                        ttDirFile.DirName    = cStartDir
                        ttDirFile.SourceName = cFileName
                        no-error.
                end.

        end. /* if lookup(cFileName, '.,..') = 0 */

    end.

    input close.

end procedure.

procedure InsertLicense:

    define variable cFileName as character no-undo.
    define variable cLine     as character no-undo.
    define variable lcBody    as longchar  no-undo.
    define variable lComment  as logical   no-undo.
    define variable cTempFile as character no-undo.
    define variable lUpdated  as logical   no-undo.

    for each ttDir:

        for each ttDirFile
            where ttDirFile.DirName = ttDir.DirName
            no-lock:

            cFileName = ttDir.DirName + '~\' + ttDirFile.SourceName.
            cTempFile = ttDir.DirName + '~\' + '__tmp'.


            output to value(cFileName) append.
            put unformatted ' ' skip.
            output close.


            copy-lob from file cFileName to lcBody no-error.
            lComment = false.

            if index(lcBody,'License     :') gt 0 then next.

            if index(lcBody,'/*--') > 0 and
               index(lcBody,'--*/') > index(lcBody,'/*--') then
                lComment = true.


            lcBody = ''.
            lUpdated = false.


            output to value(cTempFile).

            if not lComment then
            do:
                put unformatted '/*------------------------------------------------------------------------' skip.
                run PutLincenseText.
                put unformatted '----------------------------------------------------------------------*/' skip(1).
                lUpdated = true.
            end.


            input from value(cFileName).
            repeat:
                cLine = ''.
                import unformatted cLine.
                if index(cLine,'--*/') gt 0 and
                   lUpdated = false then do:
                    run PutLincenseText.
                    lUpdated = true.
                end.


                if cLine eq '' then
                    put unformatted ' ' skip.


                put unformatted cLine skip.
            end.


            input close.
            output close.

            copy-lob from file cTempFile to file cFileName no-error.
            os-delete value(cTempFile).


        end.
    end.

end procedure.


procedure PutLincenseText:


    put unformatted('    License     :                                                             ') skip.
    put unformatted('    This file is part of the QRX-SRV-OE software framework.                  ') skip.
    put unformatted('    Copyright (C) 2011, SC Yonder SRL (http://www.tss-yonder.com)            ') skip(1).


    put unformatted('    The QRX-SRV-OE software framework is free software; you can redistribute ') skip.
    put unformatted('    it and/or modify it under the terms of the GNU Lesser General Public     ') skip.
    put unformatted('    License as published by the Free Software Foundation; either version 2.1 ') skip.
    put unformatted('    of the License, or (at your option) any later version.                   ') skip(1).


    put unformatted('    The QRX-SRV-OE software framework is distributed in the hope that it will') skip.
    put unformatted('    be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of') skip.
    put unformatted('    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser ') skip.
    put unformatted('    General Public License for more details.                                 ') skip(1).


    put unformatted('    You should have received a copy of the GNU Lesser General Public License ') skip.
    put unformatted('    along with the QRX-SRV-OE software framework; if not, write to the Free  ') skip.
    put unformatted('    Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA   ') skip.
    put unformatted('    02110-1301  USA or on the internet at the following address:             ') skip.
    put unformatted('    http://www.gnu.org/licenses/lgpl-2.1.txt                                 ') skip.


end procedure.


