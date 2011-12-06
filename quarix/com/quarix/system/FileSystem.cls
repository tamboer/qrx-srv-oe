
 /*------------------------------------------------------------------------
    File        : FileSystem
    Purpose     : FileSystem operation both on UNIX and WIN32
    Syntax      :
    Description :
    Author(s)   : Balazs Elod (EIB)
    Created     : Wed Dec 17 10:49:33 EET 2008
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
routine-level on error undo, throw.

class com.quarix.system.FileSystem
        inherits com.quarix.base.BaseObject
        implements com.quarix.base.iSingleton use-widget-pool final:

   method public logical CanRead
      (nodeName as character):
       /* Test if the node exists in the file system and it's readable. */
       return GetFullPath(nodeName) ne ? and
              index(file-info:file-type, 'r':u) gt 0.
       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
       end catch.
   end method.

   method public logical CanWrite
      (nodeName as character):
	/* Test if the node exists in the file system and it's writable
	   (a file is also writable if it doesn't exists and the directory is writable). */
       if GetFullPath(nodeName) ne ? and
          index(file-info:file-type, 'w':u) gt 0 then
          return true.
       file-info:file-name = GetPath(nodeName).
       return file-info:file-type ne ? and
              index(file-info:file-type, 'd':u) gt 0 and
              index(file-info:file-type, 'w':u) gt 0.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
       end catch.
   end method.

   method public logical DirCreate
      (dirName as character):
      /* Create a directory on file system (recursive if necessary). */
      define variable parentDir as character no-undo.

      if IsDir(dirName) then
         return true.

      parentDir = GetPath(dirName).
      if num-entries(parentDir, '~/':u) gt 1 and not IsDir(parentDir) then do:
         DirCreate(parentDir).
      end.
      os-create-dir value(dirName).
      return os-error eq 0.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical FileCreate
      (fileName as character):
      /* Create a file on the file system (recursive create the directories if necessary). */
      define variable parentDir as character no-undo.

      if IsFile(fileName) then
         return true.

      parentDir = GetPath(fileName).
      if (IsDir(parentDir) eq true or DirCreate(parentDir) eq true) and
         CanWrite(fileName) then do:
         output to value(fileName).
         output close.
         return true.
      end.
      return false.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public datetime GetCreateTime
      (nodeName as character):
	   /* Get the create time of the node. */
       if GetFullPath(nodeName) ne ? then
          return datetime(file-info:file-create-date, file-info:file-create-time).
       return ?.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
       end catch.
   end method.


   method public character GetName
      (nodeName as character):
      /* Get the name of the node from the given node path. */

      if Util:IsEmpty(nodeName) then do:
         nodeName = right-trim(replace(nodeName, chr(92), '~/':u), '~/':u).
         if index(nodeName, '~/':u) gt 0 then
            return substring(nodeName, r-index(nodeName, '~/':u) + 1, -1, 'character':u).
      end.
      return nodeName.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
   end method.

   method public character GetPath
      (nodeName as character):
      /* Get the path of the parent directory from the given node path. */

      if not Util:IsEmpty(nodeName) then do:
         file-info:file-name = nodeName.
         nodeName = Util:Nvl(file-info:full-pathname, nodeName).
         nodeName = right-trim(replace(nodeName, chr(92), '~/':u), '~/':u).
         if index(nodeName, '~/':u) gt 0 then
            return substring(nodeName, 1, r-index(nodeName, '~/':u), 'character':u).
       end.
       return ?.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
       end catch.
   end method.

   method public integer GetSize
       (nodeName as character):
	/* Get the size of the file (or all files under the node if it's a directory). */
        def var cFileName as char no-undo.
        def var iDirSize as inte no-undo.

       if Util:IsEmpty(nodeName) then
           return 0.
       assign nodeName = trim(replace(nodeName, chr(92), "~/"), "~/").
       if IsFile(nodeName) then do:
           assign file-info:file-name = nodeName.
           return file-info:file-size.
       end.
       else do:
           input from os-dir(nodeName).
           repeat
               on error undo, throw:
               import cFileName.
               if IsFile(nodeName + "~/" + cFileName) then do:
                   file-info:file-name = nodeName + "~/" + cFileName.
                   iDirSize = iDirSize + file-info:file-size.
               end.
           end.
           input close.
           return iDirSize.
       end. /* if IsFile(nodeName) then do: */
       return 0.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return 0.
       end catch.
   end method.

   method public integer GetSize
       (nodeName as character, recursiveCount as logical):
	/* Get the size of all files under the node if it's a directory, recursive count all directories under it. */
       def var cFileName as char no-undo.
       def var iDirSize as inte no-undo.
       if Util:IsEmpty(nodeName) then
           return 0.
       assign nodeName = trim(replace(nodeName, chr(92), "~/"), "~/").
       if IsFile(nodeName) then do:
           assign file-info:file-name = nodeName.
           return file-info:file-size.
       end.
       else do:
           input from os-dir(nodeName).
           repeat
               on error undo, throw:
               import cFileName.
               if cFileName <> "." and cFileName <> ".." and cFileName <> ? then do:
                   if IsFile(nodeName + "~/" + cFileName) then do:
                       file-info:file-name = nodeName + "/" + cFileName.
                       iDirSize = iDirSize + file-info:file-size.
                   end.
                   else if IsDir(nodeName + "~/" + cFileName) and recursiveCount then
                            assign iDirSize = iDirSize + GetSize(nodeName + "~/" + cFileName, yes).
               end.
           end.
           input close.
           return iDirSize.
       end. /* if IsFile(nodeName) then do: */
       return 0.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return 0.
       end catch.
   end method.

   method public character GetTempFile
      (dirName as character):
      return GetTempFile(dirName, 'qrx':u).
   end method.

   method public character GetTempFile
      (dirName as character, filePrefix as character):
      define variable fileName as character no-undo.
      define variable iCnt     as integer   no-undo.
      /* Get a temporary file name that does not exist in a given directory. */
      dirName = Util:Nvl(dirName, session:temp-dir).
      do iCnt = 1 to 100:
         fileName = substitute('&1~/&2_&3.tmp', dirName, filePrefix, encode(guid(generate-uuid))).
         if FileCreate(fileName) then
            return fileName.
      end.
      return ?.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
   end method.

   method public datetime GetUpdateTime
      (nodeName as character):
      /* Get the last update time of the node. */
      if GetFullPath(nodeName) ne ? then
          return datetime(file-info:file-mod-date, file-info:file-mod-time).
      return ?.
   end method.

   method public logical IsDir
      (nodeName as character):
      /* Test if the node exists in the file system and it's a directory. */
      file-info:file-name = GetFullPath(nodeName).
      return file-info:file-type ne ? and
             index(file-info:file-type, 'd':u) gt 0.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical IsFile
      (nodeName as character):
      file-info:file-name = GetFullPath(nodeName).
      return file-info:file-type ne ? and
             index(file-info:file-type, 'f':u) gt 0.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical OsCopy
       (sourceNode as character, targetNode as character):
	/* Copy a file/directory to another place under the file system. */
       def var cFileName     as char no-undo.
       def var cFullFileName as char no-undo.
       def var lSuccess      as logi no-undo init yes.

       if Util:IsEmpty(sourceNode) or Util:IsEmpty(targetNode) then
       if (not IsDir(sourceNode) and not IsFile(sourceNode)) then
           return false.

       if not IsDir(targetNode) then
           assign lSuccess = DirCreate(targetNode).

       if IsFile(sourceNode) then do:
           assign cFileName = trim(replace(sourceNode, chr(92), "~/"), "~/")
                  cFileName = substring(cFileName, r-index(cFileName, "~/") + 1, length(cFileName)).
           os-copy value(sourceNode) value(trim(replace(targetNode, chr(92), "~/"), "~/") + "~/" + cFileName).
           return os-error = 0.
       end.
       if IsDir(sourceNode) then do:
           input from os-dir(sourceNode).
           repeat
               on error undo, throw:
               import cFileName.
               if cFileName = "." or cFileName = ".." then
                   next.
               assign cFullFileName = trim(replace(sourceNode, chr(92), "~/"), "~/") + "~/" + cFileName.

               assign cFileName = trim(replace(cFullFileName, chr(92), "~/"), "~/")
                      cFileName = substring(cFileName, r-index(cFileName, "~/") + 1, length(cFileName)).

               if IsFile(cFullFileName) then do:
                   os-copy value(cFullFileName) value(trim(replace(targetNode, chr(92), "~/"), "~/") + "~/" + cFileName).
                   assign lSuccess = lSuccess and os-error = 0.
               end.
               if IsDir(cFullFileName) then do:
                   assign lSuccess = DirCreate(trim(replace(targetNode, chr(92), "~/"), "~/") + "~/" + cFileName)
                          lSuccess = lSuccess and OsCopy(cFullFileName, trim(replace(targetNode, chr(92), "~/"), "~/") + "~/" + cFileName).
               end.
           end.
           input close.
       end.

       return lSuccess.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
       end catch.
   end method.

   method public logical OsDelete
      (nodeName as character):
      /* Delete file/directory. */
      if GetFullPath(nodeName) eq ? then
         return true.

      os-delete value(nodeName) recursive.
      return os-error eq 0.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
      end catch.
   end method.

   method public logical OsDelete
       (nodeName as character, nameMask as character):
	/* Delete from a directory all files/directories having the given name mask. */
       def var cFileName as char    no-undo.
       def var lSuccess  as logical no-undo init yes.

       if Util:IsEmpty(nodeName) or not IsDir(nodeName) then
           return false.
       if IsDir(nodeName) then do:
           input from os-dir(nodeName).
           repeat
               on error undo, throw:
               import cFileName.
               if cFileName matches(nameMask) then
                assign lSuccess = lSuccess and OsDelete(trim(replace(nodeName, chr(92), "~/"), "~/") + "~/" + cFileName).
           end.
           input close.
           return lSuccess.
       end.

       return false.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
       end catch.
   end method.

   method public logical OsMove
       (sourceNode as character, targetNode as character):
	/* Move a file/directory to another place under the file system. */
       def var cFileName     as char no-undo.
       def var cFullFileName as char no-undo.
       def var lSuccess      as logi no-undo.

       if Util:IsEmpty(sourceNode) or Util:IsEmpty(targetNode) then
       if (not IsDir(sourceNode) and not IsFile(sourceNode)) or not IsDir(targetNode) then
           return false.

       assign lSuccess = OsCopy(sourceNode, targetNode).
       if lSuccess then
           assign lSuccess = OsDelete(sourceNode).

       return lSuccess.

       catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return false.
       end catch.
   end method.

   method public character GetFullPath (nodeName as character):
      if not Util:IsEmpty(nodeName) then do:
         file-info:file-name = replace(nodeName, chr(92), '~/':u).
         return file-info:full-pathname.
      end.
      return ?.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ?.
      end catch.
   end method.

   method public void Reset():
   end method.

   method public character GetExtension
      (nodeName as character):

      if not Util:IsEmpty(nodeName) and
         index(nodeName, '.':u) gt 0 then
         return substring(nodeName, r-index(nodeName, '.':u) + 1, -1, 'character':u).
      return ''.

      catch appError as Progress.Lang.Error :
          ThrowError(input appError).
          delete object appError.
          return ''.
      end catch.
   end method.
end class.
