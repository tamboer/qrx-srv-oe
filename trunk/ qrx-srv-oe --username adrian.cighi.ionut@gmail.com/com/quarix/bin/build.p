/*------------------------------------------------------------------------
    File        : build.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Marian
    Created     : Fri Oct 09 09:51:25 EEST 2009
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

/* ***************************  Definitions  ************************** */
define variable srcPath   as character no-undo.
define variable binPath   as character no-undo.

define temp-table ttDir no-undo
   field id as integer
   field parentDirId as integer
   field dirName as character
   field fullDirName as character
   field dirProcessed as logical
   index uq_id as primary unique dirName
   index uq_dir_name as unique parentDirId dirName.

define temp-table ttFile no-undo
   field id as integer
   field parentDirId as integer
   field fileName  as character
   index uq_filename as primary unique parentDirId filename.

define temp-table ttErrors no-undo
   field fileId as integer
   field errorNumber as integer
   field errorText as character
   index uq_fileid_errorno as primary unique fileId errorNumber.

define stream _strOs.
/* ***************************  Main Block  *************************** */


assign
   srcPath                    = entry(1, session:parameter)
   binPath                    = entry(2, session:parameter) no-error.
file-information:file-name = srcPath.
if file-information:file-type eq ? then do:
   put unformatted substitute('Source code directory do not exist: &1.', srcPath).
   return.
end.

file-information:file-name = binPath.
if file-information:file-type eq ? then do:
   put unformatted substitute('Build directory do not exist: &1.', binPath).
   return.
end.

run buildSrcList(srcPath).
run createTargetDirs.
run addDirsToPropath.
run compileFiles.

/* build compile list */
procedure buildSrcList:
   define input parameter dirName   as character no-undo.

   define buffer ttDir for ttDir.
   define buffer ttFile for ttFile.

   define variable fileName as character no-undo.
   define variable fileType as character no-undo.

   define variable cDirList as character no-undo.
   define variable iCount as integer no-undo.
   define variable iDirId as integer initial 1 no-undo.
   define variable iFileId as integer initial 1 no-undo.
   define variable iCurDirId as integer initial -1 no-undo.

   for each ttDir
      by ttDir.id desc:
      iDirId = ttDir.id + 1.
      leave.
   end.

   file-information:file-name = dirName.
   if file-information:file-type eq ? or
      index(file-information:file-type, 'd':u) eq 0 or
      index(file-information:file-type, 'r':u) eq 0 then
      return.

   if not can-find(first ttDir where ttDir.fullDirName eq file-information:full-pathname) then do:
       create ttDir.
       assign
          ttDir.id          = iDirId
          iDirId            = iDirId + 1
          ttDir.parentDirId = iCurDirId
          iCurDirId         = ttDir.id
          ttDir.dirName     = right-trim(trim(replace(file-information:full-pathname, srcPath, ''),chr(92)),'~/')
          ttDir.fullDirName = file-information:full-pathname.
   end.
   else  for each ttDir
      where ttDir.fullDirName eq srcPath:
         iCurDirId = ttDir.id.
   end.

   input stream _strOs from os-dir (file-information:file-name).

   repeat:
      import stream _strOs ^ fileName fileType.
      file-information:file-name = filename.
      if index(fileType, 'D':u) gt 0 and
         r-index(fileName, '.') lt length(fileName) then do:
             create ttDir.
             assign
                ttDir.id          = iDirId
                iDirId            = iDirId + 1
                ttDir.parentDirId = iCurDirId
                ttDir.dirName     = trim(trim(replace(file-information:full-pathname, srcPath, ''),chr(92)),'~/')
                ttDir.fullDirName = file-information:full-pathname.
      end.

      if index(fileType, 'F':u) gt 0 and
         can-do('p,w,cls', entry(num-entries(file-information:full-pathname, '.'), file-information:full-pathname,'.')) and
         not  can-find(first ttFile where ttFile.filename eq filename) then do:
         create ttFile.
         assign
            ttFile.id          = iFileId
            ttFile.parentDirId = iCurDirId
            ttFile.filename    = filename
            iFileId            = iFileId + 1 no-error.
      end.
   end.

   for each ttDir
      where ttDir.fullDirName eq dirName:
         ttDir.dirProcessed = true.
   end.

   for each ttDir
      where not ttDir.dirProcessed:
      run buildSrcList(ttDir.fullDirName).
   end.
end procedure.

procedure addDirsToPropath:
   for each ttDir:
      propath = substitute("&1,&2", propath, ttDir.fullDirName).
   end.
end procedure.


procedure createTargetDirs:
   for each ttDir:
      os-create-dir value(replace(ttDir.fullDirName, srcPath, binPath)).
   end.
end procedure.

procedure compileFiles:
   define variable iCount as integer no-undo.

   for each ttFile,
      first ttDir where ttDir.id eq ttFile.parentDirId:
      compile value(ttFile.fileName) save = true into value(replace(ttDir.fullDirName, srcPath, binPath)) no-error.
      if error-status:num-messages gt 0 then do iCount = 1 to error-status:num-messages:
         create ttErrors.
         assign
            ttErrors.fileId = ttFile.id
            ttErrors.errorNumber = error-status:get-number (iCount)
            ttErrors.errorText   = error-status:get-message (iCount).
      end.
   end.
end procedure.


quit.


