 
/*------------------------------------------------------------------------
   File        : FileSystem
   Purpose     : Static file system utility class  
   Syntax      : 
   Description : 
   Author(s)   : Andrei
   Created     : Wed May 15 09:12:26 EEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS com.mrg.framework.util.FileSystem: 
    
    /* creates a folder name from 2 parts */
    METHOD PUBLIC STATIC CHARACTER BuildFolderName (part1 AS CHARACTER, part2 AS CHARACTER):
        RETURN RIGHT-TRIM (part1, '/~\':U) + GetOSFileSeparator () + LEFT-TRIM (part2, '/~\':U).        
    END METHOD.        

    /* creates a folder name from 3 parts */
    METHOD PUBLIC STATIC CHARACTER BuildFolderName (part1 AS CHARACTER, part2 AS CHARACTER, part3 AS CHARACTER):
        RETURN RIGHT-TRIM (part1, '/~\':U) + GetOSFileSeparator () + LEFT-TRIM (part2, '/~\':U) + GetOSFileSeparator () + LEFT-TRIM (part3, '/~\':U).        
    END METHOD.        

    /* creates a folder name from 4 parts */
    METHOD PUBLIC STATIC CHARACTER BuildFolderName (part1 AS CHARACTER, part2 AS CHARACTER, part3 AS CHARACTER, part4 AS CHARACTER):
        RETURN RIGHT-TRIM (part1, '/~\':U) + GetOSFileSeparator () + LEFT-TRIM (part2, '/~\':U) + GetOSFileSeparator () + LEFT-TRIM (part3, '/~\':U) + GetOSFileSeparator () + LEFT-TRIM (part4, '/~\':U).        
    END METHOD.        

    /* returns true if the folder exists on disk */
    METHOD PUBLIC STATIC LOGICAL FolderExist (INPUT folderPath AS CHARACTER):
        FILE-INFO:FILE-NAME = folderPath.
        RETURN FILE-INFO:FULL-PATHNAME <> ? AND FILE-INFO:FILE-TYPE MATCHES "*D*":U.
    END METHOD.
                                
    /* returns the operating system specific file separator */
    METHOD PUBLIC STATIC CHARACTER GetOSFileSeparator ():
        RETURN (IF OPSYS = 'UNIX':U THEN '/':U ELSE '~\':U).
    END METHOD.        
                            
    /* File name in Unix-style */
    METHOD PUBLIC STATIC CHARACTER UnixName (INPUT filePath AS CHARACTER):
        IF filePath = ? THEN /* Prevent endless loop in the repeat */
            RETURN ?.
        filePath = REPLACE(filePath, '~\':U, '/':U).
        REPEAT:
            IF INDEX (filePath, '//':U) = 0 THEN
                LEAVE.
            filePath = REPLACE(filePath, '//':U, '/':U).
        END.
        RETURN filePath.
    END METHOD.                            
                            
    /* Abstract file pathname from a full path */
    METHOD PUBLIC STATIC CHARACTER FilePath (INPUT filePath AS CHARACTER):
        DEFINE VARIABLE slashIdx     AS INTEGER NO-UNDO.
        DEFINE VARIABLE backSlashIdx AS INTEGER NO-UNDO.
        ASSIGN
            slashIdx     = R-INDEX(filePath, '/':U)
            backSlashIdx = R-INDEX(filePath, '~\':U).
        IF slashIdx > backSlashIdx THEN
            RETURN SUBSTRING(filePath, 1, slashIdx - 1).
        ELSE IF backSlashIdx > slashIdx THEN
            RETURN SUBSTRING(filePath, 1, backSlashIdx - 1).
        ELSE
            RETURN '':U.            
    END METHOD.
                        
    /* Create the folder part of the given filename */
    METHOD PUBLIC STATIC VOID CreateFilePath (INPUT fileName AS CHARACTER):
        CreateFolder (FilePath (fileName)).
    END METHOD.         
                   
    /* Create a folder tree */
    METHOD PUBLIC STATIC VOID CreateFolder (INPUT folderPath AS CHARACTER):
        DEFINE VARIABLE tempPath AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pathIdx  AS INTEGER   NO-UNDO.
        
        folderPath = UnixName(folderPath).

        DO pathIdx = 1 TO NUM-ENTRIES (folderPath, '/':U):
            tempPath = tempPath + (IF pathIdx = 1 THEN '':U ELSE '/':U) + entry (pathIdx, folderPath, '/':U).
            IF tempPath = '/':U THEN
                NEXT.
            FILE-INFO:FILE-NAME = tempPath.
            IF FILE-INFO:FULL-PATHNAME = ? OR NOT FILE-INFO:FILE-TYPE MATCHES "*D":U THEN 
            DO:
                OS-CREATE-DIR value (tempPath).
                IF OS-ERROR <> 0 THEN
                    UNDO, THROW NEW Progress.Lang.AppError(ErrorDescription(OS-ERROR)).
            END.
        END.
    END METHOD.                   

    /* Operating system error description */
    METHOD PUBLIC STATIC CHARACTER ErrorDescription (INPUT osError AS INTEGER):
        DEFINE VARIABLE errorDescription AS CHARACTER NO-UNDO.
        CASE osError:
            WHEN 0 THEN 
                errorDescription = "No error".
            WHEN 1 THEN 
                errorDescription = "Not owner".
            WHEN 2 THEN 
                errorDescription = "No such file or directory".
            WHEN 3 THEN 
                errorDescription = "Interrupted System call".
            WHEN 4 THEN 
                errorDescription = "I/O error".
            WHEN 5 THEN 
                errorDescription = "Bad file number".
            WHEN 6 THEN 
                errorDescription = "No more processes".
            WHEN 7 THEN 
                errorDescription = "Not enough core memory".
            WHEN 8 THEN 
                errorDescription = "Permission denied".
            WHEN 9 THEN 
                errorDescription = "Bad Address".
            WHEN 10 THEN 
                errorDescription = "File exists".
            WHEN 11 THEN 
                errorDescription = "No such device".
            WHEN 12 THEN 
                errorDescription = "Not a directory".
            WHEN 13 THEN 
                errorDescription = "Is a directory".
            WHEN 14 THEN 
                errorDescription = "File table overflow".
            WHEN 15 THEN 
                errorDescription = "Too many open files".
            WHEN 16 THEN 
                errorDescription = "File too large".
            WHEN 17 THEN 
                errorDescription = "No space left on device".
            WHEN 18 THEN 
                errorDescription = "Directory not empty".
            OTHERWISE 
                errorDescription = "Unmapped error (ABL default)".
        END CASE.
        RETURN errorDescription.
    END METHOD.
                   
END CLASS.