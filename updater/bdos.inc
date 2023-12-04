;
; MSX-DOS definitions
;

BDOS			equ 5

BdosCall: MACRO function
	ld		c, function
	call	BDOS
ENDM

_TERM0			equ #00					; Program terminate
_CONIN			equ #01					; Console input
_CONOUT			equ #02					; Console output
_AUXIN			equ #03					; Auxiliary input
_AUXOUT			equ #04					; Auxiliary output
_LSTOUT			equ #05					; Printer output
_DIRIO			equ #06					; Direct console I/O
_DIRIN			equ #07					; Direct console input
_INNOE			equ #08					; Console input without echo
_STROUT			equ #09					; String output
_BUFIN			equ #0A					; Buffered line input
_CONST			equ #0B					; Console status
_CPMVER			equ #0C					; Return version number
_DSKRST			equ #0D					; Disk reset
_SELDSK			equ #0E					; Select disk
_FOPEN			equ #0F					; Open file
_FCLOSE			equ #10					; Close file
_SFIRST			equ #11					; Search for first
_SNEXT			equ #12					; Search for next
_FDEL			equ #13					; Delete file
_RDSEQ			equ #14					; Sequencial read
_WRSEQ			equ #15					; Sequencial write
_FMAKE			equ #16					; Create file
_FREN			equ #17					; Rename file
_LOGIN          equ #18					; Get login vector
_CURDRV			equ #19					; Get current drive
_SETDTA			equ #1A					; Set disk transfer address
_ALLOC			equ #1B					; Get allocation information
_RDRND			equ #21					; Random read
_WRRND			equ #22					; Random write
_FSIZE			equ #23					; Get file size
_SETRND			equ #24					; Set random record
_WRBLK			equ #26					; Random block write
_RDBLK			equ #27					; Random block read
_WRZER			equ #28					; Random write with zero fill
_GDATE			equ #2A					; Get date
_SDATE			equ #2B					; Set date
_GTIME			equ #2C					; Get time
_STIME			equ #2D					; Set time
_VERIFY			equ #2E					; Set/reset verify flag
_RDABS			equ #2F					; Absolute sector read
_WRABS			equ #30					; Absolute sector write
_DPARM			equ #31					; Get disk parameters
_FFIRST			equ #40					; Find first entry
_FNEXT			equ #41					; Find next entry
_FNEW			equ #42					; Find equ entry
_OPEN			equ #43					; Open file handle
_CREATE			equ #44					; Create file handle
_CLOSE			equ #45					; Close file handle
_ENSURE			equ #46					; Ensure file handle
_DUP			equ #47					; Duplicate file handle
_READ			equ #48					; Read from file handle
_WRITE			equ #49					; Write to file handle
_SEEK			equ #4A					; Move file handle pointer
_IOCTL			equ #4B					; I/O control for devices
_HTEST			equ #4C					; Test file handle
_DELETE			equ #4D					; Delete file or subdirectory
_RENAME			equ #4E					; Rename file or subdirectory
_MOVE			equ #4F					; Move file or subdirectory
_ATTR			equ #50					; Get/set file attributes
_FTIME			equ #51					; Get/set file date and time
_HDELETE		equ #52					; Delete file handle
_HRENAME		equ #53					; Rename file handle
_HMOVE			equ #54					; Move file handle
_HATTR			equ #55					; Get/set file handle attributes
_HFTIME			equ #56					; Get/set file handle date and time
_GETDTA			equ #57					; Get disk transfer address
_GETVFY			equ #58					; Get verify flag setting
_GETCD			equ #59					; Get current directory
_CHDIR			equ #5A					; Change current directory
_PARSE			equ #5B					; Parse pathname
_PFILE			equ #5C					; Parse filename
_CHKCHR			equ #5D					; Check character
_WPATH			equ #5E					; Get whole path string
_FLUSH			equ #5F					; Flush disk buffers
_FORK			equ #60					; Fork a child process
_JOIN			equ #61					; Rejoin parent process
_TERM			equ #62					; Terminate with error code
_DEFAB			equ #63					; Define abort exit routine
_DEFER			equ #64					; Define disk error handler routine
_ERROR			equ #65					; GFet previous error code
_EXPLAIN		equ #66					; Explain error code
_FORMAT			equ #67					; Format a disk
_RAMD			equ #68					; Create or destroy RAM disk
_BUFFER			equ #69					; Allocate sector buffers
_ASSGIN			equ #6A					; Logical drive assignment
_GENV			equ #6B					; Get environment item
_SENV			equ #6C					; Set environment item
_FENV			equ #6D					; Find environment item
_DSKCHK			equ #6E					; Get/set disk check status
_DOSVER			equ #6F					; Get MSX DOS version number
_REDIR			equ #70					; Get/set redirection status

; Errors
ERROR:
.NCOMP			equ #FF					; Incompatible disk
.WRERR			equ #FE					; Write error
.DISK			equ #FD					; Disk error
.NRDY			equ #FC					; Not ready
.VERFY			equ #FB					; Verify error
.DATA			equ #FA					; Data error
.RNF			equ #F9					; Sector not found
.WPROT			equ #F8					; Write protected disk
.UFORM			equ #F7					; Unformatted disk
.NDOS			equ #F6					; Not a DOS disk
.WDISK			equ #F5					; Wrong disk
.WFILE			equ #F4					; Wrong disk for file
.SEEK			equ #F3					; Seek error
.IFAT			equ #F2					; Bad file allocation table
.NOUPB			equ #F1					; --
.IFORM			equ #F0					; Cannot format this drive
.INTER			equ #DF					; Internal error
.NORAM			equ #DE					; Not enough memory
.IBDOS			equ #DC					; Invalid MSX-DOS call
.IDRV			equ #DB					; Invalid drive
.IFNM			equ #DA					; Invalid filename
.IPATH			equ #D9					; Invalid pathname
.PLONG			equ #D8					; Pathname too long
.NOFIL			equ #D7					; File not found
.NODIR			equ #D6					; Directory not found
.DRFUL			equ #D5					; Root directory full
.DKFUL			equ #D4					; Disk full
.DUPF			equ #D3					; Duplicate filename
.DIRE			equ #D2					; Invalid directory move
.FILRO			equ #D1					; Read only file
.DIRNE			equ #D0					; Directory not empty
.IATTR			equ #CF					; Invalid attributes
.DOT			equ #CE					; Invalid . or .. operation
.SYSX			equ #CD					; System file exists
.DIRX			equ #CC					; Directory exists
.FILEX			equ #CB					; File exists
.FOPEN			equ #CA					; File already in use
.OV64K			equ #C9					; Cannot transfer above 64K
.FILE			equ #C8					; File allocation error
.EOF			equ #C7					; End of file
.ACCV			equ #C6					; File access violation
.IPROC			equ #C5					; Invalid process id
.NHAND			equ #C4					; No spare file handles
.IHAND			equ #C3					; Invalid file handle
.NOPEN			equ #C2					; File handle not open
.IDEV			equ #C1					; Invalid device operation
.IENV			equ #C0					; Invalid environment string
.ELONG			equ #BF					; Environment string too long
.IDATE			equ #BE					; Invalid date
.ITIME			equ #BD					; Invalid time
.RAMDX			equ #BC					; RAM disk (drive H ) already exists
.NRAMD			equ #BB					; RAM disk does not exist
.HDEAD			equ #BA					; File handle has been deleted
.EOL			equ #B9					; Internal error. Should never occur
.ISBFN			equ #B8					; Invalid sub-function number
.STOP			equ #9F					; Ctrl-STOP pressed
.CTRLC			equ #9E					; Ctrl-C pressed
.ABORT			equ #9D					; Disk operation aborted
.OUTERR			equ #9C					; Error on standard output
.INERR			equ #9B					; Error on standard input
.BADCOM			equ #8F					; Wrong version of COMMAND
.BADCM			equ #8E					; Unrecognized command
.BUFUL			equ #8D					; Command too long
.OKCMD			equ #8C					; --
.IPARM			equ #8B					; Invalid parameter
.INP			equ #8A					; Too many parameters
.NOPAR			equ #89					; Missing parameter
.IOPT			equ #88					; Invalid option
.BADNO			equ #87					; Invalid number
.NOHELP			equ #86					; File for HELP not found
.BADVER			equ #85					; Wrong version of MSX-DOS
.NOCAT			equ #84					; Cannot concatenate destination file
.BADEST			equ #83					; Cannot create destination file
.COPY			equ #82					; File cannot be copied onto itself
.OVDEST			equ #81					; Cannot overwrite previous destination file

; Allocate segment parameters
USER_SEGMENT		equ 0					; Register A
SYSTEM_SEGMENT		equ 1					; "        "
PRIMARY_MAPPER		equ 0					; Register B 

; Open file flags (_OPEN)
OPEN_NO_WRITE		equ 1
OPEN_NO_READ		equ 2
OPEN_INHERITABLE	equ 4

; Seek file flags
SEEK_SET			equ 0
SEEK_CUR			equ 1
SEEK_END			equ 2

; File attributes (bit mask)
FILE_READ_ONLY		equ 1					; 
FILE_HIDDEN			equ 2					;
FILE_SYSTEM			equ 4					; 
FILE_VOLUME_NAME	equ 8					; 
FILE_DIRECTORY		equ 16				; 
FILE_ARCHIVE		equ 32				; 
FILE_RESERVED		equ 64				; 
FILE_DEVICE			equ 128				;

; Disk error handler routine parameters in register C
ERROR_WRITING		equ 1					; Set if writing
ERROR_NO_IGNORE		equ 2					; Set if ignore not recommended
ERROR_AUTO_ABORT	equ 4					; Set if auto-abort suggested
ERROR_VALID_SECTOR	equ 8					; Set if sector number is valid 


; Disk error handler routine return codes
CALL_SYSTEM_ERROR	equ 0					; Call system error routine
ERROR_ABORT			equ 1					; Abort
ERROR_RETRY			equ 2					; Retry
ERROR_IGNORE		equ 3					; Ignore


;  Params _RAMD
DESTROY_RAMDISK		equ 0
GET_RAMDISK_SIZE	equ #FF
CREATE_RAMDISK		equ 1					; CREATE_RAMDISK+n equ 1+n

; Misc
CR					equ 13				; Carridge Return
LF					equ 10				; Line Feed
LE					equ '$'				; Line end
UPPER_CASE_MASK		equ #DF

CMD_LENGTH			equ #80
CMD_LINE			equ #82

RAMAD0				equ #F341				; slotid DOS ram page 0
RAMAD1				equ #F342				; slotid DOS ram page 1
RAMAD2				equ #F343				; slotid DOS ram page 2
RAMAD3				equ #F344				; slotid DOS ram page 3
SECBUF				equ #F34D				; pointer to sectorbuffer, can be used by the diskdriver
