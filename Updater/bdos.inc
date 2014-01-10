;
; MSX-DOS definitions
;

BDOS			= 5

	MACRO BdosCall function
		ld		c, function
		call	BDOS
	ENDM

_TERM0			= $00					; Program terminate
_CONIN			= $01					; Console input
_CONOUT			= $02					; Console output
_AUXIN			= $03					; Auxiliary input
_AUXOUT			= $04					; Auxiliary output
_LSTOUT			= $05					; Printer output
_DIRIO			= $06					; Direct console I/O
_DIRIN			= $07					; Direct console input
_INNOE			= $08					; Console input without echo
_STROUT			= $09					; String output
_BUFIN			= $0A					; Buffered line input
_CONST			= $0B					; Console status
_CPMVER			= $0C					; Return version number
_DSKRST			= $0D					; Disk reset
_SELDSK			= $0E					; Select disk
_FOPEN			= $0F					; Open file
_FCLOSE			= $10					; Close file
_SFIRST			= $11					; Search for first
_SNEXT			= $12					; Search for next
_FDEL			= $13					; Delete file
_RDSEQ			= $14					; Sequencial read
_WRSEQ			= $15					; Sequencial write
_FMAKE			= $16					; Create file
_FREN			= $17					; Rename file
_LOGIN          = $18					; Get login vector
_CURDRV			= $19					; Get current drive
_SETDTA			= $1A					; Set disk transfer address
_ALLOC			= $1B					; Get allocation information
_RDRND			= $21					; Random read
_WRRND			= $22					; Random write
_FSIZE			= $23					; Get file size
_SETRND			= $24					; Set random record
_WRBLK			= $26					; Random block write
_RDBLK			= $27					; Random block read
_WRZER			= $28					; Random write with zero fill
_GDATE			= $2A					; Get date
_SDATE			= $2B					; Set date
_GTIME			= $2C					; Get time
_STIME			= $2D					; Set time
_VERIFY			= $2E					; Set/reset verify flag
_RDABS			= $2F					; Absolute sector read
_WRABS			= $30					; Absolute sector write
_DPARM			= $31					; Get disk parameters
_FFIRST			= $40					; Find first entry
_FNEXT			= $41					; Find next entry
_FNEW			= $42					; Find = entry
_OPEN			= $43					; Open file handle
_CREATE			= $44					; Create file handle
_CLOSE			= $45					; Close file handle
_ENSURE			= $46					; Ensure file handle
_DUP			= $47					; Duplicate file handle
_READ			= $48					; Read from file handle
_WRITE			= $49					; Write to file handle
_SEEK			= $4A					; Move file handle pointer
_IOCTL			= $4B					; I/O control for devices
_HTEST			= $4C					; Test file handle
_DELETE			= $4D					; Delete file or subdirectory
_RENAME			= $4E					; Rename file or subdirectory
_MOVE			= $4F					; Move file or subdirectory
_ATTR			= $50					; Get/set file attributes
_FTIME			= $51					; Get/set file date and time
_HDELETE		= $52					; Delete file handle
_HRENAME		= $53					; Rename file handle
_HMOVE			= $54					; Move file handle
_HATTR			= $55					; Get/set file handle attributes
_HFTIME			= $56					; Get/set file handle date and time
_GETDTA			= $57					; Get disk transfer address
_GETVFY			= $58					; Get verify flag setting
_GETCD			= $59					; Get current directory
_CHDIR			= $5A					; Change current directory
_PARSE			= $5B					; Parse pathname
_PFILE			= $5C					; Parse filename
_CHKCHR			= $5D					; Check character
_WPATH			= $5E					; Get whole path string
_FLUSH			= $5F					; Flush disk buffers
_FORK			= $60					; Fork a child process
_JOIN			= $61					; Rejoin parent process
_TERM			= $62					; Terminate with error code
_DEFAB			= $63					; Define abort exit routine
_DEFER			= $64					; Define disk error handler routine
_ERROR			= $65					; GFet previous error code
_EXPLAIN		= $66					; Explain error code
_FORMAT			= $67					; Format a disk
_RAMD			= $68					; Create or destroy RAM disk
_BUFFER			= $69					; Allocate sector buffers
_ASSGIN			= $6A					; Logical drive assignment
_GENV			= $6B					; Get environment item
_SENV			= $6C					; Set environment item
_FENV			= $6D					; Find environment item
_DSKCHK			= $6E					; Get/set disk check status
_DOSVER			= $6F					; Get MSX DOS version number
_REDIR			= $70					; Get/set redirection status

; Errors
ERROR:
.NCOMP			= $FF					; Incompatible disk
.WRERR			= $FE					; Write error
.DISK			= $FD					; Disk error
.NRDY			= $FC					; Not ready
.VERFY			= $FB					; Verify error
.DATA			= $FA					; Data error
.RNF			= $F9					; Sector not found
.WPROT			= $F8					; Write protected disk
.UFORM			= $F7					; Unformatted disk
.NDOS			= $F6					; Not a DOS disk
.WDISK			= $F5					; Wrong disk
.WFILE			= $F4					; Wrong disk for file
.SEEK			= $F3					; Seek error
.IFAT			= $F2					; Bad file allocation table
.NOUPB			= $F1					; --
.IFORM			= $F0					; Cannot format this drive
.INTER			= $DF					; Internal error
.NORAM			= $DE					; Not enough memory
.IBDOS			= $DC					; Invalid MSX-DOS call
.IDRV			= $DB					; Invalid drive
.IFNM			= $DA					; Invalid filename
.IPATH			= $D9					; Invalid pathname
.PLONG			= $D8					; Pathname too long
.NOFIL			= $D7					; File not found
.NODIR			= $D6					; Directory not found
.DRFUL			= $D5					; Root directory full
.DKFUL			= $D4					; Disk full
.DUPF			= $D3					; Duplicate filename
.DIRE			= $D2					; Invalid directory move
.FILRO			= $D1					; Read only file
.DIRNE			= $D0					; Directory not empty
.IATTR			= $CF					; Invalid attributes
.DOT			= $CE					; Invalid . or .. operation
.SYSX			= $CD					; System file exists
.DIRX			= $CC					; Directory exists
.FILEX			= $CB					; File exists
.FOPEN			= $CA					; File already in use
.OV64K			= $C9					; Cannot transfer above 64K
.FILE			= $C8					; File allocation error
.EOF			= $C7					; End of file
.ACCV			= $C6					; File access violation
.IPROC			= $C5					; Invalid process id
.NHAND			= $C4					; No spare file handles
.IHAND			= $C3					; Invalid file handle
.NOPEN			= $C2					; File handle not open
.IDEV			= $C1					; Invalid device operation
.IENV			= $C0					; Invalid environment string
.ELONG			= $BF					; Environment string too long
.IDATE			= $BE					; Invalid date
.ITIME			= $BD					; Invalid time
.RAMDX			= $BC					; RAM disk (drive H ) already exists
.NRAMD			= $BB					; RAM disk does not exist
.HDEAD			= $BA					; File handle has been deleted
.EOL			= $B9					; Internal error. Should never occur
.ISBFN			= $B8					; Invalid sub-function number
.STOP			= $9F					; Ctrl-STOP pressed
.CTRLC			= $9E					; Ctrl-C pressed
.ABORT			= $9D					; Disk operation aborted
.OUTERR			= $9C					; Error on standard output
.INERR			= $9B					; Error on standard input
.BADCOM			= $8F					; Wrong version of COMMAND
.BADCM			= $8E					; Unrecognized command
.BUFUL			= $8D					; Command too long
.OKCMD			= $8C					; --
.IPARM			= $8B					; Invalid parameter
.INP			= $8A					; Too many parameters
.NOPAR			= $89					; Missing parameter
.IOPT			= $88					; Invalid option
.BADNO			= $87					; Invalid number
.NOHELP			= $86					; File for HELP not found
.BADVER			= $85					; Wrong version of MSX-DOS
.NOCAT			= $84					; Cannot concatenate destination file
.BADEST			= $83					; Cannot create destination file
.COPY			= $82					; File cannot be copied onto itself
.OVDEST			= $81					; Cannot overwrite previous destination file

; Allocate segment parameters
USER_SEGMENT		= 0					; Register A
SYSTEM_SEGMENT		= 1					; "        "
PRIMARY_MAPPER		= 0					; Register B 

; Open file flags (_OPEN)
OPEN_NO_WRITE		= 1
OPEN_NO_READ		= 2
OPEN_INHERITABLE	= 4

; Seek file flags
SEEK_SET			= 0
SEEK_CUR			= 1
SEEK_END			= 2

; File attributes (bit mask)
FILE_READ_ONLY		= 1					; 
FILE_HIDDEN			= 2					;
FILE_SYSTEM			= 4					; 
FILE_VOLUME_NAME	= 8					; 
FILE_DIRECTORY		= 16				; 
FILE_ARCHIVE		= 32				; 
FILE_RESERVED		= 64				; 
FILE_DEVICE			= 128				;

; Disk error handler routine parameters in register C
ERROR_WRITING		= 1					; Set if writing
ERROR_NO_IGNORE		= 2					; Set if ignore not recommended
ERROR_AUTO_ABORT	= 4					; Set if auto-abort suggested
ERROR_VALID_SECTOR	= 8					; Set if sector number is valid 


; Disk error handler routine return codes
CALL_SYSTEM_ERROR	= 0					; Call system error routine
ERROR_ABORT			= 1					; Abort
ERROR_RETRY			= 2					; Retry
ERROR_IGNORE		= 3					; Ignore


;  Params _RAMD
DESTROY_RAMDISK		= 0
GET_RAMDISK_SIZE	= $FF
CREATE_RAMDISK		= 1					; CREATE_RAMDISK+n = 1+n

; Misc
CR					= 13				; Carridge Return
LF					= 10				; Line Feed
LE					= '$'				; Line end
UPPER_CASE_MASK		= $DF

CMD_LENGTH			= $80
CMD_LINE			= $82

COM_START_ADDR		= $100

RAMAD0				= $F341				; slotid DOS ram page 0
RAMAD1				= $F342				; slotid DOS ram page 1
RAMAD2				= $F343				; slotid DOS ram page 2
RAMAD3				= $F344				; slotid DOS ram page 3
SECBUF				= $F34D				; pointer to sectorbuffer, can be used by the diskdriver
