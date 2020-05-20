000010 01  WS-USER-ID.
000020     03  WS-USER.
000030         05  WS-USE      PIC  9(03).
000040     03  WS-COMPANY.
000050         05  WS-COMPCHR  PIC  X(01) OCCURS 40.
	   03  WS-INSTR	       PIC  X(01).
	       88  APAC-END    VALUE "E".
	       88  APAC-INIT   VALUE "I".
	       88  APAC-RECV   VALUE "R".
	       88  APAC-RENEW  VALUE "X".
	   03  WS-STOCK-CODE.
	       05  WS-NEXT-PRG PIC  X(12).
	       05  WS-ACNO     PIC  X(06).
	   03  WS-DAYS	       PIC  9(02) COMP-5.
           03  WS-DEM          PIC  9(02) COMP-5.
000060     03  WS-USUB         PIC  9(04) COMP-5.
000070     03  WS-DRAW         PIC  9(02) VALUE 0.
000080     03  WS-CSS          PIC  X(01) VALUE "N".
000090     03  WS-OK           PIC  X(01) VALUE "N".
000100     03  WS-RET          PIC  X(01) VALUE " ".
	   03  TODAY-DDMMYY    PIC  9(08) COMP-5.
	   03  WS-CUR-YR       PIC  X(01) COMP-X.
	   03  WS-CUR-CEN      PIC  X(01) COMP-X.
	   03  WS-PRV-CEN      PIC  X(01) COMP-X.
	   03  WS-NXT-CEN      PIC  X(01) COMP-X.
	   03  WS-SLS-ORDNO.
	     05  WS-SLS-JOBNO  PIC  X(08) VALUE SPACES.
	   03  WS-CARDEX       PIC  X(01).
	   03  WS-DSKDRV       PIC  X(01) VALUE "N".
	   03  WS-APAC-SYSTEM  PIC  X(12).
	   03  WS-APAC-VERSION PIC  X(04).
	   03  WS-L-OR-N       PIC  X(08) VALUE "SYSTEMS\".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000340 01  WS0-PROGRAMS.
000350     03  WS0-MODULES.
000360         05  WS0-DTP     PIC  9(01).
000370         05  WS0-STP     PIC  9(01).
000380         05  WS0-HPD     PIC  9(01).
000390         05  WS0-CRP     PIC  9(01).
000400         05  WS0-GLP     PIC  9(01).
000410         05  WS0-POS     PIC  9(01).
000420         05  WS0-JCP     PIC  9(01).
000430         05  WS0-VHP     PIC  9(01).
000440         05  WS0-WAG     PIC  9(01).
000450         05  WS0-SAL     PIC  9(01).
	       05  WS0-LEV     PIC  9(01).
	       05  WS0-WHS     PIC  9(01).
	       05  WS0-RTE     PIC  9(01).
	       05  WS0-TEMP.
		   07  WS0-DBT PIC  X(06).
		   07  WS0-DSK PIC  X(01).
000470	   03  WS0-PRGM  REDEFINES WS0-MODULES.
000480         05  WS0-PMODS   PIC  X(20).
      *
      *   SYSTEM SECURITY
      *
       01  WS0-SECURITY.
	   03  WS0-NO	       PIC  9(03) VALUE ZERO.
	   03  WS0-STATION.
	       05  WS-TERM     PIC  9(03).
	   03  WS0-NAME        PIC  X(30) VALUE "ANYBODY".
	   03  WS0-PASS        PIC  X(08) VALUE SPACES.
	   03  WS0-DATE        PIC  9(08) COMP-3.
	   03  WS0-INSTR       PIC  X(01) VALUE "C".
	   03  WS0-GL	       PIC  X(01) VALUE "N".
	   03  WS0-GLLEV       PIC  9(01) VALUE ZERO.
	   03  WS0-STOCK       PIC  X(01) VALUE "N".
	   03  WS0-STLEV       PIC  9(01) VALUE ZERO.
	   03  WS0-DEBT        PIC  X(01) VALUE "N".
	   03  WS0-DBLEV       PIC  9(01) VALUE ZERO.
	   03  WS0-CRED        PIC  X(01) VALUE "N".
	   03  WS0-CRLEV       PIC  9(01) VALUE ZERO.
	   03  WS0-PARM        PIC  X(01) VALUE "N".
	   03  WS0-PALEV       PIC  9(01) VALUE ZERO.
	   03  WS0-SALES       PIC  X(01) VALUE "Y".
	   03  WS0-SALEV       PIC  9(01) VALUE ZERO.
	   03  WS0-JOBS        PIC  X(01) VALUE "N".
	   03  WS0-JBLEV       PIC  9(01) VALUE ZERO.
	   03  WS0-WAGES       PIC  X(01) VALUE "N".
	   03  WS0-WGLEV       PIC  9(01) VALUE ZERO.
	   03  WS0-ADM	       PIC  X(01) VALUE "N".
	   03  WS0-DIV	       PIC  9(03) VALUE ZERO.
	   03  WS0-SP	       PIC  9(02) VALUE ZERO.
	   03  WS0-ORD	       PIC  9(06) COMP-5 VALUE ZERO.
	   03  WS0-SEQ	       PIC  9(01) VALUE ZERO.
	   03  FILLER	       PIC  X(02) VALUE SPACES.
