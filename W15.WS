       01  W15-DATES.
004120	   03  W15-START       PIC  9(08).
004130     03  W15-SDTE-DMY REDEFINES W15-START.
004140	       05  W15-SDAY    PIC  9(02).
004150	       05  W15-SMNTH   PIC  9(02).
	       05  W15-SCENT   PIC  9(02).
004160	       05  W15-SYEAR   PIC  9(02).
	   03  W15-SDMY REDEFINES W15-START.
	       05  W15-SDDMM   PIC  9(04).
	       05  W15-SCCYY   PIC  9(04).
004170	   03  W15-S-YMD       PIC  9(08).
004180     03  W15-STRT-YMD REDEFINES W15-S-YMD.
	       05  W15-SCC     PIC  9(02).
004190	       05  W15-SYY     PIC  9(02).
004200	       05  W15-SMM     PIC  9(02).
004210	       05  W15-SDD     PIC  9(02).
004120	   03  W15-END	       PIC  9(08).
004130     03  W15-EDTE-DMY REDEFINES W15-END.
004140	       05  W15-EDAY    PIC  9(02).
004150	       05  W15-EMNTH   PIC  9(02).
	       05  W15-ECENT   PIC  9(02).
004160	       05  W15-EYEAR   PIC  9(02).
	   03  W15-EDMY REDEFINES W15-END.
	       05  W15-EDDMM   PIC  9(04).
	       05  W15-ECCYY   PIC  9(04).
004170	   03  W15-E-YMD       PIC  9(08).
004180     03  W15-END-YMD REDEFINES W15-E-YMD.
	       05  W15-ECC     PIC  9(02).
004190	       05  W15-EYY     PIC  9(02).
004200	       05  W15-EMM     PIC  9(02).
004210	       05  W15-EDD     PIC  9(02).
