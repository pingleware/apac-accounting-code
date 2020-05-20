       01  WX-INTEGRATE.
           03  WX-INT        PIC  X(01).
           03  FILLER        PIC  X(01).
001070     03  WX-DEBGL      PIC  9(06)    COMP-3.
001080     03  WX-CREGL      PIC  9(06)    COMP-3.
001090     03  WX-INTGL      PIC  9(06)    COMP-3.
001100     03  WX-BNKGL      PIC  9(06)    COMP-3.
001110     03  WX-UNPROF     PIC  9(06)    COMP-3.
001120     03  WX-REDGL      PIC  9(06)    COMP-3.
001130     03  WX-ADJGL      PIC  9(06)    COMP-3.
001140     03  WX-RLGL       PIC  9(06)    COMP-3.
001150     03  WX-DSGL       PIC  9(06)    COMP-3. 
	   03  WX-PETGL      PIC  9(06)    COMP-3.
       01  WX-LEDTRF.
	   03  WX-DATE	     PIC  9(08)    COMP-3.
           03  WX-TYPE       PIC  9(02)    COMP-5.
	   03  WX-LEDG.
	       05  WX-TRN    PIC  9(02).
	       05  FILLER    PIC  X(02).
           03  WX-AC         PIC  9(06)    COMP-3.
           03  WX-NAR.
               05  WX-DESC   PIC  X(13).
               05  WX-BOOK   PIC  X(27).
	   03  WX-VALUE      PIC S9(09)V99 COMP-3.
