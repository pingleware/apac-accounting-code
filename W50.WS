000010 01  W50-TABLE.
000020     03  W50-TDETAIL.
000030         05  FILLER      PIC  X(27)      VALUE
000040                         "!#$%&'()*+,-./0123456789:;<".
000050         05  FILLER      PIC  X(27)      VALUE
000060                         "=>?@ABCDEFGHIJKLMNOPQRSTUVW".
000070         05  FILLER      PIC  X(12)      VALUE
000080                         "XYZ[\]^`{|}~".
000090     03  W50-TAB1 REDEFINES W50-TDETAIL.
000100         05  W50-FCHAR  OCCURS 66.
000110             07  W50-CHAR PIC X(01).
000120     03  W50-SFKEY       PIC  9(04)  COMP-5.
000130     03  W50-RESULT      PIC  9(01).
000140         88  IX-FOUND    VALUE 0.
000150     03  W50-STATUS      PIC  X(02).
000160	   03  W50-LENGTH      PIC  9(08)  COMP-5.
000170	   03  W50-S1	       PIC  9(02)  COMP-5.
000180     03  W50-S2          PIC  9(02)  COMP-5.
           03  W50-EXP.
000190         05  W50-OPT     PIC  9(01).
000200     03  W50-END         PIC  9(02)  COMP-5.
000210     03  W50-DATA.
000220         05  W50-CCHAR   PIC  X(01)  OCCURS 40.
000230     03  W50-DATA-LEFT REDEFINES W50-DATA.
000240         05  FILLER      PIC  X(01).
000250         05  W50-D39     PIC  X(39).
000260     03  W50-DATA-THE REDEFINES W50-DATA.
000270         05  W50-THE     PIC  X(04).
000280         05  W50-D36.
000290             07  W50-D26 PIC  X(26).
000300             07  FIILER  PIC  X(10).
000310     03  W50-DATA-20 REDEFINES W50-DATA.
000320         05  W50-20.
000330           07  W50-ACODE PIC  X(10).
000340           07  FILLER    PIC  X(10).
000350         05  FILLER      PIC  X(20).
000360     03  W50-DATA-30 REDEFINES W50-DATA.
000370         05  W50-30.
000380           07  FILLER    PIC  X(01).
000390           07  W50-29    PIC  X(29).
000400         05  FILLER      PIC  X(10).
000410     03  W50-STOCK REDEFINES W50-DATA.
	       05  W50-EXT-ITEM.
000420		 07  W50-ITEM.
000430		   09  W50-1	     PIC  X(01).
000440		   09  W50-13.
000450		     11  W50-9.
		       13  W50-5.
000460			 15  W50-I23 PIC  X(02).
			 15  FILLER  PIC  X(03).
000470		       13  FILLER    PIC  X(04).
000480		     11  FILLER      PIC  X(04).
000490		 07  W50-ITM	     PIC  X(04).
	       05  W50-EX-CDE REDEFINES W50-EXT-ITEM.
		 07  FILLER	     PIC  X(01).
		 07  W50-17	     PIC  X(17).
000490	       05  FILLER	     PIC  X(26).
000500     03  W50-NAME REDEFINES W50-DATA.
000510         05  W50-ENTRY.
000520           07  W50-DENT.
000530             09  W50-N1  PIC  X(01).
000540             09  W50-N23 PIC  X(02).
000550             09  FILLER  PIC  X(04).
000560           07 FILLER     PIC  X(01).
	       05  W50-CPY     PIC  X(30).
000570	       05  FILLER      PIC  X(02).
000580     03  W50-SNAME REDEFINES W50-DATA.
000590         05  W50-SURNAME.
000600           07  W50-SN1   PIC  X(01).
000610           07  W50-S23   PIC  X(23).
000620         05  FILLER      PIC  X(16).
000630     03  W50-BCODE REDEFINES W50-DATA.
000640         05  W50-BAR.
000650           07  W50-BENT.
000660             09  W50-B1  PIC  X(01).
000670             09  W50-B23 PIC  X(02).
000680             09  FILLER  PIC  X(13).
000690         05  FILLER      PIC  X(24).
000700     03  W50-LOOKUP      PIC  X(50).
000710     03  W50-ASCII       PIC  X(01).
000720     03  W50-BIN REDEFINES W50-ASCII
000730                         PIC  9(02) COMP-5.
	   03  W50-ALLOC-BC    PIC  X(01).
	   03  W50-BAR-CODE    PIC  X(01).
	   03  W50-BCLNGTH     PIC  9(02) COMP-5.
	   03  W50-BC.
	       05  W50-BCC     PIC  X(01) OCCURS 14.
	   03  W50-BCD14 REDEFINES W50-BC.
	       05  W50-BC14    PIC  9(14).
	   03  W50-BCD13 REDEFINES W50-BC.
	       05  W50-BC13    PIC  9(13).
	       05  FILLER      PIC  X(01).
	   03  W50-BCD12 REDEFINES W50-BC.
	       05  W50-BC12    PIC  9(12).
	       05  FILLER      PIC  X(02).
	   03  W50-BCD11 REDEFINES W50-BC.
	       05  W50-BC11    PIC  9(11).
	       05  FILLER      PIC  X(03).
	   03  W50-BCD10 REDEFINES W50-BC.
	       05  W50-BC10    PIC  9(10).
	       05  FILLER      PIC  X(04).
	   03  W50-BCD9 REDEFINES W50-BC.
	       05  W50-BC9     PIC  9(09).
	       05  FILLER      PIC  X(05).
	   03  W50-BCD8 REDEFINES W50-BC.
	       05  W50-BC8     PIC  9(08).
	       05  FILLER      PIC  X(06).
	   03  W50-BCD7 REDEFINES W50-BC.
	       05  W50-BC7     PIC  9(07).
	       05  W50-BCQ     PIC  9(02)V999.
	       05  FILLER      PIC  X(02).
	   03  W50-BCD6 REDEFINES W50-BC.
	       05  W50-BC6     PIC  9(06).
	       05  FILLER      PIC  X(08).
