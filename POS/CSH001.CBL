      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   ******     ******    **    **     ****       ****      **    *
      *  **    **   **    **   **    **    **  **     **  **    ***    *
      *  **         **         **    **   **    **   **    **    **    *
      *  **          ******    ********   **    **   **    **    **    *
      *  **               **   **    **   **    **   **    **    **    *
      *  **    **   **    **   **    **    **  **     **  **     **    *
      *   ******     ******    **    **     ****       ****     ****   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   C A S H   S A L E S	P R O G R A M	       *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CSH001.
000030 AUTHOR.         J W LEMMON  (APAC).
000040 DATE-WRITTEN.   JUNE 1990.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2010
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 2 of the License, or (at
		   your option) any later version.

		   This program is distributed in the hope that it will
		   be useful, but WITHOUT ANY WARRANTY; without even the
		   implied warranty of MERCHANTABILITY or FITNESS FOR A
		   PARTICULAR PURPOSE.	See the GNU General Public License
		   for more details.

		   You should have received a copy of the GNU General
		   Public License along with this program; if not, write
		   to the Free Software Foundation, Inc., 59 Temple Place
		   - Suite 330, Boston, MA 02111-1307, USA.


000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.  
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

000220 COPY AUDIT.SL.

       COPY CARDEX.SL.

       COPY CHEQUE.SL.

       COPY CONTROL.SL.

000140 COPY DEPART.SL.

       COPY LEDTRF.SL.

000120 COPY PARAM.SL.

000240 COPY RECOVER.SL.

000200 COPY SALE.SL.

       COPY SHARED.SL.

000180 COPY STOCK.SL.

000140 COPY STKMEM.SL.

000180 COPY TXTRAN.SL.

000280     SELECT CSHDRW  ASSIGN W02-CSHDRW
000290                    ORGANIZATION SEQUENTIAL
000300                    ACCESS SEQUENTIAL.
000290     SELECT CSHSLE  ASSIGN DISK
000300                    ORGANIZATION LINE SEQUENTIAL
000310                    ACCESS SEQUENTIAL.
000280	   SELECT CUSDSP  ASSIGN W02-CUSDSP
000290                    ORGANIZATION SEQUENTIAL
000300                    ACCESS SEQUENTIAL.

      /
000320 DATA DIVISION.
000330 FILE SECTION.

       COPY APACFIDS.FDE.

000450 COPY AUDIT.FDE.

       COPY CARDEX.FDE.

       COPY CHEQUE.FD.

       COPY CONTROL.FDE.

000370 COPY DEPART.FDE.

       COPY LEDTRF.FDE.

000350 COPY PARAM.FDE.

000470 COPY RECOVER.GFD.

000410 COPY SALE.IFD.

       COPY SHARED.FDE.

000390 COPY STOCK.FDE.

000140 COPY STKMEM.FDE.

000180 COPY TXTRAN.FDE.

000530 FD  CSHDRW    LABEL RECORD OMITTED.
000550 01  CSH-REC.
000560     03  CSH-OPEN        PIC  X(06).
000530 FD  CSHSLE    LABEL RECORD STANDARD
000540               VALUE OF FILE-ID W02-PRINTER.
000550 01  CSH-L.
000560     03  CSH-DATA.
	       05  CSH-LINE PIC  X(40).
	       05  FILLER   PIC  X(40).
000530 FD  CUSDSP    LABEL RECORD OMITTED.
000550 01  CUS-REC.
000560*	   03  CUS-CON	       PIC  X(01).
      *	   03  CUS-INST        PIC  X(02).
	   03  CUS-LINE        PIC  X(40).
      /
000570 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000580 77  WS-DISC	   PIC	9(02)V99 COMP-3.
000590 77  WS-TDISC	   PIC	9(02)V99 COMP-3.
000600 77  WS-SUB	   PIC	9(04)	 COMP-5.
000600 77  WS-SUPER	   PIC	9(02)	 COMP-5.
000600 77  WS-S1	   PIC	9(04)	 COMP-5.
000610 77  WS-S2	   PIC	9(04)	 COMP-5.
000620 77  WS-S3	   PIC	9(04)	 COMP-5.
000630 77  WS-S4	   PIC	9(04)	 COMP-5.
000640 77  WS-S5	   PIC	9(04)	 COMP-5.
000650 77  WS-S6	   PIC	9(04)	 COMP-5.
000660 77  WS-S7	   PIC	9(04)	 COMP-5.
000670 77  WS-S8	   PIC	9(04)	 COMP-5.
000680 77  WS-S9	   PIC	9(04)	 COMP-5.
001790 77  WS-IXS	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	 COMP-5.
000690 77  WS-STKKEY	   PIC	9(04)	 COMP-5.
000700 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
000720 77  WS-NETKEY	   PIC	9(04)	 COMP-5.
000730 77  WS-AUDKEY	   PIC	9(04)	 COMP-5.
000740 77  WS-SALSTR	   PIC	9(04)	 COMP-5.
000750 77  WS-SALKEY	   PIC	9(04)	 COMP-5.
000760 77  WS-RECKEY	   PIC	9(04)	 COMP-5.
000770 77  WS-RECOVER	   PIC	9(04)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
000810 77  WS-KEY1	   PIC	9(04)	 COMP-5.
000820 77  WS-ITM	   PIC	X(14).
000830 77  WS-ITM1	   PIC	X(14).
000850 77  WS-START	   PIC	9(08).
000860 77  WS-END	   PIC	9(08).
000850 77  WS-TIME	   PIC	9(08).
000860 77  WS-TME2	   PIC	9(08).
002520 77  WS-PORT	   PIC	9(05) VALUE 259.
      * 1004.
       77  WS-DRAW1	   PIC	9(05) VALUE 260.
000600 77  WS-OPEN	   PIC	X(01) VALUE X"01".
000600 77  WS-CLOSE	   PIC	X(01) VALUE X"00".
000870 77  WS-PAR	   PIC	9(01) VALUE 0.
000880 77  WS-INO	   PIC	9(02) VALUE 0.
000890 77  WS-LINES	   PIC	9(02) VALUE 0.
000900 77  WS-FIRST	   PIC	9(03) VALUE 0.
       77  WS-SALPOS	   PIC	9(03) VALUE 0.
000910 77  WS-LAST	   PIC	9(03) VALUE 0.
000920 77  WS-PAGE	   PIC	9(04)	 COMP-5.
000930 77  WS-OPTION	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-PRC-IND	   PIC	X(01).
       77  WS-BNK-IND	   PIC	X(01).
       77  WS-SLIP-P	   PIC	X(01).
000930 77  WS-PRNINV	   PIC	X(01).
002600 77  WS-VAT-GST      PIC  X(01) VALUE "V".
002610 77  WS-SKIP         PIC  X(01) VALUE " ".
002610 77  WS-LOOK         PIC  X(01) VALUE "S".
002620 77  WS-ETYPE        PIC  X(01) VALUE "I".
       77  WS-PROMPT	   PIC	X(01).
002040 77  WS-WS	   PIC	X(01).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
000950 77  WS-NUM	   PIC	Z9.
000960 77  WS-ERROR	   PIC	9(01).
       77  PRG-NAME2	   PIC	X(12) VALUE SPACES.
001000 77  WS-COMP	   PIC	9(01).
001010 77  WS-INDI	   PIC	9(01) VALUE ZERO.
001020 77  WS-UPDATE	   PIC	9(01) VALUE ZERO.
001030 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
001040 77  WS-HEAD	   PIC	X(01).
001050 77  WS-TYPE	   PIC	X(01).
001060 77  WS-AGE	   PIC	X(01).
001060 77  WS-PRC	   PIC	X(01).
001110 77  WS-BATCH	   PIC	9(05).
001150 77  WS-PER	   PIC	Z(02)9.99.
001160 77  WS-CR	   PIC	X(01).
001170 77  WS-GSTRES	   PIC	S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
000070 77  WS-DRAW	   PIC	9(02).
       77  WS-CSHCODE	   PIC	X(06).
      /
002910 01  WS-DB-LINE.
003060	   03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍµ".
003080	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003140	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍµ".
003200     03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
003220     03  WS-TOP-LNE.
003230	       05  WS-TCHR  PIC	 X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍµ".
003240     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003250         05  FILLER  PIC X(01).
003260         05  WS-H-LINE
003270                     PIC X(78).
003280         05  FILLER  PIC X(01).
003290     03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003310	   03  WS-BOT-LNE.
003320	       05  WS-BCHR  PIC	 X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍµ".
003330	   03  WS-TP-LNE.
003340	       05  WS-TCH   PIC  X(80) VALUE "ÆÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍµ".
003350     03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003370	   03  WS-BT-LNE.
003380	       05  WS-BCH   PIC  X(80) VALUE "ÆÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍµ".
	       03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".

001260 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CSH".
	   03  WS-PROG	       PIC  X(03) VALUE "001".

       COPY LEDTRF.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

001280 01  W02-FID.

       COPY APACFIDS.ID.

000450 COPY AUDIT.ID.

       COPY CARDEX.ID.

       COPY CHEQUE.ID.

       COPY CONTROL.ID.

000370 COPY DEPART.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000470 COPY RECOVER.ID.

000410 COPY SALE.ID.

       COPY SHARED.ID.

000390 COPY STOCK.ID.

000140 COPY STKMEM.ID.

000180 COPY TXTRAN.ID.

       01  W02-FILE-IDS.
	   03  W02-PRINTER     PIC  X(12) VALUE "LPT1".
	   03  W02-WS REDEFINES W02-PRINTER.
	       05  W02-PROG    PIC  X(07).
	       05  W02-PUSER   PIC  X(05).
	   03  W02-CSHDRW      PIC X(04) VALUE "COM1".
	   03  W02-CUSDSP      PIC X(04) VALUE "COM1".

       COPY W05.VAT.

       COPY W10.SAL.

       COPY W12.WS.

001820 01  W15-CASH-SLIP.
001830     03  W15-L1.
001840* 	05  W15-ESC1	PIC  X(01).
001850* 	05  W15-DBLE	PIC  X(01).
001860         05  W15-EXPP1   PIC  X(01).
	       05  FILLER      PIC  X(04) VALUE SPACES.
001870         05  W15-COMP    PIC  X(32) VALUE SPACES.
001880         05  W15-ECAN1   PIC  X(01).
001890     03  W15-L1A.
001900* 	05  W15-ESC5	PIC  X(01).
001910* 	05  W15-DBLE3	PIC  X(01).
001920         05  W15-EXPP4   PIC  X(01).
	       05  FILLER      PIC  X(01) VALUE SPACES.
001930         05  W15-DOCTYP  PIC  X(18) VALUE SPACES.
001940         05  W15-ECAN4   PIC  X(01).
001950     03  W15-L2.
001960* 	05  W15-ESC2	PIC  X(01).
001970* 	05  W15-DCAN	PIC  X(01).
	       05  FILLER      PIC  X(04) VALUE SPACES.
001980         05  W15-ADD     PIC  X(32) VALUE SPACES.
001990     03  W15-L3.
002000	       05  FILLER      PIC  X(19) VALUE "Tel. No./Nr.     :".
002010         05  W15-TEL     PIC  X(18) VALUE SPACES.
           03  W15-L3A.
	       05  FILLER      PIC  X(19) VALUE "VAT/BTW Reg. Nr. :".
               05  W15-GNO     PIC  X(14) VALUE SPACES.
002020	   03  W15-L4.
002030	       05  FILLER      PIC  X(19) VALUE "Date/Datum       :".
002040	       05  W15-DATE    PIC  Z9/99/9999.
002020	   03  W15-L4A.
002030	       05  FILLER      PIC  X(19) VALUE "Time/Tyd         :".
002040	       05  W15-TIME    PIC  X(08).
002050     03  W15-L5.
002060	       05  FILLER      PIC  X(19) VALUE "Assist           :".
002070	       05  W15-SMAN    PIC  X(21).
002080     03  W15-L6.
002090         05  FILLER      PIC  X(20) VALUE "Rec. No./Kwit. Nr.:".
002100         05  W15-RECNO   PIC  X(08).
002110     03  W15-L7.
002120         05  FILLER      PIC  X(20) VALUE "Inv. No./Fak. Nr. :".
002130         05  W15-INVNO   PIC  X(08).
002200     03  W15-L9.
002210* 	05  W15-ESC3	PIC  X(01).
002220* 	05  W15-DBLE2	PIC  X(01).
002230         05  W15-EXPP2   PIC  X(01).
002240	       05  FILLER      PIC  X(18) VALUE "  THANK YOU/DANKIE".
002250         05  W15-ECAN2   PIC  X(01).
002260* 	05  W15-ESC4	PIC  X(01).
002270* 	05  W15-DCAN2	PIC  X(01).
002280	   03  W15-L11.
	       05  W15-DETAIL.
002340	       07  W15-DESC    PIC  X(28).
002350	       07  FILLER      PIC  X(01) VALUE SPACE.
002360	       07  W15-PRICE   PIC  Z(06)9.99-.
002280	   03  W15-L11A REDEFINES W15-L11.
002300	       05  W15-ITM     PIC  X(14).
002330	       05  FILLER      PIC  X(01).
002320	       05  W15-QUANT   PIC  ZZZ9.99.
	       05  FILLER      PIC  X(18).
002380	   03  W15-L12 REDEFINES W15-L11.
002400         05  W15-DISCOUNT.
002420             07  W15-DISC
002430			       PIC  X(23).
002440             07  W15-DSC PIC  Z9.99.
		   07  W15-BLK PIC  X(01).
002450	       05  FILLER      PIC  X(11).
	   03  W15-L12A REDEFINES W15-L11.
	       05  FILLER      PIC  X(15).
	       05  W15-AS1     PIC  X(02).
	       05  W15-CSHCODE PIC  X(06).
	       05  W15-AS2     PIC  X(02).
	       05  FILLER      PIC  X(15).
	   03  W15-CUSDSP REDEFINES W15-L11.
	       05  W15-CUSDESC.
		   07  W15-HD1 PIC  X(10).
		   07  W15-DUE PIC  Z(06)9.99.
	       05  W15-CUS2.
		   07  W15-HD2 PIC  X(10).
		   07  W15-CHG PIC  Z(06)9.99.
002460     03  W15-L13.
002470	       05  W15-REMK    PIC X(29).
002480         05  W15-VAL     PIC Z(06)9.99-.

       COPY W20.WS.

002580 01  W25-CALCS.
002590     03  W25-RESULT      PIC  9(02)V99.
002600     03  W25-RESULT1 REDEFINES W25-RESULT.
002610         05  W25-WHOLE   PIC  9(02).
002620         05  W25-DECIMAL PIC  9(02).
002630     03  W25-TOTAL       PIC  9(07)V99 COMP-3.
002640     03  W25-VALUE       PIC S9(07)V99 COMP-3.
002650     03  W25-INSTAL      PIC S9(07)V99 COMP-3.
002660     03  W25-CREDIT      PIC S9(07)V99 COMP-3.
002670     03  W25-CUR         PIC S9(07)V99 COMP-3.
002680     03  W25-INSTL.
002690         05  W25-MTHS OCCURS 6
002700                         PIC S9(07)V99 COMP-3.
002710     03  W25-INSTALS REDEFINES W25-INSTL.
002720         05  W25-MTH1    PIC S9(07)V99 COMP-3.
002730         05  W25-MTH2    PIC S9(07)V99 COMP-3.
002740         05  W25-MTH3    PIC S9(07)V99 COMP-3.
002750         05  W25-MTH4    PIC S9(07)V99 COMP-3.
002760         05  W25-MTH5    PIC S9(07)V99 COMP-3.
002770         05  W25-MTH6    PIC S9(07)V99 COMP-3.
001660	   03  W25-TIME        PIC 9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001690         05  W25-HH      PIC 9(02).
001690         05  W25-MM      PIC 9(02).
001690         05  W25-SS      PIC 9(02).
001680         05  FILLER      PIC 9(02).

       01  W40-COMPANY.
004900	   03  W40-NAME.
004910	       05  W40-NCH     PIC  X(01) OCCURS 32.

       COPY W40.WS.

       COPY FUNCTION.WS.

002870 COPY W50.WS.

       COPY W70DEBT.IWS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-SKEY       PIC 9(06) COMP-5 OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.

005950 01  W85-RECORD151.
005960     03  W85-CSHDTE      PIC  9(06).
001760     03  W85-FLOAT       PIC S9(09)V99 COMP-3.
001770     03  W85-CASH        PIC S9(09)V99 COMP-3.
001780     03  W85-CHEQUES     PIC S9(09)V99 COMP-3.
001790     03  W85-CARDS       PIC S9(09)V99 COMP-3.
001800     03  W85-VATVAL      PIC S9(09)V99 COMP-3.
001810     03  W85-VOUCHERS    PIC S9(09)V99 COMP-3.
001820     03  W85-RECEIPTS    PIC S9(09)V99 COMP-3.
001830     03  W85-CSH         PIC S9(09)V99 COMP-3.
001840     03  W85-CSHMTD      PIC S9(09)V99 COMP-3.
001850     03  W85-CSHYTD      PIC S9(09)V99 COMP-3.
005970     03  W85-ESC         PIC  X(01).
005980     03  W85-DBL         PIC  X(01).
005990     03  W85-CDBL        PIC  X(01).
006000     03  W85-SI          PIC  X(01).
006010     03  W85-CSI         PIC  X(01).
006020     03  W85-SO          PIC  X(01).
006030     03  W85-CSO         PIC  X(01).
006040     03  W85-6LPI        PIC  X(01).
006050     03  W85-8LPI        PIC  X(01).
006060     03  W85-NUL         PIC  X(01).
006070     03  W85-ADVANCE     PIC  9(02) COMP-5.
006080     03  W85-CPRN        PIC  9(01).
           03  W85-ITMS        PIC S9(07)    COMP-3.
           03  W85-CUST        PIC S9(07)    COMP-3.
           03  W85-TAXVAL      PIC S9(09)V99 COMP-3.
           03  W85-ADVTAX      PIC S9(09)V99 COMP-3.
002140	   03  W85-PETTY       PIC S9(09)V99 COMP-3.
	   03  W85-SPEED       PIC S9(09)V99 COMP-3.
	   03  W85-CUSDISP     PIC  X(01).
	   03  W85-CUSPORT     PIC  9(01).
	   03  W85-KICK        PIC  X(05).
	   03  W85-P-KICK REDEFINES W85-KICK.
	       05  W85-KC1     PIC  X(01)    COMP-X.
	       05  W85-KC2     PIC  X(01)    COMP-X.
	       05  W85-KC3     PIC  X(01)    COMP-X.
	       05  W85-KC4     PIC  X(01)    COMP-X.
	       05  W85-KC5     PIC  X(01)    COMP-X.
	   03  W85-KTCHPRN     PIC  X(01).
	   03  W85-KCHPORT     PIC  9(01).
002370	   03  FILLER	       PIC  X(01).
	   03  W85-USE-3       PIC  X(01).
	   03  W85-SCALE-TYPE  PIC  X(01).
	   03  W85-SCALE       PIC  X(01).
	   03  W85-SCALPORT    PIC  9(01).
	   03  W85-PORT        PIC  9(01).
           03  W85-DRAWER      PIC  X(01).

003160 01  W85-PASS.
003180     03  W85-SUPER OCCURS 9
003190                         PIC  X(06).
003200     03  W85-ENTRY       PIC  9(02)    COMP-3.
003210     03  W85-MARG        PIC S9(02)V99.

003230 01  W90-TRAN.
003240     03  W90-DATE        PIC  X(08).
003250     03  W90-DTE REDEFINES W90-DATE
003260                         PIC  99/99/99.
003270     03  W90-CODE        PIC  9(02).
003280     03  W90-ORD         PIC  X(12).
003290     03  W90-REF         PIC  X(08).
003300     03  W90-REG         PIC  X(09).
003310     03  W90-DB          PIC  9(07)V99 COMP-3.
003320     03  W90-CR          PIC  9(07)V99 COMP-3.
003330     03  W90-BDT         PIC S9(07)V99 COMP-3.
003340     03  W90-BCR         PIC S9(07)V99 COMP-3.
003350     03  W90-DESC        PIC  X(12).
006300     03  W90-120         PIC S9(07)V99 COMP-3.
003360     03  W90-90          PIC S9(07)V99 COMP-3.
003370     03  W90-60          PIC S9(07)V99 COMP-3.
003380     03  W90-30          PIC S9(07)V99 COMP-3.
003390     03  W90-INT         PIC S9(07)V99 COMP-3.
003400     03  W90-DUE         PIC S9(07)V99 COMP-3.
003410     03  W90-CASH        PIC S9(07)V99 COMP-3.
003420     03  W90-CHQ         PIC S9(07)V99 COMP-3.
003430     03  W90-CARD        PIC S9(07)V99 COMP-3.
003430	   03  W90-SPEED       PIC S9(07)V99 COMP-3.
003430	   03  W90-VOUCH       PIC S9(07)V99 COMP-3.
003440     03  W90-CHANGE      PIC S9(07)V99 COMP-3.
003450     03  W90-PMT         PIC S9(07)V99 COMP-3.
003460     03  W90-CUR         PIC S9(07)V99 COMP-3.
003470     03  W90-BAL         PIC S9(07)V99 COMP-3.
003480     03  W90-TAX         PIC  X(14).
003490     03  W90-DVAL        PIC  9(07)V99.
003500     03  W90-SALES       PIC S9(07)V99 COMP-3.
003510     03  W90-TDISC       PIC S9(07)V99 COMP-3.
003520	   03  W90-ATAX        PIC S9(07)V99 COMP-3.
003520     03  W90-TTAX        PIC S9(07)V99 COMP-3.
003530     03  W90-TCOST       PIC S9(07)V99 COMP-3.
003540     03  W90-TSELL       PIC S9(07)V99 COMP-3.
003550     03  W90-DISC        PIC S9(07)V99 COMP-3.
003560     03  W90-CRD         PIC S9(07)V99 COMP-3.
003570     03  W90-CDISC       PIC S9(07)V99 COMP-3.
003580     03  W90-LDISC       PIC S9(07)V99 COMP-3.
003590     03  W90-TAXVAL      PIC S9(07)V99 COMP-3.
006550     03  W90-EXCDIS      PIC S9(07)V99 COMP-3.
006550     03  W90-EXCTOT      PIC S9(07)V99 COMP-3.
003600     03  W90-NONTAX      PIC S9(07)V99 COMP-3.
003610     03  W90-VALUE       PIC S9(07)V99 COMP-3.
006570     03  W90-STOTAL      PIC S9(07)V99 COMP-3.
003620     03  W90-TOTAL       PIC S9(07)V9(06) COMP-3.
003630 01  W95-STM.
003640	   03  W95-COMP.
	       05  W95-DSP-L1  PIC  X(20).
	       05  W95-DSP-L2  PIC  X(20).
003650     03  W95-ADD1        PIC  X(30).
003660     03  W95-ADD2        PIC  X(30).
003670     03  W95-ADD3        PIC  X(30).
003680     03  W95-ADD4        PIC  X(30).
003690     03  W95-POST        PIC  9(04).
003700     03  W95-TEL         PIC  X(08).
	   03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.

003710 01  W100-EDIT.
003720     03  W100-BAL        PIC  Z(06)9.99-.
003730     03  W100-CUR        PIC  Z(06)9.99-.
003740     03  W100-30         PIC  Z(06)9.99-.
003750     03  W100-60         PIC  Z(06)9.99-.
003760     03  W100-90         PIC  Z(06)9.99-.
003770     03  W100-INT        PIC  Z(06)9.99-.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
	   03  W100-MEASURE.
	       05  W100-LENGTH PIC  ZZZ9.9999.
	       05  W100-BREADTH PIC ZZZ9.9999.
	       05  W100-NUMBER PIC  ZZZ9.

       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
003780 SCREEN SECTION.

	COPY BLANK.CRT.

003880 01  S05.
003890	   03  LINE 12 COLUMN 12 VALUE "Enter Password".
003900 01  S06.
003910	   03  LINE 12 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0 PIC  X(06)
				 USING W40-CSHPASS AUTO SECURE.
003930 01  S07.
	   03  LINE  1 COLUMN 64 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 VALUE "Cash Drawer: ".
	   03	       COLUMN 77 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC  Z9 FROM WS-DRAW.
003940     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15 VALUE "CASH SALE".
003950	   03  LINE  3 COLUMN  2 VALUE "Salesperson".
	   03	       COLUMN 14 BACKGROUND-COLOR 5
				 PIC  X(02) FROM WS-SPACE.
003960	   03  LINE  4 COLUMN  2 VALUE "Ref. No".
	   03	       COLUMN 10 BACKGROUND-COLOR 0
				 PIC  X(08) FROM WS-SPACE.
009370	   03  LINE  5 COLUMN  2 REVERSE-VIDEO VALUE "Item Code      I".
009380	   03	       COLUMN 18 REVERSE-VIDEO VALUE "TM Description  ".
009390     03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
009400     03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
009410     03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
009420     03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
009160	   03  LINE  6 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
004060	   03  LINE 20 COLUMN 3  VALUE "Sub Total ".
004110	   03  LINE 21 COLUMN 3  VALUE "Discount  ".
004150	   03  LINE 22 COLUMN 3  VALUE "T A X -VAT".
004190     03  LINE 23 COLUMN 3  VALUE "AMOUNT DUE".
004300 01  S09.
004310     03  LINE  1 COLUMN 50 VALUE "(".
004320     03          COLUMN 51 BLINK VALUE "REVERSAL".
004330     03          COLUMN 59 VALUE ")".

       01  S10.
           03  LINE 10 COLUMN 48 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 14
                                 PIC  X(24) USING W10-NAME AUTO.
           03  LINE 11 COLUMN 48 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 14
                                 PIC  X(14) USING W10-TEL AUTO.
           03  LINE 12 COLUMN 48 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 14
                                 PIC  99/99/99 USING W22-DTE1 AUTO.

007910 01  S12.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(14) USING W10-ITEM AUTO.

005530 01  S12-3.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S12-4.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S12-5.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S12-6.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S12-7.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S12-8.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S12-9.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S12-10.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S12-11.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S12-12.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S12-13.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S12-14.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(14) USING W10-I14 AUTO.

       COPY STKLUP.CRT.

004340 01  S26-QNT.
004350	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
004360                           PIC Z(04)9.99 USING W10-QUANT AUTO.

004340 01  S26-WEIGHT.
004350	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
004360				 PIC Z(03)9.999 USING W10-QUANT AUTO.

004370 01  S26-PRC.
004380	   03	       COLUMN 60 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
004390                           PIC Z(05)9.99 USING W10-SELL AUTO.

004400 01  S26-DSC.
004410	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
004420                           PIC Z9.99 USING WS-DISC AUTO.

004430 01  S26-TDSC.
004440	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
004450                           PIC Z9.99 USING WS-TDISC AUTO.

004460 01  S26-AVGE.
004470	   03	       COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
004480				 PIC Z(04)9.999 USING STK-AVGE AUTO.

004490 01  S26-CASH.
004500	   03  LINE 15 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
004510				 PIC Z(06)9.99- USING W90-CASH AUTO.

004520 01  S26-CHQ.
004530	   03  LINE 16 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
004540				 PIC Z(06)9.99- USING W90-CHQ AUTO.

004550 01  S26-CARD.
004560	   03  LINE 17 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
004570				 PIC Z(06)9.99- USING W90-CARD AUTO.

004550 01  S26-SPEED.
004560	   03  LINE 18 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
004570				 PIC Z(06)9.99- USING W90-SPEED AUTO.

004550 01  S26-VOUCH.
004560	   03  LINE 19 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
004570				 PIC Z(06)9.99- USING W90-VOUCH AUTO.

       COPY S99.CRT.

       01  S-MEASURE.
	   03  LINE  4 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ú".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
				 PIC X(19) USING WS-SRT-LINE.
	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "¿".
007450	   03  LINE  5 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(04) FROM WS-SPACE.
	   03	       COLUMN 20 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
				 VALUE "MEASUREMENTS".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 PIC  X(03) FROM WS-SPACE.
007460	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE  6 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Length  >".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".     
007460	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE  7 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Breadth >".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
007460	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
007450	   03  LINE  8 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Pieces  >    <".
007460	   03	       COLUMN 30 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
				 VALUE "     ³".
	   03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "À".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14
				 PIC X(19) USING WS-SRT-LINE.
	   03	       COLUMN 35 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 14 VALUE "Ù".

       01  SA-MEASURE.
	   03  LINE  6 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZZ9.9999 USING W10-LENGTH AUTO.
	   03  LINE  7 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZZ9.9999 USING W10-BREADTH AUTO.
	   03  LINE  8 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC ZZZ9 USING W10-NUMBER AUTO.

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.

       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004790 AA000		  SECTION.
004800 AA000-INIT.
003240	   IF LS0-SALEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004810	     PERFORM ZA000-INIT.
	     PERFORM DA000.
	   IF W85-CUSDISP = "Y"
	       CLOSE CUSDSP.
	     CLOSE CSHSLE
		   CHEQUE
		   RECOVER
		   SALE.
       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010480 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
010770	     PERFORM STOCK-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
010890	     MOVE "S" TO WS-LOOK.
010900 AA99.
010910       EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
011010 AA300              SECTION.
011020 AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     "=Item/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     "=Alt/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     "=Desc/"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     "=X/Ref Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     "=Order,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F7" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Prompts,"
011070		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011110 AA305.
011120       EXIT.

      /
      *    ****    READ FILES ROUTINES
      *
004930 AC000-READ              SECTION.

011150 COPY AUDIT.RD.

       COPY CHEQUE.RD.

011170 COPY CONTROL.RD.

011230 COPY DEPART.RD.

       COPY LEDTRF.RD.

011290 COPY PARAM.RD.

011310 COPY SALE.RD.

       COPY SHARED.RD.

011330 COPY STOCK.RD.

000140 COPY STKMEM.RD.

011350 COPY TXTRAN.RD.

      /
006420 AD000-WRITE             SECTION.

011150 COPY AUDIT.WR.

       COPY CARDEX.WR.

       COPY CHEQUE.WR.

011170 COPY CONTROL.WR.

011230 COPY DEPART.WR.

       COPY LEDTRF.WR.

011290 COPY PARAM.WR.

011310 COPY SALE.WR.

       COPY SHARED.WR.

011330 COPY STOCK.WR.

011350 COPY TXTRAN.WR.

       COPY CASHDRAW.OPN.

       COPY STOCK.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011670 AY000           SECTION.
011680 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011730       GO TO AY50.
011850 AY05.
011860       MOVE 05             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE ZERO           TO REC-TYPE.
011890	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
011900       MOVE STK-RECORD1    TO REC-STOCKF.
011910       GO TO AY50.
011920 AY16.
011930       MOVE 16             TO REC-FILE.
011940       MOVE WS-AUDKEY      TO REC-KEY.
011950       MOVE AUD-REC1       TO REC-AUDITF.
011960       GO TO AY50.
011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.
       AY36.
             MOVE 36             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE CHQ-RECORD1    TO REC-CHEQUE.
             GO TO AY50.
012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.
012090 AY38.
012100       MOVE 38             TO REC-FILE.
012110       MOVE WS-DEPKEY      TO REC-KEY.
012120	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
012130       MOVE DPT-RECORD     TO REC-DEPART.
012140       GO TO AY50.
012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.
012200 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
011070 AY43.
011080	     MOVE 43		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE CRD-REC1	 TO REC-CARDEX.
011110       GO TO AY50.
       AY49.
012210       MOVE 99             TO REC-FILE.
012220       MOVE ZERO           TO REC-KEY.
012230       MOVE SPACES         TO REC-DETAIL.
012240       PERFORM AY50.
012250       ADD 1               TO WS-TRANS.
012260     IF WS-RECOVER > 95
012270         CLOSE RECOVER
012280         OPEN OUTPUT RECOVER
012290         CLOSE RECOVER
012300         OPEN I-O RECOVER
012310         MOVE ZERO         TO WS-RECOVER.
012320       GO TO AY59.
012330 AY50.
012340       ADD 1               TO WS-RECOVER.
012350       MOVE WS-RECOVER     TO WS-RECKEY.
012360       MOVE WS-TRANS       TO REC-TRAN.
012370       WRITE REC-RECORD.
012380     IF WS-STATUS NOT = "00"
012390         DISPLAY "Write error Recovery file - Status " AT 2212
012400                  WITH FOREGROUND-COLOR 14
012410                  WS-STATUS WITH FOREGROUND-COLOR 15
012420         STOP RUN.
012430       CLOSE RECOVER.
012440       OPEN I-O RECOVER.
012450 AY59.
012460       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "DS"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012670	     MOVE 1		 TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
	   IF WS-STOCK = "Y"
012710	       MOVE 3		 TO WS-SHARED
012720	       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
	       PERFORM AY37 THRU AY59
	       MOVE SHR-RECORD	 TO NET-RECORD
	   ELSE
012710	       MOVE 3		 TO WS-NETKEY
012720	       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730	       PERFORM AY39 THRU AY59.
012740       GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012990       MOVE 1              TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020       PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

013080 AY999.
013090       EXIT.
      /
      *    ****    G E T   M E A S U R E M E N T S   (LENGTH/BREADTH)
      * 	   A N D   N U M B E R	 O F   P I E C E S
      *
       BB050	       SECTION.
       BB50.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 17		 TO SHADE-COL.
	     MOVE 19		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
019090	     DISPLAY S-MEASURE.
	     PERFORM SCREEN-SHADOW.
       BB55.
	     ACCEPT SA-MEASURE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BB55.
	     COMPUTE W10-QUANT ROUNDED = W10-LENGTH * W10-BREADTH
				       * W10-NUMBER.
       BB60.
	     PERFORM RESTORE-SCREEN-2.
       BB69.
             EXIT.

      /
007970 BG00-TRAN               SECTION.
008600 BG80.
008610       MOVE W70-AUDIT      TO WS-AUDKEY.
008620       MOVE LOW-VALUES     TO AUD-REC1.
008630       PERFORM AY16 THRU AY59.
008640       MOVE STK-CODE       TO AUD-CODE.
008650       MOVE 2              TO AUD-TYPE.
008660       MOVE WS-DRAW        TO AUD-SUB.
008680       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
008690       ADD 1               TO W70-AUDIT.
008700       MOVE 1              TO WS-AUDKEY.
008710       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
008720       PERFORM AY16 THRU AY59.
008730       MOVE 1              TO AUD-REP2.
008740       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
008750 BG85.
008760       EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

       COPY LEDTRF.UPD.

      /
009550 DA000		       SECTION 50.
009560 DA00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
018520       MOVE 5              TO WS-PARKEY.
018530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018540       MOVE PAR-PW         TO W85-PASS. 
009600	     ADD 150 WS-DRAW	 GIVING WS-PARKEY.
009610       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009620	     MOVE PAR-RECORD151  TO W85-RECORD151.

009630 DA05.
	   IF W85-CUSDISP = "Y"
      *	       MOVE X"1B"	 TO CUS-CON
      *	       MOVE "QA"	 TO CUS-INST
	       MOVE W95-COMP	 TO CUS-LINE
	       WRITE CUS-REC.
      *        MOVE "QB"	 TO CUS-INST
      *        MOVE W95-DSP-L2	 TO CUS-LINE
      *        WRITE CUS-REC.
009640	     DISPLAY CLR-SCREEN.
009650	     DISPLAY S07.
009660	     MOVE 7		 TO LIN.
009670       PERFORM DA48 UNTIL LIN > 18.
009700	     MOVE ZERO		 TO W90-BDT W90-NONTAX W90-LDISC
                                    W90-BAL W90-TAXVAL W90-SALES 
                                    W90-CUR W90-EXCTOT W10-SMAN.
009720	     MOVE 1		 TO WS-FIRST WS-SALPOS WS-LAST.
009730       MOVE SPACES         TO W90-TAX.
009740       MOVE SPACES         TO CSH-DATA.

009750 DA06.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F7" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Prompts,"
		     "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     "=Refund,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     "=Petty Cash,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ape to EXIT"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
009760	     ACCEPT W10-SMAN AT 0314
009770		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "E"	 TO LS-INSTR
		     GO TO DA150
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO DA06
		 WHEN F7-KEY
		     PERFORM DA300P
		     GO TO DA06
		 WHEN F8-KEY
		     MOVE "R"	 TO LS-INSTR
		     GO TO DA150
		 WHEN F9-KEY
		     MOVE "P"	 TO LS-INSTR
		     GO TO DA150
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA06
	       END-EVALUATE.
	   IF W10-SMAN = 99
	       GO TO DA150.
009780     IF W10-SMAN > 50
009790         GO TO DA06.
	     DISPLAY CLEAR-L25.
009800     IF W10-SMAN = ZERO
009810         GO TO DA07.
009820	     ADD 100 W10-SMAN	 GIVING WS-PARKEY.
009830       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009840     IF PAR-SALESMAN = SPACES
009850         GO TO DA06.
009860	     DISPLAY PAR-SALESMAN AT 0317
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

009870 DA07.
009970	     MOVE PAR-SALESMAN	 TO W15-SMAN.
009980	     DISPLAY CLEAR-L25.

019770 DA10.
	     MOVE "R"		 TO WS-OPTION.
	   IF WS-PROMPT = "Y"
019660	       MOVE "'R'etail,'C'ash or 'W'holesale price  [R]"
				 TO WS-ERR-MES
	       PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-PRC.
017600	   IF NOT (WS-PRC = "C" OR "R" OR "W")
017610	       GO TO DA10.
	   IF WS-PRC = "W"
018260	       DISPLAY "Enter password" AT 2551
018270			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018280	       ACCEPT W40-CSHPASS AT 2567
		      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
018290			   AUTO NO-ECHO
018300	       MOVE 6		 TO WS-LENGTH
	       CALL "CBL_TOUPPER" USING W40-CSHPASS
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
018360	       MOVE 1		 TO WS-LENGTH
	       IF NOT (W40-CSHPASS = W85-SUPER (1) OR
		       W85-SUPER (9) OR "SELOHW")
		   GO TO DA10.
009980	     DISPLAY CLEAR-L25.

010270 DA20.
010280	     MOVE 7		 TO LIN.
010290	     MOVE ZERO		 TO W90-DB.

010300 DA25.
010310       MOVE SPACES         TO W10-DESC W10-ITEM.
	   IF W10-STCK = "N"
	       IF W10-SLNGTH = 3
		   MOVE ZERO	 TO W10-I3
	       ELSE
	       IF W10-SLNGTH = 4
		   MOVE ZERO	 TO W10-I4
	       ELSE
	       IF W10-SLNGTH = 5
		   MOVE ZERO	 TO W10-I5
	       ELSE
	       IF W10-SLNGTH = 6
		   MOVE ZERO	 TO W10-I6
	       ELSE
	       IF W10-SLNGTH = 7
		   MOVE ZERO	 TO W10-I7
	       ELSE
	       IF W10-SLNGTH = 8
		   MOVE ZERO	 TO W10-I8
	       ELSE
	       IF W10-SLNGTH = 9
		   MOVE ZERO	 TO W10-I9
	       ELSE
	       IF W10-SLNGTH = 10
		   MOVE ZERO	 TO W10-I10
	       ELSE
	       IF W10-SLNGTH = 11
		   MOVE ZERO	 TO W10-I11
	       ELSE
	       IF W10-SLNGTH = 12
		   MOVE ZERO	 TO W10-I12
	       ELSE
	       IF W10-SLNGTH = 13
		   MOVE ZERO	 TO W10-I13
	       ELSE
		   MOVE ZERO	 TO W10-I14.
010320	     MOVE ZERO		 TO W10-SELL WS-SUPER.
             MOVE 1              TO W10-QUANT.
010330 DA30.
020730       PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
	   IF W10-STCK = "A"
021070	       ACCEPT S12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S12-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S12-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S12-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S12-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S12-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S12-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S12-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S12-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S12-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S12-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S12-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S12-14 AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
017730		     DISPLAY (LIN, 2) WS-MIV78
			      WITH FOREGROUND-COLOR 3
				   BACKGROUND-COLOR 1
		     MOVE SPACES TO W10-ITEM
		     SUBTRACT 1  FROM WS-LAST
		     GO TO DA49
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN F5-KEY
		     MOVE "X"	 TO WS-ETYPE
		 WHEN F6-KEY
		     PERFORM YA000
		     GO TO DA30
		 WHEN F7-KEY
		     PERFORM DA300P
		     GO TO DA30
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA30
	       END-EVALUATE
016560	       PERFORM AA50
021010	       DISPLAY (LIN, 2) W10-ITEM
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
021020         IF W10-ITEM = SPACES
021030		   GO TO DA30.
021040	   IF W10-ITEM = SPACES
021050         SUBTRACT 1        FROM WS-LAST
021060	       GO TO DA49.
       DA31.
             PERFORM CA000.
           IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50
018630	       DISPLAY (LIN, 2) W10-ITEM
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
018640         IF W10-ITEM = SPACES
018720             GO TO DA30
               ELSE
                   GO TO DA31.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA30.
010430	     MOVE WS-LAST	 TO WS-SALKEY.
010440       INITIALIZE SAL-RECORD1.
010450	     MOVE WS-LAST	 TO SAL-CODE.
      *
      *    ****    I T E M   N U M B E R   O N   C A S H   S L I P
      *
010460	     DISPLAY (LIN, 17) "   "
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
010470	     MOVE STK-CODE	 TO SAL-ITEM.
010480	     DISPLAY CLEAR-L25.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
010500	   IF NOT (STK-IND = 3)
010570	       MOVE STK-DESC	 TO W10-DESC
010580	       DISPLAY (LIN, 21) W10-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0
	       GO TO DA34.
       DA33.
010510	     ACCEPT (LIN, 21) W10-SDSC
010520		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 0 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA30
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA33
	       END-EVALUATE.
       DA33A.
020960	     MOVE ZERO		 TO STK-AVGE.
010530	     DISPLAY "Index Unit cost" AT 2512
010540		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
       DA33B.
010550	     ACCEPT S26-AVGE AT LINE 25
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA33
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA33B
	       END-EVALUATE.
021060	   IF STK-AVGE = ZERO
034710	       MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO DA33B.
021100	     DISPLAY CLEAR-L25.
       DA34.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
	   IF W85-CUSDISP = "Y"
      *	       MOVE X"1B"	 TO CUS-CON
      *	       MOVE "QA"	 TO CUS-INST
	       MOVE W10-DESC	 TO W15-CUSDESC
	       MOVE W15-CUSDESC	 TO CUS-LINE
	       WRITE CUS-REC.
      *	       MOVE "QB"	 TO CUS-INST
      *	       MOVE SPACES	 TO CUS-LINE
      *	       WRITE CUS-REC.
010590	     MOVE W10-ITEM	 TO SAL-ITEM.
010600	     MOVE W10-DESC	 TO SAL-DESC.
010610	     MOVE STK-AVGE	 TO SAL-COST W10-COST.
      *
      *    ****    G E T   Q U A N T I T Y
      *
	     MOVE ZERO		 TO W10-LENGTH W10-BREADTH W10-NUMBER.
	   IF WS-PROMPT = "F"
	       IF W85-USE-3 = "N"
021370		   DISPLAY S26-QNT AT LINE LIN
	       ELSE
021370		   DISPLAY S26-WEIGHT AT LINE LIN
	       END-IF
	       GO TO DA35-FAST.

       DA35.
	     DISPLAY "F6" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     " to key in measurements "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "- Cutting size (Wood, Glass etc)"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	   IF W85-USE-3 = "Y"
021370	       ACCEPT S26-WEIGHT AT LINE LIN
	   ELSE
021370	       ACCEPT S26-QNT AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
021390		     DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
021400		     GO TO DA25
		 WHEN F6-KEY
		     PERFORM BB050
021370		     DISPLAY S26-WEIGHT AT LINE LIN
		 END-EVALUATE.
	   IF (W10-QUANT = ZERO)
021390	       DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
021400	       GO TO DA25.

       DA35-FAST.
021180	     DISPLAY CLEAR-L25.
      *
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *
021190     IF STK-IND < 3
021320         IF W90-TAX NOT = SPACES
021330             IF WS-PRC = "R"
021340                 MOVE STK-SELL 
021350                           TO W10-SELL
021360             ELSE
021370             IF WS-PRC = "C"
021380                 MOVE STK-CASH 
021390                           TO W10-SELL
021400             ELSE
021410                 MOVE STK-WSALE 
021420                           TO W10-SELL
021430         ELSE
021440             IF WS-PRC = "R"
021450                 MOVE W10-VSELL 
021460                           TO W10-SELL
021470             ELSE
021480		   IF WS-PRC = "C"
021490                 MOVE W10-VCASH 
021500                           TO W10-SELL
021510             ELSE
021520                 MOVE W10-VWSALE 
021530                           TO W10-SELL.
	   IF WS-PROMPT = "F"
010730	       DISPLAY S26-PRC AT LINE LIN
	       GO TO DA35A-FAST.

010720 DA35A.
010730       ACCEPT S26-PRC AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO DA35
	       END-EVALUATE.
010740	     MOVE W10-SELL	 TO W10-SPAPRC.
010750       PERFORM DA110 THRU DA120.
010760     IF WS-ERROR = 1
010770	       MOVE ZERO	 TO WS-ERROR
010780         GO TO DA35A.

       DA35A-FAST.
	   IF W85-CUSDISP = "Y"
      *	       MOVE X"1B"	 TO CUS-CON
      *	       MOVE "QA"	 TO CUS-INST
	       MOVE "   PRICE-"	 TO W15-HD2
	       MOVE W10-SELL	 TO W15-CHG
	       MOVE W15-CUSDSP	 TO CUS-LINE
	       WRITE CUS-REC.
      *	       MOVE "QB"	 TO CUS-INST
      *	       MOVE W15-CUS2	 TO CUS-LINE
      *	       WRITE CUS-REC.

010790 DA35B.
010800	     MOVE W10-SELL	 TO SAL-SELL.
	   IF WS-PRC-IND = "Y"
	       IF (STK-IND > 1) OR (W10-QUANT < ZERO)
		   GO TO DA40
	       END-IF
	   ELSE
010810	   IF (STK-IND > 0) OR (W10-QUANT < ZERO)
010820	       GO TO DA40.
010830     IF STK-QUANT = ZERO
010840	       DISPLAY "No stock - Continue (Y/N) > <" AT 2512
010850			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
010860	       DISPLAY "'Y' will make stock holding negative" AT 2542
010870			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
010880	       ACCEPT (25, 39) WS-OPTION
		      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
010940	       DISPLAY CLEAR-L25
010950         IF WS-OPTION = "N"
010960		   MOVE W10-QUANT
				 TO W10-REORD
010970             GO TO DA25.
010980	   IF W10-QUANT NOT > STK-QUANT
011000         GO TO DA40.
	     DISPLAY "Negative Stock" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT.
018260	     DISPLAY "Enter password" AT 2551
018270		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
018280	     ACCEPT W40-CSHPASS AT 2567
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
018290			 AUTO NO-ECHO.
018300	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018360	     MOVE 1		 TO WS-LENGTH.
	     IF NOT (W40-CSHPASS = W85-SUPER (1) OR
		     W85-SUPER (9) OR "YTNAUQ")
		 GO TO DA25.

011200 DA40.
      *
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *
011210	     MOVE W10-QUANT	 TO SAL-QNT.
011220       COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
011230	     MOVE W25-VALUE	 TO W15-PRICE SAL-VAL W100-EXT.
011240	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
      *
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *
011260	     MOVE ZERO		 TO WS-DISC W90-CDISC.
	   IF WS-PROMPT = "F"
	       GO TO DA45.
011250	     DISPLAY (25, 18) "Discount %"
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.

011270 DA42.       
011280       ACCEPT S26-DSC AT LINE 25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
011140		     DISPLAY CLEAR-L25
		     GO TO DA35A
	       END-EVALUATE.
011290	   IF WS-DISC = ZERO
011300         GO TO DA45.
011310       COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
011320		    (W10-SELL * WS-DISC / 100.00000).
      *
      *    ****    C H E C K   M A R G I N
      *
011330       PERFORM DA110 THRU DA120.
011340     IF WS-ERROR = 1
011350         GO TO DA42.
	     DISPLAY CLEAR-L25.
011360	     MOVE WS-DISC	 TO SAL-DSC.
011370	     COMPUTE W90-CDISC = ((W25-VALUE * WS-DISC) / 100).
011380	     MOVE W90-CDISC	 TO W15-PRICE SAL-DISC.
011390	     DISPLAY (25, 40) W15-PRICE
		     WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
011400 DA45.
011510	   IF (STK-IND < 2) OR (STK-IND = 3)
011520         COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
011530	     ADD W90-CDISC	 TO W90-LDISC.
011540	     SUBTRACT W90-CDISC FROM W25-VALUE
				 GIVING W90-VALUE.
           IF STK-TAX = 0
022540         ADD W90-VALUE     TO W90-NONTAX
022550         MOVE W90-VALUE    TO SAL-NONTAX
022560     ELSE
022570         MOVE W90-VALUE    TO SAL-TAXVAL
022580         ADD W90-VALUE     TO W90-TAXVAL.
           IF W90-TAX NOT = SPACES
               COMPUTE SAL-VAT ROUNDED = 
                       SAL-TAXVAL * W05-RATE / 100.
      *
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *
011610	     ADD W25-VALUE	 TO W90-DB.
011620 DA45B.
011630	     MOVE W90-DB	 TO W90-TOTAL.
011640	     SUBTRACT W90-LDISC  FROM W90-TOTAL.
011650	     MOVE W90-DB	 TO W15-PRICE.
011660	     DISPLAY (20, 15) W15-PRICE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
011670	     MULTIPLY -1 BY W90-LDISC
				 GIVING W15-PRICE.
011680	     DISPLAY (21, 15) W15-PRICE
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
       DA45-AA.
011750	     MOVE W90-TOTAL	 TO W15-VAL.
011760	     DISPLAY (23, 15) W15-VAL
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	   IF W85-CUSDISP = "Y"
	       MOVE "AMT DUE:-"	 TO W15-HD1
	       MOVE W90-TOTAL	 TO W15-DUE
	       MOVE W15-CUSDESC	 TO CUS-LINE
	       WRITE CUS-REC.

       DA45-AB.
	   IF WS-PROMPT = "F"
	       GO TO DA46.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *
011770 DA45C.
036100	     MOVE "'A'ccept or 'C'ancel   [A]"
				 TO WS-ERR-MES.
022850	     MOVE "A"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
011900	   IF NOT (WS-OPTION = "A" OR "C")
011910	       GO TO DA45C.
011930     IF WS-OPTION = "C"
011940	       DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
               PERFORM DA47
               PERFORM DA45B THRU DA45-AA
011950         GO TO DA25.               

011820 DA46.
             MOVE WS-SUPER       TO SAL-SUPER.
011960	     PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
011970	     ADD 1		 TO WS-LAST.
      *
      *    ****    T E S T   F O R   C U T T I N G   S P E C S
      *
	   IF W10-LENGTH > ZERO AND
	      W10-BREADTH > ZERO AND
	      W10-NUMBER > ZERO
023410	      IF LIN < 18
023420		  ADD 1		 TO LIN
	      ELSE
023440		  MOVE 7	 TO WS-S7
023450		  PERFORM DA46-SHIFT
	       END-IF
023760	       MOVE WS-LAST	 TO WS-SALKEY
023770	       INITIALIZE SAL-RECORD1
023780	       MOVE WS-LAST	 TO SAL-CODE
	       MOVE SPACES	 TO W10-REMKS
	       MOVE W10-LENGTH	 TO W100-LENGTH
	       MOVE W10-BREADTH  TO W100-BREADTH
	       MOVE W10-NUMBER	 TO W100-NUMBER
	       STRING W100-LENGTH DELIMITED SIZE
		      " by " DELIMITED SIZE
		      W100-BREADTH DELIMITED SIZE
		      " X " DELIMITED SIZE
		      W100-NUMBER DELIMITED SIZE
		      INTO W10-REMKS
023790	       MOVE W10-REMKS	 TO SAL-MSER
	       MOVE WS-SUPER	 TO SAL-SUPER
023390	       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT
023710	       DISPLAY (LIN, 21) W10-REMKS
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0 UPDATE AUTO
023750*	       DISPLAY (LIN, 17) "   "
      *			WITH FOREGROUND-COLOR 3 HIGHLIGHT
      *			     BACKGROUND-COLOR 0
011970	     ADD 1		 TO WS-LAST.
011980     IF LIN < 18
011990	       ADD 1		 TO LIN
012000         GO TO DA25.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 7		 TO WS-S7.

       DA46-SHIFT.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO DA46-SHIFT.
	     PERFORM RESTORE-SCREEN-2.
023890	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
	     ADD 1		 TO WS-SALPOS.

       DA46-EXIT.
012040	     GO TO DA25.

       DA47.
025340	     SUBTRACT W25-VALUE  FROM W90-DB.
025350	     SUBTRACT W90-CDISC  FROM W90-LDISC.
025360       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
025370       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL.
025380     IF (STK-IND < 2) OR (STK-IND = 3)
025390         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).

012050 DA48.
023890	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
012070	     ADD 1		 TO LIN.

012080 DA49.
012090     IF WS-LAST = ZERO
012100         GO TO DA05.
      *
      *    ****    P R O C E S S   T H E   P A Y M E N T
      *
	   IF WS-PROMPT = "F"
	       GO TO DA55.

012110 DA50.
036900	     MOVE "'P'rocess, 'A'mend, 'C'ancel or 'M'ore items"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
012220	   IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
012230	       GO TO DA50.
023780     IF WS-OPTION = "M"
023790         ADD 1             TO WS-LAST
023800         GO TO DA25.
012240     IF WS-OPTION = "A"
012250	       PERFORM DA200A
012260         GO TO DA50.
           IF WS-OPTION = "C"
               GO TO DA05.

       DA55.
012270	     MOVE ZERO		 TO WS-TDISC W90-TDISC.
012280     IF W90-LDISC NOT = ZERO
012290	       MOVE W90-LDISC	 TO W90-TDISC
012300         GO TO DA60.
012310	     MOVE W90-DB	 TO W90-DVAL.

012520 DA60.
012530       SUBTRACT W90-TDISC FROM W90-DB GIVING W90-TSELL W90-TOTAL.
012540       MULTIPLY -1 BY W90-TDISC GIVING W15-VAL.
012550	     DISPLAY (21, 15) W15-VAL
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
012560	     MOVE ZERO		 TO W90-TTAX W90-ATAX.
      *
      *    ****    VERSION 6.62 - ALLOW FOR SPEED-POINT
      * 			- USE WINDOW FOR PAYMENT
      *
	     PERFORM SAVE-SCREEN-2.

       DA62.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1043
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "¿"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1143
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " P A Y M E N T   D E T A I L S ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1243
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " AMOUNT DUE                    ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1343
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Balance                       ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1443
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "                               ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1543
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Cash Received                 ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1643
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Cheque Received               ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1743
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Credit Card Slip              ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1843
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Speed Point Slip              ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 1943
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Voucher (Gift)                ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 2043
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "                               ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 2143
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " C H A N G E		     ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 2243
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 45		 TO SHADE-COL.
	     MOVE 31		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.

012660 DA65.
012670	     MOVE ZERO		 TO W90-DUE W90-CASH W90-VOUCH
012680				    W90-PMT W90-CARD W90-CHANGE
				    W90-CHQ W90-SPEED.
012690	     MOVE W90-TOTAL	 TO W15-VAL W90-DUE.
012700	     DISPLAY (23, 15) W15-VAL
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
012700	     DISPLAY W15-VAL AT 1263
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 7.
      *
      *    ****    C A L C U L A T E   U N P A I D   B A L A N C E
      *
012710 DA71.
012720       COMPUTE W90-CRD = W90-DUE - W90-PMT.
012730	     MOVE W90-CRD	 TO W100-BAL.
012740	     DISPLAY W100-BAL AT 1363
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 7.
012750 DA72.
      *
      *    ****    C A S H   V A L U E
      *
012760       ACCEPT S26-CASH.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO DA50
	       END-EVALUATE.
012770	     MOVE W90-CASH	 TO W90-PMT.
012800       PERFORM DA71.
012780     IF W90-PMT NOT < W90-DUE
012790         GO TO DA74.
012810 DA73.
      *
      *    ****    C H E Q U E   V A L U E
      *
012820       ACCEPT S26-CHQ.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO DA50
	       END-EVALUATE.
012830	     ADD W90-CHQ	 TO W90-PMT.
012860       PERFORM DA71.
012840     IF W90-PMT NOT < W90-DUE
012850         GO TO DA74.
      *
      *    ****    C R E D I T   C A R D   V A L U E
      *
012870       ACCEPT S26-CARD.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO DA50
	       END-EVALUATE.
012880	     ADD W90-CARD	 TO W90-PMT.
012860       PERFORM DA71.
012840	   IF W90-PMT NOT < W90-DUE
012850         GO TO DA74.
      *
      *    ****    S P E E D   P O I N T   V A L U E
      *
012870	     ACCEPT S26-SPEED.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO DA50
	       END-EVALUATE.
012880	     ADD W90-SPEED	 TO W90-PMT.
012860       PERFORM DA71.
012840	   IF W90-PMT NOT < W90-DUE
012850         GO TO DA74.
      *
      *    ****    V O U C H E R   V A L U E
      *
012870	     ACCEPT S26-VOUCH.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO DA50
	       END-EVALUATE.
012880	     ADD W90-VOUCH	 TO W90-PMT.
012860       PERFORM DA71.
      *
      *    ****    C H E C K   F O R   S H O R T   P A Y M E N T
      *
012890     IF W90-PMT < W90-DUE
012900         GO TO DA76.
012910 DA74.
      *
      *    ****    A R E   P A Y M E N T   D E T A I L S   C O R R E C T
      *
024810	     MOVE "'C'orrect or 'R'ekey payment"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
012950 DA75.
	     PERFORM OPT-MESSAGE.
013020	   IF NOT (WS-OPTION = "C" OR "R")
013030	       GO TO DA74.
013050     IF WS-OPTION = "R"
013060         GO TO DA65.           
	   IF NOT (WS-BNK-IND = "Y")
	       GO TO DA75B.
           IF W90-CHQ = ZERO
               GO TO DA75B.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		      AT 0933 WITH REVERSE-VIDEO.
	     DISPLAY "³Name                                  ³"
                      AT 1033 WITH REVERSE-VIDEO.
	     DISPLAY "³Telephone No.                         ³"
                      AT 1133 WITH REVERSE-VIDEO.
	     DISPLAY "³Cheque Date                           ³"
                      AT 1233 WITH REVERSE-VIDEO.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      AT 1333 WITH REVERSE-VIDEO.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 35		 TO SHADE-COL.
	     MOVE 38		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE SPACES	 TO W10-NAME W10-TEL.
             MOVE W12-TODAY      TO W22-DTE1.
       DA75A.
             ACCEPT S10.
	     DISPLAY CLEAR-L25.
           IF W10-NAME = SPACES OR
              W10-TEL = SPACES OR
              W22-DTE1 = ZERO
	       MOVE "Require name, telephone and date"
				 TO WS-ERR-STRING
025040	       PERFORM ERROR-MESSAGE
               GO TO DA75A.
             MOVE W22-DTE1       TO W22-DTE3
                                    W10-EDTE.
             PERFORM CHECK-DATE.
             MOVE W20-DTE        TO W22-DTE1.
             MOVE W22-DTE3       TO W20-DTE.
           IF WS-ERROR = 1
	       MOVE "Invalid date"
				 TO WS-ERR-STRING
025040	       PERFORM ERROR-MESSAGE
               GO TO DA75A.
	     PERFORM RESTORE-SCREEN-3.
       DA75B.
013070       COMPUTE W90-CHANGE = W90-PMT - W90-DUE.
013080	     MOVE W90-CHANGE	 TO W15-VAL.
013090	     DISPLAY W15-VAL AT 2163
		     WITH FOREGROUND-COLOR 0
			  BACKGROUND-COLOR 7.
	   IF W85-CUSDISP = "Y"
      *	       MOVE X"1B"	 TO CUS-CON
      *	       MOVE "QA"	 TO CUS-INST
	       MOVE " CHANGE:-"	 TO W15-HD2
	       MOVE W90-CHANGE	 TO W15-CHG
	       MOVE W15-CUSDSP	 TO CUS-LINE
	       WRITE CUS-REC.
      *	       MOVE "QB"	 TO CUS-INST
      *	       MOVE W15-CUS2	 TO CUS-LINE
      *	       WRITE CUS-REC.
013100	     GO TO DA79.
013110 DA76.
025020	     MOVE "Short - REKEY Payment"
				 TO WS-ERR-STRING.
025040	     PERFORM ERROR-MESSAGE.
013150	     MOVE 20		 TO LIN.
013160 DA78.
013220	     GO TO DA62.

013230 DA79.
	     MOVE WS-SLIP-P	 TO WS-PRNINV.
	   IF WS-PROMPT = "F"
	       GO TO DA79-FAST.

       DA79-LOOP.
	     MOVE WS-PRNINV	 TO WS-OPTION.
	     MOVE SPACES	 TO WS-ERR-MES
	     STRING "Print Invoice Y/N  ["
		    DELIMITED SIZE
		    WS-OPTION DELIMITED SIZE
		    "]" DELIMITED SIZE
			     INTO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-PRNINV.
	   IF NOT (WS-PRNINV = "Y" OR "N")
	       GO TO DA79-LOOP.

       DA79-FAST.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
	     DISPLAY CLEAR-L25.
013250       DISPLAY "Processing Sale" AT 2520 WITH
013260		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
013270       MOVE 1              TO WS-SALKEY.
013280       PERFORM AY60 THRU AY999.
013330       MOVE W70-INVOICE    TO W90-REF W15-RECNO.
013290     IF W70-INVOICE = 99999999
013300         MOVE 1            TO W70-INVOICE
013310     ELSE
013320         ADD 1             TO W70-INVOICE.
013340	     DISPLAY W90-REF AT 0410
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 0.
014800       PERFORM OPEN-CASH-DRAWER THRU OPEN-CASH-DRAWER-EXIT.
	   IF WS-PRNINV = "Y"
	       MOVE W85-SI	 TO CSH-DATA
	       WRITE CSH-L.
013350	     MOVE "CASH SALE/KONTANT"
				 TO W15-DOCTYP.
           IF WS-PRNINV = "N"
               GO TO DA80.
013360       MOVE W15-L1A        TO CSH-DATA.
013370       WRITE CSH-L.
013380       MOVE W15-L1         TO CSH-DATA.
013390       WRITE CSH-L.
013400       MOVE W15-L2         TO CSH-DATA.
013410       WRITE CSH-L.
013420       MOVE W15-L3         TO CSH-DATA.
013430       WRITE CSH-L.
           IF W05-VATNO NOT = SPACES
               MOVE W05-VATNO    TO W15-GNO
               MOVE W15-L3A      TO CSH-DATA
               WRITE CSH-L.
013440       MOVE W15-L4         TO CSH-DATA.
013450       WRITE CSH-L.
012150	     ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
	     MOVE W95-TIME	 TO W15-TIME.
013440	     MOVE W15-L4A	 TO CSH-DATA.
013450       WRITE CSH-L.
013460	   IF W10-SMAN NOT = ZERO
013470         MOVE W15-L5       TO CSH-DATA
013480         WRITE CSH-L.
013490       MOVE W15-L6         TO CSH-DATA.
013500       WRITE CSH-L.
013510	     MOVE ALL "-"	 TO CSH-LINE.
013520       WRITE CSH-L.
013530 DA80.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            P R I N T   T H E   C A S H   S L I P
      *
013540     IF WS-SALKEY > WS-LAST
013550         GO TO DA95.
013560       PERFORM READ-SALE THRU READ-SALE-EXIT.
026910     IF SAL-ITEM = SPACES
	       MOVE SPACES	 TO W15-L13
013600	       MOVE SAL-DESC	 TO W15-REMK
	       IF WS-PRNINV = "Y"
013680		   MOVE W15-L13	 TO CSH-DATA
013690		   WRITE CSH-L
	       END-IF
	       GO TO DA90.
013570       MOVE SAL-ITEM       TO STK-CODE.
             PERFORM AY05 THRU AY59.
	     MOVE SPACES	 TO W15-DETAIL.
	     MOVE "CASH"	 TO CRD-AC.
	     MOVE ZERO		 TO CRD-SEQ.
017540	     MOVE W10-SMAN	 TO CRD-SMAN.
017550	     MOVE W90-REF	 TO CRD-REF.
	     MOVE STK-SUPP	 TO CRD-SUPP.
	     MOVE W12-T-YMD	 TO CRD-DTE.
	     MOVE "CASH SALE"	 TO CRD-NAME.
	     MOVE SPACES	 TO CRD-WHS.
013590	     MOVE SAL-ITEM	 TO W15-ITM CRD-CODE.
	     MOVE STK-LDG	 TO CRD-DEPT.
013610	     MOVE SAL-QNT	 TO W10-QUANT W15-QUANT CRD-QNT.
	   IF WS-PRNINV = "Y"
013680	       MOVE W15-L11A	 TO CSH-DATA
013690         WRITE CSH-L.
	     MOVE SPACES	 TO W15-DETAIL.
013600       MOVE SAL-DESC       TO W15-DESC.
013620       MOVE SAL-VAL        TO W15-PRICE W25-VALUE.
013630	     MOVE SAL-COST	 TO W10-COST CRD-COST.
013640	     MOVE SAL-SELL	 TO W10-SELL.
013650       MOVE SAL-DISC       TO W90-CDISC.
013660       MOVE SAL-DSC        TO WS-DISC.
013670	     SUBTRACT W90-CDISC FROM W25-VALUE GIVING W90-VALUE.
             MOVE SAL-SUPER      TO WS-SUPER.
	   IF W90-TAX = SPACES
               IF STK-TAX NOT = ZERO
                   MOVE W05-VAT (STK-TAX)
                                 TO W05-RATE
                   COMPUTE W90-VALUE ROUNDED = W90-VALUE -
                       (((W90-VALUE / (W05-RATE + 100.00000)) * 
                          W05-RATE))
		   COMPUTE W90-ATAX = W90-ATAX + ((W25-VALUE -
				      W90-CDISC) - W90-VALUE)
                   COMPUTE W90-EXCDIS ROUNDED = W90-CDISC -
                       (((W90-CDISC / (W05-RATE + 100.00000)) *
                          W05-RATE)).
             ADD W90-VALUE       TO W90-EXCTOT.
	     COMPUTE CRD-SELL ROUNDED = W90-VALUE / SAL-QNT.
	   IF WS-CARDEX = "Y"
	       PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
015400	       MOVE 1		 TO WS-ACTION
	       PERFORM AY43 THRU AY59.
	   IF WS-PRNINV = "Y"
013680	       MOVE W15-L11	 TO CSH-DATA
013690         WRITE CSH-L.
      *
      *    ****    U P D A T E   S A L E S   A N A L Y S I S
      *
026950       MOVE STK-LEDG       TO WS-DEPKEY.
026960       PERFORM AY38 THRU AY59.
026970       ADD W90-VALUE       TO DPT-SALES-DAY
026980                              DPT-SALES-MTD
026990                              DPT-SALES-YTD.
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
               MOVE STK-LEDG     TO WX-LEDG
               MOVE "CASH SALES" TO WX-NAR
               COMPUTE WX-VALUE = W90-VALUE * -1
               MOVE DPT-SALE-GL  TO WX-AC
               MOVE 04           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   C O S T - O F - S A L E S
      *
027160       COMPUTE W90-BCR ROUNDED = (1.000 * W10-QUANT * W10-COST).
027000       ADD W90-BCR         TO DPT-COST-DAY
                                    DPT-COST-MTD
027010                              DPT-COST-YTD.
           IF WX-INT = "Y"
               IF W90-BCR NOT = ZERO
                   MOVE W90-BCR  TO WX-VALUE
                   MOVE DPT-COST-GL
                                 TO WX-AC
                   MOVE 08       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE
                   COMPUTE WX-VALUE = W90-BCR * -1
                   MOVE DPT-PUR-GL
                                 TO WX-AC
                   MOVE 19       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   D I S C O U N T   G I V E N
      *
027020       ADD W90-EXCDIS      TO DPT-DISC-DAY
027030                              DPT-DISC-MTD
027040                              DPT-DISC-YTD
                                    DPT-SALES-DAY
                                    DPT-SALES-MTD
                                    DPT-SALES-YTD.
027050       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
013750       MOVE W12-T-YMD      TO STK-ACT.
      *
      *    ****    C H E C K   I F   S T O C K   C O N T R O L L E D    
      *
	   IF WS-PRC-IND = "Y"
027590	       IF STK-IND > 1
027600		   GO TO DA85
	       END-IF
	   ELSE
	   IF STK-IND > ZERO
013770	       GO TO DA85.
013780       SUBTRACT W10-QUANT  FROM STK-QUANT.
013790	   IF STK-DISC > ZERO
013800         GO TO DA85.
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
013810     IF STK-QUANT NOT > STK-LEVEL
013820         MOVE 1            TO STK-DISC
013830         PERFORM BG80.
013840 DA85.
013860       ADD W90-BCR         TO W90-BDT STK-MTDC STK-YTDC.
013870       ADD W10-QUANT       TO STK-MTD STK-YTD.
013880       ADD W90-VALUE       TO STK-MTDV W90-SALES STK-YTDV.
013950       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      *
      *    ****    L O G   T R A N S A C T I O N   T O	 T H E
      * 	   A U D I T   F I L E
      *
013960	     MOVE W70-AUDIT	 TO WS-AUDKEY.
013970       MOVE LOW-VALUES     TO AUD-REC1.
013980       PERFORM AY16 THRU AY59.
013990       MOVE 6              TO AUD-TYPE.
014000       MOVE WS-DRAW        TO AUD-SUB.
014010       MOVE W90-REF        TO AUD-CSH.
014020       MOVE W10-SMAN       TO AUD-SMAN.
014030       MOVE W90-VALUE      TO AUD-SEL.
014040       MOVE W90-BCR        TO AUD-CST.
014050       MOVE STK-CODE       TO AUD-STOCK.
             MOVE WS-SUPER       TO AUD-SUPER.
014060       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
014070       ADD 1               TO W70-AUDIT.
014080       MOVE 1              TO WS-AUDKEY.
014090       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
014100       PERFORM AY16 THRU AY59.
014110       MOVE 1              TO AUD-REP6.
014120       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
014130     IF W90-CDISC = ZERO
014140         GO TO DA90.
	     MOVE SPACES	 TO W15-DETAIL.
014150       MOVE "DISCOUNT/AFSLAG %" 
                                 TO W15-DISC.
014160       MOVE WS-DISC        TO W15-DSC.
014170       MULTIPLY -1 BY W90-CDISC 
				 GIVING W15-PRICE.
           IF WS-PRNINV = "Y"
014180         MOVE W15-L11      TO CSH-DATA
014190         WRITE CSH-L.

014200 DA90.
014210       MOVE SPACES         TO CSH-DATA.
014220       ADD 1               TO WS-SALKEY.
014230       GO TO DA80.

014240 DA95.
           IF WS-PRNINV = "Y"
013510	       MOVE ALL "-"	 TO CSH-LINE
014250         WRITE CSH-L.
014260       MOVE "Sub TOTAL/TOTAAL :" 
                                 TO W15-REMK.
014270       MOVE W90-DB         TO W15-VAL W90-TOTAL.
           IF WS-PRNINV = "Y"
014280         MOVE W15-L13      TO CSH-DATA
014290         WRITE CSH-L.
014300     IF W90-TDISC NOT = ZERO
014310         IF W90-LDISC = ZERO
027690             MOVE 298      TO WS-DEPKEY
027700             PERFORM AY38 THRU AY59
027710             ADD W90-TDISC TO DPT-DISC-DAY
027720                              DPT-DISC-MTD
027730                              DPT-DISC-YTD
                   IF WX-INT = "Y"
                       MOVE W90-EXCDIS
                                 TO WX-VALUE
                       MOVE DPT-DISC-GL
                                 TO WX-AC
                       MOVE 15   TO WX-TYPE
                       PERFORM GL-TRF-UPDATE
                   END-IF
027740             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
014370     IF W90-TDISC NOT = ZERO
014380         MOVE "DISCOUNT/AFSLAG  :" 
                                 TO W15-REMK
014390         COMPUTE W15-VAL = W90-TDISC * -1
               IF WS-PRNINV = "Y"
014400             MOVE W15-L13  TO CSH-DATA
014410             WRITE CSH-L.
           IF W90-TAX NOT = SPACES
014440         MOVE "TAX / BELASTING  :" 
                                 TO W15-REMK.
           IF W90-TAX = SPACES
               MOVE W05-VAT-R1   TO W05-RATE
               COMPUTE W90-TTAX ROUNDED = (W90-TAXVAL / 
                  (100.00000 + W05-RATE)) * W05-RATE
               COMPUTE W90-TAXVAL = W90-TAXVAL - W90-TTAX.
014480       MOVE 9              TO WS-PARKEY.
014500       PERFORM AY01 THRU AY59.
027830       MOVE 300            TO WS-DEPKEY.
027840       PERFORM AY38 THRU AY59.
027850	     ADD W90-ATAX	 TO DPT-SALES-DAY
027860                              DPT-SALES-MTD
027870                              DPT-SALES-YTD.
	   IF (WX-INT = "Y") AND (W90-ATAX NOT = ZERO)
		COMPUTE WX-VALUE = W90-ATAX * -1
                MOVE DPT-SALE-GL TO WX-AC
                MOVE 10          TO WX-TYPE
                PERFORM GL-TRF-UPDATE.
027880       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
             ADD W90-NONTAX      TO PAR-DNGS-DAY
                                    PAR-DNGS-MTD
                                    PAR-DNGS-YTD.
             ADD W90-TAXVAL      TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
                                    PAR-DVAT-YTD.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
027890       MOVE W05-RATE       TO WS-PER.
           IF W90-TAX = SPACES
               GO TO DA100.
014450       MOVE W90-TTAX       TO W15-VAL.
           IF WS-PRNINV = "Y"
014460         MOVE W15-L13      TO CSH-DATA
014470         WRITE CSH-L.
014540 DA100.
014550       MOVE W90-BDT        TO W90-TCOST.
             MOVE 2              TO TAX-TYPE.
             MOVE 2              TO TAX-ACTYPE.
             MOVE 01             TO TAX-CODE.
             MOVE W12-T-YMD      TO TAX-DATE.
             MOVE W90-REF        TO TAX-REF.
             MOVE W90-TTAX       TO TAX-VAT.
             MOVE W90-NONTAX     TO TAX-TAXFREE.
             SUBTRACT W90-DISC FROM W90-TOTAL
                                 GIVING TAX-VALUE.
             MOVE ZERO           TO TAX-SEQ
                                    TAX-AC.
             PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY19 THRU AY59.
014560       MOVE "DUE / VERSKULDIG :" 
                                 TO W15-REMK.
014570	     SUBTRACT W90-TDISC FROM W90-TOTAL
				 GIVING W15-VAL.
           IF WS-PRNINV = "N"
               GO TO DA101.
014580       MOVE W15-L13        TO CSH-DATA
014590       WRITE CSH-L.
014600     IF W90-CASH > ZERO
014610	       MOVE "CASH REC/KONTANT :"
                                 TO W15-REMK
014620         MOVE W90-CASH     TO W15-VAL
014630         MOVE W15-L13      TO CSH-DATA
014640         WRITE CSH-L.
014650     IF W90-CHQ > ZERO
014660	       MOVE "CHEQUE REC/TJEK  :"
                                 TO W15-REMK
014670         MOVE W90-CHQ      TO W15-VAL
014680         MOVE W15-L13      TO CSH-DATA
014690         WRITE CSH-L.
014700	   IF W90-CARD > ZERO
014710         MOVE "CR-CARD/KT-KAART :"
                                 TO W15-REMK
014720         MOVE W90-CARD     TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L.
014700	   IF W90-SPEED > ZERO
014710         MOVE "CR-CARD/KT-KAART :"
                                 TO W15-REMK
014720	       MOVE W90-SPEED	 TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L.
014700	   IF W90-VOUCH > ZERO
014710	       MOVE "VOUCHER/KT-KAART :"
                                 TO W15-REMK
014720	       MOVE W90-VOUCH	 TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L.
014750	   IF W90-CHANGE > ZERO
014760         MOVE "CHANGE/KLEINGELD :" 
                                 TO W15-REMK
014770         MOVE W90-CHANGE   TO W15-VAL
014780         MOVE W15-L13      TO CSH-DATA
014790         WRITE CSH-L.

       DA101.
           IF WS-PRNINV = "N"
               GO TO DA106.
014810       MOVE SPACES         TO CSH-DATA.
014820       WRITE CSH-L.
           IF W90-TAX = SPACES
               IF W05-VATNO NOT = SPACES
                   MOVE "VAT Inclusive / BTW Ingesluit"
                                 TO CSH-DATA
                   WRITE CSH-L
                   MOVE SPACES   TO CSH-DATA.
           IF W05-VATNO NOT = SPACES
               MOVE ALL "="      TO CSH-LINE
               WRITE CSH-L
               MOVE SPACES       TO CSH-DATA
	       MOVE "   TAX INVOICE"
                                 TO W15-DOCTYP
013360         MOVE W15-L1A      TO CSH-DATA
               WRITE CSH-L
               MOVE SPACES       TO CSH-DATA
               MOVE "TAXABLE/BELASBARE :"
                                 TO W15-REMK
               MOVE W90-TAXVAL   TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L
               MOVE "V A T / B T W     :"
                                 TO W15-REMK
               MOVE W90-TTAX     TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L
               MOVE ALL "="      TO CSH-LINE
               WRITE CSH-L
               MOVE SPACES       TO CSH-DATA
014790         WRITE CSH-L.
014830       MOVE W15-L9         TO CSH-DATA.
014840       WRITE CSH-L.
014850	     MOVE "    APAC accounting - (011) 462-2272"
				 TO CSH-DATA.
014880       WRITE CSH-L.
014850	     MOVE SPACES	 TO CSH-DATA W15-DETAIL.
	     MOVE "**"		 TO W15-AS1 W15-AS2.
	     MOVE WS-CSHCODE	 TO W15-CSHCODE.
	     MOVE W15-L12A	 TO CSH-DATA.
014860	     MOVE 1		 TO WS-S9.
014870 DA105.     
014880       WRITE CSH-L.
014850	     MOVE SPACES	 TO CSH-DATA.
014890     IF WS-S9 < W85-ADVANCE
014900         ADD 1             TO WS-S9
014910         GO TO DA105.
	     MOVE W85-CSI	 TO CSH-DATA.
014880       WRITE CSH-L.
014850       MOVE SPACES         TO CSH-DATA.
	     CLOSE CSHSLE.
	     OPEN OUTPUT CSHSLE.
	     CALL X"91" USING X91-RES X91-FUN CSHSLE.
       DA106.
014920       MOVE W70-AUDIT      TO WS-AUDKEY.
014930       MOVE LOW-VALUES     TO AUD-REC1.
014940       PERFORM AY16 THRU AY59.
014950       MOVE 5              TO AUD-TYPE.
014960	     MOVE WS-DRAW	 TO AUD-SUB.
014970	     MOVE W90-REF	 TO AUD-CSH.
             COMPUTE AUD-SELL = W90-TSELL - W90-TTAX.
014990	     MOVE W90-TCOST	 TO AUD-COST.
015000	     MOVE W10-SMAN	 TO AUD-SMAN.
015010       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015020	     ADD 1		 TO W70-AUDIT.
015030	     MOVE 1		 TO WS-AUDKEY.
015040       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
015050       PERFORM AY16 THRU AY59.
015060	     MOVE 1		 TO AUD-REP5.
015070       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015080	     MOVE W70-AUDIT	 TO WS-AUDKEY.
015090	     MOVE LOW-VALUES	 TO AUD-REC1.
015100       PERFORM AY16 THRU AY59.
015110	     MOVE 4		 TO AUD-TYPE.
015120	     MOVE WS-DRAW	 TO AUD-SUB.
015130	     MOVE W10-SMAN	 TO AUD-AC.
015140	     MOVE W90-REF	 TO AUD-REF.
015150       COMPUTE AUD-VAL = (W90-TOTAL - W90-TDISC).
015160       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015170	     ADD 1		 TO W70-AUDIT.
015180	     MOVE 1		 TO WS-AUDKEY.
015190       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
015200       PERFORM AY16 THRU AY59.
015210	     MOVE 1		 TO AUD-REP4.
015220       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015230     IF W10-SMAN NOT = ZERO
015240	       ADD 100 W10-SMAN  GIVING WS-PARKEY
015260         PERFORM AY01 THRU AY59
015270         ADD W90-TCOST     TO PAR-CT-MTD PAR-CT-YTD
015280         ADD W90-EXCTOT    TO PAR-SL-MTD PAR-SL-YTD
015290         PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015330       SUBTRACT W90-BAL FROM STK-TOTAL.
015340	     ADD W90-SALES	 TO STK-SMTD STK-SYTD.
015360	     ADD 150 WS-DRAW	 GIVING WS-PARKEY.
015380       PERFORM AY01 THRU AY59.
015390       ADD W90-CASH        TO PAR-CASH.
015400       ADD W90-CHQ         TO PAR-CHEQUES.
015410       ADD W90-CARD        TO PAR-CARDS.
015410	     ADD W90-SPEED	 TO PAR-SPEED.
015410	     ADD W90-VOUCH	 TO PAR-VOUCHERS.
015420       SUBTRACT W90-CHANGE FROM PAR-CASH.
             ADD W90-TTAX        TO PAR-VATVAL.
029070       ADD W90-TAXVAL      TO PAR-TAXVAL.
015440       COMPUTE PAR-CSH = PAR-CSH + W90-TAXVAL + W90-NONTAX.
015450       COMPUTE PAR-CSHMTD = PAR-CSHMTD + W90-TAXVAL + W90-NONTAX.
015460       COMPUTE PAR-CSHYTD = PAR-CSHYTD + W90-TAXVAL + W90-NONTAX.
015470       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015370       MOVE PAR-RECORD151  TO W85-RECORD151.
015480	     MOVE 7		 TO WS-PARKEY.
015500       PERFORM AY01 THRU AY59.
             COMPUTE PAR-CASH-DAY  = PAR-CASH-DAY + (W90-CASH 
                                     - W90-CHANGE) + W90-CHQ 
				     + W90-CARD + W90-VOUCH
				     + W90-SPEED.
             COMPUTE PAR-CASH-MTH  = PAR-CASH-MTH + (W90-CASH 
                                     - W90-CHANGE) + W90-CHQ
				     + W90-CARD + W90-VOUCH
				     + W90-SPEED.
             COMPUTE PAR-CASH-YEAR = PAR-CASH-YEAR + (W90-CASH 
                                     - W90-CHANGE) + W90-CHQ 
				     + W90-CARD + W90-VOUCH
				     + W90-SPEED.
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
               MOVE 01           TO WX-LEDG
               MOVE "CASH SLIPS" TO WX-NAR
	       COMPUTE WX-VALUE = (W90-TOTAL - W90-TDISC)
               MOVE PAR-CASH-GL  TO WX-AC
               MOVE 26           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015480	     MOVE 8		 TO WS-PARKEY.
015500       PERFORM AY01 THRU AY59.
015510	     ADD W90-BDT	 TO PAR-CSHT PAR-CMTD PAR-CYTD.
015520       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	   IF WS-BNK-IND = "Y" AND
	      W90-CHQ NOT = ZERO
               INITIALIZE CHQ-RECORD2
               MOVE W90-REF      TO CHQ-TR-NUMBER
               MOVE W22-DTE1     TO CHQ-YMD
               MOVE W20-DTE      TO CHQ-DTE
               MOVE W10-NAME     TO CHQ-NAME
               MOVE W10-TEL      TO CHQ-TEL
               MOVE W90-CHQ      TO CHQ-VALUE
               PERFORM WRITE-CHEQUE THRU WRITE-CHEQUE-EXIT
               MOVE 1            TO WS-ACTION
               PERFORM AY36 THRU AY59.
015530       PERFORM AY70 THRU AY999.
015540       CLOSE PARAM AUDIT STOCK SALE NETWORK DEPART.
028820       OPEN OUTPUT SALE.
028830       CLOSE SALE.
015550       OPEN I-O PARAM AUDIT STOCK SALE NETWORK DEPART.
029700	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
015660	   IF WS-OPTION = "E"
015670	       GO TO DA150.
015680       GO TO DA05.
015690 DA110.
015700	     MOVE ZERO		 TO WS-ERROR.
	     MOVE W10-SPAPRC	 TO W10-ORDER.
	   IF W90-TAX = SPACES
018020	       IF STK-TAX NOT = ZERO
018030		   COMPUTE W10-ORDER ROUNDED = (W10-SPAPRC /
018040		      (100.0000 + W05-RATE)) * 100.
	   IF (WS-PRC = "R") AND (STK-SELL NOT > W10-ORDER)
015720         GO TO DA120
           ELSE
	   IF (WS-PRC = "C") AND (STK-CASH NOT > W10-ORDER)
015720         GO TO DA120
           ELSE
015710	   IF STK-WSALE NOT > W10-ORDER
015720         GO TO DA120.
015730	     COMPUTE W10-MARG = ((W10-ORDER - W10-COST) / W10-ORDER)
015740                          * 100.00.
015750     IF W10-MARG NOT < W85-MARG
015760         GO TO DA120.
015790 DA115.
018110	     MOVE "Margin less than allowed - Override Y/N?"
				 TO WS-ERR-MES.
	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
015860	   IF NOT (WS-OPTION = "N" OR "Y")
015870         GO TO DA115.
015880     IF WS-OPTION = "N"
015890	       MOVE 1		 TO WS-ERROR
015900         GO TO DA120.
       DA116.
018260	     DISPLAY "Enter password" AT 2524
018270		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
018280	     ACCEPT W40-CSHPASS AT 2539
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
018290			 AUTO NO-ECHO.
018300	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018360	     MOVE 1		 TO WS-LENGTH.

       DA117.
	   IF W40-CSHPASS = W85-SUPER (1)
	       MOVE 1		 TO WS-SUPER
	       GO TO DA118.
	   IF W40-CSHPASS = "NIGRAM"
               MOVE 10           TO WS-SUPER
           ELSE
	   IF W40-CSHPASS = W85-SUPER (9)
	       MOVE 9		 TO WS-SUPER
           ELSE
018470	       MOVE 1		 TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
	   IF W10-SPAPRC < W10-COST
018470		MOVE 1		 TO WS-ERROR
		MOVE ZERO	 TO WS-SUPER.

       DA118.
	     DISPLAY CLEAR-L25.

016060 DA120.
016070       EXIT.

       DA150.
	     EXIT.
      *
      *    ****    A M E N D   S A L E S   D E T A I L S
      *
       DA200A	       SECTION 50.
016080 DA200.
016090	     MOVE 7		 TO LIN.
016100       PERFORM DA48 UNTIL LIN > 18.
016110	     MOVE 7		 TO LIN.
016120	     MOVE 1		 TO WS-LINES WS-FIRST.

016130 DA202.
016140	     MOVE WS-FIRST	 TO WS-SALKEY WS-SALPOS.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
016150 DA205.
016160     IF WS-SALKEY > WS-LAST
016170	       GO TO DA208.
016180       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
016200	     DISPLAY (LIN, 2) SAL-ITEM
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030090	     DISPLAY ( , 17) "   "
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
016210	     DISPLAY ( , 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
016220	     MOVE SAL-QNT	 TO W10-QUANT.
	   IF W85-USE-3 = "N"
016230	       DISPLAY S26-QNT AT LINE LIN
	   ELSE
	       DISPLAY S26-WEIGHT AT LINE LIN.
016240	     MOVE SAL-SELL	 TO W10-SELL.
016250       DISPLAY S26-PRC AT LINE LIN.
016260	     MOVE SAL-VAL	 TO W15-PRICE W100-EXT.
016270	     DISPLAY ( , 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
016280	   IF LIN < 18
016290	       ADD 1		 TO LIN WS-SALKEY WS-LINES
016300         GO TO DA205.

       DA208.
	     MOVE 6		 TO LIN.
	     DISPLAY CLEAR-L25.

016310 DA210.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
001020	     DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 14
					BACKGROUND-COLOR 3
001030		     " highlight," WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1
			      BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 14
				 BACKGROUND-COLOR 3
		     "," WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3
001040		     "A" WITH FOREGROUND-COLOR 14
				  BACKGROUND-COLOR 3
001050		     "mend," WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
001040		     "D" WITH FOREGROUND-COLOR 14
				  BACKGROUND-COLOR 3
001050		     "elete or " WITH FOREGROUND-COLOR 1
				       BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 14
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1
				BACKGROUND-COLOR 3.
      *
      *    ****    S A V E   C U R R E N T   P A G E
      *
	     PERFORM SAVE-SCREEN-2.
      *
      *    ****    S C R E E N	 A T T R I B U T E S
      *
       DA214.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.
      *
      *    ****    G E T   U S E R   O P T I O N
      *
016580 DA215.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO DA216
		 WHEN DOWN-KEY
001340		     GO TO DA217
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DA215
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO DA265
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
		 WHEN PAGE-DOWN
001340		     GO TO DA222
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
		 WHEN PAGE-UP
001340		     GO TO DA220
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DA215
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION = "A" OR "D") AND
		  (LIN > 6)
		   DISPLAY CLEAR-L25
		   GO TO DA224.
	     CALL X"E5".
001280	     GO TO DA215.
      *
      *    ****    U P	 K E Y	 ( M O V E   H I G H L I G H T	 U P )
      *
       DA216.
	   IF (WS-SALPOS = 1) OR (LIN < 7)
	       GO TO DA218.
	     SUBTRACT 1 	 FROM WS-SALPOS.
	   IF LIN > 7
	       SUBTRACT 1	 FROM LIN
	       GO TO DA218.
	     MOVE 18		 TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      * 	    O N E   L I N E
      *
       DA216-LOOP.
	     SUBTRACT 1 FROM WS-S7
				 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S7 > 8
	       SUBTRACT 1	 FROM WS-S7
	       GO TO DA216-LOOP.
	     MOVE WS-SALPOS	 TO WS-SALKEY WS-FIRST.
      *
      *    ****    C L E A R   F I R S T   O R	 L A S T
      * 	   D E T A I L	 L I N E.
      *
       DA216-DISPLAY.
	     PERFORM RESTORE-SCREEN-2.
023890	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
      *
      *    ****    G E T   N E X T   O R   P R E V I O U S
      * 	   D E T A I L	 L I N E.
      *
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
030100	   IF SAL-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
030120	       GO TO DA216-END-DISP.
030130	     DISPLAY (LIN, 2) SAL-ITEM
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030090	     DISPLAY ( , 17) "   "
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030140	     DISPLAY ( , 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030150       MOVE SAL-QNT        TO W10-QUANT.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY ( , 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.

       DA216-END-DISP.
	     PERFORM SAVE-SCREEN-2.
	     GO TO DA218.
      *
      *    ****    D O W N   K E Y   ( M O V E	 H I G H L I G H T
      * 			       D O W N )
      *
       DA217.
	   IF (WS-SALPOS = WS-LAST) AND (LIN > 7)
	       GO TO DA218.
      *
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *
	   IF LIN > 6
	       ADD 1		 TO WS-SALPOS.
	   IF LIN < 18
	       ADD 1		 TO LIN
	       GO TO DA218.
	     MOVE 7		 TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      * 	    L I N E
      *
       DA217-LOOP.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO DA217-LOOP.
	     ADD 1		 TO WS-FIRST.
	     MOVE WS-SALPOS	 TO WS-SALKEY.
	     GO TO DA216-DISPLAY.
      *
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *
001980 DA218.
001990	     PERFORM DA214.
	   IF LIN > 6
002000	       DISPLAY (LIN, 1) W43-DET (LIN)
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
002010	     GO TO DA215.
      *
      *    ****    C L E A R   S C R E E N
      *
030780 DA219.
030790	     MOVE 7		 TO LIN.
030800	     PERFORM DA48 UNTIL LIN > 18.
030810	     MOVE 7		 TO LIN.
030820       MOVE 1              TO WS-LINES.
      *
      *    ****   P R E V I O U S   S C R E E N	  R E Q U I R E D
      *
       DA220.
	   IF WS-FIRST = 1
	       GO TO DA215.
	   IF WS-FIRST > 12
	       SUBTRACT 12	 FROM WS-FIRST
	   ELSE
	       MOVE 1		 TO WS-FIRST.
	     PERFORM DA219.
	     GO TO DA202.
      *
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *
       DA222.
	   IF (WS-FIRST + 12) > WS-LAST
	       GO TO DA215.
	     ADD 12		 TO WS-FIRST.
	     PERFORM DA219.
	     GO TO DA202.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
       DA224.
030630	   IF WS-OPTION = "A"
030640	       GO TO DA228.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *
       DA225.
043780	     MOVE "Are you sure Y/N?"
				 TO WS-ERR-MES.
030880	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030970	   IF NOT (WS-OPTION = "N" OR "Y")
030980	       GO TO DA225.
031000     IF WS-OPTION = "N"
031010	       GO TO DA210.
031020       MOVE "D"            TO WS-OPTION.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
       DA228.
031220	     MOVE WS-SALPOS	 TO WS-SALKEY.
031230       PERFORM READ-SALE THRU READ-SALE-EXIT.
017290	     MOVE SAL-ITEM	 TO W10-ITEM STK-CODE.
017310       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
017320	     MOVE SAL-QNT	 TO W10-QUANT.
017330	     MOVE SAL-COST	 TO W10-COST.
017340	     MOVE SAL-SELL	 TO W10-SELL.
017350	     MOVE SAL-DISC	 TO W90-CDISC.
017360	     MOVE SAL-DSC	 TO WS-DISC.
017370	     MOVE SAL-VAL	 TO W25-VALUE.
      *
      *    ****    R E D U C E   T O T A L S 
      *
017380	     SUBTRACT W25-VALUE  FROM W90-DB.
017390	     SUBTRACT W90-CDISC  FROM W90-LDISC.
017400       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
017410       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL.
017420     IF (STK-IND < 2) OR (STK-IND = 3)
017430         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
	     PERFORM DA214.
017440	   IF WS-OPTION = "A"
017450         GO TO DA250.
      *
      *    ****    R E - D I S P L A Y   T O T A L S
      *
017460	     PERFORM DA45B THRU DA45-AA.
017470 DA245.
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
017480	     MOVE WS-SALKEY	 TO WS-SALSTR.
017490	     ADD 1		 TO WS-SALKEY.
017500     IF WS-SALKEY > WS-LAST
017510         SUBTRACT 1 FROM WS-LAST
017520         GO TO DA200.
017530       PERFORM READ-SALE THRU READ-SALE-EXIT.
017540	     MOVE WS-SALSTR	 TO WS-SALKEY.
030650       SUBTRACT 1          FROM SAL-CODE.
017550       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
017560	     ADD 1		 TO WS-SALKEY.
017570       GO TO DA245.

017580 DA250.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
	   IF W85-USE-3 = "Y"
017590	       ACCEPT S26-WEIGHT AT LINE LIN
	   ELSE
017590	       ACCEPT S26-QNT AT LINE LIN.
017600	     DISPLAY CLEAR-L25.
017610     IF W10-QUANT = ZERO
044520	       MOVE "Quantity MAY NOT be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
031670	       MOVE SAL-QNT	 TO W10-QUANT
017640	       GO TO DA250.
      *
      *    ****    R E C A L C U L A T E   V A L U E S
      *
017650	     MULTIPLY W10-SELL BY W10-QUANT GIVING W25-VALUE.
017660	     MOVE W25-VALUE	 TO W15-PRICE W100-EXT.
017670	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
017780	     MOVE W10-QUANT	 TO SAL-QNT.
017790	     MOVE W25-VALUE	 TO SAL-VAL.
017800     IF WS-DISC = ZERO
017810         GO TO DA255.
017820	     COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
017830	     MOVE W90-CDISC	 TO W15-PRICE SAL-DISC.
      *
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *
017840 DA255.
017850	     PERFORM DA45 THRU DA45-AA.

       DA260.
017860       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
017870       GO TO DA200.
      *
      *    ****    C L E A R   D E T A I L   L I N E S
      *
031940 DA265.
	     PERFORM DA219.
      *
      *    ****    D I S P L A Y   L A S T   P A G E
      *
	   IF WS-LAST > 10
	       SUBTRACT 9 FROM WS-LAST
				 GIVING WS-SALKEY
	   ELSE
	       MOVE 1		 TO WS-SALKEY.

030050 DA270.
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
030100	   IF SAL-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
030120	       GO TO DA275.
030130	     DISPLAY (LIN, 2) SAL-ITEM
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030090	     DISPLAY ( , 17) "   "
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030140	     DISPLAY ( , 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
030150	     MOVE SAL-QNT	 TO W10-QUANT.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY ( , 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
       DA275.
	     ADD 1		 TO LIN.
030060	   IF WS-SALKEY < WS-LAST
	       ADD 1		 TO WS-SALKEY
030070	       GO TO DA270.

017880 DA299.
017890	     EXIT.

       DA300P	       SECTION 50.
       DA300.
	     PERFORM SAVE-SCREEN.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0843
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³  P R O M P T S  ³" AT 0943
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" AT 1043
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³ " AT 1143
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "P" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "rompts (Slow)  ³"
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³ " AT 1243
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "M" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "inimum Prompts ³"
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "³ " AT 1343
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "F" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "ast Capture    ³"
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1443
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6.
	     MOVE 09		 TO SHADE-ROW.
	     MOVE 45		 TO SHADE-COL.
	     MOVE 17		 TO SHADE-WIDTH.
	     MOVE 6		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     PERFORM HIDE-THE-CURSOR.

       DA305.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	   ELSE
	       CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO DA305.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "P" OR "M" OR "F")
	       CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO DA305.
	   IF WS-OPTION = "M"
	       MOVE "N" 	 TO WS-OPTION.
	   IF WS-OPTION = "P"
	       MOVE "Y" 	 TO WS-OPTION.
	     PERFORM DISPLAY-THE-CURSOR.
	     PERFORM RESTORE-SCREEN.
	     MOVE WS-OPTION	 TO WS-PROMPT.

017900 DA999.
017910       EXIT.
      /
       YA000	       SECTION 90.
       YA00.
	     MOVE "STP\STPSOR"	 TO PRG-NAME2.
	     PERFORM YA100.
012270	     CLOSE RECOVER.
	     MOVE "STP\ORDPRN2"	 TO PRG-NAME2.
	     PERFORM YA100.
021020	     OPEN OUTPUT RECOVER.
021030       CLOSE RECOVER.
012300	     OPEN I-O RECOVER.
       YA99.
	     EXIT.

       YA100	       SECTION 90.
       YA101.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     PERFORM SAVE-SCREEN-3.
	     CALL PRG-NAME2
		  USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY
		  ON EXCEPTION
		      GO TO YA110
	     END-CALL.
	     CANCEL PRG-NAME2.
       YA105.
	     PERFORM RESTORE-SCREEN-3.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     GO TO YA199.
       YA110.
	     MOVE SPACES	 TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     PRG-NAME2 DELIMITED " "
		     " not on disk, press ANY key"
		     DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
	     GO TO YA105.

       YA199.
	     EXIT.
      /
020720 ZA000-INIT	       SECTION 91.
020730 ZA000-OPEN.
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-USUB	 TO WS-USUB.
	     MOVE LS-DRAW	 TO WS-DRAW.
049200	     DISPLAY CLR-SCREEN.
020750	     DISPLAY "CASH DRAWER PROGRAM" AT 0231
020760                WITH FOREGROUND-COLOR 15.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
	     EXIT.

       ZA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "CHEQUE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CHEQUE.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
020770	     MOVE ZERO		 TO WS-LINES.
           IF WS-DRAW = ZERO
	       MOVE 40		 TO WS-DRAW.
020890	     MOVE 3		 TO WS-PARKEY.
020900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
	     MOVE PAR-VAT-REG	 TO W05-VATNO.
	     MOVE PAR-PROMPT	 TO WS-PROMPT.
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	     MOVE PAR-PRICED-IND TO WS-PRC-IND.
	     MOVE PAR-BANK-IND	 TO WS-BNK-IND.
	     MOVE PAR-CSHCODE	 TO WS-CSHCODE.
	     MOVE PAR-SLIP-P	 TO WS-SLIP-P.
021020	     OPEN OUTPUT RECOVER.
021030       CLOSE RECOVER.
021040       OPEN I-O RECOVER.
040400       OPEN I-O CHEQUE.
040410	   IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 36       TO WS-F-ERROR
                   GO TO ZA50.
040430     IF WS-STATUS NOT = "00"
040440         MOVE 36           TO WS-F-ERROR
040450         PERFORM OPEN-ERROR.
021290	     OPEN OUTPUT SALE.
054310     IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49.
021300     IF WS-STATUS NOT = "00"
021310	       MOVE 19		 TO WS-F-ERROR
021320         PERFORM OPEN-ERROR.
021330	     CLOSE SALE.
021340       OPEN I-O SALE.
021350	     MOVE 5		 TO WS-PARKEY.
021370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050550       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
021400	     MOVE 1		 TO WS-PARKEY.
021410       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021590     IF PAR-CSHEAD = SPACES
021600	       MOVE "Setup cash slip headings"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
021620         GO TO ZA205.
021430	     MOVE PAR-DMY	 TO W12-TODAY.
021440	     MOVE PAR-YMD	 TO W12-T-YMD.
             MOVE PAR-CSHEAD     TO W15-COMP W40-NAME.
021660       MOVE PAR-CSHADD     TO W15-ADD.
021670       MOVE PAR-TELEPHONE  TO W15-TEL.
	     MOVE 6		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-WHOLESALE  TO WS-WS.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
             MOVE PAR-DSGL       TO WX-DSGL.
021490       ADD 150 WS-DRAW GIVING WS-PARKEY.
021500       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021510	     MOVE W12-TODAY	 TO PAR-CSHDTE W15-DATE.
021520	     MOVE PAR-RECORD151  TO W85-RECORD151.
021530       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
021540	     MOVE 5		 TO WS-PARKEY.
021550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021560	     MOVE PAR-PW	 TO W85-PASS.
021680	     MOVE 32		 TO WS-S1.
020780       DISPLAY "Waiting for printer response" AT 1431
020790                WITH FOREGROUND-COLOR 14 BLINK.
021690 ZA05.
021700     IF W40-NCH (WS-S1) = SPACE
021710         SUBTRACT 1 FROM WS-S1
021720         GO TO ZA05.
021730     IF WS-S1 > 16
021740	       MOVE W85-NUL	 TO W15-EXPP1 W15-ECAN1
021750     ELSE
021760	       MOVE W85-SO	 TO W15-EXPP1
021770	       MOVE W85-CSO	 TO W15-ECAN1.
021800	     MOVE W85-SO	 TO W15-EXPP2 W15-EXPP4.
021810	     MOVE W85-CSO	 TO W15-ECAN2 W15-ECAN4.
	   IF W85-CPRN = 1
	       MOVE "LPT1"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 2
	       MOVE "LPT2"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 3
	       MOVE "LPT3"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 4
	       MOVE "COM1"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 5
	       MOVE "COM2"	 TO W02-PRINTER
           ELSE
           IF W85-CPRN = 9
	       MOVE "CSH001."	 TO W02-PRINTER
	       MOVE LS-USER	 TO W02-PUSER.
             OPEN OUTPUT CSHSLE.
	     CALL X"91" USING X91-RES X91-FUN CSHSLE.
      *
      *     O L D   S T Y L E   C A S H   D R A W E R
      *
           IF W85-DRAWER  = "F"
	       IF W85-PORT = 2
                   MOVE "COM2"   TO W02-CSHDRW
               ELSE
	       IF W85-PORT = 3
                   MOVE "COM3"   TO W02-CSHDRW
               ELSE
	       IF W85-PORT = 4
                   MOVE "COM4"   TO W02-CSHDRW.
           IF W85-DRAWER = "F"
               OPEN OUTPUT CSHDRW
	       MOVE ALL "A"	 TO CSH-OPEN.
	   IF W85-CUSDISP = "Y"
	       IF W85-CUSPORT = 2
		   MOVE "COM2"	 TO W02-CUSDSP
               ELSE
	       IF W85-CUSPORT = 3
		   MOVE "COM3"	 TO W02-CUSDSP
               ELSE
	       IF W85-CUSPORT = 4
		   MOVE "COM4"	 TO W02-CUSDSP.
	     MOVE 7		 TO WS-PARKEY.
021500	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF W85-CUSDISP = "Y"
	       OPEN OUTPUT CUSDSP
      *	       MOVE X"1B"	 TO CUS-CON
      *	       MOVE "QA"	 TO CUS-INST
	       MOVE SPACES	 TO CUS-LINE.
	   IF W85-CUSDISP = "Y"
	       MOVE PAR-DSP-HDG  TO W95-COMP.
	     GO TO ZA999.
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
       ZA50.
041850	   IF WS-F-ERROR = 36
041860         MOVE "PD CHEQUE file"
				 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.

022010 ZA200.
022020       DISPLAY "Files locked - Try later" AT 2312
022030                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
022040       DISPLAY "Press any key" AT 2512 
022050		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
022060	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
022090 ZA999.
022100       EXIT.
      /
022110 ZB000-ERROR	     SECTION 92.

051650 OPEN-ERROR.
051660       DISPLAY CLR-SCREEN.
051670       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
051680       PERFORM DISPLAY-FILE-NAME.
051690
051700 READ-ERROR.
051710       DISPLAY CLR-SCREEN.
051720       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.
051740
051750 WRITE-ERROR.
051760       DISPLAY CLR-SCREEN.
051770       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.
051780

051790 DISPLAY-FILE-NAME.
051800     IF WS-F-ERROR = 1
051810	       MOVE W02-AUDITF	 TO WS-FILE
051820	       MOVE WS-AUDKEY	 TO WS-KEY
051830     ELSE
051840     IF WS-F-ERROR = 2
051850	       MOVE W02-NETWORK  TO WS-FILE
051860	       MOVE WS-NETKEY	 TO WS-KEY
051950     ELSE
051960     IF WS-F-ERROR = 7
051970	       MOVE W02-DEPART	 TO WS-FILE
051980	       MOVE WS-DEPKEY	 TO WS-KEY
052070	   ELSE
052080     IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100         MOVE WS-PARKEY    TO WS-KEY
052110     ELSE
052120     IF WS-F-ERROR = 18
052130         MOVE W02-RECOVER  TO WS-FILE
052140         MOVE WS-RECKEY    TO WS-KEY
052150     ELSE
052160     IF WS-F-ERROR = 19
052170	       MOVE W02-SALE	 TO WS-FILE
052180	       MOVE WS-SALKEY	 TO WS-KEY
052190     ELSE
052200     IF WS-F-ERROR = 22
052210         MOVE W02-STOCKF   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE SME-KEY      TO WS-KEYX
052270     ELSE
052280     IF WS-F-ERROR = 32
052290         MOVE W02-TXTRAN   TO WS-FILE
052300         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 36
041860         MOVE W02-CHEQUE   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE CHQ-TR-NUMBER
				 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX.
052310     IF WS-STATUS = "10"
052320	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
052330     ELSE
052340     IF WS-STATUS = "22"
052350	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
052360     ELSE
052370     IF WS-STATUS = "23"
052380	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
052390     ELSE
052400     IF WS-STATUS = "24"
052410	       MOVE "DISK full"  TO WS-STAT-MESSAGE
052420     ELSE
052430     IF WS-STATUS = "30"
052440         MOVE "DISK error" TO WS-STAT-MESSAGE
052450     ELSE
052460     IF WS-STATUS = "94"
052470	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
052480       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
052490       DISPLAY "Status " AT 1212 
052500                WS-STATUS WITH FOREGROUND-COLOR 11
052510               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
052520     IF WS-STATUS NOT = "94" 
052530         DISPLAY "Key    " AT 1412 
052540                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
052550         DISPLAY "Reverse backup or contact program Support"
052560                  AT 1612.
052570         DISPLAY "Please make a note of these details" AT 1812.
052580       STOP RUN.
