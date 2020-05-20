      $set LINKCOUNT"572"
      ******************************************************************
      *                                                                *
      *    ******    ******   **    **	  ****	    ****     ******    *
      *   **	**  **	  **  **    **	 **  **	   **  **   **	  **   *
      *   **	    **	      **    **	**    **  **	**	 **    *
      *   **	     ******   ********	**    **  **	**     **      *
      *   **		  **  **    **	**    **  **	**   **	       *
      *   **	**  **	  **  **    **	 **  **	   **  **   **	       *
      *    ******    ******   **    **	  ****	    ****    ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   C A S H   R E F U N D   P R O G R A M        *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
      * 							       *
      * August 2008    - New file (STKALT) - Stock alternate index     *
      * 		 included for lookups, using any word con-     *
      * 		 tained in the Stock description.	       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CSH002.
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

       COPY APACFIDS.SL.

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
000930 77  WS-PRNINV	   PIC	X(01) VALUE "Y".
002610 77  WS-SKIP         PIC  X(01) VALUE " ".
002610 77  WS-LOOK         PIC  X(01) VALUE "S".
002620 77  WS-ETYPE        PIC  X(01) VALUE "I".
002040 77  WS-WS	   PIC	X(01).
002180 77  WS-USE-CASES    PIC	X(01).
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
000950 77  WS-NUM	   PIC	Z9.
000960 77  WS-ERROR	   PIC	9(01).
001000 77  WS-COMP	   PIC	9(01).
001010 77  WS-INDI	   PIC	9(01) VALUE ZERO.
001020 77  WS-UPDATE	   PIC	9(01) VALUE ZERO.
001030 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
001040 77  WS-HEAD	   PIC	X(01).
001050 77  WS-TYPE	   PIC	X(01).
001060 77  WS-AGE	   PIC	X(01).
001060 77  WS-PRC	   PIC	X(01).
001110 77  WS-BATCH	   PIC	9(05).
001140 77  WS-A 	   PIC	X(19) VALUE "Any key to continue".
001150 77  WS-PER	   PIC	Z(02)9.99.
001160 77  WS-CR	   PIC	X(01).
001170 77  WS-GSTRES	   PIC	S9(07)V9(08) COMP-3.
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
000070 77  WS-DRAW	   PIC	9(02).
      /
002910 01  WS-DB-LINE.
002920     03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "фммммммммммммммямммямммммм
      -        "мммммммммммммммммммммммяммммммммямммммммммямммммммммм╣".
003080	   03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "зддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддддд©".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003140	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "Ё              Ё   Ё
      -        "                       Ё        Ё         Ё          Ё".
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "╟".
	       05  FILLER      PIC  X(01) VALUE "Ё".
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "фммммммммммммммомммомммммм
      -        "мммммммммммммммммммммммоммммммммомммммммммомммммммммм╣".
003200     03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "юддддддддддддддддддддддддд
      -        "ддддддддддддддддддддддддддддддддддддддддддддддддддддды".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
003220     03  WS-TOP-LNE.
003230	       05  WS-TCHR  PIC	 X(80) VALUE "фммммммммммммммммямммммммм
      -        "ммммммммммммммммммммммяммммммммммямммммммммяммммммммм╣".
003240     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003250         05  FILLER  PIC X(01).
003260         05  WS-H-LINE
003270                     PIC X(78).
003280         05  FILLER  PIC X(01).
003290     03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "Ё                Ё
      -        "                      Ё          Ё         Ё         Ё".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003310	   03  WS-BOT-LNE.
003320	       05  WS-BCHR  PIC	 X(80) VALUE "фммммммммммммммммомммммммм
      -        "ммммммммммммммммммммммоммммммммммомммммммммоммммммммм╣".
003330	   03  WS-TP-LNE.
003340	       05  WS-TCH   PIC  X(80) VALUE "фммммммямммммммммммммммммм
      -        "ммммммммммммммммммммммямммммммммммяммммммммммяммммммм╣".
003350     03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "Ё      Ё
      -        "                      Ё           Ё          Ё       Ё".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003370	   03  WS-BT-LNE.
003380	       05  WS-BCH   PIC  X(80) VALUE "фммммммомммммммммммммммммм
      -        "ммммммммммммммммммммммомммммммммммоммммммммммоммммммм╣".
	       03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "м".

001260 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CSH".
	   03  WS-PROG	       PIC  X(03) VALUE "002".

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
           03  W02-PRINTER     PIC X(12) VALUE "LPT1".
           03  W02-CSHDRW     PIC X(04) VALUE "COM1".

       COPY W05.VAT.

       COPY W10.SAL.

       COPY W12.WS.

       01  W15-PRICE-NAMES.
	   03  W15-PNAME.
	       05  W15-CHECK   PIC  X(01).
	       05  W15-REST    PIC  X(09).

001820 01  W15-CASH-SLIP.
001830     03  W15-L1.
001860	       05  W15-EXPP1   PIC  X(01).
	       05  FILLER      PIC  X(04) VALUE SPACES.
001870         05  W15-COMP    PIC  X(32) VALUE SPACES.
001880         05  W15-ECAN1   PIC  X(01).
001890     03  W15-L1A.
001920	       05  W15-EXPP4   PIC  X(01).
	       05  FILLER      PIC  X(01) VALUE SPACES.
001930         05  W15-DOCTYP  PIC  X(18) VALUE SPACES.
001940	       05  W15-ECAN4   PIC  X(01).
001950     03  W15-L2.
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
002140     03  W15-L7A.
002150	       05  FILLER      PIC  X(19) VALUE "Cr Note/Kt Nota  :".
002160         05  W15-CRNO    PIC  X(08).
002170     03  W15-L8.
002180	       05  FILLER      PIC  X(20) VALUE "Acc. No./Rek. Nr. :".
002190         05  W15-ACCNO   PIC  X(06).
002200     03  W15-L9.
002230         05  W15-EXPP2   PIC  X(01).
002240	       05  FILLER      PIC  X(18) VALUE "  THANK YOU/DANKIE".
002250         05  W15-ECAN2   PIC  X(01).
002280	   03  W15-L11.
	       05  W15-DETAIL.
002340	       07  W15-DESC    PIC  X(28).
002350	       07  FILLER      PIC  X(01) VALUE SPACE.
002360	       07  W15-PRICE   PIC  Z(06)9.99-.
002280	   03  W15-L11A REDEFINES W15-L11.
	       05  W15-EXT-ITEM.
002300		   07  W15-ITM PIC  X(14).
		   07  W15-SL  PIC  X(01).
		   07  W15-EXT PIC  X(03).
002330	       05  FILLER      PIC  X(01).
002320	       05  W15-QUANT   PIC  ZZZ9.999-.
	       05  FILLER      PIC  X(12).
002380	   03  W15-L12 REDEFINES W15-L11.
002400         05  W15-DISCOUNT.
002420             07  W15-DISC
002430			       PIC  X(23).
002440             07  W15-DSC PIC  Z9.99.
		   07  W15-BLK PIC  X(01).
002450	       05  FILLER      PIC  X(11).
	   03  W15-L12A REDEFINES W15-L11.
	       05  FILLER      PIC  X(15).
	       05  W15-CSHCODE PIC  X(10).
	       05  FILLER      PIC  X(15).
	   03  W15-CUSDSP REDEFINES W15-L11.
	       05  W15-CUSDESC.
		   07  W15-HD1 PIC  X(10).
		   07  W15-DUE PIC  Z(06)9.99.
	       05  W15-CUS2.
		   07  W15-HD2 PIC  X(10).
		   07  W15-CHG PIC  Z(06)9.99.
	   03  W15-CARTONS REDEFINES W15-L11.
	       05  FILLER      PIC  X(10).
	       05  W15-HD3     PIC  X(16).
	       05  W15-CRTNS   PIC  ZZ9.
	       05  FILLER      PIC  X(11).
002460	   03  W15-L13.
002470	       05  W15-REMK    PIC X(29).
002480         05  W15-VAL     PIC Z(06)9.99-.
	   03  W15-L14 REDEFINES W15-L13.
	       05  W15-KQ      PIC  ZZZZZZZ9-.
	       05  FILLER      PIC  X(01).
	       05  W15-KD      PIC  X(30).

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

       COPY W85.WS.

003160 01  W85-PASS.
003180     03  W85-SUPER OCCURS 9
003190                         PIC  X(06).
003200     03  W85-ENTRY       PIC  9(02)    COMP-3.
003210     03  W85-MARG        PIC S9(02)V99.

003230 01  W90-TRAN.
003990	   03  W90-DATE        PIC  9(08)    COMP-3.
004000	   03  W90-DTE	       PIC  99/99/9999.
003270	   03  W90-CODE        PIC  9(02).
003280     03  W90-ORD         PIC  X(12).
003290	   03  W90-REF.
	       05  W90-REF-NO  PIC  9(08).
003300     03  W90-REG         PIC  X(09).
003310	   03  W90-DB	       PIC  9(09)V99 COMP-3.
003320	   03  W90-CR	       PIC  9(09)V99 COMP-3.
003330	   03  W90-BDT	       PIC S9(09)V99 COMP-3.
003340	   03  W90-BCR	       PIC S9(09)V99 COMP-3.
003350     03  W90-DESC        PIC  X(12).
003400	   03  W90-DUE	       PIC S9(09)V99 COMP-3.
003410	   03  W90-CASH        PIC S9(09)V99 COMP-3.
003420	   03  W90-CHQ	       PIC S9(09)V99 COMP-3.
003420	   03  W90-EFT	       PIC S9(09)V99 COMP-3.
003430	   03  W90-CARD        PIC S9(09)V99 COMP-3.
003430	   03  W90-SPEED       PIC S9(09)V99 COMP-3.
003430	   03  W90-VOUCH       PIC S9(09)V99 COMP-3.
003430	   03  W90-CRNOTE      PIC S9(09)V99 COMP-3.
003440	   03  W90-CHANGE      PIC S9(09)V99 COMP-3.
003450	   03  W90-PMT	       PIC S9(09)V99 COMP-3.
003470	   03  W90-BAL	       PIC S9(08)V99 COMP-3.
003480     03  W90-TAX         PIC  X(14).
003490	   03  W90-DVAL        PIC  9(09)V99.
003500	   03  W90-SALES       PIC S9(09)V99  COMP-3.
003510	   03  W90-TDISC       PIC S9(09)V99  COMP-3.
003520	   03  W90-ATAX        PIC S9(09)V99  COMP-3.
003520	   03  W90-TTAX        PIC S9(09)V99  COMP-3.
003530	   03  W90-TCOST       PIC S9(09)V99  COMP-3.
003540	   03  W90-TSELL       PIC S9(09)V99  COMP-3.
003550	   03  W90-DISC        PIC S9(09)V99  COMP-3.
003560	   03  W90-CRD	       PIC S9(09)V99  COMP-3.
003570	   03  W90-CDISC       PIC S9(09)V99  COMP-3.
003580	   03  W90-LDISC       PIC S9(09)V99  COMP-3.
	   03  W90-TAXABLE     OCCURS 12.
006790	       05  W90-TAXVAL  PIC S9(09)V99  COMP-3.
	       05  W90-VAT     PIC S9(09)V99  COMP-3.
006550	   03  W90-EXCDIS      PIC S9(09)V99  COMP-3.
006550	   03  W90-EXCVAL      PIC S9(09)V99  COMP-3.
006550	   03  W90-EXCTOT      PIC S9(09)V99  COMP-3.
003600	   03  W90-NONTAX      PIC S9(09)V99  COMP-3.
003610	   03  W90-VALUE       PIC S9(09)V99  COMP-3.
006570	   03  W90-STOTAL      PIC S9(09)V99  COMP-3.
003620	   03  W90-TOTAL       PIC S9(09)V9(06) COMP-3.
	   03  W90-SLIP-TOT    PIC  9(09)V9(02).
	   03  W90-SLIP-RND REDEFINES W90-SLIP-TOT.
	       05  FILLER      PIC  X(06).
	       05  W90-RR      PIC  9(01).
	       05  W90-RC      PIC  9(02).
	   03  W90-ROUND       PIC  9(01)V99.
	   03  W90-ROUNDING REDEFINES W90-ROUND.
	       05  W90-R       PIC  9(01).
	       05  W90-C       PIC  9(02).
	   03  W90-RTVAL       PIC  9(01)V99 COMP-3.
	   03  W90-RTTAX       PIC  9(01)V99 COMP-3.
	   03  W90-CLNT-VAT    PIC  X(14).

003630 01  W95-STM.
003640	   03  W95-COMP.
	       05  W95-DSP-L1  PIC  X(20).
	       05  W95-DSP-L2  PIC  X(20).
	   03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.

003710 01  W100-EDIT.
003720     03  W100-BAL        PIC  Z(06)9.99-.
003970	   03  W100-V2.
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

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
003780 SCREEN SECTION.

	COPY BLANK.CRT.

003880 01  S05.
003890	   03  LINE 12 COLUMN 12 VALUE "Enter Password".

       01  S06.
003910	   03  LINE 12 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC  X(06)
				 USING W40-CSHPASS AUTO SECURE.

003930 01  S07.
	   03  LINE  1 COLUMN 64 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 VALUE "Cash Drawer: ".
	   03	       COLUMN 77 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC  Z9 FROM WS-DRAW.
003940	   03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15 VALUE "CASH SALE".
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
009160	   03  LINE  6 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ф".
009160	   03	       COLUMN  2 VALUE "ммммммммммммммямммяммммммммммммм
      -        "ммммммммммммммммяммммммммямммммммммямммммммммм".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "╣".
009160	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ф".
009170	   03	       COLUMN  2 VALUE "ммммммммммммммомммоммммммммммммм
      -        "ммммммммммммммммоммммммммомммммммммомммммммммм".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "╣".
004060	   03  LINE 20 COLUMN 3  VALUE "Sub Total ".
004110	   03  LINE 21 COLUMN 3  VALUE "Discount  ".
004150	   03  LINE 22 COLUMN 3  VALUE "T A X -VAT".
004190     03  LINE 23 COLUMN 3  VALUE "AMOUNT DUE".

007910 01  S12.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(14) USING W10-ITEM AUTO.

       01  S12-ITM.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(03) USING W10-ITM AUTO.

005530 01  S12-EXT.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

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

004550 01  S26-CRNOTE.
004560	   03  LINE 20 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
004570				 PIC Z(06)9.99- USING W90-CRNOTE AUTO.

004550 01  S26-EFT.
004560	   03  LINE 21 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
004570				 PIC Z(06)9.99- USING W90-EFT AUTO.

009920 01  S29.
009930	   03  LINE 12 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009940                           PIC Z(06)9.99 USING PAR-FLOAT AUTO.

       COPY S99.CRT.

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
	     PERFORM JA000.
	     CLOSE CSHSLE
		   CHEQUE
		   RECOVER
		   SALE.
       AA49.
             EXIT PROGRAM.

       COPY AA50.LUP.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  SCREEN-SHADOW 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:						       Ё
      *	   Ё	       SHADE-ROW   - Top line of the box + 1.	       Ё
      *	   Ё	       SHADE-COL   - Left line of box + 2.	       Ё
      *	   Ё	       SHADE-WIDTH - Width of the box - 2.	       Ё
      *	   Ё	       SHADE-LINES - Hight of box - 1.		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  CHECK-CORRECT 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё	      "Correct ENTER - N if incorrect"		       Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 13 (default); move another value to SLIN and PERFORM      Ё
      *    Ё CHECK-SETUP THRU CHECK-EXIT.			       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  ERROR-MESSAGE 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display an error message with " - Press ANY key" at    Ё
      *    Ё the end; use PERFORM ERROR-MESSAGE.		       Ё
      *    Ё							       Ё
      *    Ё To display just the error message; use PERFORM	       Ё
      *    Ё ERROR-LENGTH THRU ERROR-EXIT.			       Ё
      *    Ё							       Ё
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   Ё
      *    Ё or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     Ё
      *    Ё number to be used to SLIN and then PERFORM ERROR-DISPLAY  Ё
      *    Ё THRU ERROR-EXIT.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY FUNCTION.CRT.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			  OPT-MESSAGE			       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine is used to allow the OPERATOR to respond to  Ё
      *    Ё a request for an option, so that the correct action can   Ё
      *    Ё be performed by the program. The routine will display the Ё
      *    Ё message in a pop-up window and allow the OPERATOR to      Ё
      *    Ё respond to the 'question'. 			       Ё
      *    Ё							       Ё
      *    Ё The option request must be placed in WS-ERR-MES and may   Ё
      *    Ё not exceed 48 characters. The message will be centred in  Ё
      *    Ё the window. An example of a message request follows:      Ё
      *    Ё							       Ё
      *    Ё   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      Ё
      *    Ё   PERFORM OPT-MESSAGE.				       Ё
      *    Ё							       Ё
      *    Ё This would be displayed as:			       Ё
      *    Ё	здддддддддддддддддддддддддддддддддддддддддддддддд©     Ё
      *    Ё	Ё	   Print transactions (Y/N) [ ] 	 Ё╟╟   Ё
      *    Ё	юдддддддддддддддддддддддддддддддддддддддддддддддды╟╟   Ё
      *    Ё	  ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟   Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To display the request message higher or lower (default   Ё
      *    Ё is line 13) move the line number to be used to SLIN and   Ё
      *    Ё then PEFORM OPT-SETUP THRU OPT-EXIT.		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

      *    *************************************************************
      *    ****        D A T E	 C H E C K   R O U T I N E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Perform this routine with the date in W10-DATE (PIC      Ё
      *    Ё  99/99/9999) with the format as DD/MM/CCYY 	       Ё
      *    Ё							       Ё
      *    Ё  On exiting - WS-ERROR will be zero (0) if the date is    Ё
      *    Ё  correct or one (1) if the date is incorrect.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY DATE.CHK.

      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P	O P T I O N S
      *    *************************************************************
011010 AA300		  SECTION.
011020 AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011020		     "=Item,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011040		     "=Alt,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F4" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011060		     "=Desc,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F5" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011060		     "=Desc2,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F6" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011080		     "=X/Ref,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F9" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011080		     "=Word,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape to end"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

011110 AA305.
011120       EXIT.

      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       R E A D	 F I L E S   R O U T I N E S	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
004930 AC000-READ	       SECTION.

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

      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    ЁR E W R I T E   &	W R I T E   F I L E S	R O U T I N E SЁ
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
006420 AD000-WRITE	       SECTION.

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

       COPY APAC.HLP.

      /
      *       ****   *****   ***    ***   *   *  *****	****   *   *
      *       *   *  *	    *	*  *   *  *   *  *	*   *  *   *
      *       ****   ***    *	   *   *  *   *  ***	****	* *
      *       *   *  *	    *	*  *   *   * *	 *	*   *	 *
      *       *   *  *****   ***    ***     *	 *****	*   *	 *
      *    ммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм
      * 	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм
      *        зддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *        Ё ORIGINAL ACTION (REC-TYPE)  дд  RECOVERY PROCESS Ё
      *        Ё 0 = RECORD CHANGED	     дд     (REWRITE)	   Ё
      *        Ё 1 = RECORD WAS ADDED	     дд     (DELETE)	   Ё
      *        Ё 2 = RECORD WAS DELETED      дд     (WRITE)	   Ё
      *        юддддддддддддддддддддддддддддддддддддддддддддддддддды
011670 AY000	       SECTION.
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
	     MOVE 38		 TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE.
012120	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "XXXX"	 TO DPT-CODE
	       GO TO AY38.
012130	     MOVE DPT-RECORD	 TO REC-DEPART.
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

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

       COPY LEDTRF.UPD.

      /
009550 JA000		       SECTION 50.
009560 JA00.
018730       DISPLAY CLR-SCREEN.
018740	     DISPLAY "PROCESS A CASH REFUND" AT 0230
018750                WITH FOREGROUND-COLOR 15.
018760	     MOVE SPACES	 TO W40-CSHPASS.
018770       DISPLAY S05.
018790       ACCEPT S06.
018800       MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018860	     MOVE 1		 TO WS-LENGTH.
018870       MOVE 5              TO WS-PARKEY.
018880       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018890       MOVE PAR-PW         TO W85-PASS.
018900     IF NOT (W40-CSHPASS = W85-SUPER (1) OR
018940                           W85-SUPER (5) OR
018990				 "DNUFER" OR "ERHSAC")
048290	       MOVE "Invalid password"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
019030	       GO TO JA150.
009600       ADD 150 WS-DRAW     GIVING WS-PARKEY.
009610       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009620       MOVE PAR-RECORD151  TO W85-RECORD151.

009630 JA05.
009640       DISPLAY CLR-SCREEN.
009650       DISPLAY S07.
             DISPLAY "CASH REFUND" AT 0234 WITH FOREGROUND-COLOR 15.
009660	     MOVE 7		 TO LIN.
009670	     PERFORM JA48 UNTIL LIN > 18.
021510	     MOVE W12-TODAY	 TO  W90-DTE W15-DATE.
009690	     DISPLAY W90-DTE AT 0370
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
009700	     MOVE ZERO		 TO W90-LDISC W90-BDT
				    W90-RTVAL W90-BAL
				    W90-SALES W90-EXCTOT
				    W90-RTTAX W90-NONTAX
				    W10-SMAN  W90-EXCVAL.
009720	     MOVE 1		 TO WS-FIRST WS-SALPOS WS-LAST.
009730       MOVE SPACES         TO W90-TAX.
009740       MOVE SPACES         TO CSH-DATA.
	     MOVE 1		 TO WS-S9.

       JA05A.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO JA05A.

009750 JA06.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
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
		     GO TO JA150
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO JA06
		 WHEN OTHER
		     CALL X"E5"
		     GO TO JA06
	       END-EVALUATE.
	   IF W10-SMAN = 99
	       GO TO JA150.
009780     IF W10-SMAN > 50
009790         GO TO JA06.
	     DISPLAY CLEAR-L25.
009800     IF W10-SMAN = ZERO
009810         GO TO JA07.
009820       ADD 100 W10-SMAN    GIVING WS-PARKEY.
009830       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
009840     IF PAR-SALESMAN = SPACES
009850         GO TO JA06.
009860	     DISPLAY PAR-SALESMAN AT 0317
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

009870 JA07.
009970	     MOVE PAR-SALESMAN	 TO W15-SMAN.
009980	     DISPLAY CLEAR-L25.

       JA08.
	     MOVE SPACES	 TO WS-ERR-MES.
	     STRING "'1'=" DELIMITED SIZE
		    WS-RT-HD DELIMITED "  "
		    "," DELIMITED SIZE
		    "'2'=" DELIMITED SIZE
		    WS-CS-HD DELIMITED "  "
		    "," DELIMITED SIZE
		    "'3'=" DELIMITED SIZE
		    WS-WS-HD DELIMITED "  "
		    " [1]" DELIMITED SIZE
		    INTO WS-ERR-MES.
	     MOVE "1"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-PRC.
017600	   IF NOT (WS-PRC = "1" OR "2" OR "3")
017610	       GO TO JA08.

019770 JA10.
010260	     DISPLAY CLEAR-L25.

010270 JA20.
010280       MOVE 7              TO LIN.
010290       MOVE ZERO           TO W90-DB.

010300 JA25.
010310	     MOVE SPACES	 TO W10-DESC W10-EXT-ITEM.
	   IF W10-STCK = "N"
	       MOVE ZERO	 TO W10-I14.
010320	     MOVE ZERO		 TO W10-SELL WS-SUPER.
             MOVE 1              TO W10-QUANT.
      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P	O P T I O N S
      *    *************************************************************
010330 JA30.
             PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Is an 18 character stock/inventory item code being used?  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF WS-EXT-STK = "Y"
	       ACCEPT S12-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	   IF W10-STCK = "A"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Is a 3 character extension code being used in addition to Ё
      *    Ё the 14 character stock/inventory item code?	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       IF WS-USE-ITM = "Y"
		   ACCEPT S12-ITM AT LINE LIN
	       ELSE
021070		   ACCEPT S12 AT LINE LIN
	       END-IF
	   ELSE
      *    *************************************************************
      *    ****       N U M E R I C   C O D E S	  A P P L Y	    ****
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Check the length of the numeric Stock/Inventory item code Ё
      *    Ё being used and allow the User to key in the code.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF W10-SLNGTH = 3
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   3 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   4 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   5 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 6
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   6 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   7 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   8 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   9 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   10 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   11 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   12 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   13 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   14 Digit numeric Stock/Inventory item code	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       ACCEPT S12-14 AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
017730		     DISPLAY (LIN, 2) WS-MIV78
			      WITH FOREGROUND-COLOR 3
				   BACKGROUND-COLOR 1
		     MOVE SPACES TO W10-EXT-ITEM
		     SUBTRACT 1  FROM WS-LAST
		     GO TO JA49
		 WHEN F1-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   Apac Help Routine				       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   Lookup using 1 or more characters of Item Code      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё   Lookup using 1 or more characters of Alternate Code     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё   Lookup using 1 or more characters of Item Description   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   Lookup using 1 or more chars of Item Description 2  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё	   Lookup using 1 or more characters of X-Ref Code     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё  Lookup using any word or part in the Stock Description.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO JA30
	       END-EVALUATE
016560	       PERFORM AA50-LOOKUP
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	       IF WS-EXT-STK = "Y"
		   DISPLAY S12-EXT AT LINE LIN
	       ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S12-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S12 AT LINE LIN
		   END-IF
	       END-IF
021020	       IF W10-EXT-ITEM = SPACES
021030		   GO TO JA30.
021040	   IF W10-EXT-ITEM = SPACES
021050         SUBTRACT 1        FROM WS-LAST
021060	       GO TO JA49.

       JA31.
             PERFORM CA000.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S12-EXT AT LINE LIN
	   ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S12-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S12 AT LINE LIN.
010400	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50-LOOKUP
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	       IF WS-EXT-STK = "Y"
		   DISPLAY S12-EXT AT LINE LIN
	       ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S12-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S12 AT LINE LIN
		   END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720		   GO TO JA30
               ELSE
                   GO TO JA31.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JA30.
010430	     MOVE WS-LAST	 TO WS-SALKEY.
010440       INITIALIZE SAL-RECORD1.
010450	     MOVE WS-LAST	 TO SAL-CODE.
      *
      *    ****    I T E M   N U M B E R   O N   C A S H   S L I P
      *
010460	     DISPLAY (LIN, 17) "   "
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
010470	     MOVE STK-CODE	 TO SAL-EXT-ITEM.
010480	     DISPLAY CLEAR-L25.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
010490 JA35.
010500     IF STK-IND = 3
               MOVE ZERO         TO STK-AVGE
010510	       ACCEPT (LIN, 21) W10-SDSC
010520		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5 UPDATE AUTO
010530         DISPLAY (25, 12) "Index Unit cost"
010540		       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
010550         ACCEPT S26-AVGE AT LINE 25
010560     ELSE
010570         MOVE STK-DESC TO W10-DESC
010580	       DISPLAY (LIN, 21) W10-SDSC
			WITH FOREGROUND-COLOR 3 HIGHLIGHT
			     BACKGROUND-COLOR 0.
021050     IF STK-IND = 3
021060         IF STK-AVGE = ZERO
034710		   MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO JA35.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Until the condition that occasionaly causes the Average   Ё
      *    Ё to be negative is identified and corrected, the following Ё
      *    Ё precaution will remain in place.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF (STK-AVGE < ZERO) OR
	      (STK-AVGE > (STK-COST * 2))
	       MOVE STK-COST	 TO STK-AVGE.
021100	     DISPLAY CLEAR-L25.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
010590	     MOVE W10-EXT-ITEM	 TO SAL-EXT-ITEM.
010600	     MOVE W10-DESC	 TO SAL-DESC.
010610	     MOVE STK-AVGE	 TO SAL-COST W10-COST.
      *
      *    ****    G E T   Q U A N T I T Y
      *
	   IF W85-USE-3 = "Y"
021370	       ACCEPT S26-WEIGHT AT LINE LIN
	   ELSE
010620	       ACCEPT S26-QNT AT LINE LIN.
017720     IF W10-QUANT = ZERO
017730	       DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
017740         GO TO JA25.
010630	     DISPLAY CLEAR-L25.
      *
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *
021190     IF STK-IND < 3
021320         IF W90-TAX NOT = SPACES
021330		   IF WS-PRC = "1"
021340                 MOVE STK-SELL 
021350                           TO W10-SELL
021360             ELSE
021370		   IF WS-PRC = "2"
021380                 MOVE STK-CASH 
021390                           TO W10-SELL
021400             ELSE
021410                 MOVE STK-WSALE 
021420                           TO W10-SELL
021430         ELSE
021440		   IF WS-PRC = "1"
021450                 MOVE W10-VSELL 
021460                           TO W10-SELL
021470             ELSE
021480		   IF WS-PRC = "2"
021490                 MOVE W10-VCASH 
021500                           TO W10-SELL
021510             ELSE
021520                 MOVE W10-VWSALE 
021530                           TO W10-SELL.
      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Store selling price as determined above to check against	Ё
      *    Ё  the amount of refund when a user changes the price. A re- Ё
      *    Ё  fund may not be more than the original price paid!	Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE W10-SELL	 TO W10-CHECK.

010720 JA35A.
      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Only Users with a sales security level greater than 2 are	Ё
      *    Ё allowed to change the selling price.			Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF (LS0-SALEV > 2)
010730	       ACCEPT S26-PRC AT LINE LIN
	   ELSE
	       DISPLAY S26-PRC AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO JA35
	       END-EVALUATE.
      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё If the selling price for a refund exceeds the normal price Ё
      *    Ё selling price, a password is required!			Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF W10-SELL > W10-CHECK
018260	       DISPLAY "Enter password" AT 2551
018270			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
	       MOVE SPACES	 TO W40-CSHPASS
018280	       ACCEPT W40-CSHPASS AT 2567
		      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
018290			   AUTO NO-ECHO
018300	       MOVE 6		 TO WS-LENGTH
	       CALL "CBL_TOUPPER" USING W40-CSHPASS
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
018360	       MOVE 1		 TO WS-LENGTH
	       IF NOT (W40-CSHPASS = W85-SUPER (1) OR
		       W85-SUPER (9) OR "LLES<L")
		   MOVE W10-CHECK
				 TO W10-SELL
		   DISPLAY S26-PRC AT LINE LIN.
010740	     MOVE W10-SELL	 TO W10-SPAPRC.

       JA35B.
	     MOVE W10-SELL	 TO SAL-SELL.

011200 JA40.
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
011250	     DISPLAY (25, 18) "Discount %"
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
011260       MOVE ZERO TO WS-DISC W90-CDISC.

011270 JA42.       
011280       ACCEPT S26-DSC AT LINE 25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
011140		     DISPLAY CLEAR-L25
		     GO TO JA35A
	       END-EVALUATE.
011290	   IF WS-DISC = ZERO
011300         GO TO JA45.
011310       COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
011320		    (W10-SELL * WS-DISC / 100.00000).
011360	     MOVE WS-DISC	 TO SAL-DSC.
011370	     COMPUTE W90-CDISC = ((W25-VALUE * WS-DISC)
                 / 100.00000).
011380	     MOVE W90-CDISC	 TO W15-PRICE SAL-DISC.
011390	     DISPLAY (25, 40) W15-PRICE
		     WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
011400 JA45.
011510	   IF (STK-IND < 2) OR (STK-IND = 3)
011520         COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
011530	     ADD W90-CDISC	 TO W90-LDISC.
011540	     SUBTRACT W90-CDISC FROM W25-VALUE
				 GIVING W90-VALUE.
	     MOVE STK-TAX	 TO SAL-VAT-SUB.
	     MOVE W05-VAT(SAL-VAT-SUB)
				 TO W05-RATE.
	   IF STK-TAX = 0
022540         ADD W90-VALUE     TO W90-NONTAX
022550         MOVE W90-VALUE    TO SAL-NONTAX
022560     ELSE
022570         MOVE W90-VALUE    TO SAL-TAXVAL
022580	       ADD W90-VALUE	 TO W90-TAXVAL(SAL-VAT-SUB).
           IF W90-TAX NOT = SPACES
	       COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
	     ADD SAL-VAT	 TO W90-VAT(SAL-VAT-SUB).
      *
      *    ****    A C C U M U L A T E   T O T A L   R E F U N D
      *
011610	     ADD W25-VALUE	 TO W90-DB.

011620 JA45B.
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

       JA45-AA.
011750	     MOVE W90-TOTAL	 TO W15-VAL.
011760	     DISPLAY (23, 15) W15-VAL
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *
011770 JA45C.
036100	     MOVE "'A'ccept or 'C'ancel   [A]"
				 TO WS-ERR-MES.
022850	     MOVE "A"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
011900	   IF NOT (WS-OPTION = "A" OR "C")
011910	       GO TO JA45C.
011930     IF WS-OPTION = "C"
011940	       DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
               PERFORM JA47
               PERFORM JA45B THRU JA45-AA
011950         GO TO JA25.               

011820 JA46.
	     MOVE WS-SUPER	 TO SAL-SUPER.
011960       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
011970	     ADD 1		 TO WS-LAST.
011980     IF LIN < 18
011990	       ADD 1		 TO LIN
012000         GO TO JA25.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 7		 TO WS-S7.

       JA46-SHIFT.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO JA46-SHIFT.
	     PERFORM RESTORE-SCREEN-2.
023890	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
	     ADD 1		 TO WS-SALPOS.

       JA46-EXIT.
012040	     GO TO JA25.

       JA47.
025340	     SUBTRACT W25-VALUE  FROM W90-DB.
025350	     SUBTRACT W90-CDISC  FROM W90-LDISC.
025360       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF SAL-VAT-SUB > ZERO
036780	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(SAL-VAT-SUB).
025380	   IF (STK-IND < 2) OR (STK-IND = 3)
025390         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).

012050 JA48.
023890	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
012070	     ADD 1		 TO LIN.

012080 JA49.
012090     IF WS-LAST = ZERO
012100         GO TO JA05.
      *
      *    ****    P R O C E S S   T H E   R E F U N D
      *
012110 JA50.
036900	     MOVE "'P'rocess, 'A'mend, 'C'ancel or 'M'ore items"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
012150 JA51.
	     PERFORM OPT-MESSAGE.
012220	   IF NOT (WS-OPTION = "P" OR "A" OR "C" or "M")
012230	       GO TO JA50.
023780     IF WS-OPTION = "M"
023790         ADD 1             TO WS-LAST
023800         GO TO JA25.
012240     IF WS-OPTION = "A"
012250	       PERFORM JA200A
012260         GO TO JA50.
           IF WS-OPTION = "C"
               GO TO JA05.

       JA55.
012270	     MOVE ZERO		 TO WS-TDISC W90-TDISC.
012280     IF W90-LDISC NOT = ZERO
012290	       MOVE W90-LDISC	 TO W90-TDISC
012300         GO TO JA60.
012310	     MOVE W90-DB	 TO W90-DVAL.

012520 JA60.
012530       SUBTRACT W90-TDISC FROM W90-DB GIVING W90-TSELL W90-TOTAL.
012540       MULTIPLY -1 BY W90-TDISC GIVING W15-VAL.
012550	     DISPLAY W15-VAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
012560	     MOVE ZERO		 TO W90-TTAX W90-ATAX.
      *
      *    ****    VERSION 6.62 - ALLOW FOR SPEED-POINT
      * 			- USE WINDOW FOR PAYMENT
      *
	     PERFORM SAVE-SCREEN-2.

       JA62.
	     DISPLAY "зддддддддддддддддддддддддддддддд" AT 1043
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "©"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 1143
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " R E F U N D  - D E T A I L S  Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 1243
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " REFUND DUE   		     Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 1343
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Balance      		     Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 1443
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "                               Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 1543
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Cash                          Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 1643
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Check/Cheque                  Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 1743
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Credit Card Slip              Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 1843
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Speed Point Slip              Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 1943
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Voucher (Gift)		     Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 2043
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " Credit Note                   Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "Ё" AT 2143
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     " E.F.T.                        Ё"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "ю" AT 2243
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	     DISPLAY "ддддддддддддддддддддддддддддддды"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 45		 TO SHADE-COL.
	     MOVE 31		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.

012660 JA65.
012670	     MOVE ZERO		 TO W90-DUE W90-CASH  W90-VOUCH
012680				    W90-PMT W90-CARD  W90-CHANGE
				    W90-CHQ W90-SPEED W90-CRNOTE
				    W90-EFT.
012690	     MOVE W90-TOTAL	 TO W15-VAL W90-DUE.
012700	     DISPLAY W15-VAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
012700	     DISPLAY W15-VAL AT 1263
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *
      *    ****    C A L C U L A T E   U N P A I D   B A L A N C E
      *
012710 JA71.
012720       COMPUTE W90-CRD = W90-DUE - W90-PMT.
012730	     MOVE W90-CRD	 TO W100-BAL.
012740	     DISPLAY W100-BAL AT 1363
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			C A S H   V A L U E		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
012750 JA72.
012760	     ACCEPT S26-CASH.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO JA50
	       END-EVALUATE.
012770	     MOVE W90-CASH	 TO W90-PMT.
012800       PERFORM JA71.
012780     IF W90-PMT NOT < W90-DUE
012790         GO TO JA74.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      C H E Q U E   V A L U E		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
012810 JA73.
012820	     ACCEPT S26-CHQ.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO JA50
	       END-EVALUATE.
012830	     ADD W90-CHQ	 TO W90-PMT.
012860       PERFORM JA71.
012840     IF W90-PMT NOT < W90-DUE
012850         GO TO JA74.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		 C R E D I T   C A R D	 V A L U E	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
012870	     ACCEPT S26-CARD.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO JA50
	       END-EVALUATE.
012880	     ADD W90-CARD	 TO W90-PMT.
012860       PERFORM JA71.
012840	   IF W90-PMT NOT < W90-DUE
012850         GO TO JA74.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		 S P E E D   P O I N T	 V A L U E	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
012870	     ACCEPT S26-SPEED.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO JA50
	       END-EVALUATE.
012880	     ADD W90-SPEED	 TO W90-PMT.
012860       PERFORM JA71.
012840	   IF W90-PMT NOT < W90-DUE
012850         GO TO JA74.
      *
      *    ****    V O U C H E R   V A L U E
      *
012870	     ACCEPT S26-VOUCH.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO JA50
	       END-EVALUATE.
012880	     ADD W90-VOUCH	 TO W90-PMT.
012860       PERFORM JA71.
012840	   IF W90-PMT NOT < W90-DUE
012850         GO TO JA74.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      E . F . T .   V A L U E		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
012820	     ACCEPT S26-EFT.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO JA50
	       END-EVALUATE.
012830	     ADD W90-EFT	 TO W90-PMT.
012860	     PERFORM JA71.
012840	   IF W90-PMT NOT < W90-DUE
012850         GO TO JA74.
      *
      *    ****    C R E D I T	 N O T E   V A L U E
      *
012870	     ACCEPT S26-CRNOTE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     PERFORM RESTORE-SCREEN-2
		     GO TO JA50
	       END-EVALUATE.
012880	     ADD W90-CRNOTE	 TO W90-PMT.
012860       PERFORM JA71.
      *
      *    ****    C H E C K   F O R   S H O R T   P A Y M E N T
      *
012890     IF W90-PMT < W90-DUE
012900         GO TO JA76.
012910 JA74.
      *
      *    ****    A R E   P A Y M E N T   D E T A I L S   C O R R E C T
      *
           IF W90-PMT NOT = W90-DUE
	       MOVE "May NOT exceed Refund Amount"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JA65.
      *
      *    ****    A R E   P A Y M E N T   D E T A I L S   C O R R E C T
      *
012950 JA75.
024810	     MOVE "'C'orrect or 'R'ekey refund"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
013020	   IF NOT (WS-OPTION = "C" OR "R")
013030         GO TO JA75.
013050     IF WS-OPTION = "R"
013060         GO TO JA65.           
013100       GO TO JA79.
013110 JA76.
013120	     MOVE "Short - REKEY Refund"
				 TO WS-ERR-STRING
025040	       PERFORM ERROR-MESSAGE
013150       MOVE 20 TO LIN.
013160 JA78.
013220	     GO TO JA62.
013230 JA79.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
013240	     DISPLAY CLEAR-L25.
013250       DISPLAY "Processing Refund" AT 2520 WITH
013260		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
013270	     MOVE 1		 TO WS-SALKEY.
013280       PERFORM AY60 THRU AY999.
013330	     MOVE W70-INVOICE	 TO W90-REF-NO W15-CRNO.
013290     IF W70-INVOICE = 99999999
013300	       MOVE 1		 TO W70-INVOICE
013310     ELSE
013320	       ADD 1		 TO W70-INVOICE.
013340	     DISPLAY (4, 10) W90-REF
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     MOVE W85-SI	 TO CSH-DATA.
	     WRITE CSH-L.
013350	     MOVE "    CASH REFUND    "
				 TO W15-DOCTYP.
013360	     MOVE W15-L1A	 TO CSH-DATA.
013370       WRITE CSH-L.
013380	     MOVE W15-L1	 TO CSH-DATA.
013390       WRITE CSH-L.
013400	     MOVE W15-L2	 TO CSH-DATA.
013410       WRITE CSH-L.
013420	     MOVE W15-L3	 TO CSH-DATA.
013430       WRITE CSH-L.
           IF W05-VATNO NOT = SPACES
               MOVE W05-VATNO    TO W15-GNO
               MOVE W15-L3A      TO CSH-DATA
               WRITE CSH-L.
013440	     MOVE W15-L4	 TO CSH-DATA.
013450       WRITE CSH-L.
012150	     ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
	     MOVE W95-TIME	 TO W15-TIME.
013440	     MOVE W15-L4A	 TO CSH-DATA.
013450       WRITE CSH-L.
013460	   IF W10-SMAN NOT = ZERO
013470	       MOVE W15-L5	 TO CSH-DATA
013480         WRITE CSH-L.
013490	     MOVE W15-L7A	 TO CSH-DATA.
013500       WRITE CSH-L.
013510	     MOVE ALL "-"	 TO CSH-LINE.
013520       WRITE CSH-L.
013530 JA80.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            P R I N T   T H E   C A S H   S L I P
      *
013540     IF WS-SALKEY > WS-LAST
013550         GO TO JA95.
013560       PERFORM READ-SALE THRU READ-SALE-EXIT.
013570	     MOVE SAL-EXT-ITEM	 TO STK-CODE.
             PERFORM AY05 THRU AY59.
	     MOVE SPACES	 TO W15-DETAIL.
	     MOVE "CASH"	 TO CRD-AC.
	     MOVE ZERO		 TO CRD-SEQ.
017540	     MOVE W10-SMAN	 TO CRD-SMAN.
017550	     MOVE W90-REF	 TO CRD-REF.
	     MOVE STK-SUPP	 TO CRD-SUPP.
	     MOVE W12-T-YMD	 TO CRD-DTE.
	     MOVE "CASH REFUND"	 TO CRD-NAME.
	     MOVE SPACES	 TO CRD-WHS.
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO W15-ITM
	       MOVE "/" 	 TO W15-SL
	       MOVE STK-ITM	 TO W15-EXT
	   ELSE
	       MOVE STK-CODE	 TO W15-EXT-ITEM.
013590	     MOVE SAL-EXT-ITEM	 TO CRD-EXT-CODE.
013610       MOVE SAL-QNT        TO W10-QUANT W15-QUANT.
	     MOVE STK-LDG	 TO CRD-DEPT.
	     MULTIPLY SAL-QNT BY -1 GIVING CRD-QNT.
013680	     MOVE W15-L11A	 TO CSH-DATA
013690	     WRITE CSH-L.
	     MOVE SPACES	 TO W15-DETAIL.
013600	     MOVE SAL-DESC	 TO W15-DESC.
013620       MOVE SAL-VAL        TO W15-PRICE W25-VALUE.
013630       MOVE SAL-COST       TO W10-COST.
	     MULTIPLY SAL-COST BY -1 GIVING CRD-COST.
013640       MOVE SAL-SELL       TO W10-SELL.
013650	     MOVE SAL-DISC	 TO W90-CDISC.
013660       MOVE SAL-DSC        TO WS-DISC.
011540	     SUBTRACT W90-CDISC FROM W25-VALUE
				 GIVING W90-VALUE.
	     MOVE SAL-SUPER	 TO WS-SUPER.
      *
      *    ****   I F	V A T	A P P L I C A B L E ? -
      * 	  CALCULATE THE VAT AND THE EXCLUSIVE AMOUNT.
      *
	   IF SAL-VAT-SUB NOT = ZERO
	       MOVE W05-VAT(SAL-VAT-SUB)
                                 TO W05-RATE
	       COMPUTE W90-VALUE ROUNDED = (W90-VALUE -
		     ((W90-VALUE / (W05-RATE + 100.00000)) *
		       W05-RATE))
	       COMPUTE W90-ATAX = W90-ATAX + ((W25-VALUE +
				  W90-CDISC) - W90-VALUE)
	       COMPUTE W90-EXCDIS ROUNDED = (W90-CDISC -
		     ((W90-CDISC / (W05-RATE + 100.00000)) *
		       W05-RATE))
      *    *************************************************************
      *    ****    A C C U M U L A T E	 T H E	 T O T A L
      * 	   E X C L U S I V E   V A L U E
      *    *************************************************************
	       ADD W90-VALUE	 TO W90-EXCVAL.
      *    *************************************************************
      *    ****    A C C U M U L A T E	 T O T A L   A M O U N T
      * 	   E X C L U D I N G   V A T
      *    *************************************************************
	     ADD W90-VALUE	 TO W90-EXCTOT.
	     COMPUTE CRD-SELL ROUNDED = (W90-VALUE / SAL-QNT) * -1.
	   IF WS-CARDEX = "Y"
	       PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
015400	       MOVE 1		 TO WS-ACTION
	       PERFORM AY43 THRU AY59.
013680	     MOVE W15-L11 TO CSH-DATA.
013690       WRITE CSH-L.
      *
      *    ****    U P D A T E   S A L E S   A N A L Y S I S
      *
026950	     MOVE STK-LDG	 TO DPT-CODE.
	     MOVE ZERO		 TO WS-ACTION.
026960	     PERFORM AY38 THRU AY59.
026970	     ADD W90-VALUE	 TO DPT-RET-DAY
				    DPT-RET-MTD
026980				    DPT-RET-YTD.
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
	       MOVE STK-LDG	 TO WX-LEDG
               MOVE "CASH REFUNDS"
                                 TO WX-NAR
	       MOVE W90-VALUE	 TO WX-VALUE
               MOVE DPT-SALE-GL  TO WX-AC
               MOVE 05           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   C O S T - O F - S A L E S
      *
013850       COMPUTE W90-BCR ROUNDED = (1.000 * W10-QUANT * W10-COST).
013920	     ADD W90-BCR	 TO DPT-CRET-DAY
				    DPT-CRET-MTD
				    DPT-CRET-YTD.
           IF WX-INT = "Y"
	       IF W90-BCR NOT = ZERO
		   COMPUTE WX-VALUE = W90-BCR * -1
                   MOVE DPT-COST-GL
                                 TO WX-AC
                   MOVE 09       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE
		   MOVE W90-BCR  TO WX-VALUE
                   MOVE DPT-PUR-GL
                                 TO WX-AC
                   MOVE 20       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   D I S C O U N T   G I V E N
      *
013930       SUBTRACT W90-EXCDIS FROM DPT-DISC-DAY
                                      DPT-DISC-MTD
                                      DPT-DISC-YTD.
             ADD W90-EXCDIS      TO DPT-RET-DAY
                                    DPT-RET-MTD
                                    DPT-RET-YTD.
013940       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
	   IF (WS-PRC-IND = "Y" AND STK-IND = 1) OR
040790	      (STK-IND = 0)
013780	       ADD W10-QUANT TO STK-QUANT.

013840 JA85.
030720     IF STK-YTD NOT < W10-QUANT
030730         SUBTRACT W10-QUANT FROM STK-YTD.
030740     IF STK-MTD NOT < W10-QUANT
030750         SUBTRACT W10-QUANT FROM STK-MTD.
030760     IF STK-YTDV NOT < W90-VALUE
030770         SUBTRACT W90-VALUE FROM STK-YTDV.
030780     IF STK-MTDC NOT < W90-BCR
030790         SUBTRACT W90-BCR FROM STK-MTDC.
030800     IF STK-YTDC NOT < W90-BCR
030810         SUBTRACT W90-BCR FROM STK-YTDC.
030820     IF STK-MTDV NOT < W90-VALUE
030830         SUBTRACT W90-VALUE FROM STK-MTDV.
030840       ADD W90-VALUE       TO W90-SALES.
	     ADD W90-BCR	 TO W90-BDT.
013950       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
013960       MOVE W70-AUDIT      TO WS-AUDKEY.
013970       MOVE LOW-VALUES     TO AUD-REC1.
013980       PERFORM AY16 THRU AY59.
013990       MOVE 6              TO AUD-TYPE.
014000       MOVE WS-DRAW        TO AUD-SUB.
014010       MOVE W90-REF        TO AUD-CSH.
014020       MOVE W10-SMAN       TO AUD-SMAN.
014030       MULTIPLY -1 BY W90-VALUE
                                 GIVING AUD-SEL.
014040       MULTIPLY -1 BY W90-BCR
                                 GIVING AUD-CST.
014050	     MOVE STK-CODE	 TO AUD-EXT-STOCK.
014060       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
014070       ADD 1               TO W70-AUDIT.
014080       MOVE 1              TO WS-AUDKEY.
014090       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
014100       PERFORM AY16 THRU AY59.
014110       MOVE 1              TO AUD-REP6.
014120       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
014130     IF W90-CDISC = ZERO
014140         GO TO JA90.
             MOVE SPACES         TO W15-DISCOUNT.
014150	     MOVE "DISCOUNT %       "
                                 TO W15-DISC.
014160       MOVE WS-DISC        TO W15-DSC.
014170       MULTIPLY -1 BY W90-CDISC 
                                 GIVING W15-PRICE.
014180       MOVE W15-L11        TO CSH-DATA.
014190       WRITE CSH-L.

014200 JA90.
014210       MOVE SPACES         TO CSH-DATA.
014220       ADD 1               TO WS-SALKEY.
014230       GO TO JA80.

014240 JA95.
013510	     MOVE ALL "-"	 TO CSH-LINE.
014250	     WRITE CSH-L.
014260	     MOVE "Sub TOTAL        :"
				 TO W15-REMK.
014270       MOVE W90-DB         TO W15-VAL.
014280       MOVE W15-L13        TO CSH-DATA.
014290       WRITE CSH-L.
014300     IF W90-TDISC NOT = ZERO
014310         IF W90-LDISC = ZERO
027690		   MOVE "YYYY"	 TO DPT-CODE
		   MOVE ZERO	 TO WS-ACTION
014340		   PERFORM AY38 THRU AY59
014350             SUBTRACT W90-TDISC 
                                 FROM DPT-DISC-DAY
                                      DPT-DISC-MTD
                                      DPT-DISC-YTD
                   IF WX-INT = "Y"
                       COMPUTE WX-VALUE = W90-EXCDIS * -1
		       MOVE DPT-DISC-GL
				 TO WX-AC
		       MOVE 16	 TO WX-TYPE
		       PERFORM GL-TRF-UPDATE
		   END-IF
014360		   PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
014370     IF W90-TDISC NOT = ZERO
014380	       MOVE "DISCOUNT         :"
014380				 TO W15-REMK
014390         COMPUTE W15-VAL = W90-TDISC * -1
014400	       MOVE W15-L13	 TO CSH-DATA
014410         WRITE CSH-L.
           IF W90-TAX NOT = SPACES
014440	       MOVE "TAX AMOUNT       :"
                                 TO W15-REMK.
	     MOVE 1		 TO WS-VAT-SUB.
      *
      *    ****    C A L C U L A T E   V A T   O N   T O T A L S
      * 	   F O R   E A C H   V A T   R A T E
      *
       JA95-VAT-LOOP.
	   IF NOT (W90-TAXVAL(WS-VAT-SUB) = ZERO)
014710	       MOVE W05-VAT(WS-VAT-SUB)
014720				 TO W05-RATE
	       COMPUTE W90-VAT(WS-VAT-SUB) ROUNDED =
		      (W90-TAXVAL(WS-VAT-SUB) /
		      (100.00000 + W05-RATE)) * W05-RATE
	       COMPUTE W90-TAXVAL(WS-VAT-SUB) =
		       W90-TAXVAL(WS-VAT-SUB) - W90-VAT(WS-VAT-SUB)
	       ADD W90-VAT(WS-VAT-SUB)
				 TO W90-TTAX.
	   IF WS-VAT-SUB < 6
	       ADD 1		 TO WS-VAT-SUB
	       GO TO JA95-VAT-LOOP.
	     MULTIPLY -1	 BY W90-TTAX.
014480	     MOVE 9		 TO WS-PARKEY.
014500       PERFORM AY01 THRU AY59.
	     MOVE 1		 TO W05-V-RATE.

       JA95-VAT.
	   IF W90-VAT(W05-V-RATE) = ZERO
	       GO TO JA95-CHECK-END.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
027840	     PERFORM AY38 THRU AY59.
027850	     SUBTRACT W90-VAT(W05-V-RATE)
				 FROM DPT-O-VRET-DAY
027860				      DPT-O-VRET-MTD
027870				      DPT-O-VRET-YTD
	     SUBTRACT W90-TAXVAL(W05-V-RATE)
				 FROM DPT-V-RET-DAY
				      DPT-V-RET-MTD
				      DPT-V-RET-YTD.
041840	     ADD W90-TAXVAL(W05-V-RATE)
				 TO PAR-CVAT-DAY
041850				    PAR-CVAT-MTD
041860				    PAR-CVAT-YTD.
      *
      *    ****    I N T E G R A T I O N   T O	 T H E	 G / L E D G E R
      *
	   IF (WX-INT = "Y") AND (W90-VAT(W05-V-RATE) NOT = ZERO)
		MOVE W90-VAT(W05-V-RATE)
				 TO WX-VALUE
		MOVE DPT-I-VAT-GL
				 TO WX-AC
		MOVE DPT-CODE	 TO WX-LEDG
		MOVE 11 	 TO WX-TYPE
                PERFORM GL-TRF-UPDATE.
027880	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.

       JA95-CHECK-END.
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO JA95-VAT.
             ADD W90-NONTAX      TO PAR-CNGS-DAY
                                    PAR-CNGS-MTD
                                    PAR-CNGS-YTD.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
027890       MOVE W05-RATE       TO WS-PER.
           IF W90-TAX = SPACES
               GO TO JA100.
014450	     MOVE W90-TTAX	 TO W15-VAL.
014460	     MOVE W15-L13	 TO CSH-DATA.
014470       WRITE CSH-L.

       JA100.
014530       ADD W25-TOTAL W90-DB GIVING W90-TOTAL.
014550       MOVE W90-BDT        TO W90-TCOST.
             MOVE 1              TO TAX-TYPE.
             MOVE 2              TO TAX-ACTYPE.
             MOVE 02             TO TAX-CODE.
             MOVE W12-T-YMD      TO TAX-DATE.
             MOVE W90-REF        TO TAX-REF.
	     MULTIPLY -1 BY W90-NONTAX
                                 GIVING TAX-TAXFREE.
             COMPUTE TAX-VALUE = (W90-TOTAL - W90-DISC) * -1.
             MOVE ZERO           TO TAX-SEQ
                                    TAX-AC.
	     MOVE 1		 TO WS-S9.

       JA102.
	     MULTIPLY -1 BY W90-TAXVAL(WS-S9)
				 GIVING TAX-VAL(WS-S9).
	     MULTIPLY -1 BY W90-VAT(WS-S9)
				 GIVING TAX-VAT(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO JA102.
000890	     PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
000900	     MOVE 1		 TO WS-ACTION.
000910	     PERFORM AY19 THRU AY59.
014560	     MOVE "AMOUNT PAYABLE   :"
				 TO W15-REMK.
	     SUBTRACT W90-TDISC FROM W90-TOTAL
				 GIVING W15-VAL.
014580	     MOVE W15-L13	 TO CSH-DATA.
014590       WRITE CSH-L.
014600     IF W90-CASH > ZERO
014610	       MOVE "CASH REFUND      :"
				 TO W15-REMK
014620	       MOVE W90-CASH	 TO W15-VAL
014630	       MOVE W15-L13	 TO CSH-DATA
014640         WRITE CSH-L.
014650     IF W90-CHQ > ZERO
014660	       MOVE "CHECK / CHEQUE   :"
				 TO W15-REMK
014670	       MOVE W90-CHQ	 TO W15-VAL
014680	       MOVE W15-L13	 TO CSH-DATA
014690         WRITE CSH-L.
014700     IF W90-CARD > ZERO
014710	       MOVE "CREDIT CARD      :"
				 TO W15-REMK
014720	       MOVE W90-CARD	 TO W15-VAL
014730	       MOVE W15-L13	 TO CSH-DATA
014740         WRITE CSH-L.
014700	   IF W90-SPEED > ZERO
014710	       MOVE "SPEED POINT      :"
				 TO W15-REMK
014720	       MOVE W90-SPEED	 TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L.
014700	   IF W90-VOUCH > ZERO
014710	       MOVE "GIFT VOUCHER     :"
				 TO W15-REMK
014720	       MOVE W90-VOUCH	 TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L.
014700	   IF W90-CRNOTE > ZERO
014710	       MOVE "CREDIT NOTE      :"
				 TO W15-REMK
014720	       MOVE W90-CRNOTE	 TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L.
014700	   IF W90-EFT > ZERO
014710	       MOVE "EFT/E OORBTLNG   :"
                                 TO W15-REMK
014720	       MOVE W90-EFT	 TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L.
014800	     PERFORM OPEN-CASH-DRAWER THRU OPEN-CASH-DRAWER-EXIT.
014810       MOVE SPACES TO CSH-DATA.
014820       WRITE CSH-L.
           IF W90-TAX = SPACES
               IF W05-VATNO NOT = SPACES
		   MOVE "        VAT Inclusive        "
				 TO CSH-DATA
                   WRITE CSH-L
                   MOVE SPACES   TO CSH-DATA.
	   IF W05-VATNO NOT = SPACES
               MOVE ALL "="      TO CSH-LINE
               WRITE CSH-L
               MOVE SPACES       TO CSH-DATA
	       MOVE " TAX CREDIT NOTE"
                                 TO W15-DOCTYP
013360         MOVE W15-L1A      TO CSH-DATA
               WRITE CSH-L
               MOVE SPACES       TO CSH-DATA
	       MOVE "TAXABLE AMOUNT    :"
				 TO W15-REMK
	       MOVE W90-EXCVAL	 TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L
	       MOVE "V A T             :"
				 TO W15-REMK
               MOVE W90-TTAX     TO W15-VAL
014730         MOVE W15-L13      TO CSH-DATA
014740         WRITE CSH-L
               MOVE ALL "="      TO CSH-LINE
               WRITE CSH-L
               MOVE SPACES       TO CSH-DATA
014790         WRITE CSH-L.
014830	     MOVE W15-L9	 TO CSH-DATA.
014840       WRITE CSH-L.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	    Advertisement for Devloper or Service Agent        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
014850	     MOVE "    APAC accounting - 082-566-3095"
				 TO CSH-DATA.
014860	     MOVE 1		 TO WS-S9.
014870 JA105.     
014880       WRITE CSH-L.
014850	     MOVE SPACES	 TO CSH-DATA.
014890     IF WS-S9 < W85-ADVANCE
014900	       ADD 1		 TO WS-S9
014910         GO TO JA105.
	     MOVE W85-CSI	 TO CSH-DATA.
014880       WRITE CSH-L.
014850       MOVE SPACES         TO CSH-DATA.
	     CLOSE CSHSLE.
	     OPEN OUTPUT CSHSLE.
	     CALL X"91" USING X91-RES X91-FUN CSHSLE.
014920	     MOVE W70-AUDIT	 TO WS-AUDKEY.
014930	     MOVE LOW-VALUES	 TO AUD-REC1.
014940       PERFORM AY16 THRU AY59.
014950	     MOVE 5		 TO AUD-TYPE.
014960	     MOVE WS-DRAW	 TO AUD-SUB.
014970	     MOVE W90-REF	 TO AUD-CSH.
	     COMPUTE AUD-SELL = (W90-TSELL + W90-TTAX) * -1.
014990	     MULTIPLY -1 BY W90-TCOST
				 GIVING AUD-COST.
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
             COMPUTE AUD-VAL = (W90-TOTAL - W90-TDISC) * -1.
029880	   IF NOT (W90-CASH = ZERO)
	       MOVE "C" 	 TO AUD-PAYMENT
	   ELSE
029890	   IF NOT (W90-CHQ = ZERO)
	       MOVE "Q" 	 TO AUD-PAYMENT
	   ELSE
029900	   IF NOT (W90-CARD = ZERO)
	       MOVE "D" 	 TO AUD-PAYMENT
	   ELSE
015410	   IF NOT (W90-SPEED = ZERO)
	       MOVE "S" 	 TO AUD-PAYMENT
	   ELSE
015410	   IF NOT (W90-VOUCH = ZERO)
	       MOVE "V" 	 TO AUD-PAYMENT
	   ELSE
015410	   IF NOT (W90-CRNOTE = ZERO)
	       MOVE "N" 	 TO AUD-PAYMENT.
015160	     PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015170	     ADD 1		 TO W70-AUDIT.
015180	     MOVE 1		 TO WS-AUDKEY.
015190       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
015200       PERFORM AY16 THRU AY59.
015210	     MOVE 1		 TO AUD-REP4.
015220       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
015230     IF W10-SMAN NOT = ZERO
015240	       ADD 100 W10-SMAN  GIVING WS-PARKEY
015260         PERFORM AY01 THRU AY59
015270	       SUBTRACT W90-TCOST
				 FROM PAR-CT-MTD PAR-CT-YTD
015280	       SUBTRACT W90-TSELL
				 FROM PAR-SL-MTD PAR-SL-YTD
015290         PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015330	     ADD W90-BAL	 TO STK-TOTAL.
015340	     SUBTRACT W90-SALES  FROM STK-SMTD STK-SYTD.
015360	     ADD 150 WS-DRAW	 GIVING WS-PARKEY.
015380       PERFORM AY01 THRU AY59.
015390	     SUBTRACT W90-CASH	 FROM PAR-CASH.
015400	     SUBTRACT W90-CHQ	 FROM PAR-CHEQUES.
015410	     SUBTRACT W90-CARD	 FROM PAR-CARDS.
015410	     SUBTRACT W90-SPEED	 FROM PAR-SPEED.
015410	     SUBTRACT W90-VOUCH	 FROM PAR-VOUCHERS.
015410	     SUBTRACT W90-CRNOTE FROM PAR-BANKTRF.
015410	     SUBTRACT W90-EFT	 FROM PAR-BANKTRF.
015430	     SUBTRACT W90-EXCVAL FROM PAR-TAXVAL.
	   IF W90-TTAX < ZERO
	       ADD W90-TTAX	 TO PAR-VATVAL
	   ELSE
	       SUBTRACT W90-TTAX FROM PAR-VATVAL.
015440	     COMPUTE PAR-CSH = PAR-CSH - (W90-EXCVAL + W90-NONTAX).
015450       COMPUTE PAR-CSHMTD = PAR-CSHMTD - 
				 (W90-EXCVAL + W90-NONTAX).
015460       COMPUTE PAR-CSHYTD = PAR-CSHYTD -
				 (W90-EXCVAL + W90-NONTAX).
015470       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015370	     MOVE PAR-RECORD151  TO W85-RECORD151.
015480       MOVE 7              TO WS-PARKEY.
015500       PERFORM AY01 THRU AY59.
             COMPUTE PAR-CASH-RDAY  = PAR-CASH-RDAY +
                                      W90-CASH + W90-CHQ + W90-CARD.
             COMPUTE PAR-CASH-RMTH  = PAR-CASH-RMTH +
                                      W90-CASH + W90-CHQ + W90-CARD.
             COMPUTE PAR-CASH-RYEAR = PAR-CASH-RYEAR +
                                      W90-CASH + W90-CHQ + W90-CARD.
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
	       MOVE "04PS"	 TO WX-LEDG
               MOVE "CASH REFUND SLIPS"
                                 TO WX-NAR
	       COMPUTE WX-VALUE = (W90-TOTAL - W90-TDISC) * -1
               MOVE PAR-CASH-GL  TO WX-AC
               MOVE 29           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015480       MOVE 8              TO WS-PARKEY.
015500       PERFORM AY01 THRU AY59.
015510       SUBTRACT W90-BDT    FROM PAR-CSHT PAR-CMTD PAR-CYTD.
015520       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015530       PERFORM AY70 THRU AY999.
015540       CLOSE PARAM AUDIT STOCK SALE NETWORK DEPART.
028820       OPEN OUTPUT SALE.
028830       CLOSE SALE.
015550       OPEN I-O PARAM AUDIT STOCK SALE NETWORK DEPART.
029700	     MOVE "ENTER to continue"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
015680	     GO TO JA150.
015690 JA110.
015700       MOVE ZERO TO WS-ERROR.
	   IF (WS-PRC = "1") AND (STK-SELL NOT > W10-SPAPRC)
015720         GO TO JA120
           ELSE
	   IF (WS-PRC = "2") AND (STK-CASH NOT > W10-SPAPRC)
015720         GO TO JA120
           ELSE
015710     IF STK-WSALE NOT > W10-SPAPRC
015720         GO TO JA120.
015730       COMPUTE W10-MARG = ((W10-SPAPRC - W10-COST) / W10-SPAPRC)
015740                          * 100.00.
015750     IF W10-MARG NOT < W85-MARG
015760         GO TO JA120.
015790 JA115.
018110	     MOVE "Margin less than allowed - Override Y/N?"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
015860	   IF NOT (WS-OPTION = "N" OR "Y")
015870         GO TO JA115.
015880     IF WS-OPTION = "N"
015890         MOVE 1 TO WS-ERROR
015900         GO TO JA120.
       JA116.
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
             MOVE 1              TO WS-SUPER.
       JA117.
018370     IF W40-CSHPASS = W85-SUPER (WS-SUPER) 
               GO TO JA118.
           IF WS-SUPER < 9
               ADD 1             TO WS-SUPER
               GO TO JA117
           ELSE
           IF W40-CSHPASS = "!@#$%^"
               MOVE 10           TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
016050	       MOVE 1		 TO WS-ERROR.
       JA118.
	     DISPLAY CLEAR-L25..

016060 JA120.
016070       EXIT.

       JA150.
	     EXIT.
      *
      *    ****    A M E N D   S A L E S   D E T A I L S
      *
       JA200A	       SECTION 50.
016080 JA200.
016090	     MOVE 7		 TO LIN.
016100       PERFORM JA48 UNTIL LIN > 18.
016110	     MOVE 7		 TO LIN.
016120	     MOVE 1		 TO WS-LINES WS-FIRST.

016130 JA202.
016140	     MOVE WS-FIRST	 TO WS-SALKEY WS-SALPOS.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
016150 JA205.
016160     IF WS-SALKEY > WS-LAST
016170         GO TO JA210.
016180       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S12-EXT AT LINE LIN
	   ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S12-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S12 AT LINE LIN.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
016210	     DISPLAY ( , 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
016220	     MOVE SAL-QNT	 TO W10-QUANT.
	   IF W85-USE-3 = "Y"
016230	       DISPLAY S26-WEIGHT AT LINE LIN
	   ELSE
016230	       DISPLAY S26-QNT AT LINE LIN.
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
016300         GO TO JA205.

       JA208.
	     MOVE 6		 TO LIN.
	     DISPLAY CLEAR-L25.

016310 JA210.
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
       JA214.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.
      *
      *    ****    G E T   U S E R   O P T I O N
      *
016580 JA215.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO JA216
		 WHEN DOWN-KEY
001340		     GO TO JA217
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO JA215
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO JA265
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
		 WHEN PAGE-DOWN
001340		     GO TO JA222
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
		 WHEN PAGE-UP
001340		     GO TO JA220
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO JA215
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION = "A" OR "D") AND
		  (LIN > 6)
		   DISPLAY CLEAR-L25
		   GO TO JA224.
	     CALL X"E5".
001280	     GO TO JA215.
      *
      *    ****    U P	 K E Y	 ( M O V E   H I G H L I G H T	 U P )
      *
       JA216.
	   IF (WS-SALPOS = 1) OR (LIN < 7)
	       GO TO JA218.
	     SUBTRACT 1 	 FROM WS-SALPOS.
	   IF LIN > 7
	       SUBTRACT 1	 FROM LIN
	       GO TO JA218.
	     MOVE 18		 TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      * 	    O N E   L I N E
      *
       JA216-LOOP.
	     SUBTRACT 1 FROM WS-S7
				 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S7 > 8
	       SUBTRACT 1	 FROM WS-S7
	       GO TO JA216-LOOP.
	     MOVE WS-SALPOS	 TO WS-SALKEY WS-FIRST.
      *
      *    ****    C L E A R   F I R S T   O R	 L A S T
      * 	   D E T A I L	 L I N E.
      *
       JA216-DISPLAY.
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
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
030120	       GO TO JA216-END-DISP.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S12-EXT AT LINE LIN
	   ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S12-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S12 AT LINE LIN.
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

       JA216-END-DISP.
	     PERFORM SAVE-SCREEN-2.
	     GO TO JA218.
      *
      *    ****    D O W N   K E Y   ( M O V E	 H I G H L I G H T
      * 			       D O W N )
      *
       JA217.
	   IF WS-SALPOS = WS-LAST
	       GO TO JA218.
      *
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *
	   IF LIN > 6
	       ADD 1		 TO WS-SALPOS.
	   IF LIN < 18
	       ADD 1		 TO LIN
	       GO TO JA218.
	     MOVE 7		 TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      * 	    L I N E
      *
       JA217-LOOP.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO JA217-LOOP.
	     ADD 1		 TO WS-FIRST.
	     MOVE WS-SALPOS	 TO WS-SALKEY.
	     GO TO JA216-DISPLAY.
      *
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *
001980 JA218.
001990	     PERFORM JA214.
	   IF LIN > 6
002000	       DISPLAY (LIN, 1) W43-DET (LIN)
			WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
002010	     GO TO JA215.
      *
      *    ****    C L E A R   S C R E E N
      *
030780 JA219.
030790	     MOVE 7		 TO LIN.
030800	     PERFORM JA48 UNTIL LIN > 18.
030810	     MOVE 7		 TO LIN.
030820       MOVE 1              TO WS-LINES.
      *
      *    ****   P R E V I O U S   S C R E E N	  R E Q U I R E D
      *
       JA220.
	   IF WS-FIRST = 1
	       GO TO JA215.
	   IF WS-FIRST > 12
	       SUBTRACT 12	 FROM WS-FIRST
	   ELSE
	       MOVE 1		 TO WS-FIRST.
	     PERFORM JA219.
	     GO TO JA202.
      *
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *
       JA222.
	   IF (WS-FIRST + 12) > WS-LAST
	       GO TO JA215.
	     ADD 12		 TO WS-FIRST.
	     PERFORM JA219.
	     GO TO JA202.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
       JA224.
030630	   IF WS-OPTION = "A"
016910	       GO TO JA228.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *
       JA225.
043780	     MOVE "Are you sure Y/N?"
				 TO WS-ERR-MES.
016940	     MOVE "N"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
017030	   IF NOT (WS-OPTION = "N" OR "Y")
017040	       GO TO JA225.
031000	   IF WS-OPTION = "N"
031010	       GO TO JA210.
031020       MOVE "D"            TO WS-OPTION.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
       JA228.
031220	     MOVE WS-SALPOS	 TO WS-SALKEY.
031230       PERFORM READ-SALE THRU READ-SALE-EXIT.
017290	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM STK-CODE.
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
	   IF SAL-VAT-SUB > ZERO
017410	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(SAL-VAT-SUB).
017420     IF (STK-IND < 2) OR (STK-IND = 3)
017430         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
	     PERFORM JA214.
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
017440	   IF WS-OPTION = "A"
017450         GO TO JA250.
      *
      *    ****    R E - D I S P L A Y   T O T A L S
      *
017460	     PERFORM JA45B THRU JA45-AA.
017470 JA245.
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
017480	     MOVE WS-SALKEY	 TO WS-SALSTR.
017490	     ADD 1		 TO WS-SALKEY.
017500     IF WS-SALKEY > WS-LAST
017510         SUBTRACT 1 FROM WS-LAST
017520         GO TO JA200.
017530       PERFORM READ-SALE THRU READ-SALE-EXIT.
017540	     MOVE WS-SALSTR	 TO WS-SALKEY.
030650       SUBTRACT 1          FROM SAL-CODE.
017550       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
017560	     ADD 1		 TO WS-SALKEY.
017570       GO TO JA245.

017580 JA250.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
	   IF W85-USE-3 = "Y"
017590	       ACCEPT S26-WEIGHT AT LINE LIN
	   ELSE
	       ACCEPT S26-QNT AT LINE LIN.
017600	     DISPLAY CLEAR-L25.
017610     IF W10-QUANT = ZERO
044520	       MOVE "Quantity MAY NOT be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
031670	       MOVE SAL-QNT	 TO W10-QUANT
017640	       GO TO JA250.
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
017810         GO TO JA255.
017820       COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
017830	     MOVE W90-CDISC	 TO W15-PRICE SAL-DISC.
      *
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *
017840 JA255.
017850	     PERFORM JA45 THRU JA45-AA.

017880 JA260.
017860       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
017870       GO TO JA200.
      *
      *    ****    C L E A R   D E T A I L   L I N E S
      *
031940 JA265.
	     PERFORM JA219.
      *
      *    ****    D I S P L A Y   L A S T   P A G E
      *
	   IF WS-LAST > 10
	       SUBTRACT 9 FROM WS-LAST
				 GIVING WS-SALKEY
	   ELSE
	       MOVE 1		 TO WS-SALKEY.

030050 JA270.
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
030120	       GO TO JA275.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S12-EXT AT LINE LIN
	   ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S12-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S12 AT LINE LIN.
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
       JA275.
	     ADD 1		 TO LIN.
030060	   IF WS-SALKEY < WS-LAST
	       ADD 1		 TO WS-SALKEY
030070	       GO TO JA270.

017880 JA299.
017890	     EXIT.
      /    *************************************************************
      *    ****		I N I T I A L I S E   P R O G R A M	    ****
      *    *************************************************************
020720 ZA000-INIT	       SECTION 91.
020730 ZA000-OPEN.
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorд"  TO WS-WRKHD
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
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   S e t   u p	 t h e	 F U N C T I O N   k e y s     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		       ESC, F1 to F10 keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		    PAGE-UP and PAGE-DOWN keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   A C T I V A T E   M O U S E		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
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
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Set MOUSE key to act as FUNCTION key	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	    Setup the file ID's including path names           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
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
      *    *************************************************************
      *    **** 	D A I L Y   -	A U D I T   F I L E	    ****
      *    *************************************************************
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
      *    *************************************************************
      *    ****       S A L E S	  -   H I S T O R Y   F I L E	    ****
      *    *************************************************************
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
      *    *************************************************************
      *    ****      C H E Q U E S   R E C E I V E D   F I L E	    ****
      *    *************************************************************
	     MOVE "CHEQUE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CHEQUE.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	    R E C O V E R Y   F I L E		    ****
      *    *************************************************************
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
      *    *************************************************************
      *    **** 		S A L E S   F I L E		    ****
      *    *************************************************************
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *    *************************************************************
      *    ***	S T O C K   ( I V E N T O R Y )   M E M O   F I L E  ***
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
      *    *************************************************************
      *    ****      S T O C K	 ( I V E N T O R Y )   F I L E	    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    ****       V A L U E	  A D D E D   T A X   F I L E	    ****
      *    *************************************************************
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
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-CASES	 TO WS-USE-CASES.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
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
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	     MOVE PAR-PRICED-IND TO WS-PRC-IND.
021020	     OPEN OUTPUT RECOVER.
021030       CLOSE RECOVER.
021040       OPEN I-O RECOVER.
040400       OPEN I-O CHEQUE.
040410     IF RUNTIME-ERROR
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
021310         MOVE 9 TO WS-ERROR
021320         PERFORM OPEN-ERROR.
021330	     CLOSE SALE.
021340       OPEN I-O SALE.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT (PAR-RT-HD = X"00000000000000000000")
	       MOVE PAR-RT-HD	 TO WS-RT-HD.
	     INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-RT-HD REPLACING LEADING "-" BY " ".
	   IF WS-CS-HD = SPACES
	       MOVE "Cash"	 TO WS-CS-HD.
	   IF WS-WS-HD = SPACES
	       MOVE "Wholesale"	 TO WS-WS-HD.
	   IF WS-RT-HD = SPACES
	       MOVE "Retail"	 TO WS-RT-HD.
	     MOVE WS-CS-HD	 TO W15-PNAME.

       ZA05-NAME-LOOP.
	   IF W15-CHECK = SPACE
	       MOVE W15-REST	 TO W15-PNAME
	       GO TO ZA05-NAME-LOOP.

       ZA05-EXIT.
	     MOVE W15-PNAME	 TO WS-CS-HD.
	     MOVE WS-WS-HD	 TO W15-PNAME.
	     PERFORM ZA05-NAME-LOOP.
	     MOVE W15-PNAME	 TO WS-WS-HD.
	     MOVE WS-RT-HD	 TO W15-PNAME.
	     PERFORM ZA05-NAME-LOOP.
	     MOVE W15-PNAME	 TO WS-RT-HD.
021400	     MOVE 1		 TO WS-PARKEY.
021410       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021590     IF PAR-CSHEAD = SPACES
021600	       MOVE "Setup cash slip headings"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
021620	       GO TO ZA205.
021430	     MOVE PAR-DMY	 TO W12-TODAY.
021440	     MOVE PAR-YMD	 TO W12-T-YMD.
             MOVE PAR-CSHEAD     TO W15-COMP W40-NAME.
021660       MOVE PAR-CSHADD     TO W15-ADD.
021670       MOVE PAR-TELEPHONE  TO W15-TEL.
             MOVE 6 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-WHOLESALE TO WS-WS.
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
021520	     MOVE PAR-RECORD151  TO W85-RECORD151.
021540	     MOVE 5		 TO WS-PARKEY.
021550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021560	     MOVE PAR-PW	 TO W85-PASS.
021680	     MOVE 32		 TO WS-S1.
020780       DISPLAY "Waiting for printer response" AT 1431
020790                WITH FOREGROUND-COLOR 14 BLINK.
021690 ZA05.
021700     IF W40-NCH (WS-S1) = SPACE
021710	       SUBTRACT 1	 FROM WS-S1
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
               MOVE "CSH001.PRN" TO W02-PRINTER.
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
               MOVE ALL "A"  TO CSH-OPEN.
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
051810         MOVE W02-AUDITF TO WS-FILE
051820         MOVE WS-AUDKEY TO WS-KEY
051830     ELSE
051840     IF WS-F-ERROR = 2
051850         MOVE W02-NETWORK TO WS-FILE
051860         MOVE WS-NETKEY TO WS-KEY
051950     ELSE
051960     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
051990*     ELSE
052000*     IF WS-F-ERROR = 9
052010*         MOVE W02-GARTEE TO WS-FILE
      *         MOVE ZERO         TO WS-KEY
      *         MOVE GUA-KEY      TO WS-KEYX
052070     ELSE
052080     IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100         MOVE WS-PARKEY    TO WS-KEY
052110     ELSE
052120     IF WS-F-ERROR = 18
052130         MOVE W02-RECOVER  TO WS-FILE
052140         MOVE WS-RECKEY    TO WS-KEY
052150     ELSE
052160     IF WS-F-ERROR = 19
052170         MOVE W02-SALE TO WS-FILE
052180         MOVE WS-SALKEY TO WS-KEY
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
041840     ELSE
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
052320         MOVE "End of FILE" TO WS-STAT-MESSAGE
052330     ELSE
052340     IF WS-STATUS = "22"
052350         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
052360     ELSE
052370     IF WS-STATUS = "23"
052380         MOVE "Invalid record number" TO WS-STAT-MESSAGE
052390     ELSE
052400     IF WS-STATUS = "24"
052410         MOVE "DISK full" TO WS-STAT-MESSAGE
052420     ELSE
052430     IF WS-STATUS = "30"
052440         MOVE "DISK error" TO WS-STAT-MESSAGE
052450     ELSE
052460     IF WS-STATUS = "94"
052470         MOVE "FILE locked" TO WS-STAT-MESSAGE.
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
