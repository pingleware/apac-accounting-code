      $set LINKCOUNT"712"
      ******************************************************************
      *                                                                *
      *    ******   **         ******      ****      ****       **     *
      *   **    **  **        **    **    **  **    **  **     ***     *
      *   **        **        **         **    **  **    **     **     *
      *    ******   **         ******    **    **  **    **     **     *
      *         **  **              **   **    **  **    **     **     *
      *   **    **  **        **    **    **  **    **  **      **     *
      *    ******   ********   ******      ****      ****     ******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      S A L E S   O R D E R S   P R O G R A M                   *
      *                                                                *
      *     VERSION 8.15.04 - January 2012			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Feb 2007	- New file (DEBDEP/AR_REST.DAT) - Debtor sales *
      * 		  restrictions file has been included which    *
      * 		  will allow for departments to be excluded or *
      * 		  only selected departments allowed. This will *
      * 		  allow the user the abillity to allow or bar  *
      * 		  sales of selected departments.	       *
      * 							       *
      * August 2008    - New file (STKALT) - Stock alternate index     *
      * 		 included for lookups, using any word con-     *
      * 		 tained in the Stock description.	       *
      * 							       *
      * November 2009	- Include words from Description 2 and from    *
      * 		  the Item code (Some item codes are comprised *
      * 		  of individual words and these will be        *
      * 		  included in the alternate Index)	       *
      * 							       *
      *  Jan 2010 - Allow for two discounts for a Debtor on the same   *
      * 	    Sales Ledger code. Included two additional Debtor  *
      * 	    discount codes (11 and 12) each with a field to    *
      * 	    define which debtor discount code uses this as an  *
      * 	    alternate discount. The relevant Stock Item will   *
      * 	    have an indicator field to instruct the system to  *
      * 	    use the alternate discount. The alternate discount *
      * 	    is only applicable if the discount code for which  *
      * 	    this alternate discount is applicable is the code  *
      * 	    on the Debtor account record.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SLS001.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2012
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
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000130 COPY CONTROL.SL.

000140 COPY DEBALT.SL.

000160 COPY DEBDEP.SL.

000170 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000190 COPY DEPART.SL.

000210 COPY PARAM.SL.

000230 COPY RECOVER.SL.

000250 COPY SALE.SL.

       COPY SHARED.SL.

000270 COPY SORDER.SL.

000290 COPY SPARTS.SL.

000140 COPY STKMEM.SL.

000310 COPY STOCK.SL.

000200 COPY SYSUSE.SL.

000370	   SELECT PRNDSK  ASSIGN W02-PRNDISK
000380                    ORGANIZATION LINE SEQUENTIAL.

000370     SELECT PRNREP  ASSIGN W02-PRINTER
000380                    ORGANIZATION LINE SEQUENTIAL.

      /
000330 DATA DIVISION.
000340 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000310 COPY DEBALT.FDE.

000330 COPY DEBDEP.FDE.

000400 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000420 COPY DEPART.FDE.

000440 COPY PARAM.FDE.

000460 COPY RECOVER.GFD.

000480 COPY SALE.IFD.

       COPY SHARED.FDE.

000500 COPY SORDER.FDE.

000520 COPY SPARTS.FDE.

000140 COPY STKMEM.FDE.

000540 COPY STOCK.FDE.

       COPY SYSUSE.FDE.

000660 FD  PRNDSK    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
000680 01  REP-L1.
	   03  REP-PG.
	       05  REP-LN      PIC  9(04).
000710	   03  REP-DETAIL2     PIC  X(96).

000700 01  REP-L2.
	   03  FILLER	       PIC  X(04).
000690	   03  REP-DETAIL1     PIC  X(80).

000720 01  REP-L3.
	   03  FILLER	       PIC  X(04).
000730	   03  REP-AC	       PIC  X(10).
000740	   03  REP-ACNO        PIC  X(06).
	   03  FILLER	       PIC  X(01).
000750	   03  REP-NAME        PIC  X(40).
	   03  FILLER	       PIC  X(06).
000760	   03  REP-DATE        PIC  X(06).
           03  REP-TIME.
000770	       05  REP-DTE     PIC  Z9/99/9999.

       01  REP-L3A.
	   03  FILLER	       PIC  X(04).
	   03  FILLER	       PIC  X(10).
	   03  REP-TEL	       PIC  X(14).
	   03  REP-CNTCT       PIC  X(10).
	   03  REP-CONTACT     PIC  X(30).
	   03  FILLER	       PIC  X(32).

000790 01  REP-L4.
	   03  FILLER	       PIC  X(04).
000800	   03  REP-H1	       PIC  X(29).
000810	   03  REP-REF	       PIC  9(08)/99.
000860	   03  FILLER	       PIC  X(40).

000870 01  REP-L5.
	   03  FILLER	       PIC  X(04).
000880	   03  REP-H2	       PIC  X(20).
000890	   03  REP-H3	       PIC  X(32).
000900	   03  REP-H4	       PIC  X(12).
000930	   03  REP-H5	       PIC  X(16).

000940 01  REP-L6.
	   03  FILLER	       PIC  X(04).
	   03  REP-EXT-ITEM.
000950	       05  REP-ITM1    PIC  X(14).
	       05  REP-SL      PIC  X(01).
	       05  REP-ITM     PIC  X(03).
	   03  FILLER	       PIC  X(02).
000960	   03  REP-DESC        PIC  X(30).
	   03  FILLER	       PIC  X(02).
	   03  REP-QTY	       PIC  Z(05)9.999 BLANK WHEN ZERO.
	   03  FILLER	       PIC  X(02).
	   03  REP-PRICE       PIC  Z(06)9.999.
	   03  FILLER	       PIC  X(05).

       01  REP-L7.
	   03  FILLER	       PIC  X(24).
	   03  REP-EXT-ITEM2.
000980	       05  REP-ITM2    PIC  X(14).
	       05  REP-SL2     PIC  X(01).
	       05  REP-ITM3    PIC  X(03).
000990	   03  FILLER	       PIC  X(01).
	   03  REP-DSCR	       PIC  X(30).
000990	   03  FILLER	       PIC  X(01).
001410	   03  REP-QNT	       PIC  Z(05)9.999.
	   03  FILLER	       PIC  X(12).

000660 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
000680 01  PRN-L1.
000690     03  PRN-DETAIL2     PIC X(96).
000700 01  PRN-L2.
000710     03  PRN-DETAIL1     PIC X(80).

      /
000560 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000570 77  WS-DISC	   PIC	99V99	 COMP-3.
000580 77  WS-TDISC        PIC  99V99    COMP-3.
000590 77  WS-CASH-AC      PIC  X(06).
000600 77  WS-SUB          PIC  9(04)    COMP-5.
002170 77  WS-SUPER        PIC  9(02)    COMP-5.
002180 77  WS-P1	   PIC	9(04)	 COMP-5.
000610 77  WS-S1           PIC  9(04)    COMP-5.
000620 77  WS-S2           PIC  9(04)    COMP-5.
000630 77  WS-S3           PIC  9(04)    COMP-5.
000640 77  WS-S4           PIC  9(04)    COMP-5.
000650 77  WS-S5           PIC  9(04)    COMP-5.
000660 77  WS-S6           PIC  9(04)    COMP-5.
000670 77  WS-S7           PIC  9(04)    COMP-5.
000680 77  WS-S8           PIC  9(04)    COMP-5.
000690 77  WS-S9           PIC  9(04)    COMP-5.
000700 77  WS-IXS          PIC  9(04)    COMP-5.
000710 77  WS-IXS1         PIC  9(04)    COMP-5.
000720 77  WS-IXS2         PIC  9(04)    COMP-5.
000730 77  WS-IXS3         PIC  9(04)    COMP-5.
000740 77  WS-IXS4         PIC  9(04)    COMP-5.
000750 77  WS-PARKEY       PIC  9(04)    COMP-5.
000780 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
000790 77  WS-DBTKEY       PIC  9(06)    COMP-5.
000800 77  WS-SALSTR       PIC  9(04)    COMP-5.
000810 77  WS-SALKEY       PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
000820 77  WS-NETKEY       PIC  9(04)    COMP-5.
000830 77  WS-AUDKEY       PIC  9(06)    COMP-5.
000840 77  WS-RECKEY       PIC  9(06)    COMP-5.
000850 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000870 77  WS-PRN1	   PIC	9(01)	 VALUE 0.
000880 77  WS-PRN2	   PIC	9(01)	 VALUE 0.
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-PIC-PRC	   PIC	X(01).
000900 77  WS-START	   PIC	9(08).
000910 77  WS-END          PIC  9(08).
000930 77  WS-TME2         PIC  9(08).
000940 77  WS-PORT	   PIC	9(05)	 VALUE 1004.
000950 77  WS-OPEN	   PIC	X(01)	 VALUE X"01".
000960 77  WS-CLOSE	   PIC	X(01)	 VALUE X"00".
000970 77  WS-PAR	   PIC	9(01)	 VALUE 0.
000980 77  WS-INO	   PIC	9(03)	 VALUE 0.
000990 77  WS-LINES	   PIC	9(02)	 VALUE 0.
001000 77  WS-FIRST	   PIC	9(03)	 VALUE 0.
       77  WS-SALPOS	   PIC	9(03)	 VALUE 0.
001010 77  WS-LAST	   PIC	9(03)	 VALUE 0.
001020 77  WS-LASTINV      PIC  9(04)    COMP-5.
001030 77  WS-LINE         PIC  9(04)    COMP-5.
001040 77  WS-PAGE         PIC  9(02)    COMP-5.
001050 77  WS-L1           PIC  9(04)    COMP-5.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
001060 77  WS-OPTION	   PIC	X(01).
001070 77  WS-PGADV	   PIC	X(01)	 VALUE "Y".
001070 77  WS-LOOP	   PIC	X(01)	 VALUE " ".
001070 77  WS-SKIP	   PIC	X(01)	 VALUE " ".
001080 77  WS-LOOK	   PIC	X(01)	 VALUE "S".
002620 77  WS-ETYPE	   PIC	X(01)	 VALUE "I".
       77  WS-IN-OPT	   PIC	X(01)	 VALUE "N".
001110 77  WS-NUM          PIC  Z9.
001120 77  WS-ERROR        PIC  9(01).
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC	X(01)	 COMP-X.
001160 77  WS-COMP         PIC  9(01).
001170 77  WS-UPDATE	   PIC	9(01)	 VALUE ZERO.
001180 77  WS-CONREC	   PIC	9(01)	 VALUE ZERO.
002730 77  WS-DELIV	   PIC	X(01).
       77  WS-ITMPRC	   PIC	X(01).
       77  WS-RT-HD	   PIC	X(10)	 VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10)	 VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10)	 VALUE "-Cash sale".
000750 77  WS-TYP	   PIC	X(01).
001200 77  WS-TYPE	   PIC	X(01).
001210 77  WS-AGE	   PIC	X(01).
001220 77  WS-PRC	   PIC	X(01).
       77  WS-RESTRICT	   PIC	X(01).
       77  WS-RESTCODE	   PIC	X(01).
       77  WS-MEASURE	   PIC	X(01)	 VALUE "N".
       77  PRG-NAME	   PIC	X(12)	 VALUE "STP\STPLOOK".
001280 77  WS-A 	   PIC	X(19)	 VALUE "Any key to continue".
001290 77  WS-E 	   PIC	X(15)	 VALUE "**END** - ENTER".
001300 77  WS-ERR1	   PIC	X(22)	 VALUE "Invalid Account Number".
001310 77  WS-ERR2	   PIC	X(09)	 VALUE "No Record".
001320 77  WS-PER          PIC  Z(02)9.99.
001330 77  WS-CR	   PIC	X(01)	 VALUE "I".
001340 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08)	 COMP-5.
       77  WS-USUB	   PIC	9(04)	 COMP-5.
      /
       01  PRG-NAME3.
	   03  FILLER	   PIC	X(08)	 VALUE "INV\INVF".
	   03  PRG-NUMBER  PIC	9(02)	 VALUE ZERO.

001350 01  WS-DB-LINE.
003060	   03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍµ".
003080     03  WS-TOP-LNE2.
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
	   03  WS-MINV REDEFINES WS-MID-LINE.
	       05  WS-INV1     PIC  X(01).
	       05  WS-INV78    PIC  X(78).
	       05  WS-INV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍµ".
003200	   03  WS-BOT-LNE2.
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
	   03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".
003220	   03  WS-TOP-LNE.
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
001690	   03  WS-TP-LINE.
001700         05  WS-TCA  PIC X(01) OCCURS 80.
001710     03  WS-MD-LINE.
001720         05  WS-MCA  PIC X(01) OCCURS 80.
	   03  WS-SO78-L REDEFINES WS-MD-LINE.
	       05  WS-MSO1     PIC  X(01).
	       05  WS-MSO78    PIC  X(78).
	       05  WS-MSO80    PIC  X(01).
001730	   03  WS-BT-LINE.
001740         05  WS-BCA  PIC X(01) OCCURS 80.
001750 01  WS-ENTRY.
001760     03  WS-ENT0     PIC  X(08).
001770     03  WS-TYPE0    PIC  X(01).
001780     03  WS-KEY0     PIC  9(06)    COMP-5.
001790 01  WS-ENTRY1.
001800     03  WS-ENT1     PIC  X(08).
001810     03  WS-TYPE1    PIC  X(01).
001820     03  WS-KEY1     PIC  9(06)    COMP-5.

001840 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "SLS".
	   03  WS-PROG	       PIC  X(03) VALUE "001".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  WS-LOCAL-ID.
	   03  WS-LOCAL-DRIVE  PIC  X(01).
	   03  FILLER	       PIC  X(14) VALUE ":\APACSHR\SORD".
	   03  WS-USER-ID      PIC  X(03).
	   03  FILLER	       PIC  X(04) VALUE ".ORD".

       01  WS-LOCAL-ID2.
	   03  WS-LOCAL-DRIVE2 PIC  X(01).
	   03  FILLER	       PIC  X(19) VALUE ":\APACSHR\USERS.DAT".

       01  WS-SORDER-STR.
	   03  FILLER	       PIC  X(64).

001900 01  W00-TRAN-CODES.
001910     03  W00-T-VAT   PIC S9(09)V99 COMP-3.
001920     03  W00-E-DESC  PIC  X(12).
001930     03  W00-A-DESC  PIC  X(12).
001940     03  W00-ACTION  PIC  9(01).
001950     03  W00-T-CODE  PIC  9(02)    COMP-5.
001960     03  W00-T-GL    PIC  9(06)    COMP-3.
001970     03  W00-T-DAY   PIC  9(06)    COMP-5.
001980     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
001990     03  W00-T-MTD   PIC  9(06)    COMP-5.
002000     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
002010     03  W00-T-YTD   PIC  9(06)    COMP-5.
002020     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

002040 01  W02-FID.

       COPY APACFIDS.ID.

002060 COPY CONTROL.ID.

       COPY DEBALT.ID.

000330 COPY DEBDEP.ID.

002100 COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

002120 COPY DEPART.ID.

       COPY PARAM.ID.

002140 COPY RECOVER.ID.

002160 COPY SALE.ID.

       COPY SHARED.ID.

002180 COPY SORDER.ID.

002200 COPY SPARTS.ID.

000140 COPY STKMEM.ID.

002220 COPY STOCK.ID.

       COPY SYSUSE.ID.

       COPY PRNDISK.ID.

003770 01  W02-FILE-IDS.
003910     03  W02-PRINTER.
               05  W02-REPORT PIC X(07) VALUE "SLS001.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W05.VAT.

       COPY W10.SAL.

003230 01  W11-PARTS.
003240     03  W11-PART-DETAILS OCCURS 30.
003250         05  W11-CODE   PIC X(14).
003260         05  W11-DESC   PIC X(30).
003270         05  W11-QUANT  PIC S9(05)V9999 COMP-3.

       COPY W12.WS.

       01  W14-ORDER.
           03  W14-REF         PIC Z(08).
           03  W14-AC          PIC X(06).
           03  W14-NAME.
	       05  W14-NME     PIC X(27).
	       05  FILLER      PIC X(13).
	   03  W14-DATE        PIC Z9/99/9999.
           03  W14-PS          PIC 9(02).
           03  W14-DOC         PIC X(01).
           03  W14-DDOC.
               05  W14-INV     PIC Z(08).
           03  W14-VALUE       PIC Z(07)9.99.

003510 01  W14-DELETE.
003520     03  W14-SUB         PIC 9(03)     COMP-3.
003530     03  W14-SQN         PIC 9(05)     COMP-3 OCCURS 200.

003540 01  W15-DISCOUNT.
003550     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       01  W15-PRICE-NAMES.
	   03  W15-PNAME.
	       05  W15-CHECK   PIC  X(01).
	       05  W15-REST    PIC  X(09).

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).

       COPY W20.WS.

003650 01  W25-CALCS.
003660     03  W25-RESULT      PIC 9(05)V99.
003670     03  W25-RESULT1 REDEFINES W25-RESULT.
003680         05  W25-WHOLE   PIC 9(05).
003690         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003700     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
003710     03  W25-VALUE       PIC S9(07)V99 COMP-3.
003720     03  W25-CREDIT      PIC S9(07)V99 COMP-3.
001660     03  W25-TIME        PIC 9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001690         05  W25-HH      PIC 9(02).
001690         05  W25-MM      PIC 9(02).
001690         05  W25-SS      PIC 9(02).
001680         05  FILLER      PIC 9(02).

       01  W32-MESSAGE.
	   03  W32-REMARKS OCCURS 10
			       PIC  X(63).
       01  W34-HEADER.
	   03  W34-FORMAT      PIC  X(01).
	   03  W34-MARGIN      PIC  9(02) COMP-5.
	   03  W34-SALUTATION  PIC  X(63).

       01  W35-DETKEY.
           03  W35-KEY.
000050       05  W35-REF       PIC  X(08).
             05  W35-SEQN.
                 07  W35-SEQ   PIC  9(03).
                 07  W35-SUB   PIC  9(02).
             05  W35-SQN REDEFINES W35-SEQN
                               PIC  9(05).

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

004540 COPY W50.WS.

004560 01  W60-NME-ADD.
004570     03  W60-NAME.
004580         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

004740 COPY W70DEBT.IWS.

004760 01  W75-KEYS.
004770     03  W75-S          PIC 9(02) COMP-5.
004780     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
004790     03  W75-DNO        PIC X(06) OCCURS 18.
004810	   03  W75-SORD       PIC 9(08) OCCURS 18.

004820 01  W75-TITLES.
004830     03  W75-DESCRIP.
004840         05  FILLER      PIC X(28)      VALUE
004850                     "Mr     Mnr    Mrs    Mev    ".
004860         05  FILLER      PIC X(28)      VALUE
004870                     "Miss   Mej    Ms     Ms     ".
004880         05  FILLER      PIC X(28)      VALUE
004890                     "Dr     Dr     Rev    Ds     ".
004900         05  FILLER      PIC X(28)      VALUE
004910                     "Prof   Prof   The HonSy Edel".
004920         05  FILLER      PIC X(28)      VALUE
004930                     "                            ".
004940     03  W75-CODES REDEFINES W75-DESCRIP.
004950         05  W75-TITLE  OCCURS 10.
004960             07  W75-ETITLE PIC X(07).
004970             07  W75-ATITLE PIC X(07).

004980 01  W80-ACCOUNT.
004990     03  W80-ACNO        PIC X(06).
005000     03  W80-CRED REDEFINES W80-ACNO.
005010         05  W80-CNO     PIC 9(05).
005020         05  W80-CAC REDEFINES W80-CNO.
005030             07  W80-CTYPE PIC 9(01).
005040             07  W80-SER   PIC 9(04).
005050         05  FILLER        PIC 9(01).
005060     03  W80-TYPE        PIC 9(01).

005070 01  W85-PASS.
005080     03  W85-SUPER       PIC  X(06)    OCCURS 9.
005090     03  W85-ENTRY       PIC  9(02)    COMP-3.
005100     03  W85-MARG        PIC S9(02)V99.

005110 01  W85-BATCHES.
005120         05  W85-BATCH   PIC 9(05).

       COPY W90.SAL.

005680 01  W95-STM.
005690     03  W95-COMP        PIC X(40).
005700     03  W95-ADD1        PIC X(30).
005710     03  W95-ADD2        PIC X(30).
005720     03  W95-ADD3        PIC X(30).
005730     03  W95-ADD4        PIC X(30).
005740     03  W95-PC.
005750         05  W95-POST    PIC 9(04).
005760     03  W95-TEL         PIC X(14).
005770     03  W95-V1.
005780         05  W95-T       PIC Z(04)9.99.
005790     03  W95-V2.
005800         05  W95-B       PIC Z(06)9.99-.
005810     03  W95-D.
005820	       05  W95-DTE     PIC Z9/99/9999.
           03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.
005830 01  W100-EDIT.
005840     03  W100-LBAL       PIC Z(06)9.99-.
	   03  W100-LNGTH REDEFINES W100-LBAL.
	       05  FILLER      PIC  X(02).
	       05  W100-LENGTH PIC  ZZZ9.9999.
005850	   03  W100-LARR       PIC Z(06)9.99.
	   03  W100-BRDTH REDEFINES W100-LARR.
	       05  FILLER      PIC  X(01).
	       05  W100-BREADTH PIC ZZZ9.9999.
005860	   03  W100-BAL        PIC Z(08)9.99-.
005870     03  W100-CUR        PIC Z(08)9.99-.
005880     03  W100-30         PIC Z(08)9.99-.
005890     03  W100-60         PIC Z(08)9.99-.
005900     03  W100-90         PIC Z(08)9.99-.
005910     03  W100-120        PIC Z(08)9.99-.
005920     03  W100-INT        PIC Z(08)9.99-.
005930     03  W100-CRL        PIC Z(06)9.
	   03  W100-NMB REDEFINES W100-CRL.
	       05  FILLER      PIC  X(02).
	       05  W100-NUMBER PIC  ZZZ9.
005940	   03  W100-V2.
005950         05  W100-S6V2   PIC Z(05)9.99-.
005960     03  W100-V3 REDEFINES W100-V2.
005970         05  W100-PRICE  PIC X(09).
005980         05  FILLER      PIC X(01).
005990     03  W100-V4 REDEFINES W100-V2.
006000         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

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
006010 SCREEN SECTION.

       COPY BLANK.CRT.

006590 01  S02.
006600	   03  LINE 2  COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
006610                           VALUE "SALES/ORDER PROGRAM CONTROL".

007910 01  S07.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(03) USING W10-ITM AUTO.

005530 01  S07-EXT.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

005530 01  S07-3.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S07-4.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S07-5.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S07-6.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S07-7.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S07-8.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S07-9.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S07-10.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S07-11.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S07-12.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S07-13.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S07-14.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC Z(14) USING W10-I14 AUTO.

       COPY SORLUP.CRT.

       COPY DEBLUP.CRT.

008990 01  S25.
	   03  LINE  7 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
009000	   03  LINE  8 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "³Addresses:                 ".
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Balance  ".
009010	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-BAL.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009030	   03  LINE  9 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ 1 ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  MTD      ".
009040	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING W100-CUR.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009060	   03  LINE 10 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Current  ".
009070	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-CUR.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009090	   03  LINE 11 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD3.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   30 Days ".
009100	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-30.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009120	   03  LINE 12 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD4.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   60 Days ".
009130	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-60.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009150	   03  LINE 13 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PC1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   90 Days ".
009160	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-90.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009180	   03  LINE 14 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ 2 ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  120 Days ".
009190	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-120.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009210	   03  LINE 15 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Interest ".
009220	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-INT.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009240	   03  LINE 16 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD3.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Cr Limit ".
009250	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9 USING DEB-CRL.
009090	   03	       COLUMN 69 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "    ³".
	   03  LINE 17 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD4.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Last Pmt ".
	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC 9999/99/99 USING DEB-PMT.
009090	   03	       COLUMN 70 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   ³".
	   03  LINE 18 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PC2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "       Amt ".
	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-LAST-PMT.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
	   03  LINE 19 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

009600 01  S26-QNT.
009610	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009620                           PIC Z(04)9.99 USING W10-QUANT AUTO.

009630 01  S26-PRC.
009640	   03	       COLUMN 60 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009650                           PIC Z(05)9.99 USING W10-SELL AUTO.

009660 01  S26-DSC.
009670	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009680                           PIC Z9.99 USING WS-DISC AUTO.

009690 01  S26-TDSC.
009700	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009710                           PIC Z9.99 USING WS-TDISC AUTO.

009720 01  S26-AVGE.
009730	   03	       COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009740				 PIC Z(04)9.999 USING STK-AVGE AUTO.

009870 01  S28.
009880	   03  LINE 25 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009890                           PIC ZZZ9 USING WS-SALKEY AUTO.

010020 01  S30.
010030	   03  LINE  2 COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
010040                           VALUE "SALES ORDER".
010050     03  LINE  3 COLUMN  2 VALUE "Ac No".
010060     03          COLUMN 15 VALUE "Name".
	   03	       COLUMN 20 BACKGROUND-COLOR 5
				 PIC  X(40) FROM WS-SPACE.
010070	   03	       COLUMN 61 VALUE "Ord No°".
	   03	       COLUMN 68 BACKGROUND-COLOR 5
				 PIC  X(12) FROM WS-SPACE.
010080	   03  LINE  4 COLUMN  2 VALUE "Assisted".
	   03	       COLUMN 11 BACKGROUND-COLOR 5
				 PIC  X(02) FROM WS-SPACE.
	   03	       COLUMN 61 VALUE "Tel.".
	   03	       COLUMN 66 BACKGROUND-COLOR 5
				 PIC  X(14) FROM WS-SPACE.
010090	   03  LINE  5 COLUMN  2 VALUE "Slip No".
	   03	       COLUMN 10 BACKGROUND-COLOR 0
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 20 VALUE "Contact".
	   03	       COLUMN 29 BACKGROUND-COLOR 5
				 PIC  X(14) FROM WS-SPACE.
010100	   03	       COLUMN 65 VALUE "Date".
	   03	       COLUMN 70 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPACE.
009370	   03  LINE  6 COLUMN  2 REVERSE-VIDEO VALUE "Item Code      I".
009380	   03	       COLUMN 18 REVERSE-VIDEO VALUE "TM Description  ".
009390     03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
009400     03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
009410     03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
009420     03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
009160	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
010190	   03  LINE 20 COLUMN  3 VALUE "Sub Total".
010200     03  LINE 21 COLUMN  3 VALUE "Discount".
010210     03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
010230     03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".

010240 01  S30A.
010250	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08) USING W10-SORD AUTO.

010260 01  S31.
010270	   03  LINE  2 COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
010280                           VALUE "AMEND SALES ORDER".

010290 01  S31A.
010300	   03  LINE  2 COLUMN 32 FOREGROUND-COLOR 7 HIGHLIGHT
010310                           VALUE "DELETE SALES ORDER".

009950 01  S36.
009960	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

010440 01  S040-TRANS.
010450	   03  LINE  6 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "C".
010460	   03	       COLUMN 13 VALUE "reate, ".
010470	   03	       COLUMN 20 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "A".
010480	   03	       COLUMN 21 VALUE "mend or ".
010490	   03	       COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "D".
010500	   03	       COLUMN 30 VALUE "elete Order - ".
010550	   03	       COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "E".
010560	   03	       COLUMN 45 VALUE "xit or ".
	   03	       COLUMN 52 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "Esc".
	   03	       COLUMN 55 VALUE "ape".

       01  S40.
	   03  LINE  8 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ú".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "¿".
007450	   03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "CLIENT DETAILS".
	   03	       COLUMN 48 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 10 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "Name    >".
	   03	       COLUMN 65 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "<".     
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 11 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "Address >".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 12 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE ">".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 13 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE ">".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 14 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE ">".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 15 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE ">".
	   03	       COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "<".
	   03	       COLUMN 30 BACKGROUND-COLOR 2
				 PIC  X(36) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 16 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "Tel.    >".
	   03	       COLUMN 39 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "<".
	   03	       COLUMN 40 BACKGROUND-COLOR 2
				 PIC  X(26) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 17 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "Id.No.  >".
	   03	       COLUMN 39 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
                                 VALUE "<".
	   03	       COLUMN 40 BACKGROUND-COLOR 2
				 PIC  X(26) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03  LINE 18 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ù".

       01  S41.
	   03  LINE 10 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(40) USING W40-NM1 AUTO.
	   03  LINE 11 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-AD1 AUTO.
	   03  LINE 12 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-AD2 AUTO.
	   03  LINE 13 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-AD3 AUTO.
	   03  LINE 14 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-AD4 AUTO.
	   03  LINE 15 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(04) USING W40-PC AUTO.
	   03  LINE 16 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(14) USING W40-HTEL AUTO.
	   03  LINE 17 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(14) USING W40-ID AUTO.

       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".

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

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

       COPY UPDATE.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010580 AA000-MAIN	       SECTION.
010590 AA000-INIT.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	       Check the security level of the USER.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
003240	   IF LS0-SALEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010600	     PERFORM ZA000-INIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  If the system is using seperate order numbers per user - ³
      *    ³  check that the files that are being used may be updated  ³
      *    ³  or if update from local/network is required first.       ³
      *    ³  LS-DSKDRV is used to indicate which drive is in use for  ³
      *    ³  the current session. L=Local drive and N=Network drive.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF W70-ORD-SYS = "Y"
	       IF LS-DSKDRV = "N"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *        IF NOT(WS-LOCAL-DRIVE = "C")
		   OPEN I-O SYSUSER
	       END-IF
	       MOVE LS0-NO	 TO USE-NO
	       PERFORM READ-SYSUSER THRU READ-SYSUSER-EXIT
	       IF (WS-F-ERROR = 64) OR
		  (USE-SEQ NOT = 1)
		  MOVE "Please UPDATE files (Orders Spec)"
				 TO WS-ERR-STRING
003260		  PERFORM ERROR-MESSAGE
		  IF LS-DSKDRV = "N"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *		  IF NOT(WS-LOCAL-DRIVE = "C")
		     CLOSE SYSUSER
		  END-IF
		  GO TO AZ000-EOJ.
	   IF LS-DSKDRV = "N"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *	   IF NOT(WS-LOCAL-DRIVE = "C")
	       CLOSE SYSUSER.
010760	     GO TO BA00.

       AA49.
	     EXIT PROGRAM.

       COPY AA50DSO.LUP.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  SCREEN-SHADOW 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:						       ³
      *	   ³	       SHADE-ROW   - Top line of the box + 1.	       ³
      *	   ³	       SHADE-COL   - Left line of box + 2.	       ³
      *	   ³	       SHADE-WIDTH - Width of the box - 2.	       ³
      *	   ³	       SHADE-LINES - Hight of box - 1.		       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  CHECK-CORRECT 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³	      "Correct ENTER - N if incorrect"		       ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.			       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  ERROR-MESSAGE 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.		       ³
      *    ³							       ³
      *    ³ To display just the error message; use PERFORM	       ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.			       ³
      *    ³							       ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY FUNCTION.CRT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  OPT-MESSAGE			       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'. 			       ³
      *    ³							       ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³							       ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE (Use this format if function or any ³
      *    ³	       other keys are to be tested on exit)	       ³
      *    ³			     OR 			       ³
      *    ³   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       ³
      *    ³   (Use TEST AFTER if WS-OPTION is preloaded with a        ³
      *    ³   default option)					       ³
      *    ³							       ³
      *    ³ This would be displayed as:			       ³
      *    ³	ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³	³	   Print transactions (Y/N) [ ] 	 ³°°   ³
      *    ³	ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³	  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PERFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ³ (The UNTIL and TEST AFTER options also apply here)        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

011430 COPY DEBTOR.TRN.

      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *    *************************************************************
011450 AA300              SECTION.
011460 AA301.
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
011550 AA305.
011560       EXIT.
      *    *************************************************************
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *    *************************************************************
011570 AA400              SECTION.
011580 AA401.
011590	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011600		     " Order no./ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011610		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011620		     " Account no./ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F4" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
                     " From specified date Lookup" 
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011630 AA405.
011640       EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011650 AC000		  SECTION.

011670 COPY CONTROL.RD.

       COPY DEBALT.RD.

011190 COPY DEBDEP.RD.

011710 COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

011730 COPY DEPART.RD.

011750 COPY PARAM.RD.

011770 COPY SALE.RD.

       COPY SHARED.RD.

011790 COPY SORDER.RD.

011810 COPY SPARTS.RD.

000140 COPY STKMEM.RD.

011830 COPY STOCK.RD.

       COPY SYSUSE.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011850 AD000-WRITE	       SECTION.

011870 COPY CONTROL.WR.

011890 COPY PARAM.WR.

011910 COPY SALE.WR.

011930 COPY SORDER.WR.

       COPY STOCK.WR.

       COPY SYSUSE.WR.

011970 COPY DEBTOR.ILU.

011990 COPY SORDER.LUP.

       COPY APAC.HLP.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012010 AY000	       SECTION.
011490 AY05.
011500       MOVE 05             TO REC-FILE.
011510       MOVE ZERO           TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
	   IF WS-ACTION = 9
	       MOVE 0		 TO REC-TYPE
	   ELSE
	       MOVE WS-ACTION	 TO REC-TYPE.
011520       MOVE STK-RECORD1    TO REC-STOCKF.
011530       GO TO AY50.

012020 AY17.
012030       MOVE 17             TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060       MOVE SOR-REC1       TO REC-SORDER.
012070       GO TO AY50.
012080 AY39.
012090       MOVE 39             TO REC-FILE.
012100       MOVE WS-NETKEY      TO REC-KEY.
012110       MOVE NET-RECORD     TO REC-NETWORK.
012120       GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  End transaction in recovery log	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012130 AY49.
012140       MOVE 99             TO REC-FILE.
012150       MOVE ZERO           TO REC-KEY.
012160       MOVE SPACES         TO REC-DETAIL.
012170       PERFORM AY50.
012180       ADD 1               TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
012190	   IF WS-RECOVER > 95
012200         CLOSE RECOVER
012210         OPEN OUTPUT RECOVER
012220         CLOSE RECOVER
012230         OPEN I-O RECOVER
012240         MOVE ZERO         TO WS-RECOVER.
012250       GO TO AY59.
012260 AY50.
012270       ADD 1               TO WS-RECOVER.
012280       MOVE WS-RECOVER     TO WS-RECKEY.
012290       MOVE WS-TRANS       TO REC-TRAN.
012300       WRITE REC-RECORD.
012310     IF WS-STATUS NOT = "00"
012320         DISPLAY "Write error Recovery file - Status " AT 2212
012330			WITH FOREGROUND-COLOR 6 HIGHLIGHT
012340			WS-STATUS WITH FOREGROUND-COLOR 7 HIGHLIGHT
012350         STOP RUN.
012360       CLOSE RECOVER.
012370       OPEN I-O RECOVER.
012380 AY59.
012390       EXIT.
      *    *************************************************************
      *    ****    Start processing transaction
      *    *************************************************************
012470 AY60.
012480	   IF WS-SKIP = "Y" OR "A"
012490         GO TO AY999.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *    *************************************************************
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *    *************************************************************
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *    *************************************************************
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *    *************************************************************
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 4 HIGHLIGHT.
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
      *    *************************************************************
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *    *************************************************************
012670	     MOVE 1		 TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
012740	     GO TO AY999.
      *    *************************************************************
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *    *************************************************************
012750 AY62.
	   IF NOT(PAR-PROG(WS-SUB) = SPACES OR
		  PAR-USR(WS-SUB) = SPACES)
      *    *************************************************************
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *    *************************************************************
	       IF NOT(PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *    *************************************************************
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *    *************************************************************
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *    *************************************************************
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *    *************************************************************
	       IF PAR-USR(WS-SUB) = LS-USER
      *    *************************************************************
      *    * I S   I T	 N E X T   I N	 L I N E   T O	 P R O C E S S *
      *    *************************************************************
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
      *    *************************************************************
      *    **** 	  S E T   W A I T   P E R I O D 	    ****
      *    *************************************************************
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT(PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *    *************************************************************
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *    *************************************************************
012990	     MOVE 1		 TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020       PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT(PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT(PAR-PROG(WS-SUB) = SPACES)
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

013000 AY999.
013010       EXIT.
      /
013020 AZ000-END               SECTION.
013030 AZ000-EOJ.
013040	     CLOSE RECOVER
		   DEBDEP
013100		   SALE.
	   IF W70-ORD-SYS = "Y"
	       IF LS-DSKDRV = "L"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *        IF WS-LOCAL-DRIVE = "C"
		   CLOSE SORDER
			 SYSUSER
		   MOVE WS-SORDER-STR
				 TO W02-SORDER
		   OPEN I-O SORDER.

013140 AZ100.
013150       EXIT PROGRAM.

       COPY CA000.STK.

       COPY CA100.STK.

      *
      *    ****    Pack the delivery address for printing
      *
017330 CA110-ADDRESS       SECTION 2.
017340 CA110.
017350       MOVE ZERO           TO WS-ERROR.
017360       MOVE 1              TO WS-S1 WS-S2.
017370       MOVE 30             TO WS-S3.
017380       MOVE SPACES         TO W60-ADDRESS.
017390 CA115.
017460	   IF W40-PCHAR(WS-S3) NOT = SPACE
017470         GO TO CA120.
017480     IF WS-S3 > WS-S2
017490         SUBTRACT 1        FROM WS-S3
017500         GO TO CA115.
017510 CA120.
017550	     MOVE W40-PCHAR(WS-S2)
				 TO W60-ACHAR(WS-S1).
017560     IF WS-S2 = WS-S3
017570         GO TO CA125.
017580       ADD 1               TO WS-S1.
017590       ADD 1               TO WS-S2.
017600     IF WS-S1 > 70
017610         MOVE 1            TO WS-ERROR
017620     ELSE
017630         GO TO CA120.
017640 CA125.
017650     IF WS-ERROR = 1
017660         GO TO CA130.
017670     IF WS-S1 < 70
017680         ADD 1             TO WS-S1
017690	       MOVE "*" 	 TO W60-ACHAR(WS-S1).
017700 CA130.
017710       ADD 1               TO WS-S1.
017720 CA135.
017730       MOVE 31             TO WS-S2.
017740       MOVE 60             TO WS-S3.
017750       PERFORM CA115 THRU CA130.
017760     IF WS-ERROR = 1
017770         GO TO CA140-EXIT.
017780       MOVE 61             TO WS-S2.
017790       MOVE 90             TO WS-S3.
017800       PERFORM CA115 THRU CA130.
017810     IF WS-ERROR = 1
017820         GO TO CA140-EXIT.
017830       MOVE 91             TO WS-S2.
017840       MOVE 120            TO WS-S3.
017850       PERFORM CA115 THRU CA130.
017860 CA140-EXIT.
017870       MOVE ZERO TO WS-S6.
017880 CA140.
017890       EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Read debtor record - using account number.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
013610 CA155-GET-DEBTOR  SECTION 2.
013620 CA155.
013630       MOVE W80-ACNO       TO DEB-ACNO.
013640       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
013650     IF WS-F-ERROR = 6
013660         MOVE 1            TO WS-ERROR
013670     ELSE
013680     IF DEB-ACNO NOT = W80-ACNO
013690         MOVE 2            TO WS-ERROR
013700     ELSE
013710         MOVE 0            TO WS-ERROR
013720         MOVE DEB-TYPE     TO W80-TYPE.
013730 CA160-EXIT.
013740       EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Unpack the name of an individual into 1st, 2nd and surname³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
013750 CA165-NAME	 SECTION 2.
013760 CA165.
013770       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013780                           INTO W40-SURNAME 
013790                                W40-FNAME 
013800                                W40-SNAME
013810       OVERFLOW
013820           MOVE 1          TO WS-ERROR.
013830 CA170-EXIT.
013840       EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Unpack the postal address into line 1, 2, 3 and 4     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
013850 CA175-ADDRESS	 SECTION 2.
013860 CA175.
013870       UNSTRING W60-ADDRESS DELIMITED BY "*"
013880                           INTO W40-ADD1 
013890                                W40-ADD2 
013900                                W40-ADD3 
013910                                W40-ADD4
013920       OVERFLOW
013930           MOVE 1          TO WS-ERROR.
013940 CA175-EXIT.
013950       EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	Unpack the physical address into line 1, 2, 3 and 4    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
013960 CA185-ADDRESS	 SECTION 2.
013970 CA185.
013980       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
013990                           INTO W40-PADD1 
014000                                W40-PADD2 
014010                                W40-PADD3 
014020                                W40-PADD4
014030       OVERFLOW
014040           MOVE 1          TO WS-ERROR.
014050 CA185-EXIT.
014060	     EXIT.

       COPY CA200.STK.

       COPY CA500.STK.

      *    *************************************************************
      *    ***	G E T	S O R D E R   R E C O R D   U S I N G	SOR-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT ORDER RECORD (QUOTE OR INVOICE)
      *                    3 = ORDER DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *    *************************************************************
014550 GET-SORDER           SECTION.

014570 GET-SORD-ORDNO.
             INITIALIZE W14-DELETE.
	     MOVE 1		 TO W14-SUB WS-S7.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC
                                    W35-KEY.
014580       MOVE W10-SORD       TO SOR-REF W90-RF.
014590       MOVE 100            TO SOR-SQN.
014600       MOVE ZERO           TO WS-ERROR.
014610       PERFORM READ-SORDER THRU READ-SORDER-EXIT.
014620     IF WS-F-ERROR = 20
014630         MOVE 1            TO WS-ERROR
014640         GO TO GET-SORDER-EXIT.
014650     IF SOR-DOC NOT = "O"
014660	       MOVE 2		 TO WS-ERROR
014670         GO TO GET-SORDER-EXIT.
014680       MOVE 1              TO WS-LAST WS-SALKEY.
014690       GO TO GET-SORD-TEST-TYPE.

014710 GET-SORD-NEXT.
014720       PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
014730     IF WS-F-ERROR = 20
014740         MOVE 3            TO WS-ERROR
014750         GO TO GET-SORDER-EXIT.

014770 GET-SORD-TEST-TYPE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		     H E A D E R   R E C O R D		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
014780     IF SOR-TYPE = 0
014790         MOVE SOR-AC       TO W80-ACNO DEB-ACNO
014800         PERFORM CA155-GET-DEBTOR
014810         IF WS-ERROR NOT = ZERO
014820             MOVE 4        TO WS-ERROR
014830             GO TO GET-SORDER-EXIT
014840         END-IF
014850         MOVE SOR-OREF     TO W90-REG
014860         MOVE SOR-NAME     TO W40-NAME W40-NM1
               MOVE SOR-PICNO    TO W90-PICK
014870         MOVE SOR-ORD      TO W90-ORD
014880         MOVE SOR-SMAN     TO W10-SMAN
014890         MOVE SOR-EXP      TO W90-EXP
014910         MOVE SOR-PRC      TO WS-PRC
               MOVE SOR-CONTACT  TO W40-CONTACT
               MOVE SOR-TEL      TO W40-TEL
014920     ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      I T E M	D E T A I L S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
014930     IF SOR-TYPE = 1
014940         INITIALIZE SAL-RECORD1
014950	       MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
014960         MOVE SOR-DESC     TO SAL-MSER
014970         MOVE SOR-QNT      TO SAL-QNT
014980         MOVE SOR-COST     TO SAL-COST
014990         MOVE SOR-SELL     TO SAL-SELL
015000         MOVE SOR-VAT      TO SAL-VAT
015000	       MOVE SOR-VAT-SUB  TO SAL-VAT-SUB
015010         MOVE SOR-VAL      TO SAL-VAL
015020         MOVE SOR-INCTAX   TO SAL-INCTAX
015030         MOVE SOR-NONTAX   TO SAL-NONTAX
015040         MOVE SOR-TAXVAL   TO SAL-TAXVAL
015050         MOVE SOR-DSC      TO SAL-DSC
015060	       MOVE SOR-DISC	 TO SAL-DISC
	       MOVE SOR-PACK	 TO SAL-PACK
015070         MOVE SOR-SQN      TO SAL-SQN
	       IF SAL-VAT-SUB > ZERO
		   ADD SOR-TAXVAL
				 TO W90-TAXVAL(SAL-VAT-SUB)
	       END-IF
	       MOVE "Y" 	 TO SAL-INSTRUCT
015080         PERFORM GET-SORD-WRITE-SALE
015090     ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		     P A R T S	 D E T A I L S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015100     IF SOR-TYPE = 2
015110         INITIALIZE SAL-RECORD1
014950	       MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
014960         MOVE SOR-DESC     TO SAL-MSER
014970         MOVE SOR-QNT      TO SAL-QNT
015140         MOVE SOR-SQN      TO SAL-SQN
015160     ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			   R E M A R K S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015170     IF SOR-TYPE = 3
015180         INITIALIZE SAL-RECORD1
015190         MOVE SOR-SERIAL   TO SAL-MSER
015200         MOVE SOR-SQN      TO SAL-SQN
015210         PERFORM GET-SORD-WRITE-SALE
015160     ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    I N T E R N A L   N O T E S 	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015170     IF SOR-TYPE = 4
015180         INITIALIZE SAL-RECORD1
015190         MOVE SOR-DESC     TO SAL-MSER
015200         MOVE SOR-SQN      TO SAL-SQN
               MOVE 1            TO SAL-SUPER
015210         PERFORM GET-SORD-WRITE-SALE
015160     ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			   A D D R E S S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015170     IF SOR-TYPE = 5
               MOVE SOR-KEY      TO W35-KEY
               MOVE SOR-AD1      TO W40-AD1
               MOVE SOR-AD2      TO W40-AD2
               MOVE SOR-AD3      TO W40-AD3
               MOVE SOR-AD4      TO W40-AD4
               MOVE SOR-PC       TO W40-PC
	   ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			S A L U T A T I O N		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015170	   IF SOR-TYPE = 7
	       MOVE SOR-TEXT	 TO W34-SALUTATION
	   ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			     T E X T			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015170	   IF SOR-TYPE = 8
	       MOVE SOR-TEXT	 TO W32-REMARKS(WS-S7)
	       ADD 1		 TO WS-S7
015220	   ELSE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  T R A I L E R 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015230         MOVE SOR-SUBTOT   TO W90-DB
015240         MULTIPLY -1 BY SOR-DSCNT    
015250                           GIVING W90-TDISC
015260         MOVE W90-TDISC    TO W90-LDISC
015270         MOVE SOR-NON-TXBL TO W90-NONTAX
015280	       MOVE SOR-TAXABLE  TO W90-EXCTOT
015290         MOVE SOR-TAX      TO W90-TTAX
015300         MOVE SOR-TOT      TO W90-TOTAL
015310         MOVE SOR-BAL      TO W90-BAL
               MOVE SOR-INSTRUCT TO W90-INSTRUCT
               MOVE SOR-PMETH    TO W90-PMETH
               MOVE SOR-STATUS   TO W40-STATUS
               MOVE SOR-HTEL     TO W40-HTEL
               MOVE SOR-ID       TO W40-ID
015330         GO TO GET-SORDER-EXIT.
015340       GO TO GET-SORD-NEXT.

015360 GET-SORD-WRITE-SALE.
015370       MOVE WS-LAST        TO SAL-CODE.
015380       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
015390       ADD 1               TO WS-LAST WS-SALKEY.
015400 
015410 GET-SORDER-EXIT.
015420       EXIT.
      /
015430 BA000                    SECTION.
015440 BA00.
015450       DISPLAY CLR-SCREEN.
015460       DISPLAY S02.
015480       DISPLAY S040-TRANS.
	   IF LS-DAYS > 65
	       COMPUTE WS-NUM = 15 - (LS-DAYS - 65)
               DISPLAY "This package must be registered within " AT 2502
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 4 HIGHLIGHT
			WS-NUM WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 7 HIGHLIGHT BLINK
		       " days" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 4 HIGHLIGHT.
015500 BA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
015600		     GO TO AZ000-EOJ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	   ELSE
	       CALL X"E5"
	       GO TO BA05.
015570	     MOVE ZERO		 TO W90-BAL.
015580	     DISPLAY CLEAR-L25.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       E X I T	 P R O G R A M		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015590	   IF WS-OPTION = "E"
015600         GO TO AZ000-EOJ.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       C R E A T E   O R D E R		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015610	   IF WS-OPTION = "C"
015620         MOVE "O"          TO W90-ORG
015630         PERFORM BC000
015640         GO TO BA00.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			A M E N D   O R D E R		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015650	   IF WS-OPTION = "A"
015660         MOVE "O"          TO W90-ORG
015670         PERFORM BD000
015680         GO TO BA00.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       D E L E T E   O R D E R		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
015690	   IF WS-OPTION = "D"
015700         MOVE "O"          TO W90-ORG
015710         PERFORM BE000
015720         GO TO BA00.
015790	     GO TO BA05.
      /
       BB000               SECTION.
       BB00.
           IF DEB-NCASH = "CASH SALE" OR
                          "KONTANT V"
               GO TO BB06.
       BB05.
	     MOVE "Enter client details Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y".
	   IF WS-OPTION = "N"
               GO TO BB20.
       BB06.
	     PERFORM SAVE-SCREEN.
	     MOVE SPACES	 TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-HTEL
                                    W40-ID  W40-PC.
             MOVE 0815           TO CPOS.
       BB10.
             DISPLAY 
		 "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
		 AT CPOS.
           IF CLIN < 18
               ADD 1             TO CLIN
               GO TO BB10.
	     DISPLAY S40.

       BB15.
             ACCEPT S41.
           IF DEB-NCASH = "CASH SALE" OR
                          "KONTANT V"
               IF (W40-NM1 = SPACES) OR
                  (W40-AD1 = SPACES) OR
                  (W40-HTEL = SPACES)
		   MOVE "Require Name, Address & Telephone"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO BB15.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
               GO TO BB15.
           IF DEB-NCASH = "CASH SALE" OR
                          "KONTANT V"
               MOVE W40-NM1      TO W40-NAME
               MOVE W40-AD1      TO W40-ADD1
               MOVE W40-AD2      TO W40-ADD2
               MOVE W40-AD3      TO W40-ADD3
               MOVE W40-AD4      TO W40-ADD4
               MOVE W40-PC       TO W40-PC1.
       BB20.
	     DISPLAY CLEAR-L25.
       BB29.
             EXIT.
      /
017690 BB050               SECTION.
017700 BB50.
             MOVE WS-EXPP        TO REP-DETAIL2.
             MOVE ZERO           TO REP-LN.
	     WRITE REP-L2.
	     MOVE W95-COMP	 TO REP-DETAIL2.
             MOVE 2              TO REP-LN.
	     WRITE REP-L2.
017710	     MOVE SPACES	 TO REP-DETAIL2.
017750       MOVE "PICKING SLIP - REFERENCE NO:"
                                 TO REP-H1.
             MOVE W90-PS         TO REP-REF.
             MOVE ZERO           TO REP-LN.
	     WRITE REP-L2.
             MOVE 2              TO REP-LN.
017760	     WRITE REP-L2.
           IF W90-PICK > 01
               MOVE SPACES       TO REP-DETAIL2
               MOVE "REPLACE PREVIOUS PICKING SLIP"              
                                 TO REP-H1
               MOVE 2            TO REP-LN
	       WRITE REP-L2.

       BB60.
             MOVE WS-ECAN        TO REP-DETAIL2.
             MOVE ZERO           TO REP-LN.
	     WRITE REP-L2.
017710       MOVE SPACES         TO REP-DETAIL2.
             MOVE "Account :"    TO REP-AC.
017770       MOVE W80-ACNO       TO REP-ACNO.
             MOVE W40-NAME       TO REP-NAME.
             MOVE "Date:"        TO REP-DATE.
             MOVE W12-TODAY      TO REP-DTE.
             MOVE 1              TO REP-LN.
017760	     WRITE REP-L2.
017790       MOVE SPACES         TO REP-DETAIL2.
             MOVE " Contact:"    TO REP-CNTCT.
             MOVE W40-CONTACT    TO REP-CONTACT.
             MOVE "Tel :"        TO REP-AC.
             MOVE W40-TEL        TO REP-TEL.
             MOVE "Time:"        TO REP-DATE.
012150       ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
             MOVE W95-TIME       TO REP-TIME.
	     MOVE 1		 TO REP-LN.
017760	     WRITE REP-L2.
017790       MOVE SPACES         TO REP-DETAIL2.
	   IF WS-DELIV = "Y"
	       IF NOT(W40-PADDRESS = SPACES)
		   PERFORM CA110-ADDRESS
		   INSPECT W60-ADDRESS REPLACING ALL "*" BY "."
		   MOVE 1	 TO REP-LN
		   MOVE W60-ADDRESS
				 TO REP-DETAIL2
		   WRITE REP-L2
		   MOVE SPACES	       TO REP-DETAIL2.
	     MOVE ALL "="	 TO REP-DETAIL1.
             MOVE 2              TO REP-LN.
017760	     WRITE REP-L2.
017790       MOVE SPACES         TO REP-DETAIL2.
017800	     MOVE "Item Code"	 TO REP-H2.
017820	     MOVE "Description"  TO REP-H3.
017840	     MOVE "  Quantity"	 TO REP-H4.
	   IF WS-PIC-PRC = "Y"
	       MOVE "     Value" TO REP-H5
	   ELSE
	   IF WS-ITMPRC = "Y"
	       MOVE "Item Price" TO REP-H5
	   ELSE
	       MOVE "Remarks"	 TO REP-H5.
	     MOVE 2		 TO REP-LN.
017850	     WRITE REP-L2.
             MOVE SPACES         TO REP-DETAIL2.
017950 BB65.
017960       EXIT.
      /
015800 BB100               SECTION.
015810 BB100-MARG.
015820       MOVE ZERO TO WS-ERROR.
015830     IF W90-TAX = SPACES
015840         IF STK-TAX NOT = ZERO
015850             COMPUTE W10-SPAPRC = (W10-SPAPRC /
015860                                  (100.0000 + W05-RATE)) * 100. 
	   IF WS-PRC = "3"
018050         IF STK-WSALE NOT > W10-SPAPRC
018060             GO TO BB110
               END-IF
           ELSE
	   IF WS-PRC = "2"
018050         IF STK-CASH NOT > W10-SPAPRC
018060             GO TO BB110
               END-IF
           ELSE
           IF STK-SELL NOT > W10-SPAPRC
               GO TO BB110.
	   IF NOT(W10-SPAPRC = ZERO)
	       GO TO BB104.
	BB103.
	     MOVE "Is this a no-charge item Y/N"
				 TO WS-ERR-MES.
	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".
	     GO TO BB110.
       BB104.
015890       COMPUTE W10-MARG = ((W10-SPAPRC - W10-COST) / W10-SPAPRC)
015900                          * 100.00.
015910     IF W10-MARG NOT < W85-MARG
015920         GO TO BB110.
015950 BB105.
018110	     MOVE "Margin less than allowed - Override Y/N?"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".
016040	   IF WS-OPTION = "N"
016050         MOVE 1 TO WS-ERROR
016060         GO TO BB110.
016070 BB106.
016080	     DISPLAY "Enter password" AT 2551
016090		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
016100	     ACCEPT W40-CSHPASS AT 2567
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT
016110			 AUTO NO-ECHO.
016120	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016180	     MOVE 1		 TO WS-LENGTH.
       BB107.
           IF W40-CSHPASS = "!@$^*)"
               MOVE 10           TO WS-SUPER
           ELSE
	   IF W40-CSHPASS = W85-SUPER(1)
               MOVE 1            TO WS-SUPER
           ELSE
	   IF W40-CSHPASS = W85-SUPER(9)
               MOVE 9            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
           GO TO BB110.
       BB108.
           IF W40-CSHPASS = "!#%&()"
               MOVE 10           TO WS-SUPER
           ELSE
	   IF W40-CSHPASS = W85-SUPER(1)
               MOVE 1            TO WS-SUPER
           ELSE
	   IF W40-CSHPASS = W85-SUPER(6)
               MOVE 6            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
018480 BB110.
018490       EXIT.
      /    *************************************************************
      *    ****    P I C K I N G   S L I P S
      *    *************************************************************
018500 BB200         SECTION.
018570 BB205.
           IF WS-PGADV = "Y"
	       OPEN OUTPUT PRNDSK
	       CALL X"91" USING X91-RES X91-FUN PRNDSK.
	     MOVE LS-SLS-ORDNO	 TO W90-REF W10-SORD.
             PERFORM GET-SORDER.
           IF WS-ERROR NOT = ZERO
	       MOVE "Database corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BB399.
       BB210.
      *    *************************************************************
      *    ****    R E A D   D E T A I L S   A N D   P R I N T   S L I P
      *    *************************************************************
       BB265.
025200	     DISPLAY "Printing Picking Slip" AT 2502
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
025210			   BACKGROUND-COLOR 3 BLINK.
025220       MOVE ZERO           TO WS-SALKEY.
025230       PERFORM AY60 THRU AY999.
026210 BB270.
      *    *************************************************************
      *    ****    R E A D   O R D E R   F I L E   A N D   P R I N T
      *    *************************************************************
026330       MOVE W90-REF        TO SOR-REF.
             MOVE ZERO           TO SOR-SQN.
             PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
	       MOVE "Database corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BB399.
       BB275.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO BB300.
           IF SOR-REF NOT = W90-REF
               GO TO BB300.
           IF SOR-TYPE = 0
               ADD 1             TO W90-PICK
               PERFORM BB050
               GO TO BB275.
           IF SOR-TYPE = 9
               GO TO BB300.
           IF SOR-TYPE NOT = 1
               IF SOR-TYPE = 2 OR 5
                   GO TO BB275
               ELSE
                   MOVE SOR-DESC TO REP-DESC
                   PERFORM BB280
                   GO TO BB275.
	     MOVE SOR-EXT-ITEM	 TO PRT-EXT-ITEM.
	   IF WS-USE-ITM = "Y"
	       MOVE SOR-ITEM	 TO REP-ITM1
	       MOVE "/" 	 TO REP-SL
	       MOVE SOR-ITM	 TO REP-ITM
	   ELSE
	       MOVE SOR-EXT-ITEM TO REP-EXT-ITEM.
	     MOVE SOR-DESC	 TO REP-DESC.
             MOVE SOR-QNT        TO REP-QTY.
	   IF WS-PIC-PRC = "Y"
	       MOVE SOR-VAL	 TO REP-PRICE
	   ELSE
	   IF WS-ITMPRC = "Y"
	       MOVE SOR-SELL	 TO REP-PRICE.
020530	     PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510     IF WS-F-ERROR = 21
      * 	MOVE SOR-QNT	  TO REP-QNT
               PERFORM BB280
021310         GO TO BB275.

       BB280.
	   IF LINAGE-COUNTER OF PRNDSK < WS-PRN-LENGTH
               MOVE 1            TO REP-LN
	       WRITE REP-L2
           ELSE
               MOVE "PAGE"       TO REP-PG
	       WRITE REP-L2
               MOVE WS-EXPP      TO REP-DETAIL2
               MOVE ZERO         TO REP-LN
	       WRITE REP-L2
               MOVE "PICKING SLIP - CONTINUED ..."
                                 TO REP-H1
	       WRITE REP-L2
               MOVE 2            TO REP-LN
	       WRITE REP-L2
               PERFORM BB60 THRU BB65.
             MOVE SPACES         TO REP-DETAIL2.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
       BB285.
021370       MOVE 1              TO WS-S6.
021420 BB290.
021430	   IF PRT-EXT-CODE(WS-S6) = SPACES
021440         ADD 1             TO WS-S6
021450         GO TO BB295.
	   IF WS-USE-ITM = "Y"
	       MOVE PRT-CODE(WS-S6)
				 TO REP-ITM2
	       MOVE "/" 	 TO REP-SL2
	       MOVE PRT-PITM(WS-S6)
				 TO REP-ITM3
	   ELSE
	       MOVE PRT-EXT-CODE(WS-S6)
				 TO REP-EXT-ITEM2.
021470	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W10-EXT-ITEM.
021480	     MULTIPLY PRT-QUANT(WS-S6) BY SOR-QNT
                                 GIVING REP-QNT.
021490       PERFORM CA000.
021500     IF WS-F-ERROR = 22
021510         ADD 1             TO WS-S6
021520         GO TO BB295.
021540       MOVE STK-DESC       TO REP-DSCR.
             PERFORM BB280
021600	     ADD 1		 TO WS-S6.

021660 BB295.
021670     IF WS-S6 < 31
021680         GO TO BB290.
             GO TO BB275.
      *    *************************************************************
      *    ****    U P D A T E   O R D E R   F I L E 
      *    *************************************************************
       BB300.
             MOVE SPACES         TO REP-DETAIL2.
	     MOVE 1		 TO REP-LN.
	     WRITE REP-L2.
	     MOVE ALL "="	 TO REP-DETAIL1.
026330	     MOVE W90-REF	 TO SOR-REF.
             MOVE 100            TO SOR-SQN.
	     PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             ADD 1               TO SOR-PICNO.
             PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             PERFORM AY70 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ To avoid problems of having data corrupted (MULTI USER)   ³
      *    ³ by the cache (buffering) of data on the individual Work   ³
      *    ³ stations. This results in data from one work station not  ³
      *    ³ being available on another Work station until the cache   ³
      *    ³ has been ceared. Including a close of the file after each ³
      *    ³ new order has been added and then re-opening the file is  ³
      *    ³ the way in which the system will release the data for it  ³
      *    ³ to be written to the server file and should resolve this  ³
      *    ³ problem.						       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     CLOSE SORDER.
	     OPEN I-O SORDER.

       BB310.
	     MOVE "Y"		 TO WS-OPTION.
	     MOVE WS-OPTION	 TO WS-PGADV.
	     MOVE "PAGE"	 TO REP-PG.
	     WRITE REP-L2.
	     CLOSE PRNDSK.
	     PERFORM BB400.
             MOVE SPACES         TO REP-DETAIL2.

       BB399.
             EXIT.
      /
       BB400         SECTION.
       BB405.
	     OPEN INPUT PRNDSK.
	     CALL X"91" USING X91-RES X91-FUN PRNDSK.
       BB410.
             READ PRNDSK
                  AT END
                     CLOSE PRNDSK
                     GO TO BB499.
             MOVE REP-DETAIL2    TO PRN-DETAIL2.
           IF REP-PG = "PAGE"
               WRITE PRN-L2 BEFORE PAGE
           ELSE
               MOVE REP-LN       TO WS-LINES
               WRITE PRN-L1 BEFORE WS-LINES LINES.
             GO TO BB410.
       BB499.
             EXIT.
      /    *************************************************************
      *    ****    O R D E R S 
      *    *************************************************************
016320 BC000         SECTION 50.
016330 BC00.
      *    *************************************************************
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *    *************************************************************
016340       MOVE 5              TO WS-PARKEY.
016350       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016360       MOVE PAR-PW         TO W85-PASS. 
      *    *************************************************************
      *    ****    S A L E S   O R D E R   S C R E E N
      *    *************************************************************
016370 BC05.
016380	     DISPLAY CLR-SCREEN.
016390       DISPLAY S30.
016400       MOVE 8              TO LIN.
016410       PERFORM BC115 UNTIL LIN > 18.
016420       MOVE W12-TODAY      TO W90-DTE.
016430	     DISPLAY W90-DTE AT 0570
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
016440	     MOVE ZERO		 TO W90-BDT W90-LDISC W90-NONTAX
016450				    W90-BAL W90-SALES W90-EXCTOT
016460				    W90-CUR W90-PMETH W40-STATUS.
016470	     MOVE 1		 TO WS-FIRST WS-SALPOS WS-LAST.
016480	     MOVE SPACES	 TO W90-TAX W90-INSTRUCT W40-SPAR.
             MOVE "N"            TO WS-IN-OPT.
	     MOVE 1		 TO WS-S9.

       BC08.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BC08.
      *    *************************************************************
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *    *************************************************************
016490 BC10.
016500	     MOVE SPACES	 TO W80-ACNO.

016510 BC11.
016520	     DISPLAY "F1" AT 2502 WITH BACKGROUND-COLOR 3
				       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Debtor Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "=Create,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F9" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Debtor Lookup (Any Word)"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
016540	     ACCEPT S36 AT 0308.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO BC265
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50D THRU AA59
		 WHEN F8-KEY
003240		     IF LS0-DBLEV > 0
			 PERFORM GA000
			 MOVE LS0-DBT
				 TO W80-ACNO
		     END-IF
		 WHEN F9-KEY
		     MOVE "A"	 TO WS-TYP
016560		     PERFORM AA50D THRU AA59
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC11
	       END-EVALUATE
016570	       DISPLAY S36 AT 0308
016580         IF W80-ACNO = SPACES
016590             GO TO BC11.
016600	     DISPLAY CLEAR-L25.
016610     IF W80-ACNO = SPACES
016620	       GO TO BC265.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.

016630 BC12.
016650       PERFORM CA155-GET-DEBTOR.
016660     IF WS-ERROR = 1
016670	       MOVE "No record"  TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
016680         GO TO BC11.
016690     IF WS-ERROR = 2
016700	       MOVE "Invalid account"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
016720         GO TO BC11.
      *    *************************************************************
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *    *************************************************************
016730 BC15.
016740	     MOVE DEB-NAME	 TO W40-NAME.
016750     IF W80-TYPE = 1
016760         INSPECT W40-NAME REPLACING ALL "*" BY " "
016770         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
016780	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
016790       COMPUTE W100-CUR = DEB-BAL - (DEB-CUR + DEB-30 +
016800                          DEB-60 + DEB-90 + DEB-120 + DEB-INT).
	     PERFORM BC189 THRU BC215.
      *    *************************************************************
      *    ****    I S   T H I S   T H E   C O R R E C T   A C C O U N T
      *    *************************************************************
             PERFORM BC18-MEMO THRU BC18E.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 51		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
019090	     DISPLAY S25.
	     PERFORM SCREEN-SHADOW.
      *    *************************************************************
      *    ****   C A T E G O R Y   8 0   -  S U S P E N D E D   A / C
      *    *************************************************************
           IF DEB-CAT = 80
	       MOVE "This account has been suspended"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC05.
	     MOVE "Press any key to continue"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	     PERFORM RESTORE-SCREEN-2.
      *    *************************************************************
      *    ****   U S E R   O P T I O N
      *    *************************************************************
       BC16.
032920	     MOVE "'C'ontinue, 'R'ekey, 'E'xit, 'F9' Memo/Remarks"
				 TO WS-ERR-MES.
      *    *************************************************************
      *    ****   D e f a u l t   -   C o n t i n u e
      *    *************************************************************
	     MOVE "C"		 TO WS-OPTION.

016890 BC18.
	     PERFORM OPT-MESSAGE.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN NOT F9-KEY GO TO BC19
	       END-EVALUATE
	   ELSE
	       GO TO BC19.

       BC18-MEMO.
019250	     DISPLAY CLEAR-L25.
             MOVE W80-ACNO       TO DME-AC.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO BC18E.
             INITIALIZE W200-MEM-FLDS.

       BC18A.
             MOVE DME-SEQ        TO WS-S1.
	     MOVE DME-DATA	 TO W200-MEM(WS-S1).
             PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
                   GO TO BC18A.
	     PERFORM SAVE-SCREEN.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY S-DEB-MEM.
             DISPLAY SA-MEM.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     PERFORM RESTORE-SCREEN.

       BC18E.
	     EXIT.

       BC18-E.
	     GO TO BC16.

       BC19.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
016970	   IF NOT(WS-OPTION = "C" OR "E" OR "R")
016980         GO TO BC18.
016990       MOVE 8              TO LIN.
017000       PERFORM BC115 UNTIL LIN > 18.

017010 BC20.
017020	     DISPLAY CLEAR-L25.
017030     IF WS-OPTION = "R"
017040         GO TO BC05.
017050     IF WS-OPTION = "E"
017060	       GO TO BC265.
017070       MOVE "Y"            TO WS-OPTION.
      *    *************************************************************
      *    ****  C H E C K   A R R E A R S   &	 C R E D I T   L I M I T
      *    *************************************************************
017080     IF (DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
019360	       MOVE "Arrears > 30 days - Override Y/N"
				 TO WS-ERR-MES
	       MOVE SPACE	 TO WS-OPTION
	       PERFORM OPT-MESSAGE TEST AFTER
		       UNTIL WS-OPTION = "Y" OR "N"
017180	       IF WS-OPTION = "Y"
017190             MOVE ZERO     TO WS-ERROR
017200             PERFORM BB106
019470             PERFORM BB108
017210             IF WS-ERROR = 1
017220                 MOVE "N"  TO WS-OPTION.                       
017240	   IF WS-OPTION = "N"
017250         GO TO BC05.
017260     IF DEB-CRL > ZERO
017270         IF DEB-BAL > DEB-CRL
019550		   MOVE "Credit limit exceded - Override Y/N"
				 TO WS-ERR-MES
		   MOVE SPACE	 TO WS-OPTION
		   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "Y" OR "N"
017370	       IF WS-OPTION = "Y"
017380             MOVE ZERO     TO WS-ERROR
017390             PERFORM BB106
019470             PERFORM BB108
017400             IF WS-ERROR = 1
017410                 MOVE "N"  TO WS-OPTION.                       
017430	   IF WS-OPTION = "N"
017440         GO TO BC05.   
           IF DEB-NCASH = "CASH SALE" OR
                             "KONTANT V"
               MOVE 1            TO W90-PMETH
	       PERFORM SAVE-SCREEN-3
               PERFORM BB000
	       PERFORM RESTORE-SCREEN-3.

017530 BC22.
	   IF NOT VALID-PRICE
	       MOVE "1" 	 TO DEB-PRICE.
             MOVE DEB-PRICE      TO WS-PRC.
	     MOVE SPACES	 TO WS-RESTRICT.
	     MOVE "N"		 TO WS-RESTCODE.
      *    *************************************************************
      *    ****   D E B T O R	S A L E S   R E S T R I C T I O N S
      *    *************************************************************
	   IF DEB-RESTRICT = "A" OR "E"
	       MOVE DEB-ACNO	 TO DBD-ACNO
	       PERFORM READ-DEBDEP THRU READ-DEBDEP-EXIT
	       IF WS-F-ERROR = 0
		   MOVE DEB-RESTRICT
				 TO WS-RESTRICT.

017620 BC25.
017630       MOVE SPACES         TO W90-ORD W90-REG.

017640 BC30.
017660	     ACCEPT W90-ORD AT 0368
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
017670			 BACKGROUND-COLOR 5 UPDATE AUTO.
      *    *************************************************************
      *    ****    S A L E S M A N   N U M B E R
      *    *************************************************************
017680       MOVE DEB-SMAN       TO W10-SMAN.
             MOVE DEB-CONTACT    TO W40-CONTACT.
	   IF NOT(DEB-NCASH = "CASH SALE" OR "KONTANT V")
               IF DEB-TYPE = 1
                   MOVE DEB-H-TEL 
                                 TO W40-TEL
               ELSE
                   MOVE DEB-B-TEL
                                 TO W40-TEL.

017690 BC35.
	   IF W10-SMAN = ZERO
017700	       ACCEPT W10-SMAN AT 0411
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
017710			   BACKGROUND-COLOR 5 UPDATE AUTO.
017720     IF W10-SMAN > 50
017730         GO TO BC35.
017740     IF W10-SMAN = ZERO
017750         GO TO BC40.
017760       ADD 100 W10-SMAN    GIVING WS-PARKEY.
017770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017780     IF PAR-SALESMAN = SPACES
017790	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017810         GO TO BC35.
017830	     DISPLAY PAR-SALESMAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

017840 BC40.
             ACCEPT W40-TEL AT 0466 
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
             ACCEPT W40-CONTACT AT 0528 
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF DEB-CAT = 79
	       MOVE "C" 	 TO W90-EXP
	       MOVE SPACES	 TO W90-TAX
	       GO TO BC45.
020300	   IF DEB-INV-TYPE = "E"
020310         MOVE "Y"      TO WS-OPTION
	   ELSE
	   IF DEB-INV-TYPE = "I"
020330	       MOVE "N"      TO WS-OPTION
	   ELSE
	       MOVE "X"      TO WS-OPTION.

       BC42.
	     MOVE SPACES     TO WS-ERR-MES.
033840	     STRING "TAX Exclusive (Y/N) or eXport?  ["
		    DELIMITED SIZE
		    WS-OPTION DELIMITED SIZE
		    "]" DELIMITED SIZE
			     INTO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y" OR "X".
018160	     MOVE WS-OPTION	 TO W90-EXP.
           IF WS-OPTION = "Y"
               MOVE "E"          TO W90-EXP.
020600     IF WS-OPTION = "N"
020610         MOVE SPACES       TO W90-TAX
               MOVE "I"          TO W90-EXP
020630     ELSE
020640         MOVE W05-VATNO    TO W90-TAX.

018390 BC45.
018400	     DISPLAY CLEAR-L25.

       BC46.
	     MOVE "COLLECT"	 TO W90-INSTRUCT.
	     PERFORM CHECK-CORRECT.
018480	   IF WS-OPTION = "N"
018490	       GO TO BC11.
018510       MOVE ZERO           TO W90-DB W90-CR.

018520 BC50.
018530       MOVE 8              TO LIN.

018540 BC55.
018550	     MOVE SPACES	 TO W10-DESC W10-EXT-ITEM.
018560       MOVE ZERO           TO W10-SELL.
018560       MOVE 1              TO W10-QUANT.

018570 BC60.
018580       PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       ACCEPT S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *        ***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT S07-ITM AT LINE LIN
	       ELSE
021070		   ACCEPT S07 AT LINE LIN
	       END-IF
	   ELSE
      *    *************************************************************
      *    ***** NUMERIC CODES APPLY *****
      *    *************************************************************
	   IF W10-SLNGTH = 3
	       ACCEPT S07-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S07-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S07-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S07-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S07-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S07-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S07-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S07-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S07-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S07-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S07-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S07-14 AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
		     GO TO BC60A
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *	   ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC60
	       END-EVALUATE
018620	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN.
018640	       IF W10-EXT-ITEM = SPACES
018650             GO TO BC60.

       BC60A.
018660	   IF W10-EXT-ITEM = SPACES
018670         SUBTRACT 1        FROM WS-LAST
018680         GO TO BC120.

       BC61.
018690       PERFORM CA000.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
018700	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN
		   END-IF
	       END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BC60
               ELSE
                   GO TO BC61.

       BC62.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC60.
      *    *************************************************************
      *    **** C H E C K   F O R   S A L E S	R E S T R I C T I O N S
      *    *************************************************************
	   IF WS-RESTRICT = "A" OR "E"
	       PERFORM MA000
	       IF ((WS-RESTCODE = "Y") AND (WS-RESTRICT = "E")) OR
		  ((WS-RESTCODE = "N") AND (WS-RESTRICT = "A"))
		   MOVE "Restricted for this Department"
                                 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
020140		   GO TO BC60.
018730	     MOVE WS-LAST	 TO WS-SALKEY.
018740       INITIALIZE SAL-RECORD1.
018750       MOVE WS-LAST        TO SAL-CODE.
018770	     MOVE STK-CODE	 TO SAL-EXT-ITEM.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
018790 BC65.
018780	     DISPLAY CLEAR-L25.
018800     IF STK-IND = 3
018810         MOVE ZERO         TO STK-AVGE
018820	       ACCEPT W10-SDSC AT LINE LIN COLUMN 21
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
018830			   BACKGROUND-COLOR 0 UPDATE AUTO
018840         DISPLAY "Index Unit cost" AT 2512
018850			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018860         ACCEPT S26-AVGE AT LINE 25
018870     ELSE
018880         MOVE STK-DESC     TO W10-DESC
018890	       DISPLAY W10-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0.
018900     IF STK-IND = 3
018910         IF STK-AVGE = ZERO
021070		   MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
018940		   GO TO BC65.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
018960	     MOVE W10-EXT-ITEM	 TO SAL-EXT-ITEM.
018970       MOVE W10-DESC       TO SAL-MSER.
018980       MOVE STK-AVGE       TO SAL-COST W10-COST.
      *    *************************************************************
      *    ****    G E T   Q U A N T I T Y
      *    *************************************************************
	     MOVE ZERO		 TO W10-LENGTH W10-BREADTH W10-NUMBER.
	     MOVE "N"		 TO WS-MEASURE.
	     MOVE 1		 TO WS-P1.
	   IF (LS0-LEV < 3) OR
	      (STK-USE-PACKS = "N")
	       GO TO BC66.
	     DISPLAY "µ" AT 0750
		      STK-PER WITH BACKGROUND-COLOR 5
				   FOREGROUND-COLOR 6 HIGHLIGHT
		     "Æ".
	     GO TO BC66.

       BC65-PACK.
	   IF NOT(STK-UNT(WS-P1) = ZERO)
	       DISPLAY STK-PR(WS-P1) AT 0751
		       WITH BACKGROUND-COLOR 5
			    FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
	   IF WS-P1 = 1
	       DISPLAY "  Each  "  AT 0751
			WITH BACKGROUND-COLOR 5
			     FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
	       SUBTRACT 1	 FROM WS-P1
	       GO TO BC65-PACK.

       BC66.
	     DISPLAY "F6" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     " to key in "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "- Cutting size (Wood, Glass etc) "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "or "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 1.
	   IF LS0-LEV > 2
	       IF STK-USE-PACKS = "Y"
		   DISPLAY "" AT 2560
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
			   " for Packs"
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1.
021140	     ACCEPT S26-QNT AT LINE LIN.
	   IF ADIS-FUNC
	       IF LS0-LEV < 3
		   GO TO BC66
	       END-IF
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF WS-P1 > 1
			 SUBTRACT 1  FROM WS-P1
			 GO TO BC65-PACK
		 WHEN DOWN-KEY
		     IF WS-P1 < 4
			 ADD 1	     TO WS-P1
			 GO TO BC65-PACK
	       END-EVALUATE
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
021390		     DISPLAY WS-INV78 AT LINE LIN COLUMN 2
			     WITH FOREGROUND-COLOR 3
021400		     GO TO BC55
		 WHEN F6-KEY
		     PERFORM BB050
021370		     DISPLAY S26-QNT AT LINE LIN
		 END-EVALUATE.
021150	   IF W10-QUANT = ZERO
021160	       DISPLAY WS-INV78 AT LINE LIN COLUMN 2
		       WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
021170	       GO TO BC55.
      ******************************************************************
      *    T O	 B E   I N C L U D E D	 A T   A   L A T E R   D A T E
      *    *************************************************************
      *    ****    C H E C K   S T O C K   Q U A N T I T I E S
      *    *************************************************************
      *	   IF W10-QUANT > STK-QUANT
      *	       IF STK-QUANT < .01
      *		   MOVE ZERO	 TO W100-QNT
      *	       ELSE
      *		   MOVE STK-QUANT
      *				 TO W100-QNT
      *	       END-IF
      *	       MOVE SPACES	 TO WS-ERR-MES
      *	       STRING "Available " W100-V2 " "
      *		      "Accept/Override A/O [ ]"
      *				 INTO WS-ERR-MES
      *	       MOVE "A" 	 TO WS-OPTION
003260*	       PERFORM OPT-MESSAGE TEST AFTER
      *		       UNTIL WS-OPTION = "A" OR "O"
      *	       IF (WS-OPTION = "A")
      *		   IF (STK-QUANT < .01)
      *		       DISPLAY WS-INV78 AT LINE LIN COLUMN 2
      *			       WITH FOREGROUND-COLOR 3
      *				    BACKGROUND-COLOR 1
021400*		       GO TO BC55
      *		   ELSE
      *		       MOVE STK-QUANT
      *				TO W10-QUANT
      *		   END-IF
      *	       ELSE
      *		   PERFORM
      *
030810* 	GO TO BC325.
	     MOVE W10-QUANT	 TO W10-QORD.
	     MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
				 GIVING W10-QNT.
      *    *************************************************************
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *    *************************************************************
       BC66A.
	   IF WS-P1 > 1
	       MULTIPLY STK-UNT(WS-P1) BY STK-AVGE
				 GIVING SAL-COST W10-COST
	   ELSE
021360	       MOVE STK-AVGE	 TO SAL-COST W10-COST.
	     MOVE W90-TAX	 TO W10-ENTRY.
      *    *************************************************************
      *    ****    C H A N G E	 F O R	 M U L T I P L E   R A T E S
      *    *************************************************************
	     MOVE STK-TAX	 TO WS-VAT-SUB.
019040	   IF STK-IND < 3
019050         IF W90-TAX NOT = SPACES
      *    *************************************************************
      *    ****    E X C L U S I V E   P R I C E S
      *    *************************************************************
021330		   IF WS-PRC = "1"
021570		       MOVE STK-SL(WS-P1)
021580                           TO W10-SELL
021360		   ELSE
021370		   IF WS-PRC = "2"
021610		       MOVE STK-CSH(WS-P1)
021620                           TO W10-SELL
021400		   ELSE
021640		       MOVE STK-WSL(WS-P1)
021650                           TO W10-SELL
021430	       ELSE
      *    *************************************************************
      *    ****    I N C L U S I V E   P R I C E S
      *    *************************************************************
021670		   IF WS-PRC = "1"
021680		       MOVE W10-VSL(WS-P1)
021690                           TO W10-SELL
021700             ELSE
021710		   IF WS-PRC = "2"
021720		       MOVE W10-VCSH(WS-P1)
021730                           TO W10-SELL
021740             ELSE
021750		       MOVE W10-VWSL(WS-P1)
021760                           TO W10-SELL.
	     MOVE W10-ENTRY	 TO W90-TAX.

       BC68.
             MOVE W10-SELL       TO W10-SPAPRC.

019270 BC70.
019280       ACCEPT S26-PRC AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC66
	       END-EVALUATE.
	   IF (W10-SELL < W10-SPAPRC) AND
	      ((W10-SPAPRC - W10-SELL) < 0.01)
	       MOVE W10-SELL	 TO W10-SPAPRC.
	   IF W10-SELL < W10-SPAPRC
021180	       DISPLAY CLEAR-L25
               COMPUTE WS-DISC ROUNDED = ((W10-SELL - W10-SPAPRC) / 
                                           W10-SELL) * 100.00000
022110	       DISPLAY "Discount %" AT 2518
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
022190         DISPLAY S26-DSC AT LINE 25
	       DISPLAY "R" AT 2541
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "ekey or "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "ENTER"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       " to continue"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
	       ACCEPT WS-OPTION AT 2568
		      WITH BACKGROUND-COLOR 5
			   FOREGROUND-COLOR 7 HIGHLIGHT AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
022440	       DISPLAY CLEAR-L25
               IF WS-OPTION = "R"
                   GO TO BC70.
	   IF W10-SELL < W10-SPAPRC
021560	       MOVE W10-SELL	 TO W10-SPAPRC
021580	       PERFORM BB100
021590	       IF WS-ERROR = 1
021600		   MOVE ZERO	 TO WS-ERROR
021610		   GO TO BC66A.

019360 BC75.
019370       MOVE W10-SELL       TO SAL-SELL.
      *    *************************************************************
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *    *************************************************************
019410 BC80.
019420	     MOVE W10-QUANT	 TO SAL-QNT.
019420*	     MOVE W10-QORD	 TO SAL-ORD.
	     MOVE WS-P1 	 TO SAL-PACK.
019430       COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
019440       MOVE W25-VALUE      TO W100-EXT SAL-VAL.
019450	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *    *************************************************************
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *    *************************************************************
022440	     DISPLAY CLEAR-L25.
019460	     DISPLAY "Discount %" AT 2518
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
019470       MOVE ZERO           TO WS-DISC W90-CDISC.
      *    *************************************************************
      *    ****   I F	D I S C O U N T	   A P P L I C A B L E
      * 	  G E T   P E R C E N T A G E	F R O M
      * 	  D E P A R T M E N T	F I L E
      *    *************************************************************
019480	   IF (DEB-DISC > 0 AND < 11)
019490	       MOVE STK-LDG	 TO DPT-CODE
019500         PERFORM READ-DEPART THRU READ-DEPART-EXIT
019510	       MOVE DPT-DSETT(DEB-DISC)
019520				 TO WS-DISC
      *    *************************************************************
      *    **  M U S T	 T H E	 A L T E R N A T E   D I S C O U N T  **
      *    **  B E   U S E D   F O R   T H I S	D E B T O R	      **
      *    *************************************************************
	       IF STK-ALT-DISC = "Y"
		   IF DPT-ALT-DD1 = DEB-DISC
		       MOVE DPT-DD11
				 TO DPT-DSETT(DEB-DISC)
				    WS-DISC
		   ELSE
		   IF DPT-ALT-DD2 = DEB-DISC
		       MOVE DPT-DD12
				 TO DPT-DSETT(DEB-DISC)
				    WS-DISC
		   END-IF
	       END-IF.

019530 BC85.
019540       ACCEPT S26-DSC AT LINE 25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
022540		     DISPLAY CLEAR-L25
		     GO TO BC70
	       END-EVALUATE.
019550	   IF WS-DISC = ZERO
022540	       DISPLAY CLEAR-L25
019560         GO TO BC90.
019570       COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
019580              (W10-SELL * WS-DISC / 100.00000).
      *    *************************************************************
      *    ****    C H E C K   M A R G I N
      *    *************************************************************
019590	   IF (DEB-DISC > 0 AND < 11)
019600	       IF DPT-DSETT(DEB-DISC) = WS-DISC
019610             GO TO BC86.
019620       PERFORM BB100.
019630     IF WS-ERROR = 1
019640         GO TO BC85.

019650 BC86.
022540	     DISPLAY CLEAR-L25.
019670       MOVE WS-DISC        TO SAL-DSC.
019680       COMPUTE W90-CDISC ROUNDED = 
                   ((W25-VALUE * WS-DISC) / 100.00000).
019690       MOVE W90-CDISC      TO W100-LBAL SAL-DISC.
019700	     DISPLAY W100-LBAL AT 2540
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
      *    *************************************************************
      *    ****    A R E   T H E   D E T A I L S   C O R R E C T
      *    *************************************************************
019710 BC90.
	     PERFORM CHECK-CORRECT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  As 'PERFORM BC66A THRU BC102' is called from paragraph   ³
      *    ³  BC340, 'WS-LOOP' is set to 'S" so that the routine will  ³
      *    ³  return to 'BC66A' if the details are not correct. After  ³
      *    ³  exiting from this perform 'WS-LOOP' is cleared to SPACE. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
027590     IF WS-OPTION = "N"
	       IF WS-LOOP = "S"
		   GO TO BC66A
	       ELSE
027600		   GO TO BC60.
      *    *************************************************************
      *    ***	  F R E S H   P R O D U C E   -   F O R   S P A R    ***
      *    *************************************************************
	   IF (PRG-NUMBER = 3) AND (STK-LDG = "98  ")
	       MOVE "Y" 	 TO SAL-STATUS W40-SPAR.
      *    *************************************************************
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *    *************************************************************
019820 BC95.
019830	   IF (STK-IND < 2) OR (STK-IND = 3)
019840         COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
019850       ADD W90-CDISC       TO W90-LDISC.
019860       SUBTRACT W90-CDISC FROM W25-VALUE 
019870                           GIVING W90-VALUE.
019880	   IF (STK-TAX = 0) OR (W90-EXP = "X" OR "C")
019890         ADD W90-VALUE     TO W90-NONTAX
019900         MOVE W90-VALUE    TO SAL-NONTAX
	       MOVE ZERO	 TO SAL-VAT-SUB
019910     ELSE
019920         MOVE W90-VALUE    TO SAL-TAXVAL
	       IF W90-EXP = "I"
		   COMPUTE W90-TAXVAL(WS-VAT-SUB) =
			   W90-TAXVAL(WS-VAT-SUB) + (W90-VALUE -
			 ((W90-VALUE / (100.0000 + W05-RATE)) *
			   W05-RATE))
	       ELSE
022890		   ADD W90-VALUE TO W90-TAXVAL(WS-VAT-SUB)
	       END-IF
	       MOVE WS-VAT-SUB	 TO SAL-VAT-SUB.
022910	   IF (W90-TAX NOT = SPACES) AND
	      (W90-EXP NOT = "C" AND "X")
022920         COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
	   IF (W90-EXP = "I") AND (STK-TAX NOT = 0)
	       COMPUTE SAL-VAT ROUNDED = (SAL-TAXVAL /
		      (100.0000 + W05-RATE)) * W05-RATE.
	   IF SAL-VAT-SUB > 0
	       ADD SAL-VAT	 TO W90-VAT(SAL-VAT-SUB).
      *    *************************************************************
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *    *************************************************************
019970       ADD W25-VALUE       TO W90-DB.

019980 BC100.
019990       MOVE W90-DB         TO W90-TOTAL.
020000       SUBTRACT W90-LDISC  FROM W90-TOTAL.
020010       MOVE W90-DB         TO W100-BAL.
020020	     DISPLAY W100-BAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
020030       MULTIPLY -1 BY W90-LDISC 
020040                           GIVING W100-BAL.
020050	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *    *************************************************************
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *    *************************************************************
020090	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
020100         GO TO BC102.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BC101.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BC101.
023100	     MOVE W25-TOTAL	 TO W100-BAL.
023120	     ADD W25-TOTAL	 TO W90-TOTAL.
023110	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

020180 BC102.
022780	     MOVE W90-TOTAL	 TO W100-BAL.
022790	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

020250 BC105.
036100	     MOVE "'A'ccept or 'C'ancel   [A]"
				 TO WS-ERR-MES.
023210	     MOVE "A"		 TO WS-OPTION.
      *    *************************************************************
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *    *************************************************************
020310 BC110.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "A" OR "C".
020420     IF WS-OPTION = "C"
020430	       DISPLAY WS-INV78 AT LINE LIN COLUMN 2
		       WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
020440         PERFORM BC114
020450         PERFORM BC100 THRU BC102
020460         GO TO BC55.               

020470 BC111.
             MOVE WS-SUPER       TO SAL-SUPER.
020480       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
020490       ADD 1               TO WS-LAST.
      *    *************************************************************
      *    ****    T E S T   F O R   C U T T I N G   S P E C S
      *    *************************************************************
	   IF W10-LENGTH > ZERO AND
	      W10-BREADTH > ZERO AND
	      W10-NUMBER > ZERO
023410	       IF LIN < 18
023420		   ADD 1	 TO LIN
	       ELSE
023440		   MOVE 8	 TO WS-S7
023450		   PERFORM BC111-SHIFT
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
023710	       DISPLAY W10-REMKS AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0 UPDATE AUTO
023400	       ADD 1		 TO WS-LAST.
      *    *************************************************************
      *    ****    T E S T   I F   S C R E E N   F U L L
      *    *************************************************************
020500     IF LIN < 18
020510         ADD 1             TO LIN
020520         GO TO BC112.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO WS-S7.
      *    *************************************************************
      *    ****    S C R O L L	 D E T A I L   L I N E S   U P
      *    *************************************************************
       BC111-SHIFT.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES(WS-S8)
				 TO W42-LINES(WS-S7).
	     MOVE W43-LINE(WS-S8)
				 TO W43-LINE(WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BC111-SHIFT.
	     PERFORM RESTORE-SCREEN-2.
      *    *************************************************************
      *    ****    C L E A R   L A S T	 D E T A I L   L I N E
      *    *************************************************************
023890	     DISPLAY WS-INV1 AT LINE LIN COLUMN 1
		     WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3
		     WS-INV78 WITH FOREGROUND-COLOR 3
		     WS-INV80 WITH BACKGROUND-COLOR 0
				   FOREGROUND-COLOR 3.
	     ADD 1		 TO WS-SALPOS.

020560 BC112.
	     MOVE SPACES     TO WS-ERR-MES.
036410	     STRING "'I'nternal notes,'R'emarks,'N'ext or 'E'nd  ["
		    DELIMITED SIZE
		    WS-IN-OPT DELIMITED SIZE
		    "]" DELIMITED SIZE
				 INTO WS-ERR-MES.
023180	     MOVE WS-IN-OPT	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "I" OR "R" OR "N" OR "E".
             MOVE WS-OPTION      TO WS-IN-OPT.
020740     IF WS-OPTION = "N"
020750         GO TO BC55.
020760     IF WS-OPTION = "E"
020770         SUBTRACT 1        FROM WS-LAST
020780         GO TO BC120.
020790       MOVE SPACES         TO W10-REMKS.
020800	     ACCEPT W10-REMKS AT LINE LIN COLUMN 21
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020810			 BACKGROUND-COLOR 0 UPDATE AUTO.
020820     IF W10-REMKS = SPACES
020830         GO TO BC112.
020850	     MOVE WS-LAST	 TO WS-SALKEY.
020860       INITIALIZE SAL-RECORD1.
020870       MOVE WS-LAST        TO SAL-CODE.
020880       MOVE W10-REMKS      TO SAL-MSER.
           IF WS-OPTION = "I"
	       DISPLAY "(I)" AT LINE LIN COLUMN 5
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
               MOVE 1            TO SAL-SUPER
           ELSE
	       DISPLAY "(R)" AT LINE LIN COLUMN 5
			WITH FOREGROUND-COLOR 7 HIGHLIGHT.
020890       GO TO BC111.

020900 BC114.
020910       SUBTRACT W25-VALUE  FROM W90-DB.
020920       SUBTRACT W90-CDISC  FROM W90-LDISC.
020930       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF SAL-VAT-SUB > ZERO
036780	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(SAL-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(SAL-VAT-SUB).
019830	   IF (STK-IND < 2) OR (STK-IND = 3)
020960         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****   C L E A R   S C R E E N   L I N E / S
      *    *************************************************************
020970 BC115.
036820	     DISPLAY WS-INV1 AT LINE LIN COLUMN 1
		     WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3
		     WS-INV78 WITH FOREGROUND-COLOR 3
		     WS-INV80 WITH BACKGROUND-COLOR 0
				   FOREGROUND-COLOR 3.
020990	     ADD 1		 TO LIN.
      *    *************************************************************
      *    ****    W A S   T H E R E   A N   E N T R Y
      *    *************************************************************
021000 BC120.
021010	   IF WS-LAST = ZERO
021020         GO TO BC05.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   S A L E
      *    *************************************************************
021030 BC125.
036900	     MOVE "'P'rocess, 'A'mend, 'C'ancel or 'M'ore items"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

021130 BC130.
	     PERFORM OPT-MESSAGE
		     UNTIL WS-OPTION = "P" OR "A" OR "C" OR "M".
021230     IF WS-OPTION = "M"
021240         ADD 1             TO WS-LAST
021250         GO TO BC112.
021260     IF WS-OPTION = "A"
021270	       PERFORM BC270A
021280         GO TO BC125.
021290     IF WS-OPTION = "C"
021300	       GO TO BC265.
021310       MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
021320     IF W90-LDISC NOT = ZERO
021330         MOVE W90-LDISC    TO W90-TDISC
021340         GO TO BC140.
021350       MOVE W90-DB         TO W90-DVAL.

021360 BC140.
021370       SUBTRACT W90-TDISC FROM W90-DB 
021380                           GIVING W90-TSELL W90-TOTAL.
021390       MULTIPLY -1 BY W90-TDISC 
021400                           GIVING W100-BAL.
021410	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
021450	     MOVE ZERO		 TO W90-TTAX
				    W90-EXCTOT.
	     MOVE 1		 TO WS-P1.
021460	   IF (W90-TAX = SPACES) OR (W90-EXP = "C" OR "X")
021470	       GO TO BC143.
      *    *************************************************************
      *    ****    C A L C U L A T E   T O T A L   V A T
      *    *************************************************************
       BC141.
037540	   IF (W90-TAXVAL(WS-P1) = ZERO)
024430	       GO TO BC142.
024440	     MOVE W05-VAT(WS-P1) TO W05-RATE.
021490	     COMPUTE W90-TTAX ROUNDED =
		    (W90-TAXVAL(WS-P1) * W05-RTE).
	     ADD W90-TAXVAL(WS-P1)
				 TO W90-EXCTOT.
      *    *************************************************************
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      * 	   H A V E   B E E N   C A L C U L A T E D
      *    *************************************************************
       BC142.
	   IF WS-P1 < 6
	       ADD 1		 TO WS-P1
	       GO TO BC141.
024290	     MOVE W90-TTAX	 TO W100-BAL.
024300	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
024310       ADD W90-TTAX        TO W90-TOTAL.

       BC143.
021590	     MOVE W90-TOTAL	 TO W100-BAL.
021600	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
021610	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   O R D E R
      *    *************************************************************
021620 BC145.
021630      DISPLAY "Processing Order" AT 2520 WITH
021640		     BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
021650       MOVE ZERO           TO WS-SALKEY.
	     GO TO BC216.
      *    *************************************************************
      *    ****    T E S T   F O R   I N D I V I D U A L
      *    *************************************************************
       BC189.
021730     IF W80-TYPE NOT = 1
021740         GO TO BC210.
021750       MOVE DEB-NAME       TO W60-NAME.
021760       PERFORM CA165-NAME.
      *    *************************************************************
      *    ****    T I T L E
      *    *************************************************************
021770     IF DEB-LANG = "A"
021780	       MOVE W75-ATITLE(DEB-TITLE)
021790                           TO W60-NAME
021800     ELSE
021810	       MOVE W75-ETITLE(DEB-TITLE)
021820                           TO W60-NAME.
021830       MOVE 7              TO WS-S6.

021840 BC190.
021850	   IF W60-NCHAR(WS-S6) NOT = SPACE
021860         GO TO BC195.
021870       SUBTRACT 1          FROM WS-S6.
021880     IF WS-S6 > 0
021890         GO TO BC190.
      *    *************************************************************
      *    ****    F I R S T   I N I T I A L
      *    *************************************************************
021900 BC195.
021910	     MOVE 1		 TO WS-S5.
021920       ADD 2               TO WS-S6.
021930	     MOVE W40-NCHAR(25) TO W60-NCHAR(WS-S6).
021940       ADD 1               TO WS-S6.
021950	     MOVE "."		 TO W60-NCHAR(WS-S6).
021960	   IF W40-NCHAR(49) = SPACE
021970         GO TO BC200.
      *    *************************************************************
      *    ****    S E C O N D   I N I T I A L
      *    *************************************************************
021980       ADD 1               TO WS-S6.
021990	     MOVE W40-NCHAR(49) TO W60-NCHAR(WS-S6).
022000       ADD 1               TO WS-S6.
022010	     MOVE "."		 TO W60-NCHAR(WS-S6).

022020 BC200.
022030       ADD 2               TO WS-S6.
      *    *************************************************************
      *    ****    S U R N A M E
      *    *************************************************************
022040 BC205.
022050	     MOVE W40-NCHAR(WS-S5)
022060				 TO W60-NCHAR(WS-S6).
022070     IF WS-S5 < 24
022080         ADD 1             TO WS-S5
022090         ADD 1             TO WS-S6
022100         GO TO BC205.
022110       MOVE W60-NAME       TO W40-NAME.

       BC210.
025970	     MOVE ZERO		 TO WS-PAGE.
025980       MOVE SPACES         TO W40-PC1 W40-PC2.
025990       MOVE DEB-ADDRESS    TO W60-ADDRESS.
026000	     PERFORM CA175-ADDRESS.
026010	   IF NOT(DEB-POST = ZERO OR 9999)
026020         MOVE DEB-POST     TO W40-PC1.
026030     IF W40-ADD4 = SPACES
026040         MOVE W40-PC1      TO W40-ADD4
026050         MOVE SPACES       TO W40-PC1.
026060     IF W40-ADD3 = SPACES
026070         MOVE W40-ADD4     TO W40-ADD3
026080         MOVE SPACES       TO W40-ADD4.
026090     IF W40-ADD2 = SPACES
026100         MOVE W40-ADD3     TO W40-ADD2
026110         MOVE SPACES       TO W40-ADD3.
026120     IF W40-ADD1 = SPACES
026130         MOVE W40-ADD2     TO W40-ADD1
026140         MOVE SPACES       TO W40-ADD2.
026150       MOVE DEB-PADDR      TO W60-ADDRESS1.
026160       PERFORM CA185-ADDRESS.
026170	   IF NOT(DEB-PPOST = ZERO OR 9999)
026180         MOVE DEB-PPOST    TO W40-PC2.
026190     IF W40-PADD4 = SPACES
026200         MOVE W40-PC2      TO W40-PADD4
026210         MOVE SPACES       TO W40-PC2.
026220     IF W40-PADD3 = SPACES
026230         MOVE W40-PADD4    TO W40-PADD3
026240         MOVE SPACES       TO W40-PADD4.
026250     IF W40-PADD2 = SPACES
026260         MOVE W40-PADD3    TO W40-PADD2
026270         MOVE SPACES       TO W40-PADD3.
026280     IF W40-PADD1 = SPACES
026290         MOVE W40-PADD2    TO W40-PADD1
026300         MOVE SPACES       TO W40-PADD2.
       BC215.
	       EXIT.
      *    *************************************************************
      *    ****    S T A R T   R E C O V E R Y   L O G G I N G
      *    *************************************************************
       BC216.
021660	     PERFORM AY60 THRU AY999.
	   IF W70-ORD-SYS = "Y"
	       MOVE LS0-NO	 TO USE-NO
	       PERFORM READ-SYSUSER-LOCK THRU READ-SYSUSER-EXIT
	       MOVE USE-NO	 TO W90-USER
	       MOVE USE-ORD	 TO W90-SEQ
	   ELSE
025250	       MOVE W70-ORDER	 TO W90-REF.
025260	     DISPLAY W90-REF AT 0510
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF W70-ORD-SYS = "Y"
	       IF USE-ORD = 99999
		   MOVE 1	 TO USE-ORD
	       ELSE
		   ADD 1	 TO USE-ORD
	       END-IF
	       PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT
	   ELSE
025270	   IF W70-ORDER = 99999999
025280	       MOVE 1		 TO W70-ORDER
025290	   ELSE
025300	       ADD 1		 TO W70-ORDER.
      *    *************************************************************
      *    ****    S T A R T   O R D E R   L O G G I N G 
      *    *************************************************************
022120 BC220.
022130	     MOVE W90-REF	 TO SOR-REF.
022140       MOVE 100            TO SOR-SQN.
022150       MOVE ZERO           TO SOR-TYPE.
022160       MOVE W80-ACNO       TO SOR-AC.
022170       MOVE W40-NAME       TO SOR-NAME.
022180       MOVE W12-T-YMD      TO SOR-DTE.
022190       MOVE ZERO           TO SOR-PICNO.
022200       MOVE W90-ORD        TO SOR-ORD.
022210       MOVE W10-SMAN       TO SOR-SMAN.
022220       MOVE "O"            TO SOR-DOC.
022230       MOVE ZERO           TO SOR-PAGE.
022240       MOVE W90-REG        TO SOR-OREF.
022250       MOVE W90-EXP        TO SOR-EXP.
022270       MOVE WS-PRC         TO SOR-PRC.
             MOVE W40-TEL        TO SOR-TEL.
             MOVE W40-CONTACT    TO SOR-CONTACT.
           IF W40-AD1 NOT = SPACES
               PERFORM BC225
               MOVE 5            TO SOR-TYPE
               ADD 1             TO SOR-SEQ
               MOVE W40-AD1      TO SOR-AD1
               MOVE W40-AD2      TO SOR-AD2
               MOVE W40-AD3      TO SOR-AD3 
               MOVE W40-AD4      TO SOR-AD4
               MOVE W40-PC       TO SOR-PC.
      *    *************************************************************
      *    ****    L O G   R E C O R D   T O   O R D E R   F I L E
      *    *************************************************************
022280 BC225.
022290       PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.
022300       MOVE 1              TO WS-ACTION.
022310       PERFORM AY17 THRU AY59.
      *    *************************************************************
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *    *************************************************************
022320 BC235.
022330	     ADD 1		 TO WS-SALKEY.
022340     IF WS-SALKEY > WS-LAST
022350         GO TO BC255.
022360       PERFORM READ-SALE THRU READ-SALE-EXIT.
022370	   IF SAL-EXT-ITEM = SPACES
022380         GO TO BC247.
022410       MOVE 1              TO SOR-TYPE.
022420	     MOVE SAL-EXT-ITEM	 TO SOR-EXT-ITEM.
022430       MOVE SAL-DESC       TO SOR-DESC.
022440	     MOVE SAL-QNT	 TO SOR-QNT.
      *				    SOR-QORD.
022450       MOVE SAL-COST       TO SOR-COST.
022460       MOVE SAL-VAT        TO SOR-VAT.
015000	     MOVE SAL-VAT-SUB	 TO SOR-VAT-SUB.
022470       MOVE SAL-SELL       TO SOR-SELL.
022480       MOVE SAL-VAL        TO SOR-VAL.
022490       MOVE SAL-DISC       TO SOR-DISC.
022500       MOVE SAL-DSC        TO SOR-DSC.
022510       MOVE SAL-INCTAX     TO SOR-INCTAX.
022520       MOVE SAL-NONTAX     TO SOR-NONTAX.
022530       MOVE SAL-TAXVAL     TO SOR-TAXVAL.
	     MOVE SAL-PACK	 TO SOR-PACK.
022540       MOVE SPACES         TO SOR-SERNO.
022550       ADD 1               TO SOR-SEQ.
	     PERFORM BC225.
      *    *************************************************************
      *    ****    U P D A T E	 P I C K I N G	 S L I P   Q T Y
      * 	   O N	 S T O C K   R E C O R D
      *    *************************************************************
024920	     MOVE SAL-EXT-ITEM	 TO STK-CODE.
	     MOVE 0		 TO WS-ACTION.
	     PERFORM AY05 THRU AY59.
	     SUBTRACT SOR-QNT	 FROM STK-QUANT.
	     ADD SOR-QNT	 TO STK-QPIC.
017190	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
	     PERFORM BC248 THRU BC254.
022560       GO TO BC235.

022570 BC247.
           IF SAL-SUPER = 1
               MOVE 4            TO SOR-TYPE
           ELSE
               MOVE 3            TO SOR-TYPE.
022590       INITIALIZE SOR-DET2.
022600       MOVE SAL-MSER       TO SOR-SERIAL.
022610       ADD 1               TO SOR-SEQ.
022620       GO TO BC225.
       BC248.
	     MOVE SOR-EXT-ITEM	 TO PRT-EXT-ITEM.
020530	     PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510     IF WS-F-ERROR = 21
               GO TO BC254.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
021370       MOVE 1              TO WS-S6.
021420 BC249.
021430	   IF PRT-EXT-CODE(WS-S6) = SPACES
021440         ADD 1             TO WS-S6
021450         GO TO BC250.
021470	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W10-EXT-ITEM.
021490       PERFORM CA000.
021500     IF WS-F-ERROR = 22
021510         ADD 1             TO WS-S6
021520         GO TO BC250.
022580       MOVE 2              TO SOR-TYPE.
022590       INITIALIZE SOR-DET2.
	     MOVE STK-CODE	 TO SOR-EXT-ITEM.
021540       MOVE STK-DESC       TO SOR-DESC.
	     MULTIPLY SAL-QNT BY PRT-QUANT(WS-S6)
                                 GIVING SOR-QNT.
022610       ADD 1               TO SOR-SEQ.
	     PERFORM BC225.
021600	     ADD 1		 TO WS-S6.
021660 BC250.
021670     IF WS-S6 < 31
021680         GO TO BC249.
       BC254.
             EXIT.
      *    *************************************************************
      *    ****    O R D E R   T O T A L S
      *    *************************************************************
022630 BC255.
022640	     INITIALIZE SOR-DET3.
022650       MOVE 9              TO SOR-TYPE.
022660       MOVE W90-DB         TO SOR-SUBTOT.
022670       MULTIPLY -1 BY W90-TDISC 
022680                           GIVING SOR-DSCNT
022690	     MOVE W90-EXCTOT	 TO SOR-TAXABLE.
022700       MOVE W90-NONTAX     TO SOR-NON-TXBL.
022710       MOVE W90-TTAX       TO SOR-TAX.
022720       MOVE W90-TOTAL      TO SOR-TOT.
022730       MOVE W90-BAL        TO SOR-BAL.
	     MOVE W90-PMETH	 TO SOR-PMETH.
             MOVE W40-STATUS     TO SOR-STATUS.
             MOVE W40-HTEL       TO SOR-HTEL.
             MOVE W40-ID         TO SOR-ID.
022750	     MOVE 99900 	 TO SOR-SQN.
             MOVE W90-INSTRUCT   TO SOR-INSTRUCT.
022760       PERFORM BC225.
      *    *************************************************************
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *    *************************************************************
022770       PERFORM AY70 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ To avoid problems of having data corrupted (MULTI USER)   ³
      *    ³ by the cache (buffering) of data on the individual Work   ³
      *    ³ stations. This results in data from one work station not  ³
      *    ³ being available on another Work station until the cache   ³
      *    ³ has been ceared. Including a close of the file after each ³
      *    ³ new order has been added and then re-opening the file is  ³
      *    ³ the way in which the system will release the data for it  ³
      *    ³ to be written to the server file and should resolve this  ³
      *    ³ problem.						       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     CLOSE SORDER.
	     OPEN I-O SORDER.

022880 BC260.
022810	     MOVE "Print picking slip (Y/N)?  [ ]"
				 TO WS-ERR-MES.
022870	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "Y" OR "N".
022980     IF WS-OPTION = "Y"
022990         DISPLAY CLR-SCREEN
023000	       DISPLAY "NEW ORDER" AT 0236
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
023010         DISPLAY "Printing picking slip" AT 1212
023020			WITH FOREGROUND-COLOR 6 HIGHLIGHT
023030	       MOVE W90-REF	 TO LS-SLS-ORDNO
               PERFORM BB200.
      *    *************************************************************
      *    ****    C L O S E   F I L E S   
      * 	   ( CLEAR THE WORKING FILE FOR NEXT TRANSACTION )
      *    *************************************************************
022780	     CLOSE SALE.
023170       OPEN OUTPUT SALE.
023180       CLOSE SALE.
023190	     OPEN I-O SALE.
029700	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
023320	   IF WS-OPTION = "E"
023330	       GO TO BC265.
023340       GO TO BC05.

       BC265.
	     EXIT.
      *    *************************************************************
      *    ****    A M E N D   S A L E S   D E T A I L S
      *    *************************************************************
       BC270A		SECTION 50.
023350 BC270.
	     MOVE "Amend - 'H'eader, 'S'ales details or 'E'xit"
				 TO WS-ERR-MES.
	     MOVE "S"		 TO WS-OPTION.

       BC271.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "E" OR "H" OR "S".
	   IF WS-OPTION = "S"
               GO TO BC272
           ELSE
           IF WS-OPTION = "E"
	       GO TO BC582.

       BC271A.
	     MOVE "Amend-'O'rd No,'S'alesman,'T'el.,'C'ont.,'E'xit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

       BC271B.
	     PERFORM OPT-MESSAGE
		     UNTIL WS-OPTION = "C" OR "E" OR "O" OR "S" OR "T".
           IF WS-OPTION = "E"
	       GO TO BC582.
           IF WS-OPTION = "C"
               ACCEPT W40-CONTACT AT 0528 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
               GO TO BC271A
           ELSE
           IF WS-OPTION = "O"
017660         ACCEPT W90-ORD AT 0368 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
               GO TO BC271A
           ELSE
           IF WS-OPTION = "T"
               ACCEPT W40-TEL AT 0466 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
               GO TO BC271A.

       BC271C.
017700	     ACCEPT W10-SMAN AT 0411
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
017710			 BACKGROUND-COLOR 5 UPDATE AUTO.
017720     IF W10-SMAN > 50
017730         GO TO BC271C.
017740     IF W10-SMAN = ZERO
	       DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 0420
                       WITH FOREGROUND-COLOR 3
017750         GO TO BC271A.
017820	     DISPLAY CLEAR-L25.
017760       ADD 100 W10-SMAN    GIVING WS-PARKEY.
017770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017780     IF PAR-SALESMAN = SPACES
017790	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017810	       GO TO BC271C.
017830	     DISPLAY PAR-SALESMAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE W10-SMAN	 TO SOR-SMAN.
             PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
	     GO TO BC271A.

       BC272.
023360       MOVE 8              TO LIN.
023370       PERFORM BC115 UNTIL LIN > 18.
023380       MOVE 8              TO LIN.
023390       MOVE 1              TO WS-LINES WS-FIRST.

023400 BC275.
023410	     MOVE WS-FIRST	 TO WS-SALKEY WS-SALPOS.
      *    *************************************************************
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *    *************************************************************
023420 BC280.
023430	   IF WS-SALKEY > WS-LAST
023440	       GO TO BC284.
023450       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
023470	   IF SAL-EXT-ITEM = SPACES
023480	       DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0
               IF SAL-SUPER = 1
		   DISPLAY "(I)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 14
               ELSE
		   DISPLAY "(R)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 7 HIGHLIGHT
               END-IF
023490         GO TO BC282.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
023510	     DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
023520       MOVE SAL-QNT        TO W10-QUANT.
      *	     MOVE SAL-ORD	 TO W10-QORD.
023530       DISPLAY S26-QNT AT LINE LIN.
023540       MOVE SAL-SELL       TO W10-SELL.
023550       DISPLAY S26-PRC AT LINE LIN.
023560       MOVE SAL-VAL        TO W100-EXT.
023570	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *    *************************************************************
      *    ****    I S   T H E   S C R E E N   F U L L
      *    *************************************************************
023580 BC282.
023590     IF LIN < 18
023600         ADD 1             TO LIN WS-SALKEY WS-LINES
023610         GO TO BC280.

       BC284.
	     MOVE 7		 TO LIN.
	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
030250 BC285.
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
      *    *************************************************************
      *    ****    S A V E   C U R R E N T   P A G E
      *    *************************************************************
	     PERFORM SAVE-SCREEN-2.
      *    *************************************************************
      *    ****    S C R E E N	 A T T R I B U T E S
      *    *************************************************************
       BC289.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.
      *    *************************************************************
      *    ****    G E T   U S E R   O P T I O N
      *    *************************************************************
030520 BC290.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BC292
		 WHEN DOWN-KEY
001340		     GO TO BC293
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BC290
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO BC582
      *    *************************************************************
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-DOWN
001340		     GO TO BC305
      *    *************************************************************
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-UP
001340		     GO TO BC300
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BC290
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION = "A" OR "D") AND
		  (LIN > 7)
		   DISPLAY CLEAR-L25
		   GO TO BC310.
	     CALL X"E5".
001280	     GO TO BC290.
      *    *************************************************************
      *    ****    U P	 K E Y	 ( M O V E   H I G H L I G H T	 U P )
      *    *************************************************************
       BC292.
	   IF (WS-SALPOS = 1) OR (LIN < 8)
	       GO TO BC294.
	     SUBTRACT 1 	 FROM WS-SALPOS.
	   IF LIN > 8
	       SUBTRACT 1	 FROM LIN
	       GO TO BC294.
	     MOVE 18		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      * 	    O N E   L I N E
      *    *************************************************************
       BC292-LOOP.
	     SUBTRACT 1 FROM WS-S7
				 GIVING WS-S8.
	     MOVE W42-LINES(WS-S8)
				 TO W42-LINES(WS-S7).
	     MOVE W43-LINE(WS-S8)
				 TO W43-LINE(WS-S7).
	   IF WS-S7 > 9
	       SUBTRACT 1	 FROM WS-S7
	       GO TO BC292-LOOP.
	     MOVE WS-SALPOS	 TO WS-SALKEY WS-FIRST.
      *    *************************************************************
      *    ****    C L E A R   F I R S T   O R	 L A S T
      * 	   D E T A I L	 L I N E.
      *    *************************************************************
       BC292-DISPLAY.
	     PERFORM RESTORE-SCREEN-2.
023890	     DISPLAY WS-INV1 AT LINE LIN COLUMN 1
		     WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3
		     WS-INV78 WITH FOREGROUND-COLOR 3
		     WS-INV80 WITH BACKGROUND-COLOR 0
				   FOREGROUND-COLOR 3.
      *    *************************************************************
      *    ****    G E T   N E X T   O R   P R E V I O U S
      * 	   D E T A I L	 L I N E.
      *    *************************************************************
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0
	       IF SAL-SUPER = 1
		   DISPLAY "(I)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 6 HIGHLIGHT
               ELSE
		   DISPLAY "(R)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 7 HIGHLIGHT
	       END-IF
030120	       GO TO BC292-END-DISP.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
030140	     DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
030150       MOVE SAL-QNT        TO W10-QUANT.
      *	     MOVE SAL-ORD	 TO W10-QORD.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.

       BC292-END-DISP.
	     PERFORM SAVE-SCREEN-2.
	     GO TO BC294.
      *    *************************************************************
      *    ****    D O W N   K E Y   ( M O V E	 H I G H L I G H T
      * 			       D O W N )
      *    *************************************************************
       BC293.
	   IF (WS-SALPOS = WS-LAST) AND (LIN > 7)
	       GO TO BC294.
      *    *************************************************************
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *    *************************************************************
	   IF LIN > 7
	       ADD 1		 TO WS-SALPOS.
	   IF LIN < 18
	       ADD 1		 TO LIN
	       GO TO BC294.
	     MOVE 8		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      * 	    L I N E
      *    *************************************************************
       BC293-LOOP.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES(WS-S8)
				 TO W42-LINES(WS-S7).
	     MOVE W43-LINE(WS-S8)
				 TO W43-LINE(WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BC293-LOOP.
	     ADD 1		 TO WS-FIRST.
	     MOVE WS-SALPOS	 TO WS-SALKEY.
	     GO TO BC292-DISPLAY.
      *    *************************************************************
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *    *************************************************************
001980 BC294.
001990	     PERFORM BC289.
	   IF LIN > 7
002000	       DISPLAY W43-DET(LIN) AT LINE LIN COLUMN 1
		       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
002010	     GO TO BC290.
      *    *************************************************************
      *    ****    C L E A R   S C R E E N
      *    *************************************************************
030780 BC295.
030790	     MOVE 8		 TO LIN.
030800       PERFORM BC115 UNTIL LIN > 18.
030810	     MOVE 8		 TO LIN.
030820       MOVE 1              TO WS-LINES.
      *    *************************************************************
      *    ****   P R E V I O U S   S C R E E N	  R E Q U I R E D
      *    *************************************************************
       BC300.
	   IF WS-FIRST = 1
	       GO TO BC290.
	   IF WS-FIRST > 11
	       SUBTRACT 11	 FROM WS-FIRST
	   ELSE
	       MOVE 1		 TO WS-FIRST.
	     PERFORM BC295.
	     GO TO BC275.
      *    *************************************************************
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
       BC305.
	   IF (WS-FIRST + 11) > WS-LAST
	       GO TO BC290.
	     ADD 11		 TO WS-FIRST.
	     PERFORM BC295.
	     GO TO BC275.
      *    *************************************************************
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *    *************************************************************
       BC310.
030630	   IF WS-OPTION = "A"
024490	       GO TO BC320.
      *    *************************************************************
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *    *************************************************************
024530 BC315.
043780	     MOVE "Are you sure Y/N?  [ ]"
				 TO WS-ERR-MES.
030880	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE
		     UNTIL WS-OPTION = "N" OR "Y".
024640     IF WS-OPTION = "N"
024660         GO TO BC285.
024670       MOVE "D"            TO WS-OPTION.
      *    *************************************************************
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *    *************************************************************
       BC320.
031220	     MOVE WS-SALPOS	 TO WS-SALKEY.
031230       PERFORM READ-SALE THRU READ-SALE-EXIT.
030370	   IF SAL-EXT-ITEM = SPACES
031250	       GO TO BC322.
024920	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM STK-CODE.
024930       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
024940       MOVE SAL-QNT        TO W10-QUANT.
      *	     MOVE SAL-ORD	 TO W10-QORD.
024950       MOVE SAL-COST       TO W10-COST.
024960       MOVE SAL-SELL       TO W10-SELL.
024970       MOVE SAL-DISC       TO W90-CDISC.
024980       MOVE SAL-DSC        TO WS-DISC.
024990       MOVE SAL-VAL        TO W25-VALUE.
	     MOVE SAL-VAT-SUB	 TO WS-VAT-SUB.
	     MOVE SAL-PACK	 TO WS-P1.
      *    *************************************************************
      *    ****    R E D U C E   T O T A L S 
      *    *************************************************************
025000       SUBTRACT W25-VALUE  FROM W90-DB.
025010       SUBTRACT W90-CDISC  FROM W90-LDISC.
025020       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF WS-VAT-SUB > ZERO
031370	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(SAL-VAT-SUB).
019830	   IF (STK-IND < 2) OR (STK-IND = 3)
025050         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****    I S   I T   A N   A M E N D M E N T
      *    *************************************************************
025060 BC322.
	     PERFORM BC289.
025070     IF WS-OPTION = "A"
025080         GO TO BC325.
      *    *************************************************************
      *    ****    R E - D I S P L A Y   T O T A L S
      *    *************************************************************
025090	   IF SAL-EXT-ITEM NOT = SPACES
025100         PERFORM BC100 THRU BC102.
      *    *************************************************************
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *    *************************************************************
030580 BC324.
025120	     MOVE WS-SALKEY	 TO WS-SALSTR.
025130       ADD 1               TO WS-SALKEY.
025140     IF WS-SALKEY > WS-LAST
025150         SUBTRACT 1        FROM WS-LAST
025160         GO TO BC272.
025170       PERFORM READ-SALE THRU READ-SALE-EXIT.
025180       SUBTRACT 1          FROM SAL-CODE.
025190       MOVE WS-SALSTR      TO WS-SALKEY.
025200       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
025210       ADD 1               TO WS-SALKEY.
025220	     GO TO BC324.

025230 BC325.
	     PERFORM BC289.
025240	   IF SAL-EXT-ITEM = SPACES
030720	       ACCEPT SAL-SDSC AT LINE LIN COLUMN 21
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
025260			   BACKGROUND-COLOR 0 UPDATE AUTO
030740	       GO TO BC581.

       BC330.
020730	     PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       ACCEPT S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *        ***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT S07-ITM AT LINE LIN
	       ELSE
021070		   ACCEPT S07 AT LINE LIN
	       END-IF
	   ELSE
      *    *************************************************************
      *    ***** NUMERIC CODES APPLY *****
      *    *************************************************************
	   IF W10-SLNGTH = 3
	       ACCEPT S07-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S07-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S07-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S07-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S07-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S07-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S07-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S07-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S07-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S07-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S07-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S07-14 AT LINE LIN.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *	   ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BC330
               END-EVALUATE
020770	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN.
020790	       IF W10-EXT-ITEM = SPACES
		   MOVE SAL-EXT-ITEM
				 TO W10-EXT-ITEM
020800		   GO TO BC330.

       BC335.
020840       PERFORM CA000.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
020850	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN
		   END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
		   MOVE SAL-EXT-ITEM
				 TO W10-EXT-ITEM
020800		   GO TO BC330
               ELSE
		   GO TO BC335.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC330.
      *    *************************************************************
      *    **** C H E C K   F O R   S A L E S	R E S T R I C T I O N S
      *    *************************************************************
	   IF WS-RESTRICT = "A" OR "E"
	       PERFORM MA000
	       IF ((WS-RESTCODE = "Y") AND (WS-RESTRICT = "E")) OR
		  ((WS-RESTCODE = "N") AND (WS-RESTRICT = "A"))
		   MOVE "Restricted for this Department"
                                 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
020140		   GO TO BC330.
020920	     MOVE STK-CODE	 TO SAL-EXT-ITEM.
020930	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
020940 BC340.
020950     IF STK-IND = 3
020960         MOVE ZERO         TO STK-AVGE
020970	       ACCEPT W10-SDSC AT LINE LIN COLUMN 21
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
020980			   BACKGROUND-COLOR 0 UPDATE AUTO
020990         DISPLAY "Index Unit cost" AT 2512
021000			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
021010         ACCEPT S26-AVGE AT LINE 25
021020     ELSE
021030         MOVE STK-DESC     TO W10-DESC
021040	       DISPLAY W10-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0.
021050     IF STK-IND = 3
021060         IF STK-AVGE = ZERO
021070		   MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
021090		   GO TO BC340.
021100	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
021110	     MOVE W10-EXT-ITEM	 TO SAL-EXT-ITEM.
021120       MOVE W10-DESC       TO SAL-MSER.
021130       MOVE STK-AVGE       TO SAL-COST W10-COST.
      *    *************************************************************
      *    ****    A M E N D   Q U A N T I T Y
      *    *************************************************************
030750       ACCEPT S26-QNT AT LINE LIN.
030770     IF W10-QUANT = ZERO
030780	       MOVE "Quantity MAY NOT be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
030800         MOVE SAL-QNT      TO W10-QUANT
030810         GO TO BC325.
      *	     MOVE W10-QUANT	 TO W10-QORD.
      *    *************************************************************
      *    ****    A M E N D   S E L L I N G   P R I C E
      *    *************************************************************
	     MOVE "S"		 TO WS-LOOP.
	     PERFORM BC66A THRU BC102.
	     MOVE " "		 TO WS-LOOP.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *
      *    The following code has been commented out, as the perform has
      *    been extended to include paragraphs up to BC102. The variable
      *    WS-LOOP has been included to facilitate this. This has been
      *    done to remove duplicated code, which will also reduce bugs
      *    when changes are made to the system.
      *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *    *************************************************************
      *    ****    R E C A L C U L A T E   V A L U E S
      *    *************************************************************
030820*      MULTIPLY W10-SELL BY W10-QUANT
      * 			 GIVING W25-VALUE.
030830*      MOVE W25-VALUE	 TO W100-EXT.
030840*     DISPLAY (LIN, 70) W100-EXT
      *	      WITH FOREGROUND-COLOR 3 HIGHLIGHT
      *		   BACKGROUND-COLOR 0.
      *     PERFORM CHECK-CORRECT.
030930*   IF WS-OPTION = "N"
030940*        GO TO BC325.
      *    *************************************************************
      *    ****    U P D A T E	 S A L E   F I L E   R E C O R D    ****
      *    *************************************************************
030950*      MOVE W10-QUANT	 TO SAL-QNT.
030950*	     MOVE W10-QORD	 TO SAL-ORD.
      *      MOVE W10-SELL	 TO SAL-SELL.
030960*      MOVE W25-VALUE	 TO SAL-VAL.
030970*    IF WS-DISC = ZERO
030980*       GO TO BC580.
030990*      COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
031000*      MOVE W90-CDISC	 TO SAL-DISC.
      *    *************************************************************
      *    * R E - C A L C U L A T E   &   D I S P L A Y   T O T A L S *
      *    *************************************************************
031010 BC580.
031020*     PERFORM BC95 THRU BC102.

031030 BC581.
031040       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
031050       GO TO BC272.
      *    *************************************************************
      *    ****    C L E A R   D E T A I L   L I N E S		    ****
      *    *************************************************************
031940 BC582.
	     PERFORM BC295.
      *    *************************************************************
      *    ****    D I S P L A Y   L A S T   P A G E		    ****
      *    *************************************************************
	   IF WS-LAST > 10
	       SUBTRACT 9 FROM WS-LAST
				 GIVING WS-SALKEY
	   ELSE
	       MOVE 1		 TO WS-SALKEY.

030050 BC583.
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S	    ****
      *    *************************************************************
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
	       IF SAL-SUPER = 1
		   DISPLAY (LIN, 5) "(I)" WITH FOREGROUND-COLOR 14
               ELSE
		   DISPLAY (LIN, 5) "(R)"
			   WITH FOREGROUND-COLOR 7 HIGHLIGHT
	       END-IF
030120	       GO TO BC584.
      *    *************************************************************
      *    ****		   E X T E N D E D   C O D E		    ****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    *****       A L P H A - N U M E R I C   C O D E	   *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
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
       BC584.
	     ADD 1		 TO LIN.
030060	   IF WS-SALKEY < WS-LAST
	       ADD 1		 TO WS-SALKEY
030070	       GO TO BC583.

031060 BC590.
031070	     EXIT.
      /    *************************************************************
      *    ****  G E T	 O V E R R I D E   F O R   Q U A N T I T Y  ****
      *    *************************************************************
       BC600	     SECTION 50.
       BC601.
      /    *************************************************************
      *    ****    A M E N D   O R D E R
      *    *************************************************************
025640 BD000	     SECTION 51.
025650 BD00.
      *    *************************************************************
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *    *************************************************************
025660       MOVE 5              TO WS-PARKEY.
025670       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025680       MOVE PAR-PW         TO W85-PASS. 
025690 BD05.
      *    *************************************************************
      *    ****    S A L E S   O R D E R   S C R E E N
      *    *************************************************************
025700       DISPLAY CLR-SCREEN.
025710       DISPLAY S30.
      *    *************************************************************
      *            C H A N G E   S C R E E N   H E A D I N G
      *    *************************************************************
025720       DISPLAY S31.
025730       MOVE 8              TO LIN.
025740       PERFORM BD115 UNTIL LIN > 18.
025750       MOVE W12-TODAY      TO W90-DTE.
025760	     DISPLAY W90-DTE AT 0570
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
025770       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
025780				    W90-BAL W90-SALES
025790				    W90-CUR W90-EXCTOT.
025800	     MOVE 1		 TO WS-FIRST WS-SALPOS WS-LAST.
025810       MOVE ZERO           TO W10-SORD.
             MOVE "N"            TO WS-IN-OPT.
	     MOVE 1		 TO WS-S9.

       BD08.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BD08.

025830 BD10.
025840       PERFORM AA400.
025850       ACCEPT S30A AT 0510.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-SORD
		     GO TO BD268
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "O"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "T"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BD10
	       END-EVALUATE
025870	       PERFORM AA50O THRU AA59
025880         DISPLAY S30A AT 0510
025890         IF W10-SORD = ZERO
025900             GO TO BD10.
025910	     DISPLAY CLEAR-L25.
025920     IF W10-SORD = ZERO
025930	       GO TO BD268.
025940       MOVE W10-SORD       TO W90-REF.

025950 BD12.
025960       PERFORM GET-SORDER.
025970     IF WS-ERROR = 1
025980	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
025990         GO TO BD10.
026000     IF WS-ERROR = 2
026010	       MOVE "Not an order"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
026020         GO TO BD10.
026030     IF WS-ERROR = 3
026040	       MOVE "Order corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
026060         GO TO BD10.
026070     IF WS-ERROR = 4
026080	       MOVE "Invalid a/c no"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
026090         GO TO BD10.
025820       INITIALIZE W14-DELETE.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
026100       DISPLAY S36 AT 0308
026110	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
026120	     DISPLAY W90-ORD AT 0368
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
026130	     DISPLAY W10-SMAN AT 0411
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
026140     IF W10-SMAN NOT = ZERO
026150         ADD 100 W10-SMAN    GIVING WS-PARKEY
026160         PERFORM READ-PARAM THRU READ-PARAM-EXIT
026170	       DISPLAY PAR-SALESMAN AT 0420
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	     DISPLAY W40-TEL AT 0466
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W40-CONTACT AT 0528
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
026180       MOVE 1              TO WS-FIRST WS-SALKEY.
026190       SUBTRACT 1          FROM WS-LAST.
	     MOVE SPACES	 TO WS-RESTRICT.
	     MOVE "N"		 TO WS-RESTCODE.
      *    *************************************************************
      *    ****   D E B T O R	S A L E S   R E S T R I C T I O N S
      *    *************************************************************
	   IF DEB-RESTRICT = "A" OR "E"
	       MOVE DEB-ACNO	 TO DBD-ACNO
	       PERFORM READ-DEBDEP THRU READ-DEBDEP-EXIT
	       IF WS-F-ERROR = 0
		   MOVE DEB-RESTRICT
				 TO WS-RESTRICT.
026200	     PERFORM BD100 THRU BD102.

026210 BD15.
026220	     PERFORM BD270A.
026230	     DISPLAY CLEAR-L25.

026240 BD20.
             ADD 1               TO WS-LAST.
026300       GO TO BD112.

026310 BD50.
026320       MOVE 8              TO LIN.

026330 BD55.
026340	     MOVE SPACES	 TO W10-DESC W10-EXT-ITEM.
026350       MOVE ZERO           TO W10-SELL.
026350       MOVE 1              TO W10-QUANT.
026360 BD60.
026370       PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       ACCEPT S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      * 		***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT S07-ITM AT LINE LIN
	       ELSE
021070		   ACCEPT S07 AT LINE LIN
	       END-IF
	   ELSE
      *    *************************************************************
      * 		***** NUMERIC CODES APPLY *****
      *    *************************************************************
	   IF W10-SLNGTH = 3
	       ACCEPT S07-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S07-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S07-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S07-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S07-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S07-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S07-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S07-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S07-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S07-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S07-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S07-14 AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-ITEM
		     GO TO BD60A
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *	   ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BD60
	       END-EVALUATE
026410	       PERFORM AA50 THRU AA59
      *    *************************************************************
      * 	     *****   E X T E N D E D   C O D E	 *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN.
026430	       IF W10-EXT-ITEM = SPACES
026440             GO TO BD60.

       BD60A.
026450	   IF W10-EXT-ITEM = SPACES
026460         SUBTRACT 1        FROM WS-LAST
026470         GO TO BD120.
       BD61.
026480       PERFORM CA000.
026490     IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN
		   END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720             GO TO BD60
               ELSE
                   GO TO BD61.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BD60.
      *    *************************************************************
      *    **** C H E C K   F O R   S A L E S	R E S T R I C T I O N S
      *    *************************************************************
	   IF WS-RESTRICT = "A" OR "E"
	       PERFORM MA000
	       IF ((WS-RESTCODE = "Y") AND (WS-RESTRICT = "E")) OR
		  ((WS-RESTCODE = "N") AND (WS-RESTRICT = "A"))
		   MOVE "Restricted for this Department"
                                 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
020140		   GO TO BD60.
026520	     MOVE WS-LAST	 TO WS-SALKEY.
026530       INITIALIZE SAL-RECORD1.
026540       MOVE WS-LAST        TO SAL-CODE.
026560	     MOVE STK-CODE	 TO SAL-EXT-ITEM.
026570	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
026580 BD65.
026590     IF STK-IND = 3
026600         MOVE ZERO         TO STK-AVGE
               MOVE SPACES       TO W10-DESC
026610	       ACCEPT W10-SDSC AT LINE LIN COLUMN 21
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
026620			   BACKGROUND-COLOR 0 UPDATE AUTO
026630         DISPLAY "Index Unit cost" AT 2512
026640			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
026650         ACCEPT S26-AVGE AT LINE 25
026660     ELSE
026670         MOVE STK-DESC     TO W10-DESC
026680	       DISPLAY W10-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0.
026690     IF STK-IND = 3
026700         IF STK-AVGE = ZERO
021070		   MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
026730		   GO TO BD65.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
026750	     MOVE W10-EXT-ITEM	 TO SAL-EXT-ITEM.
026760       MOVE W10-DESC       TO SAL-MSER.
026770       MOVE STK-AVGE       TO SAL-COST W10-COST.
      *    *************************************************************
      *    ****    G E T   Q U A N T I T Y
      *    *************************************************************
	     MOVE ZERO		 TO W10-LENGTH W10-BREADTH W10-NUMBER.
	     MOVE "N"		 TO WS-MEASURE.
	     MOVE 1		 TO WS-P1.
	   IF (LS0-LEV < 3) OR
	      (STK-USE-PACKS = "N")
	       GO TO BD66.
	     DISPLAY "µ" AT 0750
		      STK-PER WITH BACKGROUND-COLOR 5
				   FOREGROUND-COLOR 6 HIGHLIGHT
		     "Æ".
	     GO TO BD66.

       BD65-PACK.
	   IF NOT (STK-UNT(WS-P1) = ZERO)
	       DISPLAY STK-PR(WS-P1) AT 0751
		       WITH BACKGROUND-COLOR 5
			    FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
	   IF WS-P1 = 1
	       DISPLAY "  Each  "  AT 0751
			WITH BACKGROUND-COLOR 5
			     FOREGROUND-COLOR 6 HIGHLIGHT
	   ELSE
	       SUBTRACT 1	 FROM WS-P1
	       GO TO BD65-PACK.

       BD66.
	     DISPLAY "F6" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     " to key in "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "- Cutting size (Wood, Glass etc) "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 7 HIGHLIGHT
		     "or "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 1.
	   IF LS0-LEV > 2
	       IF STK-USE-PACKS = "Y"
		   DISPLAY "" AT 2560
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
			   " for Packs"
			    WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1.
021140	     ACCEPT S26-QNT AT LINE LIN.
	   IF ADIS-FUNC
	       IF LS0-LEV < 3
		   GO TO BD66
	       END-IF
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     IF WS-P1 > 1
			 SUBTRACT 1  FROM WS-P1
			 GO TO BD65-PACK
		 WHEN DOWN-KEY
		     IF WS-P1 < 4
			 ADD 1	     TO WS-P1
			 GO TO BD65-PACK
	       END-EVALUATE
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
021390		     DISPLAY (LIN, 2) WS-INV78 WITH FOREGROUND-COLOR 3
021400		     GO TO BD55
		 WHEN F6-KEY
		     PERFORM BB050
021370		     DISPLAY S26-QNT AT LINE LIN
		 END-EVALUATE.
021150	   IF W10-QUANT = ZERO
021160	       DISPLAY (LIN, 2) WS-INV78 WITH FOREGROUND-COLOR 3
021170	       GO TO BD55.
      *    *************************************************************
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *    *************************************************************
       BD66A.
	   IF WS-P1 > 1
	       MULTIPLY STK-UNT(WS-P1) BY STK-AVGE
				 GIVING SAL-COST W10-COST
	   ELSE
021360	       MOVE STK-AVGE	 TO SAL-COST W10-COST.
	     MOVE W90-TAX	 TO W10-ENTRY.
      *    *************************************************************
      *    ****    C H A N G E	 F O R	 M U L T I P L E   R A T E S
      *    *************************************************************
	     MOVE STK-TAX	 TO WS-VAT-SUB.
026830	   IF STK-IND < 3
026840         IF W90-TAX NOT = SPACES
      *    *************************************************************
      *    ****    E X C L U S I V E   P R I C E S
      *    *************************************************************
021330		   IF WS-PRC = "1"
021570		       MOVE STK-SL(WS-P1)
021580                           TO W10-SELL
021360		   ELSE
021370		   IF WS-PRC = "2"
021610		       MOVE STK-CSH(WS-P1)
021620                           TO W10-SELL
021400		   ELSE
021640		       MOVE STK-WSL(WS-P1)
021650                           TO W10-SELL
021430	       ELSE
      *    *************************************************************
      *    ****    I N C L U S I V E   P R I C E S
      *    *************************************************************
021670		   IF WS-PRC = "1"
021680		       MOVE W10-VSL(WS-P1)
021690                           TO W10-SELL
021700             ELSE
021710		   IF WS-PRC = "2"
021720		       MOVE W10-VCSH(WS-P1)
021730                           TO W10-SELL
021740             ELSE
021750		       MOVE W10-VWSL(WS-P1)
021760                           TO W10-SELL.
	     MOVE W10-ENTRY	 TO W90-TAX.

       BD68.
             MOVE W10-SELL       TO W10-SPAPRC.

027060 BD70.
027070       ACCEPT S26-PRC AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BD66
	       END-EVALUATE.
	   IF (W10-SELL < W10-SPAPRC) AND
	      ((W10-SPAPRC - W10-SELL) < 0.01)
	       MOVE W10-SELL	 TO W10-SPAPRC.
	   IF W10-SELL < W10-SPAPRC
021180	       DISPLAY CLEAR-L25
               COMPUTE WS-DISC ROUNDED = ((W10-SELL - W10-SPAPRC) / 
                                           W10-SELL) * 100.00000
022110	       DISPLAY "Discount %" AT 2518
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
022190         DISPLAY S26-DSC AT LINE 25
	       DISPLAY "R" AT 2541
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "ekey or "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "ENTER"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       " to continue"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
	       ACCEPT WS-OPTION AT 2568
		      WITH BACKGROUND-COLOR 5
			   FOREGROUND-COLOR 7 HIGHLIGHT AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
022440	       DISPLAY CLEAR-L25
               IF WS-OPTION = "R"
                   GO TO BD70.
	   IF W10-SELL < W10-SPAPRC
021560	       MOVE W10-SELL	 TO W10-SPAPRC
021580	       PERFORM BB100
021590	       IF WS-ERROR = 1
021600		   MOVE ZERO	 TO WS-ERROR
021610		   GO TO BD66A.

027150 BD75.
027160       MOVE W10-SELL       TO SAL-SELL.
      *    *************************************************************
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *    *************************************************************
027200 BD80.
027210	     MOVE W10-QUANT	 TO SAL-QNT.
	     MOVE "N"		 TO SAL-INSTRUCT.
	     MOVE WS-P1 	 TO SAL-PACK.
027220       COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
027230       MOVE W25-VALUE      TO W100-EXT SAL-VAL.
027240	     DISPLAY (LIN, 70) W100-EXT
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
      *    *************************************************************
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *    *************************************************************
022440	     DISPLAY CLEAR-L25.
027250	     DISPLAY "Discount %" AT 2518
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
027260       MOVE ZERO           TO WS-DISC W90-CDISC.
      *    *************************************************************
      *    ****   I F	D I S C O U N T	   A P P L I C A B L E
      * 	  G E T   P E R C E N T A G E	F R O M
      * 	  D E P A R T M E N T	F I L E
      *    *************************************************************
027270	   IF (DEB-DISC > 0 AND < 11)
027280	       MOVE STK-LDG	 TO DPT-CODE
027290         PERFORM READ-DEPART THRU READ-DEPART-EXIT
027300	       MOVE DPT-DSETT(DEB-DISC)
027310				 TO WS-DISC
      *    *************************************************************
      *    **  M U S T	 T H E	 A L T E R N A T E   D I S C O U N T  **
      *    **  B E   U S E D   F O R   T H I S	D E B T O R	      **
      *    *************************************************************
	       IF STK-ALT-DISC = "Y"
		   IF DPT-ALT-DD1 = DEB-DISC
		       MOVE DPT-DD11
				 TO DPT-DSETT(DEB-DISC)
				    WS-DISC
		   ELSE
		   IF DPT-ALT-DD2 = DEB-DISC
		       MOVE DPT-DD12
				 TO DPT-DSETT(DEB-DISC)
				    WS-DISC
		   END-IF
	       END-IF.

027320 BD85.
027330       ACCEPT S26-DSC AT LINE 25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
022540		     DISPLAY CLEAR-L25
		     GO TO BD70
	       END-EVALUATE.
027340	   IF WS-DISC = ZERO
022540	       DISPLAY CLEAR-L25
027350         GO TO BD90.
027360	     COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
027370		    (W10-SELL * WS-DISC / 100.0000).
      *    *************************************************************
      *    ****    C H E C K   M A R G I N
      *    *************************************************************
027380	   IF (DEB-DISC > 0 AND < 11)
027390	       IF DPT-DSETT(DEB-DISC) = WS-DISC
027400             GO TO BD86.
027410       PERFORM BB100.
027420     IF WS-ERROR = 1
027430	       GO TO BD85.

027440 BD86.
027450	     DISPLAY CLEAR-L25.
027460       MOVE WS-DISC        TO SAL-DSC.
027470	     COMPUTE W90-CDISC ROUNDED =
		   ((W25-VALUE * WS-DISC) / 100.00000).
027480       MOVE W90-CDISC      TO W100-LBAL SAL-DISC.
027490	     DISPLAY W100-LBAL AT 2540
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
      *    *************************************************************
      *    ****    A R E   T H E   D E T A I L S   C O R R E C T
      *    *************************************************************
027500 BD90.
	     PERFORM CHECK-CORRECT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  As 'PERFORM BD66A THRU BD102' is called from paragraph   ³
      *    ³  BD345, 'WS-LOOP' is set to 'S' so that the routine will  ³
      *    ³  return to 'BD66A' if the details are not correct. After  ³
      *    ³  exiting from this perform 'WS-LOOP' is cleared to SPACE. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
027590     IF WS-OPTION = "N"
	       IF WS-LOOP = "S"
		   GO TO BD66A
	       ELSE
027600		   GO TO BD60.
      *    *************************************************************
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *    *************************************************************
027610 BD95.
019830	   IF (STK-IND < 2) OR (STK-IND = 3)
027630         COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
027640       ADD W90-CDISC       TO W90-LDISC.
027650       SUBTRACT W90-CDISC FROM W25-VALUE 
027660                           GIVING W90-VALUE.
027670	   IF (STK-TAX = 0) OR (W90-EXP = "X" OR "C")
027680         ADD W90-VALUE     TO W90-NONTAX
027690         MOVE W90-VALUE    TO SAL-NONTAX
	       MOVE ZERO	 TO SAL-VAT-SUB
027700     ELSE
027710         MOVE W90-VALUE    TO SAL-TAXVAL
027720	       ADD W90-VALUE	 TO W90-TAXVAL(WS-VAT-SUB)
	       MOVE WS-VAT-SUB	 TO SAL-VAT-SUB.
022910	   IF (W90-TAX NOT = SPACES) AND
	      (W90-EXP NOT = "C" AND "X")
	       COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
	   IF (W90-EXP = "I") AND (STK-TAX NOT = 0)
	       COMPUTE SAL-VAT ROUNDED = (SAL-TAXVAL /
		      (100.0000 + W05-RATE)) * W05-RATE.
	   IF SAL-VAT-SUB > 0
	       ADD SAL-VAT	 TO W90-VAT(SAL-VAT-SUB).
      *    *************************************************************
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *    *************************************************************
027760       ADD W25-VALUE       TO W90-DB.

027770 BD100.
027780       MOVE W90-DB         TO W90-TOTAL.
027790       SUBTRACT W90-LDISC  FROM W90-TOTAL.
027800       MOVE W90-DB         TO W100-BAL.
027810	     DISPLAY W100-BAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027820       MULTIPLY -1 BY W90-LDISC 
027830                           GIVING W100-BAL.
027840	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *    *************************************************************
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *    *************************************************************
027880	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
027890         GO TO BD102.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BD101.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BD101.
023100	     MOVE W25-TOTAL	 TO W100-BAL.
023120	     ADD W25-TOTAL	 TO W90-TOTAL.
022750	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

027970 BD102.
022780	     MOVE W90-TOTAL	 TO W100-BAL.
022790	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028040 BD105.
036100	     MOVE "'A'ccept or 'C'ancel   [A]"
				 TO WS-ERR-MES.
023210	     MOVE "A"		 TO WS-OPTION.
      *    *************************************************************
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *    *************************************************************
028100 BD110.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "A" OR "C".
028210	   IF WS-OPTION = "C"
028220	       DISPLAY (LIN, 2) WS-INV78
			WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
028230         PERFORM BD114
028240         PERFORM BD100 THRU BD102
028250         GO TO BD55.               

028260 BD111.
             MOVE WS-SUPER       TO SAL-SUPER.
028270       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
028280       ADD 1               TO WS-LAST.
      *    *************************************************************
      *    ****    T E S T   F O R   C U T T I N G   S P E C S
      *    *************************************************************
	   IF W10-LENGTH > ZERO AND
	      W10-BREADTH > ZERO AND
	      W10-NUMBER > ZERO
023410	       IF LIN < 18
023420		   ADD 1	 TO LIN
	       ELSE
023440		   MOVE 8	 TO WS-S7
023450		   PERFORM BD111-SHIFT
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
023400	       ADD 1		 TO WS-LAST.
      *    *************************************************************
      *    ****    T E S T   I F   S C R E E N   F U L L
      *    *************************************************************
028290     IF LIN < 18
028300         ADD 1             TO LIN
028310         GO TO BD112.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO WS-S7.
      *    *************************************************************
      *    ****    S C R O L L	 D E T A I L   L I N E S   U P
      *    *************************************************************
       BD111-SHIFT.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES (WS-S8)
				 TO W42-LINES (WS-S7).
	     MOVE W43-LINE (WS-S8)
				 TO W43-LINE (WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BD111-SHIFT.
	     PERFORM RESTORE-SCREEN-2.
      *    *************************************************************
      *    ****    C L E A R   L A S T	 D E T A I L   L I N E
      *    *************************************************************
023890	     DISPLAY (LIN, 1) WS-INV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-INV78 WITH FOREGROUND-COLOR 3
			      WS-INV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
	     ADD 1		 TO WS-SALPOS.

028350 BD112.
	     MOVE SPACES     TO WS-ERR-MES.
036410	     STRING "'I'nternal notes,'R'emarks,'N'ext or 'E'nd  ["
		    DELIMITED SIZE
		    WS-IN-OPT DELIMITED SIZE
		    "]" DELIMITED SIZE
				 INTO WS-ERR-MES.
023180	     MOVE WS-IN-OPT	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "I" OR "R" OR "N" OR "E" OR "C".
      *    *************************************************************
      *    ****  "C"   M A Y   B E   U S E D   T O   C H A N G E
      * 	  C L I E N T	D E T A I L S
      *    *************************************************************
	   IF WS-OPTION = "C"
	       PERFORM BD591A
	       GO TO BD112.
	     MOVE WS-OPTION	 TO WS-IN-OPT.
028530     IF WS-OPTION = "N"
028540         GO TO BD55.
028550     IF WS-OPTION = "E"
028560         SUBTRACT 1        FROM WS-LAST
028570         GO TO BD120.
028580       MOVE SPACES         TO W10-REMKS.
028590	     ACCEPT W10-REMKS AT LINE LIN COLUMN 21
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
028600			 BACKGROUND-COLOR 0 UPDATE AUTO.
028610     IF W10-REMKS = SPACES
028620         GO TO BD112.
028640	     MOVE WS-LAST	 TO WS-SALKEY.
028650       INITIALIZE SAL-RECORD1.
028660       MOVE WS-LAST        TO SAL-CODE.
028670       MOVE W10-REMKS      TO SAL-MSER.
           IF WS-OPTION = "I"
	       DISPLAY "(I)" AT LINE LIN COLUMN 5
			WITH FOREGROUND-COLOR 14
               MOVE 1            TO SAL-SUPER
           ELSE
	       DISPLAY "(R)" AT LINE LIN COLUMN 5
			WITH FOREGROUND-COLOR 7 HIGHLIGHT.
028680       GO TO BD111.

028690 BD114.
028700       SUBTRACT W25-VALUE  FROM W90-DB.
028710       SUBTRACT W90-CDISC  FROM W90-LDISC.
028720       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF SAL-VAT-SUB > ZERO
036780	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(SAL-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(SAL-VAT-SUB).
019830	   IF (STK-IND < 2) OR (STK-IND = 3)
028750         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****   C L E A R   S C R E E N   L I N E / S
      *    *************************************************************
028760 BD115.
036820	     DISPLAY (LIN, 1) WS-INV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-INV78 WITH FOREGROUND-COLOR 3
			      WS-INV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
028780	     ADD 1		 TO LIN.
      *    *************************************************************
      *    ****    W A S   T H E R E   A N   E N T R Y
      *    *************************************************************
028790 BD120.
028800	   IF WS-LAST = ZERO
028810         GO TO BD05.
      *    *************************************************************
      *    ****    P R O C E S S   T H E   O R D E R
      *    *************************************************************
028820 BD125.
023600	     MOVE "'U'pdate,'P'rint,'A'mend,'C'ancel,'M'ore items"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

023680 BD130.
	     PERFORM OPT-MESSAGE
		     UNTIL WS-OPTION = "U" OR "A" OR "C" OR "M" OR "P".
029020	   IF WS-OPTION = "M"
029030         ADD 1             TO WS-LAST
029040         GO TO BD112.
029050     IF WS-OPTION = "A"
029060	       PERFORM BD270A
029070         GO TO BD125.
029080     IF WS-OPTION = "C"
029090	       GO TO BD268.
029100       MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
029110     IF W90-LDISC NOT = ZERO
029120         MOVE W90-LDISC    TO W90-TDISC
029130         GO TO BD140.
029140       MOVE W90-DB         TO W90-DVAL.

029150 BD140.
029160       SUBTRACT W90-TDISC FROM W90-DB 
029170                           GIVING W90-TSELL W90-TOTAL.
029180       MULTIPLY -1 BY W90-TDISC 
029190                           GIVING W100-BAL.
      *    *************************************************************
      *    ****         D  I  S  C  O  U  N  T
      *    *************************************************************
029200	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
029240	     MOVE ZERO		 TO W90-TTAX
				    W90-EXCTOT.
	     MOVE 1		 TO WS-P1.
      *    *************************************************************
      *    ****    V A T   ?
      *    *************************************************************
029250	   IF (W90-TAX = SPACES) OR (W90-EXP = "C" OR "X")
029260	       GO TO BD143.
      *    *************************************************************
      *    ****    C A L C U L A T E   T O T A L   V A T
      *    *************************************************************
       BD141.
037540	   IF (W90-TAXVAL(WS-P1) = ZERO)
024430	       GO TO BD142.
024440	     MOVE W05-VAT(WS-P1) TO W05-RATE.
021490	     COMPUTE W90-TTAX ROUNDED =
		    (W90-TAXVAL(WS-P1) * W05-RTE).
	     ADD W90-TAXVAL(WS-P1)
				 TO W90-EXCTOT.
      *    *************************************************************
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      * 	   H A V E   B E E N   C A L C U L A T E D
      *    *************************************************************
       BD142.
	   IF WS-P1 < 6
	       ADD 1		 TO WS-P1
	       GO TO BD141.
029300	     MOVE W90-TTAX	 TO W100-BAL.
      *    *************************************************************
      *    ****         V  A  T
      *    *************************************************************
029310	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
029320       ADD W90-TTAX        TO W90-TOTAL.

029330 BD143.
029380	     MOVE W90-TOTAL	 TO W100-BAL.
      *    *************************************************************
      *    ****         T  O  T  A  L
      *    *************************************************************
029390	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
029400	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    U P D A T E	 O R D E R   D E T A I L S
      *    *************************************************************
029410 BD145.
029420      DISPLAY "Updating Order" AT 2520 WITH
029430		     BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
029440       MOVE ZERO           TO WS-SALKEY.
029450       PERFORM AY60 THRU AY999.
      *    *************************************************************
      *    ****    U P D A T E   O R D E R   F I L E 
      *    *************************************************************
029460 BD190.
029470	     MOVE W90-REF	 TO SOR-REF.
029480     IF W14-SUB = ZERO
029490         GO TO BD205.
      *    *************************************************************
      *    ****    D E L E T E   R E D U N D A N T   R E C O R D S
      *    *************************************************************
029500	     MOVE 1		 TO WS-S1.

029510 BD195.
	   IF W14-SQN (WS-S1) NOT = ZERO
029520         MOVE W14-SQN (WS-S1)
029530                           TO SOR-SQN
029540	       PERFORM READ-SORDER THRU READ-SORDER-EXIT
029550         MOVE 2            TO WS-ACTION
029560         PERFORM AY17 THRU AY59
029570	       PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT
	       IF SOR-TYPE = 1
                   PERFORM BD196 THRU BD199.
029580	   IF WS-S1 < W14-SUB
029590         ADD 1             TO WS-S1
029600         GO TO BD195.
             GO TO BD205.

       BD196.
	     MOVE SOR-EXT-ITEM	 TO PRT-EXT-ITEM.

       BD197.
020530       PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510     IF WS-F-ERROR = 21
               GO TO BD199.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
021420 BD198.
029540         PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT
021430     IF SOR-TYPE NOT = 2
021450         GO TO BD199.
029550       MOVE 2              TO WS-ACTION.
029560       PERFORM AY17 THRU AY59.
029570       PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
             GO TO BD198.
       BD199.
             EXIT.

029610 BD200.
029620       PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *    *************************************************************
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *    *************************************************************
029630 BD205.
029640	     ADD 1		 TO WS-SALKEY.
029650     IF WS-SALKEY > WS-LAST
029660         GO TO BD255.
029670       PERFORM READ-SALE THRU READ-SALE-EXIT.
029680     IF SAL-SQN = ZERO
029690         SUBTRACT 1        FROM WS-SALKEY
029700         GO TO BD235.
029710       MOVE SAL-SQN        TO SOR-SQN.
029720       PERFORM READ-SORDER THRU READ-SORDER-EXIT.
029730       MOVE 0              TO WS-ACTION.
029740       PERFORM AY17 THRU AY59.
029750     IF SOR-TYPE NOT = 1
029760         GO TO BD210.
      *    *************************************************************
      *    ****    U P D A T E	 P I C K I N G	 S L I P   Q T Y
      * 	   O N	 S T O C K   R E C O R D
      *    *************************************************************
024920	     MOVE SAL-EXT-ITEM	 TO STK-CODE.
	     MOVE 0		 TO WS-ACTION.
	     PERFORM AY05 THRU AY59.
	     SUBTRACT SOR-QNT	 FROM STK-QPIC.
	     ADD SOR-QNT	 TO STK-QUANT.
	     MOVE SAL-EXT-ITEM	 TO SOR-EXT-ITEM.
	     MOVE SAL-MSER	 TO SOR-DESC.
029770	     MOVE SAL-QNT	 TO SOR-QNT.
      *				    SOR-QORD.
029780       MOVE SAL-COST       TO SOR-COST.
029790       MOVE SAL-VAT        TO SOR-VAT.
015000	     MOVE SAL-VAT-SUB	 TO SOR-VAT-SUB.
029800       MOVE SAL-SELL       TO SOR-SELL.
029810       MOVE SAL-VAL        TO SOR-VAL.
029820       MOVE SAL-DISC       TO SOR-DISC.
029830       MOVE SAL-DSC        TO SOR-DSC.
029840       MOVE SAL-INCTAX     TO SOR-INCTAX.
029850       MOVE SAL-NONTAX     TO SOR-NONTAX.
029860       MOVE SAL-TAXVAL     TO SOR-TAXVAL.
	     SUBTRACT SOR-QNT	 FROM STK-QUANT.
	     ADD SOR-QNT	 TO STK-QPIC.
017190	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
029620	     PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             MOVE SPACE          TO WS-OPTION.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
       BD206.
029540       PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
021430     IF SOR-TYPE NOT = 2
021450         GO TO BD206A.
             MOVE "P"            TO WS-OPTION.
029550       MOVE 2              TO WS-ACTION.
029560       PERFORM AY17 THRU AY59.
	     MOVE SOR-EXT-ITEM	 TO STK-CODE.
	     MOVE 0		 TO WS-ACTION.
	     PERFORM AY05 THRU AY59.
	     SUBTRACT SOR-QNT	 FROM STK-QPIC.
	     ADD SOR-QNT	 TO STK-QUANT.
017190	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
029570       PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
             GO TO BD206.

       BD206A.
029710       MOVE SAL-SQN        TO SOR-SQN.
029720       PERFORM READ-SORDER THRU READ-SORDER-EXIT.
           IF WS-OPTION NOT = "P"
               GO TO BD205.
020530       PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510     IF WS-F-ERROR = 21
               GO TO BD209.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
021370       MOVE 1              TO WS-S6.

021420 BD207.
021430     IF PRT-CODE (WS-S6) = SPACES
021440         ADD 1             TO WS-S6
021450         GO TO BD208.
021470	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W10-EXT-ITEM.
021490       PERFORM CA000.
021500     IF WS-F-ERROR = 22
021510         ADD 1             TO WS-S6
021520         GO TO BD208.
022580       MOVE 2              TO SOR-TYPE.
022590       INITIALIZE SOR-DET2.
	     MOVE STK-CODE	 TO SOR-EXT-ITEM.
021540       MOVE STK-DESC       TO SOR-DESC.
	     MULTIPLY SAL-QNT BY PRT-QUANT(WS-S6)
                                 GIVING SOR-QNT.
022610       ADD 1               TO SOR-SEQ.
	     MOVE 0		 TO WS-ACTION.
	     PERFORM AY05 THRU AY59.
	     SUBTRACT SOR-QNT	 FROM STK-QUANT.
	     ADD SOR-QNT	 TO STK-QPIC.
017190	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
	     PERFORM BD225.
021600	     ADD 1		 TO WS-S6.

021660 BD208.
021670     IF WS-S6 < 31
021680         GO TO BD207.

       BD209.
             GO TO BD205.

029880 BD210.
029890       MOVE SAL-DESC       TO SOR-DESC.
029900       GO TO BD200.
      *    *************************************************************
      *    ****    L O G   R E C O R D   T O   O R D E R   F I L E
      *    *************************************************************
029920 BD225.
029930       PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.
029940       MOVE 1              TO WS-ACTION.
029950       PERFORM AY17 THRU AY59.
      *    *************************************************************
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *    *************************************************************
029960 BD235.
029970	     ADD 1		 TO WS-SALKEY.
029980     IF WS-SALKEY > WS-LAST
029990         GO TO BD255.
030000       PERFORM READ-SALE THRU READ-SALE-EXIT.
030030	   IF SAL-EXT-ITEM = SPACES
030040         GO TO BD247.
030050       MOVE 1              TO SOR-TYPE.
030060	     MOVE SAL-EXT-ITEM	 TO SOR-EXT-ITEM.
030070       MOVE SAL-DESC       TO SOR-DESC.
030080	     MOVE SAL-QNT	 TO SOR-QNT.
      *				    SOR-QORD.
030090       MOVE SAL-COST       TO SOR-COST.
030100       MOVE SAL-VAT        TO SOR-VAT.
015000	     MOVE SAL-VAT-SUB	 TO SOR-VAT-SUB.
030110       MOVE SAL-SELL       TO SOR-SELL.
030120       MOVE SAL-VAL        TO SOR-VAL.
030130       MOVE SAL-DISC       TO SOR-DISC.
030140       MOVE SAL-DSC        TO SOR-DSC.
030150       MOVE SAL-INCTAX     TO SOR-INCTAX.
030160       MOVE SAL-NONTAX     TO SOR-NONTAX.
030170       MOVE SAL-TAXVAL     TO SOR-TAXVAL.
	     MOVE SAL-PACK	 TO SOR-PACK.
030180       MOVE SPACES         TO SOR-SERNO.
030190       ADD 1               TO SOR-SEQ.
             MOVE ZERO           TO SOR-SUB.
             PERFORM BD225.
      *    *************************************************************
      *    ****    U P D A T E	 P I C K I N G	 S L I P   Q T Y
      * 	   O N	 S T O C K   R E C O R D
      *    *************************************************************
024920	     MOVE SAL-EXT-ITEM	 TO STK-CODE.
	     MOVE 0		 TO WS-ACTION.
	     PERFORM AY05 THRU AY59.
	     SUBTRACT SOR-QNT	 FROM STK-QUANT.
	     ADD SOR-QNT	 TO STK-QPIC.
017190	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
	     PERFORM BD248 THRU BD254.
030200       GO TO BD235.

030210 BD247.
           IF SAL-SUPER = 1
               MOVE 4            TO SOR-TYPE
           ELSE
               MOVE 3            TO SOR-TYPE.
030230       INITIALIZE SOR-DET2.
030240       MOVE SAL-DESC       TO SOR-DESC.
030250       ADD 1               TO SOR-SEQ.
             MOVE ZERO           TO SOR-SUB.
030260       GO TO BD225.
       BD248.
	     MOVE SOR-EXT-ITEM	 TO PRT-EXT-ITEM.
020530       PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
020510     IF WS-F-ERROR = 21
               GO TO BD254.
      *    *************************************************************
      *    ****  P R O D U C T I O N   D E T A I L S
      *    *************************************************************
021370       MOVE 1              TO WS-S6.
021420 BD249.
021430	   IF PRT-EXT-CODE(WS-S6) = SPACES
021440         ADD 1             TO WS-S6
021450         GO TO BD250.
021470	     MOVE PRT-EXT-CODE(WS-S6)
				 TO W10-EXT-ITEM.
021490       PERFORM CA000.
021500     IF WS-F-ERROR = 22
021510         ADD 1             TO WS-S6
021520         GO TO BD250.
022580       MOVE 2              TO SOR-TYPE.
022590       INITIALIZE SOR-DET2.
	     MOVE STK-CODE	 TO SOR-EXT-ITEM.
021540       MOVE STK-DESC       TO SOR-DESC.
	     MULTIPLY SAL-QNT BY PRT-QUANT(WS-S6)
                                 GIVING SOR-QNT.
022610       ADD 1               TO SOR-SEQ.
             MOVE ZERO           TO SOR-SUB.
             PERFORM BD225.
021600       ADD 1               TO WS-S6.
021660 BD250.
021670     IF WS-S6 < 31
021680         GO TO BD249.

       BD254.
             EXIT.
      *    *************************************************************
      *    ****    C H E C K / U P D A T E   C L I E N T   D E T A I L S
      *    *************************************************************
030270 BD255.
	   IF W40-AD1 = SPACES
               GO TO BD256.
           IF W35-KEY = SPACES
               MOVE 5            TO SOR-TYPE
               ADD 1             TO SOR-SEQ
           ELSE
               MOVE W35-KEY      TO SOR-KEY
030290	       PERFORM READ-SORDER THRU READ-SORDER-EXIT
               MOVE 0            TO WS-ACTION
               PERFORM AY17 THRU AY59.
             MOVE W40-AD1        TO SOR-AD1.
             MOVE W40-AD2        TO SOR-AD2.
             MOVE W40-AD3        TO SOR-AD3.
             MOVE W40-AD4        TO SOR-AD4.
             MOVE W40-PC         TO SOR-PC.
           IF W35-KEY = SPACES
029930         PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT
029940         MOVE 1            TO WS-ACTION
029950         PERFORM AY17 THRU AY59
           ELSE
029620         PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *    *************************************************************
      *    ****    O R D E R   T O T A L S
      *    *************************************************************
       BD256.
030280       MOVE 99900          TO SOR-SQN.
030290	     PERFORM READ-SORDER THRU READ-SORDER-EXIT.
030300       MOVE 0              TO WS-ACTION.
030310       PERFORM AY17 THRU AY59.
030320       MOVE W90-DB         TO SOR-SUBTOT.
030330       MULTIPLY -1 BY W90-TDISC 
030340                           GIVING SOR-DSCNT
030350	     MOVE W90-EXCTOT	 TO SOR-TAXABLE.
030360       MOVE W90-NONTAX     TO SOR-NON-TXBL.
030370       MOVE W90-TTAX       TO SOR-TAX.
030380       MOVE W90-TOTAL      TO SOR-TOT.
030390       MOVE W90-BAL        TO SOR-BAL.
	     MOVE W90-INSTRUCT	 TO SOR-INSTRUCT.
030410	     PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *    *************************************************************
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *    *************************************************************
030420       PERFORM AY70 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ To avoid problems of having data corrupted (MULTI USER)   ³
      *    ³ by the cache (buffering) of data on the individual Work   ³
      *    ³ stations. This results in data from one work station not  ³
      *    ³ being available on another Work station until the cache   ³
      *    ³ has been ceared. Including a close of the file after each ³
      *    ³ new order has been added and then re-opening the file is  ³
      *    ³ the way in which the system will release the data for it  ³
      *    ³ to be written to the server file and should resolve this  ³
      *    ³ problem.						       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     CLOSE SORDER.
	     OPEN I-O SORDER.

030540 BD260.
022810	     MOVE "Print picking slip (Y/N)?  [ ]"
				 TO WS-ERR-MES.
022870	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE
		     UNTIL WS-OPTION = "Y" OR "N".
030640     IF WS-OPTION = "Y"
030650         DISPLAY CLR-SCREEN
030660	       DISPLAY "AMEND ORDER" AT 0235
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
030670         DISPLAY "Printing picking slip" AT 1212
030680                  WITH FOREGROUND-COLOR 14
030690	       MOVE W90-REF	 TO LS-SLS-ORDNO
               PERFORM BB200.
      *    *************************************************************
      *    ****    C L O S E   F I L E S   
      * 	   ( CLEAR THE WORKING FILE FOR THE NEXT TRANSACTION)
      *    *************************************************************
030840       CLOSE SALE.
030830       OPEN OUTPUT SALE.
030840	     CLOSE SALE.
030450	     OPEN I-O SALE.
029700	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030970	   IF WS-OPTION = "E"
030980	       GO TO BD268.
030990       GO TO BD05.

       BD268.
	     EXIT.
      *    *************************************************************
      *    ****    A M E N D   S A L E S   D E T A I L S
      *    *************************************************************
       BD270A		SECTION 51.
031000 BD270.
	     MOVE "Amend - 'H'eader, 'S'ales details or 'E'xit"
				 TO WS-ERR-MES.
	     MOVE "S"		 TO WS-OPTION.

       BD271.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "E" OR "H" OR "S".
           IF WS-OPTION = "S"
               GO TO BD272
           ELSE
           IF WS-OPTION = "E"
	       GO TO BD590.
014580       MOVE W10-SORD       TO SOR-REF.
014590       MOVE 100            TO SOR-SQN.
014600       MOVE ZERO           TO WS-ERROR.
014610       PERFORM READ-SORDER THRU READ-SORDER-EXIT.

       BD271A.
	     MOVE "Amend-'O'rd No,'S'alesman,'T'el.,'C'ont.,'E'xit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

       BD271B.
	     PERFORM OPT-MESSAGE
		     UNTIL WS-OPTION = "C" OR "E" OR "O" OR "S" OR "T".
           IF WS-OPTION = "E"
	       GO TO BD582.
014610	     PERFORM READ-SORDER THRU READ-SORDER-EXIT.
           IF WS-OPTION = "C"
               ACCEPT W40-CONTACT AT 0528 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
               MOVE W40-CONTACT  TO SOR-CONTACT
	       PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
               GO TO BD271A
           ELSE
           IF WS-OPTION = "O"
017660         ACCEPT W90-ORD AT 0368 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
               MOVE W90-ORD      TO SOR-ORD
	       PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
               GO TO BD271A
           ELSE
           IF WS-OPTION = "T"
               ACCEPT W40-TEL AT 0466 
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 UPDATE AUTO
               MOVE W40-TEL      TO SOR-TEL
	       PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
               GO TO BD271A.
       BD271C.
017700	     ACCEPT W10-SMAN AT 0411
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
017720     IF W10-SMAN > 50
017730         GO TO BD271C.
017740     IF W10-SMAN = ZERO
	       DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 0420
                       WITH FOREGROUND-COLOR 3
               MOVE ZERO         TO SOR-SMAN
	       PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
017750	       GO TO BD271A.
017760       ADD 100 W10-SMAN    GIVING WS-PARKEY.
017770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017780     IF PAR-SALESMAN = SPACES
017790	       MOVE "Invalid Salesman Number"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017810         GO TO BD271C.
017830	     DISPLAY PAR-SALESMAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             MOVE W10-SMAN       TO SOR-SMAN.
	     PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             GO TO BD271A.

       BD272.
031010       MOVE 8              TO LIN.
031020       PERFORM BD115 UNTIL LIN > 18.
031030       MOVE 8              TO LIN.
031040       MOVE 1              TO WS-LINES WS-FIRST.

031050 BD275.
031060	     MOVE WS-FIRST	 TO WS-SALKEY WS-SALPOS.
      *    *************************************************************
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *    *************************************************************
031070 BD280.
031080	   IF WS-SALKEY > WS-LAST
	       SUBTRACT 1	 FROM WS-SALKEY
031090	       GO TO BD284.
031100       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
029240	   IF SAL-EXT-ITEM = SPACES
029250	       DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0
	       IF SAL-SUPER = 1
		   DISPLAY "(I)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 6 HIGHLIGHT
               ELSE
		   DISPLAY "(R)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 7 HIGHLIGHT
               END-IF
029260         GO TO BD282.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
031160	     DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
031170       MOVE SAL-QNT        TO W10-QUANT.
031180       DISPLAY S26-QNT AT LINE LIN.
031190       MOVE SAL-SELL       TO W10-SELL.
031200       DISPLAY S26-PRC AT LINE LIN.
031210       MOVE SAL-VAL        TO W100-EXT.
031220	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *    *************************************************************
      *    ****    I S   T H E   S C R E E N   F U L L
      *    *************************************************************
031230 BD282.
031240     IF LIN < 18
031250         ADD 1             TO LIN WS-SALKEY WS-LINES
031260         GO TO BD280.

       BD284.
	     MOVE 7		 TO LIN.
	     DISPLAY CLEAR-L25.

029390 BD285.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
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
      *    *************************************************************
      *    ****    S A V E   C U R R E N T   P A G E
      *    *************************************************************
	     PERFORM SAVE-SCREEN-2.
      *    *************************************************************
      *    ****    S C R E E N	 A T T R I B U T E S
      *    *************************************************************
       BD289.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.
      *    *************************************************************
      *    ****    G E T   U S E R   O P T I O N
      *    *************************************************************
029660 BD290.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BD292
		 WHEN DOWN-KEY
001340		     GO TO BD293
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BD290
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     DISPLAY CLEAR-L25
		     GO TO BD582
      *    *************************************************************
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-DOWN
001340		     GO TO BD305
      *    *************************************************************
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
		 WHEN PAGE-UP
001340		     GO TO BD300
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BD290
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION = "A" OR "D") AND
		  (LIN > 7)
		   DISPLAY CLEAR-L25
		   GO TO BD310.
	     CALL X"E5".
001280	     GO TO BD290.
      *    *************************************************************
      *    ****    U P	 K E Y	 ( M O V E   H I G H L I G H T	 U P )
      *    *************************************************************
       BD292.
	   IF (WS-SALPOS = 1) OR (LIN < 8)
	       GO TO BD294.
	     SUBTRACT 1 	 FROM WS-SALPOS.
	   IF LIN > 8
	       SUBTRACT 1	 FROM LIN
	       GO TO BD294.
	     MOVE 18		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      * 	    O N E   L I N E
      *    *************************************************************
       BD292-LOOP.
	     SUBTRACT 1 FROM WS-S7
				 GIVING WS-S8.
	     MOVE W42-LINES(WS-S8)
				 TO W42-LINES(WS-S7).
	     MOVE W43-LINE(WS-S8)
				 TO W43-LINE(WS-S7).
	   IF WS-S7 > 9
	       SUBTRACT 1	 FROM WS-S7
	       GO TO BD292-LOOP.
	     MOVE WS-SALPOS	 TO WS-SALKEY WS-FIRST.
      *    *************************************************************
      *    ****    C L E A R   F I R S T   O R	 L A S T
      * 	   D E T A I L	 L I N E.
      *    *************************************************************
       BD292-DISPLAY.
	     PERFORM RESTORE-SCREEN-2.
023890	     DISPLAY WS-INV1 AT LINE LIN COLUMN 1
		     WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3
		     WS-INV78 WITH FOREGROUND-COLOR 3
		     WS-INV80 WITH BACKGROUND-COLOR 0
				   FOREGROUND-COLOR 3.
      *    *************************************************************
      *    ****    G E T   N E X T   O R   P R E V I O U S
      * 	   D E T A I L	 L I N E.
      *    *************************************************************
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0
	       IF SAL-SUPER = 1
		   DISPLAY "(I)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 6 HIGHLIGHT
               ELSE
		   DISPLAY "(R)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 7 HIGHLIGHT
	       END-IF
030120	       GO TO BD292-END-DISP.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
030140	     DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
030150       MOVE SAL-QNT        TO W10-QUANT.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.

       BD292-END-DISP.
	     PERFORM SAVE-SCREEN-2.
	     GO TO BD294.
      *    *************************************************************
      *    ****    D O W N   K E Y   ( M O V E	 H I G H L I G H T
      * 			       D O W N )
      *    *************************************************************
       BD293.
	   IF (WS-SALPOS = WS-LAST) AND (LIN > 7)
	       GO TO BD294.
      *    *************************************************************
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *    *************************************************************
	   IF LIN > 7
	       ADD 1		 TO WS-SALPOS.
	   IF LIN < 18
	       ADD 1		 TO LIN
	       GO TO BD294.
	     MOVE 8		 TO WS-S7.
      *    *************************************************************
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      * 	    L I N E
      *    *************************************************************
       BD293-LOOP.
	     ADD 1 WS-S7	 GIVING WS-S8.
	     MOVE W42-LINES(WS-S8)
				 TO W42-LINES(WS-S7).
	     MOVE W43-LINE(WS-S8)
				 TO W43-LINE(WS-S7).
	   IF WS-S8 < 18
	       ADD 1		 TO WS-S7
	       GO TO BD293-LOOP.
	     ADD 1		 TO WS-FIRST.
	     MOVE WS-SALPOS	 TO WS-SALKEY.
	     GO TO BD292-DISPLAY.
      *    *************************************************************
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *    *************************************************************
001980 BD294.
001990	     PERFORM BD289.
	   IF LIN > 7
002000	       DISPLAY W43-DET(LIN) AT LINE LIN COLUMN 1
		       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
002010	     GO TO BD290.
      *    *************************************************************
      *    ****    C L E A R   S C R E E N
      *    *************************************************************
030780 BD295.
030790	     MOVE 8		 TO LIN.
030800	     PERFORM BD115 UNTIL LIN > 18.
030810	     MOVE 8		 TO LIN.
030820       MOVE 1              TO WS-LINES.
      *    *************************************************************
      *    ****   P R E V I O U S   S C R E E N	  R E Q U I R E D
      *    *************************************************************
       BD300.
	   IF WS-FIRST = 1
	       GO TO BD290.
	   IF WS-FIRST > 11
	       SUBTRACT 11	 FROM WS-FIRST
	   ELSE
	       MOVE 1		 TO WS-FIRST.
	     PERFORM BD295.
	     GO TO BD275.
      *    *************************************************************
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
       BD305.
	   IF (WS-FIRST + 11) > WS-LAST
	       GO TO BD290.
	     ADD 11		 TO WS-FIRST.
	     PERFORM BD295.
	     GO TO BD275.
      *    *************************************************************
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *    *************************************************************
       BD310.
030630	   IF WS-OPTION = "A"
032160	       GO TO BD320.
      *    *************************************************************
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *    *************************************************************
       BD315.
043780	     MOVE "Are you sure Y/N?  [ ]"
				 TO WS-ERR-MES.
030880	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE
		     UNTIL WS-OPTION = "N" OR "Y".
031000     IF WS-OPTION = "N"
031010	       GO TO BD285.
031020       MOVE "D"            TO WS-OPTION.
      *    *************************************************************
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *    *************************************************************
       BD320.
031220	     MOVE WS-SALPOS	 TO WS-SALKEY.
031230       PERFORM READ-SALE THRU READ-SALE-EXIT.
032570	   IF SAL-EXT-ITEM = SPACES
032580	       GO TO BD322.
032590	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM STK-CODE.
032600       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
032610       MOVE SAL-QNT        TO W10-QUANT.
032620       MOVE SAL-COST       TO W10-COST.
032630       MOVE SAL-SELL       TO W10-SELL.
032640       MOVE SAL-DISC       TO W90-CDISC.
032650       MOVE SAL-DSC        TO WS-DISC.
032660       MOVE SAL-VAL        TO W25-VALUE.
	     MOVE SAL-VAT-SUB	 TO WS-VAT-SUB.
	     MOVE SAL-PACK	 TO WS-P1.
      *    *************************************************************
      *    ****    R E D U C E   T O T A L S 
      *    *************************************************************
032670       SUBTRACT W25-VALUE  FROM W90-DB.
032680       SUBTRACT W90-CDISC  FROM W90-LDISC.
032690       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
	   IF WS-VAT-SUB > ZERO
031370	       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB)
	       SUBTRACT SAL-VAT	 FROM W90-VAT(SAL-VAT-SUB).
019830	   IF (STK-IND < 2) OR (STK-IND = 3)
032720         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *    *************************************************************
      *    ****    I S   I T   A N   A M E N D M E N T
      *    *************************************************************
032730 BD322.
032740     IF WS-OPTION = "A"
032750         GO TO BD325.
001990	     PERFORM BD289.
      *    *************************************************************
      *    ****   U P D A T E	S T O C K   Q U A N T I T Y
      *    *************************************************************
032570	   IF NOT(SAL-EXT-ITEM = SPACES)
	       IF SAL-INSTRUCT = "Y"
		   ADD SAL-QNT	 TO STK-QUANT
		   SUBTRACT SAL-QNT
				 FROM STK-QPIC
017190		   PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
      *    *************************************************************
      *    ****    R E - D I S P L A Y   T O T A L S
      *    *************************************************************
032760	   IF SAL-EXT-ITEM NOT = SPACES
032770         PERFORM BD100 THRU BD102.
      *    *************************************************************
      *    ****    D E L E T E   I T E M   F R O M   O R D E R   F I L E 
      *    *************************************************************
032780       ADD 1               TO W14-SUB.
032790	     MOVE SAL-SQN	 TO W14-SQN(W14-SUB).
      *    *************************************************************
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *    *************************************************************
032800 BD324.
032810	     MOVE WS-SALKEY	 TO WS-SALSTR.
032820       ADD 1               TO WS-SALKEY.
032830     IF WS-SALKEY > WS-LAST
032840         SUBTRACT 1        FROM WS-LAST
032850         GO TO BD272.
032860       PERFORM READ-SALE THRU READ-SALE-EXIT.
032870       SUBTRACT 1          FROM SAL-CODE.
032880       MOVE WS-SALSTR      TO WS-SALKEY.
032890       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
032900       ADD 1               TO WS-SALKEY.
032910	     GO TO BD324.

032920 BD325.
	     PERFORM BD289.
032930	   IF SAL-EXT-ITEM = SPACES
032940	       ACCEPT SAL-SDSC AT LINE LIN COLUMN 21
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
032950			   BACKGROUND-COLOR 0 UPDATE AUTO
032960	       GO TO BD581.

       BD330.
020730	     PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       ACCEPT S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *        ***** ALPHA-NUMERIC CODE *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   ACCEPT S07-ITM AT LINE LIN
	       ELSE
021070		   ACCEPT S07 AT LINE LIN
	       END-IF
	   ELSE
      *    *************************************************************
      *    ***** NUMERIC CODES APPLY *****
      *    *************************************************************
	   IF W10-SLNGTH = 3
	       ACCEPT S07-3 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S07-4 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S07-5 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S07-6 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S07-7 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S07-8 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S07-9 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S07-10 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S07-11 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S07-12 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S07-13 AT LINE LIN
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S07-14 AT LINE LIN.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *	   ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BD330
               END-EVALUATE
020770	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN.
020790	       IF W10-EXT-ITEM = SPACES
		   MOVE SAL-EXT-ITEM
				 TO W10-EXT-ITEM
020800		   GO TO BD330.

       BD335.
020840       PERFORM CA000.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
020850	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50 THRU AA59
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	       IF WS-EXT-STK = "Y"
		   DISPLAY S07-EXT AT LINE LIN
	       ELSE
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-USE-ITM = "Y"
		       DISPLAY S07-ITM AT LINE LIN
		   ELSE
021070		       DISPLAY S07 AT LINE LIN
		   END-IF
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
		   MOVE SAL-EXT-ITEM
				 TO W10-EXT-ITEM
020800		   GO TO BD330
               ELSE
		   GO TO BD335.
	   IF STK-IND = 4
	       MOVE "Header Record"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BD330.
      *    *************************************************************
      *    **** C H E C K   F O R   S A L E S	R E S T R I C T I O N S
      *    *************************************************************
	   IF WS-RESTRICT = "A" OR "E"
	       PERFORM MA000
	       IF ((WS-RESTCODE = "Y") AND (WS-RESTRICT = "E")) OR
		  ((WS-RESTCODE = "N") AND (WS-RESTRICT = "A"))
		   MOVE "Restricted for this Department"
                                 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
020140		   GO TO BD330.
020920	     MOVE STK-CODE	 TO SAL-EXT-ITEM.
020930	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *    *************************************************************
020940 BD340.
020950     IF STK-IND = 3
020960         MOVE ZERO         TO STK-AVGE
020970	       ACCEPT W10-SDSC AT LINE LIN COLUMN 21
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
020980			   BACKGROUND-COLOR 0 UPDATE AUTO
020990         DISPLAY "Index Unit cost" AT 2512
021000			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
021010         ACCEPT S26-AVGE AT LINE 25
021020     ELSE
021030         MOVE STK-DESC     TO W10-DESC
021040	       DISPLAY W10-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 0.
021050     IF STK-IND = 3
021060         IF STK-AVGE = ZERO
021070		   MOVE "Unit cost is zero"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
021090		   GO TO BD340.
021100	     DISPLAY CLEAR-L25.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *    *************************************************************
021110	     MOVE W10-EXT-ITEM	 TO SAL-EXT-ITEM.
021120	     MOVE W10-DESC	 TO SAL-MSER.
021130       MOVE STK-AVGE       TO SAL-COST W10-COST.
      *    *************************************************************
      *    ****    A M E N D   Q U A N T I T Y
      *    *************************************************************
       BD345.
032970       ACCEPT S26-QNT AT LINE LIN.
032990     IF W10-QUANT = ZERO
033000	       MOVE "Quantity MAY NOT be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033020         MOVE SAL-QNT      TO W10-QUANT
033030	       GO TO BD345.
      *    *************************************************************
      *    ****    A M E N D   S E L L I N G   P R I C E
      *    *************************************************************
	     MOVE "S"		 TO WS-LOOP.
	     PERFORM BD66A THRU BD102.
	     MOVE " "		 TO WS-LOOP.
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *
      *    The following code has been commented out, as the perform has
      *    been extended to include paragraphs up to BD102. The variable
      *    WS-LOOP has been included to facilitate this. This has been
      *    done to remove duplicated code, which will also reduce bugs
      *    when changes are made to the system.
      *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *	     GO TO BD581.
      *	     PERFORM BD66A THRU BD75.
027260*       MOVE ZERO 	  TO WS-DISC W90-CDISC.
      *    *************************************************************
      *    ****    R E C A L C U L A T E   V A L U E S
      *    *************************************************************
030820*       MULTIPLY W10-SELL BY W10-QUANT
      * 			  GIVING W25-VALUE.
030830*       MOVE W25-VALUE	  TO W100-EXT.
030840*	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
      *		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
      *			  BACKGROUND-COLOR 0.
      *	     PERFORM CHECK-CORRECT.
030930*	   IF WS-OPTION = "N"
030940*	       GO TO BD325.
      *    *************************************************************
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *    *************************************************************
033180*       MOVE W10-QUANT	  TO SAL-QNT.
      *       MOVE W10-SELL	  TO SAL-SELL.
033190*       MOVE W25-VALUE	  TO SAL-VAL.
033200*     IF WS-DISC = ZERO
033210*	       GO TO BD580.
033220*       COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
033230*       MOVE W90-CDISC	  TO SAL-DISC.
      *    *************************************************************
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *    *************************************************************
033240 BD580.
033250*	     PERFORM BD95 THRU BD102.

033260 BD581.
033280       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
033290       GO TO BD272.
      *    *************************************************************
      *    ****    C L E A R   D E T A I L   L I N E S
      *    *************************************************************
031940 BD582.
	     PERFORM BD295.
      *    *************************************************************
      *    ****    D I S P L A Y   L A S T   P A G E
      *    *************************************************************
	   IF WS-LAST > 10
	       SUBTRACT 9 FROM WS-LAST
				 GIVING WS-SALKEY
	   ELSE
	       MOVE 1		 TO WS-SALKEY.

030050 BD583.
030080	     PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
030100	   IF SAL-EXT-ITEM = SPACES
030110	       DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0
	       IF SAL-SUPER = 1
		   DISPLAY "(I)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 6 HIGHLIGHT
               ELSE
		   DISPLAY "(R)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 7 HIGHLIGHT
	       END-IF
030120	       GO TO BD584.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
030140	     DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
030150	     MOVE SAL-QNT	 TO W10-QUANT.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
       BD584.
	     ADD 1		 TO LIN.
030060	   IF WS-SALKEY < WS-LAST
	       ADD 1		 TO WS-SALKEY
030070	       GO TO BD583.

033300 BD590.
033310       EXIT.

       BD591A	       SECTION 51.
       BD591.
	     PERFORM SAVE-SCREEN.
	     MOVE 0815		 TO CPOS.
       BD592.
             DISPLAY 
		 "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
                 AT CPOS.
           IF CLIN < 18
               ADD 1             TO CLIN
	       GO TO BD592.
             DISPLAY S40. 
             PERFORM BB15 THRU BB29.
	     PERFORM RESTORE-SCREEN.
       BD593.
             EXIT.      
      /    *************************************************************
      *    ****    D E L E T E   O R D E R
      *    *************************************************************
033340 BE000         SECTION 52.
033350 BE00.
      *    *************************************************************
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *    *************************************************************
033360       MOVE 5              TO WS-PARKEY.
033370       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
033380       MOVE PAR-PW         TO W85-PASS. 
033390 BE05.
      *    *************************************************************
      *    ****    S A L E S   O R D E R   S C R E E N
      *    *************************************************************
033400       DISPLAY CLR-SCREEN.
033410       DISPLAY S30.
      *    *************************************************************
      *            C H A N G E   S C R E E N   H E A D I N G
      *    *************************************************************
033420       DISPLAY S31A.
033430       MOVE 8              TO LIN.
033440       PERFORM BE115 UNTIL LIN > 18.
033450       MOVE W12-TODAY      TO W90-DTE.
033460	     DISPLAY W90-DTE AT 0570
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033470       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
033480				    W90-BAL W90-SALES
033490				    W90-CUR W90-EXCTOT.
033500	     MOVE 1		 TO WS-FIRST WS-SALPOS WS-LAST.
033510       MOVE ZERO           TO W10-SORD.

033520 BE10.
033530       PERFORM AA400.
033540       ACCEPT S30A AT 0510.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE ZERO	 TO W10-SORD
		     GO TO BE599
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "O"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "T"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE10
	       END-EVALUATE
033560	       PERFORM AA50O THRU AA59
033570         DISPLAY S30A AT 0510
033580         IF W10-SORD = ZERO
033590             GO TO BE10.
033600	     DISPLAY CLEAR-L25.
033610     IF W10-SORD = ZERO
033620         GO TO BE599.
033630       MOVE W10-SORD       TO W90-REF.
033640 BE12.
033650       PERFORM GET-SORDER.
033660     IF WS-ERROR = 1
033670	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033680         GO TO BE10.
033690     IF WS-ERROR = 2
033700	       MOVE "Not an order"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033710         GO TO BE10.
033720     IF WS-ERROR = 3
033730	       MOVE "Order corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033750         GO TO BE10.
033760     IF WS-ERROR = 4
033770	       MOVE "Invalid a/c no"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
033780         GO TO BE10.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
033790       DISPLAY S36 AT 0308
033800	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033810	     DISPLAY W90-ORD AT 0368
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033820	     DISPLAY W10-SMAN AT 0411
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033830     IF W10-SMAN NOT = ZERO
033840         ADD 100 W10-SMAN    GIVING WS-PARKEY
033850         PERFORM READ-PARAM THRU READ-PARAM-EXIT
033860	       DISPLAY PAR-SALESMAN AT 0420
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	     DISPLAY W40-TEL AT 0466
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W40-CONTACT AT 0528
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033870       MOVE 1              TO WS-SALKEY.
033880       SUBTRACT 1          FROM WS-LAST.
033890 BE15.
033900       MOVE W90-DB         TO W90-TOTAL.
033910       SUBTRACT W90-LDISC  FROM W90-TOTAL.
033920       MOVE W90-DB         TO W100-BAL.
033930	     DISPLAY W100-BAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
033940       MULTIPLY -1 BY W90-LDISC 
033950                           GIVING W100-BAL.
033960	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *    *************************************************************
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *    *************************************************************
034000	   IF W90-TAX = SPACES
034010         GO TO BE20.
034030	   IF W90-EXCTOT NOT = ZERO
034060	       MOVE W90-TTAX	 TO W100-BAL W25-TOTAL
034070	       DISPLAY W100-BAL AT 2215
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
034080         ADD W25-TOTAL     TO W90-TOTAL.
034090 BE20.
034140	     MOVE W90-TOTAL	 TO W100-BAL.
034150	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *    *************************************************************
      *    ****    D I S P L A Y   S A L E S   D E T A I L S
      *    *************************************************************
034160 BE25.
034170       MOVE 8              TO LIN.
034180       PERFORM BD115 UNTIL LIN > 18.
034190       MOVE 8              TO LIN.
034200       MOVE 1              TO WS-LINES WS-FIRST.
034210 BE30.
034220	     MOVE WS-FIRST	 TO WS-SALKEY.
034230 BE35.
      *    *************************************************************
      *    H A V E   A L L   E N T R I E S   B E E N   D I S P L A Y E D
      *    *************************************************************
034240     IF WS-SALKEY > WS-LAST
034250         GO TO BE45.
034260       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *    *************************************************************
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *    *************************************************************
034280	   IF SAL-EXT-ITEM = SPACES
034290	       DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 0
               IF SAL-SUPER = 1
		   DISPLAY "(I)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 6 HIGHLIGHT
               ELSE
		   DISPLAY "(R)" AT LINE LIN COLUMN 5
			    WITH FOREGROUND-COLOR 7 HIGHLIGHT
               END-IF
034300         GO TO BE40.
      *    *************************************************************
      *        *****   E X T E N D E D	 C O D E   *****
      *    *************************************************************
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *    *************************************************************
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
034320	     DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
034330       MOVE SAL-QNT        TO W10-QUANT.
034340       DISPLAY S26-QNT AT LINE LIN.
034350       MOVE SAL-SELL       TO W10-SELL.
034360       DISPLAY S26-PRC AT LINE LIN.
034370       MOVE SAL-VAL        TO W100-EXT.
034380	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 0.
      *    *************************************************************
      *    ****    I S   T H E   S C R E E N   F U L L
      *    *************************************************************
034390 BE40.
034400     IF LIN < 18
034410         ADD 1             TO LIN WS-SALKEY WS-LINES
034420         GO TO BE35.
034430 BE45.
034440       EXIT.
034450 BE50.
      *    *************************************************************
      *    ****    D I S P L A Y   O P T I O N S
      *    *************************************************************
034460     IF WS-LINES < 11
034470         IF WS-FIRST = 1
034480		   MOVE "'D'elete order - 'E'xit"
				 TO WS-ERR-MES
034520	       ELSE
034530		   MOVE "'D'elete order - 'P'revious screen - 'E'xit"
				 TO WS-ERR-MES
034590		   MOVE 2	 TO WS-LINES
034600     ELSE
034610     IF WS-SALKEY < WS-LAST
034620         IF WS-FIRST = 1
034630		   MOVE  "'D'elete order - 'N'ext screen - 'E'xit"
				 TO WS-ERR-MES
034690		   MOVE 1	 TO WS-LINES
034700         ELSE
034710		   MOVE  "'D'elete ord, 'N'ext,'P'revious screen,'E'xit"
				 TO WS-ERR-MES
034790		   MOVE 3	 TO WS-LINES
034800     ELSE
034810         MOVE 2            TO WS-LINES
034820	       MOVE "'D'elete order - 'P'revious screen - 'E'xit"
				 TO WS-ERR-MES.
034880 BE55.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE
		     UNTIL WS-OPTION = "D" OR "E" OR "N" OR "P".
034970     IF WS-OPTION = "E"
034980         GO TO BE05.
034990     IF WS-OPTION = "D"
035000         GO TO BE65.
      *    *************************************************************
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *    *************************************************************
035010     IF WS-LINES = 2 OR 3
035020         IF WS-OPTION = "P"
035030             PERFORM BE60
035050             SUBTRACT 11   FROM WS-FIRST
035060             GO TO BE30.
      *    *************************************************************
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *    *************************************************************
035070     IF WS-LINES = 1 OR 3
035080         IF WS-OPTION = "N"
035090             PERFORM BE60
035110             ADD 11        TO WS-FIRST
035120             GO TO BE30.
035130       GO TO BE55.
035140 BE60.
      *    *************************************************************
      *    ****    C L E A R   S C R E E N
      *    *************************************************************
035150       MOVE 8              TO LIN.
035160       PERFORM BE115 UNTIL LIN > 18.
035170       MOVE 8              TO LIN.
035180       MOVE 1              TO WS-LINES.
      *    *************************************************************
      *    ****    D E L E T E   R E C O R D S   F R O M   O R D E R
      *            F I L E
      *    *************************************************************
035190 BE65.
035200	     DISPLAY "Deleting Order" AT 2520 WITH
035210		     BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
035220       MOVE ZERO           TO WS-SALKEY.
035230       PERFORM AY60 THRU AY999.
035240 BE70.
      *    *************************************************************
      *    ****    U P D A T E   O R D E R   F I L E 
      *    *************************************************************
035250       MOVE W90-REF        TO SOR-REF.
035260       MOVE ZERO           TO SOR-SQN.
035270       PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
035280     IF WS-F-ERROR = 20
035290         GO TO BE80.
035300 BE75.
035310	     PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
035320     IF WS-F-ERROR = 20
035330         GO TO BE80.
035340     IF SOR-REF NOT = W90-REF
035350         GO TO BE80.
	   IF SOR-TYPE = 1 OR 2
	      PERFORM BE90 THRU BE95.
035360       MOVE 2              TO WS-ACTION.
035370       PERFORM AY17 THRU AY59.
035380	     PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
035390       GO TO BE75.
035400 BE80.
035410       PERFORM AY70 THRU AY999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ To avoid problems of having data corrupted (MULTI USER)   ³
      *    ³ by the cache (buffering) of data on the individual Work   ³
      *    ³ stations. This results in data from one work station not  ³
      *    ³ being available on another Work station until the cache   ³
      *    ³ has been ceared. Including a close of the file after each ³
      *    ³ new order has been added and then re-opening the file is  ³
      *    ³ the way in which the system will release the data for it  ³
      *    ³ to be written to the server file and should resolve this  ³
      *    ³ problem.						       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
035420	     CLOSE SORDER
035430             SALE.
035440       OPEN I-O SORDER.
035450       OPEN OUTPUT SALE.
035460       CLOSE SALE.
035470       OPEN I-O SALE.
035480	     GO TO BE05.

       BE90.
	   IF SOR-EXT-ITEM = SPACES
	       GO TO BE95.
	     MOVE SOR-EXT-ITEM	 TO STK-CODE.
	     MOVE 0		 TO WS-ACTION.
	     PERFORM AY05 THRU AY59.
	     ADD SOR-QNT	 TO STK-QUANT.
	     SUBTRACT SOR-QNT	 FROM STK-QPIC.
017190	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.

       BE95.
	     EXIT.

035490 BE115.
036820	     DISPLAY WS-INV1 AT LINE LIN COLUMN 1
		     WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3
		     WS-INV78 WITH FOREGROUND-COLOR 3
		     WS-INV80 WITH BACKGROUND-COLOR 0
				   FOREGROUND-COLOR 3.
035510	     ADD 1		 TO LIN.

035520 BE599.
035530       EXIT.
      *    *************************************************************
      *    ****    C A L L   D E B T O R   C R E A T I O N
      *    *************************************************************
       GA000	       SECTION 53.
       GA00.
	     PERFORM SAVE-SCREEN.
004200	     CALL "DTP\DTP001"
		   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
	     CANCEL "DTP\DTP001".
	     PERFORM RESTORE-SCREEN.
       GA99.
             EXIT.
      /    *************************************************************
      *    **** 	S A L E S   R E S T R I C T I O N S	    ****
      *    *************************************************************
       MA000	       SECTION 54.
       MA00.
	     MOVE 1		 TO WS-S3.
	     MOVE "N"		 TO WS-RESTCODE.

       MA05.
	   IF STK-LDG = DBD-DEPT-CDE(WS-S3)
	       MOVE "Y" 	 TO WS-RESTCODE
	   ELSE
	   IF WS-S3 < 10
	       ADD 1		 TO WS-S3
	       GO TO MA05.

       MA999.
	     EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
038140 ZA000-INIT	       SECTION 90.
038150 ZA000-OPEN.
038160       PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     COPY FUNCTION.PRO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    *** D E B T O R   A L T E R N A T E	 I N D E X   F I L E ***
      *    *************************************************************
	     MOVE "DEBALT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBALT.
      *    *************************************************************
      *    **** D E B T O R   S A L E S   R E S T R I T I O N	F I L E 	  ****
      *    *************************************************************
	     MOVE "DEBDEP"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBDEP.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	  P R I N T   S P O O L   F I L E	    ****
      *    *************************************************************
	     MOVE "PRNDISK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-PRNDISK.
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
      *    **** 	  S A L E S   O R D E R   F I L E	    ****
      *    *************************************************************
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *	   *************************************************************
      *    **** 	  S T O C K   P A R T S   F I L E	    ****
      *    *************************************************************
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    **** 	   S T O C K   M E M O	 F I L E	    ****
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    **** 		U S E R S   F I L E		    ****
      *    *************************************************************
	     MOVE "SYSUSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSUSER.
049320	     MOVE 3		 TO WS-PARKEY.
049330       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT(PAR-STOCK = "A" OR "N")
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
	     MOVE PAR-INV	 TO PRG-NUMBER.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF TRM-PICP(WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
	   IF TRM-PICP(WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
	   IF TRM-PICP(WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
           ELSE
	   IF TRM-PICP(WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
	   IF TRM-PICP(WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
	   IF TRM-PICP(WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
	     ADD 501 TRM-PRN1(WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE TRM-CONP(WS-S6)
                                 TO WS-COND.
	     MOVE TRM-NORP(WS-S6)
                                 TO WS-NORM.
	     MOVE TRM-EXPP(WS-S6)
				 TO WS-EXPP.
	     MOVE TRM-ECAN(WS-S6)
                                 TO WS-ECAN.
	     MOVE TRM-8LPI(WS-S6)
                                 TO WS-8LPI.
	     MOVE TRM-6LPI(WS-S6)
                                 TO WS-6LPI.
	     MOVE TRM-10CPI(WS-S6)
                                 TO WS-10CPI.
	     MOVE TRM-12CPI(WS-S6)
                                 TO WS-12CPI.
	     MOVE TRM-17CPI(WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH(WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE(WS-S6)
				 TO WS-PGE-LENGTH.

       ZA01B.
038430       MOVE PAR-VAT-REG    TO W05-VATNO.
	     OPEN I-O DEBDEP.
042220	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 33	 TO WS-F-ERROR
                   GO TO ZA50.
042240     IF WS-STATUS NOT = "00"
042250	       MOVE 33		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
038440	     OPEN OUTPUT RECOVER.
038450       CLOSE RECOVER.
038460       OPEN I-O RECOVER.
038940	     OPEN OUTPUT SALE.
038950       CLOSE SALE.
038960       OPEN I-O SALE.
039390	     MOVE 5		 TO WS-PARKEY.
039400       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039430	     MOVE PAR-DISCOUNT	 TO W15-DISCOUNT.
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
	     MOVE PAR-PIC-PRC	 TO WS-PIC-PRC.
	     MOVE PAR-DELIV	 TO WS-DELIV.
	     MOVE PAR-PS-PRC	 TO WS-ITMPRC.
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT(PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT(PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	   IF NOT(PAR-RT-HD = X"00000000000000000000")
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
039440	     MOVE 1		 TO WS-PARKEY.
039450       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039460       MOVE PAR-DMY        TO W12-TODAY.
039470       MOVE PAR-YMD        TO W12-T-YMD.
039480       MOVE PAR-COMPANY    TO W95-COMP.
039500       MOVE PAR-TELEPHONE  TO W95-TEL.
039550	     ADD 1		 TO WS-PARKEY.
039560       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039570       MOVE PAR-ADDRESS1   TO W95-ADD1.
039580       MOVE PAR-ADDRESS2   TO W95-ADD2.
039590       MOVE PAR-ADDRESS3   TO W95-ADD3.
039600       MOVE PAR-ADDRESS4   TO W95-ADD4.
039610       MOVE PAR-POST-CDE   TO W95-POST.
039620     IF W95-ADD4 = SPACES
039630         MOVE W95-PC       TO W95-ADD4
039640         MOVE SPACES       TO W95-PC.
039650     IF W95-ADD3 = SPACES
039660         MOVE W95-ADD4     TO W95-ADD3
039670         MOVE SPACES       TO W95-ADD4.
039680     IF W95-ADD2 = SPACES
039690         MOVE W95-ADD3     TO W95-ADD2
039700         MOVE SPACES       TO W95-ADD3.
039710     IF W95-ADD1 = SPACES
039720         MOVE W95-ADD2     TO W95-ADD1
039730         MOVE SPACES       TO W95-ADD2.
039740       MOVE 7              TO WS-PARKEY.
039750       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
039760       MOVE PAR-CASH-AC    TO WS-CASH-AC.
	   IF WS-PRN-NO < 3
	       PERFORM ZA210 THRU ZA215
	       IF WS-ERROR = 1
		   GO TO ZA205
	   ELSE
	       DISPLAY "Waiting for printer response" AT 1412
			WITH FOREGROUND-COLOR 14 BLINK.
049880	     OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
	     MOVE WS-10CPI	 TO PRN-DETAIL2.
	     WRITE PRN-L1 BEFORE 0 LINES.
	     MOVE SPACES	 TO PRN-DETAIL2.
012670	     MOVE 1		 TO WS-NETKEY.
012680	     PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
      *    *************************************************************
      *    ****   A R E   S E P E R A T E   O R D E R	N U M B E R S
      * 	  P E R   U S E R   B E I N G	U S E D ?
      *    *************************************************************
	   IF W70-ORD-SYS = "Y"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		      Get the hard drive ID		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       CALL "PC_READ_DRIVE" USING WS-LOCAL-DRIVE
				    RETURNING WS-STATUS
	       CALL "CBL_TOUPPER" USING WS-LOCAL-DRIVE
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If the local drive is being used change the order file-id ³
      *    ³ to SORD??? (??? = USER NO)				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       IF LS-DSKDRV = "L"
      *    *************************************************************
      *    As local drives are not always C, the following test has been
      *    replaced with the instruction above which will be more reli-
      *    able if the workstation happens to have apac on a local drive
      *    other than C.
      *    *************************************************************
      *	       IF WS-LOCAL-DRIVE = "C"
		   CLOSE SORDER
		   MOVE W02-SORDER
				 TO WS-SORDER-STR
		   MOVE LS0-NO	 TO WS-USER-ID
		   MOVE WS-LOCAL-ID
				 TO W02-SORDER
		   MOVE WS-LOCAL-DRIVE
				 TO WS-LOCAL-DRIVE2
		   MOVE WS-LOCAL-ID2
				 TO W02-SYSUSER
		   OPEN I-O SORDER
		   OPEN I-O SYSUSER.
039770	     GO TO ZA999.

       COPY ZA49.PRO.

039850 ZA50.
055350	   IF WS-F-ERROR = 33
055360	       MOVE "DEBDEP file"
055370				 TO WS-FILE
	   END-IF.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

      *    *************************************************************
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *    *************************************************************
040040 ZA55.
040050	     MOVE 1		 TO WS-S1.
040060	     MOVE SPACES	 TO WS-MD-LINE.

040090 ZA60.
040130	     MOVE WS-G1 	 TO WS-TCA(WS-S1)   WS-BCA(WS-S1).
040150	   IF WS-S1 < 80
040160         ADD 1 TO WS-S1
040170         GO TO ZA60.
040220	     MOVE WS-G14	 TO WS-TCA(1)  WS-BCA(1).
040260	     MOVE WS-G13	 TO WS-TCA(80) WS-BCA(80).
040300	     MOVE WS-G2 	 TO WS-TCA(10) WS-TCA(17)
040360				    WS-TCA(47) WS-TCA(56)
040370				    WS-TCA(59) WS-TCA(68).
040400	     MOVE WS-G3 	 TO WS-MCA(1)  WS-MCA(80)
040470				    WS-MCA(10) WS-MCA(17)
040480				    WS-MCA(45) WS-MCA(56)
040490				    WS-MCA(59) WS-MCA(68).
040550	     MOVE WS-G4 	 TO WS-BCA(10) WS-BCA(17)
040610				    WS-BCA(47) WS-BCA(56)
040620				    WS-BCA(59) WS-BCA(68).
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

040660 ZA200.

       COPY LOCKED.PRO.

040730 ZA205.
040740       EXIT PROGRAM.

       ZA210.
	     MOVE ZERO		 TO WS-ERROR
				    WS-S1 WS-S2 WS-S3 WS-S4
				    WS-S5 WS-S6 WS-S7 WS-S8.
	     CALL "PC_TEST_PRINTER" USING WS-PRN-NO
					  WS-PRN-STAT
				    RETURNING WS-STATUS.
	   IF WS-PRN-STAT > 127
	       MOVE 1		 TO WS-S1
	       SUBTRACT 128	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 63
	       MOVE 1		 TO WS-S2
	       SUBTRACT 64	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 31
	       MOVE 1		 TO WS-S3
	       SUBTRACT 32	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 15
	       MOVE 1		 TO WS-S4
	       SUBTRACT 16	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 7
	       MOVE 1		 TO WS-S5
	       SUBTRACT 8	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 3
	       MOVE 1		 TO WS-S6
	       SUBTRACT 4	 FROM WS-PRN-STAT.
	   IF WS-PRN-STAT > 1
	       MOVE 1		 TO WS-S7
	       SUBTRACT 2	 FROM WS-PRN-STAT.
	     MOVE WS-PRN-STAT	 TO WS-S8.
	   IF WS-S3 = 0 AND
	      WS-S5 = 0 AND
	      WS-S8 = 0
	       GO TO ZA215.
	     DISPLAY "Check printer - " AT 2304
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "R" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "etry - "
		     "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
		     "bort".
       ZA212.
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "A" OR "R"
	       ACCEPT WS-OPTION AT 2334
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	   IF WS-OPTION = "R"
	       GO TO ZA210.
	     MOVE 1		 TO WS-ERROR.

       ZA215.
	     EXIT.

040750 ZA999.
040760       EXIT.

040780 I-O-ERRORS      SECTION  91.

       COPY ERRORS.PRO.

040950 DISPLAY-FILE-NAME.
040960	   IF WS-F-ERROR = 2
040970	       MOVE W02-NETWORK  TO WS-FILE
040980	       MOVE WS-NETKEY	 TO WS-KEY
041760	   ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041070     ELSE
041080     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041110	   ELSE
041120     IF WS-F-ERROR = 15
041130         MOVE WS-PARID     TO WS-FILE
041140         MOVE WS-PARKEY    TO WS-KEY
041150     ELSE
041160     IF WS-F-ERROR = 18
041170	       MOVE W02-RECOVER  TO WS-FILE
041180         MOVE WS-RECKEY    TO WS-KEY
041190     ELSE
041200     IF WS-F-ERROR = 19
041210         MOVE W02-SALE TO WS-FILE
041220         MOVE WS-SALKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 20
052210         MOVE W02-SORDER   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE SOR-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE PRT-EXT-ITEM TO WS-KEYX
041310     ELSE
041320     IF WS-F-ERROR = 22
041330         MOVE W02-STOCKF   TO WS-FILE
041340         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE SME-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041760	   ELSE
041770	   IF WS-F-ERROR = 33
041780	       MOVE W02-DEBDEP	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE DBD-ACNO	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 47
041860	       MOVE W02-DEBALT	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DAL-KEY	 TO WS-KEYX
	   ELSE
041120	   IF WS-F-ERROR = 64
041130	       MOVE W02-SYSUSER	 TO WS-FILE
041140	       MOVE USE-NO	 TO WS-KEY.

       COPY DISPERR.PRO.
