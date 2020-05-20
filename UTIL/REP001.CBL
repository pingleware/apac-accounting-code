      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****       **      *
      *   **    **  **        **    **   **  **    **  **     ***      *
      *   **    **  **        **    **  **    **  **    **     **      *
      *   ******    *****     *******   **    **  **    **     **      *
      *   **   **   **        **        **    **  **    **     **      *
      *   **    **  **        **         **  **    **  **      **      *
      *   **    **  ********  **          ****      ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *       P E R I O D - E N D   &   S T A T S   P R O G R A M      *
      *                                                                *
      *     VERSION 8.15.02 - December 2011			       *
      * 							       *
      ******************************************************************
      /
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     REP001.
000030 AUTHOR.	       James William Lemmon.
000040 DATE-WRITTEN.   AUGUST 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2012
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

000060 ENVIRONMENT DIVISION.
000070 CONFIGURATION SECTION.
000080 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000420	   SELECT AUDIT   ASSIGN DISK
000430                    LOCK EXCLUSIVE
000440                    STATUS WS-STATUS
000450                    ACCESS RANDOM
000460                    ORGANIZATION RELATIVE
000470                    RELATIVE WS-AUDKEY.
000110	   SELECT NETWORK ASSIGN DISK
                          LOCK EXCLUSIVE
000120                    STATUS WS-STATUS
000130                    ACCESS RANDOM
000140                    ORGANIZATION RELATIVE
000150                    RELATIVE WS-NETKEY.
      $SET KEYCOMPRESS "7"
000230	   SELECT CREDIT  ASSIGN DISK
                          LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD P-NUMBER
                          ALTERNATE RECORD P-NKEY DUPLICATES.
000170	   SELECT DEBTOR  ASSIGN DISK
000190                    LOCK EXCLUSIVE
000180                    STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210                    RECORD DEB-ACNO
                          ALTERNATE RECORD DEB-NKEY DUPLICATES.
000110	   SELECT DEPART  ASSIGN DISK
                          LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000190                    ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD DPT-CODE
			  ALTERNATE RECORD DPT-NKEY DUPLICATES.
      $SET NOKEYCOMPRESS
000120	   SELECT PARAM   ASSIGN DISK
000130                    LOCK EXCLUSIVE
000140                    STATUS WS-STATUS
000150                    ACCESS RANDOM
000160                    ORGANIZATION RELATIVE
000170                    RELATIVE WS-PARKEY.
      $SET KEYCOMPRESS "7"
000010	   SELECT SLIP	  ASSIGN DISK
000020			  LOCK EXCLUSIVE
000030                    STATUS WS-STATUS
000190			  ACCESS DYNAMIC
000200                    ORGANIZATION INDEXED
000210			  RECORD SLP-KEY
			  ALTERNATE RECORD SLP-TKEY =
			     SLP-TYPE SLP-KEY
			  ALTERNATE RECORD SLP-DKEY =
			     SLP-DRAW SLP-KEY.
000230	   SELECT SORDER  ASSIGN DISK
                          LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD SOR-KEY
                          ALTERNATE RECORD SOR-AKEY = 
                            SOR-AC SOR-KEY DUPLICATES.
000300	   SELECT STOCK   ASSIGN DISK
000310                    LOCK EXCLUSIVE
000320                    STATUS WS-STATUS
000130                    ACCESS DYNAMIC
000140                    ORGANIZATION INDEXED
000070			  RECORD STK-CODE
000080                    ALTERNATE RECORD STK-ACODE DUPLICATES
000090                    ALTERNATE RECORD STK-BSEQ = 
000100				    STK-BIN STK-CODE
000110                    ALTERNATE RECORD STK-LSEQ =
000120				    STK-LDG STK-CODE
000130                    ALTERNATE RECORD STK-DKEY DUPLICATES
000130			  ALTERNATE RECORD STK-DKEY2 DUPLICATES
000140                    ALTERNATE RECORD STK-XREF DUPLICATES.
000230	   SELECT TXTRAN  ASSIGN DISK
                          LOCK EXCLUSIVE
000240                    STATUS WS-STATUS
000250                    ACCESS DYNAMIC
000260                    ORGANIZATION INDEXED
000270                    RECORD TAX-KEY.
      $SET NOKEYCOMPRESS

000200	   SELECT VATREP  ASSIGN W02-VATREP
000040                    STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

000490 DATA DIVISION.
000500 FILE SECTION.

       COPY APACFIDS.FDE.

000620 COPY AUDIT.IFD.

000520 COPY CONTROL.IFD.

000540 COPY CREDIT.GFD.

000540 COPY DEBTOR.GFD.

000520 COPY DEPART.GFD.

000520 COPY PARAM.IFD.

       COPY SLIP.FD.

       COPY SORDER.FD.

000560 COPY STOCK.GFD.

       COPY TXTRAN.IFD.

       FD  VATREP    LABEL RECORD OMITTED.
       01  VAT-REC1.
	   03  EXP-DATA     PIC  X(136).

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
003220 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
003230 77  WS-CLEAR	   PIC 9(01)	 VALUE ZERO.
       77  WS-AUD	   PIC	9(06)	 COMP-5 VALUE ZERO.
003240 77  WS-S1           PIC 9(04)     COMP-5.
003250 77  WS-S2           PIC 9(04)     COMP-5.
003260 77  WS-S3           PIC 9(04)     COMP-5.
003270 77  WS-S4           PIC 9(04)     COMP-5.
003280 77  WS-S5           PIC 9(04)     COMP-5.
003290 77  WS-S6           PIC 9(04)     COMP-5.
003290 77  WS-S7	   PIC 9(04)	 COMP-5.
003300 77  WS-NO           PIC 9(04)     COMP-5.
003320 77  WS-PARKEY	   PIC 9(04)	 COMP-5.
003330 77  WS-NETKEY	   PIC 9(04)	 COMP-5.
003360 77  WS-AUDKEY	   PIC 9(06)	 COMP-5.
003370 77  WS-INDEX        PIC 9(04)     COMP-5.
003380 77  WS-INDTYPE      PIC 9(01).
       77  WS-USE-ITM	   PIC	X(01).
003390 77  WS-LINES        PIC 9(02) VALUE 66.
003400 77  WS-PAGE         PIC 9(04)     COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003430 77  WS-GST-RATE	   PIC	9(02)V99   COMP-3.
003440 77  WS-OPTION       PIC  X(01).
       77  WS-STATOP       PIC  X(01).
       77  WS-VATTYPE	   PIC	X(01).
003460 77  WS-REPLY        PIC  X(01).
003470 77  WS-ERROR        PIC  9(01).
003500 77  WS-CHAR1        PIC  X(01).
003510 77  WS-CHAR2        PIC  X(01).
003520 77  WS-COMP         PIC  9(01).
003530 77  WS-TYPE         PIC  X(01).
003550 77  WS-LEDG         PIC  9(03).
003570 77  WS-NUM          PIC  Z9.
003570 77  WS-REF          PIC  X(08).
003590 77  WS-BATCH        PIC  9(05)      COMP-3.
003620 77  WS-REP1	   PIC 9(01) VALUE 0.
003630 77  WS-REP2         PIC 9(01) VALUE 0.
003640 77  WS-REP3         PIC 9(01) VALUE 0.
003650 77  WS-REP4         PIC 9(01) VALUE 0.
003660 77  WS-REP5         PIC 9(01) VALUE 0.
003670 77  WS-REP6         PIC 9(01) VALUE 0.
003680 77  WS-REP7         PIC 9(01) VALUE 0.
003690 77  WS-REP8         PIC 9(01) VALUE 0.
003700 77  WS-REP9         PIC 9(01) VALUE 0.
003710 77  WS-REP10        PIC 9(01) VALUE 0.
003710 77  WS-REP11	   PIC 9(01) VALUE 0.
003720 77  WS-H1	   PIC X(18) VALUE "*** END REPORT ***".
003730 77  WS-H2           PIC X(06) VALUE "M.T.D.".
003740 77  WS-H3           PIC X(06) VALUE "Y.T.D.".
003750 77  WS-H4           PIC X(05) VALUE "TOTAL".
003760 77  WS-H5           PIC X(06) VALUE "CREDIT".
003770 77  WS-H6           PIC X(05) VALUE "DEBIT".
003780 77  WS-H7           PIC X(09) VALUE "ITEM CODE".
003790 77  WS-H8           PIC X(11) VALUE "DESCRIPTION".
003800 77  WS-H9           PIC X(05) VALUE "QUANT".
003810 77  WS-H10          PIC X(04) VALUE "CASH".
003820 77  WS-H11          PIC X(05) VALUE "VALUE".
003830 77  WS-H12          PIC X(03) VALUE "NO.".
003840 77  WS-H13          PIC X(03) VALUE "MTD".
003850 77  WS-H14          PIC X(03) VALUE "YTD".
003860 77  WS-H15          PIC X(08) VALUE "G/LEDGER".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

003880 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003900 01  W02-FID.

       COPY APACFIDS.ID.

       COPY AUDIT.ID.

       COPY CONTROL.ID.

000540 COPY CREDIT.ID.

       COPY DEPART.ID.

       COPY DEBTOR.ID.

       COPY PARAM.ID.

       COPY SLIP.ID.

       COPY SORDER.ID.

       COPY STOCK.ID.

       COPY TXTRAN.ID.

       01  W02-VATREP.
	   03  W02-VATNAME     PIC  X(12) VALUE "VATXXXXX.CSV".

       01  W02-PRINTER-DETAILS.
	   03  W02-PRINTER     PIC  X(12).
002570	   03  W02-PGE-LENGTH  PIC  9(02).
001870	   03  W02-PRN-LENGTH  PIC  9(02).
	   03  W02-LINAGE      PIC  9(02).
	   03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      * 	   C  =  Condensed print
      * 	   E  =  Expanded print
      * 	   H  =  Header line
      * 	   X  =  Cancel expanded print
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).

000660	   03  R-L1 REDEFINES W02-PRN-LINE.
000670	       05  R-DETAIL	   PIC	X(136).

000680	   03  R-L2 REDEFINES W02-PRN-LINE.
000690	       05  R-PGE	   PIC	X(06).
000700	       05  R-PNO	   PIC	Z(03)9.
000710	       05  FILLER	   PIC	X(09).
000720	       05  R-CPY	   PIC	X(40).
000730	       05  FILLER	   PIC	X(05).
000740	       05  R-DATE	   PIC	X(06).
000750	       05  R-DTE	   PIC	Z9/99/9999.

000760	   03  R-L2A REDEFINES W02-PRN-LINE.
000770	       05  RC-PGE	   PIC	X(06).
000780	       05  RC-PNO	   PIC	Z(03)9.
000790	       05  FILLER	   PIC	X(33).
000800	       05  RC-CPY	   PIC	X(40).
000810	       05  FILLER	   PIC	X(33).
000820	       05  RC-DATE	   PIC	X(06).
000830	       05  RC-DTE	   PIC	Z9/99/9999.

000840	   03  R-L3 REDEFINES W02-PRN-LINE.
000850	       05  FILLER	   PIC	X(30).
000860	       05  R-HEAD	   PIC	X(20).
000870	       05  FILLER	   PIC	X(30).

000880	   03  R-L3A REDEFINES W02-PRN-LINE.
000890	       05  FILLER	   PIC	X(56).
000900	       05  RC-HEAD	   PIC	X(20).
000910	       05  FILLER	   PIC	X(56).

000920	   03  R-L4 REDEFINES W02-PRN-LINE.
000930	       05  FILLER	   PIC	X(02).
000940	       05  R-H1 	   PIC	X(17).
000950	       05  R-H2 	   PIC	X(27).
000960	       05  R-H3 	   PIC	X(09).
000970	       05  R-H4 	   PIC	X(17).
000980	       05  R-H5 	   PIC	X(08).

000990	   03  R-L5 REDEFINES W02-PRN-LINE.
001000	       05  FILLER	   PIC	X(02).
001010	       05  R-I1 	   PIC	X(15).
001020	       05  FILLER	   PIC	X(02).
001030	       05  R-DESC1	   PIC	X(24).
001040	       05  FILLER	   PIC	X(01).
001050	       05  R-Q1 	   PIC	Z(06)9.
001060	       05  FILLER	   PIC	X(02).
001070	       05  R-UP1	   PIC	Z(07)9.99.
001080	       05  FILLER	   PIC	X(02).
001090	       05  R-V1 	   PIC	Z(08)9.99.
001100	       05  FILLER	   PIC	X(02).

001110	   03  R-L6 REDEFINES W02-PRN-LINE.
001120	       05  R-H6 	   PIC	X(19).
001130	       05  R-H7 	   PIC	X(31).
001140	       05  R-H8 	   PIC	X(07).
001150	       05  R-H9 	   PIC	X(09).
001160	       05  R-H10	   PIC	X(08).
001170	       05  R-H11	   PIC	X(11).
001180	       05  R-H12	   PIC	X(07).
001190	       05  R-H13	   PIC	X(04).

001200	   03  R-L7 REDEFINES W02-PRN-LINE.
001210	       05  R-I2.
001780		   07  R-ITM	   PIC	X(14).
		   07  R-SL	   PIC	X(01).
		   07  R-EXT	   PIC	X(03).
001220	       05  FILLER	   PIC	X(01).
001230	       05  R-DESC2	   PIC	X(29).
001270	       05  FILLER	   PIC	X(01).
001240	       05  R-LEV	   PIC	Z(05)9.
001250	       05  FILLER	   PIC	X(01).
001260	       05  R-Q2 	   PIC	Z(05)9.
001270	       05  FILLER	   PIC	X(01).
001280	       05  R-COST1	   PIC	Z(05)9.99.
001290	       05  FILLER	   PIC	X(01).
001300	       05  R-AVGE	   PIC	Z(05)9.99.
001310	       05  FILLER	   PIC	X(01).
001320	       05  R-NMTD	   PIC	Z(05)9.
001330	       05  FILLER	   PIC	X(01).
001340	       05  R-NYTD	   PIC	Z(05)9.

001350	   03  R-L8 REDEFINES W02-PRN-LINE.
001360	       05  R-H14	   PIC	X(07).
001370	       05  R-H14A	   PIC	X(41).
001380	       05  R-H15	   PIC	X(11).
001390	       05  R-H16	   PIC	X(13).
001400	       05  R-H17	   PIC	X(08).

001410	   03  R-L9 REDEFINES W02-PRN-LINE.
001420	       05  R-AC 	   PIC	X(06).
001430	       05  FILLER	   PIC	X(01).
001440	       05  R-DNME	   PIC	X(39).
001450	       05  FILLER	   PIC	X(01).
001460	       05  R-REF	   PIC	X(08).
001470	       05  FILLER	   PIC	X(01).
001480	       05  R-DEBT	   PIC	Z(06)9.99-.
001490	       05  FILLER	   PIC	X(02).
001500	       05  R-CASH	   PIC	Z(06)9.99-.

001510	   03  R-L10 REDEFINES W02-PRN-LINE.
001520	       05  R-H18	   PIC	X(06).
001530	       05  R-H19	   PIC	X(32).
001540	       05  R-H20	   PIC	X(12).
001550	       05  R-H21	   PIC	X(12).
001560	       05  R-H22	   PIC	X(12).
001570	       05  R-H23	   PIC	X(06).

001510	   03  R-L10-TO REDEFINES W02-PRN-LINE.
001520	       05  RT-H18	   PIC	X(05).
001530	       05  RT-H19	   PIC	X(35).
001540	       05  RT-H20	   PIC	X(14).
001550	       05  RT-H21	   PIC	X(10).
001560	       05  RT-H22	   PIC	X(10).
001570	       05  RT-H23	   PIC	X(06).

001580	   03  R-L10A REDEFINES W02-PRN-LINE.
001590	       05  FILLER	   PIC	X(04).
001600	       05  RC-H18	   PIC	X(09).
001610	       05  RC-H19	   PIC	X(38).
001620	       05  RC-H20	   PIC	X(14).
001630	       05  RC-H21	   PIC	X(14).
001640	       05  RC-H22	   PIC	X(14).
001650	       05  RC-H23	   PIC	X(12).
001660	       05  RC-H24	   PIC	X(13).
001670	       05  RC-H25	   PIC	X(14).

001680	   03  R-L10B REDEFINES W02-PRN-LINE.
001690	       05  RS-H18	   PIC	X(04).
001700	       05  RS-H19	   PIC	X(36).
001710	       05  RS-H20	   PIC	X(13).
001720	       05  RS-H21	   PIC	X(13).
001730	       05  RS-H22	   PIC	X(13).
001740	       05  RS-H23	   PIC	X(13).
001750	       05  RS-H23A	   PIC	X(13).
001760	       05  RS-H23B	   PIC	X(11).
001770	       05  RS-H24	   PIC	X(08).
001780	       05  RS-H25	   PIC	X(08).

001580	   03  R-L10C REDEFINES W02-PRN-LINE.
001590	       05  FILLER	   PIC	X(01).
001600	       05  RP-H18	   PIC	X(08).
001610	       05  RP-H19	   PIC	X(37).
001620	       05  RP-H20	   PIC	X(13).
001630	       05  RP-H21	   PIC	X(13).
001640	       05  RP-H22	   PIC	X(13).
001650	       05  RP-H23	   PIC	X(09).
001650	       05  RP-H23A	   PIC	X(15).
001660	       05  RP-H24	   PIC	X(13).
001670	       05  RP-H25	   PIC	X(10).

001790	   03  R-L11 REDEFINES W02-PRN-LINE.
001800	       05  FILLER	   PIC	X(46).
001810	       05  R-H24	   PIC	X(21).
001820	       05  R-H25	   PIC	X(13).

001830	   03  R-L11A REDEFINES W02-PRN-LINE.
001840	       05  FILLER	   PIC	X(47).
001850	       05  RS-H26	   PIC	X(25).
001860	       05  RS-H27	   PIC	X(26).
001870	       05  RS-H27A	   PIC	X(22).
001880	       05  RS-H28	   PIC	X(12).

001890	   03  R-L11B REDEFINES W02-PRN-LINE.
001900	       05  FILLER	   PIC	X(56).
001910	       05  RC-H26	   PIC	X(28).
001920	       05  RC-H27	   PIC	X(27).
001930	       05  RC-H28	   PIC	X(21).

001890	   03  R-L11C REDEFINES W02-PRN-LINE.
001900	       05  FILLER	   PIC	X(51).
001910	       05  RP-H26	   PIC	X(26).
001920	       05  RP-H27	   PIC	X(37).
001930	       05  RP-H28	   PIC	X(18).

001940	   03  R-L12 REDEFINES W02-PRN-LINE.
001950	       05  FILLER	   PIC	X(01).
001960	       05  R-LEDG	   PIC	9(03).
001970	       05  FILLER	   PIC	X(01).
001980	       05  R-SDESC	   PIC	X(27).
001970	       05  FILLER	   PIC	X(01).
001990	       05  R-DMTD	   PIC	-(07)9.99.
002000	       05  FILLER	   PIC	X(01).
002010	       05  R-DYTD	   PIC	-(07)9.99.
002020	       05  FILLER	   PIC	X(01).
002030	       05  R-CMTD	   PIC	-(07)9.99.
002040	       05  FILLER	   PIC	X(01).
002050	       05  R-CYTD	   PIC	-(07)9.99.

001940	   03  R-L12-TO REDEFINES W02-PRN-LINE.
001960	       05  RT-LEDG	   PIC	X(04).
001970	       05  FILLER	   PIC	X(01).
001980	       05  RT-SDESC	   PIC	X(27).
001970	       05  FILLER	   PIC	X(01).
001990	       05  RT-DMTD	   PIC	-(09)9.99.
002000	       05  FILLER	   PIC	X(01).
002010	       05  RT-DYTD	   PIC	-(09)9.99.
002020	       05  FILLER	   PIC	X(01).
002030	       05  RT-MUP	   PIC	-(05)9.99.
002040	       05  FILLER	   PIC	X(01).
002050	       05  RT-GP	   PIC	-(05)9.99.

002060	   03  R-L12A REDEFINES W02-PRN-LINE.
002070	       05  FILLER	   PIC	X(04).
002080	       05  RC-LEDG	   PIC	X(04).
002090	       05  FILLER	   PIC	X(05).
002100	       05  RC-SDESC	   PIC	X(32).
002110	       05  RC-DMTD	   PIC	Z(08)9.99.
002120	       05  FILLER	   PIC	X(02).
002130	       05  RC-DYTD	   PIC	Z(08)9.99.
002140	       05  FILLER	   PIC	X(02).
002150	       05  RC-CMTD	   PIC	Z(08)9.99.
002160	       05  FILLER	   PIC	X(02).
002170	       05  RC-CYTD	   PIC	Z(08)9.99.
002180	       05  FILLER	   PIC	X(07).
002190	       05  RC-GL1	   PIC	9999/99.
002200	       05  FILLER	   PIC	X(06).
002210	       05  RC-GL2	   PIC	9999/99.
002220	       05  FILLER	   PIC	X(06).

002230	   03  R-L12B REDEFINES W02-PRN-LINE.
002240	       05  RS-LEDG	   PIC	X(04).
002250	       05  FILLER	   PIC	X(01).
002260	       05  RS-SDESC	   PIC	X(28).
002250	       05  FILLER	   PIC	X(01).
002270	       05  RS-DMTD	   PIC	Z(08)9.99.
002280	       05  FILLER	   PIC	X(01).
002290	       05  RS-DYTD	   PIC	Z(08)9.99.
002300	       05  FILLER	   PIC	X(01).
002310	       05  RS-CMTD	   PIC	Z(08)9.99-.
002320	       05  FILLER	   PIC	X(01).
002330	       05  RS-CYTD	   PIC	Z(08)9.99-.
002340	       05  FILLER	   PIC	X(01).
002350	       05  RS-DSMT	   PIC	Z(08)9.99-.
002360	       05  FILLER	   PIC	X(01).
002370	       05  RS-DSYT	   PIC	Z(08)9.99-.
002380	       05  FILLER	   PIC	X(01).
002390	       05  RS-GL1	   PIC	9999/99.
002400	       05  FILLER	   PIC	X(01).
002410	       05  RS-GL2	   PIC	9999/99.

002060	   03  R-L12C REDEFINES W02-PRN-LINE.
002070	       05  FILLER	   PIC	X(03).
002080	       05  RP-LEDG	   PIC	X(04).
002090	       05  FILLER	   PIC	X(02).
002100	       05  RP-SDESC	   PIC	X(31).
002110	       05  RP-DMTD	   PIC	Z(08)9.99.
002120	       05  FILLER	   PIC	X(01).
002130	       05  RP-DYTD	   PIC	Z(08)9.99.
002140	       05  FILLER	   PIC	X(01).
002150	       05  RP-CMTD	   PIC	Z(08)9.99.
002160	       05  FILLER	   PIC	X(01).
002170	       05  RP-CYTD	   PIC	Z(08)9.99.
002160	       05  FILLER	   PIC	X(01).
002170	       05  RP-ADJ	   PIC	Z(08)9.99-.
002180	       05  FILLER	   PIC	X(03).
002190	       05  RP-GL1	   PIC	9999/99.
002200	       05  FILLER	   PIC	X(06).
002210	       05  RP-GL2	   PIC	9999/99.
002220	       05  FILLER	   PIC	X(04).

002420	   03  R-L13 REDEFINES W02-PRN-LINE.
002430	       05  R-H26	   PIC	X(20).
002440	       05  FILLER	   PIC	X(02).
002450	       05  R-H27	   PIC	X(05).
002460	       05  R-H28	   PIC	X(06).
002470	       05  R-H29	   PIC	X(09).
002480	       05  R-H30	   PIC	X(06).
002490	       05  R-H31	   PIC	X(05).
002500	       05  R-H32	   PIC	X(10).
002510	       05  R-H33	   PIC	X(05).
002520	       05  R-H34	   PIC	X(06).
002530	       05  R-H35	   PIC	X(06).

002540	   03  R-L14 REDEFINES W02-PRN-LINE.
002550	       05  FILLER	   PIC	X(05).
002560	       05  R-H36	   PIC	X(15).
002570	       05  R-N1 	   PIC	Z(04)9.
	       05  R-VAT REDEFINES R-N1
				   PIC	Z9.99.
	       05  R-PERC	   PIC	X(01).
002580	       05  FILLER	   PIC	X(01).
002590	       05  R-V5 	   PIC	Z(08)9.99-.
002600	       05  FILLER	   PIC	X(01).
002610	       05  R-N2 	   PIC	Z(04)9.
002620	       05  FILLER	   PIC	X(01).
002630	       05  R-V6 	   PIC	Z(08)9.99-.
002640	       05  FILLER	   PIC	X(01).
002650	       05  R-N3 	   PIC	Z(04)9.
002660	       05  FILLER	   PIC	X(01).
002670	       05  R-V7 	   PIC	Z(08)9.99-.

000760	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000790	       05  REP-COMPANY	   PIC	X(40).
000800	       05  REP-DATE	   PIC	X(06).
000810	       05  REP-DTE	   PIC	Z9/99/9999.
000820	       05  FILLER	   PIC	X(16).

000830	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
000870	       05  REP-REMK	   PIC	X(20).
000880	       05  FILLER	   PIC	X(01).
	       05  REP-VAL-NO.
000890		   07  REP-VAL	   PIC	Z(06)9.99-.
	       05  REP-VAL-NO-R    REDEFINES REP-VAL-NO.
		   07  REP-NO	   PIC	Z(06)9.
		   07  FILLER	   PIC	X(04).
000900	       05  FILLER	   PIC	X(24).

002680	   03  SAL-L1 REDEFINES W02-PRN-LINE.
002690	       05  SAL-H1	   PIC	X(10).
002700	       05  SAL-NO	   PIC	ZZ9.
002710	       05  FILLER	   PIC	X(01).
002720	       05  SAL-SMAN	   PIC	X(30).
002730	       05  FILLER	   PIC	X(36).

002740	   03  SAL-L2 REDEFINES W02-PRN-LINE.
002760	       05  SAL-H2	   PIC	X(15).
002770	       05  SAL-H3	   PIC	X(36).
002780	       05  SAL-H4	   PIC	X(09).
002790	       05  SAL-H5	   PIC	X(11).
002800	       05  SAL-H6	   PIC	X(08).
	       05  SAL-H6A	   PIC	X(09).
	       05  SAL-H6B	   PIC	X(06).

002810	   03  SAL-L3 REDEFINES W02-PRN-LINE.
002830	       05  SAL-ITEM	   PIC	X(14).
002840	       05  FILLER	   PIC	X(01).
002850	       05  SAL-DESC	   PIC	X(28).
002860	       05  FILLER	   PIC	X(01).
002870	       05  SAL-COST	   PIC	Z(07)9.99-.
002880	       05  FILLER	   PIC	X(01).
002890	       05  SAL-SELL	   PIC	Z(07)9.99-.
002900	       05  FILLER	   PIC	X(01).
002910	       05  SAL-GP	   PIC	ZZ9.99-.
002920	       05  FILLER	   PIC	X(02).
	       05  SAL-MARKUP	   PIC	ZZ9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SAL-NOCHARGE.
		 07  FILLER	   PIC	X(01).
		 07  SAL-SUPER	   PIC	ZZ.
	       05  FILLER	   PIC	X(01).
	       05  SAL-PAY	   PIC	X(03).

002930	   03  SAL-L4 REDEFINES W02-PRN-LINE.
002940	       05  FILLER	   PIC	X(02).
002950	       05  SAL-H7	   PIC	X(14).
002960	       05  SAL-REF	   PIC	X(08).
002970	       05  FILLER	   PIC	X(02).
002980	       05  SAL-H8	   PIC	X(22).
002990	       05  FILLER	   PIC	X(30).

003000	   03  SAL-L5 REDEFINES W02-PRN-LINE.
003010	       05  FILLER	   PIC	X(02).
003020	       05  SAL-H9	   PIC	X(10).
003030	       05  SAL-H10	   PIC	X(20).
003040	       05  SAL-H11	   PIC	X(12).
003050	       05  SAL-H12	   PIC	X(16).
003060	       05  SAL-H13	   PIC	X(13).
003070	       05  SAL-H14	   PIC	X(07).

003080	   03  SAL-L6 REDEFINES W02-PRN-LINE.
003090	       05  FILLER	   PIC	X(01).
003100	       05  SAL-RF	   PIC	X(08).
003110	       05  FILLER	   PIC	X(03).
003120	       05  SAL-DSC	   PIC	X(11).
003130	       05  FILLER	   PIC	X(02).
003140	       05  SAL-CST	   PIC	Z(08)9.99-.
003150	       05  FILLER	   PIC	X(02).
003160	       05  SAL-SEL	   PIC	Z(08)9.99-.
003170	       05  FILLER	   PIC	X(02).
003180	       05  SAL-PRF	   PIC	Z(08)9.99-.
003190	       05  FILLER	   PIC	X(04).
003200	       05  SAL-GPR	   PIC	ZZ9.99.
003210	       05  FILLER	   PIC	X(02).

	   03  VAT-L1 REDEFINES W02-PRN-LINE.
	       05  VAT-DAT	   PIC	99/99/9999.
	       05  FILLER	   PIC	X(01).
	       05  VAT-REF	   PIC	X(08).
	       05  FILLER	   PIC	X(01).
	       05  VAT-TRAN	   PIC	X(11).
	       05  FILLER	   PIC	X(01).
	       05  VAT-NAME.
		   07  FILLER	   PIC	X(04).
		   07  VAT-CDE	   PIC	9(01).
		   07  FILLER	   PIC	X(03).
		   07  VAT-RTE	   PIC	Z9.99.
		   07  VAT-PERC	   PIC	X(01).
	       05  FILLER	   PIC	X(01).
	       05  VAT-TRAN-AMT    PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  VAT-NO-TAX	   PIC	Z(06)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  VAT-TXBL-AMT    PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  VAT-VAT-AMT	   PIC	Z(06)9.99-.

	   03  VAT-L2 REDEFINES W02-PRN-LINE.
	       05  VAT-H1	   PIC	X(11).
	       05  VAT-H2A.
		   07  VAT-H2	   PIC	X(09).
		   07  VAT-H3	   PIC	X(12).
	       05  VAT-H3A	   PIC	X(15).
	       05  VAT-H4	   PIC	X(13).
	       05  VAT-H5	   PIC	X(16).
	       05  VAT-H6	   PIC	X(09).
	       05  VAT-H7	   PIC	X(11).

000680	   03  VAT-L3 REDEFINES W02-PRN-LINE.
000690	       05  V-PGE	   PIC	X(06).
000700	       05  V-PNO	   PIC	Z(03)9.
	       05  V-HEADING.
000710		   07  FILLER	   PIC	X(18).
000720		   07  V-CPY.
		     09	FILLER	   PIC	X(08).
		     09	V-HEAD	   PIC	X(32).
		   07  V-RATE	   PIC	Z9.99.
		   07  V-PERC	   PIC	X(01).
000730		   07  FILLER	   PIC	X(06).
000740	       05  V-DATE	   PIC	X(06).
000750	       05  V-DTE	   PIC	Z9/99/9999.

000680	   03  GP-L1 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(10).
	       05  GP-HEAD	   PIC	X(17).
	       05  GP-HEAD2.
		   07  GP-VALUE	   PIC	Z(08)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  GP-PERC	   PIC	ZZ9.99-.
	       05  GP-P 	   PIC	X(01).
	       05  FILLER	   PIC	X(31).

       COPY W05.VAT.

       01  VTD-RECORD.
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  VTD-DATE.
	       05  VTD-DAT     PIC  99/99/9999.
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  VTD-REFER.
	       05  VTD-REF     PIC  X(08).
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  VTD-TRAN        PIC  X(11).
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  VTD-NAME.
	       05  FILLER      PIC  X(04).
	       05  VTD-CDE     PIC  9(01).
	       05  FILLER      PIC  X(03).
	       05  VTD-RTE     PIC  Z9.99.
	       05  VTD-PERC    PIC  X(01).
	       05  FILLER      PIC  X(16).
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  VTD-TRAN-AMT    PIC  -(08)9.99  BLANK WHEN ZERO.
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  VTD-NO-TAX      PIC  -(07)9.99  BLANK WHEN ZERO.
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  VTD-TXBL-AMT    PIC  -(08)9.99  BLANK WHEN ZERO.
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  VTD-VAT-AMT     PIC  -(07)9.99  BLANK WHEN ZERO.
	   03  FILLER	       PIC  X(01)   VALUE ",".

       01  VTD-RECORD2.
	   03  VTD-H1	       PIC  X(11).
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  VTD-H2A.
	       05  VTD-H2      PIC  X(09).
	       05  FILLER      PIC  X(01)   VALUE ",".
	       05  VTD-H3      PIC  X(12).
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  VTD-H3A	       PIC  X(30).
	   03  FILLER	       PIC  X(01)   VALUE QUOTE.
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  VTD-H4	       PIC  X(13).
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  VTD-H5	       PIC  X(16).
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  VTD-H6	       PIC  X(09).
	   03  FILLER	       PIC  X(01)   VALUE ",".
	   03  VTD-H7	       PIC  X(11).
	   03  FILLER	       PIC  X(01)   VALUE ",".

       01  W10-DATE.
	   03  W10-DTE	       PIC 99/99/9999.

       COPY W12.WS.

       01  W10-TAX-KEY.
      *
      *    The first record on the file is the control record
      *    and will have the type "C", taxtype 0 and the date
      *    will contain the start date for the period.
      *    
      *    TYPE: "1" = Input Tax
      *          "2" = Output Tax
      *          "3" = Input Payments
      *          "4" = Output Payments
      *
           03  W10-TAX-TYPE    PIC  X(01).
      *
      *    ACTYPE: 1 = Debtors        (Sales / Returns)
      *            2 = Cash           (Sales / Refunds)
      *            3 = Creditors      (Purchases)
      *            4 = General Ledger (Rates, Vehicles ETC)
      * 	   5 = Petty Cash     (Payments / Refunds)
      *
           03  W10-TAX-ACTYPE  PIC  9(01).
      *
      *    CODE:  This is the transaction code
      *
           03  W10-TAX-CODE    PIC  9(02).
000050	   03  W10-TAX-DATE    PIC  9(08).
           03  W10-TAX-REF     PIC  X(08).
           03  W10-TAX-SEQ     PIC  9(04).
       01  W15-VAT-TOTALS.
	   05  W15-TRAN-AMT-1  PIC S9(09)V99 COMP-3.
	   05  W15-TRAN-AMT-2  PIC S9(09)V99 COMP-3.
	   05  W15-TRAN-AMT-3  PIC S9(09)V99 COMP-3.
	   05  W15-TRAN-AMT-I  PIC S9(09)V99 COMP-3.
	   05  W15-TRAN-AMT-O  PIC S9(09)V99 COMP-3.
	   03  W15-NON-TXBL-1  PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-2  PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-3  PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-I  PIC S9(09)V99 COMP-3.
           03  W15-NON-TXBL-O  PIC S9(09)V99 COMP-3.
	   03  W15-VAT-CODES OCCURS 12.
	   05  W15-TXBL-AMT-1  PIC S9(09)V99 COMP-3.
	   05  W15-TXBL-AMT-2  PIC S9(09)V99 COMP-3.
	   05  W15-TXBL-AMT-3  PIC S9(09)V99 COMP-3.
	   05  W15-TXBL-AMT-I  PIC S9(09)V99 COMP-3.
	   05  W15-TXBL-AMT-O  PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-1   PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-2   PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-3   PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-I   PIC S9(09)V99 COMP-3.
	   05  W15-VAT-AMT-O   PIC S9(09)V99 COMP-3.
       01  W15-HEADINGS.
           03  W15-VAT-TYPE    PIC  X(15).
           03  W15-DESC        PIC  X(15).

       COPY W15.WS.

       COPY W20.WS.

       01  W24-PERIOD.
	   03  FILLER	       PIC X(16) VALUE "For the period: ".
	   03  W24-DTE1        PIC Z9/99/9999.
           03  FILLER          PIC X(04) VALUE " to ".
	   03  W24-DTE2        PIC Z9/99/9999.

004310 01  W25-CALCS.
	   03  W25-CALCULATIONS.
005040	       05  W25-RESULT	   PIC 9(05)V99.
004330	       05  W25-RESULT1 REDEFINES W25-RESULT.
004340		   07  W25-WHOLE   PIC 9(05).
004350		   07  W25-DECIMAL PIC 9(02).
	       05  W25-RESULT2 REDEFINES W25-RESULT.
		   07  W25-KEY	   PIC 9(04).
		   07  W25-SUB	   PIC 9(01).
		   07  FILLER	   PIC 9(02).
004360	       05  W25-TOTAL	   PIC S9(09)V99  COMP-3.
004370	       05  W25-VALUE	   PIC S9(09)V99  COMP-3.
004380	       05  W25-COST1	   PIC S9(09)V99  COMP-3.
004390	       05  W25-COST2	   PIC S9(09)V99  COMP-3.
004400	       05  W25-COST3	   PIC S9(09)V99  COMP-3.
004410	       05  W25-SELL1	   PIC S9(09)V99  COMP-3.
004420	       05  W25-SELL2	   PIC S9(09)V99  COMP-3.
004430	       05  W25-SELL3	   PIC S9(09)V99  COMP-3.
004380	       05  W25-CASH	   PIC S9(09)V99  COMP-3.
004390	       05  W25-REFF	   PIC S9(09)V99  COMP-3.
004400	       05  W25-INV	   PIC S9(09)V99  COMP-3.
004410	       05  W25-CRN	   PIC S9(09)V99  COMP-3.

	   03  W25-RECALC REDEFINES W25-CALCULATIONS.
005040	       05  W25-FILLER1	   PIC	9(05)V99.
	       05  W25-R-CASH	   PIC S9(09)V99  COMP-3.
	       05  W25-R-CHQ	   PIC S9(09)V99  COMP-3.
	       05  W25-R-CARD	   PIC S9(09)V99  COMP-3.
	       05  W25-R-SPEED	   PIC S9(09)V99  COMP-3.
	       05  W25-R-VOUCH	   PIC S9(09)V99  COMP-3.
	       05  W25-R-CRNOTE	   PIC S9(09)V99  COMP-3.
	       05  W25-R-TRFRS	   PIC S9(09)V99  COMP-3.
	       05  W25-R-RECEIPTS  PIC S9(09)V99  COMP-3.
	       05  W25-FILLER2	   PIC S9(09)V99  COMP-3.
	       05  W25-FILLER3	   PIC S9(09)V99  COMP-3.
	       05  W25-FILLER4	   PIC S9(09)V99  COMP-3.

       01  W30-REFERENCE.
	   03  W30-START-REF   PIC  9(08).
	   03  W30-END-REF     PIC  9(08).

       01  W30-HEADING.
	   03  FILLER	       PIC  X(12) VALUE "CASH TAKINGS".
	   03  FILLER	       PIC  X(07) VALUE SPACES.
	   03  W30-DATE	       PIC  Z9/99/9999.

004440 01  W40-INDIC.
004450     03  W40-SMAN.
004460         05  W40-SREP    PIC 9(01) COMP-5 OCCURS 50.
004470     03  W40-DRAW.
004480         05  W40-DREP    PIC 9(01) COMP-5 OCCURS 40.
004490     03  W40-EDIT.
004500         05  W40-KEY     PIC ZZZZ9.
004510         05  W40-PERC    PIC ZZ9.99-.
004520     03  W40-EDIT2 REDEFINES W40-EDIT.
004530         05  FILLER      PIC X(01).
004540	       05  W40-AMOUNT  PIC Z(08)9.99-.

       COPY W40.WS.

       COPY FUNCTION.WS.

004890 01  W70-WORK.
004900     03  W70-REP         PIC 9(04)     COMP-5.

004910 01  W80-ACCOUNT.
004920     03  W80-ACNO        PIC 9(05).

004960 01  W90-TRAN.
004970	   03  W90-DATE        PIC X(10).
004980     03  W90-DTE REDEFINES W90-DATE
004990			       PIC 99/99/9999.
005000     03  W90-CODE        PIC 9(02).
005010     03  W90-REF         PIC X(06).
005020	   03  W90-DB	       PIC 9(09)V99  COMP-3.
005030	   03  W90-CR	       PIC 9(09)V99  COMP-3.
005040	   03  W90-BDT	       PIC S9(09)V99 COMP-3.
005050	   03  W90-BCR	       PIC S9(09)V99 COMP-3.
005060     03  W90-DESC        PIC X(12).
005070	   03  W90-90	       PIC S9(09)V99 COMP-3.
005080	   03  W90-60	       PIC S9(09)V99 COMP-3.
005090	   03  W90-30	       PIC S9(09)V99 COMP-3.
005100	   03  W90-CUR	       PIC S9(09)V99 COMP-3.
005110	   03  W90-BAL	       PIC S9(09)V99 COMP-3.

005120 01  W95-STM.
005130     03  W95-COMP        PIC X(40).
005140     03  W95-ADD1        PIC X(30).
005150     03  W95-ADD2        PIC X(30).
005160     03  W95-ADD3        PIC X(30).
005170     03  W95-ADD4        PIC X(30).
005180     03  W95-POST        PIC 9(04)     COMP-5.
005190     03  W95-TEL         PIC X(08).

005200 01  W100-DAYS.
005210     03  W100-VAL.
005220         05  FILLER      PIC 9(12) VALUE 000031059090.
005230         05  FILLER      PIC 9(12) VALUE 120151181212.
005240         05  FILLER      PIC 9(12) VALUE 243273304334.
005250     03  W100-MONTH REDEFINES W100-VAL.
005260         05  W100-DAY    PIC 9(03) OCCURS 12.
005270     03  W100-DAY1       PIC 9(05)     COMP-3.
005280     03  W100-DAY2       PIC 9(05)     COMP-3.
005290     03  W100-DATE.
005300         05  W100-DD     PIC 9(02).
005310         05  W100-MM     PIC 9(02).
005320	       05  W100-CC     PIC 9(02).
005320         05  W100-YY     PIC 9(02).
005330     03  W100-DTE REDEFINES W100-DATE
005340			       PIC 9(08).
005350     03  W100-RESULT     PIC 9(05)V99.
005360     03  W100-R1 REDEFINES W100-RESULT.
005370         05  W100-WHOLE  PIC 9(05).
005380         05  W100-DEC    PIC 9(02).

005390 01  W200-TRAN.
005400     03  W200-ACCUM.
	       05  W200-LEDG   PIC  X(04)	    OCCURS 300.
005410	       05  W200-DT     PIC S9(09)V99 COMP-3 OCCURS 300.
005420	       05  W200-CR     PIC S9(09)V99 COMP-3 OCCURS 300.
005430	       05  W200-DTOT   PIC S9(09)V99 COMP-3.
005440	       05  W200-CTOT   PIC S9(09)V99 COMP-3.
005450	       05  W200-COST   PIC S9(09)V99 COMP-3.
005460	       05  W200-SELL   PIC S9(09)V99 COMP-3.
005470	       05  W200-NET    PIC S9(09)V99 COMP-3.
005470	       05  W200-ADJ    PIC S9(09)V99 COMP-3.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
      *   ******    ******   *******   ********  ********  **     ** 
      *  **    **  **    **  **    **  **        **        ***    ** 
      *  **        **        **   **   **        **        ** *   ** 
      *   ******   **        ******    *****     *****     **  *  ** 
      *        **  **        **   **   **        **        **   * ** 
      *  **    **  **    **  **    **  **        **        **    *** 
      *   ******    ******   **    **  ********  ********  **     ** 
      *                                                              
005480 SCREEN SECTION.

       COPY BLANK.CRT.

       01  MENU-INSTRUCT.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Use ".
	   03	       COLUMN  6 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN  7 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 11 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to point to an item, and ".
	   03	       COLUMN 38 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Enter>".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Select ".
	   03	       COLUMN 56 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Esc>".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Exit".

000850 01  S-PROG.
000860	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "DAY/MONTH/YEAR END".

       01  S-MENU.
	   03  LINE  4 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 56 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
	   03  LINE  5 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³    ".
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "PERIOD-END OPTIONS".
	   03	       COLUMN 52 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "    ³".
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Day-End Reports        ³".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Month-End Clear        ³".
001470	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Year-End Clear         ³".
001470	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales Analysis         ³".
001470	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Category Analysis      ³".
001470	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Purchases Analysis     ³".
001470	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "7".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Cost-of-Sales Analysis ³".
001470	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "8".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Stock Category Totals  ³".
001470	   03  LINE 14 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "A".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Stock Gross Profit     ³".
001470	   03  LINE 15 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "B".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Turnover and GP Report ³".
001470	   03  LINE 16 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "C".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales Staff Analysis   ³".
001470	   03  LINE 17 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "D".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Value Added Tax Report ³".
001470	   03  LINE 18 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "E".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Orders and Quotations  ³".
001570	   03  LINE 19 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "F".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Export VAT             ³".
001570	   03  LINE 20 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit                   ³".
	   03  LINE 21 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 30 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

005960 01  S03.
005970     03  LINE  2 COLUMN 32 FOREGROUND-COLOR 15
005980                           VALUE "DAY-END REPORTS".

005990 01  S04.
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 52 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
006000	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³  ".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
006010				 VALUE "STATISTICS REPORTS".
	   03	       COLUMN 50 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "  ³".
006020	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "1".
006030	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Debtors            ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006040	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "2".
006050	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Creditors          ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006080	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "3".
006090	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Stock              ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006100	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "4".
006110	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " All the above      ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006120	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "0".
006130	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Period-End Control ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006140	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 30 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

006160 01  S05A.
006170	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 15
006180                           VALUE "STATISTICS YEAR-END CLEAR".
006190 01  S05B.
006200	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 15
006210				 VALUE "STATISTICS MONTH-END CLEAR".
006220 01  S05C.
	   03  LINE  4 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 52 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
006230	   03  LINE  5 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "1".
006240	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Debtors            ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006250	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "2".
006260	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Creditors          ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006270	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "3".
006300	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Stock              ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006290	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "4".
006300	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Salesman           ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006290	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "5".
006320	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " All the above      ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006310	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "6".
	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Quotations         ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "7".
	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Sales Orders       ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "8".
	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Converted Orders   ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "0".
006340	   03	       COLUMN 32 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Period-End Control ".
	   03	       COLUMN 52 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006330	   03  LINE 14 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 30 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

006370 01  S06.
006380	   03  LINE  2 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
006390				 VALUE "DAY-END REPORT OPTIONS".
	   03  LINE  8 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT VALUE
				 "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 56 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE "¿".
006230	   03  LINE  9 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "1".
006410	   03	       COLUMN 26 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Stock Re-Order               ".
	   03	       COLUMN 56 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006420	   03  LINE 10 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "2".
006430	   03	       COLUMN 26 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Debtor Transactions          ".
	   03	       COLUMN 56 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006440	   03  LINE 11 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "3".
006450	   03	       COLUMN 26 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Cash Drawers                 ".
	   03	       COLUMN 56 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006460	   03  LINE 12 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "4".
006470	   03	       COLUMN 26 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Salesman                     ".
	   03	       COLUMN 56 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006480	   03  LINE 13 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "5".
006490	   03	       COLUMN 26 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Turnover and G.P.            ".
	   03	       COLUMN 56 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006500	   03  LINE 14 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "6".
006510	   03	       COLUMN 26 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Turnover and G.P. (Detailed) ".
	   03	       COLUMN 56 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
	   03  LINE 15 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "7".
	   03	       COLUMN 26 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Debtor Receipts (POS)        ".
	   03	       COLUMN 56 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
	   03  LINE 16 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "8".
	   03	       COLUMN 26 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Discounted Sales             ".
	   03	       COLUMN 56 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006520	   03  LINE 17 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "³ ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT VALUE "0".
006530	   03	       COLUMN 26 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE " Exit from this MENU          ".
	   03	       COLUMN 56 FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
				 VALUE "³".
006330	   03  LINE 18 COLUMN 23 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 2 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 24 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
				 VALUE
				 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".


       01  S07.
	   03  LINE 11 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE 12 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.
       01  S08.
	   03  LINE  8 COLUMN 67 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

       01  S09.
	   03  S09S.
	   05  LINE  9 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 6
				 PIC  Z(07)9 USING W30-START-REF AUTO.
	   03  S09E.
	   05  LINE 10 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 6
				 PIC  Z(07)9 USING W30-END-REF AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
      *        *******    *******     ******     ****** 
      *        **    **   **    **   **    **   **    **
      *        **    **   **   **    **    **   **
      *        *******    ******     **    **   **
      *        **         **   **    **    **   **
      *        **         **    **   **    **   **    **
      *        **         **    **    ******     ****** 
      *
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006570 AA00M		  SECTION.
006580 AA00.
003240	   IF LS0-PALEV < 3 AND LS0-SALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AY005.
006590	     PERFORM ZA00-INIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     CLOSE PARAM.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ As the parameter file has been assigned exclusive in this ³
      *    ³ program, it is  necessary to close it.  W02-LINAGE is set ³
      *    ³ to 99 which informs the PRINTUTL program to open the file ³
      *    ³ and close it again on exit.			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 99		 TO W02-LINAGE.
      *
      *    ****     S E T   T O   8   L I N E S   P E R	  I N C H
      *
	     MOVE 8		 TO W02-PRN-TYPE.
	     PERFORM OPEN-PRINTER.
	     MOVE ZERO		 TO W02-LINAGE.
029050	     OPEN I-O PARAM LOCK.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-L1.

006620 AA01.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
006630       DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 26		 TO SHADE-WIDTH.
	     MOVE 17		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       AA02.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 19		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 24		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       AA05.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 AA10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO AA15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO AA10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO AA10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO AA10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO AA10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO AA15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO AA05
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO AA18
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO AA05
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF (WS-OPTION < "0" OR > "8") AND
		  (WS-OPTION < "A" OR > "F") AND
		  (WS-OPTION NOT = "$")
		   CALL X"E5"
		   GO TO AA05
	       END-IF
	       GO TO AA16
	   ELSE
	       CALL X"E5"
	       GO TO AA05.

       AA15.
	     EVALUATE SCREEN-LIN
	       WHEN 5
		   MOVE "1"	 TO WS-OPTION
	       WHEN 6
		   MOVE "2"	 TO WS-OPTION
	       WHEN 7
		   MOVE "3"	 TO WS-OPTION
	       WHEN 8
		   MOVE "4"	 TO WS-OPTION
	       WHEN 9
		   MOVE "5"	 TO WS-OPTION
	       WHEN 10
		   MOVE "6"	 TO WS-OPTION
	       WHEN 11
		   MOVE "7"	 TO WS-OPTION
	       WHEN 12
		   MOVE "8"	 TO WS-OPTION
	       WHEN 13
		   MOVE "A"	 TO WS-OPTION
	       WHEN 14
		   MOVE "B"	 TO WS-OPTION
	       WHEN 15
		   MOVE "C"	 TO WS-OPTION
	       WHEN 16
		   MOVE "D"	 TO WS-OPTION
	       WHEN 17
		   MOVE "E"	 TO WS-OPTION
	       WHEN 18
		   MOVE "F"	 TO WS-OPTION
	       WHEN 19
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO AA05
	     END-EVALUATE.
	   GO TO AA18.

       AA16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 19	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 5	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 6	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "4"
		   MOVE 8	 TO SCREEN-LIN
	       WHEN "5"
		   MOVE 9	 TO SCREEN-LIN
	       WHEN "6"
		   MOVE 10	 TO SCREEN-LIN
	       WHEN "7"
		   MOVE 11	 TO SCREEN-LIN
	       WHEN "8"
		   MOVE 12	 TO SCREEN-LIN
	       WHEN "A"
		   MOVE 13	 TO SCREEN-LIN
	       WHEN "B"
		   MOVE 14	 TO SCREEN-LIN
	       WHEN "C"
		   MOVE 15	 TO SCREEN-LIN
	       WHEN "D"
		   MOVE 16	 TO SCREEN-LIN
	       WHEN "E"
		   MOVE 17	 TO SCREEN-LIN
	       WHEN "F"
		   MOVE 18	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       AA18.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     EVALUATE WS-OPTION
	       WHEN "0"
006740		   GO TO AY000-EOJ
	       WHEN "1"
006760		   PERFORM BA000D
	       WHEN "2"
006790		   PERFORM DA00M
	       WHEN "3"
006820		   PERFORM EA00Y
	       WHEN "4"
006850		   PERFORM CG00R
	       WHEN "5"
006880		   PERFORM FA00R
	       WHEN "6"
006910		   PERFORM GA00R
	       WHEN "7"
006940		   PERFORM HA00R
	       WHEN "8"
006970		   PERFORM IA000
	       WHEN "A"
007000		   PERFORM JA000
	       WHEN "B"
007000		   PERFORM LA000
	       WHEN "C"
007030		   PERFORM KA00R
	       WHEN "D"
007030		   PERFORM MA00R
	       WHEN "E"
007030		   PERFORM OA00R
	       WHEN "F"
007030		   PERFORM PA00R
	       WHEN "$"
007030		   PERFORM NA00T
	     END-EVALUATE.
	   IF WS-PRINTER = "S"
	       MOVE "I" 	 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	       MOVE "P" 	 TO WS-COMMAND.
007060	     GO TO AA01.

007070 AA50.
	     MOVE "H"		 TO W02-PRN-TYPE.
007080       MOVE SPACES         TO R-DETAIL.
007090       MOVE "PAGE:"        TO R-PGE.
007100       MOVE "DATE:"        TO R-DATE.
007110       ADD 1               TO WS-PAGE.
007120       MOVE WS-PAGE        TO R-PNO.
007130       MOVE W12-TODAY      TO R-DTE.
007140       MOVE W95-COMP       TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.

007170 AA55.
007180       EXIT.

007190 AA60.
	     MOVE "H"		 TO W02-PRN-TYPE.
007200	     MOVE SPACES	 TO R-DETAIL.
007210	     MOVE "PAGE:"	 TO RC-PGE.
007220	     MOVE "DATE:"	 TO RC-DATE.
007230	     ADD 1		 TO WS-PAGE.
007240	     MOVE WS-PAGE	 TO RC-PNO.
007250	     MOVE W12-TODAY	 TO RC-DTE.
007260	     MOVE W95-COMP	 TO RC-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007280	     MOVE SPACES	 TO R-DETAIL.

007290 AA65.
007300       EXIT.

       COPY DATE.CHK.

       COPY LOCKED.REC.

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
      *    ³   PERFORM OPT-MESSAGE.				       ³
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
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      ERASE SCREEN FROM SPECIFIED LOCATION	       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CPOS (CLIN,CCOL) as starting position and increases  ³
      *    ³ CLIN by 1 until CLIN reaches the line allocated to exit.  ³
      *    ³ The screen is cleared with Column 1 and 80 containing "³" ³
      *    ³ and columns 2 to 79 containing spaces.		       ³
      *    ³  eg.						       ³
      *    ³	  MOVE 0801	 TO CPOS.			       ³
      *    ³	  PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP6.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007310 AN00-READ	       SECTION.
008460 READ-AUDIT.
008470       READ AUDIT.
008480     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008490         GO TO READ-AUDIT-EXIT.
           IF WS-STATUS = 23
               IF (WS-AUDKEY NOT = 1) AND
		  (WS-AUDKEY < W70-REP)
		   ADD 1	 TO WS-AUD WS-AUDKEY
                   GO TO READ-AUDIT.
             MOVE ZERO           TO WS-AUD.
008500     IF WS-STAT1 = "2" OR "3" OR "4"
008510         MOVE 1            TO WS-F-ERROR
008520         PERFORM READ-ERROR.
008530     IF RECORD-LOCKED
008540         PERFORM LOCKED-RECORD
008550         GO TO READ-AUDIT.
008560	     MOVE 1		 TO WS-F-ERROR.
008570       PERFORM READ-ERROR.

008740 READ-AUDIT-EXIT.
008750       EXIT.

008650 READ-CONTROL.
008660       READ NETWORK.
008670     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008680         GO TO READ-CONTROL-EXIT.
008690     IF WS-STAT1 = "2" OR "3" OR "4"
008700         MOVE 2            TO WS-F-ERROR
008710         PERFORM READ-ERROR.
008720     IF RECORD-LOCKED
008730         PERFORM LOCKED-RECORD
008740         GO TO READ-CONTROL.
008750       MOVE 2              TO WS-F-ERROR.
008760       PERFORM READ-ERROR.

008930 READ-CONTROL-EXIT.
008940       EXIT.
      *
      *     ***  ****  ***** ****  *** *****     ***** *** *     *****
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *     ****  ***   *   *  *    *       ***    *  *     ***
      *    *     *   * *     *   *  *    *       *      *  *     *
      *    *   * *   * *     *   *  *    *       *      *  *     *
      *     ***  *   * ***** ****  ***   *       *     *** ***** *****
      *
006080 READ-CREDIT.
006090       READ CREDIT
               KEY IS P-NUMBER.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110         GO TO READ-CREDIT-EXIT.
           IF WS-STATUS = "23"
               MOVE 3            TO WS-F-ERROR
               GO TO READ-CREDIT-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130         MOVE 3            TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170         GO TO READ-CREDIT.

006520 READ-CREDIT-EXIT.
006530       EXIT.
      *
      *    ****  ***** ****    *   ****  *****	   ***** *** *	   *****
      *    *   * *     *   *  * *  *   *   *	   *	  *  *	   *
      *    *   * *     *   * *	 * *   *   *	   *	  *  *	   *
      *    *   * ***   ****  ***** ****    *	   ***	  *  *	   ***
      *    *   * *     *   * *	 * *  *    *	   *	  *  *	   *
      *    *   * *     *   * *	 * *   *   *	   *	  *  *	   *
      *    ****  ***** *   * *	 * *   *   *	   *	 *** ***** *****
      *
008650 READ-DEPART.
008660	     READ DEPART	 WITH IGNORE LOCK
	       KEY IS DPT-CODE.
008670     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008680         GO TO READ-DEPART-EXIT.
	   IF WS-STATUS = "23"
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
008690	   IF WS-STAT1 = "2" OR "3" OR "4"
008700         MOVE 7            TO WS-F-ERROR
008710         PERFORM READ-ERROR.
008770       GO TO READ-DEPART-EXIT.
      
006080 READ-DEPART-NEXT.
006090	     READ DEPART NEXT.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-DEPART-EXIT.
           IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 7		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.
006150     IF RECORD-LOCKED
006160         PERFORM LOCKED-RECORD
006170	       GO TO READ-DEPART-NEXT.
006200	     GO TO READ-DEPART-EXIT.

006080 START-AT-DEPT-CODE.
006090	     START DEPART
	       KEY >= DPT-CODE.
006100     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
006110	       GO TO READ-DEPART-EXIT.
           IF WS-STATUS = "23"
	       MOVE 7		 TO WS-F-ERROR
	       GO TO READ-DEPART-EXIT.
006120     IF WS-STAT1 = "2" OR "3" OR "4"
006130	       MOVE 7		 TO WS-F-ERROR
006140         PERFORM READ-ERROR.

008930 READ-DEPART-EXIT.
008940       EXIT.

      *
      *    ****  ***** ****  *****  ***  ****     ***** *** *     *****
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * ***   ****    *   *   * ****     ***    *  *     ***
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    *   * *     *   *   *   *   * *   *    *      *  *     *
      *    ****  ***** ****    *    ***  *   *    *     *** ***** *****
      *
000020 READ-DEBTOR.
000030       READ DEBTOR
               KEY IS DEB-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
           IF WS-STATUS = "23"
               MOVE 6            TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 6            TO WS-F-ERROR
000080         PERFORM READ-ERROR.
000090     IF RECORD-LOCKED
000100         PERFORM LOCKED-RECORD
000110         GO TO READ-DEBTOR.
             GO TO READ-DEBTOR-EXIT.

000310 READ-DEBTOR-NEXT.
000320       READ DEBTOR NEXT.
000330     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000340         GO TO READ-DEBTOR-EXIT.
000350     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
000360         MOVE 6            TO WS-F-ERROR
000370         GO TO READ-DEBTOR-EXIT.
000380     IF WS-STAT1 = "2" OR "3" OR "4"
000390         MOVE 6            TO WS-F-ERROR
000400         PERFORM READ-ERROR.
000410     IF RECORD-LOCKED
000420         PERFORM LOCKED-RECORD
000430         GO TO READ-DEBTOR-NEXT.
             GO TO READ-DEBTOR-EXIT.

000020 START-AT-DEBT-AC.
000030       START DEBTOR
               KEY >= DEB-ACNO.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
           IF WS-STATUS = "23"
               MOVE 6            TO WS-F-ERROR
000050         GO TO READ-DEBTOR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070         MOVE 6            TO WS-F-ERROR
000080         PERFORM READ-ERROR.

000470 READ-DEBTOR-EXIT.
000480       EXIT.
      
008690 READ-PARAM.
008700       READ PARAM.
008710     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008720         GO TO READ-PARAM-EXIT.
008730     IF WS-STAT1 = "2" OR "3" OR "4"
008740         MOVE 15           TO WS-F-ERROR
008750         PERFORM READ-ERROR.
008760     IF RECORD-LOCKED
008770         PERFORM LOCKED-RECORD
008780         GO TO READ-PARAM.
008790       MOVE 15             TO WS-F-ERROR.
008800       PERFORM READ-ERROR.
      
008960 READ-PARAM-EXIT.
008970       EXIT.

006840 READ-SORDER-NEXT.
006850       READ SORDER NEXT.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870         GO TO READ-SORDER-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
006890         MOVE 20           TO WS-F-ERROR
006870         GO TO READ-SORDER-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890         MOVE 20           TO WS-F-ERROR
006900         PERFORM READ-ERROR.
006910     IF RECORD-LOCKED
006920         PERFORM LOCKED-RECORD
006930         GO TO READ-SORDER-NEXT.
             GO TO READ-SORDER-EXIT.

006840 START-AT-ORDER-NO.
006850       START SORDER
               KEY >= SOR-KEY.
006860     IF WS-STATUS = "00"
006890         MOVE ZERO         TO WS-F-ERROR
006870         GO TO READ-SORDER-EXIT.
009900     IF WS-STATUS = "23"
006890         MOVE 20           TO WS-F-ERROR
006870         GO TO READ-SORDER-EXIT.
006880     IF WS-STAT1 = "2" OR "3" OR "4"
006890         MOVE 20           TO WS-F-ERROR
006900         PERFORM READ-ERROR.

007120 READ-SORDER-EXIT.
007130       EXIT.

      *
      *     ***  *****  ***   ***  *   *     ***** ***** *     *****
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *    *       *   *   * *     * *       *       *   *     *
      *     ***    *   *   * *     **        ***     *   *     ***
      *        *   *   *   * *     * *       *       *   *     *
      *    *   *   *   *   * *   * *  *      *       *   *     *
      *     ***    *    ***   ***  *   *     *     ***** ***** *****
      *
008190 READ-STOCK.
008200       READ STOCK
               KEY IS STK-CODE.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-STOCK-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 22           TO WS-F-ERROR
009920         GO TO READ-STOCK-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 22           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
008270         PERFORM LOCKED-RECORD
008280         GO TO READ-STOCK.
008310       GO TO READ-STOCK-EXIT.

008190 READ-STOCK-NEXT.
008200       READ STOCK NEXT.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-STOCK-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 22           TO WS-F-ERROR
009920         GO TO READ-STOCK-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 22           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
008270         PERFORM LOCKED-RECORD
008280         GO TO READ-STOCK-NEXT.
008310       GO TO READ-STOCK-EXIT.

008190 START-AT-STOCK-CODE.
008200       START STOCK 
               KEY >= STK-CODE.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-STOCK-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 22           TO WS-F-ERROR
009920         GO TO READ-STOCK-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 22           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008310       GO TO READ-STOCK-EXIT.

       READ-STOCK-EXIT.
008640       EXIT.
      *
      *  ***** *   * ***** ****    *   *   *     **** *** *    ****  
      *    *   *   *   *   *   *  * *  *   *     *     *  *    *     
      *    *    * *    *   *   * *   * **  *     *     *  *    *     
      *    *     *     *   ****  ***** * * *     ***   *  *    ***   
      *    *    * *    *   *   * *   * *  **     *     *  *    *     
      *    *   *   *   *   *   * *   * *   *     *     *  *    *     
      *    *   *   *   *   *   * *   * *   *     *    *** **** ****  
      *
009860 READ-TXTRAN.
009870       READ TXTRAN 
               KEY IS TAX-KEY.                
009880     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
009890         GO TO READ-TXTRAN-EXIT.
           IF WS-STATUS = 23
               MOVE 32           TO WS-F-ERROR
               GO TO READ-TXTRAN-EXIT.
009900     IF WS-STAT1 = "2" OR "3" OR "4"
009910         MOVE 32           TO WS-F-ERROR
009920         PERFORM READ-ERROR.
009930     IF RECORD-LOCKED
009940         PERFORM LOCKED-RECORD
009950         GO TO READ-TXTRAN.
009980       GO TO READ-TXTRAN-EXIT.

008190 READ-TXTRAN-NEXT.
008200       READ TXTRAN NEXT.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-TXTRAN-EXIT.
009900     IF (WS-STATUS = "23") OR
              (WS-STAT1 = "1")
009910         MOVE 32           TO WS-F-ERROR
009920         GO TO READ-TXTRAN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 32           TO WS-F-ERROR
008250         PERFORM READ-ERROR.
008260     IF RECORD-LOCKED
008270         PERFORM LOCKED-RECORD
008280         GO TO READ-TXTRAN-NEXT.
008310       GO TO READ-TXTRAN-EXIT.

008190 START-AT-TXTRAN-KEY.
008200       START TXTRAN 
               KEY >= TAX-KEY.
008210     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
008220         GO TO READ-TXTRAN-EXIT.
009900     IF WS-STATUS = "23"
009910         MOVE 32           TO WS-F-ERROR
009920         GO TO READ-TXTRAN-EXIT.
008230     IF WS-STAT1 = "2" OR "3" OR "4"
008240         MOVE 32           TO WS-F-ERROR
008250         PERFORM READ-ERROR.

010150 READ-TXTRAN-EXIT.
010160       EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
007770 AO00-WRITE	       SECTION.

       COPY AUDIT.WR.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY DEBTOR.WR.

       COPY PARAM.WR.

       COPY SORDER.WR.

       COPY STOCK.WR.

       COPY TXTRAN.WR.

      /
008260 AY000-END              SECTION.
008270 AY000-EOJ.
008280       PERFORM ZB00-EOJ.

       AY005.
             EXIT PROGRAM.
      /
008360 BA000D	       SECTION 50.
008370 BA00C.
008380       DISPLAY CLR-SCREEN.
008390       MOVE 1              TO WS-S6.

008400 BA00C-LOOP.
008410	     MOVE ZERO		 TO W40-SREP(WS-S6) W40-DREP(WS-S6).
008420     IF WS-S6 < 40
008430         ADD 1             TO WS-S6
008440         GO TO BA00C-LOOP.
008450       MOVE 41             TO WS-S6.
008460       DISPLAY S03.

008470 BA00C-LP.
008480	     MOVE ZERO		 TO W40-SREP(WS-S6).
008490     IF WS-S6 < 50
008500         ADD 1             TO WS-S6
008510         GO TO BA00C-LP.
008520       DISPLAY (4, 4)
008530       "Do you wish to clear daily statistics after printing Y/N".
008540	     ACCEPT WS-OPTION AT 0461
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
008600	   IF WS-OPTION = "Y"
008610	       MOVE 1		 TO WS-CLEAR.
	     PERFORM SAVE-SCREEN-3.
008650       PERFORM BX000.
008660     IF WS-REP1 = 1
008670         PERFORM BA00R.

008680 BA00C-MENU.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE 9		 TO SHADE-ROW.
	     MOVE 25		 TO SHADE-COL.
	     MOVE 32		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
021470	     DISPLAY S06.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

021480 BA00D.
	     MOVE 8		 TO TOP-ROW.
	     MOVE 16		 TO BOTTOM-ROW.
	     MOVE 7		 TO SCREEN-LIN.
	     MOVE 25		 TO SCREEN-COL.
	     MOVE 30		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA00D-ACCEPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 BA00D-10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA00D-15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA00D-10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA00D-10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA00D-10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA00D-10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA00D-15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA00D-ACCEPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA00DF
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA00D-ACCEPT
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < "0" OR > "8"
		   CALL X"E5"
		   GO TO BA00D-ACCEPT
	       END-IF
	       GO TO BA00D-16
	   ELSE
	       CALL X"E5"
	       GO TO BA00D-ACCEPT.

       BA00D-15.
	   IF SCREEN-LIN < 8
	       CALL X"E5"
	       GO TO BA00D-ACCEPT
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 13
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 14
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 15
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 16
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA00DF.

       BA00D-16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 16		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 13		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 14		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 15		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA00DF.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
008730	   IF WS-OPTION = "0"
008740         GO TO BA00E.
008770	     DISPLAY CLEAR-L25.
008780     IF WS-OPTION = "1"
008790         IF WS-REP2 = 1
008800             PERFORM BB00R
008810             GO TO BA00C-MENU
008820         ELSE
008830		   MOVE "No re-orders to report"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
008850		   GO TO BA00D-ACCEPT.
008860     IF WS-OPTION = "2"
008870	       IF WS-REP3 = 1 OR WS-REP4 = 1
008880             PERFORM BC00R
008890             GO TO BA00C-MENU
008900         ELSE
008910		   MOVE "No Transactions to report"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
008930		   GO TO BA00D-ACCEPT.
008940     IF WS-OPTION = "3"
008950         IF WS-REP4 = 1
008960             PERFORM BE00R
008970             GO TO BA00C-MENU
008980         ELSE
008990		   MOVE "No Cash draw details to report"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
009010		   GO TO BA00D-ACCEPT.
009020     IF WS-OPTION = "4"
009030         IF WS-REP3 = 1 OR WS-REP4 = 1
009040             PERFORM BF00R
009050             GO TO BA00C-MENU
009060         ELSE
009070		   MOVE "No Salesman details to report"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
009090		   GO TO BA00D-ACCEPT.
009100     IF WS-OPTION = "5"
009110         IF WS-REP3 = 1 OR WS-REP4 = 1
009120             PERFORM BG00R
009130             GO TO BA00C-MENU
009140         ELSE
009150		   MOVE "No Turnover to report"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
009170		   GO TO BA00D-ACCEPT.
009180	   IF WS-OPTION = "6"
009190         IF WS-REP3 = 1 OR WS-REP4 = 1
009200             PERFORM BH00R
009210             GO TO BA00C-MENU
009220         ELSE
009230		   MOVE "No Turnover to report"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
009250		   GO TO BA00D-ACCEPT.
009180	   IF WS-OPTION = "7"
009190	       IF WS-REP11 = 1
009200		   PERFORM BI00R
009210             GO TO BA00C-MENU
009220         ELSE
009230		   MOVE "No Debtor receipts to report"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
009250		   GO TO BA00D-ACCEPT.
	   IF WS-OPTION = "8"
008870	       IF WS-REP3 = 1
		   PERFORM BJ00R
009210		   GO TO BA00C-MENU
009220         ELSE
008910		   MOVE "No Transactions to report"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE.
008760	     GO TO BA00D-ACCEPT.

009270 BA00E.
	   IF WS-PRINTER = "S"
	       PERFORM RESTORE-SCREEN-3
	       MOVE "I" 	 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	       MOVE "P" 	 TO WS-COMMAND.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S04.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       BA00E-2.
	     MOVE 7		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
	     MOVE 6		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 20		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA00E-5.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 BA00E-10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA00E-15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA00E-10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA00E-10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA00E-10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA00E-10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA00E-15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA00E-5
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA00F
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA00E-5
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       IF WS-OPTION < "0" OR > "4"
		   CALL X"E5"
		   GO TO BA00E-5
	       END-IF
	       GO TO BA00E-16
	   ELSE
	       CALL X"E5"
	       GO TO BA00E-5.

       BA00E-15.
	   IF SCREEN-LIN < 7
	       CALL X"E5"
	       GO TO BA00E-5
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA00F.

       BA00E-16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 10		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA00F.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
009320	   IF WS-OPTION = "0"
009330         GO TO BA99.
	     MOVE WS-OPTION	 TO WS-STATOP.
009360       PERFORM BZ00R.
	     PERFORM RESTORE-SCREEN-3.
009370	     GO TO BA00E.

009380 BA99.
009390       EXIT.
      /
009400 BA00R          SECTION 50.
009410 BA00.
             MOVE 0701           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
009430	     DISPLAY "Busy printing Lost Sales Report" AT 1220
009440		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
009450	     MOVE ZERO		 TO W25-TOTAL.
009460	     MOVE 2		 TO WS-AUDKEY.
009470	     MOVE ZERO		 TO WS-PAGE.

009480 BA05.
009490       PERFORM AA50 THRU AA55.
009500	     MOVE "LOST SALES"	 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009520	     MOVE SPACES	 TO R-DETAIL.
009530	     MOVE WS-H7 	 TO R-H1.
009540	     MOVE WS-H8 	 TO R-H2.
009550	     MOVE WS-H9 	 TO R-H3.
009560	     MOVE "UNIT PRICE"	 TO R-H4.
009570	     MOVE WS-H11	 TO R-H5.
	     PERFORM CALL-PRINTUTL.
009590	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

009600 BA10.
009610       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
009620     IF WS-F-ERROR NOT = ZERO
009630	       MOVE ZERO	 TO WS-F-ERROR
009640         GO TO BA15.
009650     IF AUD-TYPE NOT = 1
009660         GO TO BA15.
009670	     MOVE AUD-EXT-CODE	 TO STK-CODE.
009680	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Item deleted or code changed"
				 TO STK-DESC.
009690	     MOVE STK-CODE	 TO R-I1.
009700	     MOVE STK-DESC	 TO R-DESC1.
009710	     MOVE AUD-QNT	 TO R-Q1.
009720	     MOVE STK-SELL	 TO R-UP1.
009730	     MULTIPLY STK-SELL BY AUD-QNT
				 GIVING W25-VALUE.
009740	     MOVE W25-VALUE	 TO R-V1.
009750	     ADD W25-VALUE	 TO W25-TOTAL.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BA05.
009810	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BA999.
009820 BA15.
009830     IF WS-AUDKEY < W70-REP
009840	       ADD 1		 TO WS-AUDKEY
009850         GO TO BA10.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009870	     MOVE "TOTAL LOST SALES"
				 TO R-DESC1.
009880	     MOVE W25-TOTAL	 TO R-V1.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009900	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
009920	     MOVE SPACES	 TO R-DETAIL.

009930 BA999.
009940       EXIT.
      /
009950 BB00R          SECTION 50.
009960 BB00.
	     PERFORM RESTORE-SCREEN-3.
009980	     DISPLAY (12, 20) "Printing Stock ReOrder Report"
009990			       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
010000	     MOVE ZERO		 TO WS-PAGE.
010010	     MOVE 2		 TO WS-AUDKEY.

010020 BB05.
      *
      *    ****    SET PRINTER TO 10 CPI
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
010030	     PERFORM AA50 THRU AA55.
010040	     MOVE "STOCK LOW LEVELS"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****    SET PRINTER TO 12 CPI
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
010060	     MOVE SPACES	 TO R-DETAIL.
010070	     MOVE WS-H7 	 TO R-H6.
010080	     MOVE WS-H8 	 TO R-H7.
010090	     MOVE "LEVEL"	 TO R-H8.
010100	     MOVE WS-H9 	 TO R-H9.
010110	     MOVE "COST"	 TO R-H10.
010120	     MOVE "AVERAGE"	 TO R-H11.
010130	     MOVE WS-H13	 TO R-H12.
010140	     MOVE WS-H14	 TO R-H13.
	     PERFORM CALL-PRINTUTL.
010160	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

010170 BB10.
010180       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
010190     IF WS-F-ERROR NOT = ZERO
010200	       MOVE ZERO	 TO WS-F-ERROR
010210         GO TO BB15.
010220     IF AUD-TYPE NOT = 02
010230         GO TO BB15.
010240	     MOVE AUD-EXT-CODE	 TO STK-CODE.
010250       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Item deleted or code changed"
				 TO STK-DESC.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Is a 3 character extension code being used in addition to ³
      *    ³ the 14 character stock/inventory item code?	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-USE-ITM = "Y"
	       MOVE STK-ITEM	 TO R-ITM
	       MOVE "/" 	 TO R-SL
	       MOVE STK-ITM	 TO R-EXT
	   ELSE
010260	       MOVE STK-CODE	 TO R-I2.
010270	     MOVE STK-DESC	 TO R-DESC2.
010280	     MOVE STK-LEVEL	 TO R-LEV.
010290	     MOVE STK-QUANT	 TO R-Q2.
010300	     MOVE STK-COST	 TO R-COST1.
010310	     MOVE STK-AVGE	 TO R-AVGE.
010320	     MOVE STK-MTD	 TO R-NMTD.
010330	     MOVE STK-YTD	 TO R-NYTD.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
010380	       PERFORM BB05.
010390	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BB900.
010400 BB15.
010410     IF WS-AUDKEY < W70-REP
010420	       ADD 1		 TO WS-AUDKEY
010430         GO TO BB10.
010440	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
010460	     MOVE SPACES	 TO R-DETAIL.

       BB900.
      *
      *    ****    RESET THE PRINTER CONTROLS
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.

010470 BB999.
010480       EXIT.
      /
010490 BC00R          SECTION 50.
010500 BC00.
	     PERFORM RESTORE-SCREEN-3.
010520	     DISPLAY (12, 20) "Printing Sales Transactions"
010530			       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
010540	     MOVE 1		 TO WS-S6.

010550 BC02.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
010560	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
010570	   IF WS-S6 < 300
010580	       ADD 1		 TO WS-S6
010590	       GO TO BC02.
010600	     MOVE ZERO		 TO W200-DTOT W200-CTOT
010610				    W200-COST W200-SELL W200-NET.
010620	     MOVE ZERO		 TO WS-PAGE.
010630	     MOVE 2		 TO WS-AUDKEY.

010640 BC05.
010650       PERFORM AA50 THRU AA55.
010660	     MOVE "DEBTOR TRANSACTIONS"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
010680	     MOVE SPACES	 TO R-DETAIL.
010690	     MOVE "AC-NO"	 TO R-H14.
010700	     MOVE "DEBTOR NAME"  TO R-H14A.
010710	     MOVE "REF. NO."	 TO R-H15.
010720	     MOVE "INVOICE"	 TO R-H16.
010730	     MOVE "C/NOTE"	 TO R-H17.
	     PERFORM CALL-PRINTUTL.
010750	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

010760 BC10.
010770       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
010780     IF WS-F-ERROR NOT = ZERO
010790	       MOVE ZERO	 TO WS-F-ERROR
010800         GO TO BC15.
010810     IF NOT (AUD-TYPE = 03)
010820         GO TO BC15.
010830       MOVE AUD-AC         TO R-AC W80-ACNO DEB-ACNO.
010850       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
010860	     MOVE DEB-NAME	 TO R-DNME.
010870     IF DEB-TYPE = 1
010880         INSPECT R-DNME REPLACING FIRST "*" BY "."
010890         INSPECT R-DNME REPLACING FIRST "*" BY " "
010900         INSPECT R-DNME REPLACING FIRST "!" BY " ".
010910	     MOVE AUD-REF	 TO R-REF.
010920     IF AUD-VAL < ZERO
010930	       MOVE AUD-VAL	 TO R-CASH
010940	       ADD AUD-VAL	 TO W200-CTOT
010950     ELSE
010960	       ADD AUD-VAL	 TO W200-DTOT
010970	       MOVE AUD-VAL	 TO R-DEBT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
011020	       PERFORM BC05.
011030	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BC999.
011040 BC15.
011050     IF WS-AUDKEY < W70-REP
011060	       ADD 1		 TO WS-AUDKEY
011070         GO TO BC10.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
011120	       PERFORM BC05.
011130	     MOVE WS-H4 	 TO R-H15.
011140	     MOVE W200-DTOT	 TO R-DEBT.
011150	     MOVE W200-CTOT	 TO R-CASH.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011170	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
011190	     MOVE SPACES	 TO R-DETAIL.

011200 BC999.
011210       EXIT.
      /
011220 BE00R           SECTION 50.
011230 BE00.
	     PERFORM RESTORE-SCREEN-3.
011250	     DISPLAY "Busy printing Cash Draw Report" AT 1220
011260		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
011270	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3
011280				    W25-SELL1 W25-SELL2 W25-SELL3
				    W25-TOTAL.
      *
      *    ****    SET PRINTER TO 12 CPI
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DETAIL.
011290	     MOVE ZERO		 TO WS-PAGE.
011300	     MOVE 1		 TO WS-NO.
011310 BE05.
011320	   IF W40-DREP(WS-NO) = 1
011330         GO TO BE10.
011340 BE06.
011350     IF WS-NO < 40
011360	       ADD 1		 TO WS-NO
011370         GO TO BE05.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011390	     MOVE "******"	 TO SAL-H7.
011400	     MOVE "TOTAL FOR ALL DRAWS"
				 TO SAL-H8.
011410	     MOVE W25-COST3	 TO SAL-COST.
011420	     MOVE W25-SELL3	 TO SAL-SELL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011440	     MOVE SPACES	 TO R-DETAIL.
011450	     MOVE "  **  GROSS PROFIT"
				 TO SAL-H8.
011460       COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
011470       COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) 
011480                            * 100.000 + 0.005.
011490	     MOVE W25-SELL1	 TO SAL-SELL.
011500	     MOVE W25-COST1	 TO SAL-GP.
011470       COMPUTE W25-COST1 = (W25-SELL1 / W25-COST3) 
011480                            * 100.000 + 0.005.
011500	     MOVE W25-COST1	 TO SAL-MARKUP.
	     PERFORM CALL-PRINTUTL.
011520	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "NETT SALES + TAX"
				 TO SAL-H8.
	     MOVE W25-TOTAL	 TO SAL-SELL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
011520	     MOVE SPACES	 TO R-DETAIL.
      *
      *    ****    RESET THE PRINTER CONTROLS
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DETAIL.
011530	     GO TO BE999.

011540 BE10.
	   IF NOT (WS-PRINTER = "S")
011550	       IF WS-PAGE > ZERO
011560		   MOVE SPACES	 TO R-DETAIL.
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL.
011580	     MOVE 2		 TO WS-AUDKEY.

011590 BE15.
011600       PERFORM AA50 THRU AA55.
011610	     MOVE "C A S H   D R A W   R E P O R T"
				 TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011630	     MOVE SPACES	 TO R-DETAIL.
011640	     MOVE "CASHDRAW:"	 TO SAL-H1.
011650	     MOVE WS-NO 	 TO SAL-NO.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011670	     MOVE SPACES	 TO R-DETAIL.
011680	     MOVE "ITEM CODE"	 TO SAL-H2.
011690	     MOVE "DESCRIPTION"  TO SAL-H3.
011700	     MOVE "COST"	 TO SAL-H4.
011710	     MOVE "SELLING"	 TO SAL-H5.
011720	     MOVE "G.P.%"	 TO SAL-H6.
	     MOVE "MARKUP"	 TO SAL-H6A.
             MOVE "PW"           TO SAL-H6B.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011740	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

011750 BE20.
011760       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
011770     IF WS-F-ERROR NOT = ZERO
011780	       MOVE ZERO	 TO WS-F-ERROR
011790         GO TO BE30.
011800     IF NOT (AUD-TYPE = 4 OR 6)
011810         GO TO BE30.
011820     IF AUD-SUB NOT = WS-NO
011830         GO TO BE30.
011840     IF AUD-TYPE = 4
011850         GO TO BE25.
      *
      *    ****    P R I N T   I N D V I D U A L   I T E M S
      *
011860	     MOVE AUD-EXT-STOCK	 TO STK-CODE.
011870       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Item deleted or code changed"
				 TO STK-DESC.
011880	     MOVE STK-CODE	 TO SAL-ITEM.
011890	     MOVE STK-DESC	 TO SAL-DESC.
011900	     MOVE AUD-CST	 TO SAL-COST.
011910	     MOVE AUD-SEL	 TO SAL-SELL.
011920	     COMPUTE SAL-GP = ((AUD-SEL - AUD-CST) / AUD-SEL)
011930                          * 100.000 + 0.005.
011920	     COMPUTE SAL-MARKUP = ((AUD-SEL - AUD-CST) / AUD-CST)
011930                              * 100.000 + 0.005.
011940	     ADD AUD-CST	 TO W25-COST1 W25-COST2 W25-COST3.
011950	     ADD AUD-SEL	 TO W25-SELL1 W25-SELL2 W25-SELL3.
	   IF AUD-SUPER = 99
	       MOVE "N/C"	 TO SAL-NOCHARGE
	   ELSE
	       MOVE AUD-SUPER	 TO SAL-SUPER.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
012000	       PERFORM BE15.
012010	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BE999.
012020       GO TO BE30.
      *
      *    ****    P R I N T   S A L E   T O T A L
      *
012030 BE25.
012040	     MOVE "REFERENCE NO:"
				 TO SAL-H7.
012050	     MOVE AUD-REF	 TO SAL-REF.
012060	     MOVE "CASH SALE TOTAL"
				 TO SAL-H8.
012070	     MOVE W25-COST1	 TO SAL-COST.
012080	     MOVE W25-SELL1	 TO SAL-SELL.
012090       COMPUTE SAL-GP = ((W25-SELL1 - W25-COST1) / W25-SELL1)
012100                          * 100.000 + 0.005.
012090       COMPUTE SAL-MARKUP = ((W25-SELL1 - W25-COST1) / W25-COST1)
012100                              * 100.000 + 0.005.
	   IF AUD-PAYMENT = "C"
	       MOVE "CSH"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "Q"
	       MOVE "CHQ"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "D"
	       MOVE "CRD"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "S"
	       MOVE "SPD"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "V"
	       MOVE "VCH"	 TO SAL-PAY
	   ELSE
	   IF AUD-PAYMENT = "N"
	       MOVE "CRN"	 TO SAL-PAY.
012110	     MOVE ZERO		 TO W25-SELL1 W25-COST1.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
012160	       PERFORM BE15.
012170	     MOVE SPACES	 TO R-DETAIL.
      *
      *    ****    C H E C K   E N D   O F   A U D I T   F I L E
      *
012180 BE30.
012190     IF WS-AUDKEY < W70-REP
012200	       ADD 1		 TO WS-AUDKEY
012210         GO TO BE20.
029860	     ADD 150 WS-NO	 GIVING WS-PARKEY.
	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     ADD PAR-CSH	 TO W25-TOTAL.
	     ADD PAR-VATVAL	 TO W25-TOTAL.
      *
      *    ****    P R I N T   C A S H   D R A W   T O T A L
      *
012220	   IF W02-LINAGE < (W02-PRN-LENGTH - 6)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
012260	       PERFORM BE15.
012270	     MOVE SPACES	 TO R-DETAIL.
012280	     MOVE "   ***"	 TO SAL-H7.
012290	     MOVE "CASH DRAW TOTAL"
				 TO SAL-H8.
012300	     MOVE W25-COST2	 TO SAL-COST.
012310	     MOVE W25-SELL2	 TO SAL-SELL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012330	     MOVE SPACES	 TO R-DETAIL.
012340	     MOVE "  GROSS PROFIT"
				 TO SAL-H8.
012350       COMPUTE W25-SELL1 = W25-SELL2 - W25-COST2.
012360       COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL2)
012370                            * 100.000 + 0.005.
012380	     MOVE W25-SELL1	 TO SAL-SELL.
012390	     MOVE W25-COST1	 TO SAL-GP.
012360       COMPUTE W25-COST1 = (W25-SELL1 / W25-COST2)
012370                            * 100.000 + 0.005.
012390	     MOVE W25-COST1	 TO SAL-MARKUP.
	     PERFORM CALL-PRINTUTL.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BE999.
012410	     MOVE SPACES	 TO R-DETAIL.
012420	     MOVE ZERO		 TO W25-SELL1 W25-SELL2
				    W25-COST1 W25-COST2.
012430       GO TO BE06.
012440 BE999.
012450       EXIT.
      /
012460 BF00R           SECTION 50.
012470 BF00.
	     PERFORM RESTORE-SCREEN-3.
012490	     DISPLAY "Busy printing Salesman Report" AT 1220
012500		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
012510	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3
012520				    W25-SELL1 W25-SELL2 W25-SELL3.
012530	     MOVE ZERO		 TO WS-PAGE.
012540	     MOVE 1		 TO WS-NO.

012550 BF05.
012560	   IF W40-SREP(WS-NO) = 1
012570	       GO TO BF10.
012580 BF06.
012590     IF WS-NO < 50
012600	       ADD 1		 TO WS-NO
012610         GO TO BF05.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012630	     MOVE SPACES	 TO R-DETAIL.
012640	     MOVE "  TOTAL - ALL SALESMEN"
				 TO SAL-L5.
012650	     MOVE W25-COST3	 TO SAL-CST.
012660	     MOVE W25-SELL3	 TO SAL-SEL.
012670       COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
012680       COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) 
012690                            * 100.000 + 0.005.
012700	     MOVE W25-SELL1	 TO SAL-PRF.
012710	     MOVE W25-COST1	 TO SAL-GPR.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
012730	     MOVE SPACES	 TO R-DETAIL.
012740	     GO TO BF999.
012750 BF10.
	   IF NOT (WS-PRINTER = "S")
012760	       IF WS-PAGE > ZERO
012770		   MOVE SPACES	 TO R-DETAIL
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL.
012790	     MOVE 2		 TO WS-AUDKEY.
012800	     ADD 100 WS-NO	 GIVING WS-PARKEY.
012810       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE ZERO		 TO W25-CASH W25-REFF W25-INV W25-CRN.
012820 BF15.
012830       PERFORM AA50 THRU AA55.
012840	     MOVE "S A L E S M A N   R E P O R T"
				 TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
012870	     MOVE "SALESMAN:"	 TO SAL-H1.
012880	     MOVE WS-NO 	 TO SAL-NO.
012890	     MOVE PAR-SALESMAN	 TO SAL-SMAN.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
012920	     MOVE "REF NO"	 TO SAL-H9.
012930	     MOVE "DESCRIPTION"  TO SAL-H10.
012940	     MOVE " COST"	 TO SAL-H11.
012950	     MOVE " SELLING"	 TO SAL-H12.
012960	     MOVE " PROFIT"	 TO SAL-H13.
012970	     MOVE "G.P.%"	 TO SAL-H14.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012990	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

013000 BF20.
013010       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
013020     IF WS-F-ERROR NOT = ZERO
013030	       MOVE ZERO	 TO WS-F-ERROR
013040         GO TO BF30.
013050     IF NOT (AUD-TYPE = 5 OR 7)
013060         GO TO BF30.
013070     IF AUD-SMAN NOT = WS-NO
013080         GO TO BF30.
      *
      *    ****    P R I N T   I N D V I D U A L   S A L E S
      *
013090	     MOVE AUD-CSH	 TO SAL-RF.
013100     IF AUD-TYPE = 5
	       IF AUD-SELL < ZERO
		   MOVE "CASH REFUND"
				 TO SAL-DSC
		   ADD AUD-SELL  TO W25-REFF
	       ELSE
013110		   MOVE "CASH SALE"
				 TO SAL-DSC
		   ADD AUD-SELL  TO W25-CASH
	       END-IF
013120     ELSE
013130     IF AUD-SELL < ZERO
013140	       MOVE "CREDIT NOTE"
				 TO SAL-DSC
	       ADD AUD-SELL	 TO W25-CRN
013150     ELSE
013160	       MOVE "INVOICE"	 TO SAL-DSC
	       ADD AUD-SELL	 TO W25-INV.
013170	     MOVE AUD-COST	 TO SAL-CST.
013180	     MOVE AUD-SELL	 TO SAL-SEL.
013190       COMPUTE SAL-PRF = AUD-SELL - AUD-COST.
013200       COMPUTE SAL-GPR = ((AUD-SELL - AUD-COST) / AUD-SELL)
013210                          * 100.000 + 0.005.
013220	     ADD AUD-COST	 TO W25-COST2 W25-COST3.
013230	     ADD AUD-SELL	 TO W25-SELL2 W25-SELL3.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
013280	       PERFORM BF15.
013290	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BF999.
013300 BF30.
013310     IF WS-AUDKEY < W70-REP
013320	       ADD 1		 TO WS-AUDKEY
013330         GO TO BF20.
      *
      *    ****    P R I N T   S A L E S M A N   T O T A L
      *
013340	   IF W02-LINAGE < (W02-PRN-LENGTH - 4)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
013380	       PERFORM BE15.
013390	     MOVE SPACES	 TO R-DETAIL.
013400	     MOVE "  SALESMAN TOTAL"
				 TO SAL-L5.
013410	     MOVE W25-COST2	 TO SAL-CST.
013420	     MOVE W25-SELL2	 TO SAL-SEL.
013430       COMPUTE W25-SELL1 = W25-SELL2 - W25-COST2.
013440       COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL2)
013450                            * 100.000 + 0.005.
013460	     MOVE W25-SELL1	 TO SAL-PRF.
013470	     MOVE W25-COST1	 TO SAL-GPR.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013490	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "SALES"	 TO SAL-H11.
	     MOVE " RETURNS"	 TO SAL-H12.
	     MOVE "   NETT"	 TO SAL-H13.
	     MOVE "RTN %"	 TO SAL-H14.
	     PERFORM CALL-PRINTUTL.
013490	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "CASH SALES"	 TO SAL-DSC.
	     MOVE W25-CASH	 TO SAL-CST.
	     MOVE W25-REFF	 TO SAL-SEL.
	     ADD W25-CASH W25-REFF
				 GIVING SAL-PRF.
	     COMPUTE SAL-GPR ROUNDED = ((W25-REFF * -1) /
					W25-CASH) * 100.0000.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013490	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "INVOICING"	 TO SAL-DSC.
	     MOVE W25-INV	 TO SAL-CST.
	     MOVE W25-CRN	 TO SAL-SEL.
	     ADD W25-INV W25-CRN
				 GIVING SAL-PRF.
	     COMPUTE SAL-GPR ROUNDED = ((W25-CRN * -1) /
					W25-INV) * 100.0000.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013490	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BF999.
013500	     MOVE ZERO		 TO W25-SELL1 W25-SELL2
				    W25-COST1 W25-COST2.
013510       GO TO BF06.
013520 BF999.
013530       EXIT.
      /
013540 BG00R           SECTION 50.
013550 BG00.
	     PERFORM RESTORE-SCREEN-3.
013570	     DISPLAY "Busy printing Turnover Report" AT 1220
013580		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
013590	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3
013600				    W25-SELL1 W25-SELL2 W25-SELL3.
013610	     MOVE ZERO		 TO WS-PAGE.
013620	     MOVE 2		 TO WS-AUDKEY.

013630 BG05.
013640       PERFORM AA50 THRU AA55.
013650	     MOVE "T U R N O V E R   R E P O R T"
				 TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013740	     MOVE SPACES	 TO R-DETAIL.
013670	     MOVE "REF NO"	 TO SAL-H9.
013680	     MOVE "DESCRIPTION"  TO SAL-H10.
013690	     MOVE " COST"	 TO SAL-H11.
013700	     MOVE " SELLING"	 TO SAL-H12.
013710	     MOVE " PROFIT"	 TO SAL-H13.
013720	     MOVE "G.P.%"	 TO SAL-H14.
	     PERFORM CALL-PRINTUTL.
013740	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

013750 BG10.
013760       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
013770     IF WS-F-ERROR NOT = ZERO
013780	       MOVE ZERO	 TO WS-F-ERROR
013790         GO TO BG15.
013800     IF NOT (AUD-TYPE = 5 OR 7)
013810         GO TO BG15.
      *
      *    ****    P R I N T   I N D V I D U A L   S A L E S
      *
013820	     MOVE AUD-CSH	 TO SAL-RF.
013830     IF AUD-TYPE = 5
013840	       MOVE "CASH SALE"  TO SAL-DSC
013850     ELSE
013860     IF AUD-SELL < ZERO
013870	       MOVE "CREDIT NOTE"
				 TO SAL-DSC
013880     ELSE
013890	       MOVE "INVOICE"	 TO SAL-DSC.
013900	     MOVE AUD-COST	 TO SAL-CST.
013910	     MOVE AUD-SELL	 TO SAL-SEL.
013920       COMPUTE SAL-PRF = AUD-SELL - AUD-COST.
013930       COMPUTE SAL-GPR = ((AUD-SELL - AUD-COST) / AUD-SELL)
013940                          * 100.000 + 0.005.
013950	     ADD AUD-COST	 TO W25-COST2 W25-COST3.
013960	     ADD AUD-SELL	 TO W25-SELL2 W25-SELL3.
013970	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014010	       PERFORM BG05.
014020	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BG999.
014030 BG15.
014040     IF WS-AUDKEY < W70-REP
014050	       ADD 1		 TO WS-AUDKEY
014060         GO TO BG10.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013740	     MOVE SPACES	 TO R-DETAIL.
014080	     MOVE "  TOTAL TURNOVER AND GP"
				 TO SAL-L5.
014090	     MOVE W25-COST3	 TO SAL-CST.
014100	     MOVE W25-SELL3	 TO SAL-SEL.
014110       COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
014120       COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) 
014130                            * 100.000 + 0.005.
014140	     MOVE W25-SELL1	 TO SAL-PRF.
014150	     MOVE W25-COST1	 TO SAL-GPR.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
014170	     MOVE SPACES	 TO R-DETAIL.

014180 BG999.
014190       EXIT.
      /
014200 BH00R           SECTION 50.
014210 BH00.
	     PERFORM RESTORE-SCREEN-3.
014230	     DISPLAY "Busy printing detailed Turnover Report" AT 1220
014240		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
014250	     MOVE ZERO		 TO W25-COST1 W25-COST2 W25-COST3
014260				    W25-SELL1 W25-SELL2 W25-SELL3.
014270	     MOVE ZERO		 TO WS-PAGE.
014280	     MOVE 2		 TO WS-AUDKEY.

014290 BH15.
014300       PERFORM AA50 THRU AA55.
014310	     MOVE "T U R N O V E R   R E P O R T"
				 TO R-CPY.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014330	     MOVE SPACES	 TO R-DETAIL.
014340	     MOVE "ITEM CODE"	 TO SAL-H2.
014350	     MOVE "DESCRIPTION"  TO SAL-H3.
014360	     MOVE "COST"	 TO SAL-H4.
014370	     MOVE "SELLING"	 TO SAL-H5.
014380	     MOVE "G.P.%"	 TO SAL-H6.
	     PERFORM CALL-PRINTUTL.
014400	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

014410 BH20.
014420       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
014430     IF WS-F-ERROR NOT = ZERO
014440	       MOVE ZERO	 TO WS-F-ERROR
014450         GO TO BH30.
014460     IF NOT (AUD-TYPE = 3 OR 4 OR 6 OR 8)
014470         GO TO BH30.
014480     IF AUD-TYPE = 3 OR 4
014490         GO TO BH25.
      *
      *    ****    P R I N T   I N D V I D U A L   I T E M S
      *
014500	     MOVE AUD-EXT-STOCK	 TO STK-CODE.
014510       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "Item deleted or code changed"
				 TO STK-DESC.
014520	     MOVE STK-CODE	 TO SAL-ITEM.
014530	     MOVE STK-DESC	 TO SAL-DESC.
014540	     MOVE AUD-CST	 TO SAL-COST.
014550	     MOVE AUD-SEL	 TO SAL-SELL.
014560	     COMPUTE SAL-GP = ((AUD-SEL - AUD-CST) / AUD-SEL)
014570                          * 100.000 + 0.005.
014580	     ADD AUD-CST	 TO W25-COST1 W25-COST2 W25-COST3.
014590	     ADD AUD-SEL	 TO W25-SELL1 W25-SELL2 W25-SELL3.
	   IF AUD-SUPER = 99
	       MOVE "N/C"	 TO SAL-NOCHARGE
	   ELSE
	       MOVE AUD-SUPER	 TO SAL-SUPER.
014600	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014640	       PERFORM BH15.
014650	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BH999.
014660       GO TO BH30.
      *
      *    ****    P R I N T   S A L E   T O T A L
      *
014670 BH25.
014700	   IF AUD-TYPE = 4
	       IF W25-SELL1 < ZERO
		   MOVE "CASH REFUND TOTAL"
				 TO SAL-H8
	       ELSE
014710		   MOVE "CASH SALE TOTAL"
				 TO SAL-H8
	       END-IF
014720     ELSE
010830	       MOVE AUD-AC	 TO R-AC W80-ACNO DEB-ACNO
010850	       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
010860	       MOVE DEB-NAME	 TO R-DNME
010870	       IF DEB-TYPE = 1
010880		   INSPECT R-DNME REPLACING FIRST "*" BY "."
010890		   INSPECT R-DNME REPLACING FIRST "*" BY " "
010900		   INSPECT R-DNME REPLACING FIRST "!" BY " "
	       END-IF
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014840	       MOVE SPACES	 TO R-DETAIL
	       IF W25-SELL1 < ZERO
		   MOVE "CREDIT NOTE TOTAL"
				 TO SAL-H8
	       ELSE
014730		   MOVE "INVOICE TOTAL"
				 TO SAL-H8.
014680	     MOVE "REFERENCE NO:"
				 TO SAL-H7.
014690	     MOVE AUD-REF	 TO SAL-REF.
014740	     MOVE W25-COST1	 TO SAL-COST.
014750	     MOVE W25-SELL1	 TO SAL-SELL.
014760       COMPUTE SAL-GP = ((W25-SELL1 - W25-COST1) / W25-SELL1)
014770                          * 100.000 + 0.005.
014780	     MOVE ZERO		 TO W25-SELL1 W25-COST1.
	     MOVE ZERO		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014790	   IF W02-LINAGE < (W02-PRN-LENGTH - 3)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014830	       PERFORM BH15.
014840	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO SAL-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BH999.
      *
      *    ****    C H E C K   E N D   O F   A U D I T   F I L E
      *
014850 BH30.
014860     IF WS-AUDKEY < W70-REP
014870	       ADD 1		 TO WS-AUDKEY
014880         GO TO BH20.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
013740	     MOVE SPACES	 TO R-DETAIL.
014900	     MOVE "******"	 TO SAL-H7.
014910	     MOVE "TOTAL TURNOVER"
				 TO SAL-H8.
014920	     MOVE W25-COST3	 TO SAL-COST.
014930	     MOVE W25-SELL3	 TO SAL-SELL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
014950	     MOVE SPACES	 TO R-DETAIL.
014960	     MOVE "** GROSS PROFIT"
				 TO SAL-H8.
014970       COMPUTE W25-SELL1 = W25-SELL3 - W25-COST3.
014980       COMPUTE W25-COST1 = (W25-SELL1 / W25-SELL3) 
014990                            * 100.000 + 0.005.
015000	     MOVE W25-SELL1	 TO SAL-SELL.
015010	     MOVE W25-COST1	 TO SAL-GP.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
015030	     MOVE SPACES	 TO R-DETAIL.

015050 BH999.
015060       EXIT.
      /
010490 BI00R	      SECTION 50.
010500 BI00.
	     PERFORM RESTORE-SCREEN-3.
010520	     DISPLAY (12, 20) "Printing Debtor Receipts"
010530			       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
010540	     MOVE 1		 TO WS-S6.

010550 BI02.
010560	     MOVE ZERO		 TO W200-CR(WS-S6).
010570	   IF WS-S6 < 300
010580	       ADD 1		 TO WS-S6
010590	       GO TO BI02.
010600	     MOVE ZERO		 TO W200-DTOT W200-CTOT
010610				    W200-COST W200-SELL W200-NET.
010620	     MOVE ZERO		 TO WS-PAGE.
010630	     MOVE 2		 TO WS-AUDKEY.
010640 BI05.
010650       PERFORM AA50 THRU AA55.
010660	     MOVE "DEBTOR RECEIPTS"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
010680	     MOVE SPACES	 TO R-DETAIL.
010690	     MOVE "AC-NO"	 TO R-H14.
010700	     MOVE "DEBTOR NAME"  TO R-H14A.
010710	     MOVE "REF. NO."	 TO R-H15.
010720	     MOVE "  VALUE"	 TO R-H16.
	     PERFORM CALL-PRINTUTL.
010750	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

010760 BI10.
010770       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
010780     IF WS-F-ERROR NOT = ZERO
010790	       MOVE ZERO	 TO WS-F-ERROR
010800	       GO TO BI15.
010810	   IF NOT (AUD-TYPE = 11)
010820	       GO TO BI15.
010830       MOVE AUD-AC         TO R-AC W80-ACNO DEB-ACNO.
010850       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
010860	     MOVE DEB-NAME	 TO R-DNME.
010870     IF DEB-TYPE = 1
010880         INSPECT R-DNME REPLACING FIRST "*" BY "."
010890         INSPECT R-DNME REPLACING FIRST "*" BY " "
010900         INSPECT R-DNME REPLACING FIRST "!" BY " ".
010910	     MOVE AUD-REF	 TO R-REF.
010930	     MOVE AUD-VAL	 TO R-DEBT.
010940	     ADD AUD-VAL	 TO W200-CTOT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
011020	       PERFORM BI05.
011030	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BI999.

011040 BI15.
011050     IF WS-AUDKEY < W70-REP
011060	       ADD 1		 TO WS-AUDKEY
011070	       GO TO BI10.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
011120	       PERFORM BI05.
011130	     MOVE WS-H4 	 TO R-H15.
011140	     MOVE W200-CTOT	 TO R-DEBT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
011170	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
011190	     MOVE SPACES	 TO R-DETAIL.

011200 BI999.
011210       EXIT.
      /
010490 BJ00R	      SECTION 50.
010500 BJ00.
	     PERFORM RESTORE-SCREEN-3.
010520	     DISPLAY "Printing Discounted Sales" AT 1220
010530		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.

       BJ999.
	     EXIT.
      /
015070 BX000           SECTION 50.
015080 BX00.
015090       DISPLAY "Analyzing audit file " AT 1212
015100                WITH FOREGROUND-COLOR 14 BLINK.
015110     IF W70-REP < 2
015120	       MOVE ZERO	 TO WS-REP1 WS-REP2 WS-REP3
				    WS-REP4 WS-REP5 WS-REP6
				    WS-REP7 WS-REP8 WS-REP9 WS-REP10
015140         GO TO BX999.
015150	     MOVE 2		 TO WS-AUDKEY.

015160 BX05.
015170       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
015180     IF WS-F-ERROR NOT = ZERO
015190	       MOVE ZERO	 TO WS-F-ERROR
015200         GO TO BX10.
015210     IF AUD-TYPE = 5
015220         IF AUD-SUB > 0 AND < 41
015230		   MOVE 1	 TO W40-DREP(AUD-SUB).
015250     IF AUD-TYPE = 5 OR 7
015260         IF AUD-SMAN > 0 AND < 51
015270		   MOVE 1	 TO W40-SREP(AUD-SMAN).

015280 BX10.
015290     IF WS-AUDKEY < W70-REP
015300	       ADD 1		 TO WS-AUDKEY
015310         GO TO BX05.

015320 BX999.
015330       EXIT.
      /
015340 BZ00R           SECTION 50.
015350 BZ00.
	     DISPLAY CLEAR-L25.
015360	     MOVE ZERO		 TO WS-PAGE.
015370     IF NOT (WS-STATOP = "1" OR "8")
015380         PERFORM BZ05
015390         GO TO BZ15.
015400	   IF LS0-DTP NOT = 2
015410	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
015420         IF WS-STATOP = "1"
015430             GO TO BZ999
015440         ELSE
015450             PERFORM BZ05
015460             GO TO BZ15.
015470       DISPLAY (22, 12) "Busy printing Debtors Statistics      "
015480                         WITH FOREGROUND-COLOR 14.
015490	     MOVE 1		 TO WS-NETKEY.
015500       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.

015510 BZ05.
015520       PERFORM AA50 THRU AA55.
015530	     MOVE "STATISTICS REPORT"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015550	     MOVE SPACES	 TO R-DETAIL.

015560 BZ10.
015570	     MOVE "DEBTORS"	 TO R-H26.
	     PERFORM CALL-PRINTUTL.
015590	     MOVE SPACES	 TO R-DETAIL.
015600	     MOVE "TODAY"	 TO R-H28.
015610	     MOVE WS-H13	 TO R-H31.
015620	     MOVE WS-H14	 TO R-H34.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015640	     MOVE SPACES	 TO R-DETAIL.
015650       MOVE "TRANSACTIONS" TO R-H26.
015660	     MOVE WS-H12	 TO R-H27 R-H30 R-H33.
015670	     MOVE WS-H11	 TO R-H29 R-H32 R-H35.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
015690	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       BZ10A.
             MOVE 11             TO WS-PARKEY.

       BZ11.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE 1		 TO WS-S1.

       BZ12.
	   IF PAR-E-DESC(WS-S1) = SPACES
               GO TO BZ14.
015700	     MOVE PAR-E-DESC(WS-S1)
                                 TO R-H36.
015710	     MOVE PAR-T-DAY(WS-S1)
                                 TO R-N1.
015720	     MOVE PAR-T-VAL(WS-S1)
                                 TO R-V5.
015730	     MOVE PAR-T-MTD(WS-S1)
                                 TO R-N2.
015740	     MOVE PAR-T-MTDV(WS-S1)
                                 TO R-V6.
015750	     MOVE PAR-T-YTD(WS-S1)
                                 TO R-N3.
015760	     MOVE PAR-T-YTDV(WS-S1)
                                 TO R-V7.

       BZ13.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM BZ05 THRU BZ10.
	     MOVE SPACES	 TO R-DETAIL.

       BZ14.
           IF WS-S1 < 2
               ADD 1             TO WS-S1
               GO TO BZ12.
           IF WS-PARKEY < 60
               ADD 1             TO WS-PARKEY
               GO TO BZ11.
             PERFORM BZ13.
             MOVE 9              TO WS-PARKEY.
016350       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *
      * 	P R I N T   A L L   V A T   R A T E S
      *
      *    **** INSERT A LOOP TO PRINT EACH VAT RATE
      *
	     MOVE "VAT" 	 TO DPT-SCDE.
	     MOVE 1		 TO DPT-V-RTE.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE "VAT SALES"    TO R-H36.
	     MOVE PAR-DVAT-DAY	 TO R-V5.
             MOVE PAR-DVAT-MTD   TO R-V6.
             MOVE PAR-DVAT-YTD   TO R-V7.
             PERFORM BZ13.
             MOVE "VAT"          TO R-H36.
	     MOVE DPT-O-VAT-DAY  TO R-V5.
016380	     MOVE DPT-O-VAT-MTD  TO R-V6.
016390	     MOVE DPT-O-VAT-YTD  TO R-V7.
             PERFORM BZ13.
016410       MOVE "NON VAT SALES"
                                 TO R-H36.
016420       MOVE PAR-DNGS-DAY   TO R-V5.
016430       MOVE PAR-DNGS-MTD   TO R-V6.
016440       MOVE PAR-DNGS-YTD   TO R-V7.
             PERFORM BZ13.
016460       MOVE "VAT RETURNS"  TO R-H36.
016470	     MULTIPLY -1 BY PAR-CVAT-DAY
				 GIVING R-V5.
016480	     MULTIPLY -1 BY PAR-CVAT-MTD
				 GIVING R-V6.
016490	     MULTIPLY -1 BY PAR-CVAT-YTD
				 GIVING R-V7.
	     PERFORM BZ13.
016520       MOVE "VAT REVERSED" TO R-H36.
016530	     MOVE DPT-O-VRET-DAY TO R-V5.
016530	     MOVE DPT-O-VRET-MTD TO R-V6.
016540	     MOVE DPT-O-VRET-YTD TO R-V7.
             PERFORM BZ13.
             MOVE "NET VAT - DAY"
                                 TO R-H36.
	     COMPUTE R-V5 = DPT-O-VAT-DAY + DPT-O-VRET-DAY.
             PERFORM BZ13.
016560	     MOVE "NON VAT RETURNS"
				 TO R-H36.
016570	     MULTIPLY -1 BY PAR-CNGS-DAY
				 GIVING R-V5.
016580	     MULTIPLY -1 BY PAR-CNGS-MTD
				 GIVING R-V6.
016590	     MULTIPLY -1 BY PAR-CNGS-YTD
				 GIVING R-V7.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 3		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM BZ05 THRU BZ10.
             MOVE SPACES         TO R-DETAIL.
016620       MOVE "ACCOUNT RECORDS" 
                                 TO R-H26.
             PERFORM BZ13.
016650       MOVE "DEBTOR ACCOUNTS" 
                                 TO R-H36.
016660       MOVE DEB-ACCOUNTS   TO R-N2.
             PERFORM BZ13 3 TIMES.
016690       MOVE "THIS MONTH"   TO R-H26.
             PERFORM BZ13.
016720       MOVE "OPENING BALANCE" 
                                 TO R-H36.
016730       MOVE DEB-OPEN       TO R-V6.
             PERFORM BZ13.
016750       MOVE "DEBITS"       TO R-H36.
016760       MOVE DEB-DT         TO R-V6.
             PERFORM BZ13.
016780       MOVE "CREDITS"      TO R-H36.
016790	     MULTIPLY -1 BY DEB-CR
				 GIVING R-V6.
	     PERFORM BZ13.
016810       MOVE "CLOSING BALANCE" 
                                 TO R-H36.
016820       MOVE DEB-OUT        TO R-V6.
             PERFORM BZ13 3 TIMES.
016840       MOVE SPACES         TO R-DETAIL.
016850       MOVE "AGE ANALYSIS" TO R-H26.
             PERFORM BZ13.
016880       MOVE DEB-ONE        TO W25-TOTAL.
016890       ADD DEB-TWO         TO W25-TOTAL.
016900       ADD DEB-THREE       TO W25-TOTAL.
016910       ADD DEB-INTEREST    TO W25-TOTAL.
016920	     SUBTRACT W25-TOTAL FROM DEB-OUT
				 GIVING R-V6.
016930       MOVE "CURRENT"      TO R-H36.
             PERFORM BZ13.
016950       MOVE "30 DAYS"      TO R-H36.
016960       MOVE DEB-ONE        TO R-V6.
             PERFORM BZ13.
016980       MOVE "60 DAYS"      TO R-H36.
016990       MOVE DEB-TWO        TO R-V6.
             PERFORM BZ13.
017010       MOVE "90 DAYS"      TO R-H36.
017020       MOVE DEB-THREE      TO R-V6.
             PERFORM BZ13.
017040       MOVE "INTEREST"     TO R-H36.
017050       MOVE DEB-INTEREST   TO R-V6.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017070	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BZ999.
017080	   IF LS0-STP NOT = 2
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
017160	     DISPLAY (9, 35) "Printed" WITH FOREGROUND-COLOR 14.

017170 BZ15.
017180	   IF NOT (WS-STATOP = "1" OR "3" OR "4" OR "8")
017190         GO TO BZ20.
017200	   IF LS0-STP NOT = 2
017210	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017220         IF WS-STATOP = "8"
017230             GO TO BZ20
017240         ELSE
017250             GO TO BZ999.
017260       DISPLAY (22, 12) "Busy printing Stock Statistics       "
017270                         WITH FOREGROUND-COLOR 14.
017280	     MOVE 3		 TO WS-NETKEY.
017290       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
017300	     MOVE "STOCK"	 TO R-H26.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017320	   IF (WS-STATOP = "4") OR (LS0-DTP NOT = 2)
017330         MOVE SPACES       TO R-DETAIL
017340         MOVE "TODAY"      TO R-H28
017350         MOVE WS-H13       TO R-H31
017360	       MOVE WS-H14	 TO R-H34
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL.
017380	     MOVE SPACES	 TO R-DETAIL.
017390       MOVE "PURCHASES"    TO R-H36.
017400       MOVE STK-CMTD       TO R-V6.
017410       MOVE STK-CYTD       TO R-V7.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017430	     MOVE "ADJUSTMENTS"  TO R-H36.
017440       MOVE STK-ADJMTD     TO R-V6.
017450       MOVE STK-ADJYTD     TO R-V7.
	     PERFORM CALL-PRINTUTL.
017430	     MOVE "SALES"	 TO R-H36.
017440       MOVE STK-SMTD       TO R-V6.
017450       MOVE STK-SYTD       TO R-V7.
	     PERFORM CALL-PRINTUTL.
017470	     MOVE SPACES	 TO R-DETAIL.
017480	     MOVE "STOCK ON HAND"
				 TO R-H36.
017490	     MOVE STK-TOTAL	 TO R-V5.
	     PERFORM CALL-PRINTUTL.
017510	     MOVE SPACES	 TO R-DETAIL.
017520	     MOVE "COST OF SALES"
				 TO R-H26.
017530	     MOVE 8		 TO WS-PARKEY.
017540       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017550	     MOVE PAR-CSHT	 TO R-V5.
017560	     MOVE PAR-CMTD	 TO R-V6.
017570	     MOVE PAR-CYTD	 TO R-V7.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
017620	     MOVE SPACES	 TO R-DETAIL.
017630       DISPLAY (12, 35) "Printed" WITH FOREGROUND-COLOR 14.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BZ999.
017640 BZ20.
017650     IF NOT (WS-STATOP = "2" OR "8")
017660         GO TO BZ999.
017670     IF WS-STATOP = "8"
017680	       IF LS0-DTP = 2 OR LS0-STP = 2
017690		   IF LS0-CRP = 2
017700                 PERFORM BZ05
017710             ELSE
017720                 GO TO BZ999.
017730	   IF LS0-CRP NOT = 2
017740	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
017750         GO TO BZ999.
017760       DISPLAY (22, 12) "Busy printing Creditors Statistics   "
017770                         WITH FOREGROUND-COLOR 14.
             MOVE 2              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
017800       MOVE 6              TO WS-PARKEY.
017810       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO BZ999.
017820	     MOVE "CREDITORS"	 TO R-H26.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017840	     MOVE SPACES	 TO R-DETAIL.
017850	     MOVE "TODAY"	 TO R-H28.
017860	     MOVE WS-H13	 TO R-H31.
017870	     MOVE WS-H14	 TO R-H34.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017890	     MOVE SPACES	 TO R-DETAIL.
017900       MOVE "TRANSACTIONS" TO R-H26.
017910	     MOVE WS-H12	 TO R-H27 R-H30 R-H33.
017920	     MOVE WS-H11	 TO R-H29 R-H32 R-H35.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017940	     MOVE SPACES	 TO R-DETAIL.

       BZ20A.
             MOVE 191            TO WS-PARKEY.

       BZ21.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE 1		 TO WS-S1.
       BZ22.
	   IF PAR-C-DESC(WS-S1) = SPACES
               GO TO BZ24.
015700	     MOVE PAR-C-DESC(WS-S1)
                                 TO R-H36.
015710	     MOVE PAR-C-DAY(WS-S1)
                                 TO R-N1.
015720	     MOVE PAR-C-VAL(WS-S1)
                                 TO R-V5.
015730	     MOVE PAR-C-MTD(WS-S1)
                                 TO R-N2.
015740	     MOVE PAR-C-MTDV(WS-S1)
                                 TO R-V6.
015750	     MOVE PAR-C-YTD(WS-S1)
                                 TO R-N3.
015760	     MOVE PAR-C-YTDV(WS-S1)
                                 TO R-V7.
             PERFORM BZ13.
       BZ24.
           IF WS-S1 < 2
               ADD 1             TO WS-S1
               GO TO BZ22.
           IF WS-PARKEY < 200
               ADD 1             TO WS-PARKEY
               GO TO BZ21.
             MOVE 6              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM BZ13.
018440       MOVE "RAILAGE"      TO R-H36.
018450       MOVE PAR-RAILD      TO R-V5.
018460       MOVE PAR-RAILM      TO R-V6.
             PERFORM BZ13.
018490       MOVE "DISCOUNT"     TO R-H36.
018500       MOVE PAR-DISCD      TO R-V5.
018510       MOVE PAR-DISCM      TO R-V6.
             PERFORM BZ13 3 TIMES.
018540       MOVE "THIS MONTH"   TO R-H26.
             PERFORM BZ13.
018570       MOVE "OPENING BALANCE"
                                 TO R-H36.
018580       MOVE P-OPEN         TO R-V6.
             PERFORM BZ13.
018600       MOVE "DEBITS"       TO R-H36.
018610       MOVE P-DEBT         TO R-V6.
             PERFORM BZ13.
018630       MOVE "CREDITS"      TO R-H36.
018640       MOVE P-CRED         TO R-V6.
             PERFORM BZ13.
018660       MOVE "CLOSING BALANCE" 
                                 TO R-H36.
018670       MOVE P-CLOSE        TO R-V6.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE SPACES	 TO R-DETAIL.
018760       DISPLAY (10, 35) "Printed" WITH FOREGROUND-COLOR 14.

020270 BZ999.
020280       EXIT.
      /
      *    ****    SALES ANALYSIS REPORT    ****
      *
020290 CG00R                  SECTION 51.
020300 CG00.
020310       DISPLAY CLR-SCREEN.
020320       DISPLAY (10, 20) "Busy printing Sales Analysis Report"
020330			       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
020340       DISPLAY "Are you sure Y/N?" AT 1220
020350                WITH FOREGROUND-COLOR 14.
020360 CG02.
020370	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020430	   IF NOT (WS-OPTION = "Y" OR "N")
020440         GO TO CG02.
020450     IF WS-OPTION = "N"
020460         GO TO CG999.
020470	     MOVE ZERO		 TO WS-PAGE.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
020540	     MOVE 1		 TO WS-S6.

020550 CG03.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
020560	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
020570	   IF WS-S6 < 300
020580	       ADD 1		 TO WS-S6
020590         GO TO CG03.
020600	     MOVE ZERO		 TO W200-DTOT W200-CTOT
020610				    W200-COST W200-SELL W200-NET.

020620 CG05.
020630       PERFORM AA60 THRU AA65.
020640	     MOVE "SALES ANALYSIS"
				 TO RC-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
020660	     MOVE SPACES	 TO R-DETAIL.
020670	     MOVE WS-H6 	 TO RS-H26.
020680	     MOVE WS-H5 	 TO RS-H27.
020690	     MOVE "DISCOUNT"	 TO RS-H27A.
020700	     MOVE WS-H15	 TO RS-H28.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
020720	     MOVE SPACES	 TO R-DETAIL.
020730	     MOVE "NO"		 TO RS-H18.
020740	     MOVE WS-H8 	 TO RS-H19.
020750	     MOVE WS-H2 	 TO RS-H20 RS-H22 RS-H23A.
020760	     MOVE WS-H3 	 TO RS-H21 RS-H23 RS-H23B.
020770	     MOVE "INCOME"	 TO RS-H24.
020780	     MOVE "DISCOUNT"	 TO RS-H25.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
020800	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

020810 CG10.
020820	     MOVE " !"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
020860         GO TO CG20.

020830 CG15.
020840	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
020860         GO TO CG20.
	   IF (DPT-SCDE = "VAT")
	       GO TO CG15.
020870	     MOVE DPT-CODE	 TO RS-LEDG.
020880       MOVE DPT-DESC       TO RS-SDESC.
020890	     MULTIPLY DPT-SALES-MTD BY -1
				 GIVING RS-CMTD.
020900	     MULTIPLY DPT-SALES-YTD BY -1
				 GIVING RS-CYTD.
020910       MOVE DPT-RET-MTD    TO RS-DMTD.
020920       MOVE DPT-RET-YTD    TO RS-DYTD.
020930       MOVE DPT-SALE-GL    TO RS-GL1.
020950	     SUBTRACT DPT-SALES-MTD
				 FROM W200-CR(1).
020960	     SUBTRACT DPT-SALES-YTD
				 FROM W200-CR(2).
020970	     ADD DPT-RET-MTD	 TO W200-DT(1).
020980	     ADD DPT-RET-YTD	 TO W200-DT(2).
	     MOVE DPT-DISC-MTD	 TO RS-DSMT.
	     MOVE DPT-DISC-YTD	 TO RS-DSYT.
020940	     MOVE DPT-DISC-GL	 TO RS-GL2.
021050	     ADD DPT-DISC-MTD	 TO W200-DT(3).
021060	     ADD DPT-DISC-YTD	 TO W200-DT(4).

021080 CG16.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
021130	       PERFORM CG05.
021140	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO CG999.
	     GO TO CG15.

021150 CG20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
021260	     MOVE WS-H4 	 TO RS-H19.
021270	     MOVE W200-DT(1)	 TO RS-DMTD.
021280	     MOVE W200-DT(2)	 TO RS-DYTD.
021290	     MOVE W200-CR(1)	 TO RS-CMTD.
021300	     MOVE W200-CR(2)	 TO RS-CYTD.
021310	     MOVE W200-DT(3)	 TO RS-DSMT.
021320	     MOVE W200-DT(4)	 TO RS-DSYT.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
021340	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

021380 CG999.
021390      EXIT.
      /
      *    ****    MONTH-END CLEAR (STATS)    ****
      *
021400 DA00M                  SECTION 52.
021410 DA00.
021420       DISPLAY CLR-SCREEN.
	     MOVE WS-OPTION	 TO WS-STATOP.
021430     IF WS-OPTION = "Y"
021440         DISPLAY S05A
021450     ELSE
021460         DISPLAY S05B.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 10		 TO SHADE-LINES.
021470	     DISPLAY S05C.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     PERFORM SAVE-SCREEN-3.

021480 DA00A.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE 4		 TO TOP-ROW.
	     MOVE 12		 TO BOTTOM-ROW.
	     MOVE 3		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 20		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       DA00A-5.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 DA00A-10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO DA00A-15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO DA00A-10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO DA00A-10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO DA00A-10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO DA00A-10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO DA00A-15
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DA00A-5
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO DA00AF
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO DA00A-5
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < "0" OR > "8"
		   CALL X"E5"
		   GO TO DA00A-5
	       END-IF
	       GO TO DA00A-16
	   ELSE
	       CALL X"E5"
	       GO TO DA00A-5.

       DA00A-15.
	   IF SCREEN-LIN < 4
	       CALL X"E5"
	       GO TO DA00A-5
	   ELSE
	   IF SCREEN-LIN = 4
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 5
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 6
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "7"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "8"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 12
	       MOVE "0"		 TO WS-OPTION.
	   GO TO DA00AF.

       DA00A-16.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 12		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 4		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 5		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 6		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "7"
	       MOVE 10		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "8"
	       MOVE 11		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       DA00AF.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     MOVE WS-OPTION	 TO WS-REPLY.
	     MOVE WS-STATOP	 TO WS-OPTION.
021550	   IF WS-REPLY = "0"
021560         GO TO DA999.
	   IF WS-REPLY = "6"
               GO TO DA40.
	   IF WS-REPLY = "7"
               GO TO DA65.
	   IF WS-REPLY = "8"
               GO TO DA75.
021570	   IF NOT (WS-REPLY = "1" OR "2" OR "3" OR "4" OR "5")
021580         GO TO DA00A.
021590	   IF NOT (WS-REPLY = "1" OR "5")
021600         GO TO DA01.
021610	   IF LS0-DTP NOT = 2
021620	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
021630         GO TO DA01.
021640       MOVE 11             TO WS-PARKEY.
021820       DISPLAY "Clearing DEBTOR transaction statistics" AT 2006
021830                WITH FOREGROUND-COLOR 14.
021840       DISPLAY "Param record :" AT 1806.

       DA00B.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021870       MOVE WS-PARKEY      TO W40-KEY.
021880       DISPLAY W40-KEY AT 1822 WITH FOREGROUND-COLOR 11.
	     MOVE 1		 TO WS-S1.
       DA00C.
015730	     MOVE ZERO		 TO PAR-T-MTD(WS-S1)
015740				    PAR-T-MTDV(WS-S1)
				    PAR-T-VAT(WS-S1).
021700     IF WS-OPTION = "Y"
015750	       MOVE ZERO	 TO PAR-T-YTD(WS-S1)
015760				    PAR-T-YTDV(WS-S1).
           IF WS-S1 < 2
               ADD 1             TO WS-S1
               GO TO DA00C.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 60
               ADD 1             TO WS-PARKEY
               GO TO DA00B.
021770     IF NOT (WS-OPTION = "Y")
021780         GO TO DA01.
             MOVE 1              TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE 1801           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
021820       DISPLAY "Clearing purchase statistics" AT 2006
021830                WITH FOREGROUND-COLOR 14.
021840       DISPLAY "Debtor record :" AT 1806.
             MOVE "     !"       TO DEB-ACNO.
             PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO DA01.
021850 DA00D.
021860       PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               GO TO DA01.
021880	     DISPLAY DEB-ACNO AT 1822 WITH FOREGROUND-COLOR 11.
021890       MOVE DEB-PURYTD     TO DEB-PURPRV.
021900       MOVE ZERO           TO DEB-PURYTD.
021910       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
021940       GO TO DA00D.

021950 DA01.
021960	   IF NOT (WS-REPLY = "2" OR "5")
021970         GO TO DA02.
021980	   IF LS0-CRP NOT = 2
021990	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
022000         GO TO DA02.
             MOVE 191            TO WS-PARKEY.
             MOVE 1801           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
021820       DISPLAY "Clearing CREDITOR transaction statistics" AT 2006
021830                WITH FOREGROUND-COLOR 14.
021840       DISPLAY "Param record :" AT 1806.

       DA01A.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021870       MOVE WS-PARKEY      TO W40-KEY.
021880       DISPLAY W40-KEY AT 1822 WITH FOREGROUND-COLOR 11.
	     MOVE 1		 TO WS-S1.

       DA01B.
015700	     MOVE ZERO		 TO PAR-C-MTD(WS-S1)
015740				    PAR-C-MTDV(WS-S1).
022100     IF WS-OPTION = "Y"
015750	       MOVE ZERO	 TO PAR-C-YTD(WS-S1)
				    PAR-C-YTDV(WS-S1).
           IF WS-S1 < 2
               ADD 1             TO WS-S1
               GO TO DA01B.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 200
               ADD 1             TO WS-PARKEY
               GO TO DA01A.
022060       MOVE 6              TO WS-PARKEY.
022070       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
022080	     MOVE ZERO		 TO PAR-RAILD PAR-RAILM
				    PAR-DISCD PAR-DISCM.
022090       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.    
022150	     DISPLAY "Cleared" AT 0656 WITH FOREGROUND-COLOR 14.

022160 DA02.
022360	   IF NOT (WS-REPLY = "1" OR "5")
022370	       MOVE "  !"	 TO DPT-CODE
022380	       GO TO DA08.
             MOVE 1801           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
021820       DISPLAY "Clearing sales and cost of sales" AT 2006
021830                WITH FOREGROUND-COLOR 14.
021840       DISPLAY "PARAM record :" AT 1806.
             MOVE 7              TO WS-PARKEY.

       DA04.
021870	     MOVE WS-PARKEY	 TO W40-KEY.
021880       DISPLAY W40-KEY AT 1822 WITH FOREGROUND-COLOR 11.

       DA04A.
022410       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE ZERO		 TO PAR-CASH-MTH PAR-CASH-RMTH.
           IF WS-OPTION = "Y"
	       MOVE ZERO	 TO PAR-CASH-YEAR PAR-CASH-RYEAR.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
022390       MOVE 8              TO WS-PARKEY.

022400 DA05.
             PERFORM DA04.
022410       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
022420	     MOVE ZERO		 TO PAR-CMTD PAR-ADJMTD.
022430     IF WS-OPTION = "Y"
022440	       MOVE ZERO	 TO PAR-CYTD.
022450       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
022490	     ADD 1		 TO WS-PARKEY.
022410       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE ZERO           TO PAR-DVAT-MTD
                                    PAR-CVAT-MTD
                                    PAR-DGST-MTD
                                    PAR-CGST-MTD
                                    PAR-DNGS-MTD
                                    PAR-CNGS-MTD.
           IF WS-OPTION = "Y"
               MOVE ZERO         TO PAR-DVAT-YTD
                                    PAR-CVAT-YTD
                                    PAR-DGST-YTD
                                    PAR-CGST-YTD
                                    PAR-DNGS-YTD
                                    PAR-CNGS-YTD.
022450       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
021840       DISPLAY "Sales record :" AT 1806.
	     MOVE "   !"	 TO DPT-CODE.

       DA08.
021870	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO DA12.

022500 DA10.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO DA12.
021880	     DISPLAY DPT-CODE AT 1822 WITH FOREGROUND-COLOR 11.
022520	   IF WS-REPLY = "1" OR "5"
	       IF DPT-SCDE = "VAT"
		   MOVE ZERO	 TO DPT-V-SALES-MTD
				    DPT-V-RET-MTD
				    DPT-O-VAT-MTD
				    DPT-O-VRET-MTD
	       ELSE
022530		   MOVE ZERO	 TO DPT-SALES-MTD
                                    DPT-RET-MTD
                                    DPT-DISC-MTD
                                    DPT-ADJ-MTD
                                    DPT-COST-MTD
				    DPT-CRET-MTD
	       END-IF
022550         IF WS-OPTION = "Y"
		   IF DPT-SCDE = "VAT"
		       MOVE ZERO TO DPT-V-SALES-YTD
				    DPT-V-RET-YTD
				    DPT-O-VAT-YTD
				    DPT-O-VRET-YTD
		   ELSE
022560		       MOVE ZERO TO DPT-SALES-YTD
                                    DPT-RET-YTD
                                    DPT-DISC-YTD
                                    DPT-ADJ-YTD
                                    DPT-COST-YTD
                                    DPT-CRET-YTD.
022640	   IF WS-REPLY = "2" OR "5"
	       IF DPT-SCDE = "VAT"
		   MOVE ZERO	 TO DPT-V-PUR-MTD
				    DPT-V-PRET-MTD
				    DPT-I-VAT-MTD
				    DPT-I-VRET-MTD
	       ELSE
022650		   MOVE ZERO	 TO DPT-PUR-MTD
                                    DPT-PRET-MTD
				    DPT-TRF-MTD
	       END-IF
               IF WS-OPTION = "Y"
		   IF DPT-SCDE = "VAT"
		       MOVE ZERO TO DPT-V-PUR-YTD
				    DPT-V-PRET-YTD
				    DPT-I-VAT-YTD
				    DPT-I-VRET-YTD
		   ELSE
		       MOVE ZERO TO DPT-PUR-YTD
                                    DPT-PRET-YTD
                                    DPT-TRF-YTD.
022570       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
	     GO TO DA10.

022750 DA12.
022790	   IF NOT (WS-REPLY = "3" OR "5")
022800         GO TO DA16.
022810	   IF LS0-STP NOT = 2
022820	       MOVE "Not Installed"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
022830         GO TO DA16.
022860     IF WS-OPTION NOT = "Y"
022870         GO TO DA15A.
022880       MOVE "!"            TO STK-CODE.
             PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA15A.
             MOVE 1801           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
021820       DISPLAY "Clearing STOCK sales statistics" AT 2006
021830                WITH FOREGROUND-COLOR 14.
021840       DISPLAY "Stock record :" AT 1806.

022890 DA15.
022900       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO DA15A.
             DISPLAY STK-CODE AT 1822 WITH FOREGROUND-COLOR 11.
022910       MOVE ZERO           TO STK-YTD STK-YTDC STK-YTDV.
022920       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
022950       GO TO DA15.
022960 DA15A.
022970       MOVE 3              TO WS-NETKEY.
022980       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
022990       MOVE ZERO           TO STK-CMTD STK-ADJMTD 
                                    STK-SMTD STK-REDMTD.
023000     IF WS-OPTION = "Y"
023010	       MOVE ZERO	 TO STK-CYTD STK-ADJYTD
                                    STK-SYTD STK-REDYTD.
023020       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
023030	     DISPLAY "Cleared" AT 0756 WITH FOREGROUND-COLOR 14.

023040 DA16.
023050	   IF NOT (WS-REPLY = "1" OR "5")
023060         GO TO DA30.
023070       MOVE 61             TO WS-PARKEY.
             MOVE 1801           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
021820       DISPLAY "Clearing DEBTOR category statistics" AT 2006
021830                WITH FOREGROUND-COLOR 14.
021840       DISPLAY "Param record :" AT 1806.

       DA18.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM DA04.
	     MOVE 1		 TO WS-S1.

023080 DA20.
023100	     MOVE ZERO		 TO PAR-CDRMTD(WS-S1)
				    PAR-CCRMTD(WS-S1).
023110     IF WS-OPTION = "Y"
023120	       MOVE ZERO	 TO PAR-CDRYTD(WS-S1)
				    PAR-CCRYTD(WS-S1).
           IF WS-S1 < 2
               ADD 1             TO WS-S1
	       GO TO DA20.
023130       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
023140     IF WS-PARKEY < 100
023150         ADD 1             TO WS-PARKEY
023160         GO TO DA18.
023170	     DISPLAY "Cleared" AT 0556 WITH FOREGROUND-COLOR 14.

023180 DA30.
	   IF NOT (WS-REPLY = "4" OR "5")
023190	       GO TO DA38.
022390	     MOVE 101		 TO WS-PARKEY.
             MOVE 1801           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
021820       DISPLAY "Clearing Salesman statistics" AT 2006
021830                WITH FOREGROUND-COLOR 14.
021840       DISPLAY "Param record :" AT 1806.

022400 DA35.
022410       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             PERFORM DA04.
022420       MOVE ZERO           TO PAR-SL-MTD
                                    PAR-CT-MTD
                                    PAR-VC-MTD.
022430     IF WS-OPTION = "Y"
022440         MOVE ZERO         TO PAR-SL-YTD
                                    PAR-CT-YTD
                                    PAR-VC-YTD.
022450       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 150
	       ADD 1		 TO WS-PARKEY
               GO TO DA35.
022150	     DISPLAY "Cleared" AT 0856 WITH FOREGROUND-COLOR 14.

       DA38.
022390	     MOVE 101		 TO WS-PARKEY.
             MOVE 1801           TO CPOS.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
021820	     DISPLAY "Press ANY key to continue" AT 2006
021830                WITH FOREGROUND-COLOR 14.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     GO TO DA00A.
      *
      *    ****    R E D U N D A N T   Q U O T E S
      *
       DA40.
026010       DISPLAY CLR-SCREEN.
             DISPLAY "CLEAR REDUNDANT QUOTATIONS" AT 0227
                      WITH FOREGROUND-COLOR 15.
026030       DISPLAY "This routine will clear redundant quotations" 
                      AT 0408.
026050       DISPLAY "Are you sure Y/N?" AT 0608
026060                WITH FOREGROUND-COLOR 14.

       DA45.
026080	     ACCEPT WS-OPTION AT 0626
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO DA45.
           IF WS-OPTION = "N"
               GO TO DA00.
             MOVE "Q"            TO WS-TYPE.
035570       DISPLAY "Enter Date from which QUOTATIONS are to be kept DD
035580- 	     "/MM/YYYY" AT 0808.
             MOVE ZERO           TO W15-END.

       DA50.
             DISPLAY "Order No:" AT 1008.
             ACCEPT S08.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
018530         GO TO DA50.
             MOVE W15-END        TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Invalid date"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
               GO TO DA50.
             MOVE W20-DTE        TO W15-E-YMD.
000740       MOVE "00000001"     TO SOR-REF
             MOVE ZERO           TO SOR-SEQ
000750       PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
000880     IF WS-F-ERROR = 20
               GO TO DA00.
       DA55.
000920       PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
000930     IF WS-F-ERROR = 20
000940         GO TO DA00.
       DA58.
           IF SOR-TYPE NOT = ZERO
               GO TO DA55.
             DISPLAY SOR-REF AT 1018 WITH FOREGROUND-COLOR 15.
           IF SOR-DOC = WS-TYPE
               IF SOR-DTE < W15-E-YMD
                   MOVE SOR-REF  TO WS-REF
                   GO TO DA60.
             GO TO DA55.

       DA60.
             PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
000920       PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
000930     IF WS-F-ERROR = 20
000940         GO TO DA00.
           IF SOR-REF NOT = WS-REF
               GO TO DA58.        
             GO TO DA60.
      *
      *    ****    R E D U N D A N T   O R D E R S
      *
       DA65.
026010       DISPLAY CLR-SCREEN.
             DISPLAY "CLEAR REDUNDANT SALES ORDERS" AT 0226
                      WITH FOREGROUND-COLOR 15.
026030       DISPLAY "This routine will clear redundant sales orders" 
                      AT 0408.
026050       DISPLAY "Are you sure Y/N?" AT 0608
026060                WITH FOREGROUND-COLOR 14.

       DA70.
026080	     ACCEPT WS-OPTION AT 0626
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO DA70.
           IF WS-OPTION = "N"
               GO TO DA00.
             MOVE "O"            TO WS-TYPE.
035570       DISPLAY "Enter Date from which SLS-ORDERS are to be kept DD
035580- 	     "/MM/YYYY" AT 0808.
             MOVE ZERO           TO W15-END.
             GO TO DA50.
      *
      *    ****    C O N V E R T E D   O R D E R S
      *
       DA75.
026010       DISPLAY CLR-SCREEN.
             DISPLAY "CLEAR CONVERTED ORDERS" AT 0229
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
026030       DISPLAY "This routine will clear converted orders" 
                      AT 0408.
026050       DISPLAY "Are you sure Y/N?" AT 0608
026060                WITH FOREGROUND-COLOR 14.

       DA80.
026080	     ACCEPT WS-OPTION AT 0626
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO DA80.
           IF WS-OPTION = "N"
               GO TO DA00.
             MOVE "I"            TO WS-TYPE.
035570       DISPLAY "Enter Date from which CNV-ORDERS are to be kept DD
035580- 	     "/MM/YYYY" AT 0808.
             MOVE ZERO           TO W15-END.
             GO TO DA50.

023200 DA999.
023210       EXIT.
      /
023220 EA00Y                  SECTION 52.
023230 EA00.
023240	     MOVE "Y"		 TO WS-OPTION.
023250       PERFORM DA00M.
023260 EA999.
023270       EXIT.
      /
      *    ****    CATERGORY ANALYSIS (DEBTORS)    ****
      *
023280 FA00R                  SECTION 53.
023290 FA00.
023300       DISPLAY CLR-SCREEN.
023310	     DISPLAY "Busy printing Category Analysis Report" AT 1020
023320		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
023330       DISPLAY "Are you sure Y/N?" AT 1220
023340                WITH FOREGROUND-COLOR 14.
023350 FA02.
023360	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
023420	   IF NOT (WS-OPTION = "Y" OR "N")
023430         GO TO FA02.
023440     IF WS-OPTION = "N"
023450         GO TO FA999.
023460	     MOVE ZERO		 TO WS-PAGE.
023510	     MOVE 1		 TO WS-S6.

023520 FA03.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
023530	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
023540     IF WS-S6 < 300
023550	       ADD 1		 TO WS-S6
023560         GO TO FA03.
023570	     MOVE ZERO		 TO W200-DTOT W200-CTOT
023580				    W200-COST W200-SELL W200-NET.

023590 FA05.
023600       PERFORM AA50 THRU AA55.
023610	     MOVE "CATEGORY ANALYSIS"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
023630	     MOVE SPACES	 TO R-DETAIL.
023640	     MOVE WS-H6 	 TO R-H24.
023650	     MOVE WS-H5 	 TO R-H25.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
023670	     MOVE SPACES	 TO R-DETAIL.
023680	     MOVE "CAT" 	 TO R-H18.
023690	     MOVE WS-H8 	 TO R-H19.
023700	     MOVE WS-H2 	 TO R-H20 R-H22.
023710	     MOVE WS-H3 	 TO R-H21 R-H23.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
023730	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

023740 FA10.
023750       MOVE 61             TO WS-PARKEY.

023760 FA15.
023770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 1              TO WS-S1.
       FA18.
023780	   IF PAR-CAT(WS-S1) = ZERO
023790         GO TO FA20.
023800	     MOVE PAR-CAT(WS-S1)
                                 TO R-LEDG.
023810	     MOVE PAR-CDESC(WS-S1)
                                 TO R-SDESC.
023820	     MOVE PAR-CDRMTD(WS-S1)
                                 TO R-DMTD.
023830	     MOVE PAR-CDRYTD(WS-S1)
                                 TO R-DYTD.
023840	     MOVE PAR-CCRMTD(WS-S1)
                                 TO R-CMTD.
023850	     MOVE PAR-CCRYTD(WS-S1)
                                 TO R-CYTD.
023860	     ADD PAR-CDRMTD(WS-S1)
				 TO W200-DT(1).
023870	     ADD PAR-CDRYTD(WS-S1)
				 TO W200-DT(2).
023880	     ADD PAR-CCRMTD(WS-S1)
				 TO W200-CR(1).
023890	     ADD PAR-CCRYTD(WS-S1)
				 TO W200-CR(2).
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
023940	       PERFORM FA05.
023950	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO FA999.
023960 FA20.
           IF WS-S1 < 2
               ADD 1             TO WS-S1
               GO TO FA18.
023970     IF WS-PARKEY < 100
023980	       ADD 1		 TO WS-PARKEY
023990         GO TO FA15.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
024010	     MOVE WS-H4 	 TO R-H19.
024020	     MOVE W200-DT(1)	 TO R-DMTD.
024030	     MOVE W200-DT(2)	 TO R-DYTD.
024040	     MOVE W200-CR(1)	 TO R-CMTD.
024050	     MOVE W200-CR(2)	 TO R-CYTD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
024070	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.

024090 FA999.
024100       EXIT.
      /
      *    ****    PURCHASES ANALYSIS    ****
      *
024110 GA00R                  SECTION 54.
024120 GA00.
024130       DISPLAY CLR-SCREEN.
024140       DISPLAY (10, 20) "Busy printing Purchases Analysis"
024150			       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
024160       DISPLAY "Are you sure Y/N?" AT 1220
024170                WITH FOREGROUND-COLOR 14.
024180 GA02.
024190	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024250	   IF NOT (WS-OPTION = "Y" OR "N")
024260         GO TO GA02.
024270     IF WS-OPTION = "N"
024280         GO TO GA999.
024290	     MOVE ZERO		 TO WS-PAGE.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
024360	     MOVE 1		 TO WS-S6.

024370 GA03.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
024380	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
024390     IF WS-S6 < 300
024400	       ADD 1		 TO WS-S6
024410         GO TO GA03.
024420	     MOVE ZERO		 TO W200-DTOT W200-CTOT W200-ADJ
024430				    W200-COST W200-SELL W200-NET.

024440 GA05.
024450       PERFORM AA60 THRU AA65.
024460	     MOVE "PURCHASES ANALYSIS"
				 TO RC-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
024480	     MOVE SPACES	 TO R-DETAIL.
024490	     MOVE WS-H6 	 TO RP-H26.
024500	     MOVE WS-H5 	 TO RP-H27.
024510	     MOVE WS-H15	 TO RC-H25.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
024530	     MOVE SPACES	 TO R-DETAIL.
024540	     MOVE "NUMBER"	 TO RP-H18.
024550	     MOVE WS-H8 	 TO RP-H19.
024560	     MOVE WS-H2 	 TO RP-H20 RP-H22.
024570	     MOVE WS-H3 	 TO RP-H21 RP-H23.
024580	     MOVE "ADJUSTMENTS"  TO RP-H23A.
024580	     MOVE " STOCK"	 TO RC-H25.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
024600	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

024610 GA10.
	     MOVE "   !"	 TO DPT-CODE.

       GA12.
021870	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO GA20.

024630 GA15.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
024660	       GO TO GA20.
	   IF DPT-CODE = "YYYY"
	       GO TO GA15.
024670	     MOVE DPT-CODE	 TO RP-LEDG.
024680       MOVE DPT-DESC       TO RP-SDESC.
             MOVE DPT-ADJ-MTD    TO RP-ADJ.
             ADD DPT-ADJ-MTD     TO W200-ADJ.
024710       MOVE DPT-PUR-MTD    TO RP-DMTD.
024720       MOVE DPT-PUR-YTD    TO RP-DYTD.
024730       MOVE DPT-PRET-MTD   TO RP-CMTD.
024740       MOVE DPT-PRET-YTD   TO RP-CYTD.
024750	     ADD DPT-PUR-MTD	 TO W200-DT(1).
024760	     ADD DPT-PUR-YTD	 TO W200-DT(2).
024770	     ADD DPT-PRET-MTD	 TO W200-CR(1).
024780	     ADD DPT-PRET-YTD	 TO W200-CR(2).
024790       MOVE DPT-PUR-GL     TO RC-GL2.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
024850	       PERFORM GA05.
024860	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO GA999.
	     GO TO GA15.

024870 GA20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
024920	     MOVE WS-H4 	 TO RC-H19.
024930	     MOVE W200-DT(1)	 TO RP-DMTD.
024940	     MOVE W200-DT(2)	 TO RP-DYTD.
024950	     MOVE W200-CR(1)	 TO RP-CMTD.
024960	     MOVE W200-CR(2)	 TO RP-CYTD.
	     MOVE W200-ADJ	 TO RP-ADJ.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
024980	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

025020 GA999.
025030      EXIT.
      /
      *    ****    COST-OF-SALES ANALYSIS    ****
      *
025040 HA00R                  SECTION 55.
025050 HA00.
025060       DISPLAY CLR-SCREEN.
025070       DISPLAY (10, 20) "Busy printing Cost-of-Sales Analysis"
025080			       WITH FOREGROUND-COLOR 7 HIGHLIGHT.
025090       DISPLAY "Are you sure Y/N?" AT 1220
025100                WITH FOREGROUND-COLOR 14.
025110 HA02.
025120	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
025180	   IF NOT (WS-OPTION = "Y" OR "N")
025190         GO TO HA02.
025200     IF WS-OPTION = "N"
025210         GO TO HA999.
025220	     MOVE ZERO		 TO WS-PAGE.
      *
      *    ****   1 7	C P I	( C O N D E N S E D   P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 3		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
025290	     MOVE 1		 TO WS-S6.

025300 HA03.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
025310	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
025320     IF WS-S6 < 300
025330	       ADD 1		 TO WS-S6
025340         GO TO HA03.
025350	     MOVE ZERO		 TO W200-DTOT W200-CTOT
025360				    W200-COST W200-SELL W200-NET.

025370 HA05.
025380       PERFORM AA60 THRU AA65.
025390	     MOVE "COST-OF-SALES ANALYSIS"
				 TO RC-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
025410	     MOVE SPACES	 TO R-DETAIL.
025420	     MOVE WS-H6 	 TO RC-H26.
025430	     MOVE WS-H5 	 TO RC-H27.
025440	     MOVE WS-H15	 TO RC-H28.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
025460	     MOVE SPACES	 TO R-DETAIL.
025470	     MOVE "NUMBER"	 TO RC-H18.
025480	     MOVE WS-H8 	 TO RC-H19.
025490	     MOVE WS-H2 	 TO RC-H20 RC-H22.
025500	     MOVE WS-H3 	 TO RC-H21 RC-H23.
025510	     MOVE "  C-O-S"	 TO RC-H24.
025520	     MOVE "  STOCK"	 TO RC-H25.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
025540	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

025550 HA10.
	     MOVE "   !"	 TO DPT-CODE.

       HA12.
021870	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO HA20.

025570 HA15.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO HA20.
025590	   IF DPT-CODE = "YYYY"
	       GO TO HA15.
025610	     MOVE DPT-CODE	 TO RC-LEDG.
025620       MOVE DPT-DESC       TO RC-SDESC.
025650       MOVE DPT-COST-MTD   TO RC-CMTD.
025660       MOVE DPT-COST-YTD   TO RC-CYTD.
025670       MOVE DPT-CRET-MTD   TO RC-DMTD.
025680       MOVE DPT-CRET-YTD   TO RC-DYTD.
025690	     ADD DPT-COST-MTD	 TO W200-CR(1).
025700	     ADD DPT-COST-YTD	 TO W200-CR(2).
025710	     ADD DPT-CRET-MTD	 TO W200-DT(1).
025720	     ADD DPT-CRET-YTD	 TO W200-DT(2).
025730       MOVE DPT-COST-GL    TO RC-GL1.
025740       MOVE DPT-PUR-GL     TO RC-GL2.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
025800	       PERFORM HA05.
025810	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO HA999.
	     GO TO HA15.

025820 HA20.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
025870	     MOVE WS-H4 	 TO RC-H19.
025880	     MOVE W200-DT(1)	 TO RC-DMTD.
025890	     MOVE W200-DT(2)	 TO RC-DYTD.
025900	     MOVE W200-CR(1)	 TO RC-CMTD.
025910	     MOVE W200-CR(2)	 TO RC-CYTD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
025930	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.

025970 HA999.
025980      EXIT.
      /
025990 IA000           SECTION 56.
026000 IA00.
026010       DISPLAY CLR-SCREEN.
026030       DISPLAY (3, 20) "Busy Printing Stock Category Totals"
026040			      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
026050       DISPLAY "Are you sure Y/N?" AT 0820
026060                WITH FOREGROUND-COLOR 14.

026070 IA02.
026080	     ACCEPT WS-OPTION AT 0838
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
026150         GO TO IA02.
026160     IF WS-OPTION = "N"
026170         GO TO IA999.
026180       DISPLAY (12, 20) "Stock Record:".
026190	     MOVE ZERO		 TO WS-PAGE.
026200	     MOVE 1		 TO WS-S6.

026210 IA03.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
026220	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
026230     IF WS-S6 < 300
026240	       ADD 1		 TO WS-S6
026250         GO TO IA03.
026260	     MOVE ZERO		 TO W200-DTOT W200-CTOT
026270				    W200-COST W200-SELL W200-NET.

026280 IA05.
026290       PERFORM AA50 THRU AA55.
026300	     MOVE "STOCK CATEGORY TOTALS"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DETAIL.
026330	     MOVE "CAT" 	 TO RT-H18.
026340	     MOVE WS-H8 	 TO RT-H19.
026350	     MOVE "T-COST"	 TO RT-H20.
026360	     MOVE "T-SELL"	 TO RT-H21.
026370	     MOVE "MARKUP"	 TO RT-H22.
026380	     MOVE "G.P. %"	 TO RT-H23.
	     PERFORM CALL-PRINTUTL.
026400	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

026410 IA08.
026420       MOVE "!"            TO STK-CODE.
             PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO IA15.

026430 IA10.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO IA999.
026440       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO IA15.
026460       DISPLAY STK-CODE AT 1234 WITH FOREGROUND-COLOR 11.
026510	   IF STK-IND > 1 AND < 8
026520         GO TO IA10.
026530     IF STK-QUANT = ZERO 
026540         GO TO IA10.
026550	     MOVE STK-QUANT	 TO W200-DTOT.
026560       MULTIPLY STK-AVGE BY W200-DTOT GIVING W200-CTOT.
	     MOVE 1		 TO WS-S6.

       IA12.
	   IF W200-LEDG(WS-S6) = SPACES
	       MOVE STK-LDG	 TO W200-LEDG(WS-S6)
	   ELSE
	   IF NOT (STK-LDG = W200-LEDG(WS-S6))
	       ADD 1		 TO WS-S6
	       GO TO IA12.
026580	     ADD W200-CTOT	 TO W200-CR(WS-S6).
026590	     ADD W200-CTOT	 TO W200-COST.
026600       MULTIPLY W200-DTOT BY STK-SELL GIVING W200-CTOT.
026610	     ADD W200-CTOT	 TO W200-DT(WS-S6).
026620	     ADD W200-CTOT	 TO W200-SELL.
             GO TO IA10.

026630 IA15.
026670	     MOVE 1		 TO WS-S6.

026680 IA20.
	   IF W200-LEDG(WS-S6) = SPACES
	       GO TO IA25.
026690	   IF (W200-DT(WS-S6) = ZERO) AND (W200-CR(WS-S6) = ZERO)
026840	       ADD 1		 TO WS-S6
026700	       GO TO IA20.
026710	     MOVE W200-LEDG(WS-S6)
				 TO DPT-CODE.
026720       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
026730	     MOVE DPT-CODE	 TO RT-LEDG.
026740	     MOVE DPT-DESC	 TO RT-SDESC.
026750	     MOVE W200-CR(WS-S6)
				 TO RT-DMTD W200-CTOT.
026760	     MOVE W200-DT(WS-S6)
				 TO RT-DYTD W200-DTOT.
026770       SUBTRACT W200-CTOT FROM W200-DTOT 
                                 GIVING W25-TOTAL.
026780	     COMPUTE RT-MUP = (W25-TOTAL / W200-CTOT) * 100.
026790	     COMPUTE RT-GP = (W25-TOTAL / W200-DTOT) * 100.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026810	     MOVE SPACES	 TO R-L1.
	     ADD 1		 TO WS-S6.
	     GO TO IA20.

026820 IA25.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026870	     MOVE "TOTAL"	 TO RT-SDESC.
026880	     MOVE W200-COST	 TO RT-DMTD.
026890	     MOVE W200-SELL	 TO RT-DYTD.
026900	     SUBTRACT W200-COST FROM W200-SELL
				 GIVING W25-TOTAL.
026910	     COMPUTE RT-MUP = (W25-TOTAL / W200-COST) * 100.
026920	     COMPUTE RT-GP = (W25-TOTAL / W200-SELL) * 100.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
026940	     MOVE SPACES	 TO R-L1.
026950	     MOVE 3		 TO WS-NETKEY.
026960       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
026970       MOVE W200-COST      TO STK-TOTAL.
026980       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.

026990 IA999.
027000       EXIT.
      /
      *    ****    STOCK SALES & GROSS PROFIT ENQUIRY
      *
027010 JA000           SECTION 57.
027020 JA00.
027030       DISPLAY CLR-SCREEN.
027050       DISPLAY "STOCK SALES & GP ENQUIRY" AT 0227
027060                WITH FOREGROUND-COLOR 15.
027070       DISPLAY "Are you sure Y/N?" AT 0620
027080                WITH FOREGROUND-COLOR 14.
027090 JA02.
027100	     ACCEPT WS-OPTION AT 0638
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
027160	   IF NOT (WS-OPTION = "Y" OR "N")
027170         GO TO JA02.
027180     IF WS-OPTION = "N"
027190         GO TO JA999.
027200       DISPLAY (8, 12) "Accumulating totals".
027210       DISPLAY (10, 12) "Sales Ledger :".
027240	     MOVE 1		 TO WS-S6.

027250 JA03.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
027260	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
027270	   IF WS-S6 < 300
027280	       ADD 1		 TO WS-S6
027290         GO TO JA03.
027300	     MOVE ZERO		 TO W200-DTOT W200-CTOT
027310				    W200-COST W200-SELL W200-NET.
020820	     MOVE " !"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
020860	       GO TO JA10.

020830 JA05.
020840	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
020860	       GO TO JA10.
	   IF (DPT-CODE = "YYYY") OR
	      (DPT-SCDE = "VAT")
	       GO TO JA05.
027340	     DISPLAY (10, 27) DPT-CODE WITH FOREGROUND-COLOR 11.
	   IF DPT-SALES-MTD < ZERO
               MULTIPLY -1 BY DPT-SALES-MTD
               COMPUTE DPT-SALES-YTD = DPT-SALES-YTD + 
                      (DPT-SALES-MTD * 2)
               PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
027370       DISPLAY (10, 33) DPT-DESC WITH FOREGROUND-COLOR 11.
027380	     ADD DPT-SALES-MTD	 TO W200-DT(1).
027390	     ADD DPT-SALES-YTD	 TO W200-DT(2).
027400	     ADD DPT-RET-MTD	 TO W200-DT(3).
027410	     ADD DPT-RET-YTD	 TO W200-DT(4).
027440	     ADD DPT-COST-MTD	 TO W200-DT(7).
027450	     ADD DPT-COST-YTD	 TO W200-DT(8).
027460	     ADD DPT-CRET-MTD	 TO W200-DT(9).
027470	     ADD DPT-CRET-YTD	 TO W200-DT(10).
027480	     ADD DPT-DISC-MTD	 TO W200-DT(5).
027490	     ADD DPT-DISC-YTD	 TO W200-DT(6).
	     GO TO JA05.

027510 JA10.
	     MOVE 0601		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
027570       DISPLAY (4, 12) "M.T.D. Sales   :".
027580       DISPLAY (5, 12) "       Returns :".
027590	     DISPLAY (6, 30) "-------------".
027600       DISPLAY (7, 12) "       Gross   :".
027610       DISPLAY (8, 12) " Less Discount :".
027620	     DISPLAY (9, 30) "-------------".
027630       DISPLAY (10, 12) "Nett Sales     :".
027640       DISPLAY (11, 12) "     Cost      :".
027650       DISPLAY (12, 12) "Gross Profit   :".
027660	     DISPLAY (13, 30) "=============".
027670       DISPLAY (14, 12) "Y.T.D. Sales   :".
027680       DISPLAY (15, 12) "       Returns :".
027690	     DISPLAY (16, 30) "-------------".
027700       DISPLAY (17, 12) "       Gross   :".
027710       DISPLAY (18, 12) " Less Discount :".
027720	     DISPLAY (19, 30) "-------------".
027730       DISPLAY (20, 12) "Nett Sales     :".
027740       DISPLAY (21, 12) "     Cost      :".
027750       DISPLAY (22, 12) "Gross Profit   :".
027760	     DISPLAY (23, 30) "=============".
027770	     DISPLAY (12, 51) "%".
027780	     DISPLAY (22, 51) "%".
027790	     MOVE W200-DT(1)	 TO W40-AMOUNT W200-CTOT.
027800       DISPLAY (4, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
027810	     MOVE W200-DT(3)	 TO W40-AMOUNT W200-DTOT.
027820       DISPLAY (5, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
027830	     SUBTRACT W200-DTOT  FROM W200-CTOT.
027840	     MOVE W200-CTOT	 TO W40-AMOUNT.
027850       DISPLAY (7, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
027860	     MOVE W200-DT(5)	 TO W40-AMOUNT W200-DTOT.
027870       DISPLAY (8, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
027880	     SUBTRACT W200-DTOT  FROM W200-CTOT.
027890	     MOVE W200-CTOT	 TO W40-AMOUNT W200-SELL.
027900       DISPLAY (10, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
027910	     SUBTRACT W200-DT(9) FROM W200-DT(7)
				 GIVING W200-COST.
027920	     MOVE W200-COST	 TO W40-AMOUNT.
027930       DISPLAY (11, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
027940       COMPUTE W200-DTOT = W200-SELL - W200-COST.
027950	     MOVE W200-DTOT	 TO W40-AMOUNT.
027960       DISPLAY (12, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
027970       COMPUTE W200-COST = (W200-DTOT / W200-SELL) * 100.
027980	     MOVE W200-COST	 TO W40-PERC.
027990	     DISPLAY (12, 44) W40-PERC WITH FOREGROUND-COLOR 11.
028000	     MOVE W200-DT(2)	 TO W40-AMOUNT W200-CTOT.
028010       DISPLAY (14, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
028020	     MOVE W200-DT(4)	 TO W40-AMOUNT W200-DTOT.
028030       DISPLAY (15, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
028040	     SUBTRACT W200-DTOT  FROM W200-CTOT.
028050	     MOVE W200-CTOT	 TO W40-AMOUNT.
028060       DISPLAY (17, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
028070	     MOVE W200-DT(6)	 TO W40-AMOUNT W200-DTOT.
028080       DISPLAY (18, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
028090	     SUBTRACT W200-DTOT  FROM W200-CTOT.
028100	     MOVE W200-CTOT	 TO W40-AMOUNT W200-SELL.
028110       DISPLAY (20, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
028120	     SUBTRACT W200-DT(10) FROM W200-DT(8)
				 GIVING W200-COST.
028130	     MOVE W200-COST	 TO W40-AMOUNT.
028140       DISPLAY (21, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
028150       COMPUTE W200-DTOT = W200-SELL - W200-COST.
028160	     MOVE W200-DTOT	 TO W40-AMOUNT.
028170       DISPLAY (22, 30) W40-AMOUNT WITH FOREGROUND-COLOR 11.
028180       COMPUTE W200-COST = (W200-DTOT / W200-SELL) * 100.
028190	     MOVE W200-COST	 TO W40-PERC.
028200	     DISPLAY (22, 44) W40-PERC WITH FOREGROUND-COLOR 11.
	     DISPLAY "P" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Print,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "ANY " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "other KEY to continue"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION = "P"
		   PERFORM JA15.
	     GO TO JA999.

       JA15.
	     PERFORM AA50.
009500	     MOVE "GROSS PROFIT" TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
009590	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE 1		 TO WS-ADVANCE.
027570	     MOVE "M.T.D. Sales   :"
				 TO GP-HEAD.
	     MOVE W200-DT(1)	 TO GP-VALUE W200-CTOT.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027580	     MOVE "       Returns :"
				 TO GP-HEAD.
	     MOVE W200-DT(3)	 TO GP-VALUE W200-DTOT.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027830	     SUBTRACT W200-DTOT  FROM W200-CTOT.
027590	     MOVE "-------------"
				 TO GP-HEAD2.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027600	     MOVE "       Gross   :"
				 TO GP-HEAD.
027840	     MOVE W200-CTOT	 TO GP-VALUE.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027610	     MOVE " Less Discount :"
				 TO GP-HEAD.
027860	     MOVE W200-DT(5)	 TO GP-VALUE W200-DTOT.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027620	     MOVE "-------------"
				 TO GP-HEAD2.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027630	     MOVE "Nett Sales     :"
				 TO GP-HEAD.
027880	     SUBTRACT W200-DTOT  FROM W200-CTOT.
027890	     MOVE W200-CTOT	 TO GP-VALUE W200-SELL.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027910	     SUBTRACT W200-DT(9) FROM W200-DT(7)
				 GIVING W200-COST.
027920	     MOVE W200-COST	 TO GP-VALUE.
027650	     MOVE "Gross Profit   :"
				 TO GP-HEAD.
027940	     COMPUTE W200-DTOT = W200-SELL - W200-COST.
027950	     MOVE W200-DTOT	 TO GP-VALUE.
027970	     COMPUTE W200-COST = (W200-DTOT / W200-SELL) * 100.
027980	     MOVE W200-COST	 TO GP-PERC.
027770	     MOVE "%"		 TO GP-P.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027660	     MOVE "============="
				 TO GP-HEAD2.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027670	     MOVE "Y.T.D. Sales   :"
				 TO GP-HEAD.
028000	     MOVE W200-DT(2)	 TO GP-VALUE W200-CTOT.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027680	     MOVE "       Returns :"
				 TO GP-HEAD.
028020	     MOVE W200-DT(4)	 TO GP-VALUE W200-DTOT.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027690	     MOVE "-------------"
				 TO GP-HEAD2.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027700	     MOVE "       Gross   :"
				 TO GP-HEAD.
028040	     SUBTRACT W200-DTOT  FROM W200-CTOT.
028050	     MOVE W200-CTOT	 TO GP-VALUE.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027710	     MOVE " Less Discount :"
				 TO GP-HEAD.
028070	     MOVE W200-DT(6)	 TO GP-VALUE W200-DTOT.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027720	     MOVE "-------------"
				 TO GP-HEAD2.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027730	     MOVE "Nett Sales     :"
				 TO GP-HEAD.
028090	     SUBTRACT W200-DTOT  FROM W200-CTOT.
028100	     MOVE W200-CTOT	 TO GP-VALUE W200-SELL.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027740	     MOVE "     Cost      :"
				 TO GP-HEAD.
028120	     SUBTRACT W200-DT(10) FROM W200-DT(8)
				 GIVING W200-COST.
028130	     MOVE W200-COST	 TO GP-VALUE.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027750	     MOVE "Gross Profit   :"
				 TO GP-HEAD.
027780	     MOVE "%"		 TO GP-P.
028150	     COMPUTE W200-DTOT = W200-SELL - W200-COST.
028160	     MOVE W200-DTOT	 TO GP-VALUE.
028180	     COMPUTE W200-COST = (W200-DTOT / W200-SELL) * 100.
028190	     MOVE W200-COST	 TO GP-PERC.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
027760	     MOVE "============="
				 TO GP-HEAD2.
	     PERFORM CALL-PRINTUTL.
009810	     MOVE SPACES	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

028240 JA999.
028250       EXIT.
      /
      *    ****    SALESMAN ANALYSIS (MARKUP)    ****
      *
028260 KA00R                  SECTION 58.
028270 KA00.
028280       DISPLAY CLR-SCREEN.
028290       DISPLAY "SALESMAN ANALYSIS" AT 0227
                      WITH FOREGROUND-COLOR 15.
028300       DISPLAY "Busy printing Salesman analysis report" AT 1020
028310		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
028320       DISPLAY "Are you sure Y/N?" AT 1220
028330                WITH FOREGROUND-COLOR 14.
028340 KA02.
028350	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
028410	   IF NOT (WS-OPTION = "Y" OR "N")
028420         GO TO KA02.
028430     IF WS-OPTION = "N"
028440         GO TO KA999.
028450	     MOVE ZERO		 TO WS-PAGE.
028460	     MOVE 1		 TO WS-S6.

028470 KA03.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
028480	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
028490     IF WS-S6 < 300
028500	       ADD 1		 TO WS-S6
028510         GO TO KA03.
028520	     MOVE ZERO		 TO W200-DTOT W200-CTOT
028530				    W200-COST W200-SELL W200-NET.

028540 KA05.
028550       PERFORM AA50 THRU AA55.
028560	     MOVE "SALESMAN ANALYSIS"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
028580	     MOVE SPACES	 TO R-DETAIL.
028590	     MOVE "SALES"	 TO R-H24.
028600	     MOVE "  COST "	 TO R-H25.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
028620	     MOVE "S/M" 	 TO R-H18.
028630	     MOVE "SALESMAN NAME"
				 TO R-H19.
028640	     MOVE WS-H2 	 TO R-H20 R-H22.
028650	     MOVE WS-H3 	 TO R-H21 R-H23.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
028670	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

028680 KA10.
028690	     MOVE 101		 TO WS-PARKEY.
028700	     MOVE 1		 TO WS-S1.
028710 KA15.
028720       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
028730     IF PAR-SALESMAN = SPACES
028740         GO TO KA20.
028750	     MOVE WS-S1 	 TO R-LEDG.
028760	     MOVE PAR-SALESMAN	 TO R-SDESC.
028770	     MOVE PAR-SL-MTD	 TO R-DMTD.
028780	     MOVE PAR-SL-YTD	 TO R-DYTD.
028790	     MOVE PAR-CT-MTD	 TO R-CMTD.
028800	     MOVE PAR-CT-YTD	 TO R-CYTD.
028810	     ADD PAR-SL-MTD	 TO W200-DT(1).
028820	     ADD PAR-SL-YTD	 TO W200-DT(2).
028830	     ADD PAR-CT-MTD	 TO W200-CR(1).
028840	     ADD PAR-CT-YTD	 TO W200-CR(2).
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
028860	     MOVE SPACES	 TO R-DETAIL.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO KA999.
028870 KA20.
028880     IF WS-PARKEY < 150
028890	       ADD 1		 TO WS-PARKEY WS-S1
028900         GO TO KA15.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
028920	     MOVE WS-H4 	 TO R-H19.
028930	     MOVE W200-DT(1)	 TO R-DMTD.
028940	     MOVE W200-DT(2)	 TO R-DYTD.
028950	     MOVE W200-CR(1)	 TO R-CMTD.
028960	     MOVE W200-CR(2)	 TO R-CYTD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
028980	     MOVE WS-H1 	 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.

029000 KA999.
029010       EXIT.
      /
025990 LA000           SECTION 59.
026000 LA00.
026010       DISPLAY CLR-SCREEN.
             DISPLAY "TURNOVER AND G.P." AT 0232
                      WITH FOREGROUND-COLOR 15.
026030       DISPLAY (3, 20) "Busy Printing Turnover and GP Totals"
026040			      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
026050       DISPLAY "Are you sure Y/N?" AT 0620
026060                WITH FOREGROUND-COLOR 14.

026070 LA02.
026080	     ACCEPT WS-OPTION AT 0638
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
026150         GO TO LA02.
026160     IF WS-OPTION = "N"
026170         GO TO LA999.
             DISPLAY "'M'onth-to-date or 'Y'ear-to-date" AT 0820
                      WITH FOREGROUND-COLOR 14.
	     MOVE "M"		 TO WS-OPTION.

       LA02A.
026080	     ACCEPT WS-OPTION AT 0854
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "M")
026150         GO TO LA02A.
026180       DISPLAY (12, 20) "Ledger Key:".
026190	     MOVE ZERO		 TO WS-PAGE.
026200	     MOVE 1		 TO WS-S6.

026210 LA03.
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
026220	     MOVE ZERO		 TO W200-DT(WS-S6) W200-CR(WS-S6).
026230     IF WS-S6 < 300
026240	       ADD 1		 TO WS-S6
026250         GO TO LA03.
026260	     MOVE ZERO		 TO W200-DTOT W200-CTOT
026270				    W200-COST W200-SELL W200-NET.

026280 LA05.
026290       PERFORM AA50 THRU AA55.
           IF WS-OPTION = "M"
026300	       MOVE "TURNOVER & GP% (MTD)"
				 TO R-HEAD
           ELSE
026300	       MOVE "TURNOVER & GP% (YTD)"
				 TO R-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DETAIL.
026330	     MOVE "DEP" 	 TO RT-H18.
026340	     MOVE WS-H8 	 TO RT-H19.
026350	     MOVE "T-COST"	 TO RT-H20.
026360	     MOVE "T-SELL"	 TO RT-H21.
026370	     MOVE "MARKUP"	 TO RT-H22.
026380	     MOVE "G.P. %"	 TO RT-H23.
	     PERFORM CALL-PRINTUTL.
026400	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

026410 LA08.
020820	     MOVE " !"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
020860	       GO TO LA15.

020830 LA10.
020840	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
020860	       GO TO LA15.
	   IF (DPT-CODE = "YYYY") OR
	      (DPT-SCDE = "VAT")
	       GO TO LA10.
	     MOVE 1		 TO WS-S6.

       LA12.
	   IF W200-LEDG(WS-S6) = SPACES
	       MOVE DPT-CODE	 TO W200-LEDG(WS-S6)
	   ELSE
	   IF NOT (DPT-CODE = W200-LEDG(WS-S6))
	       ADD 1		 TO WS-S6
	       GO TO LA12.
	   IF WS-OPTION = "M"
020890	       MOVE DPT-SALES-MTD
				 TO W200-DT(WS-S6)
	       SUBTRACT DPT-RET-MTD
				 FROM W200-DT(WS-S6)
           ELSE
020900	       MOVE DPT-SALES-YTD
				 TO W200-DT(WS-S6)
	       SUBTRACT DPT-RET-YTD
				 FROM W200-DT(WS-S6).
021020       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF WS-OPTION = "M"
021030	       SUBTRACT DPT-DISC-MTD
				 FROM W200-DT(WS-S6)
	       MOVE DPT-COST-MTD TO W200-CR(WS-S6)
	       SUBTRACT DPT-CRET-MTD
				 FROM W200-CR(WS-S6)
           ELSE
021040	       SUBTRACT DPT-DISC-YTD
				 FROM W200-DT(WS-S6)
	       MOVE DPT-COST-YTD TO W200-CR(WS-S6)
	       SUBTRACT DPT-CRET-YTD
				 FROM W200-CR(WS-S6).
	     ADD W200-CR(WS-S6)  TO W200-COST.
	     ADD W200-DT(WS-S6)  TO W200-SELL.
	     GO TO LA10.

021080 LA15.
026670	     MOVE 1		 TO WS-S6.

026680 LA20.
	   IF W200-LEDG(WS-S6) = SPACES
	       GO TO LA25.
026690	   IF (W200-DT(WS-S6) = ZERO) AND (W200-CR(WS-S6) = ZERO)
	       ADD 1		 TO WS-S6
026700	       GO TO LA20.
026710	     MOVE W200-LEDG(WS-S6)
				 TO DPT-CODE.
026720       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
026730	     MOVE DPT-CODE	 TO RT-LEDG.
026740	     MOVE DPT-DESC	 TO RT-SDESC.
026750	     MOVE W200-CR(WS-S6)
				 TO RT-DMTD W200-CTOT.
026760	     MOVE W200-DT(WS-S6)
				 TO RT-DYTD W200-DTOT.
026770	     SUBTRACT W200-CTOT FROM W200-DTOT
				 GIVING W25-TOTAL.
026780	     COMPUTE RT-MUP = (W25-TOTAL / W200-CTOT) * 100.
026790	     COMPUTE RT-GP = (W25-TOTAL / W200-DTOT) * 100.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026810	     MOVE SPACES	 TO R-L1.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO LA999.
	     ADD 1		 TO WS-S6.
	     GO TO LA20.

026820 LA25.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026870	     MOVE "TOTAL"	 TO RT-SDESC.
026880	     MOVE W200-COST	 TO RT-DMTD.
026890	     MOVE W200-SELL	 TO RT-DYTD.
026900	     SUBTRACT W200-COST FROM W200-SELL
				 GIVING W25-TOTAL.
026910	     COMPUTE RT-MUP = (W25-TOTAL / W200-COST) * 100.
026920	     COMPUTE RT-GP = (W25-TOTAL / W200-SELL) * 100.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
026940	     MOVE SPACES	 TO R-L1.

026990 LA999.
027000       EXIT.
      /
      *    ****    V A T   R E P O R T   &   C L E A R
      * 
       MA00R           SECTION 60.
       MA00.
026010       DISPLAY CLR-SCREEN.
	     DISPLAY "VAT REPORT" AT 0235
                      WITH FOREGROUND-COLOR 15.
	     DISPLAY "D" AT 0420 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "etailed or "
		     "S" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ummary VAT report"
	     MOVE "D"		 TO WS-OPTION.
       MA00A.
	     ACCEPT WS-OPTION AT 0451
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "D" OR "S")
	       GO TO MA00A.
	     MOVE WS-OPTION	TO WS-VATTYPE.
026010	     DISPLAY CLR-SCREEN.
	   IF WS-VATTYPE = "S"
	       DISPLAY "SUMMARY VAT REPORT" AT 0231
		       WITH FOREGROUND-COLOR 15
026030	       DISPLAY "Busy printing VAT summary report" AT 0620
	       MOVE "P" 	 TO WS-STATOP
	       GO TO MA03
	   ELSE
	       DISPLAY "DETAILED VAT REPORT" AT 0231
		       WITH FOREGROUND-COLOR 15.
	     DISPLAY "P" AT 0420 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "rint or "
		     "D" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "elete VAT transactions"
	     MOVE "P"		 TO WS-OPTION.

       MA02.
	     ACCEPT WS-OPTION AT 0453
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "P" OR "D")
	       GO TO MA02.
	     MOVE WS-OPTION	TO WS-STATOP.
	   IF WS-STATOP = "D"
	       DISPLAY "Busy deleting VAT transactions" AT 0620
	   ELSE
026030	       DISPLAY "Busy printing VAT detailed report" AT 0620.

       MA03.
026050	     DISPLAY "Are you sure Y/N?" AT 0820
026060                WITH FOREGROUND-COLOR 14.

       MA05.

	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y"
026080	       ACCEPT WS-OPTION AT 0838
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	   IF WS-OPTION = "N"
               GO TO MA999.
	     DISPLAY "For the period." AT 1020.
	     DISPLAY "Start date :" AT 1120.
	     DISPLAY "End date   :" AT 1220.
	     DISPLAY "Dates to be keyed in as DDMMYYYY" AT 1420
                      WITH FOREGROUND-COLOR 11.
             MOVE ZERO           TO W15-START W15-END.
       MA06.
             ACCEPT S07.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
018530         GO TO MA06.
	     MOVE W15-START	 TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
               GO TO MA06.
             MOVE W20-DTE        TO W15-S-YMD.
             MOVE W15-END        TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
               GO TO MA06.
             MOVE W20-DTE        TO W15-E-YMD.
	     MOVE W15-START	 TO W24-DTE1.
	     MOVE W15-END	 TO W24-DTE2.
	     DISPLAY "A" AT 1420 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ll/" "D" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ebtors/" "C" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "reditors Transactions :".
	     MOVE "A"		 TO WS-TYPE.

       MA06A.
	     ACCEPT WS-TYPE AT 1457
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-TYPE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-TYPE = "A" OR "C" OR "D")
	       GO TO MA06A.
	   IF WS-PRINTER = "S"
	       PERFORM SAVE-SCREEN-3.
	     MOVE 1		 TO W05-V-RATE.
	   IF WS-STATOP = "D"
	       GO TO MA85.

       MA08.
	     OPEN I-O TXTRAN.
	   IF WS-STATUS NOT = "00"
               PERFORM OPEN-ERROR.
	     INITIALIZE W10-TAX-KEY.
	     MOVE W10-TAX-KEY	 TO TAX-KEY.
	     MOVE "1"		 TO TAX-TYPE.
	     MOVE "0"		 TO W10-TAX-TYPE.
	     PERFORM START-AT-TXTRAN-KEY THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32
               GO TO MA75.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY "Reading TAX record:" AT 1620
			WITH FOREGROUND-COLOR 7.
	     MOVE ZERO		 TO W15-TRAN-AMT-1
				    W15-TRAN-AMT-2
				    W15-TRAN-AMT-3
				    W15-TRAN-AMT-I
				    W15-TRAN-AMT-O
				    W15-NON-TXBL-1
                                    W15-NON-TXBL-2
                                    W15-NON-TXBL-3
                                    W15-NON-TXBL-I
				    W15-NON-TXBL-O.
	     MOVE 1		 TO WS-S7.

       MA09.
	     MOVE ZERO		 TO W15-TXBL-AMT-1(WS-S7)
				    W15-TXBL-AMT-2(WS-S7)
				    W15-TXBL-AMT-3(WS-S7)
				    W15-TXBL-AMT-I(WS-S7)
				    W15-TXBL-AMT-O(WS-S7)
				    W15-VAT-AMT-1(WS-S7)
				    W15-VAT-AMT-2(WS-S7)
				    W15-VAT-AMT-3(WS-S7)
				    W15-VAT-AMT-I(WS-S7)
				    W15-VAT-AMT-O(WS-S7).
	   IF WS-S7 < 12
	       ADD 1		 TO WS-S7
	       GO TO MA09.
             MOVE ZERO           TO WS-PAGE.
      *
      *    ****    S E T   P R I N T E R   T O   1 2 C P I
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     GO TO MA15.

       MA10.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
007080	     MOVE SPACES	 TO R-DETAIL.
007090       MOVE "PAGE:"        TO V-PGE.
007100       MOVE "DATE:"        TO V-DATE.
007110       ADD 1               TO WS-PAGE.
007120       MOVE WS-PAGE        TO V-PNO.
007130       MOVE W12-TODAY      TO V-DTE.
007140       MOVE W95-COMP       TO V-CPY.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.
             MOVE "* V A T   R E P O R T *" 
                                 TO V-HEAD.
	     PERFORM CALL-PRINTUTL.
	     MOVE W15-VAT-TYPE	 TO R-DETAIL.
	     MOVE W24-PERIOD	 TO V-CPY.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.
             MOVE "Trn-date"     TO VAT-H1.
             MOVE "Doc.numb"     TO VAT-H2.
             MOVE "Tran-type"    TO VAT-H3.
             MOVE "Account Name" TO VAT-H3A.
             MOVE "Tran-amount"  TO VAT-H4.
             MOVE "Non-taxable"  TO VAT-H5.
             MOVE "Taxable"      TO VAT-H6.
             MOVE "VAT-amount"   TO VAT-H7.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.

       MA15.
	     PERFORM READ-TXTRAN-NEXT THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32
               GO TO MA70.
	   IF NOT (WS-PRINTER = "S")
	       DISPLAY TAX-DATE AT 1640 WITH FOREGROUND-COLOR 14
		       " " TAX-REF WITH FOREGROUND-COLOR 15.
           IF (TAX-DATE < W15-S-YMD) OR 
              (TAX-DATE > W15-E-YMD)
               GO TO MA15.
	   IF NOT (WS-TYPE = "A")
	       IF WS-TYPE = "D"
		   IF NOT (TAX-ACTYPE = 1 OR 2)
		       GO TO MA15
		   END-IF
	       ELSE
		   IF NOT (TAX-ACTYPE = 3)
		       GO TO MA15.
	     MOVE 1		 TO WS-S7.
           IF TAX-TYPE NOT = W10-TAX-TYPE
               IF W10-TAX-TYPE = "0"
                   PERFORM MA40
                   PERFORM MA55
                   PERFORM MA10
               ELSE
		   PERFORM MA20 THRU MA24
		   PERFORM MA25 THRU MA28
		   PERFORM MA35 THRU MA38
                   IF TAX-TYPE > 2 AND W10-TAX-TYPE < 3
                       PERFORM MA45 THRU MA50
                   ELSE
                       PERFORM MA40
           ELSE
               IF TAX-ACTYPE NOT = W10-TAX-ACTYPE
		   PERFORM MA20 THRU MA24
		   PERFORM MA25 THRU MA28
		   PERFORM MA30 THRU MA34
               ELSE
                   IF TAX-CODE NOT = W10-TAX-CODE
		       PERFORM MA20 THRU MA24.
	     MOVE 1		 TO WS-S7.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P"		 TO WS-COMMAND
	       MOVE SPACES	 TO R-DETAIL
014990	       GO TO MA999.
	     MOVE TAX-KEY	 TO W10-TAX-KEY.
	     MOVE TAX-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	   IF WS-VATTYPE = "S"
	       GO TO MA16.
	     MOVE W22-DTE3	 TO VAT-DAT.
             MOVE TAX-REF        TO VAT-REF.
             MOVE W15-DESC       TO VAT-TRAN.
           IF TAX-ACTYPE = 1
               MOVE TAX-AC       TO DEB-ACNO
               PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
               MOVE DEB-NAME     TO VAT-NAME
               INSPECT VAT-NAME REPLACING ALL "*" BY " "
               INSPECT VAT-NAME REPLACING ALL "!" BY " "
           ELSE
           IF TAX-ACTYPE = 2
               MOVE "Cash Sales" TO VAT-NAME
           ELSE
           IF TAX-ACTYPE = 3
	       MOVE TAX-AC	 TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
               MOVE P-NAME       TO VAT-NAME
           ELSE
	   IF TAX-ACTYPE = 4
               MOVE "General Ledger"
				 TO VAT-NAME
	   ELSE
	       MOVE "Petty Cash" TO VAT-NAME.
             MOVE TAX-VALUE      TO VAT-TRAN-AMT.
             MOVE TAX-TAXFREE    TO VAT-NO-TAX.

       MA16.
	     ADD TAX-TAXFREE	 TO W15-NON-TXBL-1
                                    W15-NON-TXBL-2
                                    W15-NON-TXBL-3.
	     ADD TAX-VALUE	 TO W15-TRAN-AMT-1
                                    W15-TRAN-AMT-2
                                    W15-TRAN-AMT-3.
	   IF TAX-TYPE = "1" OR "3"
               ADD TAX-VALUE     TO W15-TRAN-AMT-I
               ADD TAX-TAXFREE   TO W15-NON-TXBL-I
	   ELSE
               ADD TAX-VALUE     TO W15-TRAN-AMT-O
	       ADD TAX-TAXFREE	 TO W15-NON-TXBL-O.
	   IF WS-VATTYPE = "D"
	       IF W02-LINAGE < W02-PRN-LENGTH
		   MOVE 1	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
009750		   MOVE SPACES	 TO R-DETAIL
009780	       ELSE
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
		   PERFORM MA10.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO MA999.

       MA17.
	   IF TAX-VAL(WS-S7) = ZERO
	       GO TO MA18.
	   IF WS-S7 < 7
	       MOVE WS-S7	 TO VAT-CDE
	   ELSE
	       SUBTRACT 6 FROM WS-S7
				 GIVING VAT-CDE.
	   IF WS-VATTYPE = "S"
	       GO TO MA17A.
	     MOVE W05-VAT(WS-S7) TO VAT-RTE.
	     MOVE "%"		 TO VAT-PERC.
	     MOVE TAX-VAT(WS-S7) TO VAT-VAT-AMT.
	     MOVE TAX-VAL(WS-S7) TO VAT-TXBL-AMT.

       MA17A.
	     ADD TAX-VAL(WS-S7)  TO W15-TXBL-AMT-1(WS-S7)
				    W15-TXBL-AMT-2(WS-S7)
				    W15-TXBL-AMT-3(WS-S7).
	     ADD TAX-VAT(WS-S7)	 TO W15-VAT-AMT-1(WS-S7)
				    W15-VAT-AMT-2(WS-S7)
				    W15-VAT-AMT-3(WS-S7).
           IF TAX-TYPE = "1" OR "3"
	       ADD TAX-VAL(WS-S7)
				 TO W15-TXBL-AMT-I(WS-S7)
	       ADD TAX-VAT(WS-S7)
				 TO W15-VAT-AMT-I(WS-S7)
           ELSE
	       ADD TAX-VAL(WS-S7)
				 TO W15-TXBL-AMT-O(WS-S7)
	       ADD TAX-VAT(WS-S7)
				 TO W15-VAT-AMT-O(WS-S7).
	   IF WS-VATTYPE = "D"
	       IF W02-LINAGE < W02-PRN-LENGTH
		   MOVE 1	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
009750		   MOVE SPACES	 TO R-DETAIL
009780	       ELSE
		   MOVE 99	 TO WS-ADVANCE
		   PERFORM CALL-PRINTUTL
		   PERFORM MA10.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO R-L1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO MA999.

       MA18.
	   IF WS-S7 < 12
	       ADD 1		 TO WS-S7
	       GO TO MA17.
             GO TO MA15.

       MA20.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             MOVE " Total -"     TO VAT-H2.
             MOVE W15-DESC       TO VAT-TRAN.
             MOVE W15-TRAN-AMT-1 TO VAT-TRAN-AMT.
	     MOVE W15-NON-TXBL-1 TO VAT-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*

       MA22.
	   IF W15-TXBL-AMT-1(WS-S7) = ZERO
	       GO TO MA24.
	     MOVE W15-TXBL-AMT-1(WS-S7)
				 TO VAT-TXBL-AMT.
	     MOVE W15-VAT-AMT-1(WS-S7)
				 TO VAT-VAT-AMT.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.

	MA24.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO MA22.
             MOVE ZERO           TO W15-TRAN-AMT-1
                                    W15-NON-TXBL-1
				    W15-TXBL-AMT-1(1)
				    W15-TXBL-AMT-1(7)
				    W15-VAT-AMT-1(1)
				    W15-VAT-AMT-1(7).
             PERFORM MA55.

       MA25.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
	     MOVE "Total for:"	 TO VAT-H1.
           IF W10-TAX-ACTYPE = 1
	       MOVE "Debtors"	 TO VAT-H2A
           ELSE
	   IF W10-TAX-ACTYPE = 2
	       MOVE "Cash"	 TO VAT-H2A
	   ELSE
	   IF W10-TAX-ACTYPE = 3
	       MOVE "Creditors"  TO VAT-H2A
	   ELSE
	   IF W10-TAX-ACTYPE = 4
	       MOVE "General ledger"
				 TO VAT-H2A
	   ELSE
	       MOVE "Petty Cash"
				 TO VAT-H2A.
	     MOVE W15-TRAN-AMT-2 TO VAT-TRAN-AMT.
	     MOVE W15-NON-TXBL-2 TO VAT-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
       MA26.
	   IF W15-TXBL-AMT-2(WS-S7) = ZERO
	       GO TO MA28.
	     MOVE W15-TXBL-AMT-2(WS-S7)
				 TO VAT-TXBL-AMT.
	     MOVE W15-VAT-AMT-2(WS-S7)
				 TO VAT-VAT-AMT.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.

	MA28.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO MA26.
	     MOVE ZERO		 TO W15-TRAN-AMT-2
                                    W15-NON-TXBL-2
				    W15-TXBL-AMT-2(1)
				    W15-TXBL-AMT-2(7)
				    W15-VAT-AMT-2(1)
				    W15-VAT-AMT-2(2).
       MA30.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             MOVE "Running Total -"
                                 TO VAT-L1.
             MOVE W15-TRAN-AMT-3 TO VAT-TRAN-AMT.
	     MOVE W15-NON-TXBL-3 TO VAT-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
       MA32.
	   IF W15-TXBL-AMT-3(WS-S7) = ZERO
	       GO TO MA34.
	     MOVE W15-TXBL-AMT-3(WS-S7)
				 TO VAT-TXBL-AMT.
	     MOVE W15-VAT-AMT-3(WS-S7)
				 TO VAT-VAT-AMT.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.

       MA34.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO MA32.

       MA35.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             MOVE "Totals - "    TO VAT-H1.
             MOVE W15-VAT-TYPE   TO VAT-H2A.
             MOVE W15-TRAN-AMT-3 TO VAT-TRAN-AMT.
             MOVE W15-NON-TXBL-3 TO VAT-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
       MA36.
	   IF W15-TXBL-AMT-3(WS-S7) = ZERO
	       GO TO MA38.
	     MOVE W15-TXBL-AMT-3(WS-S7)
				 TO VAT-TXBL-AMT.
	     MOVE W15-VAT-AMT-3(WS-S7)
				 TO VAT-VAT-AMT.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.

       MA38.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO MA36.
	     MOVE ZERO		 TO W15-TRAN-AMT-3
                                    W15-NON-TXBL-3
				    W15-TXBL-AMT-3(1)
				    W15-TXBL-AMT-3(7)
				    W15-VAT-AMT-3(1)
				    W15-VAT-AMT-3(7).
       MA40.
           IF TAX-TYPE = 1
               MOVE "Input VAT"  TO W15-VAT-TYPE
           ELSE
           IF TAX-TYPE = 2
               MOVE "Output VAT" TO W15-VAT-TYPE
           ELSE
           IF TAX-TYPE = 3
               MOVE "Input payments"
                                 TO W15-VAT-TYPE
           ELSE
           IF TAX-TYPE = 4
               MOVE "Output payments"
                                 TO W15-VAT-TYPE.
       MA45.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DETAIL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM MA10.
             MOVE "Net VAT amount"
				 TO VAT-L1.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
	     ADD W15-VAT-AMT-I(1) W15-VAT-AMT-O(1)
		 W15-VAT-AMT-I(7) W15-VAT-AMT-O(7)
                                 GIVING VAT-VAT-AMT.
       MA50.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
	     PERFORM MA40.
             PERFORM MA10.
             MOVE ZERO           TO W15-TRAN-AMT-I
                                    W15-NON-TXBL-I
				    W15-TXBL-AMT-I(1)
				    W15-TXBL-AMT-I(7)
				    W15-VAT-AMT-I(1)
				    W15-VAT-AMT-I(7).
             MOVE ZERO           TO W15-TRAN-AMT-O
                                    W15-NON-TXBL-O
				    W15-TXBL-AMT-O(1)
				    W15-TXBL-AMT-O(7)
				    W15-VAT-AMT-O(1)
				    W15-VAT-AMT-O(1).

       MA55.
           IF TAX-ACTYPE = 1
               PERFORM MA60
           ELSE
           IF TAX-ACTYPE = 2
               IF TAX-CODE = 01
                   MOVE "Cash sale"
                                 TO W15-DESC
               ELSE
                   MOVE "Cash refund"
                                 TO W15-DESC
           ELSE
           IF TAX-ACTYPE = 3
	       PERFORM MA65
           ELSE
           IF TAX-ACTYPE = 4
               IF TAX-CODE = 01
		   MOVE "Inv/B.Chrg"
				 TO W15-DESC
               ELSE
		   MOVE "D.N./Reversal"
				 TO W15-DESC
           ELSE
	   IF TAX-ACTYPE = 5
               IF TAX-CODE = 01
		   MOVE "Inv/P.Cash"
				 TO W15-DESC
               ELSE
		   MOVE "D.N./P.Cash"
				 TO W15-DESC.
       MA60.
             ADD 21 TAX-CODE     GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S1.
             ADD 1               TO WS-S1.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-E-DESC(WS-S1)
                                 TO W15-DESC.
       MA65.
             ADD 381 TAX-CODE    GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S1.
             ADD 1               TO WS-S1.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-C-DESC(WS-S1)
                                 TO W15-DESC.
       MA70.
	     PERFORM MA20 THRU MA24.
	     PERFORM MA25 THRU MA28.
	     PERFORM MA35 THRU MA38.
             PERFORM MA45.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DETAIL.
             MOVE "**** E N D   R E P O R T ****"
                                 TO V-CPY.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
      *
      *    ****    RESET THE PRINTER CONTROLS
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DETAIL.

       MA75.
	     CLOSE TXTRAN.
	   IF WS-PRINTER = "S"
	       PERFORM RESTORE-SCREEN-3.

       MA80.
	     MOVE "Must the file be cleared (Y/N)"
				 TO WS-ERR-MES.
	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO MA80.
           IF WS-OPTION = "N"
               GO TO MA999.
	     MOVE 1		 TO W05-V-RATE.
       MA85.
	     OPEN I-O TXTRAN.
	     INITIALIZE W10-TAX-KEY.
	     MOVE W10-TAX-KEY	 TO TAX-KEY.
	     MOVE "1"		 TO TAX-TYPE.
	     MOVE "0"		 TO W10-TAX-TYPE.
	     PERFORM START-AT-TXTRAN-KEY THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32 OR 35
	       GO TO MA95.
	     DISPLAY "Reading TAX record:" AT 1620
                      WITH FOREGROUND-COLOR 7.
       MA90.
	     PERFORM READ-TXTRAN-NEXT THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32 OR 35
	       GO TO MA95.
	     DISPLAY TAX-DATE AT 1640 WITH FOREGROUND-COLOR 14
                     " " TAX-REF WITH FOREGROUND-COLOR 15.
           IF (TAX-DATE < W15-S-YMD) OR 
              (TAX-DATE > W15-E-YMD)
	       GO TO MA90.
	   IF NOT (WS-TYPE = "A")
	       IF WS-TYPE = "D"
		   IF NOT (TAX-ACTYPE = 1 OR 2)
		       GO TO MA90
		   END-IF
	       ELSE
		   IF NOT (TAX-ACTYPE = 3)
		       GO TO MA90.
	     PERFORM DELETE-TXTRAN-REC THRU WRITE-TXTRAN-EXIT.
	     GO TO MA90.
       MA95.
	     CLOSE TXTRAN.

       MA999.
             EXIT.
      /
      *    ****    R E C A L C U L A T E   T A K I N G S
      *
       NA00T	       SECTION 61.
       NA00.
026010       DISPLAY CLR-SCREEN.
	     DISPLAY "CASH BALANCING REPORT" AT 0235
                      WITH FOREGROUND-COLOR 15.
	     PERFORM SAVE-SCREEN-3.

       NA05.
	     MOVE "Recalculate cash takings Y/N [ ]"
				  TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO NA05.
           IF WS-OPTION = "N"
	       GO TO NA999.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0626
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 6.
	     DISPLAY "³ " AT 0726 WITH FOREGROUND-COLOR 6 HIGHLIGHT
				       BACKGROUND-COLOR 6
		     "Recalculate takings for:  "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 6.
	     DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0826
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "´" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 6.
	     DISPLAY "³ " AT 0926
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "START REFERENCE           "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 6.
	     DISPLAY "³ " AT 1026
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "END REFERENCE             "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 6.
	     DISPLAY "À" AT 1126
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 6
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 6.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 27		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE ZERO		 TO W30-START-REF W30-END-REF.

       NA10.
	     ACCEPT S09S.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		   GO TO NA999.
	   IF W30-START-REF = ZERO
	       GO TO NA10.

       NA15.
	     ACCEPT S09E.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		   GO TO NA10.
	   IF W30-END-REF = ZERO
	       GO TO NA15.
      *----------------------------------------------------------------*
      *        Check that W30-END-REF not less than W30-START-REF.     *
      *----------------------------------------------------------------*
	   IF W30-END-REF < W30-START-REF
	       MOVE "End reference prior to Start reference"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO NA10.
	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY "Busy recalculating cash takings!" AT 0625
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
009460	     MOVE 1		 TO WS-AUDKEY.
	     MOVE ZERO		 TO W25-R-CASH	W25-R-CHQ
				    W25-R-CARD	W25-R-SPEED
				    W25-R-VOUCH W25-R-CRNOTE
				    W25-R-RECEIPTS.

       NA20.
	   IF NOT(WS-AUDKEY < W70-REP)
	       GO TO NA30.
	     ADD 1		 TO WS-AUDKEY.
009610	     PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
009620	   IF WS-F-ERROR NOT = ZERO
009640	       GO TO NA30.
	     DISPLAY AUD-TYPE AT 1212
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     AUD-SUB
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	   IF AUD-TYPE = 1
	       DISPLAY "Lost Sale       " AT 1220
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	   IF AUD-TYPE = 2
	       DISPLAY "Low Level       " AT 1220
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	   IF AUD-TYPE = 3
	       DISPLAY "Invoice/Cr Note " AT 1220
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	   IF AUD-TYPE = 4
	       DISPLAY "Cash Sale       " AT 1220
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	   IF AUD-TYPE = 5 OR 7 OR 9
	       DISPLAY "Salesman Details" AT 1220
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	   IF AUD-TYPE = 6
	       DISPLAY "Cash Drawer     " AT 1220
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	   IF AUD-TYPE = 8
	       DISPLAY "Stock Adjust    " AT 1220
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	   IF AUD-TYPE = 10
	       DISPLAY "Lay Buys        " AT 1220
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
	   ELSE
	   IF AUD-TYPE = 11
	       DISPLAY "Receipts        " AT 1220
			WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      *----------------------------------------------------------------*
      * 	     Check if this is a Cash Sale/Refund.	       *
      *----------------------------------------------------------------*
009650	   IF NOT(AUD-TYPE = 4)
009660	       GO TO NA25.
	   IF AUD-PAYMENT = "C"
	       COMPUTE W25-R-CASH = W25-R-CASH + AUD-VAL
	   ELSE
	   IF AUD-PAYMENT = "Q"
	       COMPUTE W25-R-CHQ = W25-R-CHQ + AUD-VAL
	   ELSE
	   IF AUD-PAYMENT = "D"
	       COMPUTE W25-R-CARD = W25-R-CARD + AUD-VAL
	   ELSE
	   IF AUD-PAYMENT = "S"
	       COMPUTE W25-R-SPEED = W25-R-SPEED + AUD-VAL
	   ELSE
	   IF AUD-PAYMENT = "V"
	       COMPUTE W25-R-VOUCH = W25-R-VOUCH + AUD-VAL
	   ELSE
	   IF AUD-PAYMENT = "N"
	       COMPUTE W25-R-CRNOTE = W25-R-CRNOTE + AUD-VAL.

       NA25.
      *----------------------------------------------------------------*
      *		      Check if this is a Debtor Receipt.	       *
      *----------------------------------------------------------------*
009650	   IF NOT(AUD-TYPE = 11)
009660	       GO TO NA20.
	     COMPUTE W25-R-RECEIPTS = W25-R-RECEIPTS + AUD-VAL.
	     COMPUTE W25-R-CASH = W25-R-CASH + AUD-R-CSH.
	     COMPUTE W25-R-CHQ = W25-R-CHQ + AUD-R-CHQ.
	     COMPUTE W25-R-TRFRS = W25-R-TRFRS + AUD-R-TRF.
	     GO TO NA20.

       NA30.

050120	     DISPLAY CLR-SCREEN.
050130	     DISPLAY "CASH TAKINGS - BALANCING REPORT" AT 0227
050140		      WITH FOREGROUND-COLOR 15.

050190 NA35.
050310	     MOVE SPACES	 TO R-DETAIL.
050320	     MOVE "Cash Takings - Balancing Report"
050330                           TO REP-COMPANY.
050340       MOVE "Date:"        TO REP-DATE.
050350	     MOVE W12-TODAY	 TO REP-DTE W30-DATE.
	     MOVE W30-HEADING	 TO REP-LINE3.

050360 NA40.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM NA35
	       GO TO NA40.
009810	     MOVE SPACES	 TO R-DETAIL.

       NA45.
050490	     MOVE "Cash Takings" TO REP-REMK.
050500	     MOVE W25-R-CASH	 TO REP-VAL.
050510	     PERFORM NA40.
050560	     MOVE "Cheques"	 TO REP-REMK.
050570	     MOVE W25-R-CHQ	 TO REP-VAL.
050580	     PERFORM NA40.
050590	     MOVE "Credit Cards" TO REP-REMK.
050600	     MOVE W25-R-CARD	 TO REP-VAL.
050610	     PERFORM NA40.
050590	     MOVE "Speed Point"  TO REP-REMK.
050600	     MOVE W25-R-SPEED	 TO REP-VAL.
050610	     PERFORM NA40.
050590	     MOVE "Vouch/CNotes" TO REP-REMK.
050600	     MOVE W25-R-VOUCH	 TO REP-VAL.
050610	     PERFORM NA40.
	     MOVE "EF Transfers" TO REP-REMK.
      *      MOVE "Credit Notes" TO REP-REMK.
050600	     MOVE W25-R-TRFRS	 TO REP-VAL.
050610	     PERFORM NA40.
050620	   IF W25-R-RECEIPTS NOT = ZERO
050630         MOVE "Receipts"   TO REP-REMK
050640	       MOVE W25-R-RECEIPTS
				 TO REP-VAL
050650	       PERFORM NA40.
	     MOVE "To be Banked" TO REP-REMK.
050720	     COMPUTE REP-VAL = (W25-R-CASH + W25-R-CHQ +
			       W25-R-CARD).
050730	     PERFORM NA40.
050700	     MOVE "Sales - Today"
050710                           TO REP-REMK.
050720	     COMPUTE REP-VAL = (W25-R-CASH + W25-R-CHQ +
			       W25-R-CARD + W25-R-SPEED +
			       W25-R-VOUCH + W25-R-CRNOTE +
			       W25-R-TRFRS) - W25-R-RECEIPTS
050730	     PERFORM NA40.
050840	     MOVE "Any key to continue"
				 TO WS-ERR-MES.
	     MOVE "1964"	 TO SCREEN-POS.
	     PERFORM OPT-SETUP THRU OPT-EXIT.

       NA999.
	     EXIT.
      /----------------------------------------------------------------*
      * 	    ****    ORDERS AND QUOTATIONS    ****	       *
      *----------------------------------------------------------------*
021400 OA00R		      SECTION 62.
021410 OA00.
021420       DISPLAY CLR-SCREEN.
	     MOVE WS-OPTION	 TO WS-STATOP.
	     DISPLAY "ORDERS AND QUOTATIONS" AT 0229
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 22		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0429
		      WITH BACKGROUND-COLOR 2
			   FOREGROUND-COLOR 2 HIGHLIGHT
		     "¿" AT 0452 WITH BACKGROUND-COLOR 2
				      FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0529
		      WITH BACKGROUND-COLOR 2
			   FOREGROUND-COLOR 2 HIGHLIGHT
		     "1" WITH BACKGROUND-COLOR 2
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Quotations         ³"
		      WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0629
		      WITH BACKGROUND-COLOR 2
			   FOREGROUND-COLOR 2 HIGHLIGHT
		     "2" WITH BACKGROUND-COLOR 2
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Sales Orders       ³"
		      WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.
	     DISPLAY "³ " AT 0729
		      WITH BACKGROUND-COLOR 2
			   FOREGROUND-COLOR 2 HIGHLIGHT
		     "3" WITH BACKGROUND-COLOR 2
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Converted Orders   ³"
		      WITH BACKGROUND-COLOR 2 FOREGROUND-COLOR 0.

	     DISPLAY "³ " AT 0829
		      WITH BACKGROUND-COLOR 2
			   FOREGROUND-COLOR 2 HIGHLIGHT
		     "0" WITH BACKGROUND-COLOR 2
			      FOREGROUND-COLOR 7 HIGHLIGHT
		     " Period-End Control ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY "À" AT 0929
		      WITH BACKGROUND-COLOR 2
			   FOREGROUND-COLOR 2 HIGHLIGHT
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
	     PERFORM SAVE-SCREEN-3.

021480 OA00A.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE 4		 TO TOP-ROW.
	     MOVE 7		 TO BOTTOM-ROW.
	     MOVE 3		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 20		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       OA00A-5.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 OA00A-10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO OA00A-15
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO OA00A-10
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO OA00A-10
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO OA00A-10
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO OA00A-10
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO OA00A-15
		 WHEN OTHER	 CALL X"E5" CALL X"E5" CALL X"E5"
	       END-EVALUATE
	       GO TO OA00A-5
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO OA00AF
		 WHEN OTHER	 CALL X"E5" CALL X"E5" CALL X"E5"
	       END-EVALUATE
	       GO TO OA00A-5
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < "0" OR > "3"
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO OA00A-5
	       END-IF
	       GO TO OA00A-16
	   ELSE
	       CALL X"E5" CALL X"E5" CALL X"E5"
	       GO TO OA00A-5.

       OA00A-15.
	     EVALUATE SCREEN-LIN
	       WHEN 4
		   MOVE "1"	 TO WS-OPTION
	       WHEN 5
		   MOVE "2"	 TO WS-OPTION
	       WHEN 6
		   MOVE "3"	 TO WS-OPTION
	       WHEN 7
		   MOVE "0"	 TO WS-OPTION
	       WHEN OTHER
		   CALL X"E5" CALL X"E5" CALL X"E5"
		   GO TO OA00A-5
	     END-EVALUATE.
	     GO TO OA00AF.

       OA00A-16.
	     PERFORM CLEAR-BLOCK.
	     EVALUATE WS-OPTION
	       WHEN "0"
		   MOVE 7	 TO SCREEN-LIN
	       WHEN "1"
		   MOVE 4	 TO SCREEN-LIN
	       WHEN "2"
		   MOVE 5	 TO SCREEN-LIN
	       WHEN "3"
		   MOVE 6	 TO SCREEN-LIN
	     END-EVALUATE.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       OA00AF.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
	     MOVE WS-OPTION	 TO WS-REPLY.
	     MOVE WS-STATOP	 TO WS-OPTION.
021550	   IF WS-REPLY = "0"
021560	       GO TO OA999.
	   IF WS-REPLY = "1"
	       GO TO OA40.
	   IF WS-REPLY = "2"
	       GO TO OA65.
	   IF WS-REPLY = "3"
	       GO TO OA75.
	     GO TO OA00A.
      *
      *    ****    R E D U N D A N T   Q U O T E S
      *
       OA40.
026010       DISPLAY CLR-SCREEN.
             DISPLAY "CLEAR REDUNDANT QUOTATIONS" AT 0227
                      WITH FOREGROUND-COLOR 15.
026030       DISPLAY "This routine will clear redundant quotations" 
                      AT 0408.
026050       DISPLAY "Are you sure Y/N?" AT 0608
026060                WITH FOREGROUND-COLOR 14.

       OA45.
026080	     ACCEPT WS-OPTION AT 0626
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO OA45.
           IF WS-OPTION = "N"
	       GO TO OA00.
             MOVE "Q"            TO WS-TYPE.
035570       DISPLAY "Enter Date from which QUOTATIONS are to be kept DD
035580- 	     "/MM/YYYY" AT 0808.
             MOVE ZERO           TO W15-END.

       OA50.
             DISPLAY "Order No:" AT 1008.
             ACCEPT S08.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
018530	       GO TO OA50.
             MOVE W15-END        TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Invalid date"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO OA50.
             MOVE W20-DTE        TO W15-E-YMD.
000740       MOVE "00000001"     TO SOR-REF
             MOVE ZERO           TO SOR-SEQ
000750       PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
000880     IF WS-F-ERROR = 20
	       GO TO OA00.
       OA55.
000920       PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
000930     IF WS-F-ERROR = 20
000940	       GO TO OA00.
       OA58.
           IF SOR-TYPE NOT = ZERO
	       GO TO OA55.
             DISPLAY SOR-REF AT 1018 WITH FOREGROUND-COLOR 15.
           IF SOR-DOC = WS-TYPE
               IF SOR-DTE < W15-E-YMD
                   MOVE SOR-REF  TO WS-REF
		   GO TO OA60.
	     GO TO OA55.

       OA60.
             PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
000920       PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
000930     IF WS-F-ERROR = 20
000940	       GO TO OA00.
           IF SOR-REF NOT = WS-REF
	       GO TO OA58.
	     GO TO OA60.
      *
      *    ****    R E D U N D A N T   O R D E R S
      *
       OA65.
026010       DISPLAY CLR-SCREEN.
             DISPLAY "CLEAR REDUNDANT SALES ORDERS" AT 0226
                      WITH FOREGROUND-COLOR 15.
026030       DISPLAY "This routine will clear redundant sales orders" 
                      AT 0408.
026050       DISPLAY "Are you sure Y/N?" AT 0608
026060                WITH FOREGROUND-COLOR 14.

       OA70.
026080	     ACCEPT WS-OPTION AT 0626
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO OA70.
           IF WS-OPTION = "N"
	       GO TO OA00.
             MOVE "O"            TO WS-TYPE.
035570       DISPLAY "Enter Date from which SLS-ORDERS are to be kept DD
035580- 	     "/MM/YYYY" AT 0808.
             MOVE ZERO           TO W15-END.
	     GO TO OA50.
      *
      *    ****    C O N V E R T E D   O R D E R S
      *
       OA75.
026010       DISPLAY CLR-SCREEN.
             DISPLAY "CLEAR CONVERTED ORDERS" AT 0229
                      WITH FOREGROUND-COLOR 15.
026030       DISPLAY "This routine will clear converted orders" 
                      AT 0408.
026050       DISPLAY "Are you sure Y/N?" AT 0608
026060                WITH FOREGROUND-COLOR 14.

       OA80.
026080	     ACCEPT WS-OPTION AT 0626
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
026140	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO OA80.
           IF WS-OPTION = "N"
	       GO TO OA00.
             MOVE "I"            TO WS-TYPE.
035570       DISPLAY "Enter Date from which CNV-ORDERS are to be kept DD
035580- 	     "/MM/YYYY" AT 0808.
             MOVE ZERO           TO W15-END.
	     GO TO OA50.

023200 OA999.
023210       EXIT.
      /
      *    ****    V A T   E X P O R T
      * 
       PA00R	       SECTION 63.
       PA00.
026010       DISPLAY CLR-SCREEN.
	     DISPLAY "VAT REPORT - EXPORT - EXCEL" AT 0226
                      WITH FOREGROUND-COLOR 15.
	     MOVE "D"		 TO WS-VATTYPE.
026030	     DISPLAY "Busy Exporting VAT to EXCEL" AT 0620
	     MOVE "P"		 TO WS-STATOP.
026050	     DISPLAY "Are you sure Y/N?" AT 0820
026060                WITH FOREGROUND-COLOR 14.
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y"
026080	       ACCEPT WS-OPTION AT 0838
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	   IF WS-OPTION = "N"
	       GO TO PA999.
	     DISPLAY "For the period." AT 1020.
	     DISPLAY "Start date :" AT 1120.
	     DISPLAY "End date   :" AT 1220.
	     DISPLAY "Dates to be keyed in as DDMMYYYY" AT 1420
                      WITH FOREGROUND-COLOR 11.
	     MOVE ZERO		 TO W15-START W15-END.

       PA05.
             ACCEPT S07.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
018530	       GO TO PA05.
	     MOVE W15-START	 TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO PA05.
             MOVE W20-DTE        TO W15-S-YMD.
             MOVE W15-END        TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO PA05.
             MOVE W20-DTE        TO W15-E-YMD.
	     MOVE W15-START	 TO W24-DTE1.
	     MOVE W15-END	 TO W24-DTE2.

      *      DISPLAY "A" AT 1420 WITH FOREGROUND-COLOR 6 HIGHLIGHT
      *		     "ll/" "D" WITH FOREGROUND-COLOR 6 HIGHLIGHT
      *		     "ebtors/" "C" WITH FOREGROUND-COLOR 6 HIGHLIGHT
      *		     "reditors Transactions :".
	     MOVE "D"		 TO WS-TYPE.
	     DISPLAY "File to be Created:" AT 1420.

       PA06A.
	     ACCEPT W02-VATNAME AT 1440
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     MOVE 12		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W02-VATNAME
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     OPEN OUTPUT VATREP.

      *	     ACCEPT WS-TYPE AT 1457
      *		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
      *			 BACKGROUND-COLOR 5 UPDATE AUTO.
      *	     CALL "CBL_TOUPPER" USING WS-TYPE
      *				BY VALUE WS-LENGTH
      *				RETURNING WS-STATUS.
      *	   IF NOT (WS-TYPE = "A" OR "C" OR "D")
      *	       GO TO PA06A.
	     MOVE 1		 TO W05-V-RATE.

       PA08.
	     OPEN I-O TXTRAN.
	   IF WS-STATUS NOT = "00"
               PERFORM OPEN-ERROR.
	     INITIALIZE W10-TAX-KEY.
	     MOVE W10-TAX-KEY	 TO TAX-KEY.
	     MOVE "1"		 TO TAX-TYPE.
	     MOVE "0"		 TO W10-TAX-TYPE.
	     PERFORM START-AT-TXTRAN-KEY THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32
	       GO TO PA75.
	     DISPLAY "Reading TAX record:" AT 1620
		      WITH FOREGROUND-COLOR 7.
	     MOVE ZERO		 TO W15-TRAN-AMT-1
				    W15-TRAN-AMT-2
				    W15-TRAN-AMT-3
				    W15-TRAN-AMT-I
				    W15-TRAN-AMT-O
				    W15-NON-TXBL-1
                                    W15-NON-TXBL-2
                                    W15-NON-TXBL-3
                                    W15-NON-TXBL-I
				    W15-NON-TXBL-O.
	     MOVE 1		 TO WS-S7.

       PA09.
	     MOVE ZERO		 TO W15-TXBL-AMT-1(WS-S7)
				    W15-TXBL-AMT-2(WS-S7)
				    W15-TXBL-AMT-3(WS-S7)
				    W15-TXBL-AMT-I(WS-S7)
				    W15-TXBL-AMT-O(WS-S7)
				    W15-VAT-AMT-1(WS-S7)
				    W15-VAT-AMT-2(WS-S7)
				    W15-VAT-AMT-3(WS-S7)
				    W15-VAT-AMT-I(WS-S7)
				    W15-VAT-AMT-O(WS-S7).
	   IF WS-S7 < 12
	       ADD 1		 TO WS-S7
	       GO TO PA09.

       PA10.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
007140	     MOVE W95-COMP	 TO EXP-DATA.
	     WRITE VAT-REC1.
	     MOVE "* V A T   R E P O R T *"
				 TO EXP-DATA.
	     WRITE VAT-REC1.
	     MOVE W15-VAT-TYPE	 TO R-DETAIL.
	     MOVE W24-PERIOD	 TO V-CPY.
	     MOVE R-DETAIL	 TO EXP-DATA.
	     WRITE VAT-REC1.
007160	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "Trn-date"	 TO VTD-H1.
	     MOVE "Doc.numb"	 TO VTD-H2.
	     MOVE "Tran-type"	 TO VTD-H3.
	     MOVE "Account Name" TO VTD-H3A.
	     MOVE "Tran-amount"  TO VTD-H4.
	     MOVE "Non-taxable"  TO VTD-H5.
	     MOVE "Taxable"	 TO VTD-H6.
	     MOVE "VAT-amount"	 TO VTD-H7.
	     MOVE VTD-RECORD2	 TO EXP-DATA.
	     WRITE VAT-REC1.

       PA15.
	     PERFORM READ-TXTRAN-NEXT THRU READ-TXTRAN-EXIT.
	   IF WS-F-ERROR = 32
	       GO TO PA70.
	     DISPLAY TAX-DATE AT 1640 WITH FOREGROUND-COLOR 14
		     " " TAX-REF WITH FOREGROUND-COLOR 15.
           IF (TAX-DATE < W15-S-YMD) OR 
              (TAX-DATE > W15-E-YMD)
	       GO TO PA15.
	   IF NOT (WS-TYPE = "A")
	       IF WS-TYPE = "D"
		   IF NOT (TAX-ACTYPE = 1 OR 2)
		       GO TO PA15
		   END-IF
	       ELSE
		   IF NOT (TAX-ACTYPE = 3)
		       GO TO PA15.
	     MOVE 1		 TO WS-S7.
           IF TAX-TYPE NOT = W10-TAX-TYPE
               IF W10-TAX-TYPE = "0"
		   PERFORM PA40
		   PERFORM PA55
      *		   PERFORM PA10
               ELSE
		   PERFORM PA20 THRU PA24
		   PERFORM PA25 THRU PA28
		   PERFORM PA35 THRU PA38
                   IF TAX-TYPE > 2 AND W10-TAX-TYPE < 3
		       PERFORM PA45 THRU PA50
                   ELSE
		       PERFORM PA40
           ELSE
               IF TAX-ACTYPE NOT = W10-TAX-ACTYPE
		   PERFORM PA20 THRU PA24
		   PERFORM PA25 THRU PA28
		   PERFORM PA30 THRU PA34
               ELSE
                   IF TAX-CODE NOT = W10-TAX-CODE
		       PERFORM PA20 THRU PA24.
	     MOVE 1		 TO WS-S7.
             PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014990	       GO TO PA75.
	     MOVE TAX-KEY	 TO W10-TAX-KEY.
	     MOVE TAX-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	   IF WS-VATTYPE = "S"
	       GO TO PA16.
	     MOVE W22-DTE3	 TO VTD-DAT.
	     MOVE TAX-REF	 TO VTD-REF.
	     MOVE W15-DESC	 TO VTD-TRAN.
           IF TAX-ACTYPE = 1
               MOVE TAX-AC       TO DEB-ACNO
               PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT
	       MOVE DEB-NAME	 TO VTD-NAME
	       INSPECT VTD-NAME REPLACING ALL "*" BY " "
	       INSPECT VTD-NAME REPLACING ALL "!" BY " "
           ELSE
           IF TAX-ACTYPE = 2
	       MOVE "Cash Sales" TO VTD-NAME
           ELSE
           IF TAX-ACTYPE = 3
	       MOVE TAX-AC	 TO P-NUMBER
               PERFORM READ-CREDIT THRU READ-CREDIT-EXIT
	       MOVE P-NAME	 TO VTD-NAME
           ELSE
	   IF TAX-ACTYPE = 4
               MOVE "General Ledger"
				 TO VTD-NAME
	   ELSE
	       MOVE "Petty Cash" TO VTD-NAME.
	     MOVE TAX-VALUE	 TO VTD-TRAN-AMT.
	     MOVE TAX-TAXFREE	 TO VTD-NO-TAX.
	     MOVE TAX-VAT(WS-S7) TO VTD-VAT-AMT.
	     MOVE TAX-VAL(WS-S7) TO VTD-TXBL-AMT.

       PA16.
	     ADD TAX-TAXFREE	 TO W15-NON-TXBL-1
                                    W15-NON-TXBL-2
                                    W15-NON-TXBL-3.
	     ADD TAX-VALUE	 TO W15-TRAN-AMT-1
                                    W15-TRAN-AMT-2
                                    W15-TRAN-AMT-3.
	   IF TAX-TYPE = "1" OR "3"
               ADD TAX-VALUE     TO W15-TRAN-AMT-I
               ADD TAX-TAXFREE   TO W15-NON-TXBL-I
	   ELSE
               ADD TAX-VALUE     TO W15-TRAN-AMT-O
	       ADD TAX-TAXFREE	 TO W15-NON-TXBL-O.
	   IF WS-VATTYPE = "D"
	       PERFORM PA80.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       MOVE SPACES	 TO R-DETAIL
	       MOVE "P"		 TO WS-COMMAND
014990	       GO TO PA75.

       PA17.
	   IF TAX-VAL(WS-S7) = ZERO
	       GO TO PA18.

       PA17A.
	     ADD TAX-VAL(WS-S7)  TO W15-TXBL-AMT-1(WS-S7)
				    W15-TXBL-AMT-2(WS-S7)
				    W15-TXBL-AMT-3(WS-S7).
	     ADD TAX-VAT(WS-S7)	 TO W15-VAT-AMT-1(WS-S7)
				    W15-VAT-AMT-2(WS-S7)
				    W15-VAT-AMT-3(WS-S7).
           IF TAX-TYPE = "1" OR "3"
	       ADD TAX-VAL(WS-S7)
				 TO W15-TXBL-AMT-I(WS-S7)
	       ADD TAX-VAT(WS-S7)
				 TO W15-VAT-AMT-I(WS-S7)
           ELSE
	       ADD TAX-VAL(WS-S7)
				 TO W15-TXBL-AMT-O(WS-S7)
	       ADD TAX-VAT(WS-S7)
				 TO W15-VAT-AMT-O(WS-S7).

       PA18.
	   IF WS-S7 < 12
	       ADD 1		 TO WS-S7
	       GO TO PA17.
	     GO TO PA15.

       PA20.
      *	   IF W02-LINAGE < W02-PRN-LENGTH
      *	       PERFORM PA80.
	     MOVE SPACES	 TO VTD-DATE VTD-REFER.
	     MOVE "Total -"	 TO VTD-TRAN.
	     MOVE W15-DESC	 TO VTD-NAME.
	     MOVE W15-TRAN-AMT-1 TO VTD-TRAN-AMT.
	     MOVE W15-NON-TXBL-1 TO VTD-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*

       PA22.
	   IF W15-TXBL-AMT-1(WS-S7) = ZERO
	       GO TO PA24.
	     MOVE W15-TXBL-AMT-1(WS-S7)
				 TO VTD-TXBL-AMT.
	     MOVE W15-VAT-AMT-1(WS-S7)
				 TO VTD-VAT-AMT.
      *   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       PERFORM PA80.

	PA24.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO PA22.
             MOVE ZERO           TO W15-TRAN-AMT-1
                                    W15-NON-TXBL-1
				    W15-TXBL-AMT-1(1)
				    W15-TXBL-AMT-1(7)
				    W15-VAT-AMT-1(1)
				    W15-VAT-AMT-1(7).
	     PERFORM PA55.

       PA25.
      *   IF W02-LINAGE < W02-PRN-LENGTH
      *	       PERFORM PA80.
	     MOVE SPACES	 TO VTD-DATE VTD-REFER.
	     MOVE "Total for:"	 TO VTD-TRAN.
           IF W10-TAX-ACTYPE = 1
	       MOVE "Debtors"	 TO VTD-NAME
           ELSE
	   IF W10-TAX-ACTYPE = 2
	       MOVE "Cash"	 TO VTD-NAME
	   ELSE
	   IF W10-TAX-ACTYPE = 3
	       MOVE "Creditors"  TO VTD-NAME
	   ELSE
	   IF W10-TAX-ACTYPE = 4
	       MOVE "General ledger"
				 TO VTD-NAME
	   ELSE
	       MOVE "Petty Cash"
				 TO VTD-NAME.
	     MOVE W15-TRAN-AMT-2 TO VTD-TRAN-AMT.
	     MOVE W15-NON-TXBL-2 TO VTD-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
       PA26.
	   IF W15-TXBL-AMT-2(WS-S7) = ZERO
	       GO TO PA28.
	     MOVE W15-TXBL-AMT-2(WS-S7)
				 TO VTD-TXBL-AMT.
	     MOVE W15-VAT-AMT-2(WS-S7)
				 TO VTD-VAT-AMT.
      *   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       PERFORM PA80.

	PA28.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO PA26.
	     MOVE ZERO		 TO W15-TRAN-AMT-2
                                    W15-NON-TXBL-2
				    W15-TXBL-AMT-2(1)
				    W15-TXBL-AMT-2(7)
				    W15-VAT-AMT-2(1)
				    W15-VAT-AMT-2(2).
       PA30.
      *   IF W02-LINAGE < W02-PRN-LENGTH
      *	       PERFORM PA80.
	     MOVE SPACES	 TO VTD-DATE VTD-REFER VTD-TRAN.
             MOVE "Running Total -"
				 TO VTD-NAME.
	     MOVE W15-TRAN-AMT-3 TO VTD-TRAN-AMT.
	     MOVE W15-NON-TXBL-3 TO VTD-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
       PA32.
	   IF W15-TXBL-AMT-3(WS-S7) = ZERO
	       GO TO PA34.
	     MOVE W15-TXBL-AMT-3(WS-S7)
				 TO VTD-TXBL-AMT.
	     MOVE W15-VAT-AMT-3(WS-S7)
				 TO VTD-VAT-AMT.
      *   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       PERFORM PA80.

       PA34.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO PA32.

       PA35.
      *   IF W02-LINAGE < W02-PRN-LENGTH
      *	       PERFORM PA80.
	     MOVE SPACES	 TO VTD-DATE VTD-REFER.
	     MOVE "Totals -"	 TO VTD-TRAN.
	     MOVE W15-VAT-TYPE	 TO VTD-NAME.
	     MOVE W15-TRAN-AMT-3 TO VTD-TRAN-AMT.
	     MOVE W15-NON-TXBL-3 TO VTD-NO-TAX.
	     MOVE 1		 TO WS-S7.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
       PA36.
	   IF W15-TXBL-AMT-3(WS-S7) = ZERO
	       GO TO PA38.
	     MOVE W15-TXBL-AMT-3(WS-S7)
				 TO VTD-TXBL-AMT.
	     MOVE W15-VAT-AMT-3(WS-S7)
				 TO VTD-VAT-AMT.
      *   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       PERFORM PA80.

       PA38.
	   IF WS-S7 < 7
	       ADD 1		 TO WS-S7
	       GO TO PA36.
	     MOVE ZERO		 TO W15-TRAN-AMT-3
                                    W15-NON-TXBL-3
				    W15-TXBL-AMT-3(1)
				    W15-TXBL-AMT-3(7)
				    W15-VAT-AMT-3(1)
				    W15-VAT-AMT-3(7).
       PA40.
           IF TAX-TYPE = 1
               MOVE "Input VAT"  TO W15-VAT-TYPE
           ELSE
           IF TAX-TYPE = 2
               MOVE "Output VAT" TO W15-VAT-TYPE
           ELSE
           IF TAX-TYPE = 3
               MOVE "Input payments"
                                 TO W15-VAT-TYPE
           ELSE
           IF TAX-TYPE = 4
               MOVE "Output payments"
                                 TO W15-VAT-TYPE.
       PA45.
      *   IF W02-LINAGE < W02-PRN-LENGTH
      *	       PERFORM PA80.
	     MOVE SPACES	 TO VTD-DATE VTD-REFER VTD-TRAN.
	     MOVE ZERO		 TO VTD-TRAN-AMT VTD-NO-TAX
				    VTD-TXBL-AMT.
             MOVE "Net VAT amount"
				 TO VTD-NAME.
      *----------------------------------------------------------------*
      * 							       *
      * This routine to be amended when multiple rates are introduced. *
      * 							       *
      *----------------------------------------------------------------*
	     ADD W15-VAT-AMT-I(1) W15-VAT-AMT-O(1)
		 W15-VAT-AMT-I(7) W15-VAT-AMT-O(7)
				 GIVING VTD-VAT-AMT.
       PA50.
	     PERFORM PA80.
	     PERFORM PA40.
      *	     PERFORM PA10.
             MOVE ZERO           TO W15-TRAN-AMT-I
                                    W15-NON-TXBL-I
				    W15-TXBL-AMT-I(1)
				    W15-TXBL-AMT-I(7)
				    W15-VAT-AMT-I(1)
				    W15-VAT-AMT-I(7).
             MOVE ZERO           TO W15-TRAN-AMT-O
                                    W15-NON-TXBL-O
				    W15-TXBL-AMT-O(1)
				    W15-TXBL-AMT-O(7)
				    W15-VAT-AMT-O(1)
				    W15-VAT-AMT-O(1).

       PA55.
           IF TAX-ACTYPE = 1
	       PERFORM PA60
           ELSE
           IF TAX-ACTYPE = 2
               IF TAX-CODE = 01
                   MOVE "Cash sale"
                                 TO W15-DESC
               ELSE
                   MOVE "Cash refund"
                                 TO W15-DESC
           ELSE
           IF TAX-ACTYPE = 3
	       PERFORM PA65
           ELSE
           IF TAX-ACTYPE = 4
               IF TAX-CODE = 01
		   MOVE "Inv/B.Chrg"
				 TO W15-DESC
               ELSE
		   MOVE "D.N./Reversal"
				 TO W15-DESC
           ELSE
	   IF TAX-ACTYPE = 5
               IF TAX-CODE = 01
		   MOVE "Inv/P.Cash"
				 TO W15-DESC
               ELSE
		   MOVE "D.N./P.Cash"
				 TO W15-DESC.
       PA60.
             ADD 21 TAX-CODE     GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S1.
             ADD 1               TO WS-S1.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-E-DESC(WS-S1)
                                 TO W15-DESC.
       PA65.
             ADD 381 TAX-CODE    GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S1.
             ADD 1               TO WS-S1.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-C-DESC(WS-S1)
                                 TO W15-DESC.
       PA70.
	     PERFORM PA20 THRU PA24.
	     PERFORM PA25 THRU PA28.
	     PERFORM PA35 THRU PA38.
	     PERFORM PA45.
	     PERFORM PA80.
	     MOVE SPACES	 TO R-DETAIL.
	     MOVE SPACES	 TO VTD-DATE VTD-REFER VTD-TRAN.
             MOVE "**** E N D   R E P O R T ****"
				 TO VTD-NAME.
	     MOVE ZERO		 TO VTD-TRAN-AMT
				    VTD-NO-TAX
				    VTD-TXBL-AMT
				    VTD-VAT-AMT.
	     PERFORM PA80.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DETAIL.

       PA75.
	     CLOSE TXTRAN
		   VATREP.
	     GO TO PA999.

       PA80.
	     MOVE VTD-RECORD	 TO EXP-DATA.
	     WRITE VAT-REC1.

       PA999.
             EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
029020 ZA00-INIT	      SECTION 89.
       ZA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA02.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO ZA02.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(16) WS-TCHR(41)
021940				    WS-TCHR(64) WS-TCHR(71).
021950	     MOVE WS-G3 	 TO WS-MCHR(16) WS-MCHR(41)
021960				    WS-MCHR(64) WS-MCHR(71)
				    WS-MCHR(1)	WS-MCHR(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(16) WS-BCHR(41)
021980				    WS-BCHR(64) WS-BCHR(71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.
029030 ZA00-OPEN.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
029040	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    **** 	D A I L Y   -	A U D I T   F I L E	    ****
      *    *************************************************************
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
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
      *    **** 	     P O S   S L I P S	 F I L E	    ****
      *    *************************************************************
	     MOVE "PSLIPS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SLIP.
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    ****      V A T   T R A N S A C T I O N S   F I L E	    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
029050	     OPEN I-O PARAM LOCK.
029060     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
029080     IF WS-STATUS NOT = "00"
029090	       MOVE 15		 TO WS-F-ERROR
029100         PERFORM OPEN-ERROR.
	     OPEN I-O NETWORK.
029250     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
029330     IF WS-STATUS NOT = "00"
029340	       MOVE 2		 TO WS-F-ERROR
029350         PERFORM OPEN-ERROR.
	     MOVE 1		 TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
029550	     SUBTRACT 1 FROM DEB-AUDIT
				 GIVING W70-REP.
	     DISPLAY CLEAR-L25.
029560	     OPEN I-O AUDIT.
029570     IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
029590     IF WS-STATUS NOT = "00"
029600	       MOVE 1		 TO WS-F-ERROR
029610         PERFORM OPEN-ERROR.
	     MOVE ZERO		 TO WS-AUDKEY
				    WS-AUD.
	     DISPLAY "Checking AUDIT file" AT 2512
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 3 BLINK.
       ZA03.
	     ADD 1		 TO WS-AUDKEY.
	     READ AUDIT.
	   IF WS-STATUS NOT = "00"
	       IF WS-AUDKEY < W70-REP
		   GO TO ZA03
	       ELSE
	       IF WS-AUD < 1000
		   ADD 1	 TO WS-AUD
		   GO TO ZA03
	       END-IF
	   ELSE
	       MOVE ZERO	 TO WS-AUD
	   IF WS-AUDKEY > W70-REP
	       MOVE WS-AUDKEY	 TO W70-REP
	       GO TO ZA03
	   ELSE
	       GO TO ZA03.
029220	   IF LS0-STP NOT = 2
029230	       GO TO ZA05.
029240       OPEN I-O STOCK.
029250     IF RUNTIME-ERROR
               IF FLE-LOCKED
042130             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
029330     IF WS-STATUS NOT = "00"
029340	       MOVE 22		 TO WS-F-ERROR
029350         PERFORM OPEN-ERROR.
029360 ZA05.
029370	   IF LS0-DTP NOT = 2
029380         GO TO ZA06.
029390       OPEN I-O DEBTOR.
029400     IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 6        TO WS-F-ERROR
                   GO TO ZA50.
029420     IF WS-STATUS NOT = "00"
029430	       MOVE 6		 TO WS-F-ERROR
029440         PERFORM OPEN-ERROR.
029560	     OPEN I-O DEPART.
029570     IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
029590     IF WS-STATUS NOT = "00"
029600	       MOVE 7		 TO WS-F-ERROR
029610         PERFORM OPEN-ERROR.
038970       OPEN I-O SORDER.
038980     IF RUNTIME-ERROR
038990         IF FLE-LOCKED
039000             GO TO ZA200
039010         ELSE
039020         IF FLE-LIMIT
039030             GO TO ZA49
039040         ELSE
039050         IF IDX-CORRUPT
039060             MOVE 20       TO WS-F-ERROR
039070             GO TO ZA50.
039080     IF WS-STATUS NOT = "00"
039090         MOVE 20           TO WS-F-ERROR
039100         PERFORM OPEN-ERROR.
029360 ZA06.
029370	   IF LS0-CRP NOT = 2
029380         GO TO ZA10.
029390       OPEN I-O CREDIT.
029400     IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 3        TO WS-F-ERROR
                   GO TO ZA50.
029420     IF WS-STATUS NOT = "00"
029430	       MOVE 3		 TO WS-F-ERROR
029440         PERFORM OPEN-ERROR.
029620 ZA10.
029890	     MOVE 1		 TO WS-PARKEY.
029900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
029910	     MOVE PAR-DMY	 TO W12-TODAY.
029920	     MOVE PAR-YMD	 TO W12-T-YMD.
029930	     MOVE PAR-COMPANY	 TO W95-COMP.
019480	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE 1		 TO W05-V-RATE.

       ZA15-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA15-VAT.
030020	   IF LS0-DTP NOT = 2
030030         GO TO ZA999.
030040	   IF LS0-STP NOT = 2
030050         GO TO ZA999.
030060	     MOVE 1		 TO WS-AUDKEY.
030070       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
030080       MOVE AUD-REP1       TO WS-REP1.
030090       MOVE AUD-REP2       TO WS-REP2.
030100       MOVE AUD-REP3       TO WS-REP3.
030110       MOVE AUD-REP4       TO WS-REP4.
030120       MOVE AUD-REP5       TO WS-REP5.
030130       MOVE AUD-REP6       TO WS-REP6.
030140       MOVE AUD-REP7       TO WS-REP7.
030150       MOVE AUD-REP8       TO WS-REP8.
030160       MOVE AUD-REP9       TO WS-REP9.
030170       MOVE AUD-REP10      TO WS-REP10.
030170	     MOVE AUD-REP11	 TO WS-REP11.
030190       GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
	     ACCEPT WS-OPTION AT 1238
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5.
             GO TO ZA205.
       ZA50.
041850     IF WS-F-ERROR = 3
041860         MOVE "CREDITOR file"
                                 TO WS-FILE
	   ELSE
041850     IF WS-F-ERROR = 6
041860         MOVE "DEBTOR file"
                                 TO WS-FILE
	   ELSE
041850	   IF WS-F-ERROR = 20
041860	       MOVE "SALES ORDER"
                                 TO WS-FILE
           ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE "STOCK FILE" TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
	     ACCEPT WS-OPTION AT 1038
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5.
       ZA51.
             EXIT PROGRAM.
030200 ZA200.
             DISPLAY "This program can only be run when no other worksta
      -      "tions are logged into the" AT 0803
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
             DISPLAY "accounting system. All other workstations should r
      -      "eturn to the program con-" AT 0903
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
             DISPLAY "trol menu or exit from the accounting system befor
      -      "e you may continue       " AT 1003
                      WITH BACKGROUND-COLOR 4 FOREGROUND-COLOR 15.
030210       DISPLAY "Files locked - Try later" AT 2312
030220                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
030230       DISPLAY "Press any key" AT 2512 
030240		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
030250	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
030280 ZA999.
030290       EXIT.
      /
030300 ZB00-EOJ               SECTION 89.
030310 ZB00-END.
             DISPLAY CLR-SCREEN.
             DISPLAY "CLOSE DOWN PROCEDURE" AT 0231 
                      WITH FOREGROUND-COLOR 15.
030320     IF WS-CLEAR = 1
030330         DISPLAY "The daily audit file will be cleared - Is this c
030340- 	       "orrect Y/N?" AT 2512
			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			     BACKGROUND-COLOR 3
030350	       MOVE "Y" 	 TO WS-REPLY
030360	       ACCEPT WS-REPLY AT 2573
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
030370			   BACKGROUND-COLOR 5 UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-REPLY
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
030430	       IF WS-REPLY NOT = "Y"
030440		   MOVE ZERO	 TO WS-CLEAR.
030450     IF WS-CLEAR NOT = 1
030460         GO TO ZB05.
             DISPLAY "Clearing " AT 0412 WITH FOREGROUND-COLOR 7
                      W02-AUDITF WITH FOREGROUND-COLOR 14.
030470	     MOVE 1		 TO WS-AUDKEY.
030480       MOVE 1              TO WS-NETKEY.
030490       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *	   IF DEB-POSMODE = "N"
      *	       OPEN OUTPUT SLIP
      *	       CLOSE SLIP.
030500	   IF LS0-DTP = 2
030510         CLOSE AUDIT
030520         OPEN OUTPUT AUDIT
030530	       MOVE ZERO	 TO AUD-REP1 AUD-REP2 AUD-REP3
030540				    AUD-REP4 AUD-REP5 AUD-REP6
030550				    AUD-REP7 AUD-REP8 AUD-REP9
030560				    AUD-TYPE AUD-REP10
030570         PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
030580	     MOVE 2		 TO DEB-AUDIT.
030590       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             MOVE 6              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018710       MOVE ZERO           TO PAR-RAILD PAR-DISCD.
018720       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             MOVE 7              TO WS-PARKEY.
             MOVE ZERO           TO PAR-CASH-DAY PAR-CASH-RDAY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 8              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017600       MOVE ZERO           TO PAR-CSHT.
017610       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             MOVE 9              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017600       MOVE ZERO           TO PAR-DVAT-DAY
                                    PAR-CVAT-DAY
                                    PAR-DGST-DAY
                                    PAR-CGST-DAY
                                    PAR-DNGS-DAY
                                    PAR-CNGS-DAY.
017610       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             DISPLAY "Clearing " AT 0612 WITH FOREGROUND-COLOR 7
                     "DEBTOR" WITH FOREGROUND-COLOR 14
                     " transaction statistics :" 
                      WITH FOREGROUND-COLOR 7.
             MOVE 11             TO WS-PARKEY.
             MOVE 1              TO WS-S1.

       ZB02.
             MOVE WS-S1          TO W40-KEY.
             DISPLAY W40-KEY AT 0656 WITH FOREGROUND-COLOR 15.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE ZERO		 TO PAR-T-DAY(1)
				    PAR-T-DAY(2)
				    PAR-T-VAL(1)
				    PAR-T-VAL(2).
017610       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 60
               ADD 2             TO WS-S1
               ADD 1             TO WS-PARKEY
               GO TO ZB02.
             DISPLAY "Clearing " AT 0812 WITH FOREGROUND-COLOR 7
                     "CREDITOR" WITH FOREGROUND-COLOR 14
                     " transaction statistics :" 
                      WITH FOREGROUND-COLOR 7.
             MOVE 191            TO WS-PARKEY.
             MOVE 2              TO WS-S1.             
       ZB03.
             MOVE WS-S1          TO W40-KEY.
             DISPLAY W40-KEY AT 0856 WITH FOREGROUND-COLOR 15.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE ZERO		 TO PAR-C-DAY(1)
				    PAR-C-DAY(2)
				    PAR-C-VAL(1)
				    PAR-C-VAL(2).
017610       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
           IF WS-PARKEY < 200
               ADD 2             TO WS-S1
               ADD 1             TO WS-PARKEY
               GO TO ZB03.
             DISPLAY "Clearing " AT 1012 WITH FOREGROUND-COLOR 7
                     W02-DEPART WITH FOREGROUND-COLOR 14
                     " :" WITH FOREGROUND-COLOR 7.
	     MOVE " !"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO ZB05.

       ZB04.
	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO ZB05.
	     DISPLAY DPT-CODE AT 1056
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	   IF DPT-SCDE = "VAT"
	       MOVE ZERO	 TO DPT-V-SALES-DAY
				    DPT-V-RET-DAY
				    DPT-O-VAT-DAY
				    DPT-O-VRET-DAY
				    DPT-V-PUR-DAY
				    DPT-V-PRET-DAY
				    DPT-I-VAT-DAY
				    DPT-I-VRET-DAY
	   ELSE
	       MOVE ZERO	 TO DPT-SALES-DAY
                                    DPT-RET-DAY
                                    DPT-ADJ-DAY
                                    DPT-TRF-DAY
                                    DPT-DISC-DAY
                                    DPT-COST-DAY
                                    DPT-CRET-DAY
                                    DPT-PRET-DAY
                                    DPT-PUR-DAY.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
	     GO TO ZB04.

030600 ZB05.
030610	   IF LS0-STP = 2
030620         CLOSE STOCK.
030630       CLOSE PARAM.
030640	   IF LS0-DTP = 2
030650         CLOSE AUDIT
                     DEBTOR
030660               SORDER.
	   IF LS0-CRP = 2
               CLOSE CREDIT.
	     CLOSE NETWORK
		   DEPART.
      *    *************************************************************
      *    ****   6   L P I
      *    *************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 6		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

030720 ZB999.
030730       EXIT.
      /
030740 ZC00-ERROR             SECTION 89.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041570     IF WS-F-ERROR = 1
041580	       MOVE W02-AUDITF	 TO WS-FILE
041590	       MOVE WS-AUDKEY	 TO WS-KEY
041600     ELSE
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041600     ELSE
041610     IF WS-F-ERROR = 3
041620	       MOVE W02-CREDIT	 TO WS-FILE
041630	       MOVE ZERO	 TO WS-KEY
041760     ELSE
041770     IF WS-F-ERROR = 6
041780	       MOVE W02-DEBTOR	 TO WS-FILE
041790	       MOVE ZERO	 TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041800     ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 20
052210         MOVE W02-SORDER   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE SOR-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 22
041860	       MOVE W02-STOCKF	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 32
041860	       MOVE W02-TXTRAN	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY.
041880     IF WS-STATUS = "10"
041890	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980	       MOVE "DISK full"  TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
	   IF WS-STAT1 = 9
045390	       DISPLAY "Status " AT 1212
			WS-STAT1 WITH FOREGROUND-COLOR 11
			WS-STAT2 WITH FOREGROUND-COLOR 11
	   ELSE
045390	       DISPLAY "Status " AT 1212
			WS-STATUS WITH FOREGROUND-COLOR 11
		       ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
