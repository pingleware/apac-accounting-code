      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	    ****    ********   *
      *   **	**  **	  **  **    **	 **  **    **  **   **	       *
      *   **	    **	  **  **    **	**    **  **	**  *******    *
      *   **	    ******    *******	**    **  **	**	  **   *
      *   **	    **	**    **	**    **  **	**	  **   *
      *   **	**  **	 **   **	 **  **    **  **   **	  **   *
      *    ******   **	  **  **	  ****	    ****     ******    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      *       C R E D I T O R	M E M O / R E M A R K S	  M A I N T    *
      *                                                                *
      *     VERSION 8.15.01 - August 2011			       *
      * 							       *
      ******************************************************************
      * 							       *
      * August 2011 - Amended MEMO file to have 6 character Account    *
      * 	      Reference code.Increased sequence to 4 digits.   *
      * 	      Included Recovery routines and file locking for  *
      * 	      this program.				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CRP005.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2011
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@mail.com.

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

000120 COPY CONTROL.SL.

000140 COPY CREDIT.SL.

000140 COPY CRDMEM.SL.

000140 COPY MEMCRD.SL.

000120 COPY PARAM.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

000140 COPY CRDMEM.FDE.

       COPY MEMCRD.FDE.

000290 COPY PARAM.FDE.

000390 COPY RECOVER.GFD.

       COPY SHARED.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7           PIC  9(04)    COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
000480 77  WS-RECOVER	   PIC	9(04)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-TRANS	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-FIRST	   PIC	9(04)	 COMP-5 VALUE 0.
001730 77  WS-LAST	   PIC	9(04)	 COMP-5 VALUE 0.
       77  WS-MEMPOS	   PIC	9(04)	 COMP-5 VALUE 0.
001750 77  WS-FIN-RATE	   PIC S9(03)V99 COMP-3.
001760 77  WS-PERIOD       PIC  9(02).
001770 77  WS-NTP          PIC  X(01).
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
001820 77  WS-TAX          PIC  X(01).
001820 77  WS-TYPE         PIC  X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
001870 77  WS-EKEY         PIC  9(06)    COMP-5.
001900 77  WS-COMP         PIC  9(01).
002110 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
000930 77  WS-ER8          PIC  X(09) VALUE "No record".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002140 01  WS-ENTRY.
002150     03  WS-ENT0     PIC  X(08).
002160     03  WS-TYPE0    PIC  X(01).
002170     03  WS-KEY0     PIC  9(06)    COMP-5.
002180 01  WS-ENTRY1.
002190     03  WS-ENT1     PIC  X(08).
002200     03  WS-TYPE1    PIC  X(01).
002210     03  WS-KEY1     PIC  9(06)    COMP-5.
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002430	   03  WS-TP-LNE.
002440         05  WS-TCR  PIC X(01) OCCURS 80.
	   03  WS-TPCR-LNE REDEFINES WS-TP-LNE.
	       05  WS-TCR1     PIC  X(01).
	       05  WS-TCR78    PIC  X(78).
	       05  WS-TCR80    PIC  X(01).
002450	   03  WS-MD-LNE.
002460	       05  WS-MCR  PIC X(01) OCCURS 80.
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).
002470	   03  WS-BT-LNE.
002480         05  WS-BCR  PIC X(01) OCCURS 80.
	   03  WS-BTCR-LNE REDEFINES WS-BT-LNE.
	       05  WS-BCR1     PIC  X(01).
	       05  WS-BCR78    PIC  X(78).
	       05  WS-BCR80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "ณ".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "ฐ".
	       05  FILLER      PIC  X(01) VALUE "ณ".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "CRP".
	   03  WS-PROG	       PIC  X(03) VALUE "005".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY CREDIT.ID.

000140 COPY CRDMEM.ID.

000140 COPY MEMCRD.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

002500 01  W10-SPARES.
002510	   03  W10-DATE	    PIC X(08).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

       COPY W15.CRP.

002180 01  W40-NADD.
002190     03  W40-ADD.
002200         05  W40-ACHAR PIC  X(01) OCCURS 70.
           03  W40-ADDR REDEFINES W40-ADD.
               05  W40-ADDRS PIC  X(60).
               05  FILLER    PIC  X(10).
002210     03  W40-NAME.
002220         05  W40-NCHAR PIC  X(01) OCCURS 40.

       COPY W40.WS.

       COPY FUNCTION.WS.

003530 COPY W50.WS.

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
           03  W75-S         PIC  9(02) COMP-5.
           03  W75-S1        PIC  9(02) COMP-5.
	   03  W75-CNO	     PIC  X(06) OCCURS 18.

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

       01  W200-KEYS.
	   03  W200-KEY        PIC  X(10) OCCURS 100.

       01  W205-MEMO.
	   03  W205-MEM-DET.
	       05  W205-MEM    PIC  X(60) OCCURS 100.
	   03  W205-S	       PIC  9(04) COMP-5.
	   03  W205-S1	       PIC  9(04) COMP-5.
	   03  W205-S2	       PIC  9(04) COMP-5.
	   03  W205-LAST       PIC  9(04) COMP-5.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

002840 01  S05.
	   03  LINE  5 COLUMN 13 VALUE "Account number ".
002850	   03  LINE  6 COLUMN 13 VALUE "Creditor name  ".
002860	   03  LINE  7 COLUMN 13 VALUE "Address line 1 ".
002870	   03  LINE  8 COLUMN 13 VALUE "        line 2 ".
002880	   03  LINE  9 COLUMN 13 VALUE "        line 3 ".
002890	   03  LINE 10 COLUMN 13 VALUE "        line 4 ".
002900	   03  LINE 11 COLUMN 13 VALUE "Postal code    ".
	   03  LINE 12 COLUMN 13 VALUE "Contact        ".
	   03  LINE 13 COLUMN 13 VALUE "Telephone      ".
	   03  LINE 14 COLUMN 13 VALUE "Fax number     ".
002910	   03  LINE 15 COLUMN 13 VALUE "Terms       30 ".
002930	   03  LINE 16 COLUMN 13 VALUE "            60 ".
002940	   03  LINE 17 COLUMN 13 VALUE "            90 ".
002950	   03  LINE 18 COLUMN 13 VALUE "           120 ".
002960	   03  LINE 19 COLUMN 13 VALUE "           150 ".
002970	   03  LINE 20 COLUMN 13 VALUE "           180 ".
002910	   03  LINE 21 COLUMN 13 VALUE "Payment        ".
	   03  LINE 22 COLUMN 13 VALUE "VAT code       ".
	   03	       COLUMN 31 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "I".
           03          COLUMN 32 VALUE "n/".
	   03	       COLUMN 34 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "E".
           03          COLUMN 35 VALUE "xclusive/".
	   03	       COLUMN 44 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "N".
           03          COLUMN 45 VALUE "o VAT".

002980 01  S06.
002990     02  S06A.
	   03  LINE  5 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W15-ACNO AUTO.
003020     02  S06B.
003000	   03  LINE  6 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003010                           PIC X(40) USING W15-NAME AUTO.
003030	   03  LINE  7 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003040                           PIC X(30) USING W15-ADDR1 AUTO.
003050	   03  LINE  8 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003060                           PIC X(30) USING W15-ADDR2 AUTO.
003070	   03  LINE  9 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003080                           PIC X(30) USING W15-ADDR3 AUTO.
003090	   03  LINE 10 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003100                           PIC X(30) USING W15-ADDR4 AUTO.
003110	   03  LINE 11 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003120				 PIC 9(04) USING W15-PCODE AUTO.
	   03  LINE 12 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(30) USING W15-CONTACT AUTO.
	   03  LINE 13 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(14) USING W15-TEL AUTO.
	   03  LINE 14 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(14) USING W15-FAX AUTO.
003130	   03  LINE 15 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003140				 PIC 9(01) USING W15-30 AUTO.
003150	   03  LINE 16 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003160                           PIC 9(01) USING W15-60 AUTO.
003170	   03  LINE 17 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003180                           PIC 9(01) USING W15-90 AUTO.
003190	   03  LINE 18 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003200                           PIC 9(01) USING W15-120 AUTO.
003210	   03  LINE 19 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003220                           PIC 9(01) USING W15-150 AUTO.
003230	   03  LINE 20 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003240                           PIC 9(01) USING W15-180 AUTO.
003130	   03  LINE 21 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
003140                           PIC X(12) USING W15-PMNT AUTO.
	   03  LINE 22 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
                                 PIC X(01) USING W15-TAX AUTO.

       COPY CRDLUP.CRT.

004870 COPY S99.CRT.

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
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
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
	   02  SA-MEM-1.
	   03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
	   02  SA-MEM-2.
	   03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
	   02  SA-MEM-3.
	   03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
	   02  SA-MEM-4.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
	   02  SA-MEM-5.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
	   02  SA-MEM-6.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
	   02  SA-MEM-7.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
	   02  SA-MEM-8.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
	   02  SA-MEM-9.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
	   02  SA-MEM-10.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
	   02  SA-MEM-11.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
	   02  SA-MEM-12.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       Check the security level of the USER.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
003240	   IF LS0-CRLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
008250	     PERFORM ZA000-INIT.
	     PERFORM CJ000.

013140 AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
011050	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM CREDITOR-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY CREDIT.RD.

000140 COPY CRDMEM.RD.

000140 COPY MEMCRD.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

       COPY CREDIT.WR.

       COPY CRDMEM.WR.

000140 COPY MEMCRD.WR.

       COPY PARAM.WR.

       COPY CREDIT.LUP.

       COPY APAC.HLP.

      /
      *       ****   *****   ***    ***   *   *  *****	****   *   *
      *       *   *  *	    *	*  *   *  *   *  *	*   *  *   *
      *       ****   ***    *	   *   *  *   *  ***	****	* *
      *       *   *  *	    *	*  *   *   * *	 *	*   *	 *
      *       *   *  *****   ***    ***     *	 *****	*   *	 *
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *        ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *        ณ ORIGINAL ACTION (REC-TYPE)  ฤฤ  RECOVERY PROCESS ณ
      *        ณ 0 = RECORD CHANGED	     ฤฤ     (REWRITE)	   ณ
      *        ณ 1 = RECORD WAS ADDED	     ฤฤ     (DELETE)	   ณ
      *        ณ 2 = RECORD WAS DELETED      ฤฤ     (WRITE)	   ณ
      *        ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
007940 AY000	       SECTION.
008000 AY06.
008010	     MOVE 06		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
	       PERFORM READ-CRDMEM-LOCK	THRU READ-CRDMEM-EXIT.
008030	     MOVE CME-RECORD1	 TO REC-CRDMEM.
008040       GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE
	     MOVE NET-RECORD	 TO REC-NETWORK.
             GO TO AY50.
008200 AY40.
008210	     MOVE 99		 TO REC-FILE.
008220	     MOVE ZERO		 TO REC-KEY.
008230	     MOVE SPACES	 TO REC-DETAIL.
008240       PERFORM AY50.
008250	     ADD 1		 TO WS-TRANS.
008260     IF WS-RECOVER > 95
008270         CLOSE RECOVER
008280         OPEN OUTPUT RECOVER
008290         CLOSE RECOVER
008300         OPEN I-O RECOVER
008310	       MOVE ZERO	 TO WS-RECOVER.
008320       GO TO AY59.
008330 AY50.
008340	     ADD 1		 TO WS-RECOVER.
008350	     MOVE WS-RECOVER	 TO WS-RECKEY.
008360	     MOVE WS-TRANS	 TO REC-TRAN.
008370       WRITE REC-RECORD.
008380     IF WS-STATUS NOT = "00"
008390         DISPLAY "Write error Recovery file - Status " AT 2212
008400		       WITH FOREGROUND-COLOR 6 HIGHLIGHT
008410	       DISPLAY WS-STATUS AT 2247
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
008420         STOP RUN.
008450 AY59.
008460       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
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
012620	     MOVE "CR"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 2              TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT(PAR-PROG(WS-SUB) = SPACES OR
		  PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT(PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
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
      *
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   NETWORK record 2.
      *
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY40 THRU AY59.
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
013090	     COMMIT.

011860 AY999.
011870       EXIT.
      *
      *    ****    Read Creditor record - using account number.
      *
011010 CA155-GET-CREDITOR      SECTION.
011020 CA155-ACCOUNT.
011060       MOVE W15-ACNO       TO P-NUMBER.
011130       PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150	       MOVE 1		 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180	       MOVE 2		 TO WS-ERROR
011190     ELSE
011200	       MOVE 0		 TO WS-ERROR.
011210 CA160-EXIT.
011220       EXIT.
      /
      *    ****    C R E D I T O R   M E M O   M A I N T E N A N C E
      *
037050 CJ000	       SECTION 50.
037060 CJ00.
020320       DISPLAY CLR-SCREEN.
020330       DISPLAY "MEMO/REMARKS MAINTENANCE" AT 0229
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
013080	     DISPLAY S05.
013000       MOVE SPACES         TO W15-ACNO.

020360 CJ02.
	     DISPLAY "F1" AT 2501
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     " to do Creditor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
013010       ACCEPT S06A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W15-ACNO
		     GO TO CJ999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CJ02
	       END-EVALUATE
	       DISPLAY S06A
	       IF W15-ACNO = SPACES
015990		   GO TO CJ02.
	     DISPLAY CLEAR-L25.
013020     IF W15-ACNO = SPACES
013030	       GO TO CJ999.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W15-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
013040	     PERFORM CA155-GET-CREDITOR.
013050     IF WS-ERROR NOT = ZERO
013060	       MOVE WS-ER8	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
013070	       GO TO CJ02.
020360 CJ05.
013110	     MOVE P-NAME	 TO W15-NAME.
013120       MOVE P-ADDR         TO W40-ADD.
             MOVE P-CONTACT      TO W15-CONTACT.
             MOVE P-TEL          TO W15-TEL.
             MOVE P-FAX          TO W15-FAX.
013130       MOVE P-30           TO W15-30.
013140       MOVE P-60           TO W15-60.
013150       MOVE P-90           TO W15-90.
013160       MOVE P-120          TO W15-120.
013170       MOVE P-150          TO W15-150.
013180       MOVE P-180          TO W15-180.
013190       MOVE P-PC           TO W15-PCODE.
             MOVE P-TAX          TO W15-TAX.
             MOVE P-PMNT         TO W15-PMNT.
015150	     UNSTRING W40-ADD DELIMITED BY "*" INTO
015160         W15-ADDR1 W15-ADDR2 W15-ADDR3 W15-ADDR4
015170	       OVERFLOW MOVE 1	 TO WS-ERROR.
	     DISPLAY S06B.

020770 CJ20.
	     PERFORM CHECK-CORRECT.
020860	   IF WS-OPTION = "N"
020870	       GO TO CJ00.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	   D I S P L A Y   T H E   M E M O   W I N D O W
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ   M E M O / R E M A R K S / C O R R E S P O N D E N C E   ณฐฐ
      *    ฦอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออตฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ณ							       ณฐฐ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤูฐฐ
      *      ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ
       CJ25.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     PERFORM SCREEN-SHADOW.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	  C H E C K   I F   T H E R E	I S   A N Y   D A T A
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	     INITIALIZE W200-MEM-FLDS.
	     MOVE W15-ACNO	 TO CME-AC.
	     MOVE 1		 TO CME-SEQ.
018690	     MOVE 1		 TO WS-FIRST WS-MEMPOS WS-LAST.
	     PERFORM READ-CRDMEM THRU READ-CRDMEM-EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	I F   T H E R E   I S	N O   D A T A	B Y P A S S
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	   IF WS-F-ERROR NOT = ZERO
	       OPEN OUTPUT MEMCRD
	       CLOSE MEMCRD
	       OPEN I-O MEMCRD
	       MOVE ZERO	 TO WS-S9
	       MOVE SPACES	 TO W205-MEM-DET W200-KEYS
	       MOVE 1		 TO W205-S
	       GO TO CJ40.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	    G E T   E X I S T I N G   M E M O	D A T A
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	     PERFORM GET-CRDMEM-DETAILS.
	     MOVE 1		 TO W205-S.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	  S E T   S T A R T   P O S I T I O N	F O R
      * 	  D I S P L A Y   A N D   A C C E P T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CJ28.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     MOVE 1		 TO WS-S1.
	     MOVE W205-S	 TO W205-S1 W205-S2.
	     INITIALIZE W200-MEM-FLDS.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *     S T O R E	M E M O   D E T A I L S   I N	D I S P L A Y
      * 		 A N D	 A C C E P T   A R E A
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CJ30.
	     MOVE W205-MEM(W205-S1)
				 TO W200-MEM(WS-S1).
	   IF WS-S1 > 11
	       GO TO CJ40.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	C H E C K   F O R   E N D   O F   M E M O   D A T A
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CJ35.
	   IF W205-S1 < W205-LAST
	       ADD 1		 TO WS-S1 W205-S1
	       GO TO CJ30.

       CJ40.
	     DISPLAY SA-MEM.
001020	     DISPLAY " " AT 2503 WITH BACKGROUND-COLOR 3
					FOREGROUND-COLOR 6 HIGHLIGHT
001030		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgUp" WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "PgDn" WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6 HIGHLIGHT
		     "," WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001170		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape," WITH BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Insert line," WITH BACKGROUND-COLOR 3
				     FOREGROUND-COLOR 1
		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Delete line," WITH BACKGROUND-COLOR 3
					  FOREGROUND-COLOR 1
		     "F10" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Update" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 1.
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณAs the program is using a SCREEN SECTION definition for theณ
      *    ณACCEPT and DISPLAY functions, the definition was changed toณ
      *    ณenable each line to be referred to individualy. This has   ณ
      *    ณresulted in a routine being written for each line.	       ณ
      *    ณThis will be changed to use occurs clause and then these   ณ
      *    ณprocedures can be combined. As there are higher priority   ณ
      *    ณissues, this will be done once there is more time availableณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
       CJ45.
	     ACCEPT SA-MEM-1.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		  M O V E   U P   O N E   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     IF W205-S = 1
			 GO TO CJ45
		     ELSE
			 SUBTRACT 1
				 FROM W205-S
001320			 GO TO CJ28
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		M O V E   D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ50
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ45
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ45
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM1
		     PERFORM CJ130
		     GO TO CJ45
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       I N V A L I D   K E Y   S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ45.
	     ADD 1		 TO W205-S2.

       CJ50.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-2.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		 M O V E   U P	 O N E	 L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ45
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ55
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ50
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ50
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM2
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ45
		     ELSE
			 GO TO CJ50
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ50.
	     ADD 1		 TO W205-S2.

       CJ55.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-3.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		 M O V E   U P	 O N E	 L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ50
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ60
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ55
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ55
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM3
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ50
		     ELSE
			 GO TO CJ55
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ55.
	     ADD 1		 TO W205-S2.

       CJ60.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-4.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		 M O V E   U P	 O N E	 L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ55
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ65
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ60
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ60
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM4
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ55
		     ELSE
			 GO TO CJ60
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ60.
	     ADD 1		 TO W205-S2.

       CJ65.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-5.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		 M O V E   U P	 O N E	 L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ60
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ70
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ65
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ65
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM5
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ60
		     ELSE
			 GO TO CJ65
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ65.
	     ADD 1		 TO W205-S2.

       CJ70.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-6.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		  M O V E   U P   O N E   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ65
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ75
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ70
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ70
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM6
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ65
		     ELSE
			 GO TO CJ70
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ70.
	     ADD 1		 TO W205-S2.

       CJ75.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-7.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		  M O V E   U P   O N E   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ70
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ80
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ75
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ75
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM7
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ70
		     ELSE
			 GO TO CJ75
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ75.
	     ADD 1		 TO W205-S2.

       CJ80.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-8.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		  M O V E   U P   O N E   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ75
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ85
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ80
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ80
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM8
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ75
		     ELSE
			 GO TO CJ80
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ80.
	     ADD 1		 TO W205-S2.

       CJ85.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-9.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		  M O V E   U P   O N E   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ80
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ90
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ85
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ85
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM9
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ80
		     ELSE
			 GO TO CJ85
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ85.
	     ADD 1		 TO W205-S2.

       CJ90.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-10.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		  M O V E   U P   O N E   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ85
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ95
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ90
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ90
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM10
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ85
		     ELSE
			 GO TO CJ90
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ90.
	     ADD 1		 TO W205-S2.

       CJ95.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-11.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		  M O V E   U P   O N E   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ90
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     ADD 1	 TO W205-S2
001340		     GO TO CJ100
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ95
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ95
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM11
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ90
		     ELSE
			 GO TO CJ95
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ95.
	     ADD 1		 TO W205-S2.

       CJ100.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     ACCEPT SA-MEM-12.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		  M O V E   U P   O N E   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN UP-KEY
		     SUBTRACT 1	 FROM W205-S2
001320		     GO TO CJ95
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       M O V E	 D O W N   O N E    L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN DOWN-KEY
		     IF W205-S > 88
001340			 GO TO CJ100
		     ELSE
			 PERFORM CK000
			 ADD 1	 TO W205-S
			 ADD 11 W205-S
				 GIVING W205-S2
			 PERFORM CJ28 THRU CJ40
			 ADD 11  TO W205-S2
			 GO TO CJ100
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ100
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			    E X I T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN ESC-KEY
		     GO TO CJ110
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-DOWN
001340		     GO TO CJ115
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN PAGE-UP
001340		     GO TO CJ120
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F2-KEY
		     PERFORM CJ125
		     GO TO CJ100
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F3-KEY
		     MOVE SPACES TO W200-MEM12
		     PERFORM CJ130
		     IF WS-S6 < WS-S5
			 GO TO CJ95
		     ELSE
			 GO TO CJ100
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       E N D   I N P U T
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN F10-KEY
		     GO TO CJ105
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N V A L I D	K E Y	S T R O K E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO CJ100.
	     PERFORM CK000.
	   IF W205-S2 = 100
	       GO TO CJ105.
	   IF W205-S2 = W205-LAST
	       ADD 1		 TO W205-LAST.
	     ADD 1		 TO W205-S.
	     ADD 11 W205-S	 GIVING W205-S2.
	     PERFORM CJ28 THRU CJ40.
	     ADD 11		 TO W205-S2.
	     GO TO CJ100.

       CJ105.
	     PERFORM CK000.
	     ADD 11 W205-S	 GIVING W205-S2.
	   IF W205-S2 > W205-LAST
	       MOVE W205-S2	 TO W205-LAST.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
	       GO TO CJ45.
	     PERFORM CN000.

       CJ110.
	     PERFORM RESTORE-SCREEN-3.
	     CLOSE MEMCRD.
	     GO TO CJ00.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		       P A G E	 D O W N
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CJ115.
	     PERFORM CK000.
	   IF W205-S > 75
	       MOVE 88		 TO W205-S
	   ELSE
	       ADD 12		 TO W205-S.
	     GO TO CJ28.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 			 P A G E   U P
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CJ120.
	     PERFORM CK000.
	   IF W205-S < 13
	       MOVE 1		 TO W205-S
	   ELSE
	       SUBTRACT 12	 FROM W205-S.
	     GO TO CJ28.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   I N S E R T	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CJ125.
	     PERFORM CK000.
	  IF WS-LAST < 100
	      PERFORM CL000.
	     PERFORM CJ28 THRU CJ40.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		   D E L E T E	 A   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CJ130.
	     PERFORM CK000.
	     MOVE W205-S2	 TO WS-S6 WS-S5.
	     PERFORM CM000.
	     PERFORM CJ28 THRU CJ40.
	     MOVE WS-S6 	 TO W205-S2.

       CJ999.
	     EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	  M O V E   D A T A   F R O M	A C C E P T   A R E A
      * 	  T O	W 2 0 5   A N D   W R I T E   T H E S E
      * 	  D E T A I L S   T O	T H E	W O R K   F I L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CK000	       SECTION 51.
       CK00.
	     MOVE W205-S	 TO W205-S1.
	     MOVE 1		 TO WS-S1.
	     MOVE P-NUMBER	 TO MEM-AC.

       CK05.
	     MOVE W205-S1	 TO MEM-SEQ.
	     MOVE W200-MEM(WS-S1)
				 TO W205-MEM(W205-S1)
				    MEM-DATA.
	     PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
	   IF WS-F-ERROR = 62
	       MOVE ZERO	 TO WS-F-ERROR
	       IF NOT(MEM-DATA = SPACES)
		   PERFORM WRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
	   IF WS-S1 < 12
	       ADD 1		 TO WS-S1 W205-S1
	       GO TO CK05.

       CK999.
	     EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       I N S E R T   A	 B L A N K   L I N E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CL000	       SECTION 52.
       CL00.
	     MOVE W205-S1	 TO WS-S7.
	     MOVE W205-S2	 TO WS-S8.
	     MOVE W205-LAST	 TO W205-S1.
	     ADD 1		 TO W205-LAST.
	     MOVE W205-LAST	 TO W205-S2.
	     MOVE P-NUMBER	 TO MEM-AC.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *      M O V E   D A T A	 U P   I N   W O R K   A R E A	 A N D
      *      I N   T H E   W O R K   F I L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CL05.
	     MOVE W205-S2	 TO MEM-SEQ.
	     MOVE W205-MEM(W205-S1)
				 TO W205-MEM(W205-S2)
				    MEM-DATA.
	   IF W205-S2 = W205-LAST
	       PERFORM WRITE-MEMCRD THRU WRITE-MEMCRD-EXIT
	       IF WS-F-ERROR = 62
		   PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT
		   MOVE ZERO	 TO WS-F-ERROR
	       END-IF
	   ELSE
	       PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
	   IF W205-S1 > WS-S8
	       SUBTRACT 1	 FROM W205-S1 W205-S2
	       GO TO CL05.
	     MOVE SPACES	 TO W205-MEM(W205-S1)
				    MEM-DATA.
	     MOVE W205-S1	 TO MEM-SEQ.

       CL10.
	     PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
	     MOVE WS-S7		 TO W205-S1.
	     MOVE WS-S8		 TO W205-S2.

       CL999.
	     EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *       D E L E T E   T H E   L I N E   B E I N G   E D I T E D
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CM000	       SECTION 53.
       CM00.
	     MOVE W205-S1	 TO WS-S7.
	     MOVE W205-S2	 TO WS-S8.
	     MOVE P-NUMBER	 TO MEM-AC.

       CM02.
	   IF W205-MEM(W205-LAST) = SPACES
	       IF W205-S2 = W205-LAST
		   GO TO CM999
	       ELSE
	       IF W205-S2 < W205-LAST
		   SUBTRACT 1	 FROM W205-LAST
		   GO TO CM02.
	   IF W205-S2 = W205-LAST
	       MOVE SPACES	 TO W205-MEM(W205-S2)
	       GO TO CM10.
	     ADD 1 W205-S2	 GIVING	W205-S1.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *        M O V E	 D A T A   D O W N   I N   W O R K   A R E A
      *        A N D   I N   T H E   W O R K   F I L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CM05.
	     MOVE W205-S2	 TO MEM-SEQ.
	     MOVE W205-MEM(W205-S1)
				 TO W205-MEM(W205-S2)
				    MEM-DATA.
	     PERFORM REWRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
	     MOVE SPACES	 TO W205-MEM(W205-S1).
	   IF W205-S1 < W205-LAST
	       ADD 1		 TO W205-S1 W205-S2
	       GO TO CM05.

       CM10.
	     MOVE SPACES	 TO MEM-DATA.
	     MOVE W205-LAST	 TO MEM-SEQ.
	     PERFORM READ-MEMCRD THRU READ-MEMCRD-EXIT.
	     PERFORM DELETE-MEMCRD-REC THRU WRITE-MEMCRD-EXIT.
	     SUBTRACT 1 	 FROM W205-LAST.
	     MOVE WS-S7		 TO W205-S1.
	     MOVE WS-S8		 TO W205-S2.
	   IF W205-S2 > W205-LAST
	       MOVE W205-LAST	 TO W205-S2 WS-S6.

       CM999.
	     EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	      U P D A T E   T H E   M E M O   F I L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN000	       SECTION 54.
       CN00.
	     MOVE 1		 TO W205-S1 WS-S6.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	  F I N D   L A S T   L I N E	W I T H   D A T A
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN02.
	   IF W205-MEM(W205-LAST) = SPACES
	       SUBTRACT 1	 FROM W205-LAST
	       GO TO CN02.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		R E M O V E   B L A N K   L I N E S
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN05.
	   IF W205-MEM(W205-S1) = SPACES
	       PERFORM CO000
	       GO TO CN05.
	   IF W205-S1 < W205-LAST
	       ADD 1		 TO W205-S1
	       GO TO CN05.
	     MOVE 1		 TO W205-S1.
	     MOVE P-NUMBER	 TO CME-AC.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	    S T A R T	R E C O V E R Y	  L O G G I N G
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
010830	     PERFORM AY60 THRU AY999.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	      U P D A T E   T H E   M E M O   F I L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN10.
	     MOVE W205-S1	 TO CME-SEQ.
	   IF CME-KEY = W200-KEY(WS-S6)
	       MOVE 0		 TO WS-ACTION
	       PERFORM AY06 THRU AY59.
	     MOVE W205-MEM(W205-S1)
				 TO CME-DATA.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	     F I N D   K E Y   I N   K E Y   T A B L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN15.
	   IF CME-KEY < W200-KEY(WS-S6)
	       GO TO CN20.
	   IF CME-KEY = W200-KEY(WS-S6)
	       GO TO CN25.
	   IF WS-S6 < W205-LAST
	       ADD 1		 TO WS-S6
	       GO TO CN15.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       A D D   N E W   M E M O	 R E C O R D
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN20.
	     PERFORM WRITE-CRDMEM THRU WRITE-CRDMEM-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY06 THRU AY59.
	     GO TO CN30.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *        U P D A T E   E X I S T I N G   M E M O	 R E C O R D
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN25.
	     PERFORM REWRITE-CRDMEM THRU WRITE-CRDMEM-EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	C L E A R   K E Y S   O F   U S E D   R E C O R D S
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	     MOVE SPACES	 TO W200-KEY(WS-S6).

       CN30.
	   IF W205-S1 < W205-LAST
	       ADD 1		 TO W205-S1
	       GO TO CN10.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *      D E L E T E   R E D U N D A N T   R E C O R D S   F R O M
      *      M E M O   F I L E
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	     MOVE 1		 TO WS-S6.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 	       F I N D	 R E D U N D A N T   K E Y S
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CN35.
	   IF NOT(W200-KEY(WS-S6) = SPACES)
	       MOVE W200-KEY(WS-S6)
				 TO CME-KEY
	       MOVE 2		 TO WS-ACTION
	       PERFORM AY06 THRU AY59
	       PERFORM DELETE-CRDMEM-REC THRU WRITE-CRDMEM-EXIT.
	   IF WS-S6 < WS-S9
	       ADD 1		 TO WS-S6
	       GO TO CN35.

       CN900.
	     PERFORM AY70 THRU AY999.

       CN999.
	     EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *        M O V E	 D A T A   D O W N   I N   W O R K   A R E A
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       CO000	       SECTION 54.
       CO00.
	     MOVE W205-S1	 TO WS-S7.
	     ADD 1 WS-S7	 GIVING WS-S8.

       CO05.
	     MOVE W205-MEM(WS-S8)
				 TO W205-MEM(WS-S7).
	     MOVE SPACES	 TO W205-MEM(WS-S8).
	   IF WS-S8 < W205-LAST
	       ADD 1		 TO WS-S8 WS-S7
	       GO TO CO05.
	     SUBTRACT 1 	 FROM W205-LAST.

       CO999.
	     EXIT.
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      *      G E T   T H E   E X I S T I N G   M E M O	 D E T A I L S
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
       GET-CRDMEM-DETAILS SECTION 55.
       OPEN-MEMCRD.
	     OPEN OUTPUT MEMCRD.
	     CLOSE MEMCRD.
	     OPEN I-O MEMCRD.
	     MOVE ZERO		 TO WS-S9.
	     MOVE SPACES	 TO W205-MEM-DET W200-KEYS.
	     MOVE CME-AC	 TO MEM-AC.

       WRITE-MEMO-DETAILS.
	     MOVE CME-SEQ	 TO MEM-SEQ WS-LAST.
	     ADD 1		 TO WS-S9.
	     MOVE CME-DATA	 TO MEM-DATA W205-MEM(WS-S9).
	     MOVE CME-KEY	 TO W200-KEY(WS-S9).
	     PERFORM WRITE-MEMCRD THRU WRITE-MEMCRD-EXIT.
	     PERFORM READ-CRDMEM-NEXT THRU READ-CRDMEM-EXIT.
	   IF WS-F-ERROR = ZERO
	       IF CME-AC = W15-ACNO
		   GO TO WRITE-MEMO-DETAILS.
	     MOVE WS-S9 	 TO W205-LAST.

       GET-CRDMEM-DETAILS-EXIT.
	     EXIT.

      /    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      * 		I N I T I A L I Z E   P R O G R A M
      *    อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
041980 ZA000-INIT	       SECTION 8.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ  Setup various keys and the mouse for use with menus and  ณ
      *    ณ  other required user interface functions and procedures.  ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
	     COPY FUNCTION.PRO.
      *    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
      *    ณ	    Setup the file ID's including path names           ณ
      *    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
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
      *    **** 	C R E D I T O R	  M E M O   F I L E	    ****
      *    *************************************************************
	     MOVE "CRDMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRDMEM.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    ****    C R E D I T O R   M E M O   W O R K	 F I L E    ****
      *    *************************************************************
	     MOVE "MEMCRD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-MEMCRD.
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
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
019930	     OPEN OUTPUT RECOVER.
019940       CLOSE RECOVER.
019950       OPEN I-O RECOVER.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
044780	     GO TO ZA999-EXIT.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MD-LNE.

041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCR(WS-S1) WS-BCR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1) WS-BCH(WS-S1).
041260	   IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280	       GO TO ZA60.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
021970	     MOVE WS-G4 	 TO WS-BCR(8)  WS-BCR(50)
021980				    WS-BCR(66).
021950	     MOVE WS-G3 	 TO WS-MCR(8)  WS-MCR(50)
021960				    WS-MCR(66) WS-MCR(1)
				    WS-MCR(80).
051230	     MOVE WS-G14	 TO WS-TCR(1)  WS-BCR(1).
051260	     MOVE WS-G13	 TO WS-TCR(80) WS-BCR(80).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorฤ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 8.
      
       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041760	   ELSE
041650	   IF WS-F-ERROR = 3
041660	       MOVE W02-CREDIT	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 18
041860	       MOVE W02-RECOVER  TO WS-FILE
041870	       MOVE WS-RECKEY	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 24
041860	       MOVE W02-CRDMEM	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850	   IF WS-F-ERROR = 62
041860	       MOVE W02-MEMCRD	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE MEM-KEY	 TO WS-KEYX.

       COPY DISPERR.PRO.
