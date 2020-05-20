      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	     **     ******     *
      *   **	**     **     **    **	 **  **     ***    **	 **    *
      *   **	**     **     **    **	**    **     ** 	**     *
      *   **	**     **     *******	**    **     **       **       *
      *   **	**     **     **	**    **     **     **	       *
      *   **	**     **     **	 **  **      **    **	       *
      *   *******      **     **	  ****	   ******  ********    *
      *                                                                *
      *       ENGLISH                                                  *
      *                                                                *
      * 	A C C O U N T S	  R E C E I V A B L E	/	       *
      *                                                                *
      *     D E B T O R   R E C E I P T   R E V E R S A L S	       *
      *     P R O G R A M					       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP012R.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   JULY 2004.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2004 - 2011
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

000130 COPY AUDIT.SL.

       COPY CHEQUE.SL.

000120 COPY CONTROL.SL.

000160 COPY DBTRAN.SL.

000140 COPY DEBTOR.ISL.

000140 COPY DEBTRN.SL.

000140 COPY DEBMEM.SL.

000140 COPY DEPART.SL.

       COPY LEDTRF.SL.

000120 COPY PARAM.SL.

000220 COPY RECEIPTS.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

       COPY TXTRAN.SL.

000280	   SELECT CSHDRW  ASSIGN W02-CSHDRW
000290                    ORGANIZATION SEQUENTIAL
000300                    ACCESS SEQUENTIAL.
000240     SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000420 COPY AUDIT.FDE.

       COPY CHEQUE.FD.

000120 COPY CONTROL.FDE.

000330 COPY DBTRAN.FDE.

000310 COPY DEBTOR.FDE.

000140 COPY DEBTRN.GFD.

000140 COPY DEBMEM.FDE.

000140 COPY DEPART.FDE.

       COPY LEDTRF.FDE.

000290 COPY PARAM.FDE.

000220 COPY RECEIPTS.FDE.

000390 COPY RECOVER.GFD.

       COPY SHARED.FDE.

       COPY TXTRAN.FDE.

000530 FD  CSHDRW    LABEL RECORD OMITTED.
000550 01  CSH-REC.
000560     03  CSH-OPEN        PIC  X(06).
      /
000410 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
000430 01  R-L1.
000460     03  R-DET2          PIC  X(80).
000590 01  CSH-L.
000600     03  CSH-DATA        PIC  X(80).
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
002410 77  WS-AUDKEY       PIC  9(06)    COMP-5.
001670 77  WS-DBTKEY	   PIC	9(06)	 COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
001720 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
       77  WS-MTHEND	   PIC	9(02)	 COMP-5.
       77  WS-CLS-AUD	   PIC	X(01)	 VALUE "Y".
000860 77  WS-TME2          PIC  9(08).
002520 77  WS-PORT	   PIC	9(05) VALUE 259.
      * 1004.
       77  WS-DRAW1	   PIC	9(05) VALUE 260.
000600 77  WS-OPEN	   PIC	X(01) VALUE X"01".
000600 77  WS-CLOSE	   PIC	X(01) VALUE X"00".
001750 77  WS-FIN-RATE	   PIC S9(03)V99 COMP-3.
001760 77  WS-PERIOD       PIC  9(02).
001770 77  WS-NTP          PIC  9(01).
001800 77  WS-PAGE         PIC  9(04)    COMP-5.
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
001810 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
001820 77  WS-OPTION       PIC  X(01).
001820 77  WS-ALLOCATE	   PIC	X(01).
001820 77  WS-CSLIP        PIC  X(01).
       77  WS-BNK-IND	   PIC	X(01).
001820 77  WS-SKIP         PIC  X(01) VALUE "N".
001820 77  WS-TAX          PIC  X(01).
001820 77  WS-TYPE         PIC  X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
001870 77  WS-EKEY         PIC  9(06)    COMP-5.
001880 77  WS-CHAR1        PIC  X(01).
001890 77  WS-CHAR2        PIC  X(01).
001900 77  WS-COMP         PIC  9(01).
001930 77  WS-INDS         PIC  9(01) VALUE ZERO.
001940 77  WS-INDD         PIC  9(01) VALUE ZERO.
001950 77  WS-INDI         PIC  9(01) VALUE ZERO.
001980 77  WS-BATCH	   PIC	9(06) COMP-5.
002010 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002100 77  WS-A            PIC  X(19) VALUE "Any key to continue".
002110 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
002130 77  WS-AC           PIC  X(07) VALUE "ACCOUNT".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
000070 77  WS-DRAW	   PIC	9(02).
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
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
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
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "012".

       COPY LEDTRF.WS.

002250 01  W00-TRAN-CODES.
           03  W00-T-VAT   PIC  9(09)V99 COMP-3.
001410     03  W00-E-DESC  PIC  X(12).
001420     03  W00-A-DESC  PIC  X(12).
001430     03  W00-ACTION  PIC  9(01).
001440     03  W00-T-CODE  PIC  9(02)    COMP-5.
001450     03  W00-T-GL    PIC  9(06)    COMP-3.
001460     03  W00-T-DAY   PIC  9(06)    COMP-5.
001470     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
001480     03  W00-T-MTD   PIC  9(06)    COMP-5.
001490     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
001500     03  W00-T-YTD   PIC  9(06)    COMP-5.
001510     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

003680 COPY AUDIT.ID.

       COPY CHEQUE.ID.

       COPY CONTROL.ID.

       COPY DBTRAN.ID.

       COPY DEBTOR.ID.

000140 COPY DEBTRN.ID.

000140 COPY DEBMEM.ID.

       COPY DEPART.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000220 COPY RECEIPTS.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY TXTRAN.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "DTP012.".
               05  W02-USER   PIC X(05) VALUE SPACES.
           03  W02-CSHDRW     PIC X(04) VALUE "COM1".
               
       COPY W05.VAT.

       01  W05-DISC.
000630	   03  W05-DISCOUNT.
000640         05  W05-DSETT   PIC S9(03)V99 COMP-3 OCCURS 10.

002500 01  W10-SPARES.
002510	   03  W10-DATE        PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).
	   03  W10-REF1 REDEFINES W10-NUM.
	       05  FILLER      PIC  X(07).
	       05  W10-N1.
	       07  W10-NUM1    PIC  9(01).
	   03  W10-REF2 REDEFINES W10-NUM.
	       05  FILLER      PIC  X(06).
	       05  W10-N2.
	       07  W10-NUM2    PIC  9(02).
	   03  W10-REF3 REDEFINES W10-NUM.
	       05  FILLER      PIC  X(05).
	       05  W10-N3.
	       07  W10-NUM3    PIC  9(03).

       COPY W12.WS.

001980 01  W15-CASH-SLIP.
001990     03  W15-L1.
002000         05  W15-ESC1    PIC  X(01).
002010         05  W15-DBLE    PIC  X(01).
002020         05  W15-EXPP1   PIC  X(01).
               05  W15-COMP    PIC  X(32).
002040         05  W15-ECAN1   PIC  X(01).
002050     03  W15-L1A.
002060         05  W15-ESC5    PIC X(01).
002070         05  W15-DBLE3   PIC X(01).
002080         05  W15-EXPP4   PIC X(01).
002090         05  W15-DOCTYP  PIC X(18).    
002100         05  W15-ECAN4   PIC X(01).
002110     03  W15-L2.
002120         05  W15-ESC2    PIC  X(01).
002130         05  W15-DCAN    PIC  X(01).
002140         05  W15-ADD     PIC  X(32) VALUE SPACES.
002160     03  W15-L3.
002170	       05  FILLER      PIC  X(14) VALUE "Telephone  :".
	       05  W15-TEL     PIC  X(14) VALUE SPACES.
002190     03  W15-L4.
002200	       05  FILLER      PIC  X(18) VALUE "Date       :".
002210	       05  W15-DATE    PIC  Z9/99/9999.
002220     03  W15-L5.
002230         05  FILLER      PIC  X(08) VALUE "Assist:".
002240         05  W15-SMAN    PIC  X(24).
002250     03  W15-L6.
002260	       05  FILLER      PIC  X(22) VALUE "Receipt No.:".
002270	       05  W15-RECNO   PIC  X(08).
002280     03  W15-L7.
002290	       05  FILLER      PIC  X(22) VALUE "Invoice No.:".
002300         05  W15-INVNO   PIC  Z(05)9.
002310     03  W15-L7A.
002320	       05  FILLER      PIC  X(22) VALUE "Credit Note:".
002330         05  W15-CRNO    PIC  Z(05)9.
002280     03  W15-L7B.
002290	       05  FILLER      PIC  X(22) VALUE "Reference No.:".
002300         05  W15-REFNO   PIC  X(06).
002340     03  W15-L8.
002350	       05  FILLER      PIC  X(22) VALUE "Account No.:".
002360         05  W15-ACCNO   PIC  X(06).
002370     03  W15-L9.
002380         05  W15-ESC3    PIC  X(01).
002390         05  W15-DBLE2   PIC  X(01).
002400         05  W15-EXPP2   PIC  X(01).
002410	       05  FILLER      PIC  X(16) VALUE "    THANK YOU".
002420         05  W15-ECAN2   PIC  X(01).
002490     03  W15-L11.
002500         05  W15-COND1   PIC  X(01).
002510         05  W15-ITM     PIC  X(12).
002520         05  FILLER      PIC  X(01) VALUE SPACE.
002530         05  W15-QUANT   PIC  Z9.99.
002540         05  FILLER      PIC  X(01) VALUE SPACE.
002550         05  W15-DESC    PIC  X(24).
002560         05  FILLER      PIC  X(01) VALUE SPACE.
002570         05  W15-PRICE   PIC  Z(06)9.99-.
002580         05  FILLER      PIC  X(01) VALUE SPACE.
002590         05  W15-NORM1   PIC  X(01).
002600     03  W15-L12 REDEFINES W15-L11.
002610         05  FILLER      PIC  X(01).
002620         05  W15-DISCOUNT.
002630             07 FILLER   PIC  X(19).
002640             07  W15-DISC
002650                         PIC  X(19).
002660             07  W15-DSC PIC  Z9.99.
002670         05  FILLER      PIC  X(14).
002680     03  W15-L13.
002690         05  W15-REMK    PIC X(21).
002700         05  W15-VAL     PIC Z(06)9.99-.

       COPY W20.WS.

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.

       COPY W40.DBT.

       COPY W40.WS.

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).
003910     03  W45-ENG         PIC X(12).
003920     03  W45-AFR         PIC X(12).
003930     03  W45-ACT         PIC X(01).

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
           03  W75-S          PIC  9(02)    COMP-5.
           03  W75-S1         PIC  9(02)    COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC  X(06) OCCURS 18.

003750 01  W75-TITLES.
003760     03  W75-DESCRIP.
003770         05  FILLER      PIC X(28)      VALUE
003780                     "Mr     Mnr    Mrs    Mev    ".
003790         05  FILLER      PIC X(28)      VALUE
003800                     "Miss   Mej    Ms     Ms     ".
003810         05  FILLER      PIC X(28)      VALUE
003820                     "Dr     Dr     Rev    Ds     ".
003830         05  FILLER      PIC X(28)      VALUE
003840                     "Prof   Prof   The HonSy Edel".
003850         05  FILLER      PIC X(28)      VALUE
003860                     "                            ".
003870     03  W75-CODES REDEFINES W75-DESCRIP.
003880         05  W75-TITLE  OCCURS 10.
003890             07  W75-ETITLE  PIC X(07).
003900             07  W75-ATITLE  PIC X(07).
003910 01  W80-ACCOUNT.
003920     03  W80-ACNO        PIC X(06).
003940     03  W80-TYPE        PIC 9(01).

       COPY W85.WS.

003960 01  W85-BATCHES.
003970         05  W85-BATCH   PIC  9(06)    COMP-5.

       COPY W90.DBT.

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).
004410     03  W95-V1.
004420         05  W95-T       PIC Z(04)9.99.
004430     03  W95-V2.
004440	       05  W95-B       PIC Z(08)9.99-.
004450     03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.
004470     03  W95-POST        PIC 9(04).
004480     03  W95-TEL         PIC X(08).

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
004500     03  W100-BAL        PIC Z(08)9.99-.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
004560     03  W100-CRL        PIC Z(06)9.  
004570     03  W100-KEY        PIC Z(05)9.
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
004580 SCREEN SECTION.

       COPY BLANK.CRT.

005090 01  S08.
005100	   03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "RECEIPT REVERSAL".
008480	   03  LINE  4 COLUMN  6 VALUE "Account No.".
005140     03          COLUMN 33 VALUE "Name:".
	   03  LINE  5 COLUMN  6 VALUE "Date       ".
           03          COLUMN 33 VALUE "Bal :".
008470	   03  LINE  6 COLUMN  6 VALUE "Receipt No.".
008490	   03  LINE  7 COLUMN  6 VALUE "Amount     ".
008500	   03  LINE  9 COLUMN  6 VALUE "Cash       ".
008510	   03  LINE 10 COLUMN  6 VALUE "Cheque     ".
	   03  LINE 11 COLUMN  6 VALUE "Bank Trf.  ".
008570     03  LINE 13 COLUMN  6 VALUE "TOTAL  :".

005580 01  S10.
	   03  LINE 10 COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 73 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "¿".
005600	   03  LINE 11 COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ Mtd  : ".
028580	   03	       COLUMN 59 BACKGROUND-COLOR 7 FOREGROUND-COLOR 5
				 PIC Z(08)9.99- FROM W40-ONE.
	   03	       COLUMN 72 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE " ³".
005620	   03  LINE 12 COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ Cur  : ".
028580	   03	       COLUMN 59 BACKGROUND-COLOR 7 FOREGROUND-COLOR 5
				 PIC Z(08)9.99- FROM DEB-CUR.
	   03	       COLUMN 72 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE " ³".
005630	   03  LINE 13 COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ 30d  : ".
028580	   03	       COLUMN 59 BACKGROUND-COLOR 7 FOREGROUND-COLOR 5
				 PIC Z(08)9.99- FROM DEB-30.
	   03	       COLUMN 72 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE " ³".
005640	   03  LINE 14 COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ 60d  : ".
028580	   03	       COLUMN 59 BACKGROUND-COLOR 7 FOREGROUND-COLOR 5
				 PIC Z(08)9.99- FROM DEB-60.
	   03	       COLUMN 72 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE " ³".
005650	   03  LINE 15 COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ 90d  : ".
028580	   03	       COLUMN 59 BACKGROUND-COLOR 7 FOREGROUND-COLOR 5
				 PIC Z(08)9.99- FROM DEB-90.
	   03	       COLUMN 72 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE " ³".
005650	   03  LINE 16 COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ 120d : ".
028580	   03	       COLUMN 59 BACKGROUND-COLOR 7 FOREGROUND-COLOR 5
				 PIC Z(08)9.99- FROM DEB-120.
	   03	       COLUMN 72 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE " ³".
005660	   03  LINE 17 COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "³ Int  : ".
028580	   03	       COLUMN 59 BACKGROUND-COLOR 7 FOREGROUND-COLOR 5
				 PIC Z(08)9.99- FROM DEB-INT.
	   03	       COLUMN 72 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE " ³".
	   03  LINE 18 COLUMN 50 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 51 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
				 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

005580 01  S12.
	   03  LINE  9 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»".
	   03  LINE 10 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "º    BALANCE FORWARD ALLOCATIONS    º".
	   03  LINE 11 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶".
005600	   03  LINE 12 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "º Mtd  : ".
028580	   03	       COLUMN 20 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99- FROM W40-ONE.
	   03	       COLUMN 32 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "               º".
005620	   03  LINE 13 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "º Cur  : ".
028580	   03	       COLUMN 20 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99- FROM DEB-CUR.
	   03	       COLUMN 32 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "               º".
005630	   03  LINE 14 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "º 30d  : ".
028580	   03	       COLUMN 20 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99- FROM DEB-30.
	   03	       COLUMN 32 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "               º".
005640	   03  LINE 15 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "º 60d  : ".
028580	   03	       COLUMN 20 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99- FROM DEB-60.
	   03	       COLUMN 32 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "               º".
005650	   03  LINE 16 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "º 90d  : ".
028580	   03	       COLUMN 20 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99- FROM DEB-90.
	   03	       COLUMN 32 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "               º".
005650	   03  LINE 17 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "º 120d : ".
028580	   03	       COLUMN 20 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99- FROM DEB-120.
	   03	       COLUMN 32 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "               º".
005660	   03  LINE 18 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "º Int  : ".
028580	   03	       COLUMN 20 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99- FROM DEB-INT.
	   03	       COLUMN 32 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "               º".
	   03  LINE 19 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "º ".
	   03	       COLUMN 13 BACKGROUND-COLOR 7 FOREGROUND-COLOR 1
		       VALUE "Transaction balance:".
	   03	       COLUMN 33 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "		    º".
	   03  LINE 20 COLUMN 11 BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
		       VALUE "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼".

       COPY DEBLUP.CRT.

       01  S18.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 99/99/9999 USING W90-DATE AUTO.

       01  S18R.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC  X(08) USING W90-REF AUTO.

007810 01  S19.
007820	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99 USING W90-DB AUTO.
007810 01  S20.
007820	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99 USING W90-CASH AUTO.
007810 01  S21.
007820	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99 USING W90-CHQ AUTO.
007810 01  S21TRF.
007820	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99 USING W90-CARD AUTO.
       01  S22.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(24) USING W40-CONTACT AUTO.
       01  S23.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14) USING W40-TEL AUTO.
       01  S24.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC 99/99/9999 USING W90-IDTE AUTO.
007810 01  S29.
007820	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-MTD AUTO.
007830 01  S30.
007840	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-CUR AUTO.
007850 01  S31.
007860	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-30 AUTO.
007870 01  S32.
007880	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-60 AUTO.
007890 01  S33.
007900	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-90 AUTO.
007890 01  S34.
007900	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08)9.99- USING W40-120 AUTO.
007910 01  S35.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(06)9.99- USING W40-INT AUTO.
007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.
007910 01  S37.
           03  LINE 22 COLUMN 6 VALUE "Total Receipts (Batch): ".
007920	   03  FOREGROUND-COLOR 3 HIGHLIGHT
				  PIC Z(08)9.99- USING W25-TOTAL AUTO.

       01  S-ALLOC.
	   03  LINE 21 COLUMN 14 VALUE "Un-allocated balance : ".
	   03	       COLUMN 37 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08)9.99 FROM W75-BALANCE.
	   03	       COLUMN 51 VALUE "Allocated : ".
	   03	       COLUMN 63 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC Z(08)9.99 FROM W75-ALLOCATED.

       01  S-DTRN.
	   03  COLUMN  6 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC X(08) FROM DBT-REF.
	   03  COLUMN 15 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				   PIC 9999/99/99 FROM DBT-DATE.
	   03  COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC X(12) FROM W00-E-DESC.
	   03  COLUMN 39 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 FROM DBT-VALUE.
	   03  COLUMN 52 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 FROM DBT-OUTSTD.
	   03  COLUMN 65 FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
                                   PIC Z(08)9.99 USING DBT-ALLOC AUTO.

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

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
008230 AA000		       SECTION.
008240 AA00.
003240	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA05.
008250	     PERFORM ZA000-INIT.
	     MOVE ZERO		 TO W25-TOTAL.
             PERFORM DA000.
043610	     CLOSE CHEQUE
		   RECEIPTS
043650		   RECOVER.
	   IF WS-CLS-AUD = "Y"
	       CLOSE AUDIT.
       AA05.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM DEBTOR-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.
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

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY DEBTOR.TRN.

      *    *************************************************************
      *    ****        D A T E	 C H E C K   R O U T I N E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Perform this routine with the date in W10-DATE (PIC      ³
      *    ³  99/99/9999) with the format as DD/MM/CCYY 	       ³
      *    ³							       ³
      *    ³  On exiting - WS-ERROR will be zero (0) if the date is    ³
      *    ³  correct or one (1) if the date is incorrect.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY DATE.CHK.

       COPY LEDTRF.UPD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
008680 AC000		  SECTION.

011130 COPY AUDIT.RD.

       COPY CHEQUE.RD.

       COPY CONTROL.RD.

       COPY DBTRAN.RD1.

       COPY DEBTOR.RD.

000140 COPY DEBTRN.RD.

000140 COPY DEBMEM.RD.

       COPY DEPART.RD.

       COPY LEDTRF.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
010170 AD000		 SECTION.

011370 COPY AUDIT.WR.

       COPY CHEQUE.WR.

       COPY CONTROL.WR.

       COPY DBTRAN.WR.

       COPY DEBTOR.WR.

000140 COPY DEBTRN.WR.

000140 COPY DEBMEM.WR.

       COPY DEPART.WR.

       COPY LEDTRF.WR.

       COPY PARAM.WR.

       COPY RECEIPTS.WR.

       COPY TXTRAN.WR.

011590 COPY CASHDRAW.OPN.

       COPY DEBTOR.LUP.

       COPY APAC.HLP.

      /    *************************************************************
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    *************************************************************
010910 AY000           SECTION.
010920 AY01.
010930       MOVE 01             TO REC-FILE.
010940       MOVE WS-PARKEY      TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
010950       MOVE PAR-RECORD1    TO REC-PARAM.
010960       GO TO AY50.
010970 AY02.
010980       MOVE 02             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011000       MOVE DEB-RECORD1    TO REC-DEBTOR.
011010       GO TO AY50.
011070 AY04.
011080       MOVE 04             TO REC-FILE.
011090	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
011100	     MOVE TRA-RECORD1	 TO REC-DBTRAN.
011110       GO TO AY50.

011920 AY16.
011930       MOVE 16             TO REC-FILE.
011940       MOVE WS-AUDKEY      TO REC-KEY.
011950       MOVE AUD-REC1       TO REC-AUDITF.
011960       GO TO AY50.

       AY19.
             MOVE 19             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TAX-RECORD1    TO REC-TXTRAN.
             GO TO AY50.

       AY36.
             MOVE 36             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE CHQ-RECORD1    TO REC-CHEQUE.
             GO TO AY50.

       AY38.
	     MOVE 38		 TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
012120	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "XXXX"	 TO DPT-CODE
	       GO TO AY38.
012130       MOVE DPT-RECORD     TO REC-DEPART.
012140       GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.

011070 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		  End transaction in recovery log	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011120 AY49.
011130       MOVE 99             TO REC-FILE.
011140       MOVE ZERO           TO REC-KEY.
011150       MOVE SPACES         TO REC-DETAIL.
011160       PERFORM AY50 THRU AY59.
011170       ADD 1               TO WS-TRANS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If recovery records exceed 95 - Clear (Close and Open     ³
      *    ³ new). This allows for quicker recoveries when required.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011180	   IF WS-RECOVER > 95
011190         CLOSE RECOVER
011200         OPEN OUTPUT RECOVER
011210         CLOSE RECOVER
011220         OPEN I-O RECOVER
011230         MOVE ZERO         TO WS-RECOVER.
011240       GO TO AY59.

011250 AY50.
011260	     ADD 1		 TO WS-RECOVER.
011270       MOVE WS-RECOVER     TO WS-RECKEY.
011280       MOVE WS-TRANS       TO REC-TRAN.
011290       WRITE REC-RECORD.
011300     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212
011320                  WITH FOREGROUND-COLOR 14 WS-STATUS 
011330                  WITH FOREGROUND-COLOR 15
011340         STOP RUN.
011350       CLOSE RECOVER.
011360       OPEN I-O RECOVER.

011370 AY59.
011380       EXIT.
      *    *************************************************************
      *    ****    Start processing transaction
      *    *************************************************************
012470 AY60.
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
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "DB"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *    *************************************************************
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *    *************************************************************
012010	     MOVE 1		 TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
      *    *************************************************************
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *    *************************************************************
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *    *************************************************************
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *    *************************************************************
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
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
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
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
      *    ****   S E T   W A I T   P E R I O D
      *    *************************************************************
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
      *    *************************************************************
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   NETWORK record 2.
      *    *************************************************************
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       IF WS-USUB < 24
		   ADD 1	 TO WS-USUB
		   GO TO AY72
	       ELSE
		   GO TO AY85.

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

       AY85.
013090	     COMMIT.

011860 AY999.
011870       EXIT.
      *    *************************************************************
      *    ****    Read debtor record - using account number.
      *    *************************************************************
014260 CA155-GET-DEBTOR  SECTION 2.
014270 CA155.
012010       MOVE W80-ACNO       TO DEB-ACNO.
012020       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
014380     ELSE
014390     IF DEB-ACNO NOT = W80-ACNO
014400         MOVE 2            TO WS-ERROR
014410     ELSE
014420         MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.
014430 CA160-EXIT.
014440       EXIT.
      *
014450 CA165-NAME        SECTION 2.
014460 CA165.
014470       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
014480                           INTO W40-SURNAME 
                                      W40-FNAME 
                                      W40-SNAME
014490       OVERFLOW
014500           MOVE 1          TO WS-ERROR.
014510 CA170-EXIT.
014520       EXIT.
      *
014530 CA175-ADDRESS     SECTION 2.
014540 CA175.
014550       UNSTRING W60-ADDRESS DELIMITED BY "*"
014560                           INTO W40-ADD1 
                                      W40-ADD2 
                                      W40-ADD3 
                                      W40-ADD4
014570       OVERFLOW
014580           MOVE 1          TO WS-ERROR.
014590 CA175-EXIT.
014600       EXIT.
      *
014610 CA180-ADDRESS     SECTION 2.
014620 CA180.
014630       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
014640                           INTO W40-PADD1 
                                      W40-PADD2 
                                      W40-PADD3 
                                      W40-PADD4
014650       OVERFLOW
014660           MOVE 1          TO WS-ERROR.
014670 CA180-EXIT.
014680       EXIT.

      /    *************************************************************
      *    ****    G E N E R A T E   A   D E B T O R
      *            T R A N S A C T I O N
      *    *************************************************************
000010 GENERATE-DEBTOR-TRAN SECTION 2.
000020
000030 DEBTOR-TRAN.
000130
000140 LINK-DEBTOR-TRANS.
000230       INITIALIZE TRA-RECORD1.
000310       MOVE W20-DTE        TO TRA-DATE.
000360	     MOVE W90-CODE	 TO TRA-CODE.
000370       MOVE W90-REF        TO TRA-REF TRA-REG.
000380       MOVE DEB-ACNO       TO TRA-AC.
000390       SUBTRACT W90-CR FROM W90-DB 
000400                           GIVING TRA-VALUE.
000660	     PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
000900       MOVE 1              TO WS-ACTION.
000220       PERFORM AY04 THRU AY59.
000920       ADD TRA-VALUE       TO W00-T-VAL W00-T-MTDV W00-T-YTDV.
000930       ADD 1               TO W00-T-DAY W00-T-MTD  W00-T-YTD.
           IF WX-INT = "Y"
               PERFORM DEBTOR-INTEGRATE. 
000950       PERFORM UPDATE-TRAN-RECORD.
000980	     ADD W90-DB 	 TO DEB-DEBT.
000990       ADD W90-DB          TO DEB-BAL.
001150	     GO TO DEBTOR-TRAN-EXIT.

       DEBTOR-INTEGRATE.
             MOVE W20-DTE        TO WX-DATE.
             MOVE W90-CODE       TO WX-LEDG.
             MOVE W00-E-DESC     TO WX-DESC.
             MOVE "- DEBTORS"    TO WX-BOOK.
             MOVE TRA-VALUE      TO WX-VALUE.
             MOVE WX-DEBGL       TO WX-AC.
             MOVE 01             TO WX-TYPE.
             PERFORM GL-TRF-UPDATE.
             MOVE 14             TO WX-TYPE.
	     COMPUTE WX-VALUE = TRA-VALUE * -1.
             MOVE W00-T-GL       TO WX-AC.
             PERFORM GL-TRF-UPDATE.

001690 DEBTOR-TRAN-EXIT.
001700       EXIT.

      /    *************************************************************
      *    ****    PROCESS A DEBTOR RECEIPT REVERSAL
      *    *************************************************************
029510 DA000		       SECTION 50.
029520 DA00.
	     MOVE 1		 TO WS-S9.

       DA02.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO DA02.
029570	     MOVE ZERO		 TO W90-90  W90-120
				    W90-CUR W90-BCR
029580				    W90-60  W90-BDT
029590				    W90-30  W90-INT
				    W90-EX  W90-VAL
				    W90-TX  W90-DISCOUNT.
	     MOVE SPACES	 TO W90-REF.

029600 DA05.
029610       DISPLAY CLR-SCREEN.
029630       DISPLAY S08.
           IF W25-TOTAL NOT = ZERO
               DISPLAY S37.
029640       MOVE SPACES         TO W80-ACNO.
029660       MOVE 02             TO W90-CODE.
029670       MOVE W85-CSHDTE     TO W90-DATE.
029680       MOVE W12-T-YMD      TO W20-DATE.

029690 DA10.
029700       MOVE ZERO           TO W90-DB W90-CR.

029710 DA15.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Debtor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
029720       ACCEPT S36 AT 0419.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
032540		     CLOSE PRNREP
032550		     GO TO DA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA15
	       END-EVALUATE
               DISPLAY S36 AT 0419
               IF W80-ACNO = SPACES
015990             GO TO DA15.
	     DISPLAY CLEAR-L25.
           IF W80-ACNO = SPACES
029740         GO TO DA75.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
029750	     PERFORM CA155-GET-DEBTOR.
027460	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
029760     IF WS-ERROR = 1
026950	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
029780	       GO TO DA15.
029790     IF WS-ERROR = 2
026980	       MOVE WS-ERR1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
029810	       GO TO DA15.
029920	     MOVE DEB-NAME	 TO W60-NAME.
029930       INSPECT W60-NAME REPLACING ALL "*" BY " ".
029940       INSPECT W60-NAME REPLACING FIRST "!" BY " ".
029950	     DISPLAY W60-NAME AT 0439
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
029960       MOVE DEB-BAL TO W100-BAL.
029990	     DISPLAY W100-BAL AT 0539
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
030150	   IF WS-OPTION = "N"
030160         GO TO DA10.
030240       MOVE DEB-ACNO TO W15-ACCNO.
	     DISPLAY "Esc" AT 2502
		      WITH BACKGROUND-COLOR 3
		      FOREGROUND-COLOR 6 HIGHLIGHT
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      *    *************************************************************
      * 		    R E C E I P T   D A T E
      *    *************************************************************
       DA16.
             ACCEPT S18 AT 0519.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO DA75
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA16
	       END-EVALUATE.
027460	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
             MOVE W90-DATE       TO W10-EDTE W15-DATE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Invalid date"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
               GO TO DA16.

      *    *************************************************************
      * 		  R E C E I P T   N U M B E R
      *    *************************************************************
       DA17.
	     ACCEPT S18R AT 0619.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO DA75
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA17
	       END-EVALUATE.
	    IF W90-REF = SPACES
		MOVE "Invalid Reference"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO DA17.
      *    *************************************************************
      * 		  R E V E R S A L   A M O U N T
      *    *************************************************************
030380 DA20.
             ACCEPT S19 AT 0719.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO DA75
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA20
	       END-EVALUATE.
027460	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
	   IF W90-DB = ZERO
	       MOVE "Amount may not be zero"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO DA20.
       DA22.
032920	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030150	   IF NOT (WS-OPTION = "C" OR "R")
               GO TO DA22.
	   IF WS-OPTION = "R"
               GO TO DA20.               
030400 DA25.
030500	     MOVE ZERO		 TO W90-CASH W90-CHQ
				    W90-CARD W90-PMT
				    W90-CHANGE.
030510	     MOVE W90-DB	 TO W90-DUE.
	     DISPLAY "Amount due " AT 0806.
030520 DA30.
030530       COMPUTE W90-CRD = W90-DUE - W90-PMT.
030540       MOVE W90-CRD        TO W100-BAL.
030550	     DISPLAY W100-BAL AT 0819
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
             MOVE W90-PMT        TO W100-BAL.
030550	     DISPLAY W100-BAL AT 1319
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *    *************************************************************
      * 		    C A S H   T E N D E R E D
      *    *************************************************************
030560 DA35.
030570       ACCEPT S20 AT 0919.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO DA75
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA35
	       END-EVALUATE.
030580	     MOVE W90-CASH	 TO W90-PMT.
030590     IF W90-PMT NOT < W90-DUE
030600         GO TO DA45.
030610       PERFORM DA30.
      *    *************************************************************
      *      C H E Q U E   T E N D E R E D
      *    *************************************************************
030620 DA40.
030630       ACCEPT S21 AT 1019.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO DA75
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA40
	       END-EVALUATE.
030640	     ADD W90-CHQ	 TO W90-PMT.
030650     IF W90-PMT NOT < W90-DUE
030660         GO TO DA45.
030670       PERFORM DA30.
      *    *************************************************************
      *      B A N K   T R A N S F E R
      *    *************************************************************
030620 DA42.
030630	     ACCEPT S21TRF AT 1119.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO DA75
		 WHEN OTHER
		     CALL X"E5"
		     GO TO DA40
	       END-EVALUATE.
030640	     ADD W90-CARD	 TO W90-PMT.
030650	   IF W90-PMT = W90-DUE
030660         GO TO DA45.
030670       PERFORM DA30.
	     GO TO DA55.

030720 DA45.
             PERFORM DA30.
024810	     MOVE "'C'ontinue or 'R'ekey reversal  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.

030750 DA50.
	     PERFORM OPT-MESSAGE.
030150	   IF NOT (WS-OPTION = "C" OR "R")
	       GO TO DA50.
	   IF WS-OPTION = "R"
030820         GO TO DA25.           
       DA54.
025490	     PERFORM AY60 THRU AY999.
	     MOVE ZERO		 TO WS-ACTION.
025500	     PERFORM AY02 THRU AY59.
030830	     GO TO DA58.

030840 DA55.
030850	     MOVE "Short paid - REKEY 'P'ayment or 'R'eversal amount"
				 TO WS-ERR-MES.
             MOVE "P"            TO WS-OPTION.

030870 DA56.
030880	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
030900	   IF NOT (WS-OPTION = "P" OR "R")
030910	       GO TO DA55.
030930     IF WS-OPTION = "P"
030940         MOVE ZERO TO W90-PMT
030960         GO TO DA25.
030970	     GO TO DA20.

031040 DA58.
	     MOVE "Print a reversal Y/N?  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
030900	   IF NOT (WS-OPTION = "Y" OR "N")
	       GO TO DA58.
	     MOVE WS-OPTION	 TO WS-CSLIP.
	     MOVE 02		 TO W90-CODE.
             PERFORM GET-DEBTOR-TRAN THRU GET-DEBTOR-TRAN-EXIT.
             PERFORM AY01 THRU AY59.
028260	     PERFORM GENERATE-DEBTOR-TRAN.
      *    *************************************************************
      *    ****    S A V E   P A Y M E N T   V A L U E
      *    *************************************************************
	     MOVE W90-DB	 TO W90-PMT.

       DA65B.
             ADD 121 DEB-CAT     GIVING W90-RESULT.
             DIVIDE W90-RESULT BY 2
                                 GIVING W90-CATKEY
                                 REMAINDER W90-CS.
             ADD 1               TO W90-CS.
040190       MOVE W90-CATKEY     TO WS-PARKEY.
	     ADD W90-PMT	 TO W25-TOTAL.
             PERFORM AY01 THRU AY59.
028320	     ADD W90-DB		 TO PAR-CDRMTD (W90-CS)
028330                              PAR-CDRYTD (W90-CS)
028360	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
031120	     MOVE DEB-NAME	 TO W60-NAME.
	   IF DEB-TYPE NOT = 1
	       GO TO DA70E.
031130       PERFORM CA165-NAME.
031170	     MOVE W75-ETITLE (DEB-TITLE)
				 TO W60-NAME.
031180	     MOVE 7		 TO WS-S6.

031190 DA70A.
031200     IF W60-NCHAR (WS-S6) NOT = SPACE
031210         GO TO DA70B.
031220	     SUBTRACT 1 	 FROM WS-S6.
031230     IF WS-S6 > 0
031240         GO TO DA70A.

031250 DA70B.
031260	     MOVE 1		 TO WS-S5.
031270	     ADD 2		 TO WS-S6.
031280       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S6).
031290	     ADD 1		 TO WS-S6.
031300	     MOVE "."		 TO W60-NCHAR (WS-S6).
031310     IF W40-NCHAR (49) = SPACE
031320         GO TO DA70C.
031330	     ADD 1		 TO WS-S6.
031340       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S6).
031350	     ADD 1		 TO WS-S6.
031360	     MOVE "."		 TO W60-NCHAR (WS-S6).

031370 DA70C.
031380	     ADD 2		 TO WS-S6.

031390 DA70D.
031400	     MOVE W40-NCHAR (WS-S5)
				 TO W60-NCHAR (WS-S6).
031410     IF WS-S5 < 24
031420	       ADD 1		 TO WS-S5
031430	       ADD 1		 TO WS-S6
031440         GO TO DA70D.

       DA70E.
031450	     MOVE W60-NAME	 TO W40-NAME.
	   IF WS-CSLIP = "N"
               GO TO DA73.
	     MOVE W15-COND1	 TO CSH-DATA.
031480       WRITE CSH-L.
031460	     MOVE "RECEIPT REVERSAL"
				 TO W15-DOCTYP.
031470	     MOVE W15-L1A	 TO CSH-DATA.
031480       WRITE CSH-L.
031490	     MOVE W15-L1	 TO CSH-DATA.
031500       WRITE CSH-L.
031510	     MOVE W15-L2	 TO CSH-DATA.
031520       WRITE CSH-L.
031530	     MOVE W15-L3	 TO CSH-DATA.
031540       WRITE CSH-L.
031550	     MOVE W15-L4	 TO CSH-DATA.
031560       WRITE CSH-L.
	     MOVE W90-REF	 TO W15-RECNO.
031570	     MOVE W15-L6	 TO CSH-DATA.
031580       WRITE CSH-L.
031590	     MOVE W15-L8	 TO CSH-DATA.
031600       WRITE CSH-L.
031610	     MOVE W40-NAME	 TO CSH-DATA.
031620       WRITE CSH-L.
031630	     MOVE SPACE 	 TO CSH-DATA.
031640       WRITE CSH-L.
031650	     MOVE "Payment reversed"
				 TO W15-REMK.
031660	     MOVE W90-PMT	 TO W15-VAL.
031670	     MOVE W15-L13	 TO CSH-DATA.
031680       WRITE CSH-L.
031650	     MOVE "Outstanding Balance"
				 TO W15-REMK.
031660	     MOVE DEB-BAL	 TO W15-VAL.
031670	     MOVE W15-L13	 TO CSH-DATA.
031680       WRITE CSH-L.
031690	     MOVE SPACE 	 TO CSH-DATA.
031700       WRITE CSH-L.
031710     IF W90-CASH > ZERO
031720	       MOVE "CASH / KONTANT   :"
				 TO W15-REMK
031730	       MOVE W90-CASH	 TO W15-VAL
031740	       MOVE W15-L13	 TO CSH-DATA
031750         WRITE CSH-L.
031760     IF W90-CHQ > ZERO
031770	       MOVE "CHEQUE / TJEK    :"
				 TO W15-REMK
031780	       MOVE W90-CHQ	 TO W15-VAL
031790	       MOVE W15-L13	 TO CSH-DATA
031800         WRITE CSH-L.
031810     IF W90-CARD > ZERO
031820	       MOVE "BNK,TRF/BNK.OORP :"
				 TO W15-REMK
031830	       MOVE W90-CARD	 TO W15-VAL
031840	       MOVE W15-L13	 TO CSH-DATA
031850         WRITE CSH-L.
031940	     MOVE SPACES	 TO CSH-DATA.
031950       WRITE CSH-L.
031960       MOVE W15-L9         TO CSH-DATA.
031970       WRITE CSH-L.
	     MOVE SPACES	 TO W15-ADD.
	     MOVE W15-L2	 TO CSH-DATA.
031970       WRITE CSH-L.
	     MOVE W15-NORM1	 TO CSH-DATA.
031970       WRITE CSH-L.
032000	     MOVE SPACES	 TO CSH-DATA.
032010	     MOVE 1		 TO WS-S9.
032020 DA72.     
032030       WRITE CSH-L.
032040     IF WS-S9 < W85-ADVANCE
032050         ADD 1             TO WS-S9
032060	       GO TO DA72.

       DA73.
014800       PERFORM OPEN-CASH-DRAWER THRU OPEN-CASH-DRAWER-EXIT.
032070	     PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
015360       ADD 150 WS-DRAW GIVING WS-PARKEY.
015380       PERFORM AY01 THRU AY59.
015390	     SUBTRACT W90-CASH	 FROM PAR-CASH.
015400	     SUBTRACT W90-CHQ	 FROM PAR-CHEQUES.
015400	     SUBTRACT W90-CARD	 FROM PAR-BANKTRF.
032180	     COMPUTE PAR-RECEIPTS = PAR-RECEIPTS - W90-DB.
015470       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
015370       MOVE PAR-RECORD151  TO W85-RECORD151.
032220	     ADD W90-DB 	 TO DEB-OUT.
032230	     ADD W90-DB 	 TO DEB-DT.
      *    *************************************************************
      *    ****    L O G   D E T A I L S   T O	 A U D I T   F I L E
      *    *************************************************************
017220	     MOVE DEB-AUDIT	 TO WS-AUDKEY.
017230       MOVE LOW-VALUES     TO AUD-REC1.
017240       PERFORM AY16 THRU AY59.
017290	     MOVE 11		 TO AUD-TYPE
017300	     MOVE DEB-ACNO	 TO AUD-AC.
017310       MOVE WS-DRAW        TO AUD-SUB.
017320       MOVE W90-REF        TO AUD-REF.
017340	     COMPUTE AUD-VAL = W90-PMT * -1.
	     MOVE W90-CASH	 TO AUD-R-CSH.
	     MOVE W90-CHQ	 TO AUD-R-CHQ.
	     MOVE W90-CARD	 TO AUD-R-TRF.
      *    *************************************************************
      *    ****   L O G   D E T A I L S   T O	R E C E I P T	F I L E
      *    *************************************************************
	     MOVE W22-DTE1	 TO RCT-DATE.
000310	     MOVE DEB-ACNO	 TO RCT-AC.
000320	     MOVE W90-REF	 TO RCT-REF.
	     MOVE ZERO		 TO RCT-REF-EXT.
	     MOVE W40-NAME	 TO RCT-NAME.
	   IF NOT (W90-CASH = ZERO)
	       MOVE "C" 	 TO RCT-TYPE
	   ELSE
	   IF NOT (W90-CHQ = ZERO)
	       MOVE "Q" 	 TO RCT-TYPE
	   ELSE
	       MOVE "T" 	 TO RCT-TYPE.
000340	     MOVE W90-PMT	 TO RCT-VAL.
000340	     MOVE W90-CASH	 TO RCT-CSH.
000340	     MOVE W90-CHQ	 TO RCT-CHQ.
000340	     MOVE W90-CARD	 TO RCT-TRF.
017350	     PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017360	     ADD 1		 TO DEB-AUDIT.
017370       MOVE 1              TO WS-AUDKEY.
017380       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017390       PERFORM AY16 THRU AY59.
017430	     MOVE 1		 TO AUD-REP11.
017440       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
	     PERFORM WRITE-RECEIPTS THRU WRITE-RECEIPTS-EXIT.
031860	     PERFORM AY70 THRU AY999.
032010	     CLOSE AUDIT DEBTOR NETWORK PARAM RECEIPTS.
032020	     OPEN I-O AUDIT DEBTOR NETWORK PARAM RECEIPTS.

032490 DA75.
032500	     MOVE "'ENTER' to continue - 'E' to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
032530	   IF WS-OPTION = "E"
032540         CLOSE PRNREP
032550         GO TO DA999.
032560       GO TO DA00.

032570 DA999.
032580       EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
041980 ZA000-INIT	       SECTION 88.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     MOVE LS-DRAW	 TO WS-DRAW.
042000	     DISPLAY CLR-SCREEN.
           IF WS-DRAW = ZERO
               MOVE 40           TO WS-DRAW.
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
      *    ****    P A R A M E T E R   F I L E
      *    *************************************************************
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
	   IF WS-STAT1 = "9"
	       IF NET-ERROR OR COM-ERROR
		   PERFORM NETWORK-ERROR THRU LOCK-REC-EXIT
		   GO TO ZA00-READ-APACFIDS.
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
      *    ****      C H E Q U E S   R E C E I V E D   F I L E	    ****
      *    *************************************************************
	     MOVE "CHEQUE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CHEQUE.
      *    *************************************************************
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
      *    *************************************************************
      *    ****      T R A N S A C T I O N   W O R K   F I L E	    ****
      *    *************************************************************
	     MOVE "DEBTRN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-DEBTRN.
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
      *    ****       D E B T O R   R E C E I P T S   F I L E	    ****
      *    *************************************************************
	     MOVE "RECEIPTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECEIPTS.
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
      *    *************************************************************
      *    ****       V A L U E	  A D D E D   T A X   F I L E	    ****
      *    *************************************************************
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-BANK-IND	 TO WS-BNK-IND.
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
           IF TRM-STDP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-STDP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN3 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
                                 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
042170 ZA02.
053630	     OPEN I-O AUDIT.
053640     IF RUNTIME-ERROR
053650         IF FLE-LOCKED
053660             GO TO ZA200
053670         ELSE
053680         IF FLE-LIMIT
053690             GO TO ZA49.
053700     IF WS-STATUS NOT = "00"
	       IF WS-STATUS = "41"
		   MOVE "N"	 TO WS-CLS-AUD
	       ELSE
053710		   MOVE 1	 TO WS-F-ERROR
053720		   PERFORM OPEN-ERROR.
040400	     OPEN I-O CHEQUE.
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
040450	       PERFORM OPEN-ERROR.

       ZA03.
040400	     OPEN I-O RECEIPTS.
	   IF WS-STATUS = "05"
	       CLOSE RECEIPTS
	       OPEN OUTPUT RECEIPTS
	       CLOSE RECEIPTS
	       GO TO ZA03.
040410     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
		   GO TO ZA49.
040430	   IF WS-STATUS NOT = "00"
	       OPEN OUTPUT RECEIPTS
	       CLOSE RECEIPTS
	       GO TO ZA03.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
042720       MOVE 1              TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021590     IF PAR-CSHEAD = SPACES
021600         DISPLAY "Setup cash slip headings - press ENTER" AT 1212
021610                  WITH FOREGROUND-COLOR 14
               ACCEPT WS-OPTION AT 1251 WITH AUTO
021620         GO TO ZA205.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-CSHEAD     TO W15-COMP W40-NAME.
021660       MOVE PAR-CSHADD     TO W15-ADD.
021670       MOVE PAR-TELEPHONE  TO W15-TEL.
042520	     MOVE 5		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DISCOUNT   TO W05-DISCOUNT.
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
042520	     MOVE 6		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
021490       ADD 150 WS-DRAW     GIVING WS-PARKEY.
021500       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
021510       MOVE W12-TODAY      TO PAR-CSHDTE
021520       MOVE PAR-RECORD151  TO W85-RECORD151.
021530       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
021680       MOVE 32             TO WS-S1.

021690 ZA05.
021700     IF W40-NCHAR (WS-S1) = SPACE
021710         SUBTRACT 1        FROM WS-S1
021720         GO TO ZA05.
021730     IF WS-S1 > 16
021740         MOVE W85-NUL      TO W15-EXPP1 W15-ECAN1
021750     ELSE
021760         MOVE W85-SO       TO W15-EXPP1
021770         MOVE W85-CSO      TO W15-ECAN1.
021780       MOVE W85-ESC        TO W15-ESC1 W15-ESC2 
021790                              W15-ESC3 W15-ESC5.
021800       MOVE W85-SO         TO W15-EXPP2 W15-EXPP4.
021810       MOVE W85-CSO        TO W15-ECAN2 W15-ECAN4.
021820       MOVE W85-DBL        TO W15-DBLE W15-DBLE2 W15-DBLE3.
021830       MOVE W85-CDBL       TO W15-DCAN.
021840       MOVE W85-SI         TO W15-COND1.
021850       MOVE W85-CSI        TO W15-NORM1.
           IF W85-CPRN = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF W85-CPRN = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF W85-CPRN = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
           IF W85-CPRN = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF W85-CPRN = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF W85-CPRN = 9
	       MOVE LS-USER	 TO W02-USER.
042780       OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
      *    *************************************************************
      *    ****      O L D   S T Y L E	 C A S H   D R A W E R	    ****
      *    *************************************************************
           IF W85-DRAWER  = "F"
               IF W85-PORT = 2
                   MOVE "COM2"   TO W02-CSHDRW
               ELSE
               IF W85-PORT = 3
                   MOVE "COM3"   TO W02-CSHDRW
               ELSE
               IF W85-PORT = 4
                   MOVE "COM4"   TO W02-CSHDRW.
           IF W85-DRAWER  = "F"
               OPEN OUTPUT CSHDRW
               MOVE ALL "A"      TO CSH-OPEN.
      *    *************************************************************
      *    ****   G E T   D E B T O R	M O N T H - E N D   D A Y   ****
      *    *************************************************************
	     MOVE 1		 TO WS-NETKEY.
	     PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	   IF DEB-MTH-END < 21 OR > 31 AND < 99
	       MOVE 99		 TO DEB-MTH-END.
	     MOVE DEB-MTH-END	 TO WS-MTHEND.
044780	     GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

       ZA50.
041850     IF WS-F-ERROR = 36
041860         MOVE "PD CHEQUE file"
				 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.

       ZA51.
             EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

042800 ZA200.
042810       DISPLAY "Files locked - Try later" AT 2312
042820                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
042830       DISPLAY "Press any key" AT 2512 
042840		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
042850	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.

       ZA205.
             EXIT PROGRAM.

042880 ZA999-EXIT.
042890       EXIT.
      /    *************************************************************
      *    ****    I / O   E R R O R   M E S S A G E S
      *    *************************************************************
042900 ZB000-ERROR	     SECTION 89.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.

       I-O-ERROR-EXIT.
             EXIT.
      
041560 DISPLAY-FILE-NAME.
056270	   IF WS-F-ERROR = 1
056280         MOVE W02-AUDITF TO WS-FILE
056290         MOVE WS-AUDKEY TO WS-KEY
056300     ELSE
041610	   IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041720     ELSE
041730     IF WS-F-ERROR = 5
041740         MOVE W02-DBTRAN   TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041760     ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840	   ELSE
041810	   IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 18
041860         MOVE W02-RECOVER TO WS-FILE
041870         MOVE WS-RECKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE DME-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 31
041860         MOVE W02-DEBTRN   TO WS-FILE
041870	       MOVE WS-DBTKEY	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 32
041860         MOVE W02-TXTRAN   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840     ELSE
041850	   IF WS-F-ERROR = 36
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
041840     ELSE
041850	   IF WS-F-ERROR = 90
041860	       MOVE W02-RECEIPTS TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE RCT-REF	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890         MOVE "End of FILE" TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full" TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212 
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
