      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******   **	      *******	  ****	   ******    ******    *
      *   **	**  **	      **    **	 **  **   **	**  **	  **   *
      *   **	**  **	      **    **	**    **       **   **	       *
      *   **	**  **	      *******	**    **     **     ********   *
      *   **	**  **	      **	**    **   **	    **	  **   *
      *   **	**  **	      **	 **  **   **	    **	  **   *
      *   *******   ********  **	  ****	  ********   ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    V E H I C L E   S T O C K   R E P O R T   P R O G R A M     *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DLP026.
000030 AUTHOR.	       J.W.LEMMON.
000040 DATE-WRITTEN.   APRIL 1996.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2010
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

       COPY DEPART.SL.

000120 COPY PARAM.SL.

000180 COPY SPARTS.SL.

000140 COPY STOCK.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

000200	   SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.
      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

000350 COPY SPARTS.FDE.

000310 COPY STOCK.FDE.

000280 COPY WARHSE.FDE.

000280 COPY WSTOCK.FDE.

      /
000370 FD  PRNREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
000390 01  REP-LINE1.
000400     03  REP-DETAIL1     PIC X(140).
000410 01  REP-LINE2.
000420     03  REP-DETAIL2     PIC X(80).
000430 01  REP-LINE3.
000490     03  REP-DATE        PIC X(06).
000500     03  REP-DTE         PIC Z9/99/99.
000460     03  FILLER          PIC X(05).
           03  REP-PRC-HD.
               05  FILLER      PIC  X(01).
000470         05  REP-COMPANY PIC  X(40).
               05  FILLER      PIC  X(02).
000480     03  FILLER          PIC X(08).
000440     03  REP-PAGE        PIC X(06).
000450     03  REP-P-NO        PIC Z(03)9.
000510 01  REP-LINE4.
000520     03  FILLER          PIC X(30).
000530     03  REP-HEAD1       PIC X(20).
000540     03  FILLER          PIC X(30).
000550 01  REP-LINE5.
000560     03  FILLER          PIC X(09).
000570     03  REP-DSC         PIC X(30).
000580     03  FILLER          PIC X(93).
000590 01  REP-LINE6.
000600     03  REP-HEAD2       PIC X(10).
000610     03  REP-CNO         PIC 9(03).
000620     03  REP-CATNO REDEFINES REP-CNO PIC 9(03).
000630     03  FILLER          PIC X(01).
000640     03  REP-CAT         PIC X(30).
000650     03  FILLER          PIC X(36).
000850 01  REP-LINE9.
000860     03  REP-PG          PIC X(06).
000870     03  REP-P-N         PIC Z(03)9.
000880     03  FILLER          PIC X(22).
000890     03  REP-COMP        PIC X(40).
000900     03  REP-REPORT      PIC X(34).
000910     03  FILLER          PIC X(12).
000920     03  REP-DAT         PIC X(06).
000930     03  REP-DT          PIC Z9/99/99.
000940 01  REP-LINE9A.
000950     03  REP-HEAD6       PIC X(13).
000960     03  REP-HEAD6A      PIC X(04).
000970     03  REP-HEAD7       PIC X(25).
000980     03  REP-HEAD8       PIC X(12).
000990     03  REP-HEAD9       PIC X(08).
001000     03  REP-HEAD10      PIC X(08).
001010     03  REP-HEAD11      PIC X(09).
001020     03  REP-HEAD12      PIC X(09).
001030     03  REP-HEAD14      PIC X(07).
001040     03  REP-HEAD15      PIC X(10).
001050     03  REP-HEAD16      PIC X(07).
001060     03  REP-HEAD17      PIC X(08).
001070     03  REP-HEAD18      PIC X(12).
001080 01  REP-LINE9B.
001090     03  S-H1            PIC X(15).
001100     03  S-H2            PIC X(28).
001110     03  S-L1            PIC X(01).
001120     03  S-H3            PIC X(03).
001130     03  S-L2            PIC X(01).
001140     03  S-H4            PIC X(07).
001150     03  S-L3            PIC X(01).
001160     03  S-H5            PIC X(07).
001170     03  S-L4            PIC X(01).
001180     03  S-H6            PIC X(07).
001190     03  S-L5            PIC X(01).
001200     03  S-H7            PIC X(07).
001210     03  S-L6            PIC X(01).
001220 01  REP-LINE9C.
001230     03  S-CDE           PIC X(14).
001240     03  FILLER          PIC X(01).
001250     03  S-DSC           PIC X(28).
001260     03  FILLER          PIC X(01).
001270     03  S-BN            PIC X(03).
001280     03  FILLER          PIC X(01).
001290     03  S-QNT           PIC Z(04)9.9.
           03  S-QNTN REDEFINES S-QNT
                               PIC -(04)9.9.
001300     03  FILLER          PIC X(25).
001570 01  REP-LINE11.
001580     03  FILLER          PIC X(13).
001590     03  REP-HEAD13      PIC X(26).
001600     03  FILLER          PIC X(04).
001610     03  REP-DCOST       PIC Z(06)9.99.
001620     03  FILLER          PIC X(02).
001630     03  REP-DSELL       PIC Z(06)9.99.
001640     03  FILLER          PIC X(15).
001650 01  INV-L1.
001660     03  INV-H1          PIC X(15).
001670     03  INV-H2          PIC X(03).
001680     03  INV-H3          PIC X(21).
001690     03  INV-H4          PIC X(09).
001700     03  INV-H5          PIC X(09).
001710     03  INV-H6          PIC X(09).
001720     03  INV-H7          PIC X(12).
001730     03  INV-H8          PIC X(09).
001740     03  INV-H9          PIC X(15).
001750     03  INV-H10         PIC X(09).
001760     03  INV-H11         PIC X(06).
001770     03  INV-H12         PIC X(14).
001780 01  INV-L2.
001790     03  INV-ITM         PIC X(14).
001800     03  FILLER          PIC X(01).
001810     03  INV-BIN         PIC X(03).
001820     03  FILLER          PIC X(01).
001830     03  INV-DESC        PIC X(20).
001840     03  FILLER          PIC X(01).
001850     03  INV-QNT         PIC ZZZZ9.99-.
001860	   03  INV-COST        PIC ZZZZ9.999.
001870*     03	FILLER		PIC X(01).
001880	   03  INV-AVRG        PIC ZZZZ9.999.
001890*     03	FILLER		PIC X(01).
001900     03  INV-TCOST       PIC Z(06)9.99.
001910     03  FILLER          PIC X(01).
001920     03  INV-SELL        PIC ZZZZ9.99.
001930     03  FILLER          PIC X(01).
001940     03  INV-TSELL       PIC Z(06)9.99.
001950     03  FILLER          PIC X(01).
001960     03  INV-PROF        PIC Z(06)9.99.
001970     03  FILLER          PIC X(01).
001980     03  INV-GP          PIC Z(02)9.99.
001990     03  FILLER          PIC X(01).
002000     03  INV-XREF        PIC X(14).
002010 01  MOV-LINE1.
002020     03  MOV-H1          PIC X(15).
002030     03  MOV-H2          PIC X(04).
002040     03  MOV-H3          PIC X(23).
002050     03  MOV-MONTH.
002060         05  MOV-H4      PIC X(16).
002070         05  MOV-H5      PIC X(08).
002080         05  MOV-H6      PIC X(16).
002090     03  MOV-RMNTH REDEFINES MOV-MONTH.
002100         05  MOV-H7      PIC X(09).
002110         05  MOV-H8      PIC X(11).
002120         05  MOV-H9      PIC X(14).
002130         05  MOV-H10     PIC X(06).
002140     03  FILLER          PIC X(02).
002150     03  MOV-YEAR.
002160         05  MOV-H11     PIC X(20).
002170         05  MOV-H12     PIC X(08).
002180         05  MOV-H13     PIC X(20).
002190     03  MOV-RYR REDEFINES MOV-YEAR.
002200         05  MOV-H14     PIC X(09).
002210         05  MOV-H15     PIC X(11).
002220         05  MOV-H16     PIC X(15).
002230         05  MOV-H17     PIC X(07).
002240         05  MOV-H18     PIC X(06).
002250 01  MOV-LINE2.
002260     03  MOV-ITEM        PIC X(14).
002270     03  FILLER          PIC X(01).
002280     03  MOV-BIN         PIC X(03).
002290     03  FILLER          PIC X(01).
002300     03  MOV-DESC        PIC X(22).
002310     03  FILLER          PIC X(01).
002320     03  MOV-MQTY        PIC Z(04)9.99.
002330     03  FILLER          PIC X(01).
002340     03  MOV-MCOST       PIC Z(06)9.99.
002350     03  FILLER          PIC X(01).
002360     03  MOV-MSELL       PIC Z(06)9.99.
002370     03  FILLER          PIC X(01).
002380     03  MOV-MPROF       PIC Z(05)9.99.
002390     03  FILLER          PIC X(01).
002400     03  MOV-YQTY        PIC Z(05)9.99.
002410     03  FILLER          PIC X(01).
002420     03  MOV-YCOST       PIC Z(06)9.99.
002430     03  FILLER          PIC X(01).
002440     03  MOV-YSELL       PIC Z(06)9.99.
002450     03  FILLER          PIC X(01).
002460     03  MOV-YPROF       PIC Z(06)9.99.
002470     03  FILLER          PIC X(01).
002480     03  MOV-GP          PIC Z(02)9.99.
002490 01  ORD-LINE1.
002500     03  ORD-H1          PIC X(15).
002510     03  ORD-H2          PIC X(04).
002520     03  ORD-H3          PIC X(23).
002530     03  ORD-H4          PIC X(11).
002540     03  ORD-H5          PIC X(09).
002550     03  ORD-MOVE.
002560         05  ORD-H6      PIC X(22).
002570         05  ORD-H7      PIC X(10).
002580         05  ORD-H8      PIC X(22).
002590         05  FILLER      PIC X(01).
002600         05  ORD-H9      PIC X(15).
002610     03  ORD-MOV REDEFINES ORD-MOVE.
002620         05  ORD-H10     PIC X(09).
002630         05  ORD-H11     PIC X(09).
002640         05  ORD-H12     PIC X(08).
002650         05  ORD-H13     PIC X(11).
002660         05  ORD-H14     PIC X(11).
002670         05  ORD-H15     PIC X(07).
002680         05  ORD-H16     PIC X(11).
002690         05  ORD-H17     PIC X(04).
002700 01  ORD-LINE2.
002710     03  ORD-ITM         PIC X(14).
002720     03  FILLER          PIC X(01).
002730     03  ORD-BIN         PIC X(03).
002740     03  FILLER          PIC X(01).
002750     03  ORD-DESC        PIC X(22).
002760     03  FILLER          PIC X(01).
002770     03  ORD-QUANT       PIC Z(04)9.99-.
002780     03  ORD-REORD       PIC Z(04)9.99.
002790     03  FILLER          PIC X(01).
002800     03  ORD-MTH1        PIC Z(04)9.99.
002810     03  FILLER          PIC X(01).
002820     03  ORD-MTH2        PIC Z(04)9.99.
002830     03  FILLER          PIC X(01).
002840     03  ORD-MTH3        PIC Z(04)9.99.
002850     03  FILLER          PIC X(01).
002860     03  ORD-MAVG        PIC Z(04)9.99.
002870     03  FILLER          PIC X(01).
002880     03  ORD-MTD         PIC Z(04)9.99.
002890     03  FILLER          PIC X(01).
002900     03  ORD-YTD         PIC Z(04)9.99.
002910     03  FILLER          PIC X(01).
002920     03  ORD-ORDER       PIC Z(04)9.99.
002930     03  FILLER          PIC X(01).
002940     03  ORD-COST        PIC Z(05)9.99.
       01  PRD-LINE1.
           03  PRD-H1          PIC  X(16).
           03  PRD-H2          PIC  X(32).
           03  PRD-H2A         PIC  X(32).
       01  PRD-LINE2.
           03  FILLER          PIC  X(05).
           03  PRD-H3          PIC  X(09).
           03  PRD-H4          PIC  X(15).
           03  PRD-H5          PIC  X(31).
           03  PRD-H6          PIC  X(10).
           03  PRD-H7          PIC  X(10).
       01  PRD-LINE3.
           03  PRD-ITM         PIC  X(14).
           03  FILLER          PIC  X(02).
           03  PRD-DESC        PIC  X(30).
           03  FILLER          PIC  X(02).
           03  PRD-QUANT       PIC  ZZZZ9.
           03  FILLER          PIC  X(29).
       01  PRD-LINE4.
           03  FILLER          PIC  X(05).
           03  PRD-QNT         PIC  ZZZZ9.99.
           03  FILLER          PIC  X(01).
           03  PRD-ITM2        PIC  X(14).
           03  FILLER          PIC  X(01).
           03  PRD-DESC2       PIC  X(30).
           03  FILLER          PIC  X(01).
           03  PRD-SELL        PIC  ZZZZ9.99.
           03  FILLER          PIC  X(01).
           03  PRD-TOTAL       PIC  Z(06)9.99.
       01  QTY-L1.
           03  FILLER          PIC  X(03).
           03  QTY-H1          PIC  X(18).
           03  QTY-H2          PIC  X(35).
           03  QTY-H3          PIC  X(14).
           03  QTY-H4          PIC  X(10).
       01  QTY-L2.
           03  FILLER          PIC  X(03).
           03  QTY-ITM         PIC  X(14).
           03  FILLER          PIC  X(04).
           03  QTY-DESC        PIC  X(30).
           03  FILLER          PIC  X(04).
           03  QTY-QUANT       PIC  Z(05)9.99-.
           03  FILLER          PIC  X(03).
           03  QTY-PRC.
               05  QTY-PRICE   PIC  Z(05)9.99.
           03  QTY-DTE REDEFINES QTY-PRC.
               05  QTY-DATE    PIC  Z9/99/99.
               05  FILLER      PIC  X(01).
           03  FILLER          PIC  X(03).
       01  LAB-L1.
           03  LAB-HEAD        PIC  X(06).
           03  LAB-DESC        PIC  X(74).
       01  LAB-L2.
           03  FILLER          PIC  X(03).
           03  LAB-ITEM        PIC  X(14).
           03  FILLER          PIC  X(63).
       01  LAB-L3.
           03  FILLER          PIC  X(06).
           03  LAB-COST        PIC  X(07).
           03  FILLER          PIC  X(67).
       01  LAB-L4.
           03  FILLER          PIC  X(03).
           03  LAB-SELL        PIC  Z(04)9.99 BLANK WHEN ZERO.
           03  FILLER          PIC  X(69).
       01  LB2-L1.
           03  LB2-DET OCCURS 3.
               05  LB2-DATA.
                   07  LB2-H   PIC  X(16).
                   07  LB2-PRC PIC  ZZZZ9.99 BLANK WHEN ZERO.
               05  FILLER      PIC  X(04).
       01  LB3-L1.
           03  LB3-DET OCCURS 4.
               05  LB3-DATA.
                   07  LB3-H   PIC  X(13).
                   07  LB3-PRC PIC  ZZZZ9.99 BLANK WHEN ZERO.
               05  FILLER      PIC  X(04).
       01  LB4-L1.
           03  LB4-DET OCCURS 3.
               05  LB4-DATA.
                   07  LB4-CODE
                               PIC  X(14).
               05  FILLER      PIC  X(03).
       01  LB4-L2.
           03  LB4-DATA2 OCCURS 3.
               05  LB4-DESC    PIC  X(32).
               05  FILLER      PIC  X(02).
       01  LB4-L3.
           03  LB4-DATA3 OCCURS 3.
               05  LB4-H       PIC  X(13).
               05  LB4-PRC     PIC  ZZZZ9.99.
               05  LB4-LABCDE  PIC  X(08).
               05  LB4-DTE     PIC  9(04).
               05  FILLER      PIC  X(01).
       01  SAPDC-L1.
           03  SAPDC-DET.
               05  SAPDC-CDE1  PIC  X(13).
               05  FILLER      PIC  X(05).
               05  SAPDC-CDE2  PIC  X(13).
               05  FILLER      PIC  X(04).
               05  SAPDC-CDE3  PIC  X(13).
               05  FILLER      PIC  X(05).
               05  SAPDC-CDE4  PIC  X(13).
               05  FILLER      PIC  X(04).
       01  SAPDC-L2.
           03  SAPDC-DATA2 OCCURS 4.
               05  SAPDC-DESC  PIC  X(27).
               05  FILLER      PIC  X(08).
       01  SAPDC-L3.
           03  SAPDC-DATA3 OCCURS 4.
               05  SAPDC-H     PIC  X(18).
               05  SAPDC-PRC   PIC  ZZZZ9.99.
               05  FILLER      PIC  X(09).
       01  PRC-L1.
           03  PRC-H1          PIC  X(15).
           03  PRC-H2          PIC  X(06).
           03  PRC-H3          PIC  X(09).
           03  PRC-H4          PIC  X(32).
           03  PRC-H5.
               05  FILLER      PIC  X(01).
               05  PRC-H5A     PIC  X(12).
           03  PRC-H6.
               05  FILLER      PIC  X(05).
               05  PRC-H6A     PIC  X(04).
           03  PRC-H7.
               05  FILLER      PIC  X(06).
               05  PRC-H7A     PIC  X(06).
           03  FILLER          PIC  X(02).
           03  PRC-H8.
               05  FILLER      PIC  X(01).
               05  PRC-H8A     PIC  X(12).
           03  PRC-H9.
               05  FILLER      PIC  X(05).
               05  PRC-H9A     PIC  X(04).
           03  PRC-H10.
               05  FILLER      PIC  X(06).
               05  PRC-H10A    PIC  X(06).
000730 01  PRC-L2.
000750     03  PRC-ITM         PIC X(14).
000760     03  FILLER          PIC X(01).
000770     03  PRC-BIN         PIC X(03).
000780     03  FILLER          PIC X(01).
000790     03  PRC-CCODE       PIC Z(06)9.99.
000800     03  FILLER          PIC X(01).
000810     03  PRC-DESC        PIC X(30).
000820     03  FILLER          PIC X(02).
           03  PRC-E-WSALE     PIC Z(06)9.99.
           03  FILLER          PIC X(02).
000830     03  PRC-E-CASH      PIC Z(06)9.99.
000820     03  FILLER          PIC X(02).
           03  PRC-E-RET       PIC Z(06)9.99.
           03  FILLER          PIC X(02).
000830     03  PRC-I-WSALE     PIC Z(06)9.99.
000840     03  FILLER          PIC X(02).
           03  PRC-I-CASH      PIC Z(06)9.99.
           03  FILLER          PIC X(02).
000830     03  PRC-I-RET       PIC Z(06)9.99.
       01  PRI-L1.
           03  PRI-H1          PIC  X(15).
           03  PRI-H2          PIC  X(06).
           03  PRI-H3          PIC  X(08).
           03  PRI-H4          PIC  X(37).
           03  PRI-H5          PIC  X(13).
           03  PRI-H6          PIC  X(07).
           03  PRI-H7          PIC  X(09).
000730 01  PRC-L2.
000750     03  PRI-ITM         PIC X(14).
000760     03  FILLER          PIC X(01).
000770     03  PRI-BIN         PIC X(03).
000780     03  FILLER          PIC X(01).
000790     03  PRI-CCODE       PIC X(09).
000800     03  FILLER          PIC X(01).
000810     03  PRI-DESC        PIC X(37).
000820     03  FILLER          PIC X(01).
           03  PRI-EXCL        PIC Z(05)9.99.
           03  FILLER          PIC X(01).
000830     03  PRI-VAT         PIC Z(05)9.99.
000820     03  FILLER          PIC X(01).
000830     03  PRI-INCL        PIC Z(05)9.99.
       01  PRI-L3.
           03  FILLER          PIC  X(42).
           03  PRI-MEMO        PIC  X(60).
       01  DEP-L1.
           03  DEP-H1          PIC  X(15).
           03  DEP-H2          PIC  X(04).
           03  DEP-H3          PIC  X(34).
           03  DEP-H4          PIC  X(10).
           03  DEP-H5          PIC  X(14).
           03  DEP-H6          PIC  X(12).
           03  DEP-H7          PIC  X(07).
           03  DEP-H8.
               05  FILLER      PIC  X(01).
               05  DEP-H8A     PIC  X(06).
           03  DEP-H9.
               05  FILLER      PIC  X(02).
               05  DEP-H9A     PIC  X(04).
           03  DEP-H10.
               05  FILLER      PIC  X(01).
               05  DEP-H10A    PIC  X(06).
           03  FILLER          PIC  X(02).
           03  DEP-H11         PIC  X(09).
           03  DEP-H12         PIC  X(05).
       01  DEP-L2.
           03  DEP-ITM         PIC  X(14).
           03  FILLER          PIC  X(01).
           03  DEP-BIN         PIC  X(03).
           03  FILLER          PIC  X(01).
           03  DEP-DESC        PIC  X(30).
           03  FILLER          PIC  X(01).
           03  DEP-QNT         PIC  Z(06)9.999-.
           03  DEP-COST        PIC  Z(06)9.999.
           03  FILLER          PIC  X(01).
	   03  DEP-AVG	       PIC  Z(05)9.999.
           03  FILLER          PIC  X(01).
           03  DEP-RET         PIC  Z(06)9.99.
           03  FILLER          PIC  X(01).
           03  DEP-MWS         PIC  ZZ9.99.
           03  FILLER          PIC  X(01).
           03  DEP-MCSH        PIC  ZZ9.99.
           03  FILLER          PIC  X(01).
           03  DEP-MRET        PIC  ZZ9.99.
           03  FILLER          PIC  X(01).
           03  DEP-ACT         PIC  Z9/99/99.
           03  FILLER          PIC  X(01).
           03  DEP-GP          PIC  ZZ9.99.
      /
003090 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
003100 77  WS-S1	    PIC 9(04)	  COMP-5.
003110 77  WS-S2            PIC 9(04)     COMP-5.
003120 77  WS-S3            PIC 9(04)     COMP-5.
003130 77  WS-S4            PIC 9(04)     COMP-5.
003140 77  WS-S5            PIC 9(04)     COMP-5.
003140 77  WS-S6            PIC 9(04)     COMP-5.
003140 77  WS-S7	    PIC 9(04)	  COMP-5.
003140 77  WS-S8	    PIC 9(04)	  COMP-5.
003150 77  WS-IXS	    PIC 9(04)	  COMP-5.
003150 77  WS-IXS1          PIC 9(04)     COMP-5.
003160 77  WS-IXS2          PIC 9(04)     COMP-5.
003170 77  WS-IXS3          PIC 9(04)     COMP-5.
003180 77  WS-IXS4          PIC 9(04)     COMP-5.
003190 77  WS-CAT           PIC 9(04)     COMP-5.
003200 77  WS-SCAT          PIC 9(04)     COMP-5.
003230 77  WS-PARKEY        PIC 9(04)     COMP-5.
003240 77  WS-DEPKEY        PIC 9(04)     COMP-5.
003250 77  WS-NETKEY        PIC 9(04)     COMP-5.
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
003270 77  WS-PAGE	    PIC 9(04)	  COMP-5.
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
003300 77  WS-PQNT	    PIC X(01).
003310 77  WS-WS            PIC X(01) VALUE "N".
003310 77  WS-MEMO          PIC X(01) VALUE "N".
003310 77  WS-PRT           PIC X(01).
003320 77  WS-BIN           PIC X(01).
       77  WS-ETYPE         PIC X(01).
003330 77  WS-OPTION        PIC X(01).
003330 77  WS-ZLT	    PIC X(01) VALUE "Y".
003330 77  WS-ZQT	    PIC X(01) VALUE "Y".
       77  WS-NUM	    PIC ZZ9.
003350 77  WS-ERROR         PIC 9(01).
003380 77  WS-KEY1          PIC 9(04)     COMP-5.
003390 77  WS-KSTORE        PIC 9(04)     COMP-5.
003420 77  WS-INVF	    PIC X(01)	  COMP-X.
003420 77  WS-CHAR1         PIC X(01).
003430 77  WS-CHAR2         PIC X(01).
003440 77  WS-IND           PIC 9(01)     VALUE 0.
003450 77  WS-INDS          PIC 9(01)     VALUE 0.
003470 77  WS-ORD           PIC S9(05)    COMP-3.
001930 77  WS-GST-RATE      PIC S9(03)V99 COMP-3 VALUE ZERO.
003480 77  WS-TOTAL         PIC S9(07)V99 COMP-3.
003490 77  WS-VALUE         PIC S9(07)V99 COMP-3.
003500 77  WS-CCOST         PIC S9(07)V99 COMP-3.
003510 77  WS-CSELL         PIC S9(07)V99 COMP-3.
003520 77  WS-SCOST         PIC S9(07)V99 COMP-3.
003530 77  WS-SSELL         PIC S9(07)V99 COMP-3.
003540 77  WS-TCOST         PIC S9(07)V99 COMP-3.
003550 77  WS-TSELL         PIC S9(07)V99 COMP-3.
003560 77  WS-DEPT          PIC 9(03).
003570 77  WS-WEEK          PIC 9(01).
       77  WS-LABCDE        PIC X(08).
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
       77  TODAY-DDMMYY	   PIC	9(06) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
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
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
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

003590 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "066".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003610 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000360 COPY SPARTS.ID.

000300 COPY STOCK.ID.

000360 COPY WARHSE.ID.

000360 COPY WSTOCK.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "STP066.".
               05  W02-USER   PIC X(05) VALUE SPACES.

003740 01  W05.
003750     03  W05-S OCCURS 300   PIC 9(04)     COMP-5.
       01  W05-TAX-RATES.
000760     03  W05-VAT-RATES.
000770         05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 6.
000780     03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
000790         05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
000800         05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
000810         05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
000820         05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
000830         05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
000840         05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
           03  W05-RATE        PIC S9(03)V99 COMP-3.
           03  W05-RTE REDEFINES W05-RATE
                               PIC S9V9(04)  COMP-3.

       COPY W10.STK.

       COPY W12.WS.

004310 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003790     03  W25-VALUE          PIC S9(07)V9(04) COMP-3.
003800     03  W25-TOTAL          PIC S9(07)V9(04) COMP-3.

003810 01  W30-COST-CODE.
003820     03  W30-CODE           PIC X(01) OCCURS 10.
003830 01  W35-COST.
003840     03  W35-CST            PIC 9(08)V99.
003850     03  W35-WHOLE  REDEFINES W35-CST
                                  PIC 9(10).
003860     03  W35-CPRINT.
003870         05  W35-CEDIT      PIC ZZZZZZ9999.
003880     03  W35-EOC REDEFINES W35-CPRINT.
003890         05  W35-CHAR       PIC X(01) OCCURS 10.
003900     03  W35-EON REDEFINES W35-CPRINT.
003910         05  W35-NUM        PIC 9(01) OCCURS 10.
           03  W35-CPRN REDEFINES W35-CPRINT.
               05  FILLER         PIC  X(01).
               05  W35-CSTPRN     PIC  X(09).
003920     03  W35-S1             PIC 9(02) COMP-5.
003930     03  W35-S2             PIC 9(02) COMP-5.

       01  W40-DATES.
           03  W40-DATE1          PIC 9(06).
           03  W40-DTE1 REDEFINES W40-DATE1.
               05  W40-Y1         PIC 9(02).
               05  W40-M1         PIC 9(02).
               05  W40-D1         PIC 9(02).
           03  W40-DATE2          PIC 9(06).
           03  W40-DTE2 REDEFINES W40-DATE2.
               05  W40-Y2         PIC 9(02).
               05  W40-M2         PIC 9(02).
               05  W40-D2         PIC 9(02).

       COPY W40.WS.

002920 01  W40-WAREHOUSE.
002930     03  W40-NAME1.
002940	       05  W40-NAME    PIC  X(40).
003020	   03  W40-NAME2 REDEFINES W40-NAME1.
003030	       05  W40-NCHAR OCCURS 40 PIC X(01).
           03  W40-CONTACT     PIC  X(30).
	   03  W40-CODE	       PIC  X(06).
003070	   03  W40-ADD.
003080	       05  W40-ADDRESS1.
003090		   07  W40-ADD1	PIC X(30).
003100		   07  W40-ADD2	PIC X(30).
003110		   07  W40-ADD3	PIC X(30).
003120		   07  W40-ADD4	PIC X(30).
003130	       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
003140		   07  W40-ACHAR OCCURS 120 PIC X(01).
003230     03  W40-POST        PIC 9(04).
003250     03  W40-TEL         PIC X(14).
003260     03  W40-FAX         PIC X(14).

       COPY FUNCTION.WS.

003950 COPY W50.WS.

004040 COPY W70STOCK.WS.

004060 01  W70-KEYS.
004070     03  W70-BIND        PIC 9(05)     COMP-0 VALUE ZERO.

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
	   03  W75-WHOUSE OCCURS 18.
	       05  W75-WHCDE  PIC  X(06).

004080 01  W95-COMP	       PIC X(40).
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
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
004090 SCREEN SECTION.

       COPY BLANK.CRT.

004440 01  S03.
004450	   03  LINE 18 COLUMN 26 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
004460                           PIC X(14) USING STK-CODE.
004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".
004570 01  S07.
004580	   03  LINE  8 COLUMN 20 FOREGROUND-COLOR 14 VALUE
004590                           "Busy printing departmental report".
004600 01  S08.
004610	   03  LINE 10 COLUMN 20 VALUE "Select department".
004620 01  S09.
004630	   03  LINE 25 COLUMN 20  BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 14 VALUE
004640                           "No details for department - ENTER".
005040 01  S15.
005050	   03  LINE 18 COLUMN 12 VALUE "Stock record:".

       COPY WARLUP.CRT.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
005070 AA00M		 SECTION.
005080 AA00.
003240	   IF LS0-STLEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
005090	     PERFORM ZA000-INIT.
	     PERFORM BE000.
	     PERFORM BE00S.
	     CLOSE PRNREP.
       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM WARHSE-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

      /
005810 AB000              SECTION.
005370 AB15.
005380       MOVE SPACES         TO REP-DETAIL1.
005390	     MOVE "Page:"	 TO REP-PG.
005400	     MOVE "Date:"	 TO REP-DAT.
005410       MOVE W95-COMP       TO REP-COMP.
005420       ADD 1               TO WS-PAGE.
005430       MOVE WS-PAGE        TO REP-P-N.
005440       MOVE W12-TODAY      TO REP-DT.
005450	     MOVE "STOCK LOCATION REPORT"
                                 TO REP-REPORT.
005460       WRITE REP-LINE1 BEFORE 2 LINES.
005470	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE "Location:"	 TO REP-HEAD2.
	     MOVE WAR-NAME	 TO REP-CAT.
005510	     WRITE REP-LINE1 BEFORE 1 LINES.
005520       MOVE SPACES         TO REP-DETAIL1.
005480	     MOVE "Category:"	 TO REP-HEAD2.
005490       MOVE DPT-SALES-NO   TO REP-CATNO.
005500       MOVE DPT-DESC       TO REP-CAT.
005510       WRITE REP-LINE1 BEFORE 2 LINES.
005520       MOVE SPACES         TO REP-DETAIL1.
005530	     MOVE "Item Code"	 TO DEP-H1.
005540	     MOVE "Bin" 	 TO DEP-H2.
005570	     MOVE "Description"  TO DEP-H3.
005580	     MOVE "Quantity"	 TO DEP-H4.
005590	     MOVE "Cost-Price"	 TO DEP-H5.
005600	     MOVE "Average"	 TO DEP-H6.
005610	     MOVE "Retail"	 TO DEP-H7.
             MOVE ALL "_"        TO DEP-H8 DEP-H10.
005620	     MOVE "Markup"	 TO DEP-H9.
005660	     MOVE "Active"	 TO DEP-H11.
005650       MOVE "GP-%"         TO DEP-H12.
005680       WRITE REP-LINE1 BEFORE 1 LINES.
005690       MOVE SPACES         TO REP-DETAIL1.
005640	     MOVE WS-WS-HD	 TO DEP-H8.
005610	     MOVE WS-CS-HD	 TO DEP-H9.
005620	     MOVE "Retail"	 TO DEP-H10A.
005680       WRITE REP-LINE1 BEFORE 2 LINES.
005690       MOVE SPACES TO REP-DETAIL1.
005700 AB20.
005710       EXIT.
       AB25.
009840       MOVE WS-COND        TO REP-DETAIL1.
009850       WRITE REP-LINE1 BEFORE ZERO LINES.
009860       MOVE SPACES         TO REP-DETAIL1.
       AB30.
             EXIT.
       AB35.
009990       MOVE WS-NORM        TO REP-DETAIL1.
010000       WRITE REP-LINE1 BEFORE ZERO LINES.
010010       MOVE SPACES         TO REP-DETAIL1.
       AB40.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY STOCK.RD.

       COPY SPARTS.RD.

       COPY WARHSE.RD.

       COPY WSTOCK.RD.

       COPY STOCK.1ST.

       COPY WARHSE.LUP.

       COPY APAC.HLP.

      *
      *    ****   G E T   W A R E H O U S E   R E C O R D
      *
015170 CA000         SECTION.
015180 CA00.
013310	     MOVE 6		 TO W50-LENGTH.

       CA05.
013320	     MOVE W40-CODE	 TO W50-DATA.
           IF W50-1 = SPACE
	       MOVE W50-5	 TO W40-CODE
               GO TO CA05.
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013380	     MOVE W50-DENT	 TO W40-CODE.

       CA06.
013380	     MOVE W50-DENT	 TO WAR-CODE.

       CA10.
	     PERFORM READ-WARHSE THRU READ-WARHSE-EXIT.

       CA99.
             EXIT.

      /
008500 BE000		SECTION.
008510 BE00.
008520       DISPLAY S04.
008530	     MOVE 1		 TO WS-S2 WS-S3 WS-DEPKEY.
008550 BE20.
008570       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE WS-DEPKEY	 TO WS-NUM.
	     DISPLAY WS-NUM AT 1220
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY DPT-DESC AT 1224
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
008580 BE25.
008590     IF DPT-DESC NOT = SPACES
008600	       MOVE WS-S2	 TO W05-S (WS-S2).
008610	   IF WS-S2 < 297
008620	       ADD 1		 TO WS-S2
008630	       ADD 1		 TO WS-DEPKEY
008640         GO TO BE20.
       BE99.
	     EXIT.
      /
008500 BE00S            SECTION.
       BE00.
009800       DISPLAY CLR-SCREEN.
	     DISPLAY "LOCATION STOCK REPORT" AT 0230
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE SPACES	 TO W40-CODE.
	     DISPLAY "Warehouse Code:" AT 0404.

       BE02.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Warehouse Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     ACCEPT W40-CODE AT 0420
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO BE999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BE02
	       END-EVALUATE
	       DISPLAY W40-CODE AT 0420
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       GO TO BE02.
019250	     PERFORM CLEAR-L50.
015830	   IF W40-CODE = SPACE
015840	       GO TO BE999.

       BE03.
	     PERFORM CA000.
015860	   IF WS-F-ERROR = 51
017520	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50
	       DISPLAY W40-CODE AT 0420
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
018640	       IF W40-CODE = SPACES
018720		   GO TO BE02
               ELSE
		   GO TO BE03.
	     DISPLAY WAR-NAME AT 0427
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     PERFORM CHECK-CORRECT.
015980	   IF WS-OPTION = "N"
	       GO TO BE02.
	     DISPLAY "Print stock with Lead Time = 0.00" AT 0604.
       BE05.
	     ACCEPT WS-ZLT AT 0639
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ZLT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-ZLT = "Y" OR "N")
	       GO TO BE05.
	     DISPLAY "Print stock with zero quantity" AT 0804.
       BE10.
	     ACCEPT WS-ZQT AT 0835
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-ZQT
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-ZQT = "Y" OR "N")
	       GO TO BE10.
       BE50.
	     DISPLAY "A" AT 1004 WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ll or "
		     "S" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "elected Departments".
	     MOVE SPACE 	 TO WS-OPTION.
       BE55.
	     ACCEPT WS-OPTION AT 1033
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "A" OR "S")
	       GO TO BE55.
	   IF WS-OPTION = "S"
	       GO TO BE110.
009790 BE85.
009810       DISPLAY S07.
	     MOVE "'C'ontinue or 'E'xit  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
       BE86.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "E" OR "C")
	       GO TO BE85.
           IF WS-OPTION = "E"
	       GO TO BE999.
009820	     DISPLAY S15.
009830       MOVE 1              TO WS-CAT.
             PERFORM AB25.
009870 BE90.
009880     IF W05-S (WS-CAT) NOT = ZERO
009890         GO TO BE95.
009900 BE92.
009910	   IF WS-CAT < 297
009920         ADD 1             TO WS-CAT
009930         GO TO BE90.
009940       MOVE "STOCK TOTAL"  TO REP-HEAD13.
009950       MOVE WS-TCOST       TO REP-DCOST.
009960       MOVE WS-TSELL       TO REP-DSELL.
009970       WRITE REP-LINE1 BEFORE 1 LINES.
009980       MOVE SPACES         TO REP-DETAIL1.
009990       PERFORM AB35.
010000       WRITE REP-LINE1 BEFORE PAGE.
010020 BE93.
010030	     GO TO BE999.
010040 BE95.
010050	     MOVE 0		 TO WS-S1.
010060     IF WS-PAGE > ZERO
010070         WRITE REP-LINE1 BEFORE PAGE.
010080       MOVE WS-CAT         TO WS-DEPKEY.
010090       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     DISPLAY DPT-DESC AT 1412
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
010100       PERFORM AB15.
	     MOVE WS-CAT	 TO STK-LEDG WS-SCAT.
	     MOVE "!"		 TO STK-CODE.
009360       PERFORM START-AT-STOCK-LEDG THRU READ-STOCK-EXIT.
009370     IF WS-F-ERROR = 22
010130         GO TO BE106.
010140 BE100.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF (WS-F-ERROR = 22) OR (STK-LEDG NOT = WS-SCAT)
010160         GO TO BE105.
	   IF WS-ZLT = "N"
	       IF STK-LTIME = ZERO
		   GO TO BE100.
	     MOVE W40-CODE	 TO WST-WAR.
	     MOVE STK-CODE	 TO WST-CODE.
	     MOVE SPACES	 TO WST-ITM.
	     PERFORM READ-WSTOCK THRU READ-WSTOCK-EXIT.
	   IF WS-F-ERROR = 52
	       GO TO BE100.

	   IF WS-ZQT = "N"
	       IF WST-QUANT = ZERO
		   GO TO BE100.
	   IF STK-IND > 2
	       MOVE ZERO	 TO WST-QUANT.
	   IF STK-IND = 1
	       IF NOT (WS-INVF = 1)
		   MOVE ZERO	 TO WST-QUANT.
010200       DISPLAY S03.
010210       MOVE STK-CODE       TO DEP-ITM.
010250       MOVE STK-DESC       TO DEP-DESC.
	   IF STK-IND = 4
009710	       WRITE REP-LINE1 BEFORE 0 LINES
	       GO TO BE101.
010220       MOVE STK-BIN        TO DEP-BIN.
010260	     MOVE WST-QUANT	 TO DEP-QNT.
010270       MOVE STK-COST       TO DEP-COST.
010280       MOVE STK-SELL       TO DEP-RET.
      *    ****    A V E R A G E   C O S T
	   IF WST-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
	   IF STK-IND < 2
016950	       IF STK-AVGE = ZERO
016960		   MOVE STK-COST TO STK-AVGE.
010290	     MOVE STK-AVGE	 TO DEP-AVG.
010310       COMPUTE DEP-MWS ROUNDED = ((STK-WSALE - STK-COST)
010320                                 * 100.000) / STK-COST.
010310       COMPUTE DEP-MRET ROUNDED = ((STK-SELL - STK-COST)
010320                                  * 100.000) / STK-COST.
010330       COMPUTE DEP-MCSH ROUNDED = ((STK-CASH - STK-COST)
010340                                  * 100.000) / STK-COST.
010350	     MOVE STK-MTDV	 TO WS-SSELL.
010360	     MOVE STK-MTDC	 TO WS-SCOST.
010370     IF WS-SSELL = ZERO OR WS-SCOST = ZERO
010380	       MOVE STK-COST	 TO WS-SCOST
010390         COMPUTE WS-SSELL = (STK-SELL + STK-CASH) / 2.000 + .005.
010400       COMPUTE DEP-GP ROUNDED = ((WS-SSELL - WS-SCOST)
010410                                * 100.000) / WS-SSELL.
010420	     MOVE STK-ACT	 TO DEP-ACT.
010440	     MULTIPLY STK-AVGE BY WST-QUANT
				 GIVING WS-VALUE.
010450	     MULTIPLY STK-SELL BY WST-QUANT
				 GIVING WS-TOTAL.
010460	     ADD WS-VALUE	 TO WS-CCOST.
010470	     ADD WS-TOTAL	 TO WS-CSELL.
       BE101.
010480	   IF LINAGE-COUNTER < WS-PRN-LENGTH
010490         WRITE REP-LINE1 BEFORE 1 LINES
010500     ELSE
010510         WRITE REP-LINE1 BEFORE PAGE
010520         PERFORM AB15.
010530	     MOVE SPACES	 TO REP-DETAIL1.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
014970         MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
014980         WRITE REP-LINE1 BEFORE PAGE
               MOVE "X"          TO WS-OPTION
014990         GO TO BE106.
010540 BE102.
010560       GO TO BE100.
010570 BE105.
010580	   IF LINAGE-COUNTER > (WS-PRN-LENGTH - 4)
010590         WRITE REP-LINE1 BEFORE PAGE
010600         PERFORM AB15.
010610       MOVE SPACES         TO REP-DETAIL1.
010620       WRITE REP-LINE1 BEFORE 1 LINES.
010630       MOVE "DEPARTMENT TOTAL COST/SELL"
                                 TO REP-HEAD13.
010640       MOVE WS-CCOST       TO REP-DCOST.
010650       MOVE WS-CSELL       TO REP-DSELL.
010660       ADD WS-CCOST        TO WS-TCOST.
010670       ADD WS-CSELL        TO WS-TSELL.
010680       MOVE ZERO           TO WS-CCOST WS-CSELL.
010690       WRITE REP-LINE1 BEFORE 2 LINES.
010700       MOVE SPACES         TO REP-DETAIL1.
       BE106.
             EXIT.
010710 BE108.
010720       GO TO BE92.
010730 BE110.
010740       DISPLAY CLR-SCREEN.
             DISPLAY "SELECTIVE DEPARTMENTAL REPORT" AT 0226
                     WITH FOREGROUND-COLOR 15.
010750       DISPLAY S08.
010760       DISPLAY S15.
010770	     ACCEPT WS-DEPT AT 1038
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
010780     IF WS-DEPT = ZERO
010790	       GO TO BE999.
010800       MOVE WS-DEPT        TO WS-CAT.
	     PERFORM CLEAR-L50.
010810	   IF (WS-CAT > 297) OR (W05-S (WS-CAT) = ZERO)
010820         DISPLAY S09
010830	       ACCEPT WS-OPTION AT 2554
		      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO
010840         GO TO BE110.
010080       MOVE WS-CAT         TO WS-DEPKEY.
010090       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     DISPLAY DPT-DESC AT 1412
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
       BE115.
	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "R" OR "C")
               GO TO BE115.
           IF WS-OPTION = "R"
               GO TO BE110.
	     PERFORM AB25.
010880       PERFORM BE95 THRU BE105.
             PERFORM AB35
010000       WRITE REP-LINE1 BEFORE PAGE.
010900       MOVE ZERO           TO WS-PAGE.
010910       GO TO BE110.
       BE999.
	     EXIT.

      /
019370 ZA000-INIT	       SECTION 8.
019380 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019390	     DISPLAY CLR-SCREEN.
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



019480	     MOVE 3 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044260	     MOVE WS-PTH15	 TO W02-PTH21 W02-PTH2
				    W02-PTH22 W02-PTH7
				    W02-PTH51 W02-PTH52.
	     MOVE PAR-INV	 TO WS-INVF.
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
	       MOVE ZERO	 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
	       MOVE 1		 TO WS-PRN-NO
           ELSE
           IF TRM-STDP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
	       MOVE 2		 TO WS-PRN-NO
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
       ZA01B.
             MOVE 6 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-WHOLESALE TO WS-WS.
019880       MOVE PAR-COST-CODE TO W30-COST-CODE.
042520	     MOVE 5		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042600       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
019620	     MOVE 3 TO WS-NETKEY.
019630       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
019690       MOVE STK-WEEK TO WS-WEEK.
             MOVE STK-LABCDE     TO WS-LABCDE.
042520	     MOVE 8		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-WS-HD = X"00000000000000000000")
	       MOVE PAR-WS-HD	 TO WS-WS-HD.
	   IF NOT (PAR-CS-HD = X"00000000000000000000")
	       MOVE PAR-CS-HD	 TO WS-CS-HD.
	     INSPECT WS-CS-HD REPLACING LEADING "-" BY " ".
	     INSPECT WS-WS-HD REPLACING LEADING "-" BY " ".
019820	     MOVE 1 TO WS-PARKEY.
019830       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019840       MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W40-DATE1 W40-DATE2.
           IF W40-M2 > 3
               SUBTRACT 3        FROM W40-M2
           ELSE
               SUBTRACT 1        FROM W40-Y2
               ADD 9             TO W40-M2.
019850       MOVE PAR-COMPANY TO W95-COMP.
       ZA05.
	   IF WS-PRN-NO < 3
	       PERFORM ZA210 THRU ZA215
	       IF WS-ERROR = 1
		   GO TO ZA205
	       END-IF
	   ELSE
	       DISPLAY "Waiting for printer response" AT 1412
			WITH FOREGROUND-COLOR 14 BLINK.
019890	     OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
	     MOVE WS-10CPI	 TO REP-DETAIL1.
	     WRITE REP-LINE1 BEFORE 0 LINES.
	     MOVE SPACES	 TO REP-DETAIL1.
018380	     DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR 3.
019900	     GO TO ZA999-EXIT.
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
019910 ZA200.
019920       DISPLAY "Files locked - Try later" AT 2312
019930                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
019940       DISPLAY "Press any key" AT 2512
019950		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
019960	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
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
	     ACCEPT WS-OPTION AT 2334
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     IF NOT (WS-OPTION = "A" OR "R")
		 GO TO ZA212.
	     IF WS-OPTION = "A"
		 MOVE 1 	 TO WS-ERROR
		 GO TO ZA215.
	       GO TO ZA210.
       ZA215.
	     EXIT.
019990 ZA999-EXIT.
020000       EXIT.
      /
020010 ZB000-ERROR	       SECTION 8.

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

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820         MOVE W02-DEPART   TO WS-FILE
041830         MOVE WS-DEPKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
	   ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 51
041780	       MOVE W02-WARHSE TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WAR-CODE	 TO WS-KEYX
041840	   ELSE
041770	   IF WS-F-ERROR = 52
041780	       MOVE W02-WSTOCK TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE WST-KEY	 TO WS-KEYX.
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
