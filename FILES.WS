
       01 OPENING-PROC-STAT                    PIC 9(1)  VALUE ZERO.
           88 OPENING-PROC-SUCCESS             VALUE ZERO.

       01 OPN                                  PIC 9(1) VALUE 1.
       01 FILS-OPN-STAT                        PIC 9(1) VALUE ZERO.
           88 FILS-ARE-OPN                     VALUE 1.

       01 FIL-IS-OPN                           PIC 9(1) VALUE 1.
       01 FIL-IS-CLSD                          PIC 9(1) VALUE ZERO.
       01 UNSUCCESSFUL                         PIC 9(1) VALUE 1.
       01 SYS-ERR                              PIC X(2) VALUE "24".
       01 UPDATE-OK-FLG                        PIC X(1).
       01 ALLOW-FIL-LCKD-FLG                   PIC X(1) VALUE "N".
           88 ALLOW-FIL-LCKD                   VALUE "Y".

       01 ALLOW-INVAL-OPN-FLG                  PIC X(1) VALUE "N".
           88 ALLOW-INVAL-OPN                  VALUE "Y".

       01 ENTRY-STAT                           PIC X(1).
           88 ADD-REC                          VALUE "A".
           88 CHNG-REC                         VALUE "C".
           88 DEL-REC                          VALUE "D".

       01 FIL-STAT-COD-TABLE.
           03 NULL-STAT                        PIC X(2) VALUE SPACES.
           03 REC-ON-FIL                       PIC X(2) VALUE "00".
           03 I-O-OK                           PIC X(2) VALUE "00".
           03 DUP-ALT-KEYS                     PIC X(2) VALUE "02".
           03 INVAL-REC-LEN                    PIC X(2) VALUE "04".
           03 INIT-OPT-FIL                     PIC X(2) VALUE "05".
           03 AT-END                           PIC X(2) VALUE "10".
           03 INVAL-REL-KEY                    PIC X(2) VALUE "14".
           03 INVAL-KEY                        PIC X(2) VALUE "21".
           03 DUP-KEY                          PIC X(2) VALUE "22".
           03 REC-NOT-FND                      PIC X(2) VALUE "23".
           03 INVAL-OPN                        PIC X(2) VALUE "35".
           03 INVAL-DIR                        PIC X(2) VALUE X"3909".
           03 INVAL-DEV                        PIC X(2) VALUE X"3904".
           03 MEMORY-OVERFLOW                  PIC X(2) VALUE X"3901".
           03 HARDWARE-ERR                     PIC X(2) VALUE X"3905".
           03 BOUNDARY-ERR                     PIC X(2) VALUE X"3907".
           03 FIL-LCKD                         PIC X(2) VALUE X"3941".
           03 REC-LCKD                         PIC X(2) VALUE X"3944".

       01 TOO-MANY-FILS-CODS                   PIC X(2).
           88 TOO-MANY-FILS                    VALUES ARE X"390E",
                                                          X"390F",
                                                          X"3910",
                                                          X"3920",
                                                          X"3946".
