      *
      *    F L U S H   F I L E   B U F F E R S
      *
      *    Variables required by CALL X"84"
      *
      *    DOS interrupt number 21h
      *
       01  MSDOS       PIC  X(01) VALUE X"21".
      *
      *    Flag controls the contents of the AX, BX, CX and DX registers
      *
      *    Least significant 4 bits control the contents on entry
      *
      *    least significant bit
      *       1 = AX is loaded with the 16 bit value in the AX parameter
      *       0 = DS:AX addresses the AX parameter
      *
      *    Most significant 4 bits control the contents on exit
      *
      *    most significant bit
      *       1 = Value returned in DX is placed in the DX parameter
      *
       01  FLAG            PIC  X(01) VALUE X"11".
      *
      *    The AX parameter is used to specify the function on entry and
      *                     will contain the error code on exit.
      *
      *           flush buffer = 68h (AH)
      *
       01  AX-PARM.
           03  AH-PARM     PIC  9(02) COMP-X.
           03  AL-PARM     PIC  9(02) COMP-X.
      *
      *    The BX parameter contains the file handle 
      *
      *           file handle must be terminated with a null code
      *
       01  FILE-HANDLE.
           03  FILE-NAME   PIC  X(64).
           03  FILE-NME-CHAR REDEFINES FILE-NAME
                           PIC  X(01) OCCURS 64.
      *
      *    The CX parameter
      *
       01  CX-PARM.
           03  CH-PARM     PIC  9(02) COMP-X.
           03  CL-PARM     PIC  9(02) COMP-X.
      *
      *    The DX parameter
      *
       01  DX-PARM.
           03  DH-PARM     PIC  9(02) COMP-X.
           03  DL-PARM     PIC  9(02) COMP-X.
      *
      *    Additional variables
      *
       01  ADDITIONAL-VARIABLES.
           03  FH-S1       PIC  9(02) COMP-0.
           03  FH-S2       PIC  9(02) COMP-0.
           03  FH-FUNC     PIC  9(02) COMP-X VALUE 7.
           03  FH-PATH.
               05  FH-SL   PIC  X(01) VALUE SPACES.
               05  FILLER  PIC  X(63) VALUE SPACES.
           03  FH-FILE.
               05  FH-DSK.
                   07  FH-D PIC X(01).
                   07  FH-C PIC X(01).
               05  FH-NAME PIC  X(14).
