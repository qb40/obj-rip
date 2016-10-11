' ObjRip - Rips the raw code/data from an OMF object file
' Version 1.0 by Plasma / Jon Petrosky  [04-04-2004]  "Don't fear the ripper"
' www.phatcode.net

DEFINT A-Z
'$DYNAMIC
'$LIB:     'QB.LIB'
'$INCLUDE: 'QB.BI'

DECLARE FUNCTION RipSeg (ObjFile$, BufferSeg, SymName$(), SymOff(), SymCount)
DECLARE SUB SaveBinary (OutFile$, BufferSeg, SegLen)
DECLARE SUB SaveText (OutFile$, ObjFile$, BufferSeg, SegLen, SymName$(), SymOff(), SymCount, VarName$, LineWidth)
DECLARE SUB SortSym (SymName$(), SymOff(), SymCount)
DECLARE SUB ParseCommand (ObjFile$, OutFile$, SymFile$, VarName$, Mode, LineWidth)
DECLARE FUNCTION GetCom$ ()
DECLARE SUB ShowHelp ()
DECLARE FUNCTION DIR2$ (FileSpec$, Attr)
DECLARE SUB SaveSym (SymFile$, ObjFile$, OutFile$, SymName$(), SymOff(), SymCount)

DIM SegBuffer(32766)
DIM SymName$(8191)
DIM SymOff(8191)

CONST FALSE = 0
CONST TRUE = NOT FALSE
CONST ANYDIR = &H10
CONST ANYFILE = &H27

PRINT "ObjRip - Rips the raw code/data from an OMF object file"
PRINT "Version 1.0 by Plasma / Jon Petrosky  [04-04-2004]  " + CHR$(34) + "Don't fear the ripper" + CHR$(34)
PRINT

ParseCommand ObjFile$, OutFile$, SymFile$, VarName$, BinMode, LineWidth

IF ObjFile$ = "" THEN
  PRINT "You must specify an object file!"
  PRINT
  ShowHelp
ELSEIF DIR2$(ObjFile$, ANYFILE) = "" THEN
  PRINT "Object file " + CHR$(34) + ObjFile$ + CHR$(34) + " not found."
  END
END IF

PRINT "Object File: " + ObjFile$
PRINT "Output File: " + OutFile$
PRINT

PRINT "Ripping segment data from object file...";
SegLen = RipSeg(ObjFile$, VARSEG(SegBuffer(0)), SymName$(), SymOff(), SymCount)
PRINT "done!"
PRINT
PRINT "Segment length:"; SegLen; "bytes"
PRINT "Public symbols:"; SymCount
PRINT

SortSym SymName$(), SymOff(), SymCount
IF BinMode THEN
  PRINT "Saving output in binary format...";
  SaveBinary OutFile$, VARSEG(SegBuffer(0)), SegLen
  PRINT "done!"
  IF SymCount > 0 THEN
    PRINT "Saving public symbol list file...";
    SaveSym SymFile$, ObjFile$, OutFile$, SymName$(), SymOff(), SymCount
    PRINT "done!"
  END IF
ELSE
  PRINT "Saving output in string format...";
  SaveText OutFile$, ObjFile$, VARSEG(SegBuffer(0)), SegLen, SymName$(), SymOff(), SymCount, VarName$, LineWidth
  PRINT "done!"
END IF

END

FUNCTION DIR2$ (FileSpec$, Attr) STATIC

  'Settings for Attr: (may be combined)
  '
  ' &H40 Device
  ' &H20 Archive
  ' &H10 Directory
  ' &H8  Volume Label
  ' &H4  System File
  ' &H2  Hidden File
  ' &H1  Read-Only File

  DIM DTA AS STRING * 44
  FileSpecZ$ = FileSpec$ + CHR$(0)

  DO

    DIM Regs AS RegTypeX

    Regs.ax = &H1A00
    Regs.ds = VARSEG(DTA)
    Regs.dx = VARPTR(DTA)
    INTERRUPTX &H21, Regs, Regs

    IF FileSpecZ$ <> CHR$(0) THEN
      Regs.ax = &H4E00
      Regs.cx = Attr
      Regs.ds = VARSEG(FileSpecZ$)
      Regs.dx = SADD(FileSpecZ$)
    ELSE
      Regs.ax = &H4F00
    END IF

    INTERRUPTX &H21, Regs, Regs

    IF Regs.flags AND 1 THEN
      DIR2$ = ""
      EXIT FUNCTION
    ELSE
      realAttr = ASC(MID$(DTA, 22, 1))
      IF realAttr AND Attr THEN
        Null = INSTR(31, DTA, CHR$(0))
        DIR2$ = MID$(DTA, 31, Null - 31)
        EXIT FUNCTION
      ELSE
        FileSpecZ$ = CHR$(0)
      END IF
    END IF

  LOOP

END FUNCTION

FUNCTION GetCom$

  DIM Regs AS RegTypeX

  Regs.ax = &H2F00
  INTERRUPTX &H21, Regs, Regs

  DEF SEG = Regs.es
  Offset = Regs.bx
  Length = PEEK(Offset)

  FOR i = Offset + 1 TO Offset + Length
    Com$ = Com$ + CHR$(PEEK(i))
  NEXT

 GetCom$ = LTRIM$(RTRIM$(Com$))

END FUNCTION

SUB ParseCommand (ObjFile$, OutFile$, SymFile$, VarName$, BinMode, LineWidth)

  ObjFile$ = ""
  OutFile$ = ""
  SymFile$ = ""
  VarName$ = ""
  BinMode = FALSE
  LineWidth = 0

  OnObj = FALSE
  HaveObj = FALSE
  OnOut = FALSE
  HaveOut = FALSE
  OnSym = FALSE
  Comma = 0

  Cmd$ = GetCom$
  FOR i = 1 TO LEN(Cmd$)
    IF UCASE$(MID$(Cmd$, i, 2)) = "/?" OR UCASE$(MID$(Cmd$, i, 2)) = "-?" OR UCASE$(MID$(Cmd$, i, 2)) = "/H" OR UCASE$(MID$(Cmd$, i, 2)) = "-H" THEN
      ShowHelp
    ELSEIF UCASE$(MID$(Cmd$, i, 4)) = "/BIN" OR UCASE$(MID$(Cmd$, i, 4)) = "-BIN" THEN
      BinMode = TRUE
      i = i + 3
    ELSEIF UCASE$(MID$(Cmd$, i, 5)) = "/VAR:" OR UCASE$(MID$(Cmd$, i, 5)) = "-VAR:" THEN
      Start = FALSE
      FOR j = i + 5 TO LEN(Cmd$)
        Char$ = MID$(Cmd$, j, 1)
        IF NOT Start THEN
          IF Char$ <> " " THEN
            Start = TRUE
            VarName$ = VarName$ + Char$
          END IF
        ELSE
          IF Char$ = " " OR Char$ = "/" OR Char$ = "-" THEN
            i = j - 1
            EXIT FOR
          ELSE
            VarName$ = VarName$ + Char$
          END IF
        END IF
      NEXT
    ELSEIF UCASE$(MID$(Cmd$, i, 7)) = "/WIDTH:" OR UCASE$(MID$(Cmd$, i, 7)) = "-WIDTH:" THEN
      Start = FALSE
      FOR j = i + 7 TO LEN(Cmd$)
        Char$ = MID$(Cmd$, j, 1)
        IF NOT Start THEN
          IF Char$ <> " " THEN
            Start = TRUE
            LineWidth$ = LineWidth$ + Char$
          END IF
        ELSE
          IF Char$ = " " OR Char$ = "/" OR Char$ = "-" THEN
            i = j - 1
            EXIT FOR
          ELSE
            LineWidth$ = LineWidth$ + Char$
          END IF
        END IF
      NEXT
      LineWidth = VAL(LineWidth$)
    ELSE
      FOR j = i TO LEN(Cmd$)
        Char$ = MID$(Cmd$, j, 1)
        IF Char$ = "," THEN Comma = Comma + 1
        IF Comma = 2 AND NOT OnOut THEN OnOut = TRUE

        IF NOT OnObj THEN
          IF Char$ = "/" OR Char$ = "-" THEN
            i = j - 1
            EXIT FOR
          ELSEIF Char$ <> " " THEN
            OnObj = TRUE
            ObjFile$ = ObjFile$ + Char$
          END IF
        ELSEIF (NOT HaveObj) AND (NOT OnOut) THEN
          IF Char$ = " " OR Char$ = "," THEN
            HaveObj = TRUE
          ELSE
            ObjFile$ = ObjFile$ + Char$
          END IF
        ELSEIF NOT OnOut THEN
          IF Char$ <> " " AND Char$ <> "," THEN
            OnOut = TRUE
            OutFile$ = OutFile$ + Char$
          END IF
        ELSEIF (NOT HaveOut) AND (NOT OnSym) THEN
          IF Char$ = " " OR Char$ = "," THEN
            HaveOut = TRUE
          ELSE
            OutFile$ = OutFile$ + Char$
          END IF
        ELSEIF NOT OnSym THEN
          IF Char$ <> " " AND Char$ <> "," THEN
            HaveOut = TRUE
            SymFile$ = SymFile$ + Char$
          END IF
        ELSE
          SymFile$ = SymFile$ + Char$
        END IF
      NEXT
      IF j = LEN(Cmd$) + 1 THEN EXIT FOR
    END IF
  NEXT

  IF ObjFile$ = "" THEN EXIT SUB
  FOR i = LEN(ObjFile$) TO 1 STEP -1
    IF MID$(ObjFile$, i, 1) = "." THEN
      Base$ = LEFT$(ObjFile$, i - 1)
      EXIT FOR
    ELSEIF MID$(ObjFile$, i, 1) = "\" OR MID$(ObjFile$, i, 1) = "/" THEN
      i = 1
    END IF
  NEXT
  IF i = 0 THEN
    Base$ = ObjFile$
    ObjFile$ = ObjFile$ + ".OBJ"
  END IF

  IF OutFile$ = "" THEN
    IF BinMode THEN
      OutFile$ = Base$ + ".BIN"
    ELSE
      OutFile$ = Base$ + ".BAS"
    END IF
  ELSE
    FOR i = LEN(OutFile$) TO 1 STEP -1
      IF MID$(OutFile$, i, 1) = "." THEN
        EXIT FOR
      ELSEIF MID$(OutFile$, i, 1) = "\" OR MID$(OutFile$, i, 1) = "/" THEN
        i = 1
      END IF
    NEXT
    IF i = 0 THEN
      IF BinMode THEN
        OutFile$ = OutFile$ + ".BIN"
      ELSE
        OutFile$ = OutFile$ + ".BAS"
      END IF
    END IF
  END IF

  IF SymFile$ = "" THEN
    SymFile$ = Base$ + ".TXT"
  ELSE
    FOR i = LEN(SymFile$) TO 1 STEP -1
      IF MID$(SymFile$, i, 1) = "." THEN
        EXIT FOR
      ELSEIF MID$(SymFile$, i, 1) = "\" OR MID$(SymFile$, i, 1) = "/" THEN
        i = 1
      END IF
    NEXT
    IF i = 0 THEN
      SymFile$ = SymFile$ + ".TXT"
    END IF
  END IF

  IF VarName$ = "" THEN VarName$ = "op$"
  IF LineWidth <= 0 THEN LineWidth = 70

END SUB

FUNCTION RipSeg (ObjFile$, BufferSeg, SymName$(), SymOff(), SymCount)

  Obj = FREEFILE
  OPEN ObjFile$ FOR BINARY AS #Obj

  SymCount = 0
  MaxOff = 0
  Byte$ = SPACE$(1)
  DEF SEG = BufferSeg

  DO WHILE NOT EOF(Obj)

    GET #Obj, , Byte$
    RecType = ASC(Byte$)
    IF RecType = 0 THEN EXIT DO

    GET #Obj, , RecLen
    OldPos = SEEK(Obj)

    SELECT CASE RecType
      CASE &H8A  'End of module
        EXIT DO

      CASE &H90  'Public symbols
        GET #Obj, , Byte$
        GET #Obj, , Byte$
        SegNum = ASC(Byte$)
        IF SegNum <> 1 THEN
          CLOSE
          PRINT "error!"
          PRINT "Multiple segments are not supported."
          END
        END IF
        GET #Obj, , Byte$
        Symbol$ = SPACE$(ASC(Byte$))
        GET #Obj, , Symbol$
        GET #Obj, , Offset
        SymName$(SymCount) = Symbol$
        SymOff(SymCount) = Offset
        SymCount = SymCount + 1

      CASE &HA0  'Segment data
        GET #Obj, , Byte$
        SegNum = ASC(Byte$)
        IF SegNum <> 1 THEN
          CLOSE
          PRINT "error!"
          PRINT "Multiple segments are not supported."
          END
        END IF
        GET #Obj, , Offset
        RawData$ = SPACE$(RecLen - 4)
        GET #Obj, , RawData$
        FOR i = 1 TO LEN(RawData$)
          IF Offset > MaxOff THEN MaxOff = Offset
          POKE Offset, ASC(MID$(RawData$, i, 1))
          Offset = Offset + 1
        NEXT

      CASE &HA2  'Duplicated segment data
        GET #Obj, , Byte$
        SegNum = ASC(Byte$)
        IF SegNum <> 1 THEN
          CLOSE
          PRINT "error!"
          PRINT "Multiple segments are not supported."
          END
        END IF
        GET #Obj, , Offset
        GET #Obj, , NumDups
        GET #Obj, , Junk
        GET #Obj, , Junk
        GET #Obj, , Junk
        GET #Obj, , Byte$
        DupData$ = SPACE$(ASC(Byte$))
        GET #Obj, , DupData$

        FOR i = 1 TO NumDups
          FOR j = 1 TO LEN(DupData$)
            IF Offset > MaxOff THEN MaxOff = Offset
            POKE Offset, ASC(MID$(DupData$, j, 1))
            Offset = Offset + 1
          NEXT
        NEXT

    END SELECT
    SEEK #Obj, OldPos + RecLen

  LOOP
  CLOSE #Obj

  RipSeg = MaxOff + 1

END FUNCTION

SUB SaveBinary (OutFile$, BufferSeg, SegLen)

  OutFile = FREEFILE
  OPEN OutFile$ FOR OUTPUT AS #OutFile
  CLOSE #OutFile

  OPEN OutFile$ FOR BINARY AS #OutFile
  OutBuffer$ = ""
  DEF SEG = BufferSeg
  FOR i = 0 TO SegLen - 1
    OutBuffer$ = OutBuffer$ + CHR$(PEEK(i))
    IF LEN(OutBuffer$) = 8192 THEN
      PUT #OutFile, , OutBuffer$
      OutBuffer$ = ""
    END IF
  NEXT
  PUT #OutFile, , OutBuffer$
  CLOSE #OutFile

END SUB

SUB SaveText (OutFile$, ObjFile$, BufferSeg, SegLen, SymName$(), SymOff(), SymCount, VarName$, LineWidth)

  OutFile = FREEFILE
  OPEN OutFile$ FOR OUTPUT AS #OutFile

  PRINT #OutFile, "' Generated by ObjRip 1.0"
  PRINT #OutFile, "' Original object file: " + ObjFile$
  IF SymCount = 0 THEN
    PRINT #OutFile, "' No public symbols"
  ELSE
    PRINT #OutFile, "' Public symbols: " + LTRIM$(STR$(SymCount))
    PRINT #OutFile, "'"
    PRINT #OutFile, "' Name                             Offset"
    PRINT #OutFile, "' ----------------------------------------------"
    FOR i = 0 TO SymCount - 1
      DecOffset$ = LTRIM$(STR$(SymOff(i)))
      DecOffset$ = SPACE$(5 - LEN(DecOffset$)) + DecOffset$
      HexOffset$ = HEX$(SymOff(i))
      HexOffset$ = "(" + STRING$(4 - LEN(HexOffset$), "0") + HexOffset$ + "h)"
      Offset$ = DecOffset$ + " " + HexOffset$
      IF LEN(SymName$(i)) < 33 THEN
        PRINT #OutFile, "' " + SymName$(i) + SPACE$(33 - LEN(SymName$(i))) + Offset$
      ELSE
        PRINT #OutFile, "' " + SymName$(i) + "  " + Offset$
      END IF
    NEXT
  END IF
  PRINT #OutFile, ""

  ThisLine$ = VarName$ + " = " + CHR$(34)
  Bin$ = ""

  DEF SEG = BufferSeg
  FOR i = 0 TO SegLen - 1
    Bin$ = Bin$ + CHR$(PEEK(i))
    IF LEN(Bin$) = 8192 THEN
      DO WHILE LEN(Bin$) > 0
        ThisCode$ = HEX$(ASC(LEFT$(Bin$, 1)))
        IF LEN(ThisCode$) = 1 THEN ThisCode$ = "0" + ThisCode$
        Bin$ = RIGHT$(Bin$, LEN(Bin$) - 1)
        IF LEN(ThisLine$ + ThisCode$) >= LineWidth THEN
          ThisLine$ = ThisLine$ + CHR$(34)
          PRINT #OutFile, ThisLine$
          ThisLine$ = VarName$ + " = " + VarName$ + " + " + CHR$(34) + ThisCode$
        ELSE
          ThisLine$ = ThisLine$ + ThisCode$
        END IF
      LOOP
      Bin$ = ""
    END IF
  NEXT

  DO WHILE LEN(Bin$) > 0
    ThisCode$ = HEX$(ASC(LEFT$(Bin$, 1)))
    IF LEN(ThisCode$) = 1 THEN ThisCode$ = "0" + ThisCode$
    Bin$ = RIGHT$(Bin$, LEN(Bin$) - 1)
    IF LEN(ThisLine$ + ThisCode$) >= LineWidth THEN
      ThisLine$ = ThisLine$ + CHR$(34)
      PRINT #OutFile, ThisLine$
      ThisLine$ = VarName$ + " = " + VarName$ + " + " + CHR$(34) + ThisCode$
    ELSE
      ThisLine$ = ThisLine$ + ThisCode$
    END IF
  LOOP

  ThisLine$ = ThisLine$ + CHR$(34)
  PRINT #OutFile, ThisLine$
  CLOSE #OutFile

END SUB

SUB ShowHelp

  PRINT "Syntax: OBJRIP [/bin] [/var:varname$] [/width:nn] objfile [,outfile] [,symfile]"
  PRINT
  PRINT "/bin       Binary output (will assume string output if not given)"
  PRINT "/var       Variable name (string output only, default is op$)"
  PRINT "/width     Maximum line width (string output only, default is 70)"
  PRINT "objfile    OMF object file (.OBJ default)"
  PRINT "outfile    Binary or string output file (.BIN or .BAS default)"
  PRINT "symfile    Public symbol list file (binary output only, default is .TXT)"
  PRINT
  PRINT "Note: Object files with multiple segments are not supported."
  END

END SUB

SUB SortSym (SymName$(), SymOff(), SymCount)

  FOR i = 0 TO SymCount - 2
    IF SymOff(i) > SymOff(i + 1) THEN
      SWAP SymOff(i), SymOff(i + 1)
      SWAP SymName$(i), SymName$(i + 1)
      i = -1
    END IF
  NEXT

END SUB

SUB SaveSym (SymFile$, ObjFile$, OutFile$, SymName$(), SymOff(), SymCount)

  IF SymCount = 0 THEN EXIT SUB

  SymFile = FREEFILE
  OPEN SymFile$ FOR OUTPUT AS #SymFile

  PRINT #SymFile, "Generated by ObjRip 1.0"
  PRINT #SymFile, "Original object file: " + ObjFile$
  PRINT #SymFile, "Ripped binary file: " + OutFile$
  PRINT #SymFile, "Public symbols: " + LTRIM$(STR$(SymCount))
  PRINT #SymFile, ""
  PRINT #SymFile, "Name                             Offset"
  PRINT #SymFile, "----------------------------------------------"
  FOR i = 0 TO SymCount - 1
    DecOffset$ = LTRIM$(STR$(SymOff(i)))
    DecOffset$ = SPACE$(5 - LEN(DecOffset$)) + DecOffset$
    HexOffset$ = HEX$(SymOff(i))
    HexOffset$ = "(" + STRING$(4 - LEN(HexOffset$), "0") + HexOffset$ + "h)"
    Offset$ = DecOffset$ + " " + HexOffset$
    IF LEN(SymName$(i)) < 33 THEN
      PRINT #SymFile, SymName$(i) + SPACE$(33 - LEN(SymName$(i))) + Offset$
    ELSE
      PRINT #SymFile, SymName$(i) + "  " + Offset$
    END IF
  NEXT
  PRINT #SymFile, ""
  PRINT #SymFile, "NOTE: Offsets are valid only for the ripped binary
  PRINT #SymFile, "      file, not the original object file!"
  CLOSE #SymFile

END SUB
