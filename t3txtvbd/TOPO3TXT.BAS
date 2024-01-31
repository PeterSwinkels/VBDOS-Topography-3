DEFINT A-Z
OPTION EXPLICIT

CONST FALSE = 0
CONST TRUE = -1

TYPE DTAStr
 Reserved AS STRING * 20
 Attribute AS INTEGER
 FileTime AS INTEGER
 FileDate AS INTEGER
 FileSize AS LONG
 FileName AS STRING * 13
END TYPE

TYPE RegTypeX
 AX AS INTEGER
 bx AS INTEGER
 CX AS INTEGER
 DX AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 Flags AS INTEGER
 DS AS INTEGER
 es AS INTEGER
END TYPE

DECLARE FUNCTION Choose (Prompt AS STRING, Choices AS STRING) AS STRING
DECLARE FUNCTION DecodeCharacter (Encoded AS STRING) AS STRING
DECLARE FUNCTION DecodeText (Text AS STRING, UnencodedCount) AS STRING
DECLARE FUNCTION EncodeCharacter (Character AS STRING) AS STRING
DECLARE FUNCTION EncodeText (Text AS STRING) AS STRING
DECLARE FUNCTION GetExtension (FileName AS STRING) AS STRING
DECLARE FUNCTION IsCharacterCode (Encoded AS STRING)
DECLARE FUNCTION IsTextFile (FileName AS STRING)
DECLARE FUNCTION ReverseSearch (Text AS STRING, Character AS STRING)
DECLARE SUB DecodeFile (InputFile AS STRING, UnencodeCount)
DECLARE SUB EncodeFile (FileName AS STRING)
DECLARE SUB Main ()

CALL Main

FUNCTION Choose (Prompt AS STRING, Choices AS STRING) AS STRING
DIM Choice AS STRING

 PRINT Prompt;

 DO
  Choice = UCASE$(INPUT$(1))
 LOOP UNTIL INSTR(UCASE$(Choices), Choice) > 0
 PRINT " "; Choice;

 Choose = Choice
END FUNCTION

FUNCTION DecodeCharacter (Encoded AS STRING) AS STRING
DIM Buffer AS STRING
DIM CharacterCode
DIM Decoded AS STRING
DIM Position

 Decoded = ""
 Buffer = Encoded
 IF LEFT$(Buffer, 1) = "[" THEN
  Position = INSTR(Buffer, "]")
  IF Position > 0 THEN
   Buffer = MID$(Buffer, 2, Position - 2)
   IF IsCharacterCode(Buffer) THEN
    CharacterCode = VAL(Buffer)
    SELECT CASE CharacterCode
     CASE 9, 13, IS > 31
      Decoded = CHR$(CharacterCode)
    END SELECT
   END IF
  END IF
 END IF

 DecodeCharacter = Decoded
END FUNCTION

SUB DecodeFile (InputFile AS STRING, UnencodeCount)
DIM Continue
DIM Extension AS STRING
DIM InputFileH
DIM OutputFile AS STRING
DIM OutputFileH
DIM Text AS STRING

 InputFileH = FREEFILE
 OPEN InputFile FOR INPUT LOCK READ WRITE AS InputFileH
  LINE INPUT #InputFileH, Extension
  OutputFile = LEFT$(InputFile, LEN(InputFile) - LEN(GetExtension(InputFile))) + Extension

  Continue = FALSE
  IF DIR$(OutputFile) = "" THEN
   Continue = TRUE
  ELSE
   Continue = (Choose(OutputFile + " already exists. Overwrite y/n?", "yn") = "Y")
  END IF

  IF Continue THEN
   PRINT "Decoding "; InputFile; " -> "; OutputFile
   OutputFileH = FREEFILE
   OPEN OutputFile FOR OUTPUT LOCK READ WRITE AS OutputFileH
    DO UNTIL EOF(InputFileH)
     LINE INPUT #InputFileH, Text
     Text = DecodeText(Text, UnencodeCount)
     PRINT #OutputFileH, Text
    LOOP
   CLOSE OutputFileH
  END IF
 CLOSE InputFileH
END SUB

FUNCTION DecodeText (Text AS STRING, UnencodedCount) AS STRING
DIM Character AS STRING
DIM Decoded AS STRING
DIM InputBuffer AS STRING
DIM OutputBuffer AS STRING
DIM Position

 InputBuffer = Text
 OutputBuffer = ""

 DO UNTIL InputBuffer = ""
  Character = LEFT$(InputBuffer, 1)
  IF Character = "[" THEN
   Position = INSTR(InputBuffer, "]")
   IF Position > 0 THEN
    Decoded = DecodeCharacter(InputBuffer)
    IF Decoded = "" THEN
     OutputBuffer = OutputBuffer + LEFT$(InputBuffer, Position)
    ELSE
     OutputBuffer = OutputBuffer + Decoded$
    END IF
    InputBuffer = MID$(InputBuffer, Position + 1)
   END IF
  ELSE
   OutputBuffer = OutputBuffer + Character
   SELECT CASE ASC(Character)
    CASE 0 TO 8, 10 TO 12, 14 TO 31, 128 TO 255
     UnencodedCount = UnencodedCount + 1
   END SELECT

   InputBuffer = MID$(InputBuffer, 2)
  END IF
 LOOP

 DecodeText = OutputBuffer
END FUNCTION

FUNCTION EncodeCharacter (Character AS STRING) AS STRING
DIM Encoded AS STRING

 Encoded = ""

 SELECT CASE ASC(Character)
  CASE IS > 127
   Encoded = "[" + LTRIM$(STR$(ASC(Character))) + "]"
  CASE 9, 13, IS > 31
   Encoded = Character
 END SELECT

 EncodeCharacter = Encoded
END FUNCTION

SUB EncodeFile (InputFile AS STRING)
DIM Continue
DIM Extension AS STRING
DIM InputFileH
DIM OutputFile AS STRING
DIM OutputFileH
DIM Text AS STRING

 IF NOT IsTextFile(InputFile) THEN
  Extension = GetExtension(InputFile)
  IF NOT Extension = "" THEN
   InputFileH = FREEFILE
   OPEN InputFile FOR INPUT LOCK READ WRITE AS InputFileH
    OutputFile = LEFT$(InputFile, LEN(InputFile) - LEN(Extension)) + "TXT"

    Continue = FALSE
    IF DIR$(OutputFile) = "" THEN
     Continue = TRUE
    ELSE
     Continue = (Choose(OutputFile + " already exists. Overwrite y/n?", "yn") = "Y")
    END IF

    IF Continue THEN
     PRINT "Encoding "; InputFile; " -> "; OutputFile
     OutputFileH = FREEFILE
     OPEN OutputFile FOR OUTPUT LOCK READ WRITE AS OutputFileH
      PRINT #OutputFileH, Extension
      DO UNTIL EOF(InputFileH)
       LINE INPUT #InputFileH, Text
       Text = EncodeText(Text)
       PRINT #OutputFileH, Text
      LOOP
     CLOSE OutputFileH
    END IF
   CLOSE InputFileH
  END IF
 END IF
END SUB

FUNCTION EncodeText (Text AS STRING) AS STRING
DIM Encoded AS STRING
DIM OutputBuffer AS STRING
DIM Position

 OutputBuffer = ""
 Position = 1
 DO UNTIL Position > LEN(Text)
  Encoded = EncodeCharacter(MID$(Text, Position, 1))
  IF Encoded = "" THEN PRINT "Unsupported character code"; ASC(MID$(Text, Position, 1))
  OutputBuffer = OutputBuffer + Encoded
  Position = Position + 1
 LOOP

 EncodeText = OutputBuffer
END FUNCTION

FUNCTION GetExtension (FileName AS STRING) AS STRING
DIM Extension AS STRING
DIM Position

 Extension = ""
 Position = ReverseSearch(FileName, ".")
 IF Position > 0 THEN Extension = MID$(FileName, Position + 1)

 GetExtension = UCASE$(Extension)
END FUNCTION

FUNCTION IsCharacterCode (Encoded AS STRING)
DIM CharacterCode

 CharacterCode = VAL(Encoded)
 IsCharacterCode = (Encoded = LTRIM$(STR$(CharacterCode)) AND (CharacterCode >= 0 AND CharacterCode <= 255))
END FUNCTION

FUNCTION IsTextFile (FileName AS STRING)
 IsTextFile = (GetExtension(FileName) = "TXT")
END FUNCTION

SUB Main ()
DIM InputFile AS STRING
DIM UnencodeCount

 InputFile = COMMAND$

 IF InputFile = "" THEN
  PRINT "Specify an input file."
 ELSE
  IF IsTextFile(InputFile) THEN
   UnencodeCount = 0
   DecodeFile InputFile, UnencodeCount
   IF UnencodeCount > 0 THEN PRINT "Warning: unencoded special characters found."
  ELSE
   EncodeFile InputFile
  END IF
 END IF
END SUB

FUNCTION ReverseSearch (Text AS STRING, Character AS STRING)
DIM Position

 Position = LEN(Text)

 DO UNTIL Position = 0
  IF MID$(Text, Position, 1) = Character THEN
   EXIT DO
  ELSE
   Position = Position - 1
  END IF
 LOOP

 ReverseSearch = Position
END FUNCTION

