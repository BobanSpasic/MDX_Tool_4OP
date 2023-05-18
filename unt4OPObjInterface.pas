{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic

 Unit description:
 Methods to manipulate banks and voices as objects

}

unit unt4OPObjInterface;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, unt4OPVoice, unt4OPBank, untUtils,
  untSysExUtils, HlpHashFactory;

procedure GetVoices(aStream: TMemoryStream; var aList: TStringList);
procedure SplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string);
procedure RipMidiQuest(aStream: TMemoryStream; aOutDir: string);
procedure XSplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string; aLevel: integer);
procedure JoinVCED2VMEM(aInputDir: string; aOutFile: string);
function Hash2Name(aFile: string; aLevel: Integer): boolean;
function Test_VCEDHash(aFile: string; aLevel: integer): string;
function Test_VMEMHash(aFile: string; aLevel: integer; aVoiceNr: integer = 1): string;
function Voice2Name(aFile: string): boolean;
function CheckVMEMIntegrity(aStream: TMemoryStream; aPos: integer;
  var aNullVoice: boolean): integer;
function CheckVCEDIntegrity(aStream: TMemoryStream; aPos: integer;
  var aNullVoice: boolean): integer;
procedure NormalizeVMEM(aStream: TMemoryStream; aPos: integer; aFile: string);
procedure NormalizeVCED(aStream: TMemoryStream; aPos: integer; aFile: string);
function MultiVCED2VMEM(aFileName: string; const Report: TStrings): boolean;
function Transcribe(aFile: string): boolean;

implementation

procedure GetVoices(aStream: TMemoryStream; var aList: TStringList);
var
  fBank: T4OPBankContainer;
  i: integer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
begin
  fBank := T4OPBankContainer.Create;
  aList.Clear;
  iPos := -1;
  if PosBytes(abSysExID, aStream) >= 0 then
  begin
    aStream.Position := 3;
    tmpByte := aStream.ReadByte;
    if tmpByte = 9 then
    begin
      iPos := 6;
      aList.Add('    VMEM header found');
    end;
  end
  else
  begin
    if aStream.Size = 4096 then
    begin
      iPos := 0;
      aList.Add('    Headerless file');
    end;
  end;
  if iPos <> -1 then
  begin
    fBank.LoadBankFromStream(aStream, iPos);
    for i := 1 to 32 do
      aList.Add(Format('%.2d', [i]) + ': ' + fBank.GetVoiceName(i));
  end
  else
    aList.Add('    Not a 4OP VMEM file');

  fBank.Free;
end;

function Hash2Name(aFile: string; aLevel: integer): boolean;
var
  fVoice: T4OPVoiceContainer;
  fStream: TMemoryStream;
  fDirectory: string;
  sPart: string;
  sNPart: string;
  i: integer;
begin
  Result := False;
  fVoice := T4OPVoiceContainer.Create;
  fStream := TMemoryStream.Create;
  fStream.LoadFromFile(aFile);
  try
    if fVoice.Load_Voice_FromStream(fStream, 0) then
    begin
      fDirectory := IncludeTrailingPathDelimiter(ExtractFileDir(aFile));
      if fDirectory = PathDelim then fDirectory := '';
      sPart := fDirectory + fVoice.CalculateHash(aLevel);
      sNPart := sPart;
      i := 0;
      if aFile <> sPart + '.syx' then
      begin
        while FileExists(sNPart + '.syx') do
        begin
          Inc(i);
          sNPart := sPart + '_' + Format('%.2d', [i]);
        end;
        Result := RenameFile(aFile, sNPart + '.syx');
        if Result then
          WriteLn('File ' + aFile + ' renamed to ' + sNPart + '.syx')
        else
          WriteLn('File ' + aFile + ' failed to rename');
      end
      else
        WriteLn('File ' + aFile + ' will not be renamed');
    end
    else
      WriteLn('    Not a 4OP VCED file');
  finally
    fStream.Free;
    fVoice.Free;
  end;
end;

function Voice2Name(aFile: string): boolean;
var
  fVoice: T4OPVoiceContainer;
  i: integer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
  fStream: TMemoryStream;
  fDirectory: string;
begin
  Result := False;
  fVoice := T4OPVoiceContainer.Create;
  fStream := TMemoryStream.Create;
  fStream.LoadFromFile(aFile);
  iPos := -1;
  if PosBytes(abSysExID, fStream) >= 0 then
  begin
    fStream.Position := 3;
    tmpByte := fStream.ReadByte;
    if tmpByte = 0 then
    begin
      iPos := 6;
      WriteLn('    VCED header found');
    end;
  end
  else
  begin
    if fStream.Size = 155 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  try
    if iPos <> -1 then
    begin
      fVoice.Load_VCED_FromStream(fStream, iPos);
      fDirectory := IncludeTrailingPathDelimiter(ExtractFileDir(aFile));
      if fDirectory = PathDelim then fDirectory := '';
      //do not overwrite the file with same name, add a number to the file name
      i := 0;
      if not FileExists(fDirectory + GetValidFileName(fVoice.GetVoiceName) + '.syx') then
        RenameFile(aFile, fDirectory + GetValidFileName(fVoice.GetVoiceName) + '.syx')
      else
        while Result = False do
        begin
          Inc(i);
          if not FileExists(fDirectory + GetValidFileName(fVoice.GetVoiceName) +
            '_' + IntToStr(i) + '.syx') then
          begin
            RenameFile(aFile, fDirectory + GetValidFileName(fVoice.GetVoiceName) +
              '_' + IntToStr(i) + '.syx');
            Result := True;
          end;
        end;
      Result := True;
    end
    else
      WriteLn('    Not a 4OP VCED file');
  finally
    fStream.Free;
    fVoice.Free;
  end;
end;

procedure SplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string);
var
  fBank: T4OPBankContainer;
  fVoice: T4OPVoiceContainer;
  msVoice: TMemoryStream;
  i: integer;
  sVoiceName: string;
  slVoices: TStringList;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
begin
  fBank := T4OPBankContainer.Create;
  iPos := -1;
  if PosBytes(abSysExID, aStream) >= 0 then
  begin
    aStream.Position := 3;
    tmpByte := aStream.ReadByte;
    if tmpByte = 9 then
    begin
      iPos := 6;
      WriteLn('    VMEM header found');
    end;
  end
  else
  begin
    if aStream.Size = 4096 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  if iPos <> -1 then
  begin
    fBank.LoadBankFromStream(aStream, iPos);
    slVoices := TStringList.Create;
    for i := 1 to 32 do
    begin
      sVoiceName := IncludeTrailingPathDelimiter(aOutDir) +
        GetValidFileName(fBank.GetVoiceName(i)) + '.4OPvced.syx';
      WriteLn('Create :' + sVoiceName);
      fVoice := T4OPVoiceContainer.Create;
      fBank.GetVoice(i, fVoice);
      slVoices.AddPair(GetValidFileName(fBank.GetVoiceName(i)), fBank.GetVoiceName(i));
      msVoice := TMemoryStream.Create;
      fVoice.SysExVoiceToStream(1, msVoice);
      msVoice.SaveToFile(sVoiceName);
      msVoice.Free;
      fVoice.Free;
    end;
    slVoices.SaveToFile(IncludeTrailingPathDelimiter(aOutDir) + 'voices.lst');
    slVoices.Free;
  end
  else
    WriteLn('    Not a 4OP VMEM file');

  fBank.Free;
end;

procedure RipMidiQuest(aStream: TMemoryStream; aOutDir: string);
var
  abBODY: array[0..3] of byte = ($42, $4F, $44, $59);
  abFORM: array[0..3] of byte = ($46, $4F, $52, $4D);
  abID: array[0..1] of byte = ($F0, $43);
  msVCED: TMemoryStream;
  msHeadless: TMemoryStream;
  msTmp: TMemoryStream;
  iCounter: integer;
  iPosBODY: integer;
  iPosFORM: integer;
  iPosF0: integer;
  iPosF7: integer;
  checksum: integer;
  i: integer;
begin
  msVCED := TMemoryStream.Create;
  msHeadless := TMemoryStream.Create;
  iCounter := 1;
  iPosBODY := -1;
  WriteLn('Stream size ' + IntToStr(aStream.Size));
  while iPosBODY < aStream.Size do
  begin
    iPosBODY := PosBytes(abBODY, aStream, iPosBODY + 1);
    WriteLn('iPosBODY: ' + IntToStr(iPosBODY));
    if iPosBODY > -1 then
    begin
      iPosFORM := PosBytes(abFORM, aStream, iPosBODY + 1);
      WriteLn('iPosFORM: ' + IntToStr(iPosFORM));
      if iPosForm <> -1 then
      begin
        msTmp := TMemoryStream.Create;
        aStream.Position := iPosBODY;
        msTmp.CopyFrom(aStream, iPosFORM - iPosBODY);
        WriteLn('msTmp.Size: ' + IntToStr(msTmp.Size));
        iPosF0 := PosBytes(abID, msTmp);
        WriteLn('iPosF0: ' + IntToStr(iPosF0));
        if iPosF0 <> -1 then //it is VCED+ACED
        begin
          iPosF7 := PosBytes($F7, msTmp, iPosF0);
          WriteLn('1st iPosF7: ' + IntToStr(iPosF7));
          while iPosF7 < msTmp.Size do
          begin
            if PosBytes($F7, msTmp, iPosF7 + 1) <> -1 then
              iPosF7 := PosBytes($F7, msTmp, iPosF7 + 1)
            else
              break;
          end;
          WriteLn('Last iPosF7: ' + IntToStr(iPosF7));
          msTmp.Position := iPosF0;
          msVCED.CopyFrom(msTmp, iPosF7 - iPosF0 + 1);
          msVCED.SaveToFile(aOutDir + Format('%.6d', [iCounter]) + '.syx');
          msVCED.Clear;
          Inc(iCounter);
        end
        else
        begin
          aStream.Position := iPosBODY + 14;
          if aStream.Position + 93 < aStream.Size then
          begin
            msVCED.WriteByte($F0);
            msVCED.WriteByte($43);
            msVCED.WriteByte($00);
            msVCED.WriteByte($03);
            msVCED.WriteByte($00);
            msVCED.WriteByte($5D);

            msHeadless.CopyFrom(aStream, 93);
            msHeadless.Position := 0;

            msVCED.CopyFrom(msHeadless, 93);

            msHeadless.Position := 0;
            checksum := 0;
            for i := 1 to 93 do
              checksum := checksum + msHeadless.ReadByte;
            checksum := ((not (checksum and 255)) and 127) + 1;

            msVCED.WriteByte(checksum);
            msVCED.WriteByte($F7);
            //msHeadless.SaveToFile(aOutDir + Format('%.6d', [iCounter]) + '.hsyx');
            msVCED.SaveToFile(aOutDir + Format('%.6d', [iCounter]) + '.syx');
            msHeadless.Clear;
            msVCED.Clear;
            Inc(iCounter);
          end;
        end;
        msTmp.Free;
      end;
    end
    else
      break;
  end;
  msVCED.Free;
  msHeadless.Free;
end;

procedure XSplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string; aLevel: integer);
var
  fBank: T4OPBankContainer;
  fVoice: T4OPVoiceContainer;
  msVoice: TMemoryStream;
  i: integer;
  sVoiceName: string;
  slVoices: TStringList;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
begin
  fBank := T4OPBankContainer.Create;
  iPos := -1;
  if PosBytes(abSysExID, aStream, 0) >= 0 then
  begin
    aStream.Position := 3;
    tmpByte := aStream.ReadByte;
    if tmpByte = 4 then
    begin
      iPos := 6;
      WriteLn('    VMEM header found');
    end;
  end
  else
  begin
    if aStream.Size = 4096 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  if iPos <> -1 then
  begin
    fBank.LoadBankFromStream(aStream, iPos);
    slVoices := TStringList.Create;
    for i := 1 to 32 do
    begin
      {WriteLn('Voice name :"'+fBank.GetVoiceName(i)+'"');
      sVoiceName := IncludeTrailingPathDelimiter(aOutDir) +
        fBank.CalculateHash(i) + '.4OPvced.syx';
      WriteLn('Create :' + sVoiceName);     }

      fVoice := T4OPVoiceContainer.Create;
      fBank.GetVoice(i, fVoice);
      WriteLn('Voice name :"' + fVoice.GetVoiceName + '"');
      WriteLn('HasACED  = ' + BooltoStr(fVoice.HasACED, true));
      WriteLn('HasACED2 = ' + BooltoStr(fVoice.HasACED2, true));
      WriteLn('HasACED3 = ' + BooltoStr(fVoice.HasACED3, true));
      WriteLn('HasDELAY = ' + BooltoStr(fVoice.HasDELAY, true));
      WriteLn('HasEFEDS = ' + BooltoStr(fVoice.HasEFEDS, true));
      sVoiceName := IncludeTrailingPathDelimiter(aOutDir) +
        fVoice.CalculateHash(aLevel) + '.4OPvced.syx';
      WriteLn('Create :' + sVoiceName);
      slVoices.AddPair(fVoice.CalculateHash(aLevel), fVoice.GetVoiceName);

      msVoice := TMemoryStream.Create;
      fVoice.SysExVoiceToStream(1, msVoice);
      msVoice.SaveToFile(sVoiceName);
      msVoice.Free;
      fVoice.Free;
    end;
    slVoices.SaveToFile(IncludeTrailingPathDelimiter(aOutDir) + 'voices.lst');
    slVoices.Free;
  end
  else
    WriteLn('    Not a 4OP VMEM file');
  fBank.Free;
end;

procedure JoinVCED2VMEM(aInputDir: string; aOutFile: string);
var
  slVoices: TStringList;
  i, j: integer;
  fBank: T4OPBankContainer;
  fVoice: T4OPVoiceContainer;
  msVoice: TMemoryStream;
  //iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  iCount: integer;
  iBankCount: integer;
  slList: TStringList;
  tmpByte: byte;
  sOutFile: string;
begin
  slVoices := TStringList.Create;
  FindSYX(aInputDir, slVoices);
  fBank := T4OPBankContainer.Create;
  fVoice := T4OPVoiceContainer.Create;
  if FileExists(IncludeTrailingPathDelimiter(aInputDir) + 'voices.lst') then
  begin
    slList := TStringList.Create;
    slList.LoadFromFile(IncludeTrailingPathDelimiter(aInputDir) + 'voices.lst');
    WriteLn('Joining bank according to voices.lst');
    for i := 0 to slList.Count - 1 do
    begin
      iCount := -1;
      for j := 0 to slVoices.Count - 1 do
      begin
        if pos(slList.Names[i], slVoices[j]) > 0 then
          iCount := j;
      end;
      if iCount <> -1 then
      begin
        msVoice := TMemoryStream.Create;
        msVoice.LoadFromFile(IncludeTrailingPathDelimiter(aInputDir) + slVoices[iCount]);

        //iPos := -1;
        if PosBytes(abSysExID, msVoice) >= 0 then
        begin
          msVoice.Position := 3;
          tmpByte := msVoice.ReadByte;
          if tmpByte = 3 then
          begin
            WriteLn(slVoices[iCount] + ' - VCED header found');
            fVoice.Load_Voice_FromStream(msVoice, 0);
            fBank.SetVoice(i + 1, fVoice);
          end;
        end
        else
          WriteLn(slVoices[iCount] + ' - Not a 4OP VCED file');
        msVoice.Free;
      end;
    end;
    fBank.SaveBankToSysExFile(aOutFile);
  end
  else
  begin
    if slVoices.Count < 32 then iBankCount := 0
    else
      iBankCount := 1;
    i := 0;
    while i < slVoices.Count do
    begin
      iCount := (i + 1) mod 32;
      msVoice := TMemoryStream.Create;
      msVoice.LoadFromFile(IncludeTrailingPathDelimiter(aInputDir) + slVoices[i]);
      //iPos := -1;
      if PosBytes(abSysExID, msVoice) >= 0 then
      begin
        msVoice.Position := 3;
        tmpByte := msVoice.ReadByte;
        if tmpByte = 3 then
        begin
          WriteLn(slVoices[iCount] + ' - VCED header found');
          fVoice.Load_Voice_FromStream(msVoice, 0);
          if iCount = 0 then iCount := 32;
          fBank.SetVoice(iCount, fVoice);
        end;
      end
      else
        WriteLn(slVoices[iCount] + ' - Not a 4OP VCED file');
      msVoice.Free;
      Inc(i);
      if (iCount = 32) or (i = slVoices.Count) then
      begin
        if iBankCount = 0 then
          fBank.SaveBankToSysExFile(aOutFile)
        else
        begin
          sOutFile := ExtractFileNameWithoutExt(aOutFile) + '_' +
            Format('%.3d', [iBankCount]) + '.syx';
          fBank.SaveBankToSysExFile(sOutFile);
          Inc(iBankCount);
          fBank.InitBank;
        end;
      end;
    end;
  end;
  fBank.Free;
  fVoice.Free;
  slVoices.Free;
end;

function CheckVMEMIntegrity(aStream: TMemoryStream; aPos: integer;
  var aNullVoice: boolean): integer;
var
  fBank: T4OPBankContainer;
  fVoice: T4OPVoiceContainer;
  i: integer;
  msRebuilt: TMemoryStream;
  bLoadSaveCheck: boolean;
  bMinMaxCheck: boolean;
  slCorruptedVoices: TStringList;
  slReport: TStringList;
begin
  Result := 0;
  bLoadSaveCheck := True;
  bMinMaxCheck := True;
  aNullVoice := False;

  msRebuilt := TMemoryStream.Create;
  slCorruptedVoices := TStringList.Create;
  slReport := TStringList.Create;
  fBank := T4OPBankContainer.Create;
  fVoice := T4OPVoiceContainer.Create;

  fBank.LoadBankFromStream(aStream, aPos);

  //check if the same after loading and saving again
  //because the VMEM to VCED conversion does some sanity checks
  for i := 1 to 32 do
  begin
    fBank.GetVoice(i, fVoice);
    fVoice.Save_VMEM_ToStream(msRebuilt);
  end;
  aStream.Position := aPos;
  msRebuilt.Position := 0;
  for i := 1 to 4096 do
  begin
    if aStream.ReadByte <> msRebuilt.ReadByte then bLoadSaveCheck := False;
  end;

  //check if the parameters are between min and max values
  for i := 1 to 32 do
  begin
    fBank.GetVoice(i, fVoice);
    slReport.Clear;
    if not fVoice.CheckMinMax(slReport) then
    begin
      bMinMaxCheck := False;
      Inc(Result, slReport.Count);
      slCorruptedVoices.Add(Format('%.2d', [i]) + ': ' + fVoice.GetVoiceName);
      slCorruptedVoices.AddStrings(slReport);
      //check for Nulls in voice name
      if fVoice.HasNullInName then
      begin
        aNullVoice := True;
        slCorruptedVoices.Add('*The voice name contains Null bytes*');
      end;
      slCorruptedVoices.Add('');
    end;
  end;

  if bLoadSaveCheck = False then
  begin
    WriteLn('File corruption:');
    WriteLn('  Bank contains data outside the required bits');
    Result := Result + 10000;
  end;

  if bMinMaxCheck = False then
  begin
    WriteLn('File corruption:');
    WriteLn('  The following voices have data outside the minimum/maximum parameter limits:');
    for i := 0 to slCorruptedVoices.Count - 1 do
      WriteLn('    ' + slCorruptedVoices[i]);
  end;

  msRebuilt.Free;
  fBank.Free;
  fVoice.Free;
  slReport.Free;
  slCorruptedVoices.Free;
end;

function CheckVCEDIntegrity(aStream: TMemoryStream; aPos: integer;
  var aNullVoice: boolean): integer;
var
  fVoice: T4OPVoiceContainer;
  i: integer;
  msRebuilt: TMemoryStream;
  slReport: TStringList;
begin
  Result := 0;
  aNullVoice := False;

  msRebuilt := TMemoryStream.Create;
  slReport := TStringList.Create;
  fVoice := T4OPVoiceContainer.Create;

  //Todo - wrong. aced etc also need to be loaded
  fVoice.Load_VCED_FromStream(aStream, aPos);

  //check if the parameters are between min and max values
  slReport.Clear;
  if not fVoice.CheckMinMax(slReport) then
  begin
    WriteLn('File corruption:');
    WriteLn('  The voice has parameters outside the min/max values');
    for i := 0 to slReport.Count - 1 do
    begin
      WriteLn('    ' + slReport[i]);
      Inc(Result);
    end;
    WriteLn('');
  end;

  //check for Nulls in voice name
  if fVoice.HasNullInName then
  begin
    aNullVoice := True;
    WriteLn('File corruption:');
    WriteLn('  The voice name contains Null bytes');
  end;

  msRebuilt.Free;
  fVoice.Free;
  slReport.Free;
end;

procedure NormalizeVMEM(aStream: TMemoryStream; aPos: integer; aFile: string);
var
  fBank: T4OPBankContainer;
  fVoice: T4OPVoiceContainer;
  sOutName: string;
  msOutFile: TMemoryStream;
  i: integer;
begin
  sOutName := ExtractFileName(aFile);
  sOutName := ExtractFileNameWithoutExt(sOutName);
  sOutName := IncludeTrailingPathDelimiter(ExtractFileDir(aFile)) +
    sOutName + '.normalized.syx';

  msOutFile := TMemoryStream.Create;

  fBank := T4OPBankContainer.Create;
  fVoice := T4OPVoiceContainer.Create;

  fBank.LoadBankFromStream(aStream, aPos);

  for i := 1 to 32 do
  begin
    fBank.GetVoice(i, fVoice);
    fVoice.Normalize;
    fBank.SetVoice(i, fVoice);
  end;
  fBank.SysExBankToStream(1, msOutFile);
  msOutFile.SaveToFile(sOutName);

  msOutFile.Free;
  fBank.Free;
  fVoice.Free;
end;

procedure NormalizeVCED(aStream: TMemoryStream; aPos: integer; aFile: string);
var
  fVoice: T4OPVoiceContainer;
  sOutName: string;
  msOutFile: TMemoryStream;
begin
  sOutName := ExtractFileName(aFile);
  sOutName := ExtractFileNameWithoutExt(sOutName);
  sOutName := IncludeTrailingPathDelimiter(ExtractFileDir(aFile)) +
    sOutName + '.normalized.syx';

  msOutFile := TMemoryStream.Create;

  fVoice := T4OPVoiceContainer.Create;

  fVoice.Load_VCED_FromStream(aStream, aPos);
  fVoice.Normalize;

  fVoice.SysExVoiceToStream(1, msOutFile);
  msOutFile.SaveToFile(sOutName);

  msOutFile.Free;
  fVoice.Free;
end;

function Test_VCEDHash(aFile: string; aLevel: integer): string;
var
  fVoice: T4OPVoiceContainer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
  fStream: TMemoryStream;
  fTmpStream: TMemoryStream;
  i: integer;
  bStreamCheck: boolean;
begin
  Result := '0';
  fVoice := T4OPVoiceContainer.Create;
  fStream := TMemoryStream.Create;
  fStream.LoadFromFile(aFile);
  iPos := -1;
  if PosBytes(abSysExID, fStream) >= 0 then
  begin
    fStream.Position := 3;
    tmpByte := fStream.ReadByte;
    if tmpByte = 0 then
    begin
      iPos := 6;
      WriteLn('    VCED header found');
    end;
  end
  else
  begin
    if fStream.Size = 155 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  try
    if iPos <> -1 then
    begin
      Writeln('Stream data before loading:');
      WriteLn(SysExStreamToStr(fStream));
      fVoice.Load_VCED_FromStream(fStream, iPos);
      fTmpStream := TMemoryStream.Create;
      fVoice.Add_VCED_ToStream(fTmpStream);
      Writeln('Stream data after saving:');
      WriteLn(SysExStreamToStr(fTmpStream));
      if (fStream.Size - 8) = fTmpStream.Size then
      begin
        bStreamCheck := True;
        fTmpStream.Position := 0;
        //cut off the headers in a dirty way
        fStream.Position := 6;
        for i := 0 to fStream.Size - 9 do
          if fStream.ReadByte <> fTmpStream.ReadByte then bStreamCheck := False;
        if bStreamCheck then WriteLn('Stream check PASS')
        else
          WriteLn('Stream check FAIL');
      end
      else
        WriteLn('Load and save streams not the same size');
      Result := fVoice.CalculateHash(aLevel);
      WriteLn('Hash summ from record: ' + Result);
      fTmpStream.SetSize(144);
      fTmpStream.Position := 0;
      WriteLn('Hash summ from stream: ' +
        THashFactory.TCrypto.CreateSHA2_256().ComputeStream(fTmpStream).ToString());
      fTmpStream.Free;
    end
    else
      WriteLn('    Not a 4OP VCED file');
  finally
    fStream.Free;
    fVoice.Free;
  end;
end;

function Test_VMEMHash(aFile: string; aLevel: integer; aVoiceNr: integer = 1): string;
var
  fBank: T4OPBankContainer;
  fVoice: T4OPVoiceContainer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
  fStream: TMemoryStream;
  fTmpStream: TMemoryStream;
begin
  Result := '0';

  fStream := TMemoryStream.Create;
  fStream.LoadFromFile(aFile);

  fBank := T4OPBankContainer.Create;
  iPos := -1;
  if PosBytes(abSysExID, fStream) >= 0 then
  begin
    fStream.Position := 3;
    tmpByte := fStream.ReadByte;
    if tmpByte = 9 then
    begin
      iPos := 6;
      WriteLn('    VMEM header found');
    end;
  end
  else
  begin
    if fStream.Size = 4096 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  if iPos <> -1 then
  begin
    fBank.LoadBankFromStream(fStream, iPos);
    fTmpStream := TMemoryStream.Create;
    fVoice := T4OPVoiceContainer.Create;
    fBank.GetVoice(aVoiceNr, fVoice);
    Writeln('Loading voice ' + IntToStr(aVoiceNr) + ': ' + fVoice.GetVoiceName);
    fVoice.Add_VCED_ToStream(fTmpStream);
    Writeln('Stream data after loading from VMEM to VCED:');
    WriteLn(SysExStreamToStr(fTmpStream));
    Result := fVoice.CalculateHash(aLevel);
    WriteLn('Hash summ from record: ' + Result);
    fTmpStream.SetSize(144);
    fTmpStream.Position := 0;
    WriteLn('Hash summ from stream: ' +
      THashFactory.TCrypto.CreateSHA2_256().ComputeStream(fTmpStream).ToString());
    fTmpStream.Free;
    fVoice.Free;
  end
  else
    WriteLn('    Not a 4OP VMEM file');
  fBank.Free;
  fStream.Free;
end;

function MultiVCED2VMEM(aFileName: string; const Report: TStrings): boolean;
var
  fBank: T4OPBankContainer;
  fVoice: T4OPVoiceContainer;
  msFile: TMemoryStream;
  i: integer;
  iVoiceNr: integer;
begin
  Result := False;
  fBank := T4OPBankContainer.Create;
  fVoice := T4OPVoiceContainer.Create;
  msFile := TMemoryStream.Create;
  msFile.LoadFromFile(aFileName);
  if msFile.Size = 4544 then
  begin
    Report.Add('Multi-VCED file found. Converting to VMEM');
    i := 0;
    iVoiceNr := 1;
    while i < 4544 do
    begin
      fVoice.Load_Voice_FromStream(msFile, i);
      fBank.SetVoice(iVoiceNr, fVoice);
      Inc(i, 142);
      Inc(iVoiceNr);
    end;
    Result := True;
    fBank.SaveBankToSysExFile(ExtractFileNameWithoutExt(aFileName) + '.vmem.syx');
  end
  else
    Report.Add('Not a Multi-VCED');
  fBank.Free;
  fVoice.Free;
end;

function Transcribe(aFile: string): boolean;
var
  fVoice: T4OPVoiceContainer;
  msFile: TMemoryStream;
begin
  Result := False;
  fVoice := T4OPVoiceContainer.Create;
  msFile := TMemoryStream.Create;
  msFile.LoadFromFile(aFile);
  if (msFile.Size = 101) or (msFile.Size = 142) or (msFile.Size = 170) then
  begin
    Result := fVoice.Load_Voice_FromStream(msFile, 0);
    if Result then
    begin
      fVoice.SysExVoiceToStream(1, msFile);
      msFile.SaveToFile(ExtractFileNameWithoutExt(aFile) + '.fat.syx');
    end;
  end;
  msFile.Free;
  fVoice.Free;
end;

end.
