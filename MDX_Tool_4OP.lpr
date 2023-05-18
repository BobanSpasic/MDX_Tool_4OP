{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic

 Program description:
 This program can get some info from Yamaha 4OP SysEx files. The info is more about the integrity/corruption of the files.
 Second aspect of the program is to repair some of the common corrupted files found on the internet.
 Third aspect (not yet implemented) will be the conversion from VMEM to VCED and vice-versa.
}

program MDX_Tool_4OP;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
                   {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  untUtils,
  untDXUtils,
  unt4OPObjInterface;

type

  { TMDX_Tool_4OP }

  TMDX_Tool_4OP = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TMDX_Tool_4OP }

  procedure TMDX_Tool_4OP.DoRun;
  var
    ErrorMsg: string;
    fInput: string;
    fOutputDir: string;
    slReport: TStringList;
    msInputFile: TMemoryStream;
    i: integer;
    iStartPos: integer;
    iDmpPos: integer;
    bNullVoice: boolean;
    iErrCode: integer;
    sLevel: string;
    iLevel: integer;
  begin
    bNullVoice := False;
    fInput := '';
    // quick check parameters
    ErrorMsg := CheckOptions('him::nzywrvcsx::jqtf:d:',
      'help info hname:: vname normalize markcorr marknull repair voices crop split xsplit:: join quest transcribe file: dir:');

    if ErrorMsg <> '' then
    begin
      WriteHelp;
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    if (ParamCount = 0) or HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('f', 'file') then
      fInput := GetOptionValue('f', 'file');

    if HasOption('r', 'repair') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end;

      slReport := TStringList.Create;

      WriteLn('Repairing file ' + ExtractFileName(fInput));
      if trim(ExtractFileDir(fInput)) = '' then
        fInput := IncludeTrailingPathDelimiter(GetCurrentDir) + fInput;
      if Repair4OPSysEx(fInput, slReport) then
      begin
        for i := 0 to slReport.Count - 1 do
          WriteLn(slReport[i]);
      end
      else
      if MultiVCED2VMEM(fInput, slReport) then
      begin
        for i := 0 to slReport.Count - 1 do
          WriteLn(slReport[i]);
      end
      else
        WriteLn('No defects recognized');

      slReport.Free;
    end;

    if HasOption('i', 'info') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        slReport := TStringList.Create;
        msInputFile := TMemoryStream.Create;
        msInputFile.LoadFromFile(fInput);
        iStartPos := 0;
        iDmpPos := 0;

        if ContainsDX_FourOP_Data_New(msInputFile, iStartPos, slReport) then
        begin
          for i := 0 to slReport.Count - 1 do
            WriteLn(slReport[i]);
          iStartPos := 0;
          if Contains4OPBankDump(msInputFile, iStartPos, iDmpPos) then
            if CheckVMEMIntegrity(msInputFile, iDmpPos, bNullVoice) = 0 then
              WriteLn('VMEM data integrity is OK');
          iStartPos := 0;
          if Contains4OPVoiceDump(msInputFile, iStartPos, iDmpPos) then
            if CheckVCEDIntegrity(msInputFile, iDmpPos, bNullVoice) = 0 then
              WriteLn('VCED data integrity is OK');
        end
        else
        begin
          for i := 0 to slReport.Count - 1 do
            WriteLn(slReport[i]);
        end;

        msInputFile.Free;
        slReport.Free;
      end;
    end;

    if HasOption('y', 'markcorr') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        slReport := TStringList.Create;
        msInputFile := TMemoryStream.Create;
        msInputFile.LoadFromFile(fInput);
        iStartPos := 0;
        iDmpPos := 0;

        if ContainsDX_FourOP_Data_New(msInputFile, iStartPos, slReport) then
        begin
          for i := 0 to slReport.Count - 1 do
            WriteLn(slReport[i]);
          iStartPos := 0;
          if Contains4OPBankDump(msInputFile, iStartPos, iDmpPos) then
          begin
            iErrCode := CheckVMEMIntegrity(msInputFile, iDmpPos, bNullVoice);
            if iErrCode <> 0 then
            begin
              WriteLn('VMEM data is corrupted. Renaming file');
              if bNullVoice then
                RenameFile(fInput, fInput + '.nl_' + IntToStr(iErrCode) + '_corr')
              else
                RenameFile(fInput, fInput + '.' + IntToStr(iErrCode) + '_corr');
            end;
          end;
          iStartPos := 0;
          if Contains4OPVoiceDump(msInputFile, iStartPos, iDmpPos) then
          begin
            iErrCode := CheckVCEDIntegrity(msInputFile, iDmpPos, bNullVoice);
            if iErrCode <> 0 then
            begin
              WriteLn('VCED data is corrupted. Renaming file');
              if bNullVoice then
                RenameFile(fInput, fInput + '.nl_' + IntToStr(iErrCode) + '_corr')
              else
                RenameFile(fInput, fInput + '.' + IntToStr(iErrCode) + '_corr');
            end;
          end;
        end
        else
        begin
          for i := 0 to slReport.Count - 1 do
            WriteLn(slReport[i]);
        end;

        msInputFile.Free;
        slReport.Free;
      end;
    end;

    if HasOption('w', 'marknull') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        slReport := TStringList.Create;
        msInputFile := TMemoryStream.Create;
        msInputFile.LoadFromFile(fInput);
        iStartPos := 0;
        iDmpPos := 0;

        if ContainsDX_FourOP_Data_New(msInputFile, iStartPos, slReport) then
        begin
          for i := 0 to slReport.Count - 1 do
            WriteLn(slReport[i]);
          iStartPos := 0;
          if Contains4OPBankDump(msInputFile, iStartPos, iDmpPos) then
          begin
            iErrCode := CheckVMEMIntegrity(msInputFile, iDmpPos, bNullVoice);
            if iErrCode <> 0 then
            begin
              if bNullVoice then
              begin
                WriteLn('Voice name contains nulls ($00). Renaming file');
                RenameFile(fInput, fInput + '.nl');
              end;
            end;
          end;
          iStartPos := 0;
          if Contains4OPVoiceDump(msInputFile, iStartPos, iDmpPos) then
          begin
            iErrCode := CheckVCEDIntegrity(msInputFile, iDmpPos, bNullVoice);
            if iErrCode <> 0 then
            begin
              if bNullVoice then
              begin
                WriteLn('Voice name contains nulls ($00). Renaming file');
                RenameFile(fInput, fInput + '.nl');
              end;
            end;
          end;
        end
        else
        begin
          for i := 0 to slReport.Count - 1 do
            WriteLn(slReport[i]);
        end;

        msInputFile.Free;
        slReport.Free;
      end;
    end;

    if HasOption('z', 'normalize') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        slReport := TStringList.Create;
        msInputFile := TMemoryStream.Create;
        if trim(ExtractFileDir(fInput)) = '' then
          fInput := IncludeTrailingPathDelimiter(GetCurrentDir) + fInput;
        WriteLn('Input file: ' + fInput);
        msInputFile.LoadFromFile(fInput);
        iStartPos := 0;
        iDmpPos := 0;

        if ContainsDX_FourOP_Data_New(msInputFile, iStartPos, slReport) then
        begin
          for i := 0 to slReport.Count - 1 do
            WriteLn(slReport[i]);
          iStartPos := 0;
          if Contains4OPBankDump(msInputFile, iStartPos, iDmpPos) then
          begin
            if CheckVMEMIntegrity(msInputFile, iDmpPos, bNullVoice) = 0 then
            begin
              WriteLn('VMEM data integrity is OK');
            end
            else
            begin
              NormalizeVMEM(msInputFile, iDmpPos, fInput);
              WriteLn('VMEM data integrity is normalized');
            end;
          end;
          iStartPos := 0;
          if Contains4OPVoiceDump(msInputFile, iStartPos, iDmpPos) then
          begin
            if CheckVCEDIntegrity(msInputFile, iDmpPos, bNullVoice) = 0 then
            begin
              WriteLn('VCED data integrity is OK');
            end
            else
            begin
              NormalizeVCED(msInputFile, iDmpPos, fInput);
              WriteLn('VCED data integrity is normalized');
            end;
          end;
        end
        else
        begin
          for i := 0 to slReport.Count - 1 do
            WriteLn(slReport[i]);
        end;

        msInputFile.Free;
        slReport.Free;
      end;
    end;

    if HasOption('m', 'hname') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        sLevel := GetOptionValue('m', 'hname');
        iLevel := StrToIntDef(sLevel, 3); //default is 3 = DX11 parameter-set
        Hash2Name(fInput, iLevel);
      end;
    end;

    if HasOption('n', 'vname') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        Voice2Name(fInput);
      end;
    end;

    if HasOption('v', 'voices') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        slReport := TStringList.Create;
        msInputFile := TMemoryStream.Create;
        msInputFile.LoadFromFile(fInput);

        GetVoices(msInputFile, slReport);

        for i := 0 to slReport.Count - 1 do
          WriteLn(slReport[i]);

        msInputFile.Free;
        slReport.Free;
      end;
    end;

    if HasOption('c', 'crop') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end;

      if trim(ExtractFileDir(fInput)) = '' then
        fInput := IncludeTrailingPathDelimiter(GetCurrentDir) + fInput;
      if CropHeaders(fInput) then
      begin
        WriteLn('Cropping file ' + ExtractFileName(fInput) + ': OK');
      end
      else
        WriteLn('Cropping file ' + ExtractFileName(fInput) + ': FAIL');
    end;

    if HasOption('s', 'split') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        if not HasOption('d', 'dir') then
        begin
          WriteLn('Parameter -d {directory} is missing');
          Terminate;
          Exit;
        end
        else
        begin
          fOutputDir := IncludeTrailingPathDelimiter(GetOptionValue('d', 'outdir'));
          if pos(':\', fOutputDir) = 0 then
            fOutputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + fOutputDir;
          if not DirectoryExists(fOutputDir) then
            ForceDirectories(fOutputDir);
          msInputFile := TMemoryStream.Create;
          msInputFile.LoadFromFile(fInput);

          SplitVMEM2VCED(msInputFile, fOutputDir);

          msInputFile.Free;
          WriteLn('Done!');
        end;
      end;
    end;

    if HasOption('x', 'xsplit') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        if not HasOption('d', 'dir') then
        begin
          WriteLn('Parameter -d {directory} is missing');
          Terminate;
          Exit;
        end
        else
        begin
          fOutputDir := IncludeTrailingPathDelimiter(GetOptionValue('d', 'outdir'));
          if pos(':\', fOutputDir) = 0 then
            fOutputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + fOutputDir;
          if not DirectoryExists(fOutputDir) then
            ForceDirectories(fOutputDir);
          msInputFile := TMemoryStream.Create;
          msInputFile.LoadFromFile(fInput);

          sLevel := GetOptionValue('x', 'xsplit');
          iLevel := StrToIntDef(sLevel, 3); //default is 3 = DX11 parameter-set
          XSplitVMEM2VCED(msInputFile, fOutputDir, iLevel);

          msInputFile.Free;
          WriteLn('Done!');
        end;
      end;
    end;

    if HasOption('j', 'join') then
    begin
      fOutputDir := IncludeTrailingPathDelimiter(GetOptionValue('d', 'dir'));
      if pos(':\', fOutputDir) = 0 then
        fOutputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + fOutputDir;
      if not DirectoryExists(fOutputDir) then
      begin
        WriteLn('Parameter -d {directory} is missing or the directory could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        if not HasOption('f', 'file') then
        begin
          WriteLn('Parameter -f {filename} is missing');
          Terminate;
          Exit;
        end
        else
        begin
          JoinVCED2VMEM(fOutputDir, fInput);
          WriteLn('Done!');
        end;
      end;
    end;

    if HasOption('q', 'quest') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        if not HasOption('d', 'dir') then
        begin
          WriteLn('Parameter -d {directory} is missing');
          Terminate;
          Exit;
        end
        else
        begin
          fOutputDir := IncludeTrailingPathDelimiter(GetOptionValue('d', 'outdir'));
          if pos(':\', fOutputDir) = 0 then
            fOutputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + fOutputDir;
          if not DirectoryExists(fOutputDir) then
            ForceDirectories(fOutputDir);
          msInputFile := TMemoryStream.Create;
          msInputFile.LoadFromFile(fInput);

          RipMidiQuest(msInputFile, fOutputDir);

          msInputFile.Free;
          WriteLn('Done!');
        end;
      end;
    end;

    if HasOption('t', 'transcribe') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        if Transcribe(fInput) then
          WriteLn('Done!')
        else WriteLn('Fail!')
      end;
    end;

    Terminate;
  end;

  constructor TMDX_Tool_4OP.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMDX_Tool_4OP.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMDX_Tool_4OP.WriteHelp;
  begin
    writeln('');
    writeln('');
    writeln('MDX_Tool_4OP 1.8 - tool for various manipulation of 4OP VMEM and VCED SysEx files');
    writeln('Author: Boban Spasic');
    writeln('https://github.com/BobanSpasic/MDX_Tool');
    writeln('');
    writeln('Usage: ', ExtractFileName(ExeName), ' -parameters');
    writeln('  Parameters (short and long form):');
    writeln('       -h               --help               This help message');
    writeln('       -i               --info               Information');
    writeln('       -v               --voices             Get the voice list from a bank');
    writeln('       -n               --vname              Rename file to the name of the contained voice');
    writeln('       -m {level}       --hname={level}      Rename file to the SHA2-256 hash of the file');
    writeln('       -z               --normalize          Set parameters to be between minimum and maximum allowed values');
    writeln('       -y               --markcorr           Mark files that have parameters outside the min/max limits');
    writeln('                                               Adds .x_corr as the file extension where x is the error code');
    writeln('                                               Lower 4 numbers are the count of min/max errors');
    writeln('                                               Higher numbers are the count of voices with values in normally unused bits');
    writeln('       -w               --marknull           Mark files that have nulls ($00) in voice names');
    writeln('                                               Adds .nl as the file extension');
    writeln('       -r               --repair             Repair/extract 4OP VMEM data from files');
    writeln('                                               Adds _4OP_repaired in the file name');
    writeln('                                               Adds _x_4OP_repaired in the file name,');
    writeln('                                               where the x is the number of the dump');
    writeln('                                               in a multi-dump file');
    writeln('       -c               --crop               Crop headers from the VMEM/VCED files');
    writeln('       -s               --split              Split bank (VMEM) into single voices (VCED)');
    writeln('       -x {level}       --xsplit={level}     Split bank (VMEM) into single voices (VCED)');
    writeln('                                               and take the SHA2-256 hash as a file name.');
    writeln('                                               Voice name (10xASCII) is not a part of the hash');
    writeln('       -j               --join               Join single voices (VCED) into a bank (VMEM)');
    writeln('                                               If the file voices.lst exists inside the input directory');
    writeln('                                               - the voices inside the bank will be sorted according to the list');
    writeln('       -q               --quest              Rip MidiQuest SQL files (tested on MidiQuest 6)');
    writeln('       -t               --transcribe         Convert single VCED Syx-files into "Fat SYX", 269-bytes files');
    writeln('');
    writeln('       -f {filename}    --file={filename}    Input file (or output file for -j parameter)');
    writeln('       -d {directory}   --dir={directory}    Output directory for -s and -x parameters');
    writeln('                                               Input directory for -j parameter');
    writeln('                                               If it does not contain a drive letter, a sub-directory in');
    writeln('                                               the current directory will be created.');
    writeLn('');
    writeLn('       {level} - optional parameter          which parameter-set is used for calculating hash sum');
    writeLn('                                             1 = DX21, 2 = TX81z, 3 = DX11, 4 = V50');
    writeLn('                                             if not specified, the default value 3 (DX11) is used');
    writeLn('');
    writeLn('  At splitting the bank into VCEDs, the VCEDs are written in a form that contains all the possible 4OP voice-parameters');
    writeLn('  It means, even if a bank was from a DX100, after splitting it will also contain V50 ACED3 set with the default/init values');
    writeLn('  Such "Fat SYX" is needed for comparing files - search for duplicates');
    writeLn('');
    writeln('  Example usage:');
    writeln('       MDX_Tool -i -f my_dx_file.syx');
    writeln('       MDX_Tool -r -f my_dx_file.syx');
    writeln('       MDX_Tool -s -f my_dx_file.syx -d new_directory');
    writeln('       MDX_Tool -j -f my_new_bank.syx -d directory_with_VCEDs');
    writeln('       MDX_Tool --info --file=my_dx_file.syx');
    writeLn('');
    writeLn('');
    writeLn('Split and Join parameters expect non-corrupted files as input (headerless files are accepted).');
  end;

var
  Application: TMDX_Tool_4OP;
begin
  Application := TMDX_Tool_4OP.Create(nil);
  Application.Title := 'MDX_Tool_4OP';
  Application.Run;
  Application.Free;
end.
