{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic

 Unit description:
 Class implementing 4OP Voice Bank Data and related functions for 32 Voices.
}

unit unt4OPBank;

{$mode ObjFPC}{$H+}


interface

uses
  Classes, SysUtils, TypInfo, unt4OPVoice;

type
  T4OP_VMEM_BankDump = array [1..32] of T4OP_V50_VMEM_Params;

type
  T4OPBankContainer = class(TPersistent)
  private
    F4OPBankParams: array [1..32] of T4OPVoiceContainer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetVoice(aVoiceNr: integer; var F4OPVoice: T4OPVoiceContainer): boolean;
    function SetVoice(aVoiceNr: integer; var F4OPVoice: T4OPVoiceContainer): boolean;
    procedure LoadBankFromStream(var aStream: TMemoryStream; Position: integer);
    function GetVoiceName(aVoiceNr: integer): string;
    function SaveBankToSysExFile(aFile: string): boolean;
    function GetChecksum: integer;
    procedure InitBank;
    procedure SysExBankToStream(aCh: integer; var aStream: TMemoryStream);
  end;

implementation

constructor T4OPBankContainer.Create;
var
  i: integer;
begin
  inherited;
  for i := 1 to 32 do
  begin
    F4OPBankParams[i] := T4OPVoiceContainer.Create;
    F4OPBankParams[i].InitVoice;
  end;
end;

destructor T4OPBankContainer.Destroy;
var
  i: integer;
begin
  for i := 32 downto 1 do
    if Assigned(F4OPBankParams[i]) then
      F4OPBankParams[i].Destroy;
  inherited;
end;

function T4OPBankContainer.GetVoice(aVoiceNr: integer;
  var F4OPVoice: T4OPVoiceContainer): boolean;
begin
  if (aVoiceNr > 0) and (aVoiceNr < 33) then
  begin
    if Assigned(F4OPBankParams[aVoiceNr]) then
    begin
      F4OPVoice.Set_VMEM_Params(F4OPBankParams[aVoiceNr].Get_VMEM_Params);
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

function T4OPBankContainer.SetVoice(aVoiceNr: integer;
  var F4OPVoice: T4OPVoiceContainer): boolean;
begin
  if (aVoiceNr > 0) and (aVoiceNr < 33) then
  begin
    F4OPBankParams[aVoiceNr].Set_VMEM_Params(F4OPVoice.Get_VMEM_Params);
    Result := True;
  end
  else
    Result := False;
end;

procedure T4OPBankContainer.LoadBankFromStream(var aStream: TMemoryStream;
  Position: integer);
var
  j: integer;
begin
  if (Position < aStream.Size) and ((aStream.Size - Position) > 4096) then   //????
    aStream.Position := Position
  else
    Exit;
  try
    for  j := 1 to 32 do
    begin
      if assigned(F4OPBankParams[j]) then
        F4OPBankParams[j].Load_VMEM_FromStream(aStream, aStream.Position);
    end;
  except

  end;
end;

function T4OPBankContainer.GetVoiceName(aVoiceNr: integer): string;
begin
  if (aVoiceNr > 0) and (aVoiceNr < 33) then
    Result := F4OPBankParams[aVoiceNr].GetVoiceName
  else
    Result := '';
end;

function T4OPBankContainer.SaveBankToSysExFile(aFile: string): boolean;
var
  tmpStream: TMemoryStream;
  i: integer;
begin
  tmpStream := TMemoryStream.Create;
  try
    Result := True;
    tmpStream.WriteByte($F0);
    tmpStream.WriteByte($43);
    tmpStream.WriteByte($00);
    tmpStream.WriteByte($04);
    tmpStream.WriteByte($20);
    tmpStream.WriteByte($00);
    for i := 1 to 32 do
      if F4OPBankParams[i].Save_VMEM_ToStream(tmpStream) = False then
        Result := False;
    tmpStream.WriteByte(GetChecksum);
    tmpStream.WriteByte($F7);
    if Result = True then
      tmpStream.SaveToFile(aFile);
  finally
    tmpStream.Free;
  end;
end;

function T4OPBankContainer.GetChecksum: integer;
var
  i: integer;
  checksum: integer;
begin
  checksum := 0;
  try
    for i := 1 to 32 do
      checksum := checksum + F4OPBankParams[i].GetChecksumPart;
    Result := ((not (checksum and 255)) and 127) + 1;
  except
    on e: Exception do Result := 0;
  end;
end;

procedure T4OPBankContainer.InitBank;
var
  i: integer;
begin
  for i := 1 to 32 do
  begin
    F4OPBankParams[i].InitVoice;
  end;
end;

procedure T4OPBankContainer.SysExBankToStream(aCh: integer; var aStream: TMemoryStream);
var
  i: integer;
  FCh: byte;
begin
  FCh := aCh - 1;
  aStream.Clear;
  aStream.Position := 0;
  aStream.WriteByte($F0);
  aStream.WriteByte($43);
  aStream.WriteByte($00 + FCh);
  aStream.WriteByte($04);
  aStream.WriteByte($20);
  aStream.WriteByte($00);
  for i := 1 to 32 do
    F4OPBankParams[i].Save_VMEM_ToStream(aStream);
  aStream.WriteByte(GetChecksum);
  aStream.WriteByte($F7);
end;

end.
