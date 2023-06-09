{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic

 Unit description:
 Class implementing 4OP Voice Data and related functions for one Voice.
}

unit unt4OPVoice;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, HlpHashFactory, SysUtils, untParConst, untUtils, Math;

type
  T4OP_VCED_Dump = array [0..93] of byte;    //DX21
  T4OP_ACED_Dump = array [0..22] of byte;    //TX81z
  T4OP_ACED2_Dump = array [0..9] of byte;    //DX11/V50
  T4OP_ACED3_Dump = array [0..19] of byte;   //V50/WT11
  T4OP_DELAY_Dump = array [0..1] of byte;    //DS55
  T4OP_EFEDS_Dump = array [0..2] of byte;    //YS
  T4OP_VMEM_Dump = array [0..72] of byte;    //DX21
  T4OP_AVMEM_Dump = array [0..10] of byte;   //TX81z add to DX21 (73 - 83)
  T4OP_AVMEM3_Dump = array [0..43] of byte;  //DX11/V50 add to TX81z (84-127)

type
  T4OP_VCED_Params = packed record
    case boolean of
      True: (params: T4OP_VCED_Dump);
      False: (
        OP4_Attack_Rate: byte;               //       0-31
        OP4_Decay_1_Rate: byte;              //       0-31
        OP4_Decay_2_Rate: byte;              //       0-31
        OP4_Release_Rate: byte;              //       1-15
        OP4_Decay_1_Level: byte;             //       0-15
        OP4_Level_Scaling: byte;             //0-99 (DX11/V50 have LS sign in ACED2)
        OP4_Rate_Scaling: byte;              //       0-3
        OP4_EG_Bias_Sens: byte;              //       0-7
        OP4_AM_Enable: byte;                 //       0-1
        OP4_Key_Vel_Sens: byte;              //0-7 (DX11/V50 have 8-14 for -7 to -1)
        OP4_OP_Output_Level: byte;           //       0-99
        OP4_Frequency: byte;                 //       0-63
        OP4_Detune: byte;                    //       0-6 (center = 3)

        OP2_Attack_Rate: byte;               //       0-31
        OP2_Decay_1_Rate: byte;              //       0-31
        OP2_Decay_2_Rate: byte;              //       0-31
        OP2_Release_Rate: byte;              //       1-15
        OP2_Decay_1_Level: byte;             //       0-15
        OP2_Level_Scaling: byte;             //       0-99
        OP2_Rate_Scaling: byte;              //       0-3
        OP2_EG_Bias_Sens: byte;              //       0-7
        OP2_AM_Enable: byte;                 //       0-1
        OP2_Key_Vel_Sens: byte;              //       0-7
        OP2_OP_Output_Level: byte;           //       0-99
        OP2_Frequency: byte;                 //       0-63
        OP2_Detune: byte;                    //       0-6 (center = 3)

        OP3_Attack_Rate: byte;               //       0-31
        OP3_Decay_1_Rate: byte;              //       0-31
        OP3_Decay_2_Rate: byte;              //       0-31
        OP3_Release_Rate: byte;              //       1-15
        OP3_Decay_1_Level: byte;             //       0-15
        OP3_Level_Scaling: byte;             //       0-99
        OP3_Rate_Scaling: byte;              //       0-3
        OP3_EG_Bias_Sens: byte;              //       0-7
        OP3_AM_Enable: byte;                 //       0-1
        OP3_Key_Vel_Sens: byte;              //       0-7
        OP3_OP_Output_Level: byte;           //       0-99
        OP3_Frequency: byte;                 //       0-63
        OP3_Detune: byte;                    //       0-6 (center = 3)

        OP1_Attack_Rate: byte;               //       0-31
        OP1_Decay_1_Rate: byte;              //       0-31
        OP1_Decay_2_Rate: byte;              //       0-31
        OP1_Release_Rate: byte;              //       1-15
        OP1_Decay_1_Level: byte;             //       0-15
        OP1_Level_Scaling: byte;             //       0-99
        OP1_Rate_Scaling: byte;              //       0-3
        OP1_EG_Bias_Sens: byte;              //       0-7
        OP1_AM_Enable: byte;                 //       0-1
        OP1_Key_Vel_Sens: byte;              //       0-7
        OP1_OP_Output_Level: byte;           //       0-99
        OP1_Frequency: byte;                 //       0-63
        OP1_Detune: byte;                    //       0-6 (center = 3)

        Algorithm: byte;                     //       0-7
        Feedback: byte;                      //       0-7
        LFO_Speed: byte;                     //       0-99
        LFO_Delay: byte;                     //       0-99
        Pitch_Mod_Depth: byte;               //       0-99
        Amplitude_Mod_Depth: byte;           //       0-99
        LFO_Sync: byte;                      //       0-1
        LFO_Wave: byte;                      //       0-3
        Pitch_Mod_Sens: byte;                //       0-7
        Amplitude_Mod_Sens: byte;            //       0-3
        Transpose: byte;                     //       0-48 (center = 24)
        Poly_Mono: byte;                     //       0-1
        Pitch_Bend_Range: byte;              //       0-12
        Portamento_Mode: byte;               //       0-1
        Portamento_Time: byte;               //       0-99
        FC_Volume: byte;                     //       0-99
        Sustain: byte;                       //       0-1
        Portamento: byte;                    //       0-1
        Chorus: byte;                        //       0-1 (Set=0)
        MW_Pitch: byte;                      //       0-99
        MW_Amplitude: byte;                  //       0-99
        BC_Pitch: byte;                      //       0-99
        BC_Amplitude: byte;                  //       0-99
        BC_Pitch_Bias: byte;                 //       0-99 (center=50)
        BC_EG_Bias: byte;                    //       0-99
        VOICE_NAME_CHAR_1: byte;             //       ASCII
        VOICE_NAME_CHAR_2: byte;             //       ASCII
        VOICE_NAME_CHAR_3: byte;             //       ASCII
        VOICE_NAME_CHAR_4: byte;             //       ASCII
        VOICE_NAME_CHAR_5: byte;             //       ASCII
        VOICE_NAME_CHAR_6: byte;             //       ASCII
        VOICE_NAME_CHAR_7: byte;             //       ASCII
        VOICE_NAME_CHAR_8: byte;             //       ASCII
        VOICE_NAME_CHAR_9: byte;             //       ASCII
        VOICE_NAME_CHAR_10: byte;            //       ASCII
        Pitch_EG_Rate_1: byte;               //       0-99  PEG
        Pitch_EG_Rate_2: byte;               //       0-99
        Pitch_EG_Rate_3: byte;               //       0-99
        Pitch_EG_Level_1: byte;              //       0-99
        Pitch_EG_Level_2: byte;              //       0-99
        Pitch_EG_Level_3: byte;              //       0-99
        OPERATOR_ON_OFF: byte;               //       0-15
      );
  end;

  T4OP_ACED_Params = packed record
    case boolean of
      True: (params: T4OP_ACED_Dump);
      False: (
        OP4_Fixed_Freq: byte;                //       0-1
        OP4_Fixed_Freq_Range: byte;          //       0-7
        OP4_Freq_Range_Fine: byte;           //       0-15
        OP4_Operator_Waveform: byte;         //       0-7
        OP4_EG_Shift: byte;                  //       0-3

        OP2_Fixed_Freq: byte;                //       0-1
        OP2_Fixed_Freq_Range: byte;          //       0-7
        OP2_Freq_Range_Fine: byte;           //       0-15
        OP2_Operator_Waveform: byte;         //       0-7
        OP2_EG_Shift: byte;                  //       0-3

        OP3_Fixed_Freq: byte;                //       0-1
        OP3_Fixed_Freq_Range: byte;          //       0-7
        OP3_Freq_Range_Fine: byte;           //       0-15
        OP3_Operator_Waveform: byte;         //       0-7
        OP3_EG_Shift: byte;                  //       0-3

        OP1_Fixed_Freq: byte;                //       0-1
        OP1_Fixed_Freq_Range: byte;          //       0-7
        OP1_Freq_Range_Fine: byte;           //       0-15
        OP1_Operator_Waveform: byte;         //       0-7
        OP1_EG_Shift: byte;                  //       0-3

        Reverb_Rate: byte;                   //       0-7
        FC_Pitch: byte;                      //       0-99
        FC_Amplitude: byte;                  //       0-99
      );
  end;

  T4OP_ACED2_Params = packed record
    case boolean of
      True: (params: T4OP_ACED2_Dump);
      False: (
        AT_Pitch: byte;                      //       0-99
        AT_Amplitude: byte;                  //       0-99
        AT_Pitch_Bias: byte;                 //       0-100 (center=50)
        AT_EG_Bias: byte;                    //       0-99
        OP4_Fix_Range_Mode: byte;            //       0-1   (0=Hi, 1=Lo)
        OP2_Fix_Range_Mode: byte;            //       0-1
        OP3_Fix_Range_Mode: byte;            //       0-1
        OP1_Fix_Range_Mode: byte;            //       0-1
        LS_Sign: byte;                       //       0-15
        Reserved: byte;
      );
  end;

  T4OP_ACED3_Params = packed record
    case boolean of
      True: (params: T4OP_ACED3_Dump);
      False: (
        Effect_Select: byte;                 //       0-32
        Balance: byte;                       //       0-100
        Out_Level: byte;                     //       0-100
        Stereo_mix: byte;                    //       0-1
        Effect_Param_1: byte;                //       0-75
        Effect_Param_2: byte;                //       0-99
        Effect_Param_3: byte;                //       0-99
        Reserved_40: byte;
        Reserved_41: byte;
        Reserved_42: byte;
        Reserved_43: byte;
        Reserved_44: byte;
        Reserved_45: byte;
        Reserved_46: byte;
        Reserved_47: byte;
        Reserved_48: byte;
        Reserved_49: byte;
        Reserved_50: byte;
        Reserved_51: byte;
        Reserved_52: byte;
      );
  end;

  T4OP_DELAY_Params = packed record
    case boolean of
      True: (params: T4OP_DELAY_Dump);
      False: (
        Delay_SW: byte;
        Long_Short: byte;
      );
  end;

  T4OP_EFEDS_Params = packed record
    case boolean of
      True: (params: T4OP_EFEDS_Dump);
      False: (
        Preset: byte;
        Time: byte;
        Balance: byte;
      );
  end;

  T4OP_VMEM_Params = packed record
    case boolean of
      True: (params: T4OP_VMEM_Dump);
      False: (
        OP4_Attack_Rate: byte;               //       0-31
        OP4_Decay_1_Rate: byte;              //       0-31
        OP4_Decay_2_Rate: byte;              //       0-31
        OP4_Release_Rate: byte;              //       1-15
        OP4_Decay_1_Level: byte;             //       0-15
        OP4_Level_Scaling: byte;             //       0-99
        OP4_AME_EBS_KVS: byte;               //       0-1,0-7,0-7
        OP4_OP_Output_Level: byte;           //       0-99
        OP4_Frequency: byte;                 //       0-63
        OP4_LS2_KVS2_RS_DET: byte;           //       0-1,0-1,0-3,0-6

        OP2_Attack_Rate: byte;               //       0-31
        OP2_Decay_1_Rate: byte;              //       0-31
        OP2_Decay_2_Rate: byte;              //       0-31
        OP2_Release_Rate: byte;              //       1-15
        OP2_Decay_1_Level: byte;             //       0-15
        OP2_Level_Scaling: byte;             //       0-99
        OP2_AME_EBS_KVS: byte;               //       0-1,0-7,0-7
        OP2_OP_Output_Level: byte;           //       0-99
        OP2_Frequency: byte;                 //       0-63
        OP2_LS2_KVS2_RS_DET: byte;           //       0-1,0-1,0-3,0-6

        OP3_Attack_Rate: byte;               //       0-31
        OP3_Decay_1_Rate: byte;              //       0-31
        OP3_Decay_2_Rate: byte;              //       0-31
        OP3_Release_Rate: byte;              //       1-15
        OP3_Decay_1_Level: byte;             //       0-15
        OP3_Level_Scaling: byte;             //       0-99
        OP3_AME_EBS_KVS: byte;               //       0-1,0-7,0-7
        OP3_OP_Output_Level: byte;           //       0-99
        OP3_Frequency: byte;                 //       0-63
        OP3_LS2_KVS2_RS_DET: byte;           //       0-1,0-1,0-3,0-6

        OP1_Attack_Rate: byte;               //       0-31
        OP1_Decay_1_Rate: byte;              //       0-31
        OP1_Decay_2_Rate: byte;              //       0-31
        OP1_Release_Rate: byte;              //       1-15
        OP1_Decay_1_Level: byte;             //       0-15
        OP1_Level_Scaling: byte;             //       0-99
        OP1_AME_EBS_KVS: byte;               //       0-1,0-7,0-7
        OP1_OP_Output_Level: byte;           //       0-99
        OP1_Frequency: byte;                 //       0-63
        OP1_LS2_KVS2_RS_DET: byte;           //       0-1,0-1,0-3,0-6

        SY_FBL_ALG: byte;                    //       0--1,0-7,0-7
        LFO_Speed: byte;                     //       0-99
        LFO_Delay: byte;                     //       0-99
        Pitch_Mod_Depth: byte;               //       0-99
        Amplitude_Mod_Depth: byte;           //       0-99
        PMS_AMS_LFW: byte;                   //       0-7,0-3,0-3
        Transpose: byte;                     //       0-48
        Pitch_Bend_Range: byte;              //       0-12
        CH_MO_SU_PO_PM: byte;                //       0-1,0-1,0-1,0-1,0-1
        Portamento_Time: byte;               //       0-99
        FC_Volume: byte;                     //       0-99
        MW_Pitch: byte;                      //       0-99
        MW_Amplitude: byte;                  //       0-99
        BC_Pitch: byte;                      //       0-99
        BC_Amplitude: byte;                  //       0-99
        BC_Pitch_Bias: byte;                 //       0-99 (center=50)
        BC_EG_Bias: byte;                    //       0-99
        VOICE_NAME_CHAR_1: byte;             //       ASCII
        VOICE_NAME_CHAR_2: byte;             //       ASCII
        VOICE_NAME_CHAR_3: byte;             //       ASCII
        VOICE_NAME_CHAR_4: byte;             //       ASCII
        VOICE_NAME_CHAR_5: byte;             //       ASCII
        VOICE_NAME_CHAR_6: byte;             //       ASCII
        VOICE_NAME_CHAR_7: byte;             //       ASCII
        VOICE_NAME_CHAR_8: byte;             //       ASCII
        VOICE_NAME_CHAR_9: byte;             //       ASCII
        VOICE_NAME_CHAR_10: byte;            //       ASCII
        PR1: byte;                           //       0-99 PEG (DX21 only)
        PR2: byte;                           //       0-99 (set=99)
        PR3: byte;                           //       0-99
        PL1: byte;                           //       0-99 (set=50)
        PL2: byte;                           //       0-99
        PL3: byte;                           //       0-99
      );
  end;

  T4OP_AVMEM_Params = packed record
    case boolean of
      True: (params: T4OP_AVMEM_Dump);
      False: (
        OP4_FIXRM_EGSFT_FIX_FIXRG: byte;    // TX81z does not have FIXRM (ACED2)
        OP4_OSW_FINE: byte;                 // TX81z OPW = OSW

        OP2_FIXRM_EGSFT_FIX_FIXRG: byte;
        OP2_OSW_FINE: byte;

        OP3_FIXRM_EGSFT_FIX_FIXRG: byte;
        OP3_OSW_FINE: byte;

        OP1_FIXRM_EGSFT_FIX_FIXRG: byte;
        OP1_OSW_FINE: byte;

        Reverb_Rate: byte;                   //       0-7
        FC_Pitch: byte;                      //       0-99
        FC_Amplitude: byte;                  //       0-99
      );
  end;

  T4OP_AVMEM3_Params = packed record
    case boolean of
      True: (params: T4OP_AVMEM3_Dump);
      False: (
        AT_Pitch: byte;                      //       0-99
        AT_Amplitude: byte;                  //       0-99
        AT_Pitch_Bias: byte;                 //       0-100 (center=50)
        AT_EG_Bias: byte;                    //       0-99
        Reserved_88: byte;
        Reserved_89: byte;
        Reserved_90: byte;                   // DS55 Delay

        Effect_Preset_Nr: byte;              //       0-10 YS-Series
        Effect_Time: byte;                   //       0-40
        Effect_Balance: byte;                //       0-99

        Effect_Select: byte;                 //       0-32
        Balance: byte;                       //       0-100
        Out_Level: byte;                     //       0-100
        Stereo_mix: byte;                    //       0-1
        Effect_Param_1: byte;                //       0-75
        Effect_Param_2: byte;                //       0-99
        Effect_Param_3: byte;                //       0-99
        Reserved_101: byte;
        Reserved_102: byte;
        Reserved_103: byte;
        Reserved_104: byte;
        Reserved_105: byte;
        Reserved_106: byte;
        Reserved_107: byte;
        Reserved_108: byte;
        Reserved_109: byte;
        Reserved_110: byte;
        Reserved_111: byte;
        Reserved_112: byte;
        Reserved_113: byte;
        Reserved_114: byte;
        Reserved_115: byte;
        Reserved_116: byte;
        Reserved_117: byte;
        Reserved_118: byte;
        Reserved_119: byte;
        Reserved_120: byte;
        Reserved_121: byte;
        Reserved_122: byte;
        Reserved_123: byte;
        Reserved_124: byte;
        Reserved_125: byte;
        Reserved_126: byte;
        Reserved_127: byte;
      );
  end;

  T4OP_V50_VCED_Params = packed record
    VCED: T4OP_VCED_Params;
    ACED: T4OP_ACED_Params;
    ACED2: T4OP_ACED2_Params;
    ACED3: T4OP_ACED3_Params;
    DELAY: T4OP_DELAY_Params;
    EFEDS: T4OP_EFEDS_Params;
  end;

  T4OP_V50_VMEM_Params = packed record
    VMEM: T4OP_VMEM_Params;
    AMEM: T4OP_AVMEM_Params;
    AMEM3: T4OP_AVMEM3_Params;
  end;

type
  T4OPVoiceContainer = class(TPersistent)
  private
    F4OP_V50_VCED_Params: T4OP_V50_VCED_Params;
    F4OP_V50_VMEM_Params: T4OP_V50_VMEM_Params;
    FHasACED: boolean;
    FHasACED2: boolean;
    FHasACED3: boolean;
    FHasDELAY: boolean;
    FHasEFEDS: boolean;
  public
    //translation functions are here part of the class because of FHas... properties
    function VCEDtoVMEM(aPar: T4OP_V50_VCED_Params): T4OP_V50_VMEM_Params;
    function VMEMtoVCED(aPar: T4OP_V50_VMEM_Params): T4OP_V50_VCED_Params;

    function Load_Voice_FromStream(var aStream: TMemoryStream; aPos: integer): boolean;
    function Load_VCED_FromStream(var aStream: TMemoryStream; aPos: integer): boolean;
    function Load_ACED_FromStream(var aStream: TMemoryStream; aPos: integer): boolean;
    function Load_ACED2_FromStream(var aStream: TMemoryStream; aPos: integer): boolean;
    function Load_ACED3_FromStream(var aStream: TMemoryStream; aPos: integer): boolean;
    function Load_DELAY_FromStream(var aStream: TMemoryStream; aPos: integer): boolean;
    function Load_EFEDS_FromStream(var aStream: TMemoryStream; aPos: integer): boolean;

    function Load_VMEM_FromStream(var aStream: TMemoryStream;
      aPos: integer): boolean;
    function Save_VMEM_ToStream(var aStream: TMemoryStream): boolean;

    function Add_VCED_ToStream(var aStream: TMemoryStream): boolean;
    function Add_ACED_ToStream(var aStream: TMemoryStream): boolean;
    function Add_ACED2_ToStream(var aStream: TMemoryStream): boolean;
    function Add_ACED3_ToStream(var aStream: TMemoryStream): boolean;
    function Add_DELAY_ToStream(var aStream: TMemoryStream): boolean;
    function Add_EFEDS_ToStream(var aStream: TMemoryStream): boolean;

    procedure InitVoice;
    function GetVoiceName: string;
    function CalculateHash(aLevel: integer = 3): string;
    procedure SysExVoiceToStream(aCh: integer; var aStream: TMemoryStream);
    function GetVCEDChecksum: byte;
    function GetACEDChecksum: byte;
    function GetACED2Checksum: byte;
    function GetACED3Checksum: byte;
    function GetDELAYChecksum: byte;
    function GetEFEDSChecksum: byte;
    function GetChecksumPart: integer;

    function Get_VMEM_Params: T4OP_V50_VMEM_Params;
    function Get_VCED_Params: T4OP_V50_VCED_Params;
    function Set_VMEM_Params(aParams: T4OP_V50_VMEM_Params): boolean;
    function Set_VCED_Params(aParams: T4OP_V50_VCED_Params): boolean;

    function CheckMinMax(var slReport: TStringList): boolean;
    function HasNullInName: boolean;
    procedure Normalize;

    property HasACED: boolean read FHasACED write FHasACED;
    property HasACED2: boolean read FHasACED2 write FHasACED2;
    property HasACED3: boolean read FHasACED3 write FHasACED3;
    property HasDELAY: boolean read FHasDELAY write FHasDELAY;
    property HasEFEDS: boolean read FHasEFEDS write FHasEFEDS;
  end;

implementation

function T4OPVoiceContainer.VCEDtoVMEM(aPar: T4OP_V50_VCED_Params): T4OP_V50_VMEM_Params;
var
  t: T4OP_V50_VMEM_Params;
  sign: integer;
begin
  //first the parameters without conversion
  t.VMEM.OP4_Attack_Rate := aPar.VCED.OP4_Attack_Rate and 31;
  t.VMEM.OP4_Decay_1_Rate := aPar.VCED.OP4_Decay_1_Rate and 31;
  t.VMEM.OP4_Decay_2_Rate := aPar.VCED.OP4_Decay_2_Rate and 31;
  t.VMEM.OP4_Release_Rate := aPar.VCED.OP4_Release_Rate and 15;
  t.VMEM.OP4_Decay_1_Level := aPar.VCED.OP4_Decay_1_Level and 15;
  t.VMEM.OP4_Level_Scaling := aPar.VCED.OP4_Level_Scaling;
  t.VMEM.OP4_OP_Output_Level := aPar.VCED.OP4_OP_Output_Level;
  t.VMEM.OP4_Frequency := aPar.VCED.OP4_Frequency and 63;

  //now parameters with conversion
  if aPar.VCED.OP4_Key_Vel_Sens > 7 then
  begin
    sign := 1;
    t.VMEM.OP4_AME_EBS_KVS :=
      ((aPar.VCED.OP4_AM_Enable and 1) shl 6) +
      ((aPar.VCED.OP4_EG_Bias_Sens and 7) shl 3) +
      (15 - aPar.VCED.OP4_Key_Vel_Sens and 7);
  end
  else
  begin
    sign := 0;
    t.VMEM.OP4_AME_EBS_KVS :=
      ((aPar.VCED.OP4_AM_Enable and 1) shl 6) +
      ((aPar.VCED.OP4_EG_Bias_Sens and 7) shl 3) + (aPar.VCED.OP4_Key_Vel_Sens and 7);
  end;
  t.VMEM.OP4_LS2_KVS2_RS_DET :=
    ((aPar.ACED2.LS_Sign and 1) shl 6) + (sign shl 5) +
    ((aPar.VCED.OP4_Rate_Scaling and 3) shl 3) + (aPar.VCED.OP4_Detune and 7);
  {$IFDEF Debug }
  WriteLn('OP4_LS2_KVS2_RS_DET : ' + IntToStr(t.VMEM.OP4_LS2_KVS2_RS_DET));
  {$ENDIF}

  //first the parameters without conversion
  t.VMEM.OP3_Attack_Rate := aPar.VCED.OP3_Attack_Rate and 31;
  t.VMEM.OP3_Decay_1_Rate := aPar.VCED.OP3_Decay_1_Rate and 31;
  t.VMEM.OP3_Decay_2_Rate := aPar.VCED.OP3_Decay_2_Rate and 31;
  t.VMEM.OP3_Release_Rate := aPar.VCED.OP3_Release_Rate and 15;
  t.VMEM.OP3_Decay_1_Level := aPar.VCED.OP3_Decay_1_Level and 15;
  t.VMEM.OP3_Level_Scaling := aPar.VCED.OP3_Level_Scaling;
  t.VMEM.OP3_OP_Output_Level := aPar.VCED.OP3_OP_Output_Level;
  t.VMEM.OP3_Frequency := aPar.VCED.OP3_Frequency and 63;

  //now parameters with conversion
  if aPar.VCED.OP3_Key_Vel_Sens > 7 then
  begin
    sign := 1;
    t.VMEM.OP3_AME_EBS_KVS :=
      ((aPar.VCED.OP3_AM_Enable and 1) shl 6) +
      ((aPar.VCED.OP3_EG_Bias_Sens and 7) shl 3) +
      (15 - aPar.VCED.OP3_Key_Vel_Sens and 7);
  end
  else
  begin
    sign := 0;
    t.VMEM.OP3_AME_EBS_KVS :=
      ((aPar.VCED.OP3_AM_Enable and 1) shl 6) +
      ((aPar.VCED.OP3_EG_Bias_Sens and 7) shl 3) + (aPar.VCED.OP3_Key_Vel_Sens and 7);
  end;
  t.VMEM.OP3_LS2_KVS2_RS_DET :=
    ((aPar.ACED2.LS_Sign and 2) shl 5) + (sign shl 5) +
    ((aPar.VCED.OP3_Rate_Scaling and 3) shl 3) + (aPar.VCED.OP3_Detune and 7);

  //first the parameters without conversion
  t.VMEM.OP2_Attack_Rate := aPar.VCED.OP2_Attack_Rate and 31;
  t.VMEM.OP2_Decay_1_Rate := aPar.VCED.OP2_Decay_1_Rate and 31;
  t.VMEM.OP2_Decay_2_Rate := aPar.VCED.OP2_Decay_2_Rate and 31;
  t.VMEM.OP2_Release_Rate := aPar.VCED.OP2_Release_Rate and 15;
  t.VMEM.OP2_Decay_1_Level := aPar.VCED.OP2_Decay_1_Level and 15;
  t.VMEM.OP2_Level_Scaling := aPar.VCED.OP2_Level_Scaling;
  t.VMEM.OP2_OP_Output_Level := aPar.VCED.OP2_OP_Output_Level;
  t.VMEM.OP2_Frequency := aPar.VCED.OP2_Frequency and 63;

  //now parameters with conversion
  if aPar.VCED.OP2_Key_Vel_Sens > 7 then
  begin
    sign := 1;
    t.VMEM.OP2_AME_EBS_KVS :=
      ((aPar.VCED.OP2_AM_Enable and 1) shl 6) +
      ((aPar.VCED.OP2_EG_Bias_Sens and 7) shl 3) +
      (15 - aPar.VCED.OP2_Key_Vel_Sens and 7);
  end
  else
  begin
    sign := 0;
    t.VMEM.OP2_AME_EBS_KVS :=
      ((aPar.VCED.OP2_AM_Enable and 1) shl 6) +
      ((aPar.VCED.OP2_EG_Bias_Sens and 7) shl 3) + (aPar.VCED.OP2_Key_Vel_Sens and 7);
  end;
  t.VMEM.OP2_LS2_KVS2_RS_DET :=
    ((aPar.ACED2.LS_Sign and 4) shl 4) + (sign shl 5) +
    ((aPar.VCED.OP2_Rate_Scaling and 3) shl 3) + (aPar.VCED.OP2_Detune and 7);

  //first the parameters without conversion
  t.VMEM.OP1_Attack_Rate := aPar.VCED.OP1_Attack_Rate and 31;
  t.VMEM.OP1_Decay_1_Rate := aPar.VCED.OP1_Decay_1_Rate and 31;
  t.VMEM.OP1_Decay_2_Rate := aPar.VCED.OP1_Decay_2_Rate and 31;
  t.VMEM.OP1_Release_Rate := aPar.VCED.OP1_Release_Rate and 15;
  t.VMEM.OP1_Decay_1_Level := aPar.VCED.OP1_Decay_1_Level and 15;
  t.VMEM.OP1_Level_Scaling := aPar.VCED.OP1_Level_Scaling;
  t.VMEM.OP1_OP_Output_Level := aPar.VCED.OP1_OP_Output_Level;
  t.VMEM.OP1_Frequency := aPar.VCED.OP1_Frequency and 63;

  //now parameters with conversion
  if aPar.VCED.OP1_Key_Vel_Sens > 7 then
  begin
    sign := 1;
    t.VMEM.OP1_AME_EBS_KVS :=
      ((aPar.VCED.OP1_AM_Enable and 1) shl 6) +
      ((aPar.VCED.OP1_EG_Bias_Sens and 7) shl 3) +
      (15 - aPar.VCED.OP1_Key_Vel_Sens and 7);
  end
  else
  begin
    sign := 0;
    t.VMEM.OP1_AME_EBS_KVS :=
      ((aPar.VCED.OP1_AM_Enable and 1) shl 6) +
      ((aPar.VCED.OP1_EG_Bias_Sens and 7) shl 3) + (aPar.VCED.OP1_Key_Vel_Sens and 7);
  end;
  t.VMEM.OP1_LS2_KVS2_RS_DET :=
    ((aPar.ACED2.LS_Sign and 8) shl 3) + (sign shl 5) +
    ((aPar.VCED.OP1_Rate_Scaling and 3) shl 3) + (aPar.VCED.OP1_Detune and 7);

  //global parameters
  t.VMEM.SY_FBL_ALG := ((aPar.VCED.LFO_Sync and 1) shl 6) +
    ((aPar.VCED.Feedback and 7) shl 3) + (aPar.VCED.Algorithm and 7);
  t.VMEM.LFO_Speed := aPar.VCED.LFO_Speed;
  t.VMEM.LFO_Delay := aPar.VCED.LFO_Delay;
  t.VMEM.Pitch_Mod_Depth := aPar.VCED.Pitch_Mod_Depth;
  t.VMEM.Amplitude_Mod_Depth := aPar.VCED.Amplitude_Mod_Depth;
  t.VMEM.PMS_AMS_LFW := ((aPar.VCED.Pitch_Mod_Sens and 7) shl 4) +
    ((aPar.VCED.Amplitude_Mod_Sens and 3) shl 2) + (aPar.VCED.LFO_Wave and 3);
  t.VMEM.Transpose := aPar.VCED.Transpose and 63;
  t.VMEM.Pitch_Bend_Range := aPar.VCED.Pitch_Bend_Range and 15;
  t.VMEM.CH_MO_SU_PO_PM := ((aPar.VCED.Chorus and 1) shl 4) +
    ((aPar.VCED.Poly_Mono and 1) shl 3) + ((aPar.VCED.Sustain and 1) shl 2) +
    ((aPar.VCED.Portamento and 1) shl 1) + (aPar.VCED.Portamento_Mode and 1);
  t.VMEM.Portamento_Time := aPar.VCED.Portamento_Time;
  t.VMEM.FC_Volume := aPar.VCED.FC_Volume;
  t.VMEM.MW_Pitch := aPar.VCED.MW_Pitch;
  t.VMEM.MW_Amplitude := aPar.VCED.MW_Amplitude;
  t.VMEM.BC_Pitch := aPar.VCED.BC_Pitch;
  t.VMEM.BC_Amplitude := aPar.VCED.BC_Amplitude;
  t.VMEM.BC_Pitch_Bias := aPar.VCED.BC_Pitch_Bias;
  t.VMEM.BC_EG_Bias := aPar.VCED.BC_EG_Bias;

  t.VMEM.VOICE_NAME_CHAR_1 := aPar.VCED.VOICE_NAME_CHAR_1;
  t.VMEM.VOICE_NAME_CHAR_2 := aPar.VCED.VOICE_NAME_CHAR_2;
  t.VMEM.VOICE_NAME_CHAR_3 := aPar.VCED.VOICE_NAME_CHAR_3;
  t.VMEM.VOICE_NAME_CHAR_4 := aPar.VCED.VOICE_NAME_CHAR_4;
  t.VMEM.VOICE_NAME_CHAR_5 := aPar.VCED.VOICE_NAME_CHAR_5;
  t.VMEM.VOICE_NAME_CHAR_6 := aPar.VCED.VOICE_NAME_CHAR_6;
  t.VMEM.VOICE_NAME_CHAR_7 := aPar.VCED.VOICE_NAME_CHAR_7;
  t.VMEM.VOICE_NAME_CHAR_8 := aPar.VCED.VOICE_NAME_CHAR_8;
  t.VMEM.VOICE_NAME_CHAR_9 := aPar.VCED.VOICE_NAME_CHAR_9;
  t.VMEM.VOICE_NAME_CHAR_10 := aPar.VCED.VOICE_NAME_CHAR_10;

  t.VMEM.PR1 := aPar.VCED.Pitch_EG_Rate_1;
  t.VMEM.PR2 := aPar.VCED.Pitch_EG_Rate_2;
  t.VMEM.PR3 := aPar.VCED.Pitch_EG_Rate_3;
  t.VMEM.PL1 := aPar.VCED.Pitch_EG_Level_1;
  t.VMEM.PL2 := aPar.VCED.Pitch_EG_Level_2;
  t.VMEM.PL3 := aPar.VCED.Pitch_EG_Level_3;

  t.AMEM.OP4_FIXRM_EGSFT_FIX_FIXRG :=
    ((aPAr.ACED2.OP4_Fix_Range_Mode and 1) shl 6) +
    ((aPar.ACED.OP4_EG_Shift and 3) shl 4) + ((aPar.ACED.OP4_Fixed_Freq and 1) shl 3) +
    (aPar.ACED.OP4_Fixed_Freq_Range and 7);
  t.AMEM.OP4_OSW_FINE := ((aPar.ACED.OP4_Operator_Waveform and 7) shl 4) +
    (aPar.ACED.OP4_Freq_Range_Fine and 15);

  t.AMEM.OP3_FIXRM_EGSFT_FIX_FIXRG :=
    ((aPAr.ACED2.OP3_Fix_Range_Mode and 1) shl 6) +
    ((aPar.ACED.OP3_EG_Shift and 3) shl 4) + ((aPar.ACED.OP3_Fixed_Freq and 1) shl 3) +
    (aPar.ACED.OP3_Fixed_Freq_Range and 7);
  t.AMEM.OP3_OSW_FINE := ((aPar.ACED.OP3_Operator_Waveform and 7) shl 4) +
    (aPar.ACED.OP3_Freq_Range_Fine and 15);

  t.AMEM.OP2_FIXRM_EGSFT_FIX_FIXRG :=
    ((aPAr.ACED2.OP2_Fix_Range_Mode and 1) shl 6) +
    ((aPar.ACED.OP2_EG_Shift and 3) shl 4) + ((aPar.ACED.OP2_Fixed_Freq and 1) shl 3) +
    (aPar.ACED.OP2_Fixed_Freq_Range and 7);
  t.AMEM.OP2_OSW_FINE := ((aPar.ACED.OP2_Operator_Waveform and 7) shl 4) +
    (aPar.ACED.OP2_Freq_Range_Fine and 15);

  t.AMEM.OP1_FIXRM_EGSFT_FIX_FIXRG :=
    ((aPAr.ACED2.OP1_Fix_Range_Mode and 1) shl 6) +
    ((aPar.ACED.OP1_EG_Shift and 3) shl 4) + ((aPar.ACED.OP1_Fixed_Freq and 1) shl 3) +
    (aPar.ACED.OP1_Fixed_Freq_Range and 7);
  t.AMEM.OP1_OSW_FINE := ((aPar.ACED.OP1_Operator_Waveform and 7) shl 4) +
    (aPar.ACED.OP1_Freq_Range_Fine and 15);

  t.AMEM.Reverb_Rate := aPar.ACED.Reverb_Rate and 7;
  t.AMEM.FC_Pitch := aPar.ACED.FC_Pitch and 127;
  t.AMEM.FC_Amplitude := aPar.ACED.FC_Amplitude and 127;

  t.AMEM3.AT_Pitch := aPar.ACED2.AT_Pitch;
  t.AMEM3.AT_Amplitude := aPar.ACED2.AT_Amplitude;
  t.AMEM3.AT_Pitch_Bias := aPar.ACED2.AT_Pitch_Bias;
  t.AMEM3.AT_EG_Bias := aPar.ACED2.AT_EG_Bias;

  t.AMEM3.Reserved_88 := 0;
  t.AMEM3.Reserved_89 := 0;

  t.AMEM3.Reserved_90 := ((aPar.DELAY.Delay_SW and 1) shl 1) + aPar.DELAY.Long_Short;
  //DS55 Delay

  t.AMEM3.Effect_Preset_Nr := aPar.EFEDS.Preset; //YS effects
  t.AMEM3.Effect_Time := aPar.EFEDS.Time;
  t.AMEM3.Effect_Balance := aPar.EFEDS.Balance;

  t.AMEM3.Effect_Select := aPar.ACED3.Effect_Select and 63;
  t.AMEM3.Balance := aPar.ACED3.Balance and 127;
  t.AMEM3.Out_Level := aPar.ACED3.Out_Level and 127;
  t.AMEM3.Stereo_mix := aPar.ACED3.Stereo_mix and 127;
  t.AMEM3.Effect_Param_1 := aPar.ACED3.Effect_Param_1 and 127;
  t.AMEM3.Effect_Param_2 := aPar.ACED3.Effect_Param_2 and 127;
  t.AMEM3.Effect_Param_3 := aPar.ACED3.Effect_Param_3 and 127;
  t.AMEM3.Reserved_101 := 0;
  t.AMEM3.Reserved_102 := 0;
  t.AMEM3.Reserved_103 := 0;
  t.AMEM3.Reserved_104 := 0;
  t.AMEM3.Reserved_105 := 0;
  t.AMEM3.Reserved_106 := 0;
  t.AMEM3.Reserved_107 := 0;
  t.AMEM3.Reserved_108 := 0;
  t.AMEM3.Reserved_109 := 0;
  t.AMEM3.Reserved_110 := 0;
  t.AMEM3.Reserved_111 := 0;
  t.AMEM3.Reserved_112 := 0;
  t.AMEM3.Reserved_113 := 0;
  t.AMEM3.Reserved_114 := 0;
  t.AMEM3.Reserved_115 := 0;
  t.AMEM3.Reserved_116 := 0;
  t.AMEM3.Reserved_117 := 0;
  t.AMEM3.Reserved_118 := 0;
  t.AMEM3.Reserved_119 := 0;
  t.AMEM3.Reserved_120 := 0;
  t.AMEM3.Reserved_121 := 0;
  t.AMEM3.Reserved_122 := 0;
  t.AMEM3.Reserved_123 := 0;
  t.AMEM3.Reserved_124 := 0;
  t.AMEM3.Reserved_125 := 0;
  t.AMEM3.Reserved_126 := 0;
  t.AMEM3.Reserved_127 := 0;

  Result := t;
end;

function T4OPVoiceContainer.VMEMtoVCED(aPar: T4OP_V50_VMEM_Params): T4OP_V50_VCED_Params;
var
  t: T4OP_V50_VCED_Params;
  kvs1, kvs2: byte;
  b0, b1, b2, b3: byte;
begin
  t.VCED.OP4_Attack_Rate := aPar.VMEM.OP4_Attack_Rate and 31;
  t.VCED.OP4_Decay_1_Rate := aPar.VMEM.OP4_Decay_1_Rate and 31;
  t.VCED.OP4_Decay_2_Rate := aPar.VMEM.OP4_Decay_2_Rate and 31;
  t.VCED.OP4_Release_Rate := max(1, aPar.VMEM.OP4_Release_Rate and 15);
  t.VCED.OP4_Decay_1_Level := aPar.VMEM.OP4_Decay_1_Level and 15;
  t.VCED.OP4_Level_Scaling := aPar.VMEM.OP4_Level_Scaling and 127;
  t.VCED.OP4_Rate_Scaling := (aPar.VMEM.OP4_LS2_KVS2_RS_DET shr 3) and 3;
  t.VCED.OP4_EG_Bias_Sens := (aPar.VMEM.OP4_AME_EBS_KVS shr 3) and 7;
  t.VCED.OP4_AM_Enable := (aPar.VMEM.OP4_AME_EBS_KVS shr 6) and 1;
  kvs1 := aPar.VMEM.OP4_AME_EBS_KVS and 7;
  kvs2 := (aPar.VMEM.OP4_LS2_KVS2_RS_DET shr 5) and 1;
  if kvs2 = 0 then
    t.VCED.OP4_Key_Vel_Sens := kvs1
  else
    t.VCED.OP4_Key_Vel_Sens := 15 - kvs1;
  {$IFDEF Debug }
  WriteLn('OP4_KVS : ' + IntToStr(t.VCED.OP4_Key_Vel_Sens));
  {$ENDIF}
  t.VCED.OP4_OP_Output_Level := aPar.VMEM.OP4_OP_Output_Level and 127;
  t.VCED.OP4_Frequency := aPar.VMEM.OP4_Frequency and 63;
  t.VCED.OP4_Detune := aPar.VMEM.OP4_LS2_KVS2_RS_DET and 7;

  t.ACED.OP4_Fixed_Freq := (aPar.AMEM.OP4_FIXRM_EGSFT_FIX_FIXRG shr 3) and 1;
  t.ACED.OP4_Fixed_Freq_Range := aPar.AMEM.OP4_FIXRM_EGSFT_FIX_FIXRG and 7;
  t.ACED.OP4_Freq_Range_Fine := aPar.AMEM.OP4_OSW_FINE and 15;
  t.ACED.OP4_Operator_Waveform := (aPar.AMEM.OP4_OSW_FINE shr 4) and 7;
  t.ACED.OP4_EG_Shift := (aPar.AMEM.OP4_FIXRM_EGSFT_FIX_FIXRG shr 4) and 3;

  t.ACED2.OP4_Fix_Range_Mode := (aPar.AMEM.OP4_FIXRM_EGSFT_FIX_FIXRG shr 6) and 1;

  t.VCED.OP3_Attack_Rate := aPar.VMEM.OP3_Attack_Rate and 31;
  t.VCED.OP3_Decay_1_Rate := aPar.VMEM.OP3_Decay_1_Rate and 31;
  t.VCED.OP3_Decay_2_Rate := aPar.VMEM.OP3_Decay_2_Rate and 31;
  t.VCED.OP3_Release_Rate := max(1, aPar.VMEM.OP3_Release_Rate and 15);
  t.VCED.OP3_Decay_1_Level := aPar.VMEM.OP3_Decay_1_Level and 15;
  t.VCED.OP3_Level_Scaling := aPar.VMEM.OP3_Level_Scaling and 127;
  t.VCED.OP3_Rate_Scaling := (aPar.VMEM.OP3_LS2_KVS2_RS_DET shr 3) and 3;
  t.VCED.OP3_EG_Bias_Sens := (aPar.VMEM.OP3_AME_EBS_KVS shr 3) and 7;
  t.VCED.OP3_AM_Enable := (aPar.VMEM.OP3_AME_EBS_KVS shr 6) and 1;
  kvs1 := aPar.VMEM.OP3_AME_EBS_KVS and 7;
  kvs2 := (aPar.VMEM.OP3_LS2_KVS2_RS_DET shr 5) and 1;
  if kvs2 = 0 then
    t.VCED.OP3_Key_Vel_Sens := kvs1
  else
    t.VCED.OP3_Key_Vel_Sens := 15 - kvs1;
  {$IFDEF Debug }
  WriteLn('OP3_KVS : ' + IntToStr(t.VCED.OP3_Key_Vel_Sens));
  {$ENDIF}
  t.VCED.OP3_OP_Output_Level := aPar.VMEM.OP3_OP_Output_Level and 127;
  t.VCED.OP3_Frequency := aPar.VMEM.OP3_Frequency and 63;
  t.VCED.OP3_Detune := aPar.VMEM.OP3_LS2_KVS2_RS_DET and 7;

  t.ACED.OP3_Fixed_Freq := (aPar.AMEM.OP3_FIXRM_EGSFT_FIX_FIXRG shr 3) and 1;
  t.ACED.OP3_Fixed_Freq_Range := aPar.AMEM.OP3_FIXRM_EGSFT_FIX_FIXRG and 7;
  t.ACED.OP3_Freq_Range_Fine := aPar.AMEM.OP3_OSW_FINE and 15;
  t.ACED.OP3_Operator_Waveform := (aPar.AMEM.OP3_OSW_FINE shr 4) and 7;
  t.ACED.OP3_EG_Shift := (aPar.AMEM.OP3_FIXRM_EGSFT_FIX_FIXRG shr 4) and 3;

  t.ACED2.OP3_Fix_Range_Mode := (aPar.AMEM.OP3_FIXRM_EGSFT_FIX_FIXRG shr 6) and 1;

  t.VCED.OP2_Attack_Rate := aPar.VMEM.OP2_Attack_Rate and 31;
  t.VCED.OP2_Decay_1_Rate := aPar.VMEM.OP2_Decay_1_Rate and 31;
  t.VCED.OP2_Decay_2_Rate := aPar.VMEM.OP2_Decay_2_Rate and 31;
  t.VCED.OP2_Release_Rate := max(1, aPar.VMEM.OP2_Release_Rate and 15);
  t.VCED.OP2_Decay_1_Level := aPar.VMEM.OP2_Decay_1_Level and 15;
  t.VCED.OP2_Level_Scaling := aPar.VMEM.OP2_Level_Scaling and 127;
  t.VCED.OP2_Rate_Scaling := (aPar.VMEM.OP2_LS2_KVS2_RS_DET shr 3) and 3;
  t.VCED.OP2_EG_Bias_Sens := (aPar.VMEM.OP2_AME_EBS_KVS shr 3) and 7;
  t.VCED.OP2_AM_Enable := (aPar.VMEM.OP2_AME_EBS_KVS shr 6) and 1;
  kvs1 := aPar.VMEM.OP2_AME_EBS_KVS and 7;
  kvs2 := (aPar.VMEM.OP2_LS2_KVS2_RS_DET shr 5) and 1;
  if kvs2 = 0 then
    t.VCED.OP2_Key_Vel_Sens := kvs1
  else
    t.VCED.OP2_Key_Vel_Sens := 15 - kvs1;
  {$IFDEF Debug }
  WriteLn('OP2_KVS : ' + IntToStr(t.VCED.OP2_Key_Vel_Sens));
  {$ENDIF}
  t.VCED.OP2_OP_Output_Level := aPar.VMEM.OP2_OP_Output_Level and 127;
  t.VCED.OP2_Frequency := aPar.VMEM.OP2_Frequency and 63;
  t.VCED.OP2_Detune := aPar.VMEM.OP2_LS2_KVS2_RS_DET and 7;

  t.ACED.OP2_Fixed_Freq := (aPar.AMEM.OP2_FIXRM_EGSFT_FIX_FIXRG shr 3) and 1;
  t.ACED.OP2_Fixed_Freq_Range := aPar.AMEM.OP2_FIXRM_EGSFT_FIX_FIXRG and 7;
  t.ACED.OP2_Freq_Range_Fine := aPar.AMEM.OP2_OSW_FINE and 15;
  t.ACED.OP2_Operator_Waveform := (aPar.AMEM.OP2_OSW_FINE shr 4) and 7;
  t.ACED.OP2_EG_Shift := (aPar.AMEM.OP2_FIXRM_EGSFT_FIX_FIXRG shr 4) and 3;

  t.ACED2.OP2_Fix_Range_Mode := (aPar.AMEM.OP2_FIXRM_EGSFT_FIX_FIXRG shr 6) and 1;

  t.VCED.OP1_Attack_Rate := aPar.VMEM.OP1_Attack_Rate and 31;
  t.VCED.OP1_Decay_1_Rate := aPar.VMEM.OP1_Decay_1_Rate and 31;
  t.VCED.OP1_Decay_2_Rate := aPar.VMEM.OP1_Decay_2_Rate and 31;
  t.VCED.OP1_Release_Rate := max(1, aPar.VMEM.OP1_Release_Rate and 15);
  t.VCED.OP1_Decay_1_Level := aPar.VMEM.OP1_Decay_1_Level and 15;
  t.VCED.OP1_Level_Scaling := aPar.VMEM.OP1_Level_Scaling and 127;
  t.VCED.OP1_Rate_Scaling := (aPar.VMEM.OP1_LS2_KVS2_RS_DET shr 3) and 3;
  t.VCED.OP1_EG_Bias_Sens := (aPar.VMEM.OP1_AME_EBS_KVS shr 3) and 7;
  t.VCED.OP1_AM_Enable := (aPar.VMEM.OP1_AME_EBS_KVS shr 6) and 1;
  kvs1 := aPar.VMEM.OP1_AME_EBS_KVS and 7;
  kvs2 := (aPar.VMEM.OP1_LS2_KVS2_RS_DET shr 5) and 1;
  if kvs2 = 0 then
    t.VCED.OP1_Key_Vel_Sens := kvs1
  else
    t.VCED.OP1_Key_Vel_Sens := 15 - kvs1;
  {$IFDEF Debug }
  WriteLn('OP1_KVS : ' + IntToStr(t.VCED.OP1_Key_Vel_Sens));
  {$ENDIF}
  t.VCED.OP1_OP_Output_Level := aPar.VMEM.OP1_OP_Output_Level and 127;
  t.VCED.OP1_Frequency := aPar.VMEM.OP1_Frequency and 63;
  t.VCED.OP1_Detune := aPar.VMEM.OP1_LS2_KVS2_RS_DET and 7;

  t.ACED.OP1_Fixed_Freq := (aPar.AMEM.OP1_FIXRM_EGSFT_FIX_FIXRG shr 3) and 1;
  t.ACED.OP1_Fixed_Freq_Range := aPar.AMEM.OP1_FIXRM_EGSFT_FIX_FIXRG and 7;
  t.ACED.OP1_Freq_Range_Fine := aPar.AMEM.OP1_OSW_FINE and 15;
  t.ACED.OP1_Operator_Waveform := (aPar.AMEM.OP1_OSW_FINE shr 4) and 7;
  t.ACED.OP1_EG_Shift := (aPar.AMEM.OP1_FIXRM_EGSFT_FIX_FIXRG shr 4) and 3;

  t.ACED2.OP1_Fix_Range_Mode := (aPar.AMEM.OP1_FIXRM_EGSFT_FIX_FIXRG shr 6) and 1;

  t.VCED.Algorithm := aPar.VMEM.SY_FBL_ALG and 7;
  t.VCED.Feedback := (aPar.VMEM.SY_FBL_ALG shr 3) and 7;
  t.VCED.LFO_Speed := aPar.VMEM.LFO_Speed and 127;
  t.VCED.LFO_Delay := aPar.VMEM.LFO_Delay and 127;
  t.VCED.Pitch_Mod_Depth := aPar.VMEM.Pitch_Mod_Depth and 127;
  t.VCED.Amplitude_Mod_Depth := aPar.VMEM.Amplitude_Mod_Depth and 127;
  t.VCED.LFO_Sync := (aPar.VMEM.SY_FBL_ALG shr 6) and 1;
  t.VCED.LFO_Wave := aPar.VMEM.PMS_AMS_LFW and 3;
  t.VCED.Pitch_Mod_Sens := (aPar.VMEM.PMS_AMS_LFW shr 4) and 7;
  t.VCED.Amplitude_Mod_Sens := (aPar.VMEM.PMS_AMS_LFW shr 2) and 3;
  t.VCED.Transpose := aPar.VMEM.Transpose and 63;
  t.VCED.Poly_Mono := (aPar.VMEM.CH_MO_SU_PO_PM shr 3) and 1;
  t.VCED.Pitch_Bend_Range := aPar.VMEM.Pitch_Bend_Range and 15;
  t.VCED.Portamento_Mode := aPar.VMEM.CH_MO_SU_PO_PM and 1;
  t.VCED.Portamento_Time := aPar.VMEM.Portamento_Time and 127;
  t.VCED.FC_Volume := aPar.VMEM.FC_Volume and 127;
  t.VCED.Sustain := (aPar.VMEM.CH_MO_SU_PO_PM shr 2) and 1;
  t.VCED.Portamento := (aPar.VMEM.CH_MO_SU_PO_PM shr 1) and 1;
  t.VCED.Chorus := (aPar.VMEM.CH_MO_SU_PO_PM shr 4) and 1;
  t.VCED.MW_Pitch := aPar.VMEM.MW_Pitch and 127;
  t.VCED.MW_Amplitude := aPar.VMEM.MW_Amplitude and 127;
  t.VCED.BC_Pitch := aPar.VMEM.BC_Pitch and 127;
  t.VCED.BC_Amplitude := aPar.VMEM.BC_Amplitude and 127;
  t.VCED.BC_Pitch_Bias := aPar.VMEM.BC_Pitch_Bias and 127;
  t.VCED.BC_EG_Bias := aPar.VMEM.BC_EG_Bias and 127;
  t.VCED.VOICE_NAME_CHAR_1 := aPar.VMEM.VOICE_NAME_CHAR_1;
  t.VCED.VOICE_NAME_CHAR_2 := aPar.VMEM.VOICE_NAME_CHAR_2;
  t.VCED.VOICE_NAME_CHAR_3 := aPar.VMEM.VOICE_NAME_CHAR_3;
  t.VCED.VOICE_NAME_CHAR_4 := aPar.VMEM.VOICE_NAME_CHAR_4;
  t.VCED.VOICE_NAME_CHAR_5 := aPar.VMEM.VOICE_NAME_CHAR_5;
  t.VCED.VOICE_NAME_CHAR_6 := aPar.VMEM.VOICE_NAME_CHAR_6;
  t.VCED.VOICE_NAME_CHAR_7 := aPar.VMEM.VOICE_NAME_CHAR_7;
  t.VCED.VOICE_NAME_CHAR_8 := aPar.VMEM.VOICE_NAME_CHAR_8;
  t.VCED.VOICE_NAME_CHAR_9 := aPar.VMEM.VOICE_NAME_CHAR_9;
  t.VCED.VOICE_NAME_CHAR_10 := aPar.VMEM.VOICE_NAME_CHAR_10;
  t.VCED.Pitch_EG_Level_1 := aPar.VMEM.PL1;
  t.VCED.Pitch_EG_Level_2 := aPar.VMEM.PL2;
  t.VCED.Pitch_EG_Level_3 := aPar.VMEM.PL3;
  t.VCED.Pitch_EG_Rate_1 := aPar.VMEM.PR1;
  t.VCED.Pitch_EG_Rate_2 := aPar.VMEM.PR2;
  t.VCED.Pitch_EG_Rate_3 := aPar.VMEM.PR3;
  t.VCED.OPERATOR_ON_OFF := 15; //just set to all OP=on; not part of VMEM

  t.ACED.Reverb_Rate := aPar.AMEM.Reverb_Rate and 7;
  t.ACED.FC_Pitch := aPar.AMEM.FC_Pitch and 127;
  t.ACED.FC_Amplitude := aPar.AMEM.FC_Amplitude and 127;

  if (t.ACED.Reverb_Rate + t.ACED.FC_Pitch + t.ACED.FC_Amplitude) <> 0 then
    FHasACED := True;
  if (t.ACED.OP1_EG_Shift + t.ACED.OP1_Fixed_Freq + t.ACED.OP1_Fixed_Freq_Range +
    t.ACED.OP1_Freq_Range_Fine + t.ACED.OP1_Operator_Waveform) <> 0 then
    FHasACED := True;
  if (t.ACED.OP2_EG_Shift + t.ACED.OP2_Fixed_Freq + t.ACED.OP2_Fixed_Freq_Range +
    t.ACED.OP2_Freq_Range_Fine + t.ACED.OP2_Operator_Waveform) <> 0 then
    FHasACED := True;
  if (t.ACED.OP3_EG_Shift + t.ACED.OP3_Fixed_Freq + t.ACED.OP3_Fixed_Freq_Range +
    t.ACED.OP3_Freq_Range_Fine + t.ACED.OP3_Operator_Waveform) <> 0 then
    FHasACED := True;
  if (t.ACED.OP4_EG_Shift + t.ACED.OP4_Fixed_Freq + t.ACED.OP4_Fixed_Freq_Range +
    t.ACED.OP4_Freq_Range_Fine + t.ACED.OP4_Operator_Waveform) <> 0 then
    FHasACED := True;


  t.ACED2.AT_Pitch := aPar.AMEM3.AT_Pitch and 127;
  t.ACED2.AT_Amplitude := aPar.AMEM3.AT_Amplitude and 127;
  t.ACED2.AT_Pitch_Bias := aPar.AMEM3.AT_Pitch_Bias and 127;
  t.ACED2.AT_EG_Bias := aPar.AMEM3.AT_EG_Bias and 127;

  b0 := (aPar.VMEM.OP4_LS2_KVS2_RS_DET shr 6) and 1;
  b1 := (aPar.VMEM.OP3_LS2_KVS2_RS_DET shr 5) and 2;
  b2 := (aPar.VMEM.OP2_LS2_KVS2_RS_DET shr 4) and 4;
  b3 := (aPar.VMEM.OP1_LS2_KVS2_RS_DET shr 3) and 8;
  t.ACED2.LS_Sign := b0 + b1 + b2 + b3;
  t.ACED2.Reserved := 0;

  if (t.ACED2.AT_Pitch + t.ACED2.AT_Amplitude + t.ACED2.AT_Pitch_Bias +
    t.ACED2.AT_EG_Bias + t.ACED2.LS_Sign) <> 0 then
    FHasACED2 := True;

  t.ACED3.Effect_Select := aPar.AMEM3.Effect_Select;
  t.ACED3.Balance := aPar.AMEM3.Balance;
  t.ACED3.Out_Level := aPar.AMEM3.Out_Level;
  t.ACED3.Stereo_mix := aPar.AMEM3.Stereo_mix;
  t.ACED3.Effect_Param_1 := aPar.AMEM3.Effect_Param_1;
  t.ACED3.Effect_Param_2 := aPar.AMEM3.Effect_Param_2;
  t.ACED3.Effect_Param_3 := aPar.AMEM3.Effect_Param_3;
  t.ACED3.Reserved_40 := 0;
  t.ACED3.Reserved_41 := 0;
  t.ACED3.Reserved_42 := 0;
  t.ACED3.Reserved_43 := 0;
  t.ACED3.Reserved_44 := 0;
  t.ACED3.Reserved_45 := 0;
  t.ACED3.Reserved_46 := 0;
  t.ACED3.Reserved_47 := 0;
  t.ACED3.Reserved_48 := 0;
  t.ACED3.Reserved_49 := 0;
  t.ACED3.Reserved_50 := 0;
  t.ACED3.Reserved_51 := 0;
  t.ACED3.Reserved_52 := 0;

  if (t.ACED3.Effect_Select + t.ACED3.Balance + t.ACED3.Out_Level +
    t.ACED3.Effect_Param_1 + t.ACED3.Effect_Param_2 + t.ACED3.Effect_Param_3) <> 0 then
    FHasACED3 := True;

  t.EFEDS.Preset := aPar.AMEM3.Effect_Preset_Nr;
  t.EFEDS.Time := aPar.AMEM3.Effect_Time;
  t.EFEDS.Balance := aPar.AMEM3.Effect_Balance;

  if (t.EFEDS.Preset + t.EFEDS.Time + t.EFEDS.Balance) <> 0 then FHasEFEDS := True;

  t.DELAY.Delay_SW := (aPar.AMEM3.Reserved_90 shr 1) and 1;
  t.DELAY.Long_Short := aPar.AMEM3.Reserved_90 and 1;

  if (t.DELAY.Delay_SW + t.DELAY.Long_Short) <> 0 then FHasDELAY := True;

  Result := t;
end;

function T4OPVoiceContainer.Load_VMEM_FromStream(var aStream: TMemoryStream;
  aPos: integer): boolean;
var
  i: integer;
begin
  Result := False;
  if (aPos + 127) <= aStream.Size then
    aStream.Position := aPos
  else
    Exit;
  try
    for i := 0 to 72 do
      F4OP_V50_VMEM_Params.VMEM.params[i] := aStream.ReadByte;

    for i := 0 to 10 do
      F4OP_V50_VMEM_Params.AMEM.params[i] := aStream.ReadByte;

    for i := 0 to 43 do
      F4OP_V50_VMEM_Params.AMEM3.params[i] := aStream.ReadByte;

    F4OP_V50_VCED_Params := VMEMtoVCED(F4OP_V50_VMEM_Params);
    Result := True;
  except
    Result := False;
  end;
end;

function T4OPVoiceContainer.Save_VMEM_ToStream(var aStream: TMemoryStream): boolean;
var
  i: integer;
begin
  //dont clear the stream here or else bulk dump won't work
  if Assigned(aStream) then
  begin
    for i := 0 to 72 do
      aStream.WriteByte(F4OP_V50_VMEM_Params.VMEM.params[i]);
    for i := 0 to 10 do
      aStream.WriteByte(F4OP_V50_VMEM_Params.AMEM.params[i]);
    for i := 0 to 43 do
      aStream.WriteByte(F4OP_V50_VMEM_Params.AMEM3.params[i]);
    Result := True;
  end
  else
    Result := False;
end;

function T4OPVoiceContainer.Load_Voice_FromStream(var aStream: TMemoryStream;
  aPos: integer): boolean;
var
  iPosVCED: integer;
  iPosACED: integer;
  iPosACED2: integer;
  iPosACED3: integer;
  iPosDELAY: integer;
  iPosEFEDS: integer;
begin
  InitVoice;
  iPosVCED := PosBytes([$03, $00, $5D], aStream, aPos);
  if iPosVCED - 3 >= 0 then
  begin
    aStream.Position := iPosVCED - 3;
    if (aStream.ReadByte = $F0) and (aStream.ReadByte = $43) then
      iPosVCED := iPosVCED - 3
    else
      iPosVCED := -1;
  end;
  Result := False;
  if iPosVCED <> -1 then
    Result := Load_VCED_FromStream(aStream, iPosVCED + 6);
  iPosACED := PosBytes(abLMType[0], aStream, aPos);
  FHasACED := iPosACED <> -1;
  iPosACED2 := PosBytes(abLMType[1], aStream, aPos);
  FHasACED2 := iPosACED2 <> -1;
  iPosACED3 := PosBytes(abLMType[2], aStream, aPos);
  FHasACED3 := iPosACED3 <> -1;
  iPosDELAY := PosBytes(abLMType[8], aStream, aPos);
  FHasDELAY := iPosDELAY <> -1;
  iPosEFEDS := PosBytes(abLMType[7], aStream, aPos);
  FHasEFEDS := iPosEFEDS <> -1;

  if FHasACED then
    Load_ACED_FromStream(aStream, iPosACED + 10);
  if FHasACED2 then
    Load_ACED2_FromStream(aStream, iPosACED2 + 10);
  if FHasACED3 then
    Load_ACED3_FromStream(aStream, iPosACED3 + 10);
  if FHasDELAY then
    Load_DELAY_FromStream(aStream, iPosDELAY + 10);
  if FHasEFEDS then
    Load_EFEDS_FromStream(aStream, iPosEFEDS + 10);
{$IFDEF Debug }
  WriteLn('HasACED  = ' + BooltoStr(FHasACED, true));
  WriteLn('HasACED2 = ' + BooltoStr(FHasACED2, true));
  WriteLn('HasACED3 = ' + BooltoStr(FHasACED3, true));
  WriteLn('HasDELAY = ' + BooltoStr(FHasDELAY, true));
  WriteLn('HasEFEDS = ' + BooltoStr(FHasEFEDS, true));
{$ENDIF}
end;

function T4OPVoiceContainer.Load_VCED_FromStream(var aStream: TMemoryStream;
  aPos: integer): boolean;
var
  i: integer;
begin
  Result := False;
  InitVoice;
  if (aPos + 92) <= aStream.Size then
    aStream.Position := aPos
  else
    Exit;
  try
    for i := 0 to 92 do
      F4OP_V50_VCED_Params.VCED.params[i] := aStream.ReadByte;

    F4OP_V50_VMEM_Params := VCEDtoVMEM(F4OP_V50_VCED_Params);
    Result := True;
  except
    Result := False;
  end;
end;

function T4OPVoiceContainer.Load_ACED_FromStream(var aStream: TMemoryStream;
  aPos: integer): boolean;
var
  i: integer;
begin
  Result := False;
  if (aPos + 22) <= aStream.Size then
    aStream.Position := aPos
  else
    Exit;
  try
    for i := 0 to 22 do
      F4OP_V50_VCED_Params.ACED.params[i] := aStream.ReadByte;
    F4OP_V50_VMEM_Params := VCEDtoVMEM(F4OP_V50_VCED_Params);
    Result := True;
    FHasACED := True;
  except
    Result := False;
    FHasACED := False;
  end;
end;

function T4OPVoiceContainer.Load_ACED2_FromStream(var aStream: TMemoryStream;
  aPos: integer): boolean;
var
  i: integer;
begin
  Result := False;
  if (aPos + 9) <= aStream.Size then
    aStream.Position := aPos
  else
    Exit;
  try
    for i := 0 to 9 do
      F4OP_V50_VCED_Params.ACED2.params[i] := aStream.ReadByte;
    F4OP_V50_VMEM_Params := VCEDtoVMEM(F4OP_V50_VCED_Params);
    Result := True;
    FHasACED2 := True;
  except
    Result := False;
    FHasACED2 := False;
  end;
end;

function T4OPVoiceContainer.Load_ACED3_FromStream(var aStream: TMemoryStream;
  aPos: integer): boolean;
var
  i: integer;
begin
  Result := False;
  if (aPos + 19) <= aStream.Size then
    aStream.Position := aPos
  else
    Exit;
  try
    for i := 0 to 19 do
      F4OP_V50_VCED_Params.ACED3.params[i] := aStream.ReadByte;
    F4OP_V50_VMEM_Params := VCEDtoVMEM(F4OP_V50_VCED_Params);
    Result := True;
    FHasACED3 := True;
  except
    Result := False;
    FHasACED3 := False;
  end;
end;

function T4OPVoiceContainer.Load_DELAY_FromStream(var aStream: TMemoryStream;
  aPos: integer): boolean;
var
  i: integer;
begin
  Result := False;
  if (aPos + 2) <= aStream.Size then
    aStream.Position := aPos
  else
    Exit;
  try
    for i := 0 to 1 do
      F4OP_V50_VCED_Params.DELAY.params[i] := aStream.ReadByte;
    F4OP_V50_VMEM_Params := VCEDtoVMEM(F4OP_V50_VCED_Params);
    Result := True;
    FHasDELAY := True;
  except
    Result := False;
    FHasDELAY := False;
  end;
end;

function T4OPVoiceContainer.Load_EFEDS_FromStream(var aStream: TMemoryStream;
  aPos: integer): boolean;
var
  i: integer;
begin
  Result := False;
  if (aPos + 3) <= aStream.Size then
    aStream.Position := aPos
  else
    Exit;
  try
    for i := 0 to 2 do
      F4OP_V50_VCED_Params.EFEDS.params[i] := aStream.ReadByte;
    F4OP_V50_VMEM_Params := VCEDtoVMEM(F4OP_V50_VCED_Params);
    Result := True;
    FHasEFEDS := True;
  except
    Result := False;
    FHasEFEDS := False;
  end;
end;

procedure T4OPVoiceContainer.InitVoice;
begin
  GetDefinedValues(V50VCED, fInit, F4OP_V50_VCED_Params.VCED.params);
  GetDefinedValues(V50ACED, fInit, F4OP_V50_VCED_Params.ACED.params);
  GetDefinedValues(V50ACED2, fInit, F4OP_V50_VCED_Params.ACED2.params);
  GetDefinedValues(V50ACED3, fInit, F4OP_V50_VCED_Params.ACED3.params);
  GetDefinedValues(DS55, fInit, F4OP_V50_VCED_Params.DELAY.params);
  GetDefinedValues(YS, fInit, F4OP_V50_VCED_Params.EFEDS.params);

  F4OP_V50_VMEM_Params := VCEDtoVMEM(F4OP_V50_VCED_Params);
  FHasACED := False;
  FHasACED2 := False;
  FHasACED3 := False;
  FHasDELAY := False;
  FHasEFEDS := False;
end;

function T4OPVoiceContainer.GetVoiceName: string;
var
  s: string;
begin
  s := '';
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_1));
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_2));
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_3));
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_4));
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_5));
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_6));
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_7));
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_8));
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_9));
  s := s + Printable(chr(F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_10));
  Result := s;
end;

function T4OPVoiceContainer.CalculateHash(aLevel: integer = 3): string;
var
  aStream: TMemoryStream;
  i: integer;
begin
  aStream := TMemoryStream.Create;
  for i := 0 to 76 do
    aStream.WriteByte(F4OP_V50_VCED_Params.VCED.params[i]);
  if aLevel > 1 then  //TX81z
  begin
    for i := 0 to 22 do
      aStream.WriteByte(F4OP_V50_VCED_Params.ACED.params[i]);
    if aLevel > 2 then  //DX11 = default
    begin
      for i := 0 to 9 do
        aStream.WriteByte(F4OP_V50_VCED_Params.ACED2.params[i]);
      if aLevel > 3 then //V50
      begin
        for i := 0 to 19 do
          aStream.WriteByte(F4OP_V50_VCED_Params.ACED3.params[i]);
      end;
    end;
  end;

  aStream.Position := 0;
  Result := THashFactory.TCrypto.CreateSHA2_256().ComputeStream(aStream).ToString();
  aStream.Free;
end;

function T4OPVoiceContainer.Add_VCED_ToStream(var aStream: TMemoryStream): boolean;
var
  i: integer;
begin
  if Assigned(aStream) then
  begin
    for i := 0 to 92 do     // OP on/off is not a part of the VCED SysEx
      aStream.WriteByte(F4OP_V50_VCED_Params.VCED.params[i]);
    Result := True;
  end
  else
    Result := False;
end;

function T4OPVoiceContainer.Add_ACED_ToStream(var aStream: TMemoryStream): boolean;
var
  i: integer;
begin
  if Assigned(aStream) then
  begin
    for i := 0 to 22 do
      aStream.WriteByte(F4OP_V50_VCED_Params.ACED.params[i]);
    Result := True;
  end
  else
    Result := False;
end;

function T4OPVoiceContainer.Add_ACED2_ToStream(var aStream: TMemoryStream): boolean;
var
  i: integer;
begin
  if Assigned(aStream) then
  begin
    for i := 0 to 9 do
      aStream.WriteByte(F4OP_V50_VCED_Params.ACED2.params[i]);
    Result := True;
  end
  else
    Result := False;
end;

function T4OPVoiceContainer.Add_ACED3_ToStream(var aStream: TMemoryStream): boolean;
var
  i: integer;
begin
  if Assigned(aStream) then
  begin
    for i := 0 to 19 do
      aStream.WriteByte(F4OP_V50_VCED_Params.ACED3.params[i]);
    Result := True;
  end
  else
    Result := False;
end;

function T4OPVoiceContainer.Add_DELAY_ToStream(var aStream: TMemoryStream): boolean;
var
  i: integer;
begin
  if Assigned(aStream) then
  begin
    for i := 0 to 1 do
      aStream.WriteByte(F4OP_V50_VCED_Params.DELAY.params[i]);
    Result := True;
  end
  else
    Result := False;
end;

function T4OPVoiceContainer.Add_EFEDS_ToStream(var aStream: TMemoryStream): boolean;
var
  i: integer;
begin
  if Assigned(aStream) then
  begin
    for i := 0 to 2 do
      aStream.WriteByte(F4OP_V50_VCED_Params.EFEDS.params[i]);
    Result := True;
  end
  else
    Result := False;
end;

procedure T4OPVoiceContainer.SysExVoiceToStream(aCh: integer;
  var aStream: TMemoryStream);
var
  FCh: byte;
begin
  FCh := aCh - 1;
  aStream.Clear;
  aStream.Position := 0;
  aStream.WriteByte($F0);
  aStream.WriteByte($43);
  aStream.WriteByte($00 + FCh); //MIDI channel
  aStream.WriteByte($03);
  aStream.WriteByte($00);
  aStream.WriteByte($5D);
  Add_VCED_ToStream(aStream);
  aStream.WriteByte(GetVCEDChecksum);
  aStream.WriteByte($F7);

  //  if FHasACED then
  //  begin
  aStream.WriteByte($F0);
  aStream.WriteByte($43);
  aStream.WriteByte($00 + FCh); //MIDI channel
  aStream.WriteByte($7E);
  aStream.WriteByte($00);
  aStream.WriteByte($21);
  aStream.WriteAnsiString('LM  8976AE');
  Add_ACED_ToStream(aStream);
  aStream.WriteByte(GetACEDChecksum);
  aStream.WriteByte($F7);
  //  end;
  //  if FHasACED2 then
  //  begin
  aStream.WriteByte($F0);
  aStream.WriteByte($43);
  aStream.WriteByte($00 + FCh); //MIDI channel
  aStream.WriteByte($7E);
  aStream.WriteByte($00);
  aStream.WriteByte($14);
  aStream.WriteAnsiString('LM  8023AE');
  Add_ACED2_ToStream(aStream);
  aStream.WriteByte(GetACED2Checksum);
  aStream.WriteByte($F7);
  //  end;
  //  if FHasACED3 then
  //  begin
  aStream.WriteByte($F0);
  aStream.WriteByte($43);
  aStream.WriteByte($00 + FCh); //MIDI channel
  aStream.WriteByte($7E);
  aStream.WriteByte($00);
  aStream.WriteByte($1E);
  aStream.WriteAnsiString('LM  8073AE');
  Add_ACED3_ToStream(aStream);
  aStream.WriteByte(GetACED3Checksum);
  aStream.WriteByte($F7);
  //  end;
  //  if FHasDELAY then
  //  begin
  aStream.WriteByte($F0);
  aStream.WriteByte($43);
  aStream.WriteByte($00 + FCh); //MIDI channel
  aStream.WriteByte($7E);
  aStream.WriteByte($00);
  aStream.WriteByte($0C);
  aStream.WriteAnsiString('LM  8054DL');
  Add_DELAY_ToStream(aStream);
  aStream.WriteByte(GetDELAYChecksum);
  aStream.WriteByte($F7);
  //  end;
  //  if FHasEFEDS then
  //  begin
  aStream.WriteByte($F0);
  aStream.WriteByte($43);
  aStream.WriteByte($00 + FCh); //MIDI channel
  aStream.WriteByte($7E);
  aStream.WriteByte($00);
  aStream.WriteByte($0D);
  aStream.WriteAnsiString('LM  8036EF');
  Add_EFEDS_ToStream(aStream);
  aStream.WriteByte(GetEFEDSChecksum);
  aStream.WriteByte($F7);
  //  end;
end;

function T4OPVoiceContainer.GetVCEDChecksum: byte;
var
  checksum: integer;
  i: integer;
begin
  checksum := 0;
  for i := 0 to 92 do
    checksum := checksum + F4OP_V50_VCED_Params.VCED.params[i];
  Result := ((not (checksum and 255)) and 127) + 1;
end;

function T4OPVoiceContainer.GetACEDChecksum: byte;
var
  checksum: integer;
  i: integer;
  tmpStream: TMemoryStream;
begin
  checksum := 0;
  tmpStream := TMemoryStream.Create;
  tmpStream.WriteAnsiString('LM  8976AE');
  Add_ACED_ToStream(tmpStream);
  tmpStream.Position := 0;
  for i := 0 to 32 do
    checksum := checksum + tmpStream.ReadByte;
  Result := ((not (checksum and 255)) and 127) + 1;
  tmpStream.Free;
end;

function T4OPVoiceContainer.GetACED2Checksum: byte;
var
  checksum: integer;
  i: integer;
  tmpStream: TMemoryStream;
begin
  checksum := 0;
  tmpStream := TMemoryStream.Create;
  tmpStream.WriteAnsiString('LM  8023AE');
  Add_ACED2_ToStream(tmpStream);
  tmpStream.Position := 0;
  for i := 0 to 19 do
    checksum := checksum + tmpStream.ReadByte;
  Result := ((not (checksum and 255)) and 127) + 1;
  tmpStream.Free;
end;

function T4OPVoiceContainer.GetACED3Checksum: byte;
var
  checksum: integer;
  i: integer;
  tmpStream: TMemoryStream;
begin
  checksum := 0;
  tmpStream := TMemoryStream.Create;
  tmpStream.WriteAnsiString('LM  8073AE');
  Add_ACED3_ToStream(tmpStream);
  tmpStream.Position := 0;
  for i := 0 to 29 do
    checksum := checksum + tmpStream.ReadByte;
  Result := ((not (checksum and 255)) and 127) + 1;
  tmpStream.Free;
end;

function T4OPVoiceContainer.GetDELAYChecksum: byte;
var
  checksum: integer;
  i: integer;
  tmpStream: TMemoryStream;
begin
  checksum := 0;
  tmpStream := TMemoryStream.Create;
  tmpStream.WriteAnsiString('LM  8054DL');
  Add_DELAY_ToStream(tmpStream);
  tmpStream.Position := 0;
  for i := 0 to 11 do
    checksum := checksum + tmpStream.ReadByte;
  Result := ((not (checksum and 255)) and 127) + 1;
  tmpStream.Free;
end;

function T4OPVoiceContainer.GetEFEDSChecksum: byte;
var
  checksum: integer;
  i: integer;
  tmpStream: TMemoryStream;
begin
  checksum := 0;
  tmpStream := TMemoryStream.Create;
  tmpStream.WriteAnsiString('LM  8036EF');
  Add_EFEDS_ToStream(tmpStream);
  tmpStream.Position := 0;
  for i := 0 to 12 do
    checksum := checksum + tmpStream.ReadByte;
  Result := ((not (checksum and 255)) and 127) + 1;
  tmpStream.Free;
end;

function T4OPVoiceContainer.GetChecksumPart: integer;
var
  checksum: integer;
  i: integer;
  tmpStream: TMemoryStream;
begin
  checksum := 0;
  tmpStream := TMemoryStream.Create;
  Save_VMEM_ToStream(tmpStream);
  tmpStream.Position := 0;
  for i := 0 to tmpStream.Size - 1 do
    checksum := checksum + tmpStream.ReadByte;
  Result := checksum;
  tmpStream.Free;
end;

function T4OPVoiceContainer.Get_VMEM_Params: T4OP_V50_VMEM_Params;
begin
  Result := F4OP_V50_VMEM_Params;
end;

function T4OPVoiceContainer.Set_VMEM_Params(aParams: T4OP_V50_VMEM_Params): boolean;
begin
  F4OP_V50_VMEM_Params := aParams;
  F4OP_V50_VCED_Params := VMEMtoVCED(F4OP_V50_VMEM_Params);
  Result := True;
end;

function T4OPVoiceContainer.Set_VCED_Params(aParams: T4OP_V50_VCED_Params): boolean;
begin
  F4OP_V50_VCED_Params := aParams;
  F4OP_V50_VMEM_Params := VCEDtoVMEM(F4OP_V50_VCED_Params);
  Result := True;
end;

function T4OPVoiceContainer.Get_VCED_Params: T4OP_V50_VCED_Params;
begin
  Result := F4OP_V50_VCED_Params;
end;

function T4OPVoiceContainer.CheckMinMax(var slReport: TStringList): boolean;
var
  arMin: array of byte;
  arMax: array of byte;
  i: integer;
begin
  Result := True;

  SetLength(arMin, 93);
  SetLength(arMax, 93);
  GetDefinedValues(V50VCED, fMin, arMin);
  GetDefinedValues(V50VCED, fMax, arMax);
  for i := 0 to 92 do
  begin
    if (F4OP_V50_VCED_Params.VCED.params[i] < arMin[i]) or
      (F4OP_V50_VCED_Params.VCED.params[i] > arMax[i]) then
    begin
      Result := False;
      slReport.Add('Parameter ' + V50_VCED_NAMES[i, 0] + ' has value ' +
        IntToStr(F4OP_V50_VCED_Params.VCED.params[i]) + '. Allowed range is [' +
        IntToStr(arMin[i]) + ',' + IntToStr(arMax[i]) + ']');
    end;
  end;

  SetLength(arMin, 23);
  SetLength(arMax, 23);
  GetDefinedValues(V50ACED, fMin, arMin);
  GetDefinedValues(V50ACED, fMax, arMax);
  for i := 0 to 22 do
  begin
    if (F4OP_V50_VCED_Params.ACED.params[i] < arMin[i]) or
      (F4OP_V50_VCED_Params.ACED.params[i] > arMax[i]) then
    begin
      Result := False;
      slReport.Add('Parameter ' + V50_ACED_NAMES[i, 0] + ' has value ' +
        IntToStr(F4OP_V50_VCED_Params.ACED.params[i]) + '. Allowed range is [' +
        IntToStr(arMin[i]) + ',' + IntToStr(arMax[i]) + ']');
    end;
  end;

  SetLength(arMin, 10);
  SetLength(arMax, 10);
  GetDefinedValues(V50ACED2, fMin, arMin);
  GetDefinedValues(V50ACED2, fMax, arMax);
  for i := 0 to 9 do
  begin
    if (F4OP_V50_VCED_Params.ACED2.params[i] < arMin[i]) or
      (F4OP_V50_VCED_Params.ACED2.params[i] > arMax[i]) then
    begin
      Result := False;
      slReport.Add('Parameter ' + V50_ACED2_NAMES[i, 0] + ' has value ' +
        IntToStr(F4OP_V50_VCED_Params.ACED2.params[i]) + '. Allowed range is [' +
        IntToStr(arMin[i]) + ',' + IntToStr(arMax[i]) + ']');
    end;
  end;

  SetLength(arMin, 20);
  SetLength(arMax, 20);
  GetDefinedValues(V50ACED3, fMin, arMin);
  GetDefinedValues(V50ACED3, fMax, arMax);
  for i := 0 to 19 do
  begin
    if (F4OP_V50_VCED_Params.ACED3.params[i] < arMin[i]) or
      (F4OP_V50_VCED_Params.ACED3.params[i] > arMax[i]) then
    begin
      Result := False;
      slReport.Add('Parameter ' + V50_ACED3_NAMES[i, 0] + ' has value ' +
        IntToStr(F4OP_V50_VCED_Params.ACED3.params[i]) + '. Allowed range is [' +
        IntToStr(arMin[i]) + ',' + IntToStr(arMax[i]) + ']');
    end;
  end;

  SetLength(arMin, 2);
  SetLength(arMax, 2);
  GetDefinedValues(DS55, fMin, arMin);
  GetDefinedValues(DS55, fMax, arMax);
  for i := 0 to 1 do
  begin
    if (F4OP_V50_VCED_Params.DELAY.params[i] < arMin[i]) or
      (F4OP_V50_VCED_Params.DELAY.params[i] > arMax[i]) then
    begin
      Result := False;
      slReport.Add('Parameter ' + DS55_DELAY_NAMES[i, 0] + ' has value ' +
        IntToStr(F4OP_V50_VCED_Params.DELAY.params[i]) + '. Allowed range is [' +
        IntToStr(arMin[i]) + ',' + IntToStr(arMax[i]) + ']');
    end;
  end;

  SetLength(arMin, 3);
  SetLength(arMax, 3);
  GetDefinedValues(YS, fMin, arMin);
  GetDefinedValues(YS, fMax, arMax);
  for i := 0 to 2 do
  begin
    if (F4OP_V50_VCED_Params.EFEDS.params[i] < arMin[i]) or
      (F4OP_V50_VCED_Params.EFEDS.params[i] > arMax[i]) then
    begin
      Result := False;
      slReport.Add('Parameter ' + YS_EFEDS_NAMES[i, 0] + ' has value ' +
        IntToStr(F4OP_V50_VCED_Params.EFEDS.params[i]) + '. Allowed range is [' +
        IntToStr(arMin[i]) + ',' + IntToStr(arMax[i]) + ']');
    end;
  end;
end;

function T4OPVoiceContainer.HasNullInName: boolean;
begin
  Result := False;
  if (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_1 = 0) or
    (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_2 = 0) or
    (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_3 = 0) or
    (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_4 = 0) or
    (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_5 = 0) or
    (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_6 = 0) or
    (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_7 = 0) or
    (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_8 = 0) or
    (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_9 = 0) or
    (F4OP_V50_VMEM_Params.VMEM.VOICE_NAME_CHAR_10 = 0) then Result := True;
end;

procedure T4OPVoiceContainer.Normalize;
var
  arMin: array of byte;
  arMax: array of byte;
  i: integer;
begin
  SetLength(arMin, 93);
  SetLength(arMax, 93);
  GetDefinedValues(V50VCED, fMin, arMin);
  GetDefinedValues(V50VCED, fMax, arMax);
  for i := 0 to 92 do
  begin
    if F4OP_V50_VCED_Params.VCED.params[i] < arMin[i] then
      F4OP_V50_VCED_Params.VCED.params[i] := arMin[i];
    if F4OP_V50_VCED_Params.VCED.params[i] > arMax[i] then
      F4OP_V50_VCED_Params.VCED.params[i] := arMax[i];
  end;

  SetLength(arMin, 23);
  SetLength(arMax, 23);
  GetDefinedValues(V50ACED, fMin, arMin);
  GetDefinedValues(V50ACED, fMax, arMax);
  for i := 0 to 22 do
  begin
    if F4OP_V50_VCED_Params.ACED.params[i] < arMin[i] then
      F4OP_V50_VCED_Params.ACED.params[i] := arMin[i];
    if F4OP_V50_VCED_Params.ACED.params[i] > arMax[i] then
      F4OP_V50_VCED_Params.ACED.params[i] := arMax[i];
  end;

  SetLength(arMin, 10);
  SetLength(arMax, 10);
  GetDefinedValues(V50ACED2, fMin, arMin);
  GetDefinedValues(V50ACED2, fMax, arMax);
  for i := 0 to 9 do
  begin
    if F4OP_V50_VCED_Params.ACED2.params[i] < arMin[i] then
      F4OP_V50_VCED_Params.ACED2.params[i] := arMin[i];
    if F4OP_V50_VCED_Params.ACED2.params[i] > arMax[i] then
      F4OP_V50_VCED_Params.ACED2.params[i] := arMax[i];
  end;

  SetLength(arMin, 20);
  SetLength(arMax, 20);
  GetDefinedValues(V50ACED3, fMin, arMin);
  GetDefinedValues(V50ACED3, fMax, arMax);
  for i := 0 to 19 do
  begin
    if F4OP_V50_VCED_Params.ACED3.params[i] < arMin[i] then
      F4OP_V50_VCED_Params.ACED3.params[i] := arMin[i];
    if F4OP_V50_VCED_Params.ACED3.params[i] > arMax[i] then
      F4OP_V50_VCED_Params.ACED3.params[i] := arMax[i];
  end;

  SetLength(arMin, 2);
  SetLength(arMax, 2);
  GetDefinedValues(DS55, fMin, arMin);
  GetDefinedValues(DS55, fMax, arMax);
  for i := 0 to 1 do
  begin
    if F4OP_V50_VCED_Params.DELAY.params[i] < arMin[i] then
      F4OP_V50_VCED_Params.DELAY.params[i] := arMin[i];
    if F4OP_V50_VCED_Params.DELAY.params[i] > arMax[i] then
      F4OP_V50_VCED_Params.DELAY.params[i] := arMax[i];
  end;

  SetLength(arMin, 3);
  SetLength(arMax, 3);
  GetDefinedValues(YS, fMin, arMin);
  GetDefinedValues(YS, fMax, arMax);
  for i := 0 to 2 do
  begin
    if F4OP_V50_VCED_Params.EFEDS.params[i] < arMin[i] then
      F4OP_V50_VCED_Params.EFEDS.params[i] := arMin[i];
    if F4OP_V50_VCED_Params.EFEDS.params[i] > arMax[i] then
      F4OP_V50_VCED_Params.EFEDS.params[i] := arMax[i];
  end;

  F4OP_V50_VMEM_Params := VCEDtoVMEM(F4OP_V50_VCED_Params);
end;

end.
