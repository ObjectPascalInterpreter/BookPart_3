unit uUtils;

// Developed using Delphi for Windows and Mac platforms.

// *** Ths source is distributed under Apache 2.0 ***

// Copyright (C) 2018 Herbert M Sauro

// Author Contact Information:
// email: hsauro@gmail.com


{$WARN SYMBOL_PLATFORM OFF}  // Stops memory API usage warnings


interface

Uses Windows;

function getProcessTime : int64;
function getMemoryAllocated : UInt64;


implementation


function getProcessTime : int64;
var lpCreationTime, lpExitTime, lpKernelTime, lpUserTime: TFileTime;
    TotalTime : integer;
      hProc, b: Cardinal;
begin
  result := 0;
  b := GetCurrentProcess;
  hProc := OpenProcess( PROCESS_QUERY_INFORMATION, False, b );
  if hProc = 0 then
     exit;

  GetProcessTimes(hProc,lpCreationTime, lpExitTime, lpKernelTime, lpUserTime);
  TotalTime := int64(lpKernelTime.dwLowDateTime or (lpKernelTime.dwHighDateTime shr 32));
  result := TotalTime;
end;


function getMemoryAllocated : UInt64;
var st: TMemoryManagerState; sb: TSmallBlockTypeState;
begin
  getMemoryManagerState(st);
  result :=  st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do
      result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
end;


end.
