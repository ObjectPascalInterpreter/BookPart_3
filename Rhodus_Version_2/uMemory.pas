unit uMemory;

interface

Uses System.SysUtils;

function getMemoryAllocated : UInt64;

implementation

function getMemoryAllocated : UInt64;
var st: TMemoryManagerState; sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);
  result :=  st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do
      result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
end;



end.
