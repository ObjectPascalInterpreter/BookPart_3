//   Window List Component 1.5 by Jerry Ryle
//
//   Aaugh! I accidentally uploaded the wrong source
//   which had a nasty bug in the refresh procedure!
//   Thanks to Serge, who found my mistake and suggested
//   a few other improvements!
//
//   This component will enumerate windows and return
//   information about them in the Windows property.
//   The component currently returns a handle, caption text,
//   associated ProcessID, visibility, and dimensions.
//   For documentation, please read the accompanying
//   WindowList.txt
//
//   This component is completely free of course. If you find
//   it useful, and are compelled to send me cash, beer, or
//   dead things in envelopes, please feel free to do so.
//
//   email me if you make it better:  gryle@calpoly.edu

unit uFindWindows;

interface

uses
  Windows, Messages, SysUtils, Classes;

type

  TWindowObject = record
                    WinHandle  : HWnd;    // Window Handle
                    WinCaption : String;  // Window Caption Text (If any)
                    ProcessID  : Integer; // Process the window belongs to
                    IsVisible  : Boolean; // Is the window visible?
                    IsEnabled  : Boolean; // Is the window enabled for mouse/keyboard input?
                    IsIconic   : Boolean; // Is the window minimized?
                    WindowRect : TRect;   // Window Dimensions
                    // Add more properties here if you like,
                    // then fill them in at the WindowCallback
                    // function.
                  end;
  PTWindowObject = ^TWindowObject;

  TWindowList = class(TComponent)
  private
    WindowLst : TList;
    FCount : Integer;
  protected
    Function GetAWindow(Index : Integer) : TWindowObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure Refresh;
    Property Windows[Index : Integer]: TWindowObject read GetAWindow;
    Property Count : Integer read FCount;
  published
    // Published declarations
  end;


implementation

// Note that this function is not a member of WindowList.
// Therefore, the list to be filled needs to be passed
// as a pointer. Note that this is passed as a VAR. if you
// don't do this, bad things happen in memory.

Function WindowCallback(WHandle : HWnd; Var Parm : Pointer) : Boolean; stdcall;
// This function is called once for each window
Var MyString : PChar;
     MyInt : Integer;
     MyWindowPtr : ^TWindowObject;
begin
    New(MyWindowPtr);

    // Window Handle (Passed by the enumeration)
    MyWindowPtr.WinHandle := WHandle;

    // Window text
    MyString := Allocmem(255);
    GetWindowText(WHandle,MyString,255);
    MyWindowPtr.WinCaption := String(MyString);
    FreeMem(MyString,255);

    // Process ID
    MyInt := 0;
    MyWindowPtr.ProcessID := GetWindowThreadProcessId(WHandle,@MyInt);

    // Visiblity
    MyWindowPtr.IsVisible := IsWindowVisible(WHandle);

    // Enabled
    MyWindowPtr.IsEnabled := IsWindowEnabled(WHandle);

    // Iconic
    MyWindowPtr.IsIconic := IsIconic(WHandle);

    // Window Dimensions
    MyWindowPtr.WindowRect := Rect(0,0,0,0);
    GetWindowRect(WHandle,MyWindowPtr.WindowRect);

    // Add the structure to the list. Do not dereference Parm...
    // once again, bad things happen.
    TList(Parm).Add(MyWindowPtr);
    Result := True; // Everything's okay. Continue to enumerate windows
end;

constructor TWindowList.Create(AOwner: TComponent);
begin
  inherited;
  WindowLst := TList.Create;

  // Thanks Serge, I should've done this from the start :)
  // Sloppy me.
  If Not ( csDesigning in ComponentState ) Then
    Begin
      EnumWindows(@WindowCallback,Longint(@WindowLst));
      FCount := WindowLst.Count;
    End
  Else
    FCount := 0;
end;

destructor TWindowList.Destroy;
var I : Integer;
begin
  If WindowLst.Count > 0 Then
    Begin
      For I := 0 To (WindowLst.Count - 1) Do
        Dispose(PTWindowObject(WindowLst[I]));
    End;
  WindowLst.Free;
  inherited;
end;

procedure TWindowList.Refresh;
begin
  WindowLst.Clear; {Clear the list!}
  EnumWindows(@WindowCallback,Longint(@WindowLst));
  FCount := WindowLst.Count;
end;

function TWindowList.GetAWindow(Index : Integer) : TWindowObject;
begin
  Result := PTWindowObject(WindowLst[Index])^;
end;

end.
