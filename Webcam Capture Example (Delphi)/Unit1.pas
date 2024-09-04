unit Unit1;

interface

uses
  Winapi.Windows, // Provides access to Windows API functions and constants
  Vcl.Forms, // VCL forms and form-related functions
  Vcl.Controls, // VCL control components, such as TButton and TImage
  Vcl.ExtCtrls, // Extended VCL controls, including TImage
  Vcl.StdCtrls, // Standard VCL controls, such as TButton
  Vcl.Dialogs, // VCL dialog boxes, including message dialogs
  System.Classes, // Basic system classes, including TComponent
  Winapi.Messages, // Windows message constants and types
  System.SysUtils,
  // System utilities, including string handling and date/time functions
  System.Variants, // Variants for handling different data types
  Vcl.Graphics, // VCL graphics components and classes, including TBitmap
  ActiveX; // ActiveX support, including COM interfaces


const
  WM_USER = 1024; // Base value for user-defined messages

  // Video capture messages for interacting with the video capture window
  WM_CAP_START = WM_USER;
  WM_CAP_STOP = WM_CAP_START + 68;
  WM_CAP_DRIVER_CONNECT = WM_CAP_START + 10;
  WM_CAP_DRIVER_DISCONNECT = WM_CAP_START + 11;
  WM_CAP_SAVEDIB = WM_CAP_START + 25;
  WM_CAP_GRAB_FRAME = WM_CAP_START + 60;
  WM_CAP_SEQUENCE = WM_CAP_START + 62;
  WM_CAP_FILE_SET_CAPTURE_FILEA = WM_CAP_START + 20;
  WM_CAP_EDIT_COPY = WM_CAP_START + 30;
  WM_CAP_SET_PREVIEW = WM_CAP_START + 50;
  WM_CAP_SET_PREVIEWRATE = WM_CAP_START + 52;

type
  TForm1 = class(TForm)
    Image1: TImage; // TImage component to display the captured video frame
    Button1: TButton; // Button to stop video capture
    ComboBox1: TComboBox; // ComboBox to list available video capture devices
    procedure Button1Click(Sender: TObject); // Starts capturing video
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    // Handles form close event
    procedure FormCreate(Sender: TObject); // Handles form creation event
  private
    CaptureWnd: HWND; // Handle to the video capture window
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// Interface definition for ICreateDevEnum used for enumerating video capture devices
type
  ICreateDevEnum = interface(IUnknown)
    ['{29840822-5B84-11D0-BD3B-00A0C911CE86}']
    // Creates a class enumerator for the specified device class
    function CreateClassEnumerator(const clsidDeviceClass: TGUID;
      out ppEnumMoniker: IEnumMoniker; dwFlags: DWORD): HRESULT; stdcall;
  end;

  // Procedure to populate the ComboBox with available video capture devices
procedure ListVideoDevices;
const
  // CLSID for the system device enumerator
  CLSID_SystemDeviceEnum: TGUID = (D1: $62BE5D10; D2: $60EB; D3: $11D0;
    D4: ($BD, $3B, $00, $A0, $C9, $11, $CE, $86));
  // IID for the ICreateDevEnum interface
  IID_ICreateDevEnum: TGUID = '{29840822-5B84-11D0-BD3B-00A0C911CE86}';
  // CLSID for the video input devices category
  CLSID_VideoInputDeviceCategory: TGUID = (D1: $860BB310; D2: $5D01; D3: $11D0;
    D4: ($BD, $3B, $00, $A0, $C9, $11, $CE, $86));
var
  pDevEnum: ICreateDevEnum; // Interface for creating device enumerators
  pClassEnum: IEnumMoniker; // Interface for enumerating class monikers
  pMoniker: IMoniker; // Moniker representing a specific device
  pPropertyBag: IPropertyBag; // Property bag to access device properties
  v: OleVariant; // Variable to store device names
  cFetched: ulong; // Count of devices fetched by the enumerator
  xname: string; // Device name
begin
  // Create an instance of the system device enumerator
  CoCreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC,
    IID_ICreateDevEnum, pDevEnum);

  // Initialize the class enumerator for video input devices
  pClassEnum := nil;
  pDevEnum.CreateClassEnumerator(CLSID_VideoInputDeviceCategory, pClassEnum, 0);

  // Initialize the variant for use
  VariantInit(v);
  pMoniker := nil;

  // Enumerate through available video input devices
  while (pClassEnum.Next(1, pMoniker, @cFetched) = S_OK) do
  begin
    pPropertyBag := nil;

    // Bind the moniker to the property bag to read device properties
    if S_OK = pMoniker.BindToStorage(nil, nil, IPropertyBag, pPropertyBag) then
    begin
      // Read the friendly name of the device
      if S_OK = pPropertyBag.Read('FriendlyName', v, nil) then
      begin
        xname := v; // Store the device name in a string
        Form1.ComboBox1.Items.Add(xname); // Add the device name to the ComboBox
      end;
    end;
    VariantClear(v); // Clear the variant to free resources
  end;

  // Clean up the enumerator and other objects
  pClassEnum := nil;
  pMoniker := nil;
  pDevEnum := nil;
  pPropertyBag := nil;
end;

// External function declaration for creating a capture window
function capCreateCaptureWindowA(lpszWindowName: PChar; dwStyle: LongInt;
  x: Integer; y: Integer; nWidth: Integer; nHeight: Integer; ParentWin: HWND;
  nId: Integer): HWND; stdcall; external 'AVICAP32.DLL';

procedure TForm1.Button1Click(Sender: TObject);
var
  DeviceIndex: Integer;
begin
  // Check if a device is selected
  DeviceIndex := ComboBox1.ItemIndex;
  if DeviceIndex = -1 then
  begin
    MessageDlg('Please select a webcam device from the list.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if Self.Button1.Caption = 'Start Webcam Capture!' then
  begin
    Self.Button1.Caption := 'Stop Webcam Capture!';
    // Create a capture window
    CaptureWnd := capCreateCaptureWindowA('CaptureWindow',
      WS_CHILD or WS_VISIBLE, Image1.Left, Image1.Top, Image1.Width,
      Image1.Height, Handle, 0);
    if CaptureWnd <> 0 then
    begin
      // Connect to the selected video capture driver
      SendMessage(CaptureWnd, WM_CAP_DRIVER_CONNECT, DeviceIndex, 0);
      SendMessage(CaptureWnd, WM_CAP_SET_PREVIEWRATE, 40, 0); // Set preview rate
      SendMessage(CaptureWnd, WM_CAP_SET_PREVIEW, 1, 0); // Enable preview mode
    end
    else
      MessageDlg('Error creating capture window', mtError, [mbOK], 0);
  end
  else
  begin
    // Stop capturing video and disconnect from the video capture driver
    if CaptureWnd <> 0 then
    begin
      SendMessage(CaptureWnd, WM_CAP_DRIVER_DISCONNECT, 0, 0);
      CaptureWnd := 0; // Reset the capture window handle
    end;
    Self.Button1.Caption := 'Start Webcam Capture!';
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Stop capturing video and disconnect from the video capture driver
  if CaptureWnd <> 0 then
  begin
    SendMessage(CaptureWnd, WM_CAP_DRIVER_DISCONNECT, 0, 0);
    // Disconnect from the video capture driver
    CaptureWnd := 0; // Reset the capture window handle
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListVideoDevices;
  // Populate the ComboBox with available video capture devices
  if ComboBox1.Items.Count > 0 then ComboBox1.ItemIndex := 0;
  // Select the first item if there are any devices listed
end;

end.
