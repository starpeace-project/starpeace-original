unit CompressIntf;

interface

  uses
    Windows,
    FIFOIntf;

  type
    ICompressor =
      interface['{55624380-6518-11d3-B638-00400566F3E8}']
        function SetOutputFIFO(const aFIFO : IFIFO) : hResult; stdcall;
        function GetOutputFIFO(out aFIFO : IFIFO) : hResult; stdcall;

        function Reset : hResult; stdcall;
        function GetInputType(out SamplingRate, BitsPerSample, Channels : integer) : hResult; stdcall;
        function Convert(const Data; Size : integer) : hResult; stdcall;
      end;

    IDecompressor =
      interface['{55624381-6518-11d3-B638-00400566F3E8}']
        function SetInputFIFO(const aFIFO : IFIFO) : hResult; stdcall;
        function GetInputFIFO(out aFIFO : IFIFO) : hResult; stdcall;

        function Reset : hResult; stdcall;
        function GetOutputType(out SamplingRate, BitsPerSample, Channels : integer) : hResult; stdcall;
        function GetAvailData(out Actual : integer) : hResult; stdcall;
        function Convert(out Data; Size : integer; out Actual : integer) : hResult; stdcall;
      end;

implementation

end.
 