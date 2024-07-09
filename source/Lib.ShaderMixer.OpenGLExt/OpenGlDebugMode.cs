using Avalonia.OpenGL;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace Lib.ShaderMixer.OpenGLExt;

public unsafe class OpenGlDebugMode : IDisposable
{
    GlInterface     _gl     ;
    GlInterfaceExt  _glext  ;

    static OpenGlDebugMode()
    {
        _debugProc = &DebugProc;
    }

    static readonly delegate* unmanaged[Stdcall]<
            uint
        ,   uint
        ,   uint
        ,   uint
        ,   int
        ,   IntPtr
        ,   IntPtr
        ,   void
        > _debugProc;

    [UnmanagedCallersOnlyAttribute(CallConvs = [typeof(CallConvStdcall)])]
    static void DebugProc(
            uint    source
        ,   uint    type
        ,   uint    id
        ,   uint    severity
        ,   int     length
        ,   IntPtr  message
        ,   IntPtr  userParam
        )
    {
        try
        {
            var messageString = Marshal.PtrToStringAnsi(message, length);
            Debug.WriteLine($"OpenGL Debug: {messageString}");
        }
        catch
        {
            // Don't throw exception from an unmanaged callback proc
        }
    }
    public OpenGlDebugMode(GlInterface gl, GlInterfaceExt glext)
    {
        _gl     = gl    ;
        _glext  = glext ;

        _gl.Enable(GlConstsExt.GL_DEBUG_OUTPUT);
        _glext.DebugMessageCallback(new IntPtr(_debugProc), IntPtr.Zero);
    }
    public void Dispose()
    {
        _glext.DebugMessageCallback(IntPtr.Zero, IntPtr.Zero);
        _glext.Disable(GlConstsExt.GL_DEBUG_OUTPUT);
    }
}

