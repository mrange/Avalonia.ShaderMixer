/*
ShaderMixer for Avalonia - Mixes ShaderToy like shaders
Copyright (C) 2024  Mårten Rånge

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses
*/

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

