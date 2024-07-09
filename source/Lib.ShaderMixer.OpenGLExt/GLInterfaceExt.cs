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

namespace Lib.ShaderMixer.OpenGLExt;
using global::Avalonia.OpenGL;


public unsafe partial class GlInterfaceExt
{
  // DebugMessageCallback
  readonly delegate* unmanaged[Stdcall]<IntPtr,IntPtr,void> _addr_DebugMessageCallback;

  // Disable
  readonly delegate* unmanaged[Stdcall]<int,void> _addr_Disable;

  // GetIntegerv
  readonly delegate* unmanaged[Stdcall]<int,int*,void> _addr_GetIntegerv;

  // Uniform1i
  readonly delegate* unmanaged[Stdcall]<int,int,void> _addr_Uniform1i;

  // Uniform2f
  readonly delegate* unmanaged[Stdcall]<int,float,float,void> _addr_Uniform2f;


  public GlInterfaceExt(GlInterface glInterface)
  {
    var addr = IntPtr.Zero;

    // DebugMessageCallback
    addr = glInterface.GetProcAddress("glDebugMessageCallback");
    if (addr == IntPtr.Zero)
    {
      throw new EntryPointNotFoundException("glDebugMessageCallback");
    }
    _addr_DebugMessageCallback = (delegate* unmanaged[Stdcall]<IntPtr,IntPtr,void>)addr;


    // Disable
    addr = glInterface.GetProcAddress("glDisable");
    if (addr == IntPtr.Zero)
    {
      throw new EntryPointNotFoundException("glDisable");
    }
    _addr_Disable = (delegate* unmanaged[Stdcall]<int,void>)addr;


    // GetIntegerv
    addr = glInterface.GetProcAddress("glGetIntegerv");
    if (addr == IntPtr.Zero)
    {
      throw new EntryPointNotFoundException("glGetIntegerv");
    }
    _addr_GetIntegerv = (delegate* unmanaged[Stdcall]<int,int*,void>)addr;


    // Uniform1i
    addr = glInterface.GetProcAddress("glUniform1i");
    if (addr == IntPtr.Zero)
    {
      throw new EntryPointNotFoundException("glUniform1i");
    }
    _addr_Uniform1i = (delegate* unmanaged[Stdcall]<int,int,void>)addr;


    // Uniform2f
    addr = glInterface.GetProcAddress("glUniform2f");
    if (addr == IntPtr.Zero)
    {
      throw new EntryPointNotFoundException("glUniform2f");
    }
    _addr_Uniform2f = (delegate* unmanaged[Stdcall]<int,float,float,void>)addr;


  }

  // DebugMessageCallback
  public void DebugMessageCallback(
      IntPtr v0
    , IntPtr v1
    )
  {
    _addr_DebugMessageCallback(
      v0
    , v1
    );
  }

  // Disable
  public void Disable(
      int v0
    )
  {
    _addr_Disable(
      v0
    );
  }

  // GetIntegerv
  public void GetIntegerv(
      int v0
    , int* v1
    )
  {
    _addr_GetIntegerv(
      v0
    , v1
    );
  }

  // Uniform1i
  public void Uniform1i(
      int v0
    , int v1
    )
  {
    _addr_Uniform1i(
      v0
    , v1
    );
  }

  // Uniform2f
  public void Uniform2f(
      int v0
    , float v1
    , float v2
    )
  {
    _addr_Uniform2f(
      v0
    , v1
    , v2
    );
  }

}

