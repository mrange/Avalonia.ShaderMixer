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

using System;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[InlineArray(8)]
public struct Buffer8<T>
    where T : unmanaged
{
    T _element0;

    [UnscopedRef]
    internal unsafe T* AsPtr()
    {
        return (T*)Unsafe.AsPointer(ref _element0);
    }

    [UnscopedRef]
    public Span<T> AsSpan()
    {
        return MemoryMarshal.CreateSpan(ref _element0, 8);
    }

    [UnscopedRef]
    public readonly ReadOnlySpan<T> AsReadOnlySpan()
    {
        return MemoryMarshal.CreateReadOnlySpan(
                ref Unsafe.AsRef(in _element0)
            ,   8
            );
    }

    public T[] ToArray()
    {
        return [..AsReadOnlySpan()];
    }
}

public struct GetValues<T>
    where T : unmanaged
{
    public int Id;
    public Buffer8<T> Values;
}

public unsafe partial class GlInterfaceExt
{
  public GetValues<int> GetIntegerv(
      int v0 
    )
  {
    GetValues<int> vs = default;
    vs.Id = v0;

    _addr_GetIntegerv(
      v0 
    , vs.Values.AsPtr()
    );
    return vs;
  }
}