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