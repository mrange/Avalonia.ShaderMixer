namespace Lib.ShaderMixer.OpenGLExt;

using System.Numerics;
using System.Runtime.InteropServices;


[StructLayout(LayoutKind.Sequential, Pack = 4)]
public struct Vertex
{
    public Vector3 Position;
    public Vector2 TexCoord;
}

