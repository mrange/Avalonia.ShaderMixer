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

public static class GlConstsExt
{
    public const int GL_RED                             = 0x1903;
    public const int GL_TEXTURE_WRAP_S                  = 0x2802;
    public const int GL_TEXTURE_WRAP_T                  = 0x2803;
    public const int GL_CLAMP                           = 0x2900;
    public const int GL_REPEAT                          = 0x2901;
    public const int GL_CLAMP_TO_EDGE                   = 0x812F;
    public const int GL_R32F                            = 0x822E;
    public const int GL_MIRRORED_REPEAT                 = 0x8370;
    public const int GL_RGBA32F                         = 0x8814;
    public const int GL_ARRAY_BUFFER_BINDING            = 0x8894;
    public const int GL_ELEMENT_ARRAY_BUFFER_BINDING    = 0x8895;
    public const int GL_DEBUG_OUTPUT                    = 0x92E0;
}
