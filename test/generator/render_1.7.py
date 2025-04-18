import xcffib
import struct
import io
from dataclasses import dataclass
MAJOR_VERSION = 0
MINOR_VERSION = 11
key = xcffib.ExtensionKey("RENDER")
_events = {}
_errors = {}
@dataclass(init=False)
class PictOp:
    Clear = 0
    Src = 1
    Dst = 2
    Over = 3
    OverReverse = 4
    In = 5
    InReverse = 6
    Out = 7
    OutReverse = 8
    Atop = 9
    AtopReverse = 10
    Xor = 11
    Add = 12
    Saturate = 13
    DisjointClear = 16
    DisjointSrc = 17
    DisjointDst = 18
    DisjointOver = 19
    DisjointOverReverse = 20
    DisjointIn = 21
    DisjointInReverse = 22
    DisjointOut = 23
    DisjointOutReverse = 24
    DisjointAtop = 25
    DisjointAtopReverse = 26
    DisjointXor = 27
    ConjointClear = 32
    ConjointSrc = 33
    ConjointDst = 34
    ConjointOver = 35
    ConjointOverReverse = 36
    ConjointIn = 37
    ConjointInReverse = 38
    ConjointOut = 39
    ConjointOutReverse = 40
    ConjointAtop = 41
    ConjointAtopReverse = 42
    ConjointXor = 43
    Multiply = 48
    Screen = 49
    Overlay = 50
    Darken = 51
    Lighten = 52
    ColorDodge = 53
    ColorBurn = 54
    HardLight = 55
    SoftLight = 56
    Difference = 57
    Exclusion = 58
    HSLHue = 59
    HSLSaturation = 60
    HSLColor = 61
    HSLLuminosity = 62
xcffib._add_ext(key, render_1._7Extension, _events, _errors)
