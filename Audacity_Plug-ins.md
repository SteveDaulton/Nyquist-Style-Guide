# Appendix 1 - Audacity Plug-ins

**Contents**
* [Introduction](Introduction)
* [Supported Return Types](Supported_Return_Types)
* [Unsupported Return Types](Unsupported_Return_Types)
* [Plug-in Headers](Plug-in_Headers)
* [Scripting Commands](Scripting_Commands)
* [Case Sensitivity](Case_Sensitivity)


## Introduction

[Audacity](https://audacityteam.org) audio editor includes a version of the Nyquist programming language.
With a few exceptions, Nyquist in Audacity is the same as the stand-alone Nyquist programming language. A
few Nyquist functions have been intentionally omitted from Audacity's version, either for safety, or
because they cannot be supported within the Audacity framework. Audacity's version of Nyquist also has
a few additional extensions that are only applicable to running Nyquist within Audacity.

The simplest way to run Nyquist code within Audacity is to enter the code in Audacity's
[Nyquist Prompt](https://manual.audacityteam.org/man/nyquist_prompt.html) tool. When code is run in the
Nyquist Prompt, the value returned by the code is sent back to Audacity, and if of a
[supported type](Supported_Return_Types), Audacity will render the result. Returning an unsupported type
produces an error. The Nyquist Prompt can be very useful for running short snippets of Nyquist Code.

For running more complex Nyquist code, it is often more convenient to create a reusable Nyquist script which
may be installed as a plug-in effect. This requires that the script includes special
[comments](Spacing_and_Indentation#comments) to tell Audacity to wrap the code with a plug-in interface.
These special comments are called [plug-in headers](Plug-in_Headers).


## Supported Return Types

Nyquist supports many "data types", including "numbers" (integer or floating-point), "characters"
(such as the letter "A", the number "4", or any other ASCII character), "strings" (text), "list" (a list of data),
"array" (special kind of indexed list), and "sounds" (a sound / digital signal).

The result of the last computation within the plug-in code (the _return value_) will be given back from Nyquist
to Audacity. According to the data type of the returned value one of the following actions will be invoked in Audacity:

### Mono Sound

If a _sound_ is returned, the sound will be re-inserted into the selected part of the Audacity track,
(or a new track for "generate" type plug-ins). If the returned sound is shorter or longer than the original sound,
the selection will be reduced or augmented. If a mono sound is returned to a stereo track, the same mono sound will
be inserted into both channels of the stereo track.

### Multi-Channel / Stereo Sound

Nyquist handles multi-channel sounds as an array of sounds. The first element of the array is the left channel,
and the second element is the right channel. Audacity currently supports a maximum of two channels in a track (stereo).

To return a stereo sound without error, a stereo track must be selected before running the Nyquist code.
Returning an array of sounds to a mono track is an error.

For more information about stereo tracks, see the
[Nyquist Stereo Track Tutorial](https://web.archive.org/web/20230405140615/https://wiki.audacityteam.org/wiki/Nyquist_Stereo_Track_Tutorial).

### String / Text

When the return value is a character or string, a dialog window will appear with the data displayed as text.

### Number

When the return value is a number, a dialog window will appear with the number displayed as text.

### Labels
_(Audacity only)_

If an appropriately formatted list is returned to Audacity, a label track will be created below the audio track(s).

For point labels the format is:
```
((number "string") (number "string") ... )
```
Where:
* number - (an integer or float) is the time in seconds from the beginning of the Audacity selection, where the label will appear.
* "string" - a string to be displayed in the label's text field.

For region labels the format is:
```
((number number "string") (number number "string") ... )
```
Where the numbers are the start and end times of the region.

Audacity places returned audio or labels relative to the start of the selection, not the start of the Timeline.

### Empty String
_(Audacity only)_

Audacity treats an empty string as a [no-op](https://en.wikipedia.org/wiki/NOP_(code)) or "null return".
It means that the plug-in returns nothing, and no error. This is particularly useful when writing
[Nyquist Macros](Scripting_Commands).


## Unsupported Return Types

Any data types that are not explicitly [supported return types](Supported_Return_Types) are unsupported.
When a Nyquist plug-in returns an unsupported data type, this should be considered to be an error / bug.

Unsupported return types include:
* **Arrays** - other than [an array of 2 sounds](Multi-Channel_/_Stereo_Sound).
* **Lists** - other than specially formatted [Label lists](Labels).
* **Null Sounds** - Sound objects with zero length.
* **Multi-channel Sounds** with more than 2 channels.
* **Booleans** - `t` (True) and `NIL` (False) produce silent errors.


## Plug-in Headers

In Audacity, special comments are used in Nyquist plug-ins to pass information to Audacity.
As with other code comments, these are ignored entirely by Nyquist, but provide instructions to tell Audacity how
to create the plug-in.

Details may be found here: [Nyquist Plug-in Headers](https://web.archive.org/web/20230405012530/https://wiki.audacityteam.org/wiki/Nyquist_Plug-in_Headers)


## Scripting Commands

Nyquist is able to send [Scripting Commands](https://manual.audacityteam.org/man/scripting.html) to Audacity.
Note that these are exactly the same commands as are used for creating [Audacity Macros](https://manual.audacityteam.org/man/macros.html)
and scripting with Python. A Nyquist plug-in that modifies the project using scripting commands (rather than
processing in Nyquist) is called a _Nyquist Macro_.


## Case Sensitivity

Nyquist is case-insensitive (symbols are converted to upper-case internally, so for example `list`, `List` and `LIST` are
all identical. However, there are at least a couple of _gotchas_.

1. In 2015 I observed that keywords in SAL syntax were case-sensitive.
[Roger Dannenberg](https://www.cs.cmu.edu/~rbd/) confirmed that this was a bug rather than a feature.
This is now fixed in Audacity.
2. Case is preserved in double-quoted strings, but not in single quoted symbols.
```
# a double-quoted string
"This_is_Case_Sensitive"

# a quoted symbol, converted internally to 'NOT_CASE_SENSITIVE
'Not_CaSe_sEnSiTiVe
```
3. Watch out for file names as strings. Depending on the local file system, paths and file names
may or may-not be case-sensitive.

### Case convention

For Nyquist and XLISP, the convention is to write code in lower-case.

An exception to this in Nyquist plug-ins is to use upper-case for global constants.
The rationale for this is described in [Audacity Issue 4438](https://github.com/audacity/audacity/issues/4438).


## See Also

* [Nyquist Plug-ins Reference](https://web.archive.org/web/20230405064412/https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference#Return_Values)
* [Nyquist Plug-in Headers](https://web.archive.org/web/20230405012530/https://wiki.audacityteam.org/wiki/Nyquist_Plug-in_Headers)
* [Scripting Reference](https://manual.audacityteam.org/man/scripting_reference.html)
