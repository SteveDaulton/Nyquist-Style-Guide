# Indentation

## Introduction

This document applies to Nyquist code written using LISP syntax only. For examples of SAL syntax, please refer to the official CMU Nyquist documentation. The information contained here aims to follow best practices for other dialects of LISP, and compatibility with official CMU Nyquist documentation. There may be minor deviations from other published standards at the discretion of the author of this guide for the purposes of: 
1. Clarity, 
2. Readability, 
3. Ease of writing.

_Indentation_ is literally the type and amount of space at the beginning of each line of code. More generally it is a set of rules for laying out the code text of programs and scripts, including when to use new lines, choice of whitespace characters, choice of comment characters, and so on. It is very important to indent Lisp programs correctly. Counting parentheses is NOT a good way to read code, and should not be necessary if the code is indented correctly.

With few exceptions, indentation rules are _not_ a matter of personal opinion, but a matter of _shared culture_. The rules for properly indenting Lisp programs have evolved over many years in the Lisp community and are extremely important for maintainability. Programs are frequently maintained by people other than the original developer, so they should be written in a way that can be easily read by other LISP programmers.

These are the most common rules for indenting Lisp programs:

## General rules

### Use spaces

Do NOT use tab characters in LISP code. Tabs are interpreted differently by different text editors and are likely to make your code look like a mess for anyone using a different text editor.

### Top-level functions

All top-level function definitions start in column 1. This is not negotiable. 

### Closing parentheses

Closing parentheses are never preceded by newlines.

**Bad**

    (defun f (x)
      (when (< x 3)
         (* x 2)
      )
    )

**Good**

    (defun f (x)
      (when (< x 3)
        (* x 2)))


### Amount of indentation

Unless more specific rules state otherwise, use two spaces:

**Bad**

    (defun f (x)
        (when (< x 3)
                (* x 2)))

**Good**

    (defun f (x)
      (when (< x 3)
        (* x 2)))


## Comments

## Dollar , semicolon or hash + pipe

Audacity uses the "$" (dollar) character in place of semicolons for _translatable_ headers in plug-ins that are shipped with Audacity. This should _only_ be used in plug-ins that are shipped with Audacity as these are the only plug-ins that Audacity has translations for. All other plug-ins and scripts should _always_ use one semicolon. Translatable strings in plug-in headers must be wrapped in the UNDERSCORE function:

**Shipped plug-in header**

    $nyquist plug-in
    $version 4
    $type process
    $name (_ "High-Pass Filter")

**Any other plug-in**

    ;nyquist plug-in
    ;version 4
    ;type process
    ;name "My Filter"

The **#|** and **|#** symbols may be used to temporarily remove a block of code, but is not recommended for production code:

**Good**

    ;; Released under terms of the GNU General Public License version 2:
    ;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
    ;;
    ;; For information about writing and modifying Nyquist plug-ins:
    ;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

**May be useful while developing but not in production code**

      (setq wave (osc (hz-to-step f) dur (maketable wave) ph))
    #|
      (setq wave (lowpass2 wave (/ *sound-srate* 4) 0.56))
      (setq wave (extract-abs wl (+ dur wl)(cue wave)))
    |#
      (mult wave amp (/ (peak wave (truncate (* 4 wlength)))))))

**Better**

      (setq wave (osc (hz-to-step f) dur (maketable wave) ph))
      ; (setq wave (lowpass2 wave (/ *sound-srate* 4) 0.56))
      ; (setq wave (extract-abs wl (+ dur wl)(cue wave)))
      (mult wave amp (/ (peak wave (truncate (* 4 wlength)))))))

**Not recommended**

    #|
    Released under terms of the GNU General Public License version 2:
    http://www.gnu.org/licenses/old-licenses/gpl-2.0.html

    For information about writing and modifying Nyquist plug-ins:
    https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference
    |#


### Number of semicolons

When a comment refers to the preceding code on the same line, use a single semicolon:

    (if (> val f-limit) ; is it above highest allowed?
        (cleanup val)   ; if so, bail out
        (process val))  ; otherwise process it.

When a comment refers to several lines of code after the comment, use two semicolons:

    (when (> val f-limit)
      ;; Reinitialise and bail out.
      (setf val 0)
      (cleanup val))

Comments that document a function should have three semicolons:

    ;;; Evaluate a string as a LISP expression.
    ;;; If 'string' is not a valid LISP expression, the behaviour is undefined.
    (defun eval-string (string)
      (eval (read (make-string-input-stream string))))

or

    (defun eval-string (string)
      ;;; Evaluate a string as a LISP expression.
      ;;; If 'string' is not a valid LISP expression, the behaviour is undefined.
      (eval (read (make-string-input-stream string))))

### Comment contents

Be brief and to the point. Use the imperative form when documenting functions. Avoid redundant expressions such as *This function...*.

**Bad**

    ;;; This function returns the LFO waveform for panning
    (defun get-lfo (offset sign range rate *waveform* phase)
      (let ((sig (lfo rate 1.0 *waveform* phase)))
        (setf sig (mult sign 0.5 sig))
        (mult range sig)))



**Good**

    ;;; Returns the LFO waveform for panning.
    (defun get-lfo (offset sign range rate *waveform* phase)
      (let ((sig (lfo rate 1.0 *waveform* phase)))
        (setf sig (mult sign 0.5 sig))
        (mult range sig)))

## Indenting special forms

Special forms each have their own indentation rules. These are some common examples:

### "IF" special form

The **IF** special form has exactly three arguments; The _test_ expression, the _then_ expression, and the _else_ expression. The three arguments should line up vertically:

    (if (= x 0)
        (process-a x)
        (process-b x))


### "WHEN" special form

_WHEN_ takes an arbitrary number of arguments, the first of which is the _test_. The test is written on the first line, and all the otherarguments on separate linesbelow, indented by two spaces:

    (when (> x 0)
      (setf val 0)
      (setf y (* x 2)))


### "LET" special form

### "LET*" special form

### "DO" special form

### "DO*" special form

### "COND" special form

### "CASE" special form
