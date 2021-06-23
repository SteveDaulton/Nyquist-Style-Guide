# Indentation

## Introduction

This document applies to Nyquist code written using LISP syntax only. For examples of SAL syntax, please refer to the official CMU Nyquist documentation. The information contained here aims to follow best practices for other dialects of LISP, and compatibility with official CMU Nyquist documentation. There may be minor deviations from other published standards at the discretion of the author of this guide for the purposes of: 
1. Clarity, 
2. Readability, 
3. Ease of writing.

_Indentation_ is literally the type and amount of space at the beginning of each line of code. More generally it is a set of rules for laying out the code text of programs and scripts, including when to use new lines, choice of white-space characters, choice of comment characters, and so on. It is very important to indent Lisp programs correctly. Counting parentheses is NOT a good way to read code, and should not be necessary if the code is indented correctly.

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

### Dollar , semicolon, or hash + pipe

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

Most LISP code editors will highlight these commented lines so that it is obvious that they are commented out.

      (setq wave (osc (hz-to-step f) dur (maketable wave) ph))
      ; (setq wave (lowpass2 wave (/ *sound-srate* 4) 0.56))
      ; (setq wave (extract-abs wl (+ dur wl)(cue wave)))
      (mult wave amp (/ (peak wave (truncate (* 4 wlength)))))))

**Bad**

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

    ;;; Return the LFO waveform for panning.
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

_WHEN_ takes an arbitrary number of arguments, the first of which is the _test_. The test is written on the first line, and all the other arguments on separate lines below, indented by two spaces:

    (when (> x 0)
      (setf val 0)
      (setf y (* x 2)))

Short _WHEN_ statements may be written on a single line, with a single space between the test expression and the expression to be evaluated:

    (when is-true (print "It is true"))


### "UNLESS" special form

_UNLESS_ can be though of as the opposite of _WHEN_. The indentation follow the same pattern as _WHEN_.

    (unless (> x 0)
      (setf val 0)
      (setf y (* x 2)))

and the single line version:

    (when is-false (print "It is false"))


### "IF", "WHEN" and "UNLESS": Which to use?

In cases where there are two alternatives, the answer is obviously to use _IF_, as this is the only one of the three that provides an _"ELSE"_ clause.

When there is only one choice, use _"WHEN"_ for cases where it is more logical to test for a _True_ value, and use _"UNLESS"_ when it is more logical to test for a _False_ value.

**Bad**

    (if is-true
        (print "It is true"))

**Good**

    (if is-true
        (print "It is true")
        (print "It is false"))

**Good**

    (when is-true
      (print "It is true"))

**Bad**

    (when (not is-true)
      (print "It is not true"))

**Bad**

    (unless (not is-false)
      (print "It is not false))

**Good**

    (unless is-false
      (print "It is false"))


### "LET" special form

_LET_ is a local block construct that contains symbols with optional initializations, and a block of code expressions to evaluate.

The list of local  variables directly follows the LET statement.  The code block begins on the next line, indented by two spaces

    (let (x y z)
      (print x) (print y) (print z))

If the list of local variables is long, it may be split across multiple lines. The list of variables should line up as shown below, but it is acceptable to include multiple short name variables on the final line. The code block is indented 2 spaces relative to the LET line.

    (let (my-first-variable
          another-local-variable
          x y z)
      (setf my-first-variable 12)
      (setf  another-local-variable 42)
      (print x) (print y) (print z))
            
Local variables with bindings should normally have one variable + value pair per line:

    (let ((foo 12)
          (bar 42))
      (print (+ foo bar)))

### "LET*" special form

_LET*_ is much like _LET_ except that bindings are evaluated sequentially. As with _LET_, variable + value pairs should line up vertically, but note that this pushes the start of lines between the normal 2 space columns. The code block is still indented 2 spaces relative to the _LET*_ line.

    (let* ((foo 40)
           (bar (+ foo 2)))
      (print (+ foo bar)))  ;prints 42

Observe that sequential evaluation is _required_ in the above example; _foo_ MUST be evaluated before _bar_.

### "DO" special form

_DO_ is a special form block for looping _while_ a test condition is true.
Ensure that each local variable binding lines up with the first local variable, and that the test condition clause is indented to line up with the first local variable clause.
The code block should be indented two spaces.

    (do ((foo 0 (1+ foo))
         (bar 2 (+ 2 bar)))
        ((= foo 2) bar)
      (format t "foo=~a  bar=~a~%" foo bar))

The above code will print to the debug window:

    foo=0  bar=2
    foo=1  bar=4

and return the value of bar, which then has the value "6".

### "DO*" special form

_LET*_ is much like _LET_ except that the local variable bindings are evaluated sequentially. As with _DO*_, this means that after the first line, and subsequent local variable bindings should be indented one extra space so that they line up, and the test condition clause must also be indented one extra space.

    (do* ((foo 1 (1+ foo))
          (bar (* 2 foo) (* 2 foo)))
         ((= 3 foo) bar)
      (format t "foo=~a  bar=~a~%" foo bar))

The above prints to the debug window:

    foo=1  bar=2
    foo=2  bar=4

and returns "6".


### "COND" special form

_COND_ evaluates a series of predicate / expression pairs in sequential order. When one of the expressions evaluates to non-NIL, the associated expression or code block is evaluated and the result is returned. If all of the test expressions evaluate to NIL, then NIL is returned.

If the lines are sufficiently short, they may simply be lined up as shown below:

**Good**

    (setf val 2)
    (cond ((not (boundp 'val)) (print "Unknown val"))
          ((not (numberp val)) (print "val is not a number"))
          ((= val 1) (print "val = 1"))
          ((= val 2) (print "val = 2"))
          ((= val 3) (print "val = 3"))
          (t (print "val is not 1, 2 or 3")))

In more complex cases with larger code blocks, you may be tempted to start the code block on a new line, indented by two spaces, but consider rewriting the code so that this is not necessary.

**Not nice**

    (let* ((foo (foo-func))
           (bar (get-bar foo)))
      (cond
        ((and foo (> bar 0))
          (let ((val (/ foo bar)))
            ;more stuff with some longer lines so
            ;we are trying to conserve horizontal space.
            val))
        ((and bar (> foo 0))
          (let ((val (/ foo bar)))
            ;more stuff
            val))
        (t nil)))

The above might be better written as:

**Better**

    (let* ((foo (foo-func))
           (bar (get-bar foo)))
      (cond ((and foo (> bar 0))
             (my-foo-function foo bar))
            ((and bar (> foo 0))
             (my-foo-function bar foo))
            (t nil)))


### "CASE" special form

_CASE_ is a special form that evaluates one expression, which is then compared with a series of _values_. When a match is found, the code block _"action"_ associated with that value is evaluated and returned as the result.

The dominant style in Nyquist if for the first expression to be on the first line, and the _value + action_ pairs to be indented two spaces below:

    (case order
      (1 (lp sig low-hz))
      (2 (lowpass2 sig low-hz))
      (4 (lowpass4 sig low-hz))
      (6 (lowpass6 sig low-hz))
      (8 (lowpass8 sig low-hz))
      (t (error "Filter order invalid" order)))

However, opinions appear to be divided over the correct way to format the _CASE_ special form, and other styles as shown below may be encountered. The dominant style (above) is encouraged, but whichever style you choose, use it consistently. Do NOT mix styles within a code project.

    (case val (1 (print "one"))
              ((2 3) (print "two or three"))
              (t (print "Not 1, 2 or 3")))


    (case val (1     (print "one"))
              ((2 3) (print "two or three"))
              (t     (print "Not 1, 2 or 3")))


    (case val
      (1 (print "one"))
      ((2 3) (print "two or three"))
      (t (print "Not 1, 2 or 3")))


    (case val
      (1      (print "one"))
      ((2 3)  (print "two or three"))
      (t      (print "Not 1, 2 or 3")))




