# Description
This lisp package enables string literals just like in the C programming language.

Normal lisp string literals are relatively limited and it is difficult to include special caracters such as newlines or tabs.
The package `c-string-literals` defines a read macro that makes it possible to write

    #"Hello, world!\n"

anywhere in the code which results in a string with a newline at the end. Note the `#` dispatching macro character in front of the string.

# Usage
To use the package, first load it using ASDF:

    (require :asdf)
    (asdf:load-system :c-string-literals)

## Enabling the read macro
Once the package is loaded, it is enough to call a single function:

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (c-string-literals:enable-c-string-literals))

The call to `eval-when` is necessary to ensure that the code is evaluated before reading the code following the statement.

### Completely replace lisp string literals
**Warning:** *This is may cause subsequently loaded code to misbehave or run more slowly.*

It is possible to completely replace lisp string literals by calling

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (c-string-literals:enable-c-string-literals :replace))

It is then possible to write

    "Hello, world!\n"

without the `#` dispatching macro character.

## Using the read macro
After loading the read macro, C string literals can be used anywhere:

    (dotimes (x 10)
      (princ #"Hello, world!\n"))

The different escape sequences `\a` (alert/bell), `\b` (backspace), `\f` (form feed), `\n` (newline),
`\r` (carriage return), `\t` (horizontal tab), `\v` (vertical tab), `\\` (backslash), `\"` (double quote),
`\0` (null character) as well as the `\ooo` (octal) and `\xhh` (hexadecimal) codes are all supported.
