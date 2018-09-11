# orgtt.el [![Build Status](https://travis-ci.org/nick96/orgtt.el.svg?branch=master)](https://travis-ci.org/nick96/orgtt.el)

Creating truth tables is boring, especially when you get more than 2 or 3 variables. They're easy and mechanical but
it can be easy to make still mistakes. Wait! That sounds perfect for automation!

`orgtt.el` automates the creating of truth tables from a given formula. It creates the org table for you with all possible
valuations already inserted. It provides two functions for you to call interactively, `orgtt-create-table` and
`orgtt-create-table-and-solve` (WIP).

## `orgtt-create-table`

`orgtt-create-table` will prompt you to enter a formula (e.g. A + B) and it will insert an org table with rows for
all of the possible valuations for the variables filled in. Like the one below.

```
| A   | B   | A + B |
|-----+-----+-------|
| nil | nil |       |
| nil | t   |       |
| t   | nil |       |
| t   | t   |       |
```

You can easily define a table formula to fill in the column for the formula based by adding 
`#+TBLFM: $3='(or (intern $1) (intern $2))` where `intern` is a built-in emacs functions which turns the string "nil"
into nil and the string "t" into t. You can evaluate this formula by placing your cursor over it and pressing `C-c C-c`
giving you the table below.

```
| A   | B   | A + B |
|-----+-----+-------|
| nil | nil | nil   |
| nil | t   | t     |
| t   | nil | t     |
| t   | t   | t     |
#+TBLFM: $3='(or (intern $1) (intern $2))
```


## `orgtt-create-table-and-solve`

*This is currently a work in progress*

As with `orgtt-create-table`, `orgtt-create-table-and-solve` will prompt you for a formula. The difference is
that it will not only give you a table with all the valuations for the given formula, it will give you a table
including the evaluation of the formula. Like in the example of the second table above.

The formula syntax you can use is defined in `orgtt-connective-alist` and you can change it by adding or removing
things but by default you are able to use to following syntax in your formulas:

| Syntax | Meaning              |
|:------:|:---------------------|
| -      | Negation             |
| .      | Logical and          |
| +      | Logical or           |
| ->     | Material implication |
| <->    | Biimplication        |
| <+>    | Exclusive or         |

## Options

You can customise aspects of `orgtt.el` to suit your needs and preferences.

### `orgtt-connective-alist`

This is an alist mapping connectives to their functionality. By default it is:

``` emacs-lisp
'(("-"   . orgtt--negate)
  ("+"   . orgtt--lor)
	("."   . orgtt--land)
	("->"  . orgtt--implication)
	("<->" . orgtt--biimplication)
	("<+>" . orgtt--xor))
```

It effects how the input formula in `orgtt-create-table-and-solve` is converted into a formula usable
by orgtbl.

You can extend the list of usable connectives by adding to this list. For the sake of simplicity, only
unary and binary connectives are allowed.

### `orgtt-use-binary`

By giving `orgtt-use-binary` a truthy value, your tables will use 0 instead of nil and 1 instead of t. Like in the table
below.

```
| A | B | A + B |
|---+---+-------|
| 0 | 0 |       |
| 0 | 1 |       |
| 1 | 0 |       |
| 1 | 1 |       |
```

This could easily be extended to map `t` and `nil` to some string to you could customise however you want.
