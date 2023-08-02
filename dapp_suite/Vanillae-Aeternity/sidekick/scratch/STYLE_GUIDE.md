# Style Guide

- Generally, try to write TypeScript that is more like Erlang and
  less like Javascript

- Code is read more often than it is written. So be kind to people
  reading the code. Make it obvious and simple.

- All of these rules are fuzzy and there are times where it is good
  or necessary to break them. You're an adult, use your best
  judgment.

- It's OK to write code with a larger number of lines if the result
  is code that is more readable.

- Don't try to be l33t. Boring code is best

- Don't sacrifice readability for "performance". It's far easier to
  optimize inefficient/correct code than to correct
  efficient/incorrect code.

- It should be simple. It should just work. And it should be obvious
  to anyone reading the code why it works.

- Prefer `let` over `var` or `const`

- Avoid taking advantage of mutability. A recursive function is
  usually better than a loop

```js

```

- Put your "this does stuff" code in a function called `main` and
  then call it.

```js
// no
do_something();
do_something_else();

// yes
function main()
{
    do_something();
    do_something_else();
}
main();
```

- Every `if` should have an `else`

```js
// no
function foo(bar)
{
    if (some_condition(bar))
    {
        return;
    }

    do_something();
    do_something_else();
}

// yes
function foo(bar)
{
    if (some_condition(bar))
    {
        return;
    }
    else
    {
        do_something();
        do_something_else();
    }
}
```

- Avoid hidden state like the plague (this means don't make objects;
  use records instead). All of your ingredients should be declared
  visually near where they are used.

  This means no oopy jogger jizz, "stamps", etc

- Every function should have a unique name so that someone unfamiliar
  with the codebase can `grep -rn function_name` and find it
  immediately

- Do not use the ternary operator `returnValue = condition ? ifTrue :
  else`. It's just obnoxious (most of the time)

- In general, avoid binary operators. If you must use them, don't be
  niggardly with parentheses. Disambiguating infix precedence is a
  nightmare.

- Inequalities should follow the left-to-right orientation of the number line

```js
// cringe
let x = y > z;

// based
let x = z < y;
```

- Don't assume the person reading the code is a domain expert in
  JavaScript. Avoid using weird JS syntax or runtime quirks.

- If you must, put weird things in variables or functions that have a
  semantic name

```js
// wat
if (window == window.parent)
{
    ...
}
else
{
    ...
}

// obvious
let we_are_executing_this_in_a_browser = (window == window.parent);
if (we_are_executing_this_in_a_browser)
{
    ...
}
else
{
    ...
}
```

- Names should be in `snake_case`

- Indentation is 4 spaces

- Align things that deserve to be aligned

```js
function foo(bar  : x,
             baz  : y,
             quux : z)
{
}
```

- Open and close braces should align vertically so someone reading
  can just scan to find the other delimiter.

```js
// yuck
if (condition) {
    ...
} else if (some_other_condition) {
    ...
} else {
    ...
}

// nice
// it's easy for a reader to visually match the open/close braces,
// and then to look at the line immediately above the opening brace
// to see what the code block corresponds to
if (condition)
{
    ...
}
else if (some_other_condition)
{
    ...
}
else
{
    ...
}
```

- Avoid sawtoothy code with a lot of nesting. If you're writing
  sawtoothy code, you should probably factor it into many simple
  functions.

- It is much easier to understand 20 simple functions than 1
  complicated function.
