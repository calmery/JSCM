# Specification

What appears to be a referentially transparent function are converted to source code using Web Workers API. Also, to receive the result, the function is converted to a function that returns a `Promise`. This compiler is based on the ECMAScript 2015 specification and the Web Workers API. However, some syntaxes are not complete and can not be used.

- `const` and `let` can not be used.
- `var x, y` and `var x;` does not work.
- For other, please refer to `test/ParserSpec.hs`.

The JavaScript source code used in this command must be a single file. Do not use `import` or `require`, script tag and the others. And conversion does not work well if there is a function call before the function definition, and if you rewrite the built-in function or object.
