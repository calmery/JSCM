# JSCM

UNDER DEVELOPMENT: JavaScript compiler for multithreading. Examine the top-level function in the JavaScript source code and output the function as multithreaded source code using Web Workers API.

## Requirement

[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) or [Docker](https://www.docker.com/).

## Install

1. Clone this repository and open it in Terminal.
2. Execute `stack install`, it will be installed in `~/.local/bin`.

If you are using Docker, please execute `docker build . -t jscm`. And if you want to remove this command, please execute `rm ~/.lcoal/bin/jscm`.

## Usage

```
$ jscm --help
jscm - JavaScript compiler for multithreading

Usage: jscm [-d|--dump] (-i|--input FILE)

Available options:
  -h,--help                Show this help text
  -d,--dump                Show AST
  -i,--input FILE          Input file name
```

This program should execute with the following command.

```
$ jscm -i input.js > output.js
```

If you are using Docker, execute the following command.

```
$ docker run --rm -it jscm -i input.js > output.js
```

To execute, prepare the following HTML file and execute it on a browser. I recommend using Google Chrome.

```html
<html>
  <head>
    <script src="output.js"></script>
  </head>
  <body></body>
</html>
```

If the conversion fails, it may be a parsing error. You can output the result of parsing by adding the option `--dump` or `-d`.

```
$ jscm -i input.js -d
```

## Specification

What appears to be a referentially transparent function are converted to source code using Web Workers API. Also, to receive the result, the function is converted to a function that returns a `Promise`. This compiler is based on the ECMAScript 2015 specification and the Web Workers API. However, some syntaxes are not complete and can not be used.

- `const` and `let` can not be used.
- `var x, y` and `var x;` does not work.
- For other, please refer to `test/ParserSpec.hs`.

The JavaScript source code used in this command must be a single file. Do not use `import` or `require`, script tag and the others. And conversion does not work well if there is a function call before the function definition, and if you rewrite the built-in function or object.

## License

This software is released under the BSD 3-Clause "New" or "Revised" License.

## Author

[Marei Kikukawa](https://github.com/calmery)
