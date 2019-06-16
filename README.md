# jscm

JavaScript compiler for multi-threading. Examine the top-level function in the JavaScript source code and output the function as multi-threaded source code using Web Workers API.

## Requirement

- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

## Install

1. Clone this repository and open it in Terminal.
2. Execute `stack install`, it will be installed in `~/.local/bin`.

If you are using Docker, please execute `docker build . -t jscm`. And if you want to remove this command, please execute `rm ~/.lcoal/bin/jscm`.

## Usage

This program should execute with the following command.

```
$ jscm -i input.js > output.js
```

If you are using Docker, execute the following command.

```
$ docker run --rm -it jscm -i input.js > output.js
```

To execute, prepare the following HTML file and execute it on a browser.

```html
<html>
  <head>
    <script src="output.js"></script>
  </head>
  <body></body>
</html>
```

## Specification

Based on the ECMAScript 2015 specification and the Web Workers API. However, some syntaxes are not complete and can not be used.

- `const` and `let` can not be used.
- `var x, y` and `var x;` does not work.

The JavaScript source code used in this command must be a single file. Do not use `import` or `require`, script tag and the others.
