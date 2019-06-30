# Usage

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
