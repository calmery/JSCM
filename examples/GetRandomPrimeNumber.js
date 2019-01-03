// ECMAScript 5 and ECMAScript 9 (BigInt)

console.log(getRandomPrimeNumber(1024));

/* --- Get a prime number. --- */

/**
 * @param {number} length
 * @return {bigint} Prime number.
 */
function getRandomPrimeNumber(length) {
  var primeCandidate = getRandomPrimeNumberCandidate(length);

  if (isPrime(primeCandidate, 256)) {
    return primeCandidate;
  }

  return getRandomPrimeNumber(length);
}

/* --- Get a prime number candidate. --- */

/**
 * @param {number} length
 * @return {bigint} Prime number candidate.
 */
function getRandomPrimeNumberCandidate(length) {
  var primeCandidate = BigInt(getRandomBits(1024));

  // NOTE: Apply a mask to set MSB and LSB to `1`.
  return (
    primeCandidate | (BigInt(1) << (BigInt(length) - BigInt(1))) | BigInt(1)
  );
}

/**
 * @param {number} length
 * @return {string} Binary number.
 */
function getRandomBits(length) {
  var bits = [];

  for (var i = 0; i < length; i++) {
    bits.push(getRandomNumber(0, 1));
  }

  return "0b" + bits.join("");
}

/* --- Check whether `primeCandidate` is prime or not. --- */

/**
 * @param {bigint} primeCandidate
 * @param {number} numberOfTrials
 * @return {boolean}
 */
function isPrime(primeCandidate, numberOfTrials) {
  if (primeCandidate === BigInt(2) || primeCandidate === BigInt(3)) {
    return true;
  }

  if (primeCandidate <= BigInt(1) || primeCandidate % BigInt(2) === BigInt(0)) {
    return false;
  }

  var s = BigInt(0);
  var r = primeCandidate - BigInt(1);

  for (; (r & BigInt(1)) === BigInt(0); ) {
    s = s + BigInt(1);
    r = r / BigInt(2);
  }

  for (var i = 0; i < numberOfTrials; i++) {
    var randomBigInt = getRandomBigInt(BigInt(2), primeCandidate - BigInt(1));
    var x = modularExponentiation(randomBigInt, r, primeCandidate);

    if (x !== BigInt(1) && x !== primeCandidate - BigInt(1)) {
      for (var j = 1; BigInt(j) < s && x !== primeCandidate - BigInt(1); j++) {
        x = modularExponentiation(x, BigInt(2), primeCandidate);

        if (x === BigInt(1)) {
          return false;
        }
      }

      if (x !== primeCandidate - BigInt(1)) {
        return false;
      }
    }
  }

  return true;
}

/**
 * Modular exponentiation
 * @param {bigint} base
 * @param {bigint} exponent
 * @param {bigint} modulo
 * @return {bigint} Result of `x ** y % z`.
 */
function modularExponentiation(base, exponent, modulo) {
  var result = BigInt(1);

  for (; exponent > BigInt(0); ) {
    if ((exponent & BigInt(1)) === BigInt(1)) {
      result = (result * base) % modulo;
    }

    exponent = exponent >> BigInt(1);
    base = (base * base) % modulo;
  }

  return result;
}

/* --- Get a random `BigInt`. --- */

/**
 * @param {bigint} min
 * @param {bigint} max
 * @return {bigint} A random number between min (inclusive) and max (inclusive).
 */
function getRandomBigInt(min, max) {
  if (min === max) {
    return min;
  }

  if (min > max) {
    // NOTE: Swap two variables.
    min = [max, (max = min)][0];
  }

  var currentDigits = [];

  var minString = min.toString();
  var maxString = max.toString();

  // NOTE: Pads the `minString` with `0` so that the resulting string reaches the `maxString` length.
  // NOTE: Like `Array.padStart` (ECMAScript 2017, ECMAScript 8).
  if (minString.length < maxString.length) {
    var zeros = [];

    for (var i = 0; i < maxString.length - minString.length; i++) {
      zeros.push(0);
    }

    minString = zeros.join("") + minString;
  }

  for (var i = 0; i < minString.length; i++) {
    var currentMinDigit = parseInt(minString[i], 10);
    var currentMaxDigit = parseInt(maxString[i], 10);
    var currentMinDigits = BigInt(minString.slice(0, i + 1));
    var currentMaxDigits = BigInt(maxString.slice(0, i + 1));

    var nextDigit = getRandomNumber(0, 9);
    var nextDigits = BigInt(currentDigits.concat(nextDigit).join(""));

    if (currentMinDigits > nextDigits) {
      nextDigit = getRandomNumber(currentMinDigit, 9);
      nextDigits = BigInt(currentDigits.concat(nextDigit).join(""));

      if (currentMaxDigits < nextDigits) {
        nextDigit = getRandomNumber(currentMinDigit, currentMaxDigit);
      }
    } else if (currentMaxDigits < nextDigits) {
      nextDigit = getRandomNumber(0, currentMaxDigit);
      nextDigits = BigInt(currentDigits.concat(nextDigit).join(""));

      if (currentMinDigits > nextDigits) {
        nextDigit = getRandomNumber(currentMinDigit, currentMaxDigit);
      }
    }

    currentDigits.push(nextDigit);
  }

  return BigInt(currentDigits.join(""));
}

/**
 * @param {number} min
 * @param {number} max
 * @return {number} A random number between min (inclusive) and max (inclusive).
 */
function getRandomNumber(min, max) {
  return Math.floor(Math.random() * (max + 1 - min)) + min;
}

/* --- Result --- */
/*
JSProgram
  [JSCall
    [JSCall
      [JSNumber 1024]
      (JSIdentifier "getRandomPrimeNumber")
    ]
    (JSMember
      (JSIdentifier "log")
      (JSIdentifier "console")
    )
  ,JSFunctionDeclaration
    (JSIdentifier "getRandomPrimeNumber")
    [JSIdentifier "length"]
    (JSBlock
      [JSAssignment
        (JSVariableDeclaration
          (JSIdentifier "primeCandidate")
        )
        (JSCall
          [JSIdentifier "length"]
          (JSIdentifier "getRandomPrimeNumberCandidate")
        )
      ,JSIf
        (JSCall
          [JSIdentifier "primeCandidate"
          ,JSNumber 256
          ]
          (JSIdentifier "isPrime")
        )
        (JSBlock
          [JSReturn
            (JSIdentifier "primeCandidate")
          ]
        )
        Nothing
      ,JSReturn
        (JSCall
          [JSIdentifier "length"]
          (JSIdentifier "getRandomPrimeNumber")
        )
      ]
    )
  ,JSFunctionDeclaration
    (JSIdentifier "getRandomPrimeNumberCandidate")
    [JSIdentifier "length"]
    (JSBlock
      [JSAssignment
        (JSVariableDeclaration
          (JSIdentifier "primeCandidate")
        )
        (JSCall
          [JSCall
            [JSNumber 1024]
            (JSIdentifier "getRandomBits")
          ]
          (JSIdentifier "BigInt")
        )
      ,JSReturn
        (JSAnd
          (JSAnd
            (JSIdentifier "primeCandidate")
            (JSLeftShift
              (JSCall
                [JSNumber 1]
                (JSIdentifier "BigInt")
              )
              (JSMinus
                (JSCall
                  [JSIdentifier "length"]
                  (JSIdentifier "BigInt")
                )
                (JSCall
                  [JSNumber 1]
                  (JSIdentifier "BigInt")
                )
              )
            )
          )
          (JSCall
            [JSNumber 1]
            (JSIdentifier "BigInt")
          )
        )
      ]
    )
  ,JSFunctionDeclaration
    (JSIdentifier "getRandomBits")
    [JSIdentifier "length"]
    (JSBlock
      [JSAssignment
        (JSVariableDeclaration
          (JSIdentifier "bits")
        )
        (JSArray [])
      ,JSFor
        (JSAssignment
          (JSVariableDeclaration
            (JSIdentifier "i")
          )
          (JSNumber 0)
        ,JSLess
          (JSIdentifier "i")
          (JSIdentifier "length")
        ,JSPostfixPlusUpdate
          (JSIdentifier "i")
        )
        (JSBlock
          [JSCall
            [JSCall
              [JSNumber 0
              ,JSNumber 1
              ]
              (JSIdentifier "getRandomNumber")
            ]
            (JSMember
              (JSIdentifier "push")
              (JSIdentifier "bits")
            )
          ]
        )
      ,JSReturn
        (JSPlus
          (JSString "0b")
          (JSCall
            [JSString ""]
            (JSMember
              (JSIdentifier "join")
              (JSIdentifier "bits")
            )
          )
        )
      ]
    )
  ,JSFunctionDeclaration
    (JSIdentifier "isPrime")
    [JSIdentifier "primeCandidate"
    ,JSIdentifier "numberOfTrials"
    ]
    (JSBlock
      [JSIf
        (JSOrLogical
          (JSStrictEqual
            (JSIdentifier "primeCandidate")
            (JSCall
              [JSNumber 2]
              (JSIdentifier "BigInt")
            )
          )
          (JSStrictEqual
            (JSIdentifier "primeCandidate")
            (JSCall
              [JSNumber 3]
              (JSIdentifier "BigInt")
            )
          )
        )
        (JSBlock
          [JSReturn
            (JSBoolean True)
          ]
        )
        Nothing
      ,JSIf
        (JSOrLogical
          (JSLessOrEqual
            (JSIdentifier "primeCandidate")
            (JSCall
              [JSNumber 1]
              (JSIdentifier "BigInt")
            )
          )
          (JSStrictEqual
            (JSModulo
              (JSIdentifier "primeCandidate")
              (JSCall
                [JSNumber 2]
                (JSIdentifier "BigInt")
              )
            )
            (JSCall
              [JSNumber 0]
              (JSIdentifier "BigInt")
            )
          )
        )
        (JSBlock
          [JSReturn
            (JSBoolean False)
          ]
        )
        Nothing
      ,JSAssignment
        (JSVariableDeclaration
          (JSIdentifier "s")
        )
        (JSCall
          [JSNumber 0]
          (JSIdentifier "BigInt")
        )
      ,JSAssignment
        (JSVariableDeclaration
          (JSIdentifier "r")
        )
        (JSMinus
          (JSIdentifier "primeCandidate")
          (JSCall
            [JSNumber 1]
            (JSIdentifier "BigInt")
          )
        )
      ,JSFor
        (JSEmpty
        ,JSStrictEqual
          (JSAnd
            (JSIdentifier "r")
            (JSCall
              [JSNumber 1]
              (JSIdentifier "BigInt")
            )
          )
          (JSCall
            [JSNumber 0]
            (JSIdentifier "BigInt")
          )
        ,JSEmpty
        )
        (JSBlock
          [JSAssignment
            (JSIdentifier "s")
            (JSPlus
              (JSIdentifier "s")
              (JSCall
                [JSNumber 1]
                (JSIdentifier "BigInt")
              )
            )
          ,JSAssignment
            (JSIdentifier "r")
            (JSDivide
              (JSIdentifier "r")
              (JSCall
                [JSNumber 2]
                (JSIdentifier "BigInt")
              )
            )
          ]
        )
      ,JSFor
        (JSAssignment
          (JSVariableDeclaration
            (JSIdentifier "i")
          )
          (JSNumber 0)
        ,JSLess
          (JSIdentifier "i")
          (JSIdentifier "numberOfTrials")
        ,JSPostfixPlusUpdate
          (JSIdentifier "i")
        )
        (JSBlock
          [JSAssignment
            (JSVariableDeclaration
              (JSIdentifier "randomBigInt")
            )
            (JSCall
              [JSCall
                [JSNumber 2]
                (JSIdentifier "BigInt")
              ,JSMinus
                (JSIdentifier "primeCandidate")
                (JSCall
                  [JSNumber 1]
                  (JSIdentifier "BigInt")
                )
              ]
              (JSIdentifier "getRandomBigInt")
            )
          ,JSAssignment
            (JSVariableDeclaration
              (JSIdentifier "x")
            )
            (JSCall
              [JSIdentifier "randomBigInt"
              ,JSIdentifier "r"
              ,JSIdentifier "primeCandidate"
              ]
              (JSIdentifier "modularExponentiation")
            )
          ,JSIf
            (JSAndLogical
              (JSStrictNotEqual
                (JSIdentifier "x")
                (JSCall
                  [JSNumber 1]
                  (JSIdentifier "BigInt")
                )
              )
              (JSStrictNotEqual
                (JSIdentifier "x")
                (JSMinus
                  (JSIdentifier "primeCandidate")
                  (JSCall
                    [JSNumber 1]
                    (JSIdentifier "BigInt")
                  )
                )
              )
            )
            (JSBlock
              [JSFor
                (JSAssignment
                  (JSVariableDeclaration
                    (JSIdentifier "j")
                  )
                  (JSNumber 1)
                ,JSAndLogical
                  (JSLess
                    (JSCall
                      [JSIdentifier "j"]
                      (JSIdentifier "BigInt")
                    )
                    (JSIdentifier "s")
                  )
                  (JSStrictNotEqual
                    (JSIdentifier "x")
                    (JSMinus
                      (JSIdentifier "primeCandidate")
                      (JSCall
                        [JSNumber 1]
                        (JSIdentifier "BigInt")
                      )
                    )
                  )
                ,JSPostfixPlusUpdate
                  (JSIdentifier "j")
                )
                (JSBlock
                  [JSAssignment
                    (JSIdentifier "x")
                    (JSCall
                      [JSIdentifier "x"
                      ,JSCall
                        [JSNumber 2]
                        (JSIdentifier "BigInt")
                      ,JSIdentifier "primeCandidate"
                      ]
                      (JSIdentifier "modularExponentiation")
                    )
                  ,JSIf
                    (JSStrictEqual
                      (JSIdentifier "x")
                      (JSCall
                        [JSNumber 1]
                        (JSIdentifier "BigInt")
                      )
                    )
                    (JSBlock
                      [JSReturn
                        (JSBoolean False)
                      ]
                    )
                    Nothing
                  ]
                )
              ,JSIf
                (JSStrictNotEqual
                  (JSIdentifier "x")
                  (JSMinus
                    (JSIdentifier "primeCandidate")
                    (JSCall
                      [JSNumber 1]
                      (JSIdentifier "BigInt")
                    )
                  )
                )
                (JSBlock
                  [JSReturn
                    (JSBoolean False)
                  ]
                )
                Nothing
              ]
            )
            Nothing
          ]
        )
      ,JSReturn
        (JSBoolean True)
      ]
    )
  ,JSFunctionDeclaration
    (JSIdentifier "modularExponentiation")
    [JSIdentifier "base"
    ,JSIdentifier "exponent"
    ,JSIdentifier "modulo"
    ]
    (JSBlock
      [JSAssignment
        (JSVariableDeclaration
          (JSIdentifier "result")
        )
        (JSCall
          [JSNumber 1]
          (JSIdentifier "BigInt")
        )
      ,JSFor
        (JSEmpty
        ,JSGreater
          (JSIdentifier "exponent")
          (JSCall
            [JSNumber 0]
            (JSIdentifier "BigInt")
          )
        ,JSEmpty
        )
        (JSBlock
          [JSIf
            (JSStrictEqual
              (JSAnd
                (JSIdentifier "exponent")
                (JSCall
                  [JSNumber 1]
                  (JSIdentifier "BigInt")
                )
              )
              (JSCall
                [JSNumber 1]
                (JSIdentifier "BigInt")
              )
            )
            (JSBlock
              [JSAssignment
                (JSIdentifier "result")
                (JSModulo
                  (JSTimes
                    (JSIdentifier "result")
                    (JSIdentifier "base")
                  )
                  (JSIdentifier "modulo")
                )
              ]
            )
            Nothing
          ,JSAssignment
            (JSIdentifier "exponent")
            (JSRightShift
              (JSIdentifier "exponent")
              (JSCall
                [JSNumber 1]
                (JSIdentifier "BigInt")
              )
            )
          ,JSAssignment
            (JSIdentifier "base")
            (JSModulo
              (JSTimes
                (JSIdentifier "base")
                (JSIdentifier "base")
              )
              (JSIdentifier "modulo")
            )
          ]
        )
      ,JSReturn
        (JSIdentifier "result")
      ]
    )
  ,JSFunctionDeclaration
    (JSIdentifier "getRandomBigInt")
    [JSIdentifier "min"
    ,JSIdentifier "max"
    ]
    (JSBlock
      [JSIf
        (JSStrictEqual
          (JSIdentifier "min")
          (JSIdentifier "max")
        )
        (JSBlock
          [JSReturn
            (JSIdentifier "min")
          ]
        )
        Nothing
      ,JSIf
        (JSGreater
          (JSIdentifier "min")
          (JSIdentifier "max")
        )
        (JSBlock
          [JSAssignment
            (JSIdentifier "min")
            (JSMember
              (JSNumber 0)
              (JSArray
                [JSIdentifier "max"
                ,JSAssignment
                  (JSIdentifier "max")
                  (JSIdentifier "min")
                ]
              )
            )
          ]
        )
        Nothing
      ,JSAssignment
        (JSVariableDeclaration
          (JSIdentifier "currentDigits")
        )
        (JSArray [])
      ,JSAssignment
        (JSVariableDeclaration
          (JSIdentifier "minString")
        )
        (JSCall
          []
          (JSMember
            (JSIdentifier "toString")
            (JSIdentifier "min")
          )
        )
      ,JSAssignment
        (JSVariableDeclaration
          (JSIdentifier "maxString")
        )
        (JSCall
          []
          (JSMember
            (JSIdentifier "toString")
            (JSIdentifier "max")
          )
        )
      ,JSIf
        (JSLess
          (JSMember
            (JSIdentifier "length")
            (JSIdentifier "minString")
          )
          (JSMember
            (JSIdentifier "length")
            (JSIdentifier "maxString")
          )
        )
        (JSBlock
          [JSAssignment
            (JSVariableDeclaration
              (JSIdentifier "zeros")
            )
            (JSArray [])
          ,JSFor
            (JSAssignment
              (JSVariableDeclaration
                (JSIdentifier "i")
              )
              (JSNumber 0)
            ,JSLess
              (JSIdentifier "i")
              (JSMinus
                (JSMember
                  (JSIdentifier "length")
                  (JSIdentifier "maxString")
                )
                (JSMember
                  (JSIdentifier "length")
                  (JSIdentifier "minString")
                )
              )
            ,JSPostfixPlusUpdate
              (JSIdentifier "i")
            )
            (JSBlock
              [JSCall
                [JSNumber 0]
                (JSMember
                  (JSIdentifier "push")
                  (JSIdentifier "zeros")
                )
              ]
            )
          ,JSAssignment
            (JSIdentifier "minString")
            (JSPlus
              (JSCall
                [JSString ""]
                (JSMember
                  (JSIdentifier "join")
                  (JSIdentifier "zeros")
                )
              )
              (JSIdentifier "minString")
            )
          ]
        )
        Nothing
      ,JSFor
        (JSAssignment
          (JSVariableDeclaration
            (JSIdentifier "i")
          )
          (JSNumber 0)
        ,JSLess
          (JSIdentifier "i")
          (JSMember
            (JSIdentifier "length")
            (JSIdentifier "minString")
          )
        ,JSPostfixPlusUpdate
          (JSIdentifier "i")
        )
        (JSBlock
          [JSAssignment
            (JSVariableDeclaration
              (JSIdentifier "currentMinDigit")
            )
            (JSCall
              [JSMember
                (JSIdentifier "i")
                (JSIdentifier "minString")
              ,JSNumber 10
              ]
              (JSIdentifier "parseInt")
            )
          ,JSAssignment
            (JSVariableDeclaration
              (JSIdentifier "currentMaxDigit")
            )
            (JSCall
              [JSMember
                (JSIdentifier "i")
                (JSIdentifier "maxString")
              ,JSNumber 10
              ]
              (JSIdentifier "parseInt")
            )
          ,JSAssignment
            (JSVariableDeclaration
              (JSIdentifier "currentMinDigits")
            )
            (JSCall
              [JSCall
                [JSNumber 0
                ,JSPlus
                  (JSIdentifier "i")
                  (JSNumber 1)
                ]
                (JSMember
                  (JSIdentifier "slice")
                  (JSIdentifier "minString")
                )
              ]
              (JSIdentifier "BigInt")
            )
          ,JSAssignment
            (JSVariableDeclaration
              (JSIdentifier "currentMaxDigits")
            )
            (JSCall
              [JSCall
                [JSNumber 0
                ,JSPlus
                  (JSIdentifier "i")
                  (JSNumber 1)
                ]
                (JSMember
                  (JSIdentifier "slice")
                  (JSIdentifier "maxString")
                )
              ]
              (JSIdentifier "BigInt")
            )
          ,JSAssignment
            (JSVariableDeclaration
              (JSIdentifier "nextDigit")
            )
            (JSCall
              [JSNumber 0
              ,JSNumber 9
              ]
              (JSIdentifier "getRandomNumber")
            )
          ,JSAssignment
            (JSVariableDeclaration
              (JSIdentifier "nextDigits")
            )
            (JSCall
              [JSCall
                [JSString ""]
                (JSMember
                  (JSIdentifier "join")
                  (JSCall
                    [JSIdentifier "nextDigit"]
                    (JSMember
                      (JSIdentifier "concat")
                      (JSIdentifier "currentDigits")
                    )
                  )
                )
              ]
              (JSIdentifier "BigInt")
            )
          ,JSIf
            (JSGreater
              (JSIdentifier "currentMinDigits")
              (JSIdentifier "nextDigits")
            )
            (JSBlock
              [JSAssignment
                (JSIdentifier "nextDigit")
                (JSCall
                  [JSIdentifier "currentMinDigit"
                  ,JSNumber 9
                  ]
                  (JSIdentifier "getRandomNumber")
                )
              ,JSAssignment
                (JSIdentifier "nextDigits")
                (JSCall
                  [JSCall
                    [JSString ""]
                    (JSMember
                      (JSIdentifier "join")
                      (JSCall
                        [JSIdentifier "nextDigit"]
                        (JSMember
                          (JSIdentifier "concat")
                          (JSIdentifier "currentDigits")
                        )
                      )
                    )
                  ]
                  (JSIdentifier "BigInt")
                )
              ,JSIf
                (JSLess
                  (JSIdentifier "currentMaxDigits")
                  (JSIdentifier "nextDigits")
                )
                (JSBlock
                  [JSAssignment
                    (JSIdentifier "nextDigit")
                    (JSCall
                      [JSIdentifier "currentMinDigit"
                      ,JSIdentifier "currentMaxDigit"
                      ]
                      (JSIdentifier "getRandomNumber")
                    )
                  ]
                )
                Nothing
              ]
            )
            (Just
              (JSIf
                (JSLess
                  (JSIdentifier "currentMaxDigits")
                  (JSIdentifier "nextDigits")
                )
                (JSBlock
                  [JSAssignment
                    (JSIdentifier "nextDigit")
                    (JSCall
                      [JSNumber 0
                      ,JSIdentifier "currentMaxDigit"
                      ]
                      (JSIdentifier "getRandomNumber")
                    )
                  ,JSAssignment
                    (JSIdentifier "nextDigits")
                    (JSCall
                      [JSCall
                        [JSString ""]
                        (JSMember
                          (JSIdentifier "join")
                          (JSCall
                            [JSIdentifier "nextDigit"]
                            (JSMember
                              (JSIdentifier "concat")
                              (JSIdentifier "currentDigits")
                            )
                          )
                        )
                      ]
                      (JSIdentifier "BigInt")
                    )
                  ,JSIf
                    (JSGreater
                      (JSIdentifier "currentMinDigits")
                      (JSIdentifier "nextDigits")
                    )
                    (JSBlock
                      [JSAssignment
                        (JSIdentifier "nextDigit")
                        (JSCall
                          [JSIdentifier "currentMinDigit"
                          ,JSIdentifier "currentMaxDigit"
                          ]
                          (JSIdentifier "getRandomNumber")
                        )
                      ]
                    )
                    Nothing
                  ]
                )
                Nothing
              )
            )
          ,JSCall
            [JSIdentifier "nextDigit"]
            (JSMember
              (JSIdentifier "push")
              (JSIdentifier "currentDigits")
            )
          ]
        )
      ,JSReturn
        (JSCall
          [JSCall
            [JSString ""]
            (JSMember
              (JSIdentifier "join")
              (JSIdentifier "currentDigits")
            )
          ]
          (JSIdentifier "BigInt")
        )
      ]
    )
  ,JSFunctionDeclaration
    (JSIdentifier "getRandomNumber")
    [JSIdentifier "min"
    ,JSIdentifier "max"
    ]
    (JSBlock
      [JSReturn
        (JSPlus
          (JSCall
            [JSTimes
              (JSCall
                []
                (JSMember
                  (JSIdentifier "random")
                  (JSIdentifier "Math")
                )
              )
              (JSMinus
                (JSPlus
                  (JSIdentifier "max")
                  (JSNumber 1)
                )
                (JSIdentifier "min")
              )
            ]
            (JSMember
              (JSIdentifier "floor")
              (JSIdentifier "Math")
            )
          )
          (JSIdentifier "min")
        )
      ]
    )
  ]
*/
