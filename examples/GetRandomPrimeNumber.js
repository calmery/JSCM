// ECMAScript 5 and ECMAScript 9 (BigInt)

getRandomPrimeNumber(1024);

/* --- Get a prime number. --- */

/**
 * @param {number} length
 * @return {bigint} Prime number.
 */
function getRandomPrimeNumber(length) {
  var primeCandidate = getRandomPrimeNumberCandidate(length);

  if (isPrime(primeCandidate, 256)) {
    console.log(primeCandidate);
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
