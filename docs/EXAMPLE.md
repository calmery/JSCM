# Example

We have several examples. Here is the first one to get you started.

## Hello World

Input

```javascript
function main() {
  console.log("Hello World");
}

main();
```

Output

```javascript
function main(){
  var _w=new Worker(URL.createObjectURL(new Blob([`

  function main(){
  console.log("Hello World")

  }onmessage=function(e){postMessage(main.apply(this,e.data));};`])));_w.postMessage([]);return new Promise(r=>{_w.onmessage=function(e){r(e.data);_w.terminate();};});
  } main()
```

## Fibonacci numbers

Input

```javascript
function fibonacci(n) {
  if (n < 2) {
    return n;
  } else {
    return fibonacci(n - 2) + fibonacci(n - 1);
  }
}

const result = fibonacci(48);
console.log(result);
```

Output. Even if it is a time-consuming process, it works without stopping the main processing.

```javascript
function fibonacci(n){
  var _w=new Worker(URL.createObjectURL(new Blob([`

  function fibonacci(n){
  if ((n < 2)) {
  return n
  } else {
  return (fibonacci((n - 2))
   + fibonacci((n - 1))
  )
  }
  }onmessage=function(e){postMessage(fibonacci.apply(this,e.data));};`])));_w.postMessage([n,]);return new Promise(r=>{_w.onmessage=function(e){r(e.data);_w.terminate();};});
  } fibonacci(48)
  .then(

  function (result){
  console.log(result)

  })
```

## Quick Sort

Input

```javascript
function quicksort(xs) {
  if (xs.length === 0) {
    return [];
  }

  var x = xs.shift();
  var lt = xs.filter(function lt(y) {
    return y < x;
  });
  var gteq = xs.filter(function gteq(y) {
    return y >= x;
  });

  return quicksort(lt)
    .concat([x])
    .concat(quicksort(gteq));
}

var xs = [];

for (var i = 0; i < 1000000; i++) {
  xs.push(Math.floor(Math.random() * 1000000));
}

var result = quicksort(xs);
console.log(result);
```

Output. Even if it is a time-consuming process, it works without stopping the main processing.

```javascript
function quicksort(xs){
var _w=new Worker(URL.createObjectURL(new Blob([`

function quicksort(xs){
if ((xs.length === 0)) {
return []
}
var x = xs.shift()


var lt = xs.filter(

function lt(y){
return (y < x)
})


var gteq = xs.filter(

function gteq(y){
return (y >= x)
})


return quicksort(lt)
.concat([x,])
.concat(quicksort(gteq)
)

}onmessage=function(e){postMessage(quicksort.apply(this,e.data));};`])));_w.postMessage([xs,]);return new Promise(r=>{_w.onmessage=function(e){r(e.data);_w.terminate();};});
} var xs = []
 for (var i = 0
; (i < 1000000); (i++)) {
xs.push(Math.floor((Math.random()
 * 1000000))
)

} quicksort(xs)
.then(

function (result){
console.log(result)

})
```
